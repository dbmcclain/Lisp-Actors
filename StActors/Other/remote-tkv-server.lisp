;; remote-tkv-server.lisp -- a Key-Value service aimed at minimizing
;; network traffic
;;
;; DM/RAL 11/17
;;
;; --------------------------------------------------------------------------------

#|
(defpackage #:remote-tkv-server
  (:use #:common-lisp)
  (:nicknames #:rstkv)
  (:import-from #:bfly
   #:!
   #:!?
   #:make-service
   #:unregister-service
   #:log-info)
  (:export
   #:make-remote-stkv-service
   #:rollback
   #:commit
   #:get-key
   #:get-keys
   #:set-key
   #:delete-key
   #:map-locally
   #:save
   #:revert
   #:rollback-exception
   ))
|#

(in-package #:remote-tkv-server)

;; ---------------------------------------------------------------
(defconstant +STKV-Signature+ "{972F35AC-B87E-11E7-913F-985AEBDA9C2A}")
;; ---------------------------------------------------------------

(define-condition rollback-exception (error)
  ())

(defconstant +rollback-exception+
  (load-time-value (make-condition 'rollback-exception)))

;; ------------------------------------------------

(defun get-ver ()
  (uuid:make-v1-uuid))

(defstruct main-table
  (tbl  (maps:empty))
  (ver  (uuid:make-null-uuid))
  (chk  (uuid:make-null-uuid)))

;; ------------------------------------------------

(defun s-get-key (main-table ver key default)
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl     main-table-tbl)) main-table
    (cond ((uuid:uuid< ver tbl-ver)
           ;; nope - outdated, try again
           (error +rollback-exception+))
          
          (t
           (maps:find key tbl default))
          )))

(defun s-get-keys (main-table ver keys default)
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl     main-table-tbl)) main-table
    (cond ((uuid:uuid< ver tbl-ver)
           ;; nope - outdated, try again
           (error +rollback-exception+))
          
          (t
           (um:accum acc
             (mapc (lambda (key)
                     (multiple-value-bind (val found)
                         (maps:find key tbl default)
                       (acc `(,key ,val ,found))))
                   keys)))
          )))

(defun s-get-all-keys (main-table)
  (declare (main-table main-table))
  (with-accessors ((tbl  main-table-tbl)) main-table
    (um:accum acc
      (maps:iter (lambda (k v)
                   (declare (ignore v))
                   (acc k))
                 tbl))))


(defun s-commit (main-table sync ver adds dels)
  ;; commit changes, returning new version ID
  ;;
  ;; Because of our committment to remote operation, and deep-copy
  ;; when local, the trans will always have a changed table, even if
  ;; nothing really did change in it.
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl     main-table-tbl)) main-table
    (cond ((uuid:uuid< ver tbl-ver)
           ;; nope - outdated, try again
           (error +rollback-exception+))
          
          ((and (sets:is-empty adds)
                (sets:is-empty dels))
           tbl-ver)
          
          (t
           (prog1
               ;; first remove overlapping keys before adding back in
               ;; new values, since we can't control which cell gets
               ;; planted in a union.
               (setf tbl (sets:union
                          (sets:diff
                           (sets:diff tbl dels)
                           adds)
                          adds)
                     tbl-ver (get-ver))
             (mp:schedule-timer-relative sync 60)))
          )))

;; ---------------------------------------------------------------

(defun database-pathname ()
  (merge-pathnames "STKV-Persistent-Store.dat"
                   #-:LINUX
                   (sys:get-folder-path :documents)
                   #+:LINUX
                   #P"~/Documents/"))

(defun s-load-database (main-table)
  (declare (main-table main-table))
  (with-accessors ((ver  main-table-ver)
                   (chk  main-table-chk)
                   (tbl  main-table-tbl)) main-table
    (let ((fname (database-pathname)))
      (if (probe-file fname)
          (with-open-file (f fname
                             :direction :input
                             :element-type '(unsigned-byte 8))
            
            (optima:match (loenc:deserialize f
                                             :use-magic (um:magic-word "STKV"))
              ((list signature _ new-ver new-table) when (string= +stkv-signature+ signature)
               (setf tbl new-table
                     ver new-ver
                     chk new-ver)
               (log-info :system-log
                         (format nil "Loaded STKV Store ~A" new-ver)))
              
              (_
               ;; else
               (error "Not an STKV Persistent Store: ~A" fname))
              ))
        ;; else - no persistent copy, just reset to initial state
        (setf tbl  (maps:empty)
              ver  (uuid:make-null-uuid)
              chk  ver))
      )))

(defun s-save-database (main-table)
  (declare (main-table main-table))
  (with-accessors ((ver   main-table-ver)
                   (chk   main-table-chk)
                   (tbl   main-table-tbl)) main-table
    (unless (uuid:uuid= ver chk) ;; anything actually changed?
      (ensure-directories-exist (database-pathname))
      (with-open-file (f (database-pathname)
                         :direction :output
                         :if-exists :rename
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
        
        (loenc:serialize
         (list +stkv-signature+
               (format nil
                       " --- This is an STKV-SERVER Persistent Store, Version: ~A, Created: ~A --- "
                       ver (uuid:when-created ver))
               ver tbl)
         f
         :use-magic (um:magic-word "STKV"))
        (setf chk ver)
        (log-info :system-log
                  (format nil "Saved STKV Store ~A" ver))
        ))))

;; ---------------------------------------------------------------
;; bare minimum services offered - keeps comm traffic to a minimum
;; across network. Puts burden on clients to do most of the work
;; locally.
;;
;; Old version was based on a hash-table read-only master table, and
;; private COW hash-tables. Tried to keep communication costs low by
;; transferring only changed pairs. But it suffered from SMP safe
;; opening and commits, where an entire hash-table needs to be
;; constructed, copying every element from two older tables on commit.
;; Open and commit were lengthy operations.
;;
;; This version is now based on purely functional balanced binary
;; trees (a bit slower - O(Log(N)) ) but offers the possibility that,
;; once a transaction has begun, it will be with forever consistent
;; data. Open and commit are extremely quick, except for network
;; traffic in any event.
;;
;; On successful commit, the transaction ID is updated so you can keep
;; rolling forward with the same transaction object.  On a rollback
;; exception, you need to restart with a call to ROLLBACK to get a new
;; transaction object.
;;
;; Only on commit do we find out if we have been outdated. But before
;; signalling a rollback exception, the server updates the transaction
;; to the latest version. (... that doesn't help a remote
;; connection) The transaction object continues to be valid going
;; forward, and does not need to be reconstructed anew.
;;
;; However, each client :open/:commit causes a full table copy over
;; the network.
;;
;; You don't ever have to :commit. You can just use your own private
;; copy of the table for whatever purposes, and simply discard it at
;; the end without updating the master table. But none of your changes
;; will persist unless you :commit.
;;
;; This version may be a bit less efficient on read/write/open/commit
;; in one sense, but it is far more efficient with regard to SMP
;; sharing as a result of truly functonal data structures in use.
;;
;; In this case the Actor based handler isn't really needed so much
;; for serializing requests, except for commits. But it is the sole
;; keeper of the master table.  Once opened for transactions, the
;; service is never called again until a commit.
;;
;; NOTE: Even though the tables are safely shared, the value objects
;; stored in the table must be treated as read-only and should never
;; be mutated. A fresh object should always be constructed for updates
;; through SET-KEY. Of course, over a remote connection you do have
;; your own copies. In the same running Lisp image, you could perform
;; your own deep copy in order to sidestep this issue.
;;
;; NOTE: we use a canary scheme to determine when to make deep copies,
;; so go ahead and mutate table value objects.
;;
;; Another advantage of the functional map for tables is that items
;; are kept in some kind of sorted order.

(defun make-remote-stkv-service ()
  "A Simple TKV Service"
  (make-service (:actor
                 :register :RSTKV)
    (let ((main-table (make-main-table))
          (sync       (mp:make-timer #'save)))

      (s-load-database main-table)
      (um:dlambda
        
        (:shutdown ()
         (s-save-database main-table))
        
        (:open ()
         ;; return a new trans
         (main-table-ver main-table))
        
        (:commit (ver adds dels)
         ;; either update main table with trans and return a new ID, or
         ;; else signal a rollback error - client needs to start again
         ;; with a fresh :open
         (s-commit main-table sync ver adds dels)) ;; return new ID for client update
        
        (:get-key (ver key &optional default)
         (s-get-key main-table ver key default))
        
        (:get-keys (ver keys &optional default)
         (s-get-keys main-table ver keys default))

        (:get-all-keys ()
         (s-get-all-keys main-table))
        
        (:save ()
         (s-save-database main-table))
        
        (:revert ()
         (s-load-database main-table))
        
        (:quit ()
         (unregister-service :RSTKV))
        ))))

;; ---------------------------------------------------------------
;; Client Side - make client do all of the work, except for commits,
;; to cut down on comms

(defvar *rstkv-timeout*  10)

(defstruct trans
  ver
  cache
  cow
  nfnd
  dels)

(defun deep-copy (obj)
  ;;
  ;; Make a deep copy, but keep structure sharing intact. This allows
  ;; us to treat value objects as mutable.
  ;;
  ;; If two values in the object are EQ before deep-copy, they will
  ;; continue to be EQ afterward, even though the values themselves
  ;; are fresh copies. They will be NOT EQ to their former values.
  ;;
  (loenc:decode (loenc:encode obj)))

(defun rollback (&optional trans)
  ;; Rollback as often as you like, but especially after a rollback
  ;; exceeption. Resets our view to the last committed state.  Returns
  ;; a new transaction object for use in get-key, set-key, map-table,
  ;; and commit.
  (let ((new-ver (!? :rstkv :open *rstkv-timeout*)))
    (cond (trans
           (with-accessors ((ver   trans-ver)
                            (cow   trans-cow)
                            (dels  trans-dels)
                            (nfnd  trans-nfnd)
                            (cache trans-cache)) trans
             (setf cache (maps:empty)
                   cow   (maps:empty)
                   dels  (sets:empty)
                   nfnd  (sets:empty)
                   ver   new-ver)
             trans))
          
          (t
           (make-trans
            :cache (maps:empty)
            :cow   (maps:empty)
            :dels  (sets:empty)
            :nfnd  (sets:empty)
            :ver   new-ver))
          )))

(defun commit (trans)
  (with-accessors ((ver   trans-ver)
                   (cow   trans-cow)
                   (dels  trans-dels)
                   (cache trans-cache)) trans
    (unless (and (maps:is-empty cow)
                 (sets:is-empty dels))
      (setf ver (!? :rstkv `(:commit ,ver ,cow ,dels) *rstkv-timeout*)
            cache (sets:union cow cache)
            cow   (maps:empty)
            dels  (sets:empty)))
    ))

(defun get-locally (trans key)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (if (or (sets:mem key dels)
            (sets:mem key nfnd))
        (values nil nil t) ;; return default, not-found, and known missing
      (multiple-value-bind (val found)
          (maps:find key cow)
        (if found
            (values val t)
          (maps:find key cache)))
      )))
  
(defun get-key (trans key &optional default)
  (with-accessors ((ver   trans-ver)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (multiple-value-bind (val found del)
        (get-locally trans key)
      (cond (del
             (values default nil))
            (found
             (values val t))
            (t
             (multiple-value-bind (val found)
                 (!? :rstkv `(:get-key ,ver ,key ,default) *rstkv-timeout*)
               (if found
                   (setf cache (maps:add key val cache))
                 (setf nfnd (sets:add key nfnd)))
               (values val found)))
            )) ))

(defun get-keys (trans keys &optional default)
  (with-accessors ((ver   trans-ver)
                   (cache trans-cache)
                   (nfnd  trans-nfnd)) trans
    (let (local
          not-local)
      (dolist (key keys)
        (multiple-value-bind (val found del)
            (get-locally trans key)
          (cond (del
                 (push `(,key ,default nil) local))
                (found
                 (push `(,key ,val t) local))
                (t
                 (push key not-local))
                )))
      (if not-local
          (let ((remote (!? :rstkv `(:get-keys ,ver ,not-local) *rstkv-timeout*)))
            (dolist (triple remote)
              (destructuring-bind (key val found) triple
                (if found
                    (setf cache (maps:add key val cache))
                  (setf nfnd (sets:add key nfnd)))
                ))
            (nconc remote local))
        local)
      )))

(defun get-all-keys ()
  (!? :rstkv `(:get-all-keys) *rstkv-timeout*))

(defun set-key (trans key val)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (setf cow   (maps:add key val cow)
          nfnd  (sets:remove key nfnd)
          dels  (sets:remove key dels)
          cache (maps:remove key cache))
    val))

(defun delete-key (trans key)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (setf dels  (sets:add key dels)
          nfnd  (sets:remove key nfnd)
          cache (maps:remove key cache)
          cow   (maps:remove key cow))
    ))

(defun map-locally (trans fn)
  ;; only maps over those entries that we have either looked up,
  ;; added, or changed. But not the deletes and not-found.
  (with-accessors ((cow   trans-cow)
                   (cache trans-cache)) trans
    (maps:iter fn (sets:union cache cow))))

(defsetf get-key set-key)

(defun save ()
  (! :rstkv :save))

(defun revert ()
  (! :rstkv :revert))

#|
(make-remote-stkv-service)
(setf tran (rollback))
(setf (get-key tran :dog)   :cat
      (get-key tran :cat)   :mouse
      (get-key tran :mouse) :man
      (get-key tran :man)   :dog)
(commit tran)
(get-key tran :man)
(get-keys tran '(:cat :mouse :man :dog :bird))
(save)
(um:accum acc
  (map-locally tran (lambda (k v)
                      (acc (cons k v)))))

(bfly:register-service :rstkv
                       (bfly:remote-service
                        "rstkv@Dachshund.local"))

(defvar *trans* (rollback))
(progn
  (set-key *trans* :bf/all-files (car (linda:srdp '(:all-files ?x))))
  (set-key *trans* :bf/all-files-hashes (car (linda:srdp '(:all-files-hashes ?x))))
  (set-key *trans* :bf/all-files-bloom-filter (car (linda:srdp '(:all-files-bloom-filter ?x))))
  (set-key *trans* :bf/full-dir-tree (car (linda:srdp '(:full-dir-tree ?x))))
  (set-key *trans* :bf/full-directory (car (linda:srdp '(:full-directory ?x ? ?))))
  (commit *trans*))

(progn
  (delete-key *trans* :bf/all-files)
  (delete-key *trans* :bf/all-files-hashes)
  (delete-key *trans* :bf/all-files-bloom-filter)
  (delete-key *trans* :bf/full-dir-tree)
  (delete-key *trans* :bf/full-directory)
  (commit *trans*))

(let ((old-keys '(:bf-all-files
                  :bf-all-files-hashes
                  :bf-all-files-bloom-filter
                  :bf-full-dir-tree
                  :bf-full-directory))
      (new-keys '(:bf/all-files
                  :bf/all-files-hashes
                  :bf/all-files-bloom-filter
                  :bf/full-dir-tree
                  :bf/full-directory)))
  (get-keys *trans* old-keys)
  (um:foreach (lambda (k-old k-new)
                (set-key *trans* k-new
                         (get-key *trans* k-old))
                (delete-key *trans* k-old))
              old-keys new-keys)
  (commit *trans*))
|#

