;; remote-tkv-server.lisp -- a Key-Value service aimed at minimizing
;; network traffic
;;
;; DM/RAL 11/17 - original version
;; DM/RAL 09/20 - Updated to use modern Actors
;;
;; --------------------------------------------------------------------------------

#|

The latest design keeps only LZW-compressed, loenc-encoded, data
vectors in the database proper.

All keys are normalized to loenc-encodings of whatever you want to use
for keying. (Sadly, for now, this breaks lexical sort order among some
numbers and strings. Maybe we fix that later... LOENC was designed
before the idea of using it for key normalization was throught up.)

There is no need to form deep copies as they are implicit in the final
client decoding of the encoded data vectors.

So even a local server, in the same Lisp image, need not worry about
clients mutating the interior contents of database entry values. Each
client gets their own local copy on reconstruction from the original
loenc-encoding vector held in the database.

Clients assume responsibility for presenting the server with
loenc-encoded key and data vectors. But client code decodes the
contents received from the database to present normal Lisp objects to
the user. These all become unique deep copies of the database data.

Nothing can change in the database itself until clients commit their
additions / changes / deletions.

For transmission across a network, these LZW-compressed loenc-encoded
vectors will become again encoded, but the overhead of re-encoding a
byte vector is minimal. Just enough to provide a type code and length
indication.

The database on disk stores all data in a loenc-encoded LZW compressed
form. That data being an loenc-encoded FPL tree which holds the
mapping between key and data. So it becomes quite space efficient for
storage and network transmission.

|#

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

(in-package #:arstkv)

;; ---------------------------------------------------------------
(defconstant +STKV-Signature+ "{972F35AC-B87E-11E7-913F-985AEBDA9C2A}")
;; ---------------------------------------------------------------

(define-condition rollback-exception (error)
  ())

(defconstant +rollback-exception+
  (load-time-value (make-condition 'rollback-exception) t))

;; ------------------------------------------------

(defstruct main-table
  (tbl  (maps:empty))
  (ver  (uuid:make-null-uuid))
  (chk  (uuid:make-null-uuid)))

(defun get-ver ()
  (uuid:make-v1-uuid))

;; ------------------------------------------------

(defstruct normalized-key
  repr)

(defmethod normalized-key ((key normalized-key))
  key)

(defmethod normalized-key (key)
  ;; we need to ensure that keys are comparable and orderable, since
  ;; we are using sets and maps. If we used only hash tables then we
  ;; would still need to ensure that keys are comparable.
  ;;
  ;; We would like to allow arbitrary data as keys, so to normalize we
  ;; will use their printed representation
  (make-normalized-key
   :repr (loenc:encode key)))

(defmethod recover-key ((nkey-repr vector))
  (loenc:decode nkey-repr))

;; ------------------------------------------------
;; Server Class as an Actor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:dlambda*
            um:when-let
            
            ac:actor
            ac:with-as-current-actor
            ac:become
            ac:hoare-monitor
            ac:perform-in-actor
            ac:query-actor
            ac:send
            ac:ask
            ac:register-actor
            ac:unregister-actor
            ac:log-info
            ac:find-actor
            
            cps:=wait
            cps:=values

            timeout:*timeout*
            )))

(defclass stkv-server (actor)
  ((main-table :initform (make-main-table))
   (path       :initarg  :path)
   sync)
  (:default-initargs
   :path  (default-database-pathname)
   ))

(defmethod initialize-instance :after ((server stkv-server) &key &allow-other-keys)
  (with-slots (main-table path sync) server
    (with-as-current-actor server
      (setf sync (mp:make-timer 'mp:funcall-async 'save-database server))
      (if (probe-file path)
          (revert-database server)
        (progn
          (setf (main-table-ver main-table) (get-ver))
          (save-database server))
        ))))

(defmethod open-trans ((server stkv-server))
  ;; return a new trans
  (with-slots (main-table) server
    (main-table-ver main-table)))

(defmethod open-trans (server)
  (when-let (actor (find-actor server))
    (open-trans actor)))

(defmethod get-database-key ((server stkv-server) ver key)
  (with-slots (main-table) server
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl     main-table-tbl)) main-table
      (query-actor server
        (cond ((uuid:uuid< ver tbl-ver)
               ;; nope - outdated, try again
               (error +rollback-exception+))
              
              (t
               (maps:find tbl key))
              )))))

(defmethod get-database-key (server ver key)
  (when-let (actor (find-actor server))
    (get-database-key actor ver key)))

(defmethod get-database-keys ((server stkv-server) ver keys)
  (with-slots (main-table) server
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl     main-table-tbl)) main-table
      (query-actor server
        (cond ((uuid:uuid< ver tbl-ver)
               ;; nope - outdated, try again
               (error +rollback-exception+))
              
              (t
               (um:accum acc
                 (mapc (lambda (key)
                         (acc (cons key
                                    (multiple-value-list
                                     (maps:find tbl key)))
                              ))
                       keys)))
              )))))

(defmethod get-database-keys (server ver keys)
  (when-let (actor (find-actor server))
    (get-database-keys actor ver keys)))

(defmethod get-all-database-keys ((server stkv-server) ver)
  (with-slots (main-table) server
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl  main-table-tbl)) main-table
      (query-actor server
        (cond ((uuid:uuid< ver tbl-ver)
               ;; nope - outdated, try again
               (error +rollback-exception+))
              
              (t
               (um:accum acc
                 (maps:iter tbl
                            (lambda (k v)
                              (declare (ignore v))
                              (acc k)))
                 ))
              )))))

(defmethod get-all-database-keys (server ver)
  (when-let (actor (find-actor server))
    (get-all-database-keys actor ver)))

(defvar *writeback-delay*  10)

(defmethod commit-trans ((server stkv-server) ver adds dels)
  ;; either update main table with trans and return a new ID, or
  ;; else signal a rollback error - client needs to start again
  ;; with a fresh :open-trans
  (with-slots (main-table sync) server
    ;; commit changes, returning new version ID
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl     main-table-tbl)) main-table
      (query-actor server
        (cond ((uuid:uuid< ver tbl-ver)
               ;; nope - outdated, try again
               (error +rollback-exception+))
              
              ((and (sets:is-empty adds)
                    (sets:is-empty dels))
               tbl-ver)
              
              (t
               (mp:unschedule-timer sync)
               ;; the map adds, and the set dels, must alrady be using
               ;; normalized key reprs
               (prog1
                   ;; first remove all overlapping keys before adding back
                   ;; in new values, since we can't control which cell
                   ;; gets planted in a union.
                   (setf tbl (sets:union
                              (sets:diff
                               (sets:diff tbl dels)
                               adds)
                              adds)
                         tbl-ver (get-ver))
                 (mp:schedule-timer-relative sync *writeback-delay*)))
              )))))

(defmethod commit-trans (server ver adds dels)
  (when-let (actor (find-actor server))
    (commit-trans actor ver adds dels)))

;; ----------------------------------------------------------------

(defun default-database-pathname ()
  (merge-pathnames "STKV-Persistent-Store.dat"
                   #-:LINUX
                   (sys:get-folder-path :documents)
                   #+:LINUX
                   #P"~/Documents/"))

(defmethod revert-database ((server stkv-server))
  (with-slots (main-table path) server
    (declare (main-table main-table))
    (with-accessors ((ver  main-table-ver)
                     (chk  main-table-chk)
                     (tbl  main-table-tbl)) main-table
      (perform-in-actor server
        (if (probe-file path)
            (with-open-file (f path
                               :direction :input
                               :element-type '(unsigned-byte 8))
              
              (optima:match (loenc:deserialize f
                                               :use-magic (um:magic-word "STKV"))
                ((list signature _ new-ver new-table) when (string= +stkv-signature+ signature)
                 (setf tbl (lzw:decompress new-table)
                       ver new-ver
                       chk new-ver)
                 (log-info :system-log
                           (format nil "Loaded STKV Store ~A:~A" path new-ver)))
                
                (_
                 ;; else
                 (error "Not an STKV Persistent Store: ~A" path))
                ))
          ;; else - no persistent copy, just reset to initial state
          (setf tbl  (maps:empty)
                ver  (uuid:make-null-uuid)
                chk  ver))
        ))))

(defmethod revert-database (server)
  (when-let (actor (find-actor server))
    (revert-database actor)))

(defmethod save-database ((server stkv-server))
  (with-slots (main-table path) server
    (declare (main-table main-table))
    (with-accessors ((ver   main-table-ver)
                     (chk   main-table-chk)
                     (tbl   main-table-tbl)) main-table
      (perform-in-actor server
        (unless (uuid:uuid= ver chk) ;; anything actually changed?
          (ensure-directories-exist path)
          (with-open-file (f path
                             :direction :output
                             :if-exists :rename
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
            
            (loenc:serialize
             (list +stkv-signature+
                   (format nil
                           " --- This is an STKV-SERVER Persistent Store, Version: ~A, Created: ~A --- "
                           ver (uuid:when-created ver))
                   ver
                   (lzw:compress tbl))
             f
             :use-magic (um:magic-word "STKV"))
            (setf chk ver)
            (log-info :system-log
                      (format nil "Saved STKV Store ~A:~A" path ver))
            ))))))

(defmethod save-database (server)
  (when-let (actor (find-actor server))
    (save-database actor)))

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

(defvar *service-id*  :RSTKV)

(defvar *stkv-monitor*  (make-instance 'hoare-monitor))
(defvar *stkv-servers*  (maps:empty))

(defun make-local-stkv-server (&rest args
                                     &key (path (default-database-pathname))
                                     &allow-other-keys)
  (=wait (server is-new) ()
      (perform-in-actor *stkv-monitor*
        (cond ((probe-file path)
               (let* ((key    (namestring (truename path)))
                      (server (maps:find *stkv-servers* key)))
                 (if server
                     (=values server nil)
                   ;; else
                   (progn
                     (setf server (apply 'make-instance 'stkv-server :path path args))
                     (maps:addf *stkv-servers* key server)
                     (=values server t))
                   )))
              
              (t
               (let* ((server (apply 'make-instance 'stkv-server :path path args))
                      (key    (namestring (truename path))))
                 (maps:addf *stkv-servers* key server)
                 (=values server t)))
              ))
    (values server is-new)))

(defun make-remote-stkv-service (&rest args
                                       &key
                                       path
                                       (registration *service-id*))
  (declare (ignore path))
  (=wait (server) ()
      (perform-in-actor *stkv-monitor*
        (multiple-value-bind (server is-new)
            (apply 'make-local-stkv-server args)
          (if registration
              ;; if you aren't requesting registration, then you won't
              ;; need the network neutral interface
              (perform-in-actor server
                ;; have the server mutate itself, to prevent it
                ;; happening while in use by another client
                (register-actor registration server)
                (when is-new
                  (let (prev-handler)
                    (setf prev-handler
                          (become (dlambda*
                                   (:shutdown ()
                                    (:save)
                                    (:quit))
                                   
                                   (:open ()
                                    (open-trans server))
                                   
                                   (:commit (ver adds dels)
                                    (commit-trans server ver adds dels))
                                   
                                   (:get-key (ver key)
                                    (get-database-key server ver key))
                                   
                                   (:get-keys (ver keys)
                                    (get-database-keys server ver keys))
                                   
                                   (:get-all-keys (ver)
                                    (get-all-database-keys server ver))
                                   
                                   (:save ()
                                    (save-database server))
                                   
                                   (:revert ()
                                    (revert-database server))
                                   
                                   (:quit ()
                                    (unregister-actor server))
                                   
                                   (t (&rest msg)
                                      (apply prev-handler msg))
                                   )))
                    ))
                ;; server performs this
                (=values server))

            ;; monitor performs this
            (=values server))))
    server))

;; ---------------------------------------------------------------
;; Client Side - make client do all of the work, except for commits,
;; to cut down on comms

(defmacro with-server ((server) &body body)
  `(let ((*service-id* ,server))
     ,@body))

(defvar *rstkv-timeout*  10)

(defstruct trans
  (db     *service-id*)
  ver
  (cache  (maps:empty))
  (cow    (maps:empty))
  (nfnd   (sets:empty))
  (dels   (sets:empty)))

(defmethod get-server (trans)
  trans)

(defmethod get-server ((trans null))
  *service-id*)

(defmethod get-server ((trans trans))
  (trans-db trans))

(defun !kv (trans &rest msg)
  (apply 'send (get-server trans) msg))

(defun !?kv (trans &rest msg)
  (let ((*timeout* *rstkv-timeout*))
    (apply 'ask (get-server trans) msg)))

;; ---------------------------------------------

(defmethod open-trans (server)
  (!?kv server :open))

(defmethod get-database-key (server ver key)
  (!?kv server :get-key ver key))

(defmethod get-database-keys (server ver keys)
  (!?kv server :get-keys ver keys))

(defmethod get-all-database-keys (server ver)
  (!?kv server :get-all-keys ver))

(defmethod commit-trans (server ver adds dels)
  (!?kv server :commit ver adds dels))

(defmethod save-database (server)
  (!kv server :save))

(defmethod revert-database (server)
  (!kv server :revert))

(defmethod shutdown-server (server)
  (!kv server :shutdown))

(defmethod quit-server (server)
  (!kv server :quit))

;; ---------------------------------------------

(defun rollback (&optional trans (*service-id* *service-id* server-provided-p))
  ;; Rollback as often as you like, but especially after a rollback
  ;; exceeption. Resets our view to the last committed state.  Returns
  ;; a new transaction object for use in get-key, set-key, map-table,
  ;; and commit.
  (let* ((server  (if server-provided-p
                      *service-id*
                    trans))
         (new-ver (open-trans server)))
    (cond (trans
           (with-accessors ((db    trans-db)
                            (ver   trans-ver)
                            (cow   trans-cow)
                            (dels  trans-dels)
                            (nfnd  trans-nfnd)
                            (cache trans-cache)) trans
             (setf db    (get-server server)
                   cache (maps:empty)
                   cow   (maps:empty)
                   dels  (sets:empty)
                   nfnd  (sets:empty)
                   ver   new-ver)
             trans))
          
          (t
           (make-trans
            :ver  new-ver))
          )))

(defun commit (trans)
  (with-accessors ((ver   trans-ver)
                   (cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (unless (and (maps:is-empty cow)
                 (sets:is-empty dels))
      (setf ver   (commit-trans trans ver (maps:map cow 'lzw:compress) dels)
            cache (maps:union cow cache)
            nfnd  (sets:union dels nfnd)
            cow   (maps:empty)
            dels  (sets:empty)))
    trans))

(defmacro with-normalized-key ((name repr-name) key &body body)
  `(let* ((,name      (normalized-key ,key))
          (,repr-name (normalized-key-repr ,name)))
     ,@body))

(defun get-locally (trans key)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (if (or (sets:mem dels nkey-repr)
              (sets:mem nfnd nkey-repr))
          (values nil nil t) ;; return default, not-found, and known missing
        (multiple-value-bind (val found)
            (maps:find cow nkey-repr)
          (if found
              (values val t)
            (maps:find cache nkey-repr)))
        ))))
  
(defun get-key (trans key &optional default)
  (with-accessors ((ver   trans-ver)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (multiple-value-bind (val found del)
          (get-locally trans nkey)
        (cond (del
               (values default nil))
              (found
               (values val t))
              (t
               (multiple-value-bind (val found)
                   (get-database-key trans ver nkey-repr)
                 (cond (found
                        (let ((rval (lzw:decompress val)))
                          (setf cache (maps:add cache nkey-repr rval))
                          (values rval t)))
                       
                       (t
                        (setf nfnd (sets:add nfnd nkey-repr))
                        (values default nil))
                       )))
              )) )))

(defun get-keys (trans keys &optional default)
  (with-accessors ((ver   trans-ver)
                   (cache trans-cache)
                   (nfnd  trans-nfnd)) trans
    (let (local
          not-local)
      (dolist (key keys)
        (with-normalized-key (nkey nkey-repr) key
          (multiple-value-bind (val found del)
              (get-locally trans nkey)
            (cond (del
                   (push `(,key ,default nil) local))
                  (found
                   (push `(,key ,val t) local))
                  (t
                   (push nkey-repr not-local))
                  ))))
      (when not-local
        (dolist (triple (get-database-keys trans ver not-local))
          (destructuring-bind (key val found) triple
            (let ((rkey (recover-key key))
                  (rval (lzw:decompress val)))
              (cond (found
                     (setf cache (maps:add cache key rval))
                     (push `(,rkey ,rval t) local))
                    
                    (t
                     (setf nfnd (sets:add nfnd key))
                     (push `(,rkey ,default nil) local))
                    )))))
      local)))

(defun get-all-keys (trans)
  (with-accessors ((ver   trans-ver)
                   (dels  trans-dels)
                   (cow   trans-cow)) trans

    (flet ((list->set (lst &optional (init (sets:empty)))
             (reduce (lambda (ans k)
                       (sets:add ans k))
                     lst
                     :initial-value init))
           
           (map->set (map &optional (init (sets:empty)))
             (maps:fold map
                        (lambda (k v acc)
                          (declare (ignore v))
                          (sets:add acc k))
                        init))
           
           (map-set-to-list (fn set)
             (um:accum acc
               (sets:iter set
                          (lambda (k)
                            (acc (funcall fn k)))
                          ))))
      
      (let* ((rkeys (get-all-database-keys trans ver))
             ;; less the deleted keys on this end
             (rset  (sets:diff (list->set rkeys) dels))
             ;; plus any new keys on this end
             (keys  (map->set cow rset)))
        (map-set-to-list 'recover-key keys)
        ))))


(defun set-key (trans key val)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (setf cow   (maps:add cow nkey-repr val)
            nfnd  (sets:remove nfnd nkey-repr)
            dels  (sets:remove dels nkey-repr)
            cache (maps:remove cache nkey-repr))
      val)))

(defun delete-key (trans key)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (setf dels  (sets:add dels nkey-repr)
            nfnd  (sets:remove nfnd nkey-repr)
            cache (maps:remove cache nkey-repr)
            cow   (maps:remove cow nkey-repr))
      key)))

(defun map-locally (trans fn)
  ;; only maps over those entries that we have either looked up,
  ;; added, or changed. But not the deletes and not-found.
  (with-accessors ((cow   trans-cow)
                   (cache trans-cache)) trans
    (maps:iter (sets:union cache cow)
               (lambda (k v)
                 (funcall fn (recover-key k) v))
               )))
               
(defsetf get-key set-key)

(defun save (trans)
  (save-database trans))

(defun revert (trans)
  (revert-database trans))

#| ;; example
(with-server ("rstkv@rincon.local")
  (let ((trans (rollback)))
    (get-key trans :pi)))
|#
;; -----------------------------------------------------

(defmethod make-readable-entry ((entry vector))
  (recover-key entry))
   
(defmethod make-readable-entry ((entry maps::map-cell))
  (maps::make-map-cell
   :key (recover-key  (maps::map-cell-key entry))
   :val (maps::map-cell-val entry)))

(defun view-set (set)
  (sets:view-set set
                 :key 'make-readable-entry))

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

#|
(progn
  (mapc (lambda (k)
          (delete-key tran k))
        (get-all-keys tran))
  (mapc (lambda (assoc)
          (setf (get-key tran (first assoc)) (second assoc)))
        '((:CAT (:NAME "Chara" :LIKES :MOUSE))
          (:DOG (:ATTR :MANS-BEST-FRIEND :LIKES :CAT))
          (:MAN :DOG)
          (:MOUSE (:BEHAVIOR :MEEK :AFRAID-OF :CAT))
          (:WOMAN (:NAME "Helene" :MARRIED-TO "David"))))
  (commit tran)
  (rollback tran))

(ac:spawn (lambda () (let ((trans (rollback))) (ac:pr (ac:ask :rstkv :get-all-keys (trans-ver trans))))))
(let ((trans (rollback))) (ac:pr (ac:ask :rstkv :get-all-keys (trans-ver trans))))
|#
#|
(defparameter *tbl* (sets:empty))
(defun get-all-types ()
  (setf *tbl* (sets:empty))
  (hcl:sweep-all-objects
   (lambda (obj)
     (let ((item (loenc:encode (type-of obj))))
       (setf *tbl* (sets:add *tbl* item)))))
  (sets:view-set *tbl*
                 :key 'loenc:decode))

(defun get-all-classes ()
  (setf *tbl* (sets:empty))
  (hcl:sweep-all-objects
   (lambda (obj)
     (let* ((class (class-of obj))
            (item  (class-name class)))
       (setf *tbl* (sets:add *tbl* item)))))
  (sets:view-set *tbl*))

(defmethod screener ((obj stream))
  t)

(defmethod screener ((obj clos::system-object))
  t)

(defmethod screener (obj)
  nil)

(defun get-builtin-classes (type)
  (setf *tbl* (sets:empty))
  (hcl:sweep-all-objects
   (lambda (obj)
     (when (subtypep (type-of obj) type)
       (let* ((class (class-of obj))
              (item  (class-name class)))
         (setf *tbl* (sets:add *tbl* item))))))
  (um:accum acc
    (sets:iter *tbl*
               (lambda (k)
                 (acc k))
               )))

(defun sinc (x)
  (let ((phi (* 2 pi x)))
    (/ (sin phi) phi)))

(defun sinsinc (n x)
  ;; describes leakage in cell j of N-point FFT
  ;; for x = j/N - F/Fs
  (let ((phi (* 2 pi x)))
    (/ (sin (* n phi))
       (* n (sin phi)))))

|#
