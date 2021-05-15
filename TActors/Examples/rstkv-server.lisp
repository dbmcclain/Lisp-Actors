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
   #:make-stk-server
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

(in-package #:rstkv-server)

;; ---------------------------------------------------------------
(defconstant +STKV-Signature+ "{972F35AC-B87E-11E7-913F-985AEBDA9C2A}")
;; ---------------------------------------------------------------

(define-condition rollback-exception (error)
  ())

(defconstant +rollback-exception+
  (load-time-value (make-condition 'rollback-exception) t))

;; ------------------------------------------------

(defstruct main-table
  path
  (tbl  (maps:empty))
  (ver  (uuid:make-null-uuid))
  (chk  (uuid:make-null-uuid))
  sync)

(defun new-ver ()
  (uuid:make-v1-uuid))

;; ------------------------------------------------
;; Server Class as an Actor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:when-let
            )))

(defun make-remote-api-beh (tbl)
  (let ((io-ser (io (serializer
                     (α (cust fn)
                       (funcall fn cust))
                     ))
                ))
    (lambda (cust &rest msg)
      (um:dcase msg
        (:shutdown ()
         (unregister-actor self)
         (send io-ser cust
               (lambda (cust)
                 (s-save-database cust tbl))))
      
        (:open ()
         (send cust (s-open-trans tbl)))
      
        (:commit (kbad ver adds dels)
         (handler-case
             (send cust (s-commit-trans tbl ver adds dels))
           (error ()
             (send kbad))))
      
        (:get-key (kbad ver key)
         (handler-case
             (send cust (s-get-database-key tbl ver key))
           (error ()
             (send kbad))))
      
        (:get-keys (kbad ver keys)
         (handler-case
             (send cust (s-get-database-keys tbl ver keys))
           (error ()
             (send kbad))))
      
        (:get-all-keys (kbad ver)
         (handler-case
             (send cust (s-get-all-database-keys tbl ver))
           (error ()
             (send kbad))))
      
        (:save ()
         (send io-ser cust
               (lambda (cust)
                 (s-save-database cust tbl))
               ))
      
        (:revert (kbad)
         (let ((my-cust (α msg
                          (um:dcase msg
                            (:UNDEFINED _
                             (send kbad))
                            (t _
                             (repeat-send cust))
                            ))))
           (send io-ser my-cust
               (lambda (cust)
                 (s-revert-database cust tbl)))))
      
        (:quit ()
         (unregister-actor tbl)
         (send cust))
        ))))

(defun s-open-trans (main-table)
  ;; return a new trans
  (main-table-ver main-table))

(defun s-get-database-key (main-table ver key)
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl     main-table-tbl)) main-table
    (cond ((uuid:uuid< ver tbl-ver)
           ;; nope - outdated, try again
           (error +rollback-exception+))
          
          (t
           (maps:find tbl key))
          )))

(defun s-get-database-keys (main-table ver keys)
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl     main-table-tbl)) main-table
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
          )))

(defun s-get-all-database-keys (main-table ver)
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl  main-table-tbl)) main-table
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
          )))

(defvar *writeback-delay*  10)

(defun s-commit-trans (main-table ver adds dels)
  ;; either update main table with trans and return a new ID, or
  ;; else signal a rollback error - client needs to start again
  ;; with a fresh :open-trans

  ;; commit changes, returning new version ID
  (declare (main-table main-table))
  (with-accessors ((tbl-ver main-table-ver)
                   (tbl     main-table-tbl)
                   (sync    main-table-sync)) main-table
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
                     tbl-ver (new-ver))
             (mp:schedule-timer-relative sync *writeback-delay*)))
          )))

;; ----------------------------------------------------------------

(defun default-database-pathname ()
  (merge-pathnames "STKV-Persistent-Store.dat"
                   #-:LINUX
                   (sys:get-folder-path :documents)
                   #+:LINUX
                   #P"~/Documents/"))

;; -----------------------------

(defun s-revert-database (cust main-table)
  (declare (main-table main-table))
  (with-accessors ((ver  main-table-ver)
                   (chk  main-table-chk)
                   (tbl  main-table-tbl)
                   (path main-table-path)) main-table
    (if (probe-file path)
        (restart-case
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
                           (format nil "Loaded STKV Store ~A:~A" path new-ver))
                 (send cust new-ver))
                
                (_
                 (error "Not an STKV Persistent Store: ~A" path))
                ))
          (abort ()
            (send cust :UNDEFINED))
          )
      ;; else - no persistent copy, just reset to initial state
      (send cust (setf tbl  (maps:empty)
                       ver  (uuid:make-null-uuid)
                       chk  ver))
      )))

;; ---------------------------------

(defun s-save-database (cust main-table)
  (declare (main-table main-table))
  (with-accessors ((ver   main-table-ver)
                   (chk   main-table-chk)
                   (tbl   main-table-tbl)
                   (path  main-table-path)) main-table
    (if (uuid:uuid= ver chk) ;; anything actually changed?
        (send cust ver)
      (β ()
          (send (α (cust)
                  (ensure-directories-exist path)
                  (send cust))
                β)
        (β msg
            (send (α (cust)
                    (restart-case
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
                          (send cust))
                      (abort ()
                        (send cust :UNDEFINED))))
                  β)
          (um:dcase msg
            (:UNDEFINED _
             (repeat-send cust))
            (t _
               (log-info :system-log
                         (format nil "Saved STKV Store ~A:~A" path ver))
               (send cust ver))
            ))))
    ))

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
;; Another advantage of the functional map for tables is that items
;; are kept in some kind of sorted order.

(defvar *service-id*    :RSTKV)
(defvar *stkv-servers*  (maps:empty))

(defun make-stkv-server (cust
                         &key
                        (path (default-database-pathname))
                        (registration *service-id*))
  (let* ((make-new-server
          (α (cust)
            (let* ((tbl    (make-main-table
                            :path  path))
                   (server (make-actor (make-remote-api-beh tbl)))
                   (key    (namestring (truename path)))
                   (fwd    (α _
                             (maps:addf *stkv-servers* key server)
                             (send cust server))))
              (setf (main-table-sync tbl)
                    (mp:make-timer 'mp:funcall-async #'send server (sink) :save))
              (if (probe-file path)
                  (s-revert-database fwd tbl)
                (progn
                  ;; file doesn't exist - so create it
                  (setf (main-table-ver tbl) (new-ver))
                  (s-save-database fwd tbl)))
              )))
         (get-new-or-existing
          (α (cust)
            (cond ((probe-file path)
                   (let* ((key    (namestring (truename path)))
                          (server (maps:find *stkv-servers* key)))
                     (if server
                         (send cust server)
                       (send make-new-server cust))))
                  (t
                   (send make-new-server cust))
                  ))))
    (β (server)
        (send get-new-or-existing β)
      (when registration
        (register-actor registration server))
      (send cust server))))
#|
(make-stkv-server (sink))
(find-actor println :rstkv)

(let ((kbad (α ()
              (send println "Rollback Exception"))))
  (β (rstkv)
      (find-actor β :rstkv)
    (β (ver)
        (send rstkv β :open)
        ;; (send β 0)
      (β (keys)
          (send rstkv β :get-all-keys kbad ver)
        (send println (mapcar #'loenc:decode keys))
        (β (data)
            (send rstkv β :get-key kbad ver (loenc:encode :pi))
          (send println (lzw:decompress data)))))))
|#



