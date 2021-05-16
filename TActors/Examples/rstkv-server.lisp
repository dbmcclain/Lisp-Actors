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

(defpackage #:rstkv-server
  (:use #:common-lisp #:actors)
  (:export
   #:*service-id*
   #:*writeback-delay*
   #:make-stkv-server
   ))

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
  (tbl  (maps:empty))
  (ver  (uuid:make-null-uuid))
  (chk  (uuid:make-null-uuid)))

(defstruct (state
            (:constructor %make-state))
  main-table
  path
  sync
  io-ser)

(defun new-ver ()
  (uuid:make-v1-uuid))

;; ------------------------------------------------
;; Server Class as an Actor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:dcase
            um:dlambda
            um:accum
            um:magic-word
            )))

;; ------------------------------------------------

(defun make-initial-state (&key main-table path)
  (%make-state
   :path       path
   :main-table main-table
   ;; io-ser needs to persist across BECOME
   :io-ser     (io (serializer
                    (α (cust fn)
                      (funcall fn cust))))
   ))

(defun finalize-state (state server)
  (setf (state-sync state)
        (mp:make-timer #'mp:funcall-async #'send server :save (sink) )))

;; ------------------------------------------------

(defun make-kv-server-beh (state)
  ;; Actual (internal) kv-store server
  (with-accessors ((io-ser  state-io-ser)) state
    (ensure-par-safe-behavior
     ;; -- because we mutate local state
     (lambda (cust &rest msg)
       (um:dcase msg
         (:open () ;; open a transaction
          (send cust (s-open-trans state)))
         
         (:commit (kbad ver adds dels)
          (handler-case
              (send cust (s-commit-trans state ver adds dels))
            (error ()
              (send kbad))))
         
         (:get-key (kbad ver key)
          (handler-case
              (send cust (s-get-database-key state ver key))
            (error ()
              (send kbad))))
         
         (:get-keys (kbad ver keys)
          (handler-case
              (send cust (s-get-database-keys state ver keys))
            (error ()
              (send kbad))))
         
         (:get-all-keys (kbad ver)
          (handler-case
              (send cust (s-get-all-database-keys state ver))
            (error ()
              (send kbad))))
         
         (:save ()
          (send io-ser cust
                (lambda (cust)
                  (s-save-database cust state))
                ))
         
         (:revert (kbad)
          (let ((my-cust (α msg
                         (dcase msg
                           (:UNDEFINED _
                            (send kbad))
                           (t _
                              (repeat-send cust))
                           ))))
            (send io-ser my-cust
                  (lambda (cust)
                    (s-revert-database cust state)))))
         )))))
  
(defun make-remote-api-beh (kv-ser)
  ;; Gateway service using a RW Serializer
  (lambda (cust &rest msg)
    (um:dcase msg
      (:open ()
       ;; open a transaction - returns current version
       (send kv-ser cust :read :open))
    
      (:get-key (kbad ver key)
       (send kv-ser cust :read :get-key kbad ver key))
      
      (:get-keys (kbad ver keys)
       (send kv-ser cust :read :get-keys kbad ver keys))
      
      (:get-all-keys (kbad ver)
       (send kv-ser cust :read :get-all-keys kbad ver))
      
      (:commit (kbad ver adds dels)
       ;; update with eventual save
       (send kv-ser cust :write :commit kbad ver adds dels))
      
      (:save ()
       ;; save now
       (send kv-ser cust :write :save))
      
      (:revert (kbad)
       ;; revert now
       (send kv-ser cust :write :revert kbad))
      
      (:shutdown ()
       ;; orderly shutdown with save
       (unregister-actor self)
       (send kv-ser cust :write :save))
    
      (:kill ()
       ;; stop serving - database in indeterminate state
       (unregister-actor self)
       (send cust :killed))
      )))

;; ------------------------------------------------

(defun s-open-trans (state)
  ;; return a new trans
  (with-accessors ((main-table state-main-table)) state
    (main-table-ver main-table)))

(defun s-get-database-key (state ver key)
  (with-accessors ((main-table state-main-table)) state
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl     main-table-tbl)) main-table
      (cond ((uuid:uuid< ver tbl-ver)
             ;; nope - outdated, try again
             (error +rollback-exception+))
            
            (t
             (maps:find tbl key))
            ))))

(defun s-get-database-keys (state ver keys)
  (with-accessors ((main-table state-main-table)) state
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl     main-table-tbl)) main-table
      (cond ((uuid:uuid< ver tbl-ver)
             ;; nope - outdated, try again
             (error +rollback-exception+))
            
            (t
             (accum acc
               (mapc (lambda (key)
                       (acc (cons key
                                  (multiple-value-list
                                   (maps:find tbl key)))
                            ))
                     keys)))
            ))))

(defun s-get-all-database-keys (state ver)
  (with-accessors ((main-table state-main-table)) state
    (declare (main-table main-table))
    (with-accessors ((tbl-ver main-table-ver)
                     (tbl  main-table-tbl)) main-table
      (cond ((uuid:uuid< ver tbl-ver)
             ;; nope - outdated, try again
             (error +rollback-exception+))
            
            (t
             (accum acc
               (maps:iter tbl
                          (lambda (k v)
                            (declare (ignore v))
                            (acc k)))
               ))
            ))))

(defvar *writeback-delay*  10)

(defun s-commit-trans (state ver adds dels)
  ;; either update main table with trans and return a new ID, or
  ;; else signal a rollback error - client needs to start again
  ;; with a fresh :open-trans

  ;; commit changes, returning new version ID
  (with-accessors ((main-table  state-main-table)
                   (sync        state-sync)) state
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
             (mp:unschedule-timer sync)
             ;; the map adds, and the set dels, must alrady be using
             ;; normalized key reprs

             ;; first remove all overlapping keys before adding back
             ;; in new values, since we can't control which cell
             ;; gets planted in a union.
             (setf tbl-ver (new-ver)
                   tbl     (sets:union
                            (sets:diff
                             (sets:diff tbl dels)
                             adds)
                            adds))
             (mp:schedule-timer-relative (state-sync state) *writeback-delay*)
             tbl-ver)
            ))))

;; ----------------------------------------------------------------

(defun default-database-pathname ()
  (merge-pathnames "STKV-Persistent-Store.dat"
                   #-:LINUX
                   (sys:get-folder-path :documents)
                   #+:LINUX
                   #P"~/Documents/"))

;; -----------------------------

(defun s-revert-database (cust state)
  (with-accessors ((path  state-path)) state
    (if (probe-file path)
        (restart-case
            (with-open-file (f path
                               :direction :input
                               :element-type '(unsigned-byte 8))
              
              (optima:match (loenc:deserialize f
                                               :use-magic (magic-word "STKV"))
                ((list signature _ new-ver new-table) when (string= +stkv-signature+ signature)
                 (let ((new-main-table (make-main-table
                                        :tbl (lzw:decompress new-table)
                                        :ver new-ver
                                        :chk new-ver)))
                   (setf (state-main-table state) new-main-table)
                   (log-info :system-log
                             (format nil "Loaded STKV Store ~A:~A" path new-ver))
                   (send cust new-ver)))
                
                (_
                 (error "Not an STKV Persistent Store: ~A" path))
                ))
          (abort ()
            (send cust :UNDEFINED)))
      ;; else - no persistent copy, just reset to initial state
      (let ((new-tbl (make-main-table)))
        (setf (state-main-table state) new-tbl)
        (send cust (main-table-ver new-tbl))))
    ))

;; ---------------------------------

(defun s-save-database (cust state)
  (with-accessors ((main-table state-main-table)
                   (path       state-path)) state
    (declare (main-table main-table))
    (with-accessors ((ver   main-table-ver)
                     (chk   main-table-chk)
                     (tbl   main-table-tbl)) main-table
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
                             :use-magic (magic-word "STKV"))
                            (setf chk ver)
                            (send cust :OK))
                        (abort ()
                          (send cust :UNDEFINED))))
                    β)
            (dcase msg
              (:UNDEFINED _
               (repeat-send cust))
              (t _
                 (log-info :system-log
                           (format nil "Saved STKV Store ~A:~A" path ver))
                 (send cust :OK ver))
              ))))
      )))

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
            (let* ((tbl       (make-main-table))
                   (state     (make-initial-state
                               :path       path
                               :main-table tbl))
                   (kv-server (make-actor (make-kv-server-beh state)))
                   (kv-rwgate (rw-serializer kv-server))
                   (server    (make-actor (make-remote-api-beh kv-rwgate)))
                   (key       (namestring (truename path)))
                   (fwd       (α _
                                (maps:addf *stkv-servers* key server)
                                (send cust server))))
              (finalize-state state server)
              (if (probe-file path)
                  (send server fwd :revert (sink))
                (progn
                  ;; file doesn't exist - so create it
                  (setf (main-table-ver tbl) (new-ver))
                  (send server fwd :save)))
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
(β (act)
    (find-actor β :rstkv)
  (inspect act))

(let ((kbad (α ()
              (send println "Rollback Exception"))))
  (β (rstkv)
      (find-actor β :rstkv)
    (β (ver)
        (send rstkv β :open)
      (β (keys)
          (send rstkv β :get-all-keys kbad ver)
        (send println (mapcar #'loenc:decode keys))
        (β (data)
            (send rstkv β :get-key kbad ver (loenc:encode :pi))
          (send println (lzw:decompress data)))))))

|#



