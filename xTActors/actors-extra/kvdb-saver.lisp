;; kvdb-saver.lisp -- The actual File I/O stuff
;; Open databse file, save database file
;;
;; DM/RAL  2024/07/13 18:04:55 UTC
;; -----------------------------------------------------------

(in-package #:com.ral.actors.kvdb)

;; -----------------------------------------------------------

#|
(defun get-diffs (old-db new-db)
  (let* ((new-wrk   (db-rebuild new-db))
         (old-keys  (db-get-keys old-db))
         (new-keys  (db-get-keys new-wrk))
         ;; keys in old but absent from new
         (removals  (set-difference old-keys new-keys :test #'equal))
         ;; keys in new but absent from old
         (additions (mapcan #'identity
                            (mapcar (lambda (k)
                                      (list (cons k (db-find new-wrk k))))
                                    (set-difference new-keys old-keys  :test #'equal))))
         ;; value changes among common keys
         (changes   (mapcan #'identity
                            (mapcar (lambda (k)
                                      (let ((old-val (db-find old-db k new-wrk))
                                            (new-val (db-find new-wrk k)))
                                        (cond ((eq old-val new-wrk)  (break) nil)   ;; missing from old (?? - should never happen)
                                              ((eql old-val new-val) nil)   ;; unchanged
                                              (t  (list (cons k new-val)))  ;; changed
                                        )))
                                    (set-difference new-keys (mapcar #'car additions) :test #'equal))
                            ))
         (log       (list
                     removals
                     additions
                     changes)))
    ;; (send writeln log)
    log))

(defun save-database-beh (path last-db ctrl-tag)
  ;; -------------------
  (alambda
   ((cust :full-save db)
    (let ((savdb (full-save path db ctrl-tag)))
      (become (save-database-beh path savdb ctrl-tag))
      (send cust :ok)))
   
   ;; -------------------
   ;; The db gateway is the only one that knows saver's identity.
   ;; Don't bother doing anything unless the db has changed.
   ((cust :save-log new-db)
    (handler-case
        (let ((new-ver  (db-find new-db  'version))
              (prev-ver (db-find last-db 'version)))
          (when (uuid:uuid-time< prev-ver new-ver)
            (let* ((delta (get-diffs last-db new-db)))
              (with-open-file (f path
                                 :direction         :output
                                 :if-exists         :append
                                 :if-does-not-exist :error
                                 :element-type      '(unsigned-byte 8))
                (loenc:serialize delta f
                                 :max-portability t
                                 :self-sync t))
              (become (save-database-beh path new-db ctrl-tag))
              (send fmt-println "Saved KVDB Deltas: ~S" path)
              (send cust :ok)
              )))
      (error ()
        ;; expected possible error due to file not existing yet
        ;; or from non-existent version in prev-ver
        (send self cust :full-save new-db))
      ))
   ))
|#
;; -----------------------------------------------------

(deflex db-differencer
  ;; An Actor that computes and sends to cust, the list of removals,
  ;; additions, and changes, from old-db to new-db.
  (create
   (lambda (cust old-db new-db)
     (let+ ((:par (new-keys old-keys)
                ((db-get-keys new-db)   ;; new keys
                 (db-get-keys old-db))) ;; old keys
            (:par (removals additions common-keys)
                ((set-difference old-keys new-keys :test #'equal)               ;; removals
                 (let ((adds (set-difference new-keys old-keys :test #'equal))) ;; additions
                   (pairlis adds (mapcar (um:curry #'db-find new-db) adds)))
                 (intersection new-keys old-keys :test #'equal)) ))             ;; common keys
         (send (create
                (behav
                 (lambda (keys &optional changes)
                   ;; compute list of changes
                   (if keys
                       (let+ ((me  self)
                              ((key . tl) keys)
                              (:par (new-val old-val)
                                  ((db-find new-db key)     ;; new val
                                   (db-find old-db key))))  ;; old val
                         (send me tl
                               (if (eql new-val old-val)
                                   changes
                                 (acons key new-val changes))))
                     ;; else
                     (send cust (list removals
                                      additions
                                      changes))
                     ))))
               common-keys)
         ))
   ))

;; ---------------------------------------------------------------
;; The unique KVDB Signature at the front of every valid KVDB backing store.

(um:defconstant+ +db-id+  #/uuid/{6f896744-6472-11ec-8ecb-24f67702cdaa})

;; --------------------------------------------------------------------

(defun full-save (db-path db ctrl-tag)
  (ensure-directories-exist db-path)
  (let ((sav-db (db-rebuild db)))
    (with-open-file (f db-path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :rename
                       :element-type '(unsigned-byte 8))
      (let ((sig (uuid:uuid-to-byte-array +db-id+)))
        (write-sequence sig f)
        (loenc:serialize sav-db f
                         :max-portability t)
        ))
    (send fmt-println "Saved full KVDB: ~S" db-path)
    (send ctrl-tag :update-entry) ;; inode has changed
    sav-db))

;; -----------------------------------------------------

(defun save-database-beh (path last-db ctrl-tag)
  ;; The steady state of the SAVER. Await messages to either store a
  ;; full image on backing store, or just the deltas.
  ;; -------------------
  (alambda
   ((cust :retry-open)
    ;; Might happen when a file problem was encountered and user had
    ;; to respond to a Y-or-N query, meanwhile the Serializer timed
    ;; out.  If we made it to this state, then everything is happy and
    ;; we simply need to reply with our copy of the kvdb.
    ;;
    ;; If we didn't make it to this state, we will have become a CONST
    ;; with the error code.
    (send cust :opened last-db))
    
   ((cust :full-save db)
    (let ((savdb (full-save path db ctrl-tag)))
      (become (save-database-beh path savdb ctrl-tag))
      (send cust :ok)))
   
   ((cust :become intro new-db) / (eq intro self)
    (become (save-database-beh path new-db ctrl-tag))
    (send cust :ok))
   
   ;; -------------------
   ;; The db gateway is the only one that knows saver's identity.
   ;; Don't bother doing anything unless the db has changed.
   ;;
   ((cust :save-log new-db)
    (let+ ((me self)
           (:fn recover ()
            (send me cust :full-save new-db))
           (:par (new-ver prev-ver)
               ((db-find new-db 'version)
                (db-find last-db 'version))) )
      (handler-case
          (when (uuid:uuid-time< prev-ver new-ver)
            (β (delta)
                (send db-differencer β last-db new-db)
              (handler-case
                  (progn
                    (with-open-file (f path
                                       :direction         :output
                                       :if-exists         :append
                                       :if-does-not-exist :error
                                       :element-type      '(unsigned-byte 8))
                      (loenc:serialize delta f
                                       :max-portability t
                                       :self-sync t))
                    (send me cust :become me new-db)
                    (send fmt-println "Saved KVDB Deltas: ~S" path))
                (error ()
                  ;; possible error because file not existing yet
                  (recover))
                )))
        (error ()
          ;; possible error from non-existent prev-ver
          (recover))
        )))
   ))
  
;; ---------------------------------------------------------------

(define-condition not-a-kvdb (error)
  ((path :accessor not-a-kvdb-path :initarg :path))
  (:report (lambda (cx stream)
             (format stream "Not a KVDB file: ~S" (not-a-kvdb-path cx)))
   ))

(defgeneric not-a-kvdb-p (err)
  (:method (err)
   nil)
  (:method ((err not-a-kvdb))
   t))

;; ---------------------------------------------------------------

(define-condition corrupt-deltas (error)
  ((path :accessor corrupt-deltas-path :initarg :path))
  (:report (lambda (cx stream)
             (format stream "Corrupt DeltaList in KVDB: ~S" (corrupt-deltas-path cx)))
   ))

(defgeneric corrupt-deltas-p (err)
  (:method (err)
   nil)
  (:method ((err corrupt-deltas))
   t))

;; ---------------------------------------------------------------

(define-condition corrupt-kvdb (error)
  ((path :accessor corrupt-kvdb-path :initarg :path))
  (:report (lambda (cx stream)
             (format stream "Corrupt KVDB: ~S" (corrupt-kvdb-path cx)))
   ))

(defgeneric corrupt-kvdb-p (err)
  (:method (err)
   nil)
  (:method ((err corrupt-kvdb))
   t))

(defgeneric file-error-p (err)
  (:method (err)
   nil)
  (:method ((err file-error))
   t))

;; ---------------------------------------------------------------

(defun check-kvdb-sig (fd dbpath)
  ;; Verify that we really have a KVDB signature at the front of the
  ;; file.
  (let* ((sig  (uuid:uuid-to-byte-array +db-id+))
         (id   (vec-repr:make-ub8-vector (length sig))))
    (file-position fd 0)
    (read-sequence id fd)
    (or (equalp id sig)
        (error 'not-a-kvdb :path dbpath))
    ))

;; ----------------------------------------------------------------

(defun unopened-database-beh (ctrl-tag)
  ;; The starting state of the SAVER. Await a message to Open the KVDB
  ;; on backing store, set up the in-memory KVDB, then become a real
  ;; Saver which computes and saves the deltas on demand from the KVDB
  ;; manager.
  (alambda
   ;; -------------------
   ;; message from kick-off starter routine
   ((cust :open db-path)
    (block udb
      (handler-bind
          ;; we are behind a Serializer, so must reply to cust
          ((error (lambda (err)
                    (abort-beh) ;; clear out all Send & Become
                    (become (const-beh :error err))
                    (send ctrl-tag :remove-entry) ;; tell Orchestrator
                    (send cust :error err)        ;; tell KVDB Manager
                    (return-from udb)) ;; being an Actor, the return value is irrelevant and ignored.
                  ))
        (labels ((normal-exit (db)
                   ;; Become a SAVER Actor and tell the KVDB manager
                   ;; that we are now open for business, and provide
                   ;; the in-memory KVDB.
                   (become (save-database-beh db-path db ctrl-tag))
                   (send cust :opened db)
                   (return-from udb)) ;; return value irrelevant
                 
                 (prep-and-save-db (db)
                   ;; Called only when recovering from file errors of
                   ;; various kinds.
                   (let* ((dbv  (db-add db 'version (uuid:make-v1-uuid))) ;; update its version
                          (seq  (db-find dbv 'kvdb-sequence))             ;; ensure it has a kvdb-sequence
                          (dbs  (if (typep seq 'uuid:uuid)
                                    dbv
                                  (db-add dbv 'kvdb-sequence (uuid:make-v1-uuid)))))
                     (normal-exit (full-save db-path dbs ctrl-tag))))
                 
                 (prep-and-save-new-db ()
                   ;; Called when error recovery requires the nuclear
                   ;; option. A totally empty database results.
                   (prep-and-save-db (db-new)))
                 
                 (try-deserialize-db (f)
                   ;; read in the core of the database
                   (handler-case
                       (let ((db  (loenc:deserialize f)))
                         (db-find db 'version) ;; try to tickle error
                         db)
                     (error ()
                       ;; For cases:
                       ;;  1. We cannot deserialize properly, or
                       ;;  2. Not a usable database upon deserialization
                       (if (yes-or-no-p
                            "Corrupt KVDB: ~S. Rebuild?"
                            db-path)
                           (prep-and-save-new-db)
                         ;; else
                         (error 'corrupt-kvdb :path db-path)))
                     ))
                 
                 (apply-deltas (f db)
                   ;; Apply the historical log of deltas to the base
                   ;; database. The log is stored as successive
                   ;; records in self-sync form. Each record contains
                   ;; a list of removals, additions, and changes.
                   (handler-case
                       (let ((reader (self-sync:make-reader f)))
                         (loop for ans = (loenc:deserialize f :self-sync reader)
                               until   (eq ans f)
                               finally (normal-exit db)
                               do
                                 (let+ (( (removals additions changes) ans)
                                        (:fn add (pairs)
                                         (dolist (pair pairs)
                                           (let+ (( (key . val) pair))
                                             (setf db (db-add db key val))
                                             ))))
                                   (dolist (key removals)
                                     (setf db (db-remove db key)))
                                   (add additions)
                                   (add changes)
                                   )))
                     (error ()
                       ;; Happens when can't deserialize properly.
                       ;;
                       ;; Under normal circumstances, the self-sync
                       ;; encoding should just skip over damaged
                       ;; sections of the log and continue applying
                       ;; from that point forward. The database could
                       ;; be somewhat inconsistent if that happens.
                       ;;
                       ;; We should, however, never encounter this
                       ;; error which could only arise if a log record
                       ;; does not contain a list of removals,
                       ;; additions, and changes.
                       ;;
                       (if (yes-or-no-p
                            "Corrupt deltas encountered: ~S. Rebuild?"
                            db-path)
                           (prep-and-save-db db)
                         ;; else
                         (error 'corrupt-deltas :path db-path)) )
                     )))
          
          (handler-case
              (with-open-file (f db-path
                                 :direction         :input
                                 :if-does-not-exist :error
                                 :element-type      '(unsigned-byte 8))
                (check-kvdb-sig f db-path)
                (apply-deltas f  (try-deserialize-db f)))
            
            ;; Under the Orchestrator these errors should never occur.
            (file-error (err)
              (if (yes-or-no-p
                   "File does not exist: ~S. Create?"
                   (file-error-pathname err))
                  (prep-and-save-new-db)
                ;; else
                (error err)))
            
            (not-a-kvdb (err)
              (if (yes-or-no-p
                   "Not a KVDB file: ~S. Rename existing and create new?"
                   (not-a-kvdb-path err))
                  (prep-and-save-new-db)
                ;; else
                (error err)))
            )))))
   ))
  
;; ---------------------------------------------------------------

(defun ensure-file-exists (path)
  ;; Called by the Orchestrator to ensure that there is a valid
  ;; DEVICE+INODE identification for the backing store of a KVDB.
  (flet ((init-kvdb ()
           (let ((db (db-new)))
             ;; version is updated with each change to the contents
             ;; kvdb-sequence is a unique ID permanently associated with the database
             (setf db (db-add db 'version       (uuid:make-v1-uuid)))
             (setf db (db-add db 'kvdb-sequence (uuid:make-v1-uuid)))
             (full-save path db nil)
             )))
      (restart-case
          (handler-bind
              ((file-error (lambda (c)
                             (invoke-restart (find-restart 'create c) c)))
               (not-a-kvdb (lambda (c)
                             (invoke-restart (find-restart 'overwrite c) c))))
            (with-open-file (fd path
                                :direction :input
                                :element-type '(unsigned-byte 8)
                                :if-does-not-exist :error)
              (check-kvdb-sig fd path)
              ))
        ;; restarts
        (create (err)
          :test file-error-p
          (if (yes-or-no-p
               "Create file: ~S?" path)
              (init-kvdb)
            (error err)))
        (overwrite (err)
          :test not-a-kvdb-p
          (if (yes-or-no-p
               "Rename existing and create new file: ~S?" path)
              (init-kvdb)
            (error err)))
        )))

;; --------------------------------------------------------------
;; Unique file identification by DEVICE+INODE

#-:MSWINDOWS
(defun ino-key (path)
  (multiple-value-bind (dev ino)
      (um:get-ino path)
    (with-standard-io-syntax
      (um:mkstr dev #\: ino))))

#+:MSWINDOWS
(defun ino-key (path)
  (namestring (truename path)))


