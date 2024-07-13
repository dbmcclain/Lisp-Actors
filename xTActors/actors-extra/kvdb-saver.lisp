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
                ((db-get-keys new-db)
                 (db-get-keys old-db)))
            (:par (removals additions common-keys)
                ((set-difference old-keys new-keys :test #'equal)
                 (let ((adds (set-difference new-keys old-keys :test #'equal)))
                   (pairlis adds (mapcar (um:curry #'db-find new-db) adds)))
                 (intersection new-keys old-keys :test #'equal)) ))
         (send (create
                (lambda (keys &optional changes)
                  (if keys
                      (let+ ((me  self)
                             ((key . tl) keys)
                             (:par (new-val old-val)
                                 ((db-find new-db key)
                                  (db-find old-db key))))
                        (send me tl
                              (if (eql new-val old-val)
                                  changes
                                (acons key new-val changes))))
                    ;; else
                    (send cust (list removals
                                     additions
                                     changes))
                    )))
               common-keys)
         ))
   ))

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
  ;; -------------------
  (alambda
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

(um:defconstant+ +db-id+  #/uuid/{6f896744-6472-11ec-8ecb-24f67702cdaa})

(defun check-kvdb-sig (fd dbpath)
  (let* ((sig  (uuid:uuid-to-byte-array +db-id+))
         (id   (vec-repr:make-ub8-vector (length sig))))
    (file-position fd 0)
    (read-sequence id fd)
    (or (equalp id sig)
        (error 'not-a-kvdb :path dbpath))
    ))

(defun unopened-database-beh (ctrl-tag)
  (alambda
   ;; -------------------
   ;; message from kick-off starter routine
   ((cust :open db-path)
    (block udb
      (handler-bind
          ;; we are behind a Serializer, so must reply to cust
          ((error (lambda (err)
                    (abort-beh) ;; clear out all Send & Become
                    (become-sink)
                    (send ctrl-tag :remove-entry) ;; tell orchestrator
                    (send cust err)
                    (return-from udb))
                  ))
        (labels ((normal-exit (db)
                   (become (save-database-beh db-path db ctrl-tag))
                   (send cust :opened db)
                   (return-from udb))
                 
                 (prep-and-save-db (db)
                   (let* ((ver (db-find db 'version))
                          (dbv (if (typep ver 'uuid:uuid)
                                   db
                                 (db-add db 'version (uuid:make-v1-uuid))))
                          (seq  (db-find dbv 'kvdb-sequence))
                          (dbs  (if (typep seq 'uuid:uuid)
                                    dbv
                                  (db-add dbv 'kvdb-sequence (uuid:make-v1-uuid)))))
                     (normal-exit (full-save db-path dbs ctrl-tag))))
                 
                 (prep-and-save-new-db ()
                   (prep-and-save-db (db-new)))

                 (try-deserialize-db (f)
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
                   (handler-case
                       (let ((reader (self-sync:make-reader f)))
                         (loop for ans = (loenc:deserialize f :self-sync reader)
                               until   (eq ans f)
                               finally (normal-exit db)
                               do
                                 (destructuring-bind (removals additions changes) ans
                                   (dolist (key removals)
                                     (setf db (db-remove db key)))
                                   (dolist (pair additions)
                                     (destructuring-bind (key . val) pair
                                       (setf db (db-add db key val))))
                                   (dolist (pair changes)
                                     (destructuring-bind (key . val) pair
                                       (setf db (db-add db key val))))
                                   )))
                     (error ()
                       ;; happens when can't deserialize properly
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

#-:MSWINDOWS
(defun ino-key (path)
  (multiple-value-bind (dev ino)
      (um:get-ino path)
    (with-standard-io-syntax
      (um:mkstr dev #\: ino))))

#+:MSWINDOWS
(defun ino-key (path)
  (namestring (truename path)))


