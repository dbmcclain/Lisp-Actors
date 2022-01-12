;; transactional-db.lisp -- transactional database processing in Actors
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------
;; Using an FPL-pure RB-tree as the database. Hence the database
;; serves as its own transaction ID. Lock-free design.
;;
;; You can save anything you like in the KV database. But if it can't
;; be persisted then it won't be saved in the backing store, and won't
;; be restored in future sessions.
;;
;; Keys can be anything that has an order relation defined in ORD.
;;
;; If two persistable items are EQ when added to the KV database, they
;; remain EQ in future sessions.

(in-package com.ral.actors.kv-database)
  
(deflex trimmer
  (α (cust cmd db)
    (send cust sink cmd (remove-unstorable db))))

(defun trans-gate-beh (saver db)
  (alambda
   ;; -------------------
   ;; general entry for external clients
   ((cust :req)
    (send cust db))

   ;; -------------------
   ;; commit after update
   ((cust :commit old-db new-db retry)
    (cond ((eql old-db db) ;; make sure we have correct version
           (cond ((eql new-db db)
                  ;; no real change
                  (send cust new-db))
                 
                 (t
                  ;; changed db, so commit new
                  (let ((versioned-db (maps:add new-db 'version (uuid:make-v1-uuid))))
                    ;; version key is actually 'com.ral.actors.kv-database::version
                    (become (trans-gate-beh saver versioned-db))
                    (send-after 10 self saver versioned-db)
                    (send cust versioned-db)))
                 ))
          
          (t
           ;; had wrong version for old-db
           (send retry db))
          ))
     
   ;; -------------------
   ;; We are the only one that knows the identity of saver, so this
   ;; can't be forged by malicious clients. Also, a-db will only
   ;; eql db if there have been no updates within the last 10 sec.
   ((a-tag a-db) / (and (eql a-tag saver)
                        (eql a-db  db))
    (send trimmer saver :save-log db))
   
   ;; -------------------
   (('maint-full-save)
    (send trimmer saver :full-save db))
   ))

(defun nascent-database-beh (tag saver msgs)
  (alambda
   ;; -------------------
   ;; We are the only one that knows the identity of tag and saver. So
   ;; this message could not have come from anywhere except saver
   ;; itself.
   ((a-tag :opened db) / (eql a-tag tag)
    (become (trans-gate-beh saver db))
    ;; now open for business, resubmit pending client requests
    (dolist (msg msgs)
      (send* self msg)))
   
   ;; -------------------
   ;; accumulate client requests until we open for business
   (msg
    (become (nascent-database-beh tag saver (cons msg msgs) )))
   ))

;; -----------------------------------------------------------

(defconstant +db-id+  #/uuid/{6f896744-6472-11ec-8ecb-24f67702cdaa})

(defun save-database-beh (path last-db)
  (alambda
   ;; -------------------
   ((cust :full-save db)
    (become (save-database-beh path db))
    (full-save path db)
    (send cust :ok))

   ;; -------------------
   ;; The db gateway is the only one that knows saver's identity.
   ;; Don't bother doing anything unless the db has changed.
   ((cust :save-log new-db) / (not (eql new-db last-db))
    (handler-case
        (with-open-file (f path
                           :direction         :output
                           :if-exists         :append
                           :if-does-not-exist :error
                           :element-type      '(unsigned-byte 8))
          (let ((delta (get-diffs last-db new-db)))
            (loenc:serialize delta f
                             :self-sync t)
            ))
        (error ()
          (full-save path new-db)))
    (become (save-database-beh path new-db))
    (send cust :ok))
   ))

(defun unopened-database-beh ()
  (alambda
   ;; -------------------
   ;; message from kick-off starter routine
   ((cust :open db-path)
    (let ((db (maps:empty)))
      (handler-case
        (with-open-file (f db-path
                           :direction         :input
                           :if-does-not-exist :error
                           :element-type      '(unsigned-byte 8))
          (let* ((sig  (uuid:uuid-to-byte-array +db-id+))
                 (id   (make-array (length sig)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
            (read-sequence id f)
            (cond ((equalp id sig)
                   (setf db (loenc:deserialize f))
                   (let ((reader (self-sync:make-reader f)))
                     (handler-case
                         (loop for ans = (loenc:deserialize f
                                                            :self-sync  reader)
                               until (eq ans f)
                               do
                                 (destructuring-bind (removals additions changes) ans
                                   (dolist (key removals)
                                     (maps:removef db key))
                                   (dolist (pair additions)
                                     (destructuring-bind (key . val) pair
                                       (maps:addf db key val)))
                                   (dolist (pair changes)
                                     (destructuring-bind (key . val) pair
                                       (maps:addf db key val)))
                                   ))
                       (error (exn)
                         (send println (um:format-error exn)))
                       )))
                  (t
                   (error "Not a db file"))
                  )))
        (error ()
          (full-save db-path (maps:empty))))
      (become (save-database-beh db-path db))
      (send cust :opened db)))
   ))

(defun full-save (db-path db)
  (ensure-directories-exist db-path)
  (with-open-file (f db-path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (let ((sig (uuid:uuid-to-byte-array +db-id+)))
      (write-sequence sig f)
      (loenc:serialize db f))
    ))

(defun remove-unstorable (map)
  (maps:fold map (λ (key val accu)
                   (handler-case
                       (progn
                         ;; this will barf if either key or val is unstorable
                         (loenc:encode (list key val))
                         (maps:add accu key val))
                     (error ()
                       accu)))
             (maps:empty)))

(defun deep-copy (obj)
  (loenc:decode (loenc:encode obj)))

(defun get-diffs (old-db new-db)
  (let* ((removals  (mapcar 'maps:map-cell-key
                            (sets:elements
                             (sets:diff old-db new-db))))
         (additions (maps:fold (sets:diff new-db old-db) 'acons nil))
         (changes   (maps:fold new-db
                               (λ (k v accu)
                                 (let ((old-val (maps:find old-db k new-db)))
                                   (cond ((eql old-val new-db) accu) ;; missing entry
                                         ((eql old-val v) accu)
                                         (t (acons k v accu))
                                         )))
                               nil))
         (log       (list
                     removals
                     (nreverse additions)
                     (nreverse changes))))
    (send writeln log)
    log))

;; -----------------------------------------------------------

(defun db-svc-init (path)
  (α _
    (let ((tag   (tag self))
          (saver (serializer (make-actor (unopened-database-beh)))))
      (send saver tag :open path)
      (become (nascent-database-beh tag saver nil))
      (repeat-send self))))

(defvar *db-path*  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                                    (sys:get-folder-path :appdata)))

(deflex dbmgr
  (db-svc-init *db-path*))

;; -----------------------------------------------------------

(defun add-rec (cust key val)
  (β (db)
      (send dbmgr β :req)
    (send dbmgr cust :commit db (maps:add db key val) self)
    ))

(defun remove-rec (cust key)
  (β (db)
      (send dbmgr β :req)
    (let* ((val  (maps:find db key self))
           (new-db (if (eql val self)
                       db
                     (maps:remove db key))))
      (send dbmgr cust :commit db new-db self)
      )))

(defun lookup (cust key &optional default)
  (β (db)
      (send dbmgr β :req)
    (send cust (maps:find db key default))
    ))

(defun show-db ()
  (β (db)
      (send dbmgr β :req)
    (sets:view-set db)))

(defun maint-full-save ()
  (send dbmgr 'maint-full-save))

;; ------------------------------------------------------------------
;; more usable public face - can use ASK against this

(deflex kvdb
  (make-actor
   (alambda
    ((cust :lookup key . default)
     (apply 'lookup cust key default))
    
    ((cust :add key val)
     (add-rec cust key val))
    
    ((cust :remove key)
     (remove-rec cust key))
    
    ((cust :req)
     (repeat-send dbmgr))

    ((cust :commit old-db new-db retry)
     (repeat-send dbmgr))
    )))

;; -----------------------------------------------------------
#|
(ask kvdb :lookup :dave)
(uuid:when-created (ask kvdb :lookup 'version))

(dotimes (ix 5)
  (add-rec println ix ix))
(add-rec println :dave :chara)
(add-rec println :cat "dog")
(lookup writeln :cat)
(show-db)
(dotimes (ix 10)
  (remove-rec println ix))
(add-rec println :tst (λ* _))
(lookup writeln :tst)

(let ((m (maps:empty)))
  (setf m (maps:add m :dave :dog))
  (eql m (maps:add m :dave :dog)))
(let ((m (sets:empty)))
  (setf m (sets:add m :dave))
  (eql m (sets:add m :dave)))

(β (db)
    (send dbmgr β :req)
  (maps:iter db (λ (k v)
                  (send writeln (list k v)))))

(let ((x '(1 2 3)))
  (send kvdb println :add :tst1 x)
  (send kvdb println :add :tst2 x))

(let ((x1 (ask kvdb :lookup :tst1))
      (x2 (ask kvdb :lookup :tst2)))
  (send println (list x1 x2 (eq x1 x2))))


(maint-full-save)

;; ------------------------------------------------------
(with-open-file (f "~/junk.tst"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
  (dotimes (ix 10)
    (let ((str (format nil "This is test ~D!" ix)))
      (loenc:serialize str f
                       :self-sync t))))

;; Now go ahead and trash the file - see that we skip the damaged
;; records, often two adjacent records, but manage to pick up the
;; following ones just fine. Self-sync is great!!
;;
;; Even though we are encoded with LOENC:ENCODE, the trashed contents
;; don't generate an exception. We just silently skip the trashed
;; records and pick up what we can.

(with-open-file (f "~/junk.tst"
                     :direction :input
                     :if-does-not-exist :error
                     :element-type '(unsigned-byte 8))
  (let ((reader (self-sync:make-reader f)))
    (loop for ans = (loenc:deserialize f :self-sync reader)
            until (eq ans f)
            collect ans)))

|#               
