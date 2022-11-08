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
;;
;; ----------------------------------------------------------------------
;; Notes about Concurrent (Parallel and Single-Threaded) Execution...
;;
;; Under the current Actors system we have concurrency and parallelism
;; with multicore SMP. It is entirely possible for two tasks to be
;; running in the same code simultaneously, both logically and
;; physically.
;;
;; But even if we restrict the execution to a single machine thread,
;; which dispenses with SMP parallelism, we still have concurrency.
;; Two or more separate logical threads of execution can be running,
;; logically simultaneously, against the same code.
;;
;; And, any time you have concurrency, you run the risk of data race
;; conditions between separate threads of execution.
;;
;; Great! You say. I'll just make sure I write FPL-pure code. Well,
;; for SMP parallel code, in the absence of using locks, that is
;; certainly a minimum requirement. But that may not be enough.
;;
;; Imagine two threads (parallel or not) which share a reference to an
;; Actor whose state may evolve in response to messsages sent to it.
;; The Actor is written in perectly FPL-pure fashion; it never mutates
;; its state variables, and only uses BECOME to evolve to a new state.
;; Yet, outside observers only see that indeed the state has mutated.
;; They can't actually observe the internal state, but they can
;; observe a change in behavior.
;;
;; So if there is any temporal separation between the act of reading
;; state, and modifying state, we open ourselves up to potential data
;; race conditions. Here, time can be measured by the number of
;; message dispatches activating a particular Actor, that occur
;; between two positions in time. To appear logically atomic, a state
;; change must occur entirely within one activation of an Actor.
;;
;; In such case, the CAS semantics on BECOME will be enough to protect
;; against data races. But if there is temporal separation between
;; reading state, and writing state, as measured in separate Actor
;; activations, then the CAS semantics will only retry the final
;; BECOME mutation. That could leave us inconsistent. We actually need
;; to retry from the point in time of initially reading the state.
;;
;; So if, e.g., we have a system where the shared Actor is queried in
;; one message, the answer is viewed, and then an update message is
;; sent, that occupies at least two message dispatches and cannot be
;; seen as atomic. Another thread could sneak in there with the same
;; intention, but with different purpose, and interfere with the first
;; thread's plans. The result would be a system inconsistency.
;;
;; So not only does the code have to be written in FPL-pure fashion,
;; any state changes must appear logically atomic to all outside
;; observers. And the only way to effect that, in the case of
;; temporally separated read / mutate, is to halt concurrent activity
;; during that interrim period, within the mutating Actor.
;;
;; That is the purpose of SERIALIZER. It permits only one thread of
;; execution at a time to proceeed beyond to the Actor subsystem under
;; its control. All others are enqueued for execution only after a
;; response is seen from the Actor subsystem, a reply back to the
;; currently executing thread of execution.
;;
;; All client code must provide a customer to which a response will be
;; sent, even if only SINK. And the Actor system under control of the
;; SERIALIZER must send a reply to customers under all circumstances.
;; This is necessary so that the SERIALIZER can interpose between
;; client and Actor and know on the way out to enable the next waiting
;; thread of execution against the Actor subsystem.
;;
;; Subtle bugs can seep into the code if you don't pay careful
;; attention to the possibility of data race conditions.
;;
;; More obvious cases requiring only a single thread of execution
;; would be situations in which a physical resource cannot be
;; reasonably shared in parallel. E.g. file I/O. So again, a
;; SERIALIZER is called for.
;;
;; But all that aside, a SERIALIZER is only needed in one place in
;; this code - at the very opening stage of the database on disk. Only
;; one thread can be permitted to open and deserialize the initial
;; database.  Thereafter, all of the code supports fully parallel
;; concurrency.
;;
;; But if the database has changed underneath a mutation request
;; (ADD/REMOVE), then the operation must be logically retried.
;; Changing the value for any particular key is accomplished by
;; ADD'ing the key-value pair, which overwrites the previous value.
;; ----------------------------------------------------------------------

(defpackage com.ral.actors.kv-database
  (:use #:cl :com.ral.actors)
  (:export
   #:kvdb
  ))
   
(in-package com.ral.actors.kv-database)

;; ----------------------------------------------------------------------

(def-beh trans-gate-beh (saver db)
  ;; -------------------
  ;; general entry for external clients
  ((cust :req)
   (send cust db))

  ;; -------------------
  ;; commit after update
  (( (cust . retry) :commit old-db new-db)
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
   (send saver sink :save-log db))
   
  ;; -------------------
  (('maint-full-save)
   (send saver sink :full-save db)))

;; ----------------------------------------------------------------

(def-beh nascent-database-beh (tag saver msgs)
  ;; -------------------
  ;; We are the only one that knows the identity of tag and saver. So
  ;; this message could not have come from anywhere except saver
  ;; itself.
  ((a-tag :opened db) / (eql a-tag tag)
   (become (trans-gate-beh saver db))
   ;; now open for business, resubmit pending client requests
   (send-all-to self msgs))
   
  ;; -------------------
  ;; accumulate client requests until we open for business
  (msg
   (become (nascent-database-beh tag saver (cons msg msgs) ))))

;; -----------------------------------------------------------

(defconstant +db-id+  #/uuid/{6f896744-6472-11ec-8ecb-24f67702cdaa})

(def-ser-beh save-database-beh (path last-db)
  ;; -------------------
  ((cust :full-save db)
   (let ((savdb (full-save path db)))
     (become (save-database-beh path savdb))
     (send cust :ok)))

  ;; -------------------
  ;; The db gateway is the only one that knows saver's identity.
  ;; Don't bother doing anything unless the db has changed.
  ((cust :save-log new-db)
   (let ((new-ver  (maps:find new-db  'version))
         (prev-ver (maps:find last-db 'version)))
     (when (uuid:uuid-time< prev-ver new-ver)
       (let* ((savdb (persistable new-db))
              (delta (get-diffs last-db savdb)))
         (handler-case
             (with-open-file (f path
                                :direction         :output
                                :if-exists         :append
                                :if-does-not-exist :error
                                :element-type      '(unsigned-byte 8))
               (loenc:serialize delta f
                                :self-sync t))
           (error ()
             ;; expected possible error due to file not existing yet
             (full-save path savdb)))
         (become (save-database-beh path savdb))))
       (send cust :ok))))
  
;; ---------------------------------------------------------------

(def-ser-beh unopened-database-beh ()
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
         (setf db (maps:add (maps:empty) 'version (uuid:make-v1-uuid)))
         (full-save db-path db)))
     (become (save-database-beh db-path db))
     (send cust :opened db))))

;; --------------------------------------------------------------------

(defun try-encoding (k v)
  ;; this will bomb if either k or v is not persistable
  (loenc:encode (list k v)))

(defun filter-unpersistable (k v acc)
  (handler-case
      (progn
        (try-encoding k v)
        (maps:add acc k v))
    (error ()
      acc)))

(defun persistable (db)
  (handler-case
      (progn
        (maps:iter db #'try-encoding)
        db)
    (error ()
      (maps:fold db #'filter-unpersistable (maps:empty)))
    ))

(defun full-save (db-path db)
  (let ((savdb  (persistable db)))
    (ensure-directories-exist db-path)
    (with-open-file (f db-path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      (let ((sig (uuid:uuid-to-byte-array +db-id+)))
        (write-sequence sig f)
        (loenc:serialize savdb f)
        ))
    savdb))

(defun get-diffs (old-db new-db)
  (let* ((removals  (mapcar #'maps:map-cell-key
                            (sets:elements
                             (sets:diff old-db new-db))))
         (additions (maps:fold (sets:diff new-db old-db) #'acons nil))
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
    ;; (send writeln log)
    log))

;; -----------------------------------------------------------

(defun db-svc-init-beh (path)
  (λ _
    (let ((tag   (tag self))
          (saver (serializer (create (unopened-database-beh)))))
      (send saver tag :open path)
      (become (nascent-database-beh tag saver nil))
      (repeat-send self))))

(defvar *db-path*  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                                    (sys:get-folder-path :appdata)))

(deflex dbmgr
  (create (db-svc-init-beh *db-path*)))

;; -----------------------------------------------------------

(defun do-with-db (fn)
  (β (db)
      (send dbmgr β :req)
    (funcall fn db)))

(defmacro with-db (db &body body)
  `(do-with-db (lambda (,db) ,@body)))

(defun add-rec (cust key val)
  (with-db db
    (send dbmgr `(,cust . ,self) :commit db (maps:add db key val))
    ))

(defun remove-rec (cust key)
  (with-db db
    (let* ((val    (maps:find db key self))
           (new-db (if (eql val self)
                       db
                     (maps:remove db key))))
      (send dbmgr `(,cust . ,self) :commit db new-db))
    ))

(defun lookup (cust key &optional default)
  (with-db db
    (send cust (maps:find db key default))
    ))

(defun show-db ()
  (with-db db
    (sets:view-set db)))

(defun maint-full-save ()
  (send dbmgr 'maint-full-save))

;; ------------------------------------------------------------------
;; more usable public face - can use ASK against this

(deflex kvdb
  (create
   (alambda
    ((cust :lookup key . default)
     (apply 'lookup cust key default))
    
    ((cust :add key val)
     (add-rec cust key val))
    
    ((cust :remove key)
     (remove-rec cust key))
    
    ((cust :req)
     (repeat-send dbmgr))
    
    (( (cust . retry) :commit old-db new-db)
     (repeat-send dbmgr))
    )))

;; -----------------------------------------------------------
#|
(ask kvdb :lookup :dave)
(uuid:when-created (ask kvdb :lookup 'version))
(show-db)

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
