;; kvdb-ht.lisp -- transactional Key-Value database processing in Actors
;;
;; Based on FPL Hashtables. This version just might be faster than
;; using RB-Trees, since we no longer have the overhead requirement of
;; key orderability with ORD. But we do have key normalization, and as
;; a result, all keys would be orderable.
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------

(defpackage #:com.ral.actors.kvdb
  (:use #:cl #:com.ral.actors)
  (:export
   #:kvdb
   #:kvdb-maker
  ))
   
(in-package #:com.ral.actors.kvdb)

#| ----------------------------------------------------------------------
   Using an FPL-pure Hashtable as the database. Hence the database
   serves as its own transaction ID. Lock-free design.
  
   You can save anything you like in the KV database. But if it can't
   be liberally persisted then it won't be faithfully saved in the
   backing store, and won't be properly restored in future sessions.
  
   [ Liberal Persistence - when an object is encountered that can't be
   usefully persisted, a substitution is made using a proxy object, to
   allow the dataset as a whole to be persisted. However, on
   read-back, those proxy items will likely fail your needs. ]
  
   Keys can be anything that can be (strictly) persisted. No
   requirements on key orderability here. Just need an EQUAL test
   between normalized keys. Key normalization is idempotent.
  
   [ Non-Liberal or Strict Persistence - unnamed compiled functions,
   functional closures, and some system level objects, as well as data
   structures that contain them, cannot be persisted.  Other, pure
   Lisp data structures can be persisted.
  
   So this excludes open streams, Actors, and many other low-level
   things.  Liberal Persistence substitutes proxy objects in their
   place.  Non-liberal Persistence throws an error.
  
   By special arrangement, Actors can be ferried across a network
   connection using substitution by special proxy Actor designators.
   But they cannot be persisted to disk.]
  
   If two persistable items are EQ when added to the KV database, they
   are restored to EQ in future sessions.
  
   ----------------------------------------------------------------------
   Notes about Concurrent (Parallel and Single-Threaded) Execution...
  
   Under the current Actors system we have concurrency and parallelism
   with multicore SMP. It is entirely possible for two tasks to be
   running in the same code simultaneously, both logically and
   physically.
  
   But even if we restrict the execution to a single machine thread,
   which dispenses with SMP parallelism, we still have concurrency.
   Two or more separate logical threads of execution can be running,
   logically simultaneously, against the same code.
  
   And, any time you have concurrency, you run the risk of data race
   conditions between separate threads (both logical and physical) of
   execution.
  
   Great! You say. I'll just make sure I write FPL-pure code. Well,
   for SMP parallel code, in the absence of using locks, that is
   certainly a minimum requirement. But that may not be enough.
  
   Imagine two threads (parallel or not) which share a reference to an
   Actor whose state may evolve in response to messsages sent to it.
   The Actor is written in perectly FPL-pure fashion; it never mutates
   its state variables, and only uses BECOME to evolve to a new state.
   Yet, outside observers only see that indeed the state has mutated.
   They can't actually observe the internal state, but they can
   observe a change in behavior.
  
   So, for read-modify-write processes, if there is any temporal
   separation between the act of reading state, and modifying state,
   we open ourselves up to potential data race conditions.
  
   Here, time can be measured by the number of message dispatches
   activating a particular Actor, which occur between two positions in
   time. To appear logically atomic, a read-modify-write state change
   must occur entirely within one activation of an Actor.
  
   In such case, the CAS semantics on BECOME will be enough to protect
   against data races. But if there is temporal separation between
   reading state, and writing state, as measured in separate Actor
   activations, then the CAS semantics will only retry the final
   BECOME mutation. That could leave us inconsistent. We actually need
   to retry from the point in time of initially reading the state.
  
   So if, e.g., we have a system where the shared Actor is queried in
   one message, the answer is viewed, and then an update message is
   sent, that occupies at least two message dispatches and cannot be
   seen as atomic. Another thread could sneak in there with the same
   intent, but with different purpose, and interfere with the first
   thread's plans. The result would be a system inconsistency.
  
   So not only does the code have to be written in FPL-pure fashion,
   any state changes must appear logically atomic to all outside
   observers. And the only way to effect that, in the case of
   temporally separated read / mutate, is to halt concurrent activity
   during that interrim period, within the mutating Actor.
  
   That is the purpose of SERIALIZER. It permits only one thread of
   execution at a time to proceeed beyond to the Actor subsystem under
   its control. All others are enqueued for execution only after a
   response is seen from the Actor subsystem, a reply back to the
   currently executing logical thread of execution.
  
   All client code using a SERIALIZER must provide a customer to which
   a response will be sent, even if only SINK. And the Actor system
   under control of the SERIALIZER must send a reply to its customer
   under all circumstances.
  
   This is necessary because the SERIALIZER inteposes between the
   Actor being controlled, and the actual customer of the SERIALIZER.
   When the SERIALIZER receives the message intended for the actual
   customer, it forwards the message to that customer and also
   releases another pending session in its queue.
  
   Be on guard: subtle bugs can seep into the code if you don't pay
   careful attention to the possibility of data race conditions.
  
   More obvious cases requiring only a single thread of execution
   would be situations in which a physical resource cannot be
   reasonably shared in parallel. E.g. file I/O. So again, a
   SERIALIZER is called for.
  
   ---------------------------------------------------------------------
   But all that aside, a SERIALIZER is only needed in one place in
   this KVDB code - at the very opening stage of the database on disk.
   Only one thread can be permitted to open and deserialize the
   initial database.  Thereafter, all of the code supports fully
   SMP parallel concurrency.
  
   Reading the database is always permitted. Commits of mutated
   database tables will succeed only if nothing has changed in the
   database during the time between when the database table was first
   accessed and when a commit of a mutated version is performed. If
   something has changed, then your retry Actor is called with a
   correct fresh copy of the database.
  
   Changing the value for any particular key is accomplished by
   ADD'ing the key-value pair, which overwrites the previous value.
  
   Physical updates to the disk image are delayed by 10s after the
   most recent update. The timer restarts on every new update, to
   allow for a cascade of updates before actually saving the image.
  
   The database is actually a read-only FPL Hashtable. On the local
   machine it is easy to pass around a copy of the actual database
   table. But for remote access, this could become a large data
   structure over time, and we need to avoid gratuitous message
   traffic on the network.
  
   [ Since you are running Lisp, you have the ability to destructively
   discard the FPL conventions and directly mutate collections
   belonging to database keys. Please resist that temptation. Your
   inner debugger will thank you for preserving its sanity. For us, it
   is read-only solely by respected convention. ]
  
   For remote access, once you have a handle to the remote KVDB Actor,
   you should immediately request a Proxy Actor which will prevent
   unnecessary transmissions of entire tables over the network.  The
   Proxy Actor will perform queries and mutations on your behalf, on
   the host machine of the database, sending you only the information
   you seek.
  
   To support coordinated updates among multiple databases, you can
   request exclusive commit access. Doing this returns a fresh copy of
   the database table to you (or your Proxy) and causes the database
   manager to enter a mode that acts like a kind of SERIALIZER.
  
   During the time that you hold exclusive commit permission, all
   reads continue to be allowed as before.  But any additional
   requests for exclusive access, and any commits, apart from your
   own, are enqueued for later execution.
  
   If you request exclusive commit access while the database is
   already in exclusive commit state, your activity wil be logically
   stalled until the current owner relinquishes control.
  
   You must relinquish exclusive status by either sending a COMMIT
   (which always succeeds), or by sending an ABORT.
  
   For remote access, mutliple database coordination, the KVDB Proxy
   Actor follows a similar protocol on your behalf. Again, this is for
   avoiding excessive network transmissions.
   ---------------------------------------------------------------------- |#

(defgeneric normalize-key (key)
  ;; Needs to be idempotent:
  ;;   (let ((norm-key (normalize-key key)))
  ;;     (assert (eq norm-key (normalize-key norm-key))))
  (:method (key)
   (normalize-key
    (loenc:encode key
                  :max-portability t)))
  (:method ((key uuid:uuid))
   (uuid:uuid-string key))
  (:method ((key vector))
   (if (typep key 'vec-repr:ub8-vector)
       ;; these typically come from loenc:encode
       (vec-repr:str (vec-repr:base64 key))
     (call-next-method)))
  (:method ((key character))
   key)
  (:method ((key string))
   key)
  (:method ((key number))
   key)
  (:method ((key symbol))
   key)
  (:method ((key pathname))
   (namestring key)))

(defun test-normalize-key (key)
  ;; Check that NORMALIZE-KEY is idempotent.
  (let ((normkey (normalize-key key)))
    (assert (eq normkey (normalize-key normkey)))))

;; -------------------

#+:KVDB-USE-FPLHT
(defun db-new ()
  ;; preserves case matching among string keys
  (fplht:make-fpl-hashtable :test 'equal :single-thread t))

#+:KVDB-USE-MAPS
(defun db-new ()
  (maps:empty))

;; -------------------
;; One version for us...

(defmethod db-find ((tbl fplht:fpl-hashtable) key &optional default)
  (fplht:fpl-gethash tbl (normalize-key key) default))

(defmethod db-add ((tbl fplht:fpl-hashtable) key val)
  (fplht:fpl-sethash tbl (normalize-key key) val))

(defmethod db-find-or-add ((tbl fplht:fpl-hashtable) key val)
  (fplht:fpl-gethash-or-add tbl (normalize-key key) val))

(defmethod db-remove ((tbl fplht:fpl-hashtable) key)
  (fplht:fpl-remhash tbl (normalize-key key)))

(defmethod db-map ((tbl fplht:fpl-hashtable) fn)
  (fplht:fpl-maphash tbl fn))

(defmethod db-get-keys ((tbl fplht:fpl-hashtable))
  (fplht:fpl-get-keys tbl))
  
(defmethod db-rebuild ((tbl fplht:fpl-hashtable))
  (fplht:rebuild-fpl-hashtable tbl))

;; -------------------
;; ... and one version for them...

(defmethod db-find ((tbl maps:tree) key &optional default)
  (maps:find tbl (normalize-key key) default))

(defmethod db-add ((tbl maps:tree) key val)
  (maps:add tbl (normalize-key key) val))

(defmethod db-find-or-add ((tbl maps:tree) key val)
  (let* ((nkey  (normalize-key key))
         (exist (maps:find tbl nkey tbl)))
    (if (eq exist tbl)
        (values val (maps:add tbl nkey val))
      (values exist tbl))))

(defmethod db-remove ((tbl maps:tree) key)
  (maps:remove tbl (normalize-key key)))

(defmethod db-map ((tbl maps:tree) fn)
  (maps:iter tbl fn))

(defmethod db-get-keys ((tbl maps:tree))
  (maps:fold tbl (lambda (k v accu)
                   (declare (ignore v))
                   (cons k accu))
             nil))

(defmethod db-rebuild ((tbl maps:tree))
  tbl)

;; ----------------------------------------------------------------

(defun common-trans-beh (saver db msg)
  (match msg
    ;; -------------------
    ;; general entry for external clients
    ((cust :req)
     (send cust db))
    
    ;; -------------------
    ;; We are the only one that knows the identity of saver, so this
    ;; can't be forged by malicious clients. Also, a-db will only
    ;; eql db if there have been no updates within the last 10 sec.
    ((a-tag a-db) / (and (eql a-tag saver)
                         (eql a-db  db))
     (send saver sink :save-log db))
    ))
    
(defun trans-gate-beh (saver db)
  (lambda (&rest msg)
    (match msg
      ((cust :req-excl owner)
       ;; request exclusive :commit access
       ;; customer must promise to either :commit or :abort
       (send cust db)
       (become (busy-trans-gate-beh saver db owner nil)))
      
      ;; -------------------
      ;; commit after update
      (( (cust . retry) :commit old-db new-db)
       (cond ((eql old-db db) ;; make sure we have correct version
              (cond ((eql new-db db)
                     ;; no real change
                     (send cust new-db))
                    
                    (t
                     ;; changed db, so commit new
                     (let ((versioned-db (db-add new-db 'version (uuid:make-v1-uuid) )))
                       ;; version key is actually 'com.ral.actors.kv-database::version
                       (become (trans-gate-beh saver versioned-db))
                       (send-after 10 self saver versioned-db)
                       (send cust versioned-db)))
                    ))
             
             (t
              ;; had wrong version for old-db
              (send retry db))
             ))

      (('maint-full-save)
       (let ((new-db (db-rebuild db)))
         (become (trans-gate-beh saver new-db))
         (send saver sink :full-save new-db)))

      (_
       (common-trans-beh saver db msg))
      )))
    
;; ----------------------------------------------------------------

(defun busy-trans-gate-beh (saver db owner queue)
  (lambda (&rest msg)
    (labels ((release (cust db)
               (send cust db)
               (do-queue (msg queue)
                 (send* self msg))
               (become (trans-gate-beh saver db)))

             (stash ()
               (become (busy-trans-gate-beh saver db owner (addq queue msg)))))
      
      (match msg
        ((acust :req-excl an-owner)
         (if (eql an-owner owner)
             (send acust db)
           (stash)))
        
        ((acust :abort an-owner) / (eql an-owner owner)
         ;; relinquish excl :commit ownership
         (release acust db))
      
      ;; -------------------
      ;; commit after update from owner, then relinquish excl :commit ownership
      (( (acust . an-owner) :commit old-db new-db) / (eql an-owner owner)
         (cond ((eql old-db db) ;; make sure we have correct version
                (cond ((eql new-db db)
                       ;; no real change
                       (release acust db))

                      (t
                       ;; changed db, so commit new
                       (let ((versioned-db (db-add new-db 'version (uuid:make-v1-uuid) )))
                         ;; version key is actually 'com.ral.actors.kv-database::version
                         (send-after 10 self saver versioned-db)
                         (release acust versioned-db)))
                      ))
               
               (t
                ;; Sender must not have included the original db to
                ;; check against.  This is a programming error...
                (error "Should not happen"))
               ))
      
      ((_ :commit . _)
       (stash))
      
      (('maint-full-save)
       (stash))

      (_
       (common-trans-beh saver db msg))
      ))))

;; ----------------------------------------------------------------

(defun nascent-database-beh (tag saver msgs)
  ;; -------------------
  ;; We are the only one that knows the identity of tag and saver. So
  ;; this message could not have come from anywhere except saver
  ;; itself.
  (alambda
   ((a-tag :opened db) / (eql a-tag tag)
    (become (trans-gate-beh saver db))
    ;; now open for business, resubmit pending client requests
    (send-all-to self msgs))
   
   ;; -------------------
   ;; accumulate client requests until we open for business
   (msg
    (become (nascent-database-beh tag saver (cons msg msgs) )))
   ))

;; -----------------------------------------------------------

(defconstant +db-id+  {6f896744-6472-11ec-8ecb-24f67702cdaa})

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
    (send fmt-println "Saving KVDB Deltas: ~S" path)
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
              )))
      (error ()
        ;; expected possible error due to file not existing yet
        ;; or from non-existent version in prev-ver
        (full-save path new-db ctrl-tag)))
    (become (save-database-beh path new-db ctrl-tag))
    (send cust :ok))
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

(defun check-kvdb-sig (fd dbpath)
  (let* ((sig  (uuid:uuid-to-byte-array +db-id+))
         (id   (vec-repr:make-ub8-vector (length sig))))
    (file-position fd 0)
    (read-sequence id fd)
    (unless (equalp id sig)
      (error 'not-a-kvdb :path dbpath))
    ))

(defun #1=unopened-database-beh (ctrl-tag)
  (lambda (cust &rest msg)
    (handler-bind
        ((error (lambda (err)
                  (abort-beh)
                  (become-sink)
                  (send ctrl-tag :remove-entry)
                  (send cust (err-from err))
                  (return-from #1#))
                ))
      (match msg
        ;; -------------------
        ;; message from kick-off starter routine
        ((:open db-path)
         (let ((db (db-new)))
           (flet ((prep-and-save-db ()
                    (unless (db-find db 'version)
                      (setf db (db-add db 'version (uuid:make-v1-uuid))))
                    (let (seq)
                      (unless (and (setf seq (db-find db 'kvdb-sequence))
                                   (typep seq 'uuid:uuid))
                        (setf db (db-add db 'kvdb-sequence (uuid:make-v1-uuid)))
                        ))
                    (setf db (full-save db-path db ctrl-tag))))
             
             (restart-case
                 (handler-bind
                     ((not-a-kvdb (lambda (c)
                                    (if (capi:prompt-for-confirmation
                                         (format nil "Not a KVDB file: ~S. Rename existing and create new?"
                                                 (not-a-kvdb-path c)))
                                        (invoke-restart
                                         (find-restart 'rename-and-create c))
                                      ;; else
                                      (abort c))))
                      (corrupt-deltas (lambda (c)
                                        (warn "Corrupt deltas encountered: ~S. Rebuilding."
                                              (corrupt-deltas-path c))
                                        (invoke-restart
                                         (find-restart 'corrupt-deltas c))))
                      (corrupt-kvdb   (lambda (c)
                                        (warn "Corrupt KVDB: ~S. Rebuilding."
                                              (corrupt-kvdb-path c))
                                        (invoke-restart
                                         (find-restart 'corrupt-kvdb c))))
                      (file-error (lambda (c)
                                    (if (capi:prompt-for-confirmation
                                         (format nil "File does not exist: ~S. Create?"
                                                 (file-error-pathname c)))
                                        (invoke-restart
                                         (find-restart 'create c))
                                      ;; else
                                      (abort c))) ))
                   
                   (with-open-file (f db-path
                                      :direction         :input
                                      :if-does-not-exist :error
                                      :element-type      '(unsigned-byte 8))
                     (check-kvdb-sig f db-path)
                     
                     (handler-case
                         (progn
                           (setf db (loenc:deserialize f))
                           (db-find db 'version)) ;; try to force an error
                       (error (exn)
                         (send println (um:format-error exn))
                         (setf db (db-new))
                         (error 'corrupt-kvdb :path db-path)))

                     (let ((reader (self-sync:make-reader f)))
                       (handler-case
                           (loop for ans = (loenc:deserialize f :self-sync  reader)
                                 until (eq ans f)
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
                                     ))
                         (error (exn)
                           (send println (um:format-error exn))
                           (error 'corrupt-deltas :path db-path))
                         ))))
               ;; restarts
               (corrupt-kvdb ()
                 :test corrupt-kvdb-p
                 (prep-and-save-db))
               (corrupt-deltas ()
                 :test corrupt-deltas-p
                 (prep-and-save-db))
               (rename-and-create ()
                 :test not-a-kvdb-p
                 (prep-and-save-db))
               (create ()
                 :test file-error-p
                 (prep-and-save-db)))
             ;; normal exit
             (become (save-database-beh db-path db ctrl-tag))
             (send cust :opened db)))
         )))))
  
;; --------------------------------------------------------------------

(defun full-save (db-path db ctrl-tag)
  (send fmt-println "Saving full KVDB: ~S" db-path)
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
    (send ctrl-tag :update-entry) ;; inode has changed
    sav-db))

(defun get-diffs (old-db new-db)
  (let* ((new-wrk   (db-rebuild new-db))
         (old-keys  (db-get-keys old-db))
         (new-keys  (db-get-keys new-wrk))
         (removals  (set-difference old-keys new-keys :test #'equal))
         (additions (mapcan #'identity
                            (mapcar (lambda (k)
                                      (list (cons k (db-find new-wrk k))))
                                    (set-difference new-keys old-keys  :test #'equal))))
         (changes   (mapcan #'identity
                            (mapcar (lambda (k)
                                      (let ((old-val (db-find old-db k new-wrk))
                                            (new-val (db-find new-wrk k)))
                                        (cond ((eq old-val new-wrk)  nil)   ;; missing from old
                                              ((eql old-val new-val) nil)   ;; unchanged
                                        (t  (list (cons k new-val)))        ;; changed
                                        )))
                                    (set-difference new-keys (mapcar #'car additions)))
                            ))
         (log       (list
                     removals
                     additions
                     changes)))
    ;; (send writeln log)
    log))

;; -----------------------------------------------------------

(defun db-svc-init-beh (ctrl-tag path)
  (λ _
    (let ((tag-to-me (tag self))
          (saver (serializer (create (unopened-database-beh ctrl-tag)))
                 ))
      (send saver tag-to-me :open path)
      (become (nascent-database-beh tag-to-me saver nil))
      (repeat-send self))))

;; -----------------------------------------------------------

(defvar *db-path*  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                                    (sys:get-folder-path :appdata)))

#+:LISPWORKS
(editor:setup-indent "with-db" 1)

(defun %make-kvdb (ctrl-tag path)
  (let ((dbmgr  (create (db-svc-init-beh ctrl-tag path))))
    (flet
        ((do-with-db (fn)
           (β (db)
               (send dbmgr β :req)
             (funcall fn db))))
      (macrolet ((with-db (db &body body)
                   `(do-with-db (lambda (,db) ,@body))))
        (create
         (alambda
                
          ;; ------------------------------------------
          ;; :ADD and :REMOVE both return the raw kvdb map object.
          ;; Hence they are best used only on the local machine to
          ;; avoid excessive network traffic.
                
          ((cust :add key val)
           (with-db db
             (send dbmgr `(,cust . ,self) :commit db (db-add db key val))
             ))
                
          ((cust :remove key)
           (with-db db
             (unless (eq self (db-find db key self))
               (send dbmgr `(,cust . ,self) :commit db (db-remove db key)))
             ))
                
          ;; ---------------------------------------------
                
          ((cust :find key . default)
           (with-db db
             (send cust (db-find db key (car default)))
             ))
                
          ((cust :find-or-add key def-val)
           (with-db db
             (multiple-value-bind (val new-db)
                 (db-find-or-add db key def-val)
               (β _
                   (send dbmgr `(,β . ,self) :commit db new-db)
                 (send cust val))
               )))
                
          ((:show)
           (with-db db
             (let ((map (maps:empty)))
               (db-map db 
                       (lambda (k v)
                         (send writeln (list k v))
                         (maps:addf map k v)))
               (sets:view-set map))
             ))
                
          ((cust :really-let-me-see-map)
           (with-db db
             (send cust db)))
                
          ;; ----------------------------------
          ;; These routines deal in/out with physical map objects.
          ;; Hence, best used for local machine access of the kvdb

          ((_ :req)
           (repeat-send dbmgr))
                
          ((_ :abort _)
           (repeat-send dbmgr))
                
          (((_ . _) :commit _ _)
           (repeat-send dbmgr))
                
          ((_ :req-excl _)
           (repeat-send dbmgr))
                
          ;; -----------------------------------
          ;; Intended for use by remote clients wanting to access this
          ;; kvdb. But also works for local access. We avoid shipping
          ;; entire map objects back and forth.
                
          ((cust :req-proxy)
           (send cust (create (local-proxy-for-remote-db-access-beh self))))

          ((cust :db-path)
           (send cust path))
          
          ;; ---------------------------------
                
          ((:maint-full-save)
           (send dbmgr 'maint-full-save))

          #|
          (msg
           (send fmt-println "Unkown message: ~S" msg))
          |#
          ))
      ))))

(defun ensure-file-exists (path)
  (flet ((init-kvdb ()
           (let ((db (db-new)))
             (setf db (db-add db 'version       (uuid:make-v1-uuid)))
             (setf db (db-add db 'kvdb-sequence (uuid:make-v1-uuid)))
             (full-save path db nil)
             t)))
    (restart-case
        (handler-bind
            ((file-error (lambda (c)
                           (invoke-restart (find-restart 'create c))))
             (not-a-kvdb (lambda (c)
                           (invoke-restart (find-restart 'overwrite c)))))
          (with-open-file (fd path
                              :direction :input
                              :element-type '(unsigned-byte 8)
                              :if-does-not-exist :error)
            (check-kvdb-sig fd path)
            t))
      ;; restarts
      (create ()
        :test file-error-p
        (when (capi:prompt-for-confirmation
               (format nil "Create file: ~S?" path))
          (init-kvdb)))
      (overwrite ()
        :test not-a-kvdb-p
        (when (capi:prompt-for-confirmation
               (format nil "Rename existing and create new file: ~S?" path))
          (init-kvdb)))
      )))

#-:MSWINDOWS
(defun ino-key (path)
  (multiple-value-bind (dev ino)
      (um:get-ino path)
    (um:mkstr dev #\space ino)))

#+:MSWINDOWS
(defun ino-key (path)
  (namestring (truename path)))

(defun kvdb-orchestrator-beh (&optional open-dbs)
  ;; Prevent duplicate kvdb Actors for the same file.
  (alambda
   ((cust :make-kvdb path)
    (cond ((ensure-file-exists path)
           (let* ((key   (ino-key path))
                  (quad  (find key open-dbs
                               :key  #'car
                               :test #'string-equal)))
             (cond (quad
                    (send cust (third quad)))
                   (t
                    (let* ((tag-to-me  (tag self))
                           (kvdb       (%make-kvdb tag-to-me path)))
                      (become (kvdb-orchestrator-beh
                               (cons (list key tag-to-me kvdb path)
                                     open-dbs)))
                      (send cust kvdb)))
                   )))
          (t
           (send cust (const (format nil "Database ~S Not Available" path))))
          ))
   
   ((atag :update-entry)
    ;; when actual inode changes, as with full-save
    (let ((quad (find atag open-dbs
                      :key #'cadr)))
      (when quad
        (let* ((path  (fourth quad))
               (key   (ino-key path)))
          (unless (string-equal key (car quad))
            (let* ((kvdb    (third quad))
                   (new-dbs (cons (list key atag kvdb path)
                                  (remove quad open-dbs))))
              (become (kvdb-orchestrator-beh new-dbs))
              ))
          ))
      ))
   
   ((atag :remove-entry)
    (let ((quad (find atag open-dbs
                      :key #'cadr)))
      (when quad
        (become (kvdb-orchestrator-beh (remove quad open-dbs))))
      ))
   ))
  
(deflex kvdb-maker
  (serializer ;; because we are doing file ops?
   (create (kvdb-orchestrator-beh))))

;; -----------------------------------------------------
;; Local Proxy KVDB's for remote access - avoid shuttling entire map
;; objects across the network.

(defun local-proxy-for-remote-db-access-beh (kvdb)
  (alambda
   ((cust :add key val)
    (let ((me self))
      (β _
          (send kvdb β :add key val)
        (send cust me))
      ))
   
   ((cust :remove key)
    (let ((me self))
      (β _
          (send kvdb β :remove key)
        (send cust me))
      ))
      
   ((_ :find . _)
    (repeat-send kvdb))

   ((_ :find-or-add . _)
    (repeat-send kvdb))

   ((_ :really-let-me-see-map)
    ;; Okay... you asked for it...
    (repeat-send kvdb))

   ((cust :req)
    (let ((me  self)
          (tag (tag self)))
      (become (future-become-beh tag))
      (β (db)
          (send kvdb β :req)
        (send tag (local-updateable-proxy-for-remote-db-access-beh kvdb db db))
        (send cust me))
      ))
   
   ((cust :req-proxy)
    ;; Really no reason to be sending this message, we are already
    ;; a proxy Actor.
    (send cust self))

   ((cust :req-excl owner)
    (let ((me  self)
          (tag (tag self)))
      (become (future-become-beh tag))
      (β (db)
          (send kvdb β :req-excl owner)
        (send tag (local-excl-proxy-for-remote-db-access-beh kvdb db db owner))
        (send cust me))
      ))

   (((cust . _) :commit)
    ;; Nothing to do here, already fully committed. User should have
    ;; performed a :REQ first.
    (send cust self))

   ((cust :abort)
    ;; Nothing to do here, already fully committed. User should have
    ;; performed a :REQ first.
    (send cust self))

   ((_ :db-path)
    (repeat-send kvdb))
   ))

(defun local-updateable-proxy-for-remote-db-access-beh (kvdb orig-map upd-map)
  (alambda
   ((cust :add key val)
    (become (local-updateable-proxy-for-remote-db-access-beh
             kvdb orig-map
             (db-add upd-map key val)))
    (send cust self))

   ((cust :remove key)
    (become (local-updateable-proxy-for-remote-db-access-beh
             kvdb orig-map
             (db-remove upd-map key)))
    (send cust self))

   ((cust :find key . default)
    (send cust (db-find upd-map key (car default))))

   ((cust :find-or-add key def-val)
    (multiple-value-bind (val new-db)
        (db-find-or-add upd-map key def-val)
      (become (local-updateable-proxy-for-remote-db-access-beh
               kvdb orig-map new-db))
      (send cust val)))

   ((cust :really-let-me-see-map)
    ;; Okay... you asked for it...
    (send cust upd-map))

   ((cust :req)
    (send cust self))

   ((cust :req-proxy)
    (send cust self))
   
   ((cust :req-excl owner)
    ;; This might have to abandon existing updates and start fresh.
    ;; You should have requested from outset.
    (let ((me  self)
          (tag (tag self)))
      (become (future-become-beh tag))
      (β _
          (send kvdb `(,β . ,β) :commit orig-map upd-map) ;; might fail
        (β (db)
            (send kvdb β :req-excl owner)
          (send tag (local-excl-proxy-for-remote-db-access-beh kvdb db db owner))
          (send cust me))
        )))

   ((cust :abort)
    (send cust :ok)
    (become (local-proxy-for-remote-db-access-beh kvdb)))

   (((cust . retry) :commit)
    (let* ((me  self)
           (committed (create (lambda (ans)
                                (declare (ignore ans))
                                (send cust me))))
           (lcl-retry (create (lambda (ans)
                                (declare (ignore ans))
                                (send retry me)))))
      (become (local-proxy-for-remote-db-access-beh kvdb))
      (send kvdb `(,committed . ,lcl-retry) :commit orig-map upd-map)))

   ((_ :db-path)
    (repeat-send kvdb))
   ))

(defun local-excl-proxy-for-remote-db-access-beh (kvdb orig-map upd-map owner)
  ;; Inside here we have the kvdb under commit lock, so keep it short,
  ;; Jack...
  ;;
  ;; Remote user must issue one or more of these provided message
  ;; types and finish with either :ABORT or :COMMIT. This proxy avoids
  ;; shuttling entire map objects across the network.
  (alambda
   ((cust :add key val)
    (become (local-excl-proxy-for-remote-db-access-beh
             kvdb orig-map
             (db-add upd-map key val)
             owner))
    (send cust self))
   
   ((cust :remove key)
    (become (local-excl-proxy-for-remote-db-access-beh
             kvdb orig-map
             (db-remove upd-map key)
             owner))
    (send cust self))

   ((cust :find key . default)
    (send cust (db-find upd-map key (car default))))

   ((cust :find-or-add key def-val)
    (multiple-value-bind (val new-db)
        (db-find-or-add upd-map key def-val)
      (become (local-excl-proxy-for-remote-db-access-beh
               kvdb orig-map new-db owner))
      (send cust val)))

   ((cust :really-let-me-see-map)
    ;; Okay... you asked for it...
    (send cust upd-map))

   ((cust :req)
    (send cust self))

   ((cust :req-proxy)
    (send cust self))
   
   ((cust :req-excl)
    ;; Really no reason to be sending this message, we are already
    ;; under excl commit.
    (send cust self))

   ((cust :abort)
    (let ((me self))
      (become (local-proxy-for-remote-db-access-beh kvdb))
      (β _
          (send kvdb β :abort owner)
        (send cust me)
      )))

   ((cust :commit)
    (let ((me self))
      (become (local-proxy-for-remote-db-access-beh kvdb))
      (β _
          (send kvdb `(,β . ,owner) :commit orig-map upd-map)
        (send cust me)
      )))

   ((_ :db-path)
    (repeat-send kvdb))
   ))

;; -----------------------------------------------------
;; One to goof around in...

(deflex kvdb (fut kvdb-maker :make-kvdb *db-path*))

;; -----------------------------------------------------
;; more goofing around...
;;
;; These won't become permanent until/unless :maint-full-save

(defun convert-db (new-db-proto)
  (let++ ((:β db  (racurry kvdb :req))
          (new-db new-db-proto))
    (db-map db (lambda (k v)
                 (setf new-db (db-add new-db k v))))
    (send kvdb `(,writeln . ,self) :commit db new-db)))
            
(defun become-fplht ()
  (convert-db (fplht:make-fpl-hashtable :test 'equal :single-thread t)))

(defun become-maps ()
  (convert-db (maps:empty)))

#|
(become-fplht)
(become-maps)
|#
;; -----------------------------------------------------------
#|
(ask kvdb :find :dave)
(uuid:when-created (ask kvdb :find 'version))
(uuid:when-created (call-actor kvdb :find 'version))
(send kvdb :show)

(dotimes (ix 5)
  (send kvdb println :add ix ix))
(send kvdb println :add :dave :chara)
(send kvdb println :add "Dave" "Chara")
(send kvdb println :add "dave" "chara")
(send kvdb println :add :cat "dog")
(send kvdb writeln :find :cat)
(send kvdb :show)
(send kvdb writeln :really-let-me-see-map)
(dotimes (ix 10)
  (send kvdb println :remove ix))
(send kvdb println :add :tst (lambda* _))
(send kvdb writeln :find :tst)

(let ((m (db-new)))
  (setf m (db-add m :dave :dog))
  (eql m (db-add m :dave :dog)))
(let ((m (dn-new)))
  (setf m (db-add m :dave))
  (eql m (db-add m :dave)))

(β (ans)
    (send kvdb β :find-or-add :bank-bal 10)
  (send fmt-println "Bal = ~A" ans))

(β (db)
    (send kvdb β :req)
  (db-map db (λ (k v)
               (send writeln (list k v)))))

(let ((x '(1 2 3)))
  (send kvdb println :add :tst1 x)
  (send kvdb println :add :tst2 x))

(let ((x1 (ask kvdb :find :tst1))
      (x2 (ask kvdb :find :tst2)))
  (send println (list x1 x2 (eq x1 x2))))

(let (x)
  (β (db)
      (send kvdb β :req)
    (send writeln (list x db β (eq β self)))))

(β (db)
    (send kvdb β :req)
  (let ((x (maps:empty))
        y)
    (db-map db (lambda (k v)
                 (push (list k v) y)
                 #|
                 (when (eql k :cat)
                   (break))
                 |#
                 (maps:addf x k v)))
    (sets:view-set x)
    (send writeln (reverse y))))

(send kvdb :maint-full-save)

;; ------------------------------------------------------
(with-open-file (f "~/junk.tst"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :rename
                     :element-type '(unsigned-byte 8))
  (dotimes (ix 10)
    (let ((str (format nil "This is test ~D!" ix)))
      (loenc:serialize str f
                       :max-portability t
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
;; ---------------------------------------------------------
;; ---------------------------------------------------------
;; GUI for KVDB - an interesting interplay between CAPI thread and
;; Actors.

;; ---------------------------------------------------------
;; For our own sanity... Let's keep things, as much as possible, on
;; one playing field.

(deflex i-capi
  (create
   ;; Expects intf, fn, args in message
   #'capi:execute-with-interface))

(deflex p-capi
  (create
   ;; Expects pane, fn, args in message
   #'capi:apply-in-pane-process))

;; ------------

(defmacro with-capi-intf (intf &body body)
  `(send i-capi ,intf (lambda () ,@body)))

(defmacro with-capi-pane (pane &body body)
  `(send p-capi ,pane (lambda () ,@body)))

;; -----------------------------------------------------------
;; Now for the GUI...

(capi:define-interface kvdb-display ()
  ((db-pathname :reader kv-db-pathname :initarg :path))
  (:panes
   #|
   (search-pane   capi:text-input-pane
                  :accessor           search-pane
                  :title              "Search..."
                  :callback           'search-keys)
   |#
   (db-path-pane  capi:title-pane
                  :text               db-pathname
                  :foreground         :seagreen)
   (keys-list     capi:list-panel
                  :accessor           keys-panel
                  :visible-min-width  300
                  :visible-min-height 300
                  :selection-callback 'click-show-value
                  :callback-type      :item-element
                  :foreground         #+:MACOSX :yellow #-:MACOSX :black
                  :title              "KVDB Key"
                  :title-args         '(:foreground #+:MACOSX :skyblue #-:MACOSX :gray50)
                  :print-function     'key-to-string)
   (value-display capi:editor-pane
                  :accessor           value-panel
                  :title              "KVDB Value"
                  :title-args         '(:foreground #+:MACOSX :skyblue #-:MACOSX :gray50)
                  :text               ""
                  :buffer-name        :temp
                  :foreground         #+:MACOSX :yellow #-:MACOSX :black
                  :visible-min-width  400
                  :visible-min-height 300)
   (refr-but      capi:push-button
                  :text               "Refresh"
                  :foreground         :skyblue
                  :callback           'click-refresh-keys)
   (del-but       capi:push-button
                  :text               "Delete"
                  :foreground         :skyblue
                  :callback           'click-delete-key)
   (add-but       capi:push-button
                  :text               "Add/Change"
                  :foreground         :skyblue
                  :callback           'click-add/change-key))
  (:layouts
   (main-layout capi:column-layout
                '(path-layout :separator central-layout))
   (path-layout capi:row-layout
                '(nil db-path-pane nil))
   (central-layout capi:row-layout
                '(keys-layout :divider value-display))
   (keys-layout capi:column-layout
                '(#|search-pane|# keys-list but-layout))
   (but-layout capi:row-layout
               '(refr-but del-but add-but)))
  (:default-initargs
   :title "KVDB Browser"))

;; -----------------------------------------------------------
;; Utility Functions

(defun key-to-string (key)
  ;; Also used for value displays
  (with-output-to-string (s)
    (with-maximum-io-syntax ;; with-standard-io-syntax
      (let ((*package* (find-package :cl)))
        (handler-case
            (let ((*print-readably* t))
              (write key :stream s))
          (error ()
            ;; Some compiled functions refuse to display in
            ;; *PRINT-READABLE* mode, and throw us here with an ERROR.
            (let ((*print-readably* nil))
              (write key :stream s)))
          ))
      )))

(defun collect-keys (cust)
  (β (db)
      (send kvdb β :req)
    (let (keys)
      (db-map db
              (lambda (k v)
                (declare (ignore v))
                (push k keys)))
      (send cust (sort keys #'string< :key #'key-to-string))
      )))

;; -----------------------------------------------------------
;; Show the GUI

(defun show-kvdb ()
  ;; Show a KVDB Browser
  (β (path)
      (send kvdb β :db-path)
    (let ((intf (capi:display
                 (make-instance 'kvdb-display
                                :path (namestring path)))))
      (refresh-select-and-show-first-item intf))
    ))

;; -----------------------------------------------------------

(defun select-and-show-key (intf key)
  (with-capi-intf intf
    (let ((keys-pane (keys-panel intf)))
      (setf (capi:choice-selected-item keys-pane) key)
      (click-show-value key keys-pane))
    ))
  
(defun refresh-select-and-show-first-item (intf)
  (β _
      (refresh-keys nil intf β)
    (with-capi-intf intf
      (let* ((keys-pane (keys-panel intf))
             (keys      (capi:collection-items keys-pane)))
        (when (plusp (length keys))
          (select-and-show-key intf (aref keys 0))
          )))
    ))

(defun refresh-select-and-show-key (intf key)
  (β _
      (refresh-keys nil intf β)
    (select-and-show-key intf key)))
  
;; -----------------------------------------------------------
;; GUI CAPI Callback Functions

(defun refresh-keys (xxx intf &optional cust)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (declare (ignore xxx))
  (β (keys)
      (collect-keys β)
    (with-capi-intf intf
      (let ((keys-panel (keys-panel intf)))
        (setf (capi:collection-items keys-panel) keys)
        (send cust :ok))) ;; for sequencing
    ))

(defun click-show-value (key pane)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (β (val)
      (send kvdb β :find key)
    (with-capi-pane pane
      (let* ((intf    (capi:element-interface pane))
             (ed-pane (value-panel intf)))
        (setf (capi:editor-pane-text ed-pane) (key-to-string val))
        (capi:call-editor ed-pane "End of Buffer")))
    ))

#|
(defun search-keys (text intf)
  (declare (ignore intf))
  (print text))
|#

(defun click-refresh-keys (xxx intf)
  (declare (ignore xxx))
  (refresh-select-and-show-first-item intf))

(defun click-delete-key (xxx intf)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (declare (ignore xxx))
  (let* ((keys-pane (keys-panel intf))
         (key       (capi:choice-selected-item keys-pane)))
    (with-actors
      (when (capi:prompt-for-confirmation
             (format nil "Delete ~S" (key-to-string key)))
        (β _
            (send kvdb β :remove key)
          (refresh-select-and-show-first-item intf))
        ))
    ))

;; ------------------------------------------------------------
;; Popup Dialog for Add/Change...

(capi:define-interface kv-query-intf ()
  ((key-text :initarg :key-text)
   (val-text :initarg :val-text))
  (:panes
   (key-pane capi:text-input-pane
             :accessor          key-pane
             :title             "Key"
             :text              key-text
             :visible-min-width 300)
   (val-pane capi:text-input-pane
             :accessor          val-pane
             :title             "Value"
             :text              val-text
             :visible-min-width 400))
  (:layouts
   (main-layout capi:row-layout
                '(key-pane val-pane))) )

(defun grab-text-value (pane)
  (read-from-string (capi:text-input-pane-text pane)))

(defun grab-dialog-values (pane)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  ;; PANE points to our dialog pane.
  (ignore-errors ;; this works out nicely!
    (list
     (grab-text-value (key-pane pane))
     (grab-text-value (val-pane pane)))
    ))

(defun click-add/change-key (xxx intf)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (declare (ignore xxx))
  (let* ((keys-pane (keys-panel intf))
         (key       (capi:choice-selected-item keys-pane)))
    (β (val)
        (send kvdb β :find key)
      (let ((dlg  (make-instance 'kv-query-intf
                                 :key-text (key-to-string key)
                                 :val-text (key-to-string val))
                  ))
        (multiple-value-bind (result successp)
            (capi:popup-confirmer dlg "Add/Change a KVDB Entry"
                                  :value-function 'grab-dialog-values
                                  :owner intf)
          (when successp
            (destructuring-bind (key val) result
              (β _
                  (send kvdb β :add key val)
                (refresh-select-and-show-key intf key))
              ))))
      )))

#|
(show-kvdb)
|#
