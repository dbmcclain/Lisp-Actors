;; kvdb.lisp -- transactional Key-Value database processing in Actors
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------

(defpackage com.ral.actors.kv-database
  (:use #:cl :com.ral.actors)
  (:export
   #:kvdb
   #:kvdb-maker
  ))
   
(in-package com.ral.actors.kv-database)

;; ----------------------------------------------------------------------
;; Using an FPL-pure RB-tree as the database. Hence the database
;; serves as its own transaction ID. Lock-free design.
;;
;; You can save anything you like in the KV database. But if it can't
;; be persisted then it won't be faithfully saved in the backing
;; store, and won't be restored in future sessions.
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
;; conditions between separate threads (both logical and physical) of
;; execution.
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
;; So, for read-modify-write processes, if there is any temporal
;; separation between the act of reading state, and modifying state,
;; we open ourselves up to potential data race conditions.
;;
;; Here, time can be measured by the number of message dispatches
;; activating a particular Actor, which occur between two positions in
;; time. To appear logically atomic, a read-modify-write state change
;; must occur entirely within one activation of an Actor.
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
;; intent, but with different purpose, and interfere with the first
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
;; currently executing logical thread of execution.
;;
;; All client code using a SERIALIZER must provide a customer to which
;; a response will be sent, even if only SINK. And the Actor system
;; under control of the SERIALIZER must send a reply to its customer
;; under all circumstances.
;;
;; This is necessary because the SERIALIZER inteposes between the
;; Actor being controlled, and the actual customer of the SERIALIZER.
;; When the SERIALIZER receives the message intended for the actual
;; customer, it forwards the message to that customer and also
;; releases another pending session in its queue.
;;
;; Be on guard: subtle bugs can seep into the code if you don't pay
;; careful attention to the possibility of data race conditions.
;;
;; More obvious cases requiring only a single thread of execution
;; would be situations in which a physical resource cannot be
;; reasonably shared in parallel. E.g. file I/O. So again, a
;; SERIALIZER is called for.
;;
;; ---------------------------------------------------------------------
;; But all that aside, a SERIALIZER is only needed in one place in
;; this KVDB code - at the very opening stage of the database on disk.
;; Only one thread can be permitted to open and deserialize the
;; initial database.  Thereafter, all of the code supports fully
;; SMP parallel concurrency.
;;
;; Reading the database is always permitted. Commits of mutated
;; database trees will succeed only if nothing has changed in the
;; database during the time between when the database tree was first
;; accessed and when a commit of a mutated version is performed. If
;; something has changed, then your retry Actor is called with a
;; correct fresh copy of the database.
;;
;; Changing the value for any particular key is accomplished by
;; ADD'ing the key-value pair, which overwrites the previous value.
;;
;; Physical updates to the disk image are delayed by 10s after the
;; most recent update. The timer restarts on every new update, to
;; allow for a cascade of updates before actually saving the image.
;;
;; The database is actually a read-only FPL Red-Black Tree. On the
;; local machine it is easy to pass around a copy of the actual
;; database tree. But for remote access, this could become a large
;; data structure over time, and we need to avoid gratuitous message
;; traffic on the network.
;;
;; [ Since you are running Lisp, you have the ability to destructively
;; discard the FPL conventions and directly mutate collections
;; belonging to database keys. Please resist that temptation. Your
;; inner debugger will thank you for preserving its sanity. For us, it
;; is read-only solely by respected convention. ]
;;
;; For remote access, once you have a handle to the remote KVDB Actor,
;; you should immediately request a Proxy Actor which will prevent
;; unnecessary transmissions of entire trees over the network.  The
;; Proxy Actor will perform queries and mutations on your behalf, on
;; the host machine of the database, sending you only the information
;; you seek.
;;
;; To support coordinated updates among multiple databases, you can
;; request exclusive commit access. Doing this returns a fresh copy of
;; the database tree to you (or your Proxy) and causes the database
;; manager to enter a mode that acts like a kind of SERIALIZER.
;;
;; During the time that you hold exclusive commit permission, all
;; reads continue to be allowed as before.  But any additional
;; requests for exclusive access, and any commits, apart from your
;; own, are enqueued for later execution.
;;
;; If you request exclusive commit access while the database is
;; already in exclusive commit state, your activity wil be logically
;; stalled until the current owner relinquishes control.
;;
;; You must relinquish exclusive status by either sending a COMMIT
;; (which always succeeds), or by sending an ABORT.
;;
;; For remote access, mutliple database coordination, the KVDB Proxy
;; Actor follows a similar protocol on your behalf. Again, this is for
;; avoiding excessive network transmissions.
;; ----------------------------------------------------------------------

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
    
    ;; -------------------
    (('maint-full-save)
     (send saver sink :full-save db))
    ))
    
(defun trans-gate-beh (saver db)
  (lambda (&rest msg)
    (match msg
      ((cust :req-excl owner)
       ;; request exclusive :commit access
       ;; customer must promise to either :commit or :abort
       (send cust db)
       (become (busy-trans-gate-beh saver db owner +emptyq+)))
      
      ;; -------------------
      ;; commit after update
      (( (cust . retry) :commit old-db new-db)
       (cond ((eql old-db db) ;; make sure we have correct version
              (cond ((eql new-db db)
                     ;; no real change
                     (send cust new-db))
                    
                    (t
                     ;; changed db, so commit new
                     (let ((versioned-db (maps:add new-db 'version (uuid:make-v1-uuid) )))
                       ;; version key is actually 'com.ral.actors.kv-database::version
                       (become (trans-gate-beh saver versioned-db))
                       (send-after 10 self saver versioned-db)
                       (send cust versioned-db)))
                    ))
             
             (t
              ;; had wrong version for old-db
              (send retry db))
             ))

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
                       (let ((versioned-db (maps:add new-db 'version (uuid:make-v1-uuid) )))
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
      
      (_
       (common-trans-beh saver db msg))
      ))))

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
   (full-save path db)
   (become (save-database-beh path db))
   (send cust :ok))

  ;; -------------------
  ;; The db gateway is the only one that knows saver's identity.
  ;; Don't bother doing anything unless the db has changed.
  ((cust :save-log new-db)
   (handler-case
       (let ((new-ver  (maps:find new-db  'version))
             (prev-ver (maps:find last-db 'version)))
         (when (uuid:uuid-time< prev-ver new-ver)
           (let* ((delta (get-diffs last-db new-db)))
             (with-open-file (f path
                                :direction         :output
                                :if-exists         :append
                                :if-does-not-exist :error
                                :element-type      '(unsigned-byte 8))
               (loenc:serialize delta f
                                :self-sync t))
             )))
     (error ()
       ;; expected possible error due to file not existing yet
       ;; or from non-existent version in prev-ver
       (full-save path new-db)))
   (become (save-database-beh path new-db))
   (send cust :ok)))
  
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
         (unless (maps:find db 'version)
           (maps:addf db 'version (uuid:make-v1-uuid)))
         (unless (maps:find db 'kvdb-sequence)
           (maps:addf db 'kvdb-sequence (uuid:make-v1-uuid)))
         (full-save db-path db)))
     (become (save-database-beh db-path db))
     (send cust :opened db))))

;; --------------------------------------------------------------------

(defun full-save (db-path db)
  (ensure-directories-exist db-path)
  (with-open-file (f db-path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (let ((sig (uuid:uuid-to-byte-array +db-id+)))
      (write-sequence sig f)
      (loenc:serialize db f)
      ))
  db)

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

;; -----------------------------------------------------------

(defvar *db-path*  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                                    (sys:get-folder-path :appdata)))

#+:LISPWORKS
(editor:setup-indent "with-db" 1)

(defun %make-kvdb (&optional (path *db-path*))
  (let ((dbmgr  (create (db-svc-init-beh path))))
    (macrolet ((with-db (db &body body)
                 `(do-with-db (lambda (,db) ,@body))))
      (labels
          ((do-with-db (fn)
             (β (db)
                 (send dbmgr β :req)
               (funcall fn db))))
        
        (create
         (alambda

          ;; ------------------------------------------
          ;; :ADD and :REMOVE both return the raw kvdb map object.
          ;; Hence they are best used only on the local machine to
          ;; avoid excessive network traffic.
          
          ((cust :add key val)
           (with-db db
             (send dbmgr `(,cust . ,self) :commit db (maps:add db key val))
             ))
          
          ((cust :remove key)
           (with-db db
             (unless (eq self (maps:find db key self))
               (send dbmgr `(,cust . ,self) :commit db (maps:remove db key)))
             ))

          ;; ---------------------------------------------

          ((cust :find key . default)
           (with-db db
             (send cust (maps:find db key (car default)))
             ))

          ((cust :find-or-add key def-val)
           (with-db db
             (let ((me      self)
                   (old-val (maps:find db key self)))
               (cond ((eq old-val self)
                      (β _
                          (send dbmgr `(,β . ,me) :commit db (maps:add db key def-val))
                        (send cust def-val)))
                     (t
                      (send cust old-val))
                     ))))

          ((:show)
           (with-db db
             (sets:view-set db)
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
          
          ;; ---------------------------------
          
          ((:main-full-save)
           (send dbmgr 'maint-full-save))

          ))
        ))))

(defun ensure-file-exists (path)
  (handler-case
      (with-open-file (fd path
                          :direction :probe
                          :if-does-not-exist :error)
        (declare (ignore fd)))
    (error ()
      (let ((db (maps:empty)))
        (maps:addf db 'version  (uuid:make-v1-uuid))
        (maps:addf db 'kvdb-sequence (uuid:make-v1-uuid))
        (full-save path db)))
    ))

(defun kvdb-orchestrator-beh (&optional open-dbs)
  ;; Prevent duplicate kvdb Actors for the same file.
  (alambda
   ((cust :make-kvdb path)
    (ensure-file-exists path)
    (multiple-value-bind (dev ino)
        (um:get-ino path)
      (let* ((key  (um:mkstr dev #\space ino))
             (pair (find key open-dbs
                         :key  #'car
                         :test #'string-equal)))
        (cond (pair
               (send cust (cdr pair)))
              (t
               (let ((kvdb (%make-kvdb path)))
                 (become (kvdb-orchestrator-beh (acons key kvdb open-dbs)))
                 (send cust kvdb)))
              ))))
   ))

(deflex kvdb-maker (create (kvdb-orchestrator-beh)))

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
   ))

(defun local-updateable-proxy-for-remote-db-access-beh (kvdb orig-map upd-map)
  (alambda
   ((cust :add key val)
    (become (local-updateable-proxy-for-remote-db-access-beh
             kvdb orig-map
             (maps:add upd-map key val)))
    (send cust self))

   ((cust :remove key)
    (become (local-updateable-proxy-for-remote-db-access-beh
             kvdb orig-map
             (maps:remove upd-map key)))
    (send cust self))

   ((cust :find key . default)
    (send cust (maps:find upd-map key (car default))))

   ((cust :find-or-add key def-val)
    (let ((val (maps:find upd-map key self)))
      (cond ((eq val self)
             (become (local-updateable-proxy-for-remote-db-access-beh
                      kvdb orig-map
                      (maps:add upd-map key def-val)))
             (send cust def-val))
            (t
             (send cust val))
            )))

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
             (maps:add upd-map key val)
             owner))
    (send cust self))
   
   ((cust :remove key)
    (become (local-excl-proxy-for-remote-db-access-beh
             kvdb orig-map
             (maps:remove upd-map key)
             owner))
    (send cust self))

   ((cust :find key . default)
    (send cust (maps:find upd-map key (car default))))

   ((cust :find-or-add key def-val)
    (let ((val (maps:find upd-map key self)))
      (cond ((eq val self)
             (become (local-excl-proxy-for-remote-db-access-beh
                      kvdb orig-map
                      (maps:add upd-map key def-val)
                      owner))
             (send cust def-val))
            (t
             (send cust val))
            )))

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
   ))

;; -----------------------------------------------------
;; One to goof around in...

(deflex kvdb (lazy-fwd kvdb-maker :make-kvdb *db-path*))

;; -----------------------------------------------------------
#|
(ask kvdb :find :dave)
(uuid:when-created (ask kvdb :find 'version))
(uuid:when-created (call-actor kvdb :find 'version))
(send kvdb :show)

(dotimes (ix 5)
  (send kvdb println :add ix ix))
(send kvdb println :add :dave :chara)
(send kvdb println :add :cat "dog")
(send kvdb writeln :find :cat)
(send kvdb :show)
(dotimes (ix 10)
  (send kvdb println :remove ix))
(send kvdb println :add :tst (lambda* _))
(send kvdb writeln :find :tst)

(let ((m (maps:empty)))
  (setf m (maps:add m :dave :dog))
  (eql m (maps:add m :dave :dog)))
(let ((m (sets:empty)))
  (setf m (sets:add m :dave))
  (eql m (sets:add m :dave)))

(β (ans)
    (send kvdb β :find-or-add :bank-bal 10)
  (send fmt-println "Bal = ~A" ans))

(β (db)
    (send kvdb β :req)
  (maps:iter db (λ (k v)
                  (send writeln (list k v)))))

(let ((x '(1 2 3)))
  (send kvdb println :add :tst1 x)
  (send kvdb println :add :tst2 x))

(let ((x1 (ask kvdb :find :tst1))
      (x2 (ask kvdb :find :tst2)))
  (send println (list x1 x2 (eq x1 x2))))


(send kvdb :maint-full-save)

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
;; ---------------------------------------------------------
