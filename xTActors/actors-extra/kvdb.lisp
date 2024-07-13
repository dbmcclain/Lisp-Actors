;; kvdb.lisp -- transactional Key-Value database processing in Actors
;;
;; Based on FPL Hashtables. This version just might be faster than
;; using RB-Trees, since we no longer have the overhead requirement of
;; key orderability with ORD. But we do have key normalization, and as
;; a result, all keys would be orderable.
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------

(in-package #:com.ral.actors.kvdb)

#|
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

;; ----------------------------------------------------------------

(defun common-trans-beh (msg state)
  ;; Common behavior for general and exclusive use.
  (with-state-vals ((db    :db)
                    (saver :saver)) state
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
      )))

;; ------------------------------

(defun trans-gate-beh (state)
  ;; General behavior of KVDB manager
  (with-state-vals ((db    :db)
                    (saver :saver)) state
    (alambda
     ((cust :req-excl owner timeout) / (and (realp timeout)
                                            (plusp timeout))
      ;; ignored unless timeout is positive real number
      ;; request exclusive :commit access
      ;; customer must either :commit or :abort within timeout period
      (let ((fa-tag (tag self)))
        (send cust db)
        (send-after timeout fa-tag 'forced-abort)
        (become (busy-trans-gate-beh (with state
                                       :owner  owner
                                       :fa-tag fa-tag)))
        ))
     
     ;; -------------------
     ;; commit after update
     (( (cust . retry) :commit old-db new-db)
      (cond ((eql old-db db)  ;; make sure we have correct version
             (cond ((eql new-db db)
                    ;; no real change
                    (send cust db))
                   
                   (t
                    ;; changed db, so commit new
                    (let ((versioned-db (db-add new-db 'version (uuid:make-v1-uuid) )))
                      ;; version key is actually 'com.ral.actors.kvdb::version
                      (become (trans-gate-beh (with state
                                                :db versioned-db)))
                      (send-after 10 self saver versioned-db)
                      (send cust versioned-db)))
                   ))
            
            (t
             ;; had wrong version for old-db
             (send retry db))
            ))
     
     (('maint-full-save)
      (let ((new-db (db-rebuild db)))
        (become (trans-gate-beh (with state
                                  :db new-db)))
        (send saver sink :full-save new-db)))
     
     (msg
      (common-trans-beh msg state))
     )))

;; ----------------------------------------------------------------

(defun busy-trans-gate-beh (state)
  ;; Behavior for exclusive access
  (with-state-vals ((db     :db)
                    (saver  :saver)
                    (owner  :owner)
                    (fa-tag :fa-tag)
                    (queue  :queue)) state
    (lambda (&rest msg)
      (labels ((release (cust new-db)
                 (send cust new-db)
                 (do-queue (msg queue)
                   (send* self msg))
                 (become (trans-gate-beh (actor-state
                                          :db    new-db
                                          :saver saver))))
               
               (stash ()
                 (become (busy-trans-gate-beh (with state
                                                :queue (addq queue msg)))
                         )))
        
        (match msg
          ((acust :req-excl an-owner _)
           (send acust
                 (if (eql an-owner owner)
                     db
                   :fail)))
          
          ((acust :abort an-owner) / (eql an-owner owner)
           ;; relinquish excl :commit ownership
           (release acust db))
          
          ((atag 'forced-abort)  / (eql atag fa-tag)
           (release sink db))
          
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
                           ;; version key is actually 'com.ral.actors.kvdb::version
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
           (common-trans-beh msg state))
          )))))

;; ----------------------------------------------------------------

(defun nascent-database-beh (tag saver msgs)
  ;; Behavior of the manager while awaiting KVDB open. Just stash
  ;; messages to the KVDB until that time.

  ;; -------------------
  ;; We are the only one that knows the identity of TAG and SAVER. So
  ;; this message could not have come from anywhere except SAVER
  ;; itself.
  (alambda
   ((a-tag :opened db) / (eql a-tag tag)
    (become (trans-gate-beh (actor-state
                             :db    db
                             :saver saver)))
    ;; now open for business, resubmit pending client requests
    (send-all-to self msgs))

   ((a-tag :error err) / (eql a-tag tag)
    ;; Who knows what will happen with this response? But probably
    ;; better than just going silent... Orchestrator will eventually
    ;; remove references to us.
    (become (const-beh err))
    (send-all-to self msgs))
   
   ;; -------------------
   ;; accumulate client requests until we open for business
   (msg
    (become (nascent-database-beh tag saver (cons msg msgs) )))
   ))

;; -----------------------------------------------------------

(defun db-svc-init-beh (ctrl-tag path)
  ;; Factory function to produce a lazy behavior function. It waits
  ;; until a message arrives before actually opening the backing store
  ;; to the KVDB.
  (λ _
    (let ((tag-to-me (tag self))
          (saver (serializer (create (unopened-database-beh ctrl-tag))
                             :timeout 5)
                 ))
      (send saver tag-to-me :open path)
      (become (nascent-database-beh tag-to-me saver nil))
      (repeat-send self))))

;; -----------------------------------------------------------

#+:LISPWORKS
(editor:setup-indent "with-db" 1)

(defun %make-kvdb (ctrl-tag path)
  ;; Factory function to create the main interface to an underlying
  ;; transactional KVDB.
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
             (let ((map  (maps:empty))
                   (keys nil))
               (db-map db
                       (lambda (k v)
                         (push k keys)
                         (maps:addf map k v)))
               #+:LISPWORKS
               (sets:view-set map)
               (send* writeln
                      (loop for key in (sort keys #'string< :key #'key-to-string)
                            collect
                              (list key (maps:find map key))))
               )))
                
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
                
          ((_ :req-excl _ _)
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

;; ----------------------------------------------------------------
;; Multiple KVDB Coordination - Orchestrator
;;
;; Keeps a record of all opened KVDB systems, and updates these
;; entries when a fresh file is constructed, replacing its old backing
;; store.
;;
;; Regardless of the path used to reach a file (synonyms may exist),
;; the underlying file is identified by its Device+Inode pair. This
;; prevents having more than one KVDB manager for the same underlying
;; file.

(defstruct open-database
  ino-key     ;; the unique DEVICE+INODE identity of the physical backing store
  orch-tag    ;; the unique TAG used for opening this KVDB
  kvdb-actor  ;; the Actor in charge of KVDB management for this KVDB
  path)       ;; the pathname to the backing store

(defun kvdb-orchestrator-beh (&optional open-dbs)
  ;; Prevent duplicate kvdb Actors for the same file.
  (alambda
   ((cust :make-kvdb path)
    ;; The call to ENSURE-FILE-EXISTS might produce an error. If so,
    ;; we will exit immediately, and the Serializer we are behind will
    ;; open back up on a time-out.
    (ensure-file-exists path)
    (let* ((key   (ino-key path))
           (quad  (find key open-dbs
                        :key  #'open-database-ino-key
                        :test #'string-equal)))
      (if quad
          (send cust (open-database-kvdb-actor quad))
        ;; else - new Open
        (let* ((tag-to-me  (tag self))
               (kvdb       (%make-kvdb tag-to-me path)))
          (become (kvdb-orchestrator-beh
                   (cons (make-open-database
                          :ino-key    key
                          :orch-tag   tag-to-me
                          :kvdb-actor kvdb
                          :path       path)
                         open-dbs)))
          (send cust kvdb)))
      ))
   
   ((atag :update-entry)
    ;; when actual inode changes, as with full-save
    (let ((quad (find atag open-dbs
                      :key #'open-database-orch-tag)))
      (when quad
        (let* ((path  (open-database-path quad))
               (key   (ino-key path)))
          (unless (string-equal key (open-database-ino-key quad))
            (let* ((kvdb    (open-database-kvdb-actor quad))
                   (new-dbs (cons (make-open-database
                                   :ino-key    key
                                   :orch-tag   atag
                                   :kvdb-actor kvdb
                                   :path       path)
                                  (remove quad open-dbs))))
              (become (kvdb-orchestrator-beh new-dbs))
              ))
          ))
      ))
   
   ((atag :remove-entry)
    (let ((quad (find atag open-dbs
                      :key #'open-database-orch-tag)))
      (when quad
        (become (kvdb-orchestrator-beh (remove quad open-dbs))))
      ))
   ))
  
(deflex kvdb-maker
  (serializer ;; because we are doing file ops?
   (create (kvdb-orchestrator-beh))
   :timeout 5))

;; -----------------------------------------------------
;; One to goof around in...

(defvar *db-path*
  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                   #+:LISPWORKS
                   (sys:get-folder-path :appdata)
                   #+:sbcl
                   (merge-pathnames "Library/Application Support/"
                                    (format nil "~A/" (sb-ext:posix-getenv "HOME")))
                   ))

(deflex kvdb
  ;; The main Actor for just the goof-around KVDB. Make your own for
  ;; others.
  (fut kvdb-maker :make-kvdb *db-path*))

;; -----------------------------------------------------------
;; Utility Functions

(defun inspect-kvdb ()
  (β (map)
      (send kvdb β :really-let-me-see-map)
    (inspect map)))

(defun key-to-string (key &rest options)
  ;; Also used for value displays
  (with-output-to-string (s)
    (with-maximum-io-syntax
      (apply #'write key :stream s options))
    ))

(defun collect-keys (cust)
  (let+ ((:β (db)  (racurry kvdb :req))
         (keys   nil))
    (db-map db
            (lambda (k v)
              (declare (ignore v))
              (push k keys)))
    (send cust (sort keys #'string< :key #'key-to-string))
    ))

;; -----------------------------------------------------
;; more goofing around...
;;
;; These won't become permanent until/unless :maint-full-save

(defun convert-db (new-db-proto)
  (let+ ((:β (db)  (racurry kvdb :req))
         (new-db new-db-proto))
    (db-map db (lambda (k v)
                 (setf new-db (db-add new-db k v))))
    (send kvdb `(,writeln . ,self) :commit db new-db)))
            
(defun become-fplht ()
  (convert-db (apply #'fplht:make-fpl-hashtable
                     :test 'equal
                     #+:LISPWORKS '(:single-thread t)
                     #-:LISPWORKS '())))

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
