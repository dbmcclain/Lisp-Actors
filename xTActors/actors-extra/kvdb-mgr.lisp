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
  (with-actor-state state
    (match msg
      ;; -------------------
      ;; general entry for external clients
      ((cust :req)
       (send cust (state :db)))

      ((cust :find-multiple . keys)
       (send cust (mapcar (um:curry #'db-find (state :db)) keys)))

      ((cust :find key . default)
       (send cust (db-find (state :db) key (car default)) ))
      
      ;; -------------------
      ;; We are the only one that knows the identity of saver, so this
      ;; can't be forged by malicious clients. Also, a-db will only
      ;; eql db if there have been no updates within the last 10 sec.
      ((a-tag a-db) / (and (eql a-tag (state :saver))
                           (eql a-db  (state :db)))
       (send (state :saver) sink :save-log (state :db)))
      )))

;; ------------------------------
(defun trans-gate-beh (state)
  ;; General behavior of KVDB manager
  (with-actor-state state
    (labels ((upd (cust adb ans)
               (let ((new-db (db-add adb 'version (uuid:make-v1-uuid))))
                 ;; version key is actually 'com.ral.actors.kvdb::version
                 (send cust ans)
                 (send-after 10 self (state :saver) new-db)
                 (become (trans-gate-beh (state with
                                          :db  new-db)))
                 ))
             (add (cust key val)
               (let ((new-db  (db-add (state :db) key val)))
                 (upd cust new-db val))))
      (alambda
       ((cust :req-excl owner timeout) / (and (realp timeout)
                                              (plusp timeout))
        ;; ignored unless timeout is positive real number
        ;; request exclusive :commit access
        ;; customer must either :commit or :abort within timeout period
        (let ((fa-tag (tag self)))
          (send cust (state :db))
          (send-after timeout fa-tag 'forced-abort)
          (become (busy-trans-gate-beh (state with
                                         :owner  owner
                                         :fa-tag fa-tag)))
          ))
       
       ((cust :find-or-add key val)
        (let ((ans (db-find (state :db) key (state :db))))
          (if (eq ans (state :db))
              (add cust key val)
            ;; else
            (send cust ans))
          ))
              
       ((cust :add key val)
        (add cust key val))

       ((cust :remove key)
        (upd cust (db-remove (state :db) key) :ok))

       ((cust :add-multiple . keys-vals)
        ;; keys-vals should be an ALIST of (KEY . VAL) pairs
        (let ((new-db  (state :db)))
          (dolist (pair keys-vals)
            (setf new-db (db-add new-db (car pair) (cdr pair))))
          (if (eq new-db (state :db))
              (send cust :ok)
            (upd cust new-db :ok))
          ))

       ((cust :remove-multiple . keys)
        (let ((new-db (state :db)))
          (dolist (key keys)
            (setf new-db (db-remove new-db key)))
          (if (eq new-db (state :db))
              (send cust :ok)
            (upd cust new-db :ok))
          ))
       
       ;; -------------------
       ;; commit after update
       (( (cust . retry) :commit old-db new-db)
        (cond ((eql old-db (state :db))  ;; make sure we have correct version
               (cond ((eql new-db (state :db))
                      ;; no real change
                      (send cust :ok))
                     
                     (t
                      ;; changed db, so commit new
                      (upd cust new-db :ok))
                     ))
              
              (t
               ;; had wrong version for old-db
               (send retry (state :db)))
              ))
       
       (('maint-full-save)
        (let ((new-db (db-rebuild (state :db))))
          (become (trans-gate-beh (state with
                                    :db new-db)))
          (send (state :saver) sink :full-save new-db)))
       
       (msg
        (common-trans-beh msg state))
       ))))

;; ----------------------------------------------------------------
(defun busy-trans-gate-beh (state)
  ;; Behavior for exclusive access
  (with-actor-state state
    (behav (&rest msg)
      (labels ((release (cust new-db)
                 (send cust new-db)
                 (do-queue (msg (state :queue))
                   (send* self msg))
                 (become (trans-gate-beh (state with
                                                :queue nil
                                                :db    new-db) )))
               (stash ()
                 (become (busy-trans-gate-beh (state with
                                                :queue (addq (state :queue) msg)))
                         )))
        
        (match msg
          ((acust :req-excl an-owner _)
           (send acust
                 (if (eql an-owner (state :owner))
                     (state :db)
                   :fail)))
          
          ((acust :abort an-owner) / (eql an-owner (state :owner))
           ;; relinquish excl :commit ownership
           (release acust (state :db)))
          
          ((atag 'forced-abort)  / (eql atag (state :fa-tag))
           (release sink (state :db)))
          
          ;; -------------------
          ;; commit after update from owner, then relinquish excl :commit ownership
          (( (acust . an-owner) :commit old-db new-db) / (eql an-owner (state :owner))
           (cond ((eql old-db (state :db)) ;; make sure we have correct version
                  (cond ((eql new-db (state :db))
                         ;; no real change
                         (release acust (state :db)))
                        
                        (t
                         ;; changed db, so commit new
                         (let ((versioned-db (db-add new-db 'version (uuid:make-v1-uuid) )))
                           ;; version key is actually 'com.ral.actors.kvdb::version
                           (send-after 10 self (state :saver) versioned-db)
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
   ((a-tag :opened db) / (eq a-tag tag)
    (become (trans-gate-beh (actor-state
                             :db    db
                             :saver saver)))
    ;; now open for business, resubmit pending client requests
    (send-all-to self msgs))

   ((a-tag :error err) / (eq a-tag tag)
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
  (behav _
    (let ((tag-to-me (tag self))
          (saver (serializer
                  (create (unopened-database-beh ctrl-tag))
                  )))
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
           (send dbmgr cust :add key val))
                
          ((cust :remove key)
           (send dbmgr cust :remove key))
                
          ;; ---------------------------------------------
                
          ((cust :find key . default)
           (send dbmgr cust :find key (car default)))
                
          ((cust :find-or-add key def-val)
           (send dbmgr cust :find-or-add key def-val))

          ((cust :find-multiple . keys)
           (send* dbmgr cust :find-multiple keys))

          ((cust :add-multiple . keys-vals)
           (send* dbmgr cust :add-multiple keys-vals))

          ((cust :remove-multiple . keys)
           (send* dbmgr cust :remove-multiple keys))
          
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

