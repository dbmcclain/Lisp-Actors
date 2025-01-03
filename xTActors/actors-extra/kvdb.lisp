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
    ;; The call to ENSURE-FILE-EXISTS might produce an error. Since we
    ;; are behind a Serializer, we need to send that error code back
    ;; to the customer.
    ;;
    ;; OTOH, might not actually cause an error, but a timeout could
    ;; happen waiting on a user Y-or-N response. Customer will receive
    ;; the +timed-out+ message. In that case we are happy, and the
    ;; sender just needs to reissue the request.
    ;;
    (handler-bind
        ((error (lambda (e)
                  (send-to-pool cust :error e)
                  (error e))
                ))
      (let* ((key   (ino-key path)) ;; might trigger error
             (quad  (and key
                         (find key open-dbs
                               :key  #'open-database-ino-key
                               :test #'string-equal))))
        (if quad
              (send cust (open-database-kvdb-actor quad))
          ;; else - new Open
          (let* ((tag-to-orch  (tag self))
                 (kvdb         (%make-kvdb tag-to-orch path)))
                    (send cust kvdb)
                    (become (kvdb-orchestrator-beh
                             (cons (make-open-database
                                    :ino-key    key
                                    :orch-tag   tag-to-orch
                                    :kvdb-actor kvdb
                                    :path       path)
                                   open-dbs)))
                    ))
        )))
   
   ((atag :update-entry)
    ;; when actual inode changes, as with full-save
    (let ((quad (find atag open-dbs
                      :key #'open-database-orch-tag)))
      (when quad
        (let* ((path    (open-database-path quad))
               (old-key (open-database-ino-key quad))
               (key     (ino-key path)))
          (unless (and old-key
                       (string-equal key old-key))
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

(deflex* kvdb-orchestrator
  (create (kvdb-orchestrator-beh)))

;; --------------------------------------------

(defun kvdb-gateway (path)
  (labels ((initial-beh (&rest msg)
             (let ((tag  (tag self)))
               (become (stashing-beh tag (list msg)))
               (send kvdb-orchestrator tag :make-kvdb path)
               ))
             
           (stashing-beh (tag msgs)
             (alambda
              ((atag a-kvdb) / (eq atag tag)
               (become (fwd-beh a-kvdb))
               (send-all-to self msgs))

              ((atag . _) / (eq atag tag)
               ;; error situation
               (become #'initial-beh))
              
              (msg
               (become (stashing-beh tag (cons msg msgs))))
              )))
    (create #'initial-beh)
    ))
  
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

(deflex* kvdb
  ;; The main Actor for just the goof-around KVDB. Make your own for
  ;; others.
  (kvdb-gateway *db-path*))
                 
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

;; --------------------------------------------

(defun* lw-start-kvdb _
  (setf kvdb-orchestrator  (create (kvdb-orchestrator-beh)))
  (setf kvdb               (kvdb-gateway *db-path*)))

(defun* lw-kill-kvdb _
  (princ "Allowing KVDB to sync before shutting down.")
  (sleep 11)) ;; give us time to sync

#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up KVDB"
                    'lw-start-kvdb
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop KVDB"
                    'lw-kill-kvdb)

  (lw:define-action "Save Session After"
                    "Restart KVDB"
                    'lw-start-kvdb)
  )


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
