;; transactional-db.lisp -- transactional database processing in Actors
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------
;; Using an FPL-pure RB-tree as the database. Hence the database
;; serves as its own transaction ID. Lock-free design.

(in-package :ac)
  
(defun trans-gate-beh (tag-commit tag-rollback tag-write saver db)
  (flet ((try (cust target args)
           (send* target db tag-commit tag-rollback cust target args)))
    (alambda
     ((cust :req target . args)
      (try cust target args))
     
     ((atag db-old db-new cust retry-target . args) when (eql atag tag-commit)
      (cond ((eql db-old db)
             (let ((tag-write (tag self)))
               (become (trans-gate-beh tag-commit tag-rollback tag-write saver db-new))
               (send-after 10 tag-write)
               (send cust :ok)))
            (t
             (try cust retry-target args))
            ))
     
     ((atag cust retry-target . args) when (eql atag tag-rollback)
      (try cust retry-target args))

     ((atag) when (eql atag tag-write)
      (send (io saver) :save db))

     ((atag :shutdown) when (eql atag saver)
      (send (io saver) :save db)
      (become (sink-beh)))
     )))

(defun nascent-database-beh (custs saver)
  (alambda
   ((atag :open db) when (eql atag saver)
    (let ((tag-write  (tag self))
          (tag-commit (tag self))
          (tag-retry  (tag self)))
      (become (trans-gate-beh tag-commit tag-retry tag-write saver db))
      (dolist (cust custs)
        (send* self cust))
      ))

   (msg
    (become (nascent-database-beh (cons msg custs) saver)))
   ))

;; -----------------------------------------------------------

(defvar *db-id*          #/uuid/{6f896744-6472-11ec-8ecb-24f67702cdaa})
(defvar *unstorable-key* #/uuid/{e5ae746c-6479-11ec-8ecb-24f67702cdaa})

(defun save-database-beh (trans-gate path last-db)
  (alambda
   ((:open db-path)
    (let ((db (maps:empty)))
      (ignore-errors
        (with-open-file (f db-path
                           :direction         :input
                           :if-does-not-exist :error
                           :element-type      '(unsigned-byte 8))
          (let* ((sig  (uuid:uuid-to-byte-array *db-id*))
                 (id   (make-array (length sig)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
            (read-sequence id f)
            (when (equalp id sig)
              (setf db (loenc:deserialize f)))
            )))
      (become (save-database-beh trans-gate db-path db))
      (send (par-safe trans-gate) self :open db)))
   
   ((:save new-db) when (not (eql new-db last-db))
    (ensure-directories-exist path)
    (let ((trimmed (maps:remove new-db *unstorable-key*)))
      (with-open-file (f path
                         :direction         :output
                         :if-exists         :new-version
                         :if-does-not-exist :create
                         :element-type      '(unsigned-byte 8))
        (write-sequence (uuid:uuid-to-byte-array *db-id*) f)
        (loenc:serialize trimmed f)
        (become (save-database-beh trans-gate path new-db)))))
   ))

;; -----------------------------------------------------------

(defvar *db-path*  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                                    (sys:get-folder-path :appdata)))

(defun make-trans-gate (db-path)
  (actors ((trans  (nascent-database-beh nil saver))
           (saver  (save-database-beh trans nil nil)))
    (send (io saver) :open db-path)
    (lw:define-action "When quitting image"
                      "Save database"
                      (lambda (&rest args)
                        (declare (ignore args))
                        (send trans saver :shutdown)))
    trans))

(defvar *db* (make-trans-gate *db-path*))

;; -----------------------------------------------------------

(defun add-rec (cust key val)
  (send *db* cust :req
        (actor (db commit rollback &rest retry-info)
          (declare (ignore rollback))
          (let ((new-db (handler-case
                            (progn
                              (loenc:encode val) ;; tickle any possible encoding error
                              (maps:add db key val))
                          (error ()
                            (let* ((db          (maps:remove db key))
                                   (unstorables (or (maps:find db *unstorable-key*)
                                                    (maps:empty))))
                              (maps:add db *unstorable-key* (maps:add unstorables key val))
                              ))
                          )))
            (send* commit db new-db retry-info))
          )))

(defun remove-rec (cust key)
  (send *db* cust :req
        (actor (db commit rollback &rest retry-info)
          (declare (ignore rollback))
          (let* ((new-db      (maps:remove db key))
                 (unstorables (maps:find new-db *unstorable-key*)))
            (when unstorables
              (let ((new-unstorables (maps:remove unstorables key)))
                (unless (eq new-unstorables unstorables)
                  (setf new-db (maps:add new-db *unstorable-key* new-unstorables))
                  )))
            (send* commit db new-db retry-info)))
        ))

(defun lookup (cust key &optional default)
  (send *db* cust :req
        (actor (db &rest ignored)
          (declare (ignore ignored))
          (let ((val (maps:find db key self)))
            (when (eq val self)
              (let ((unstorables (or (maps:find db *unstorable-key*)
                                     (maps:empty))))
                (setf val (maps:find unstorables key default))
                ))
            (send cust val)
            ))))

(defun show-db ()
  (send *db* nil :req
        (actor (db &rest ignored)
          (declare (ignore ignored))
          (sets:view-set db))))

;; ------------------------------------------------------------------
;; more usable public face - can use ASK against this

(def-singleton-actor kvdb ()
  (make-actor
   (alambda
    ((cust :lookup key . default)
     (apply #'lookup cust key default))

    ((cust :add key val)
     (add-rec cust key val))

    ((cust :remove key)
     (remove-rec cust key))

    ((cust :req action-actor)
     (repeat-send *db*))
    )))


#|
(ask (kvdb) :lookup :dave)
(dotimes (ix 5)
  (add-rec println ix ix))
(add-rec println :dave :chara)
(add-rec println :cat "dog")
(lookup writeln :cat)
(show-db)
(dotimes (ix 10)
  (remove-rec println ix))
(add-rec println :tst (lambda (&rest args)
                        (declare (ignore args))
                        ))
(lookup writeln :tst)

(let ((m (maps:empty)))
  (setf m (maps:add m :dave :dog))
  (eql m (maps:add m :dave :dog)))
(let ((m (sets:empty)))
  (setf m (sets:add m :dave))
  (eql m (sets:add m :dave)))
|#               
