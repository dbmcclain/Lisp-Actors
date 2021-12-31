;; transactional-db.lisp -- transactional database processing in Actors
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------
;; Using an FPL-pure RB-tree as the database. Hence the database
;; serves as its own transaction ID. Lock-free design.

(in-package com.ral.actors.kv-database)
  
(defun trans-gate-beh (tag-commit tag-rollback tag-write saver db)
  (flet ((try (cust target args)
           (send* target db tag-commit tag-rollback cust target args)))
    (lambda* msg
      (with-sponsor base-sponsor
        (match msg
          ((cust :req target . args)
           (try cust target args))
     
          ((atag db-old db-new cust retry-target . args) when (eql atag tag-commit)
           (cond ((eql db-old db)
                  (unless (eql db-old db-new)
                    (let ((tag-write    (tag self))
                          (versioned-db (maps:add db-new 'version (uuid:make-v1-uuid))))
                      ;; version key = 'com.ral.actors.kv-database::version
                      (become (trans-gate-beh tag-commit tag-rollback tag-write saver versioned-db))
                      (send-after 10 tag-write)))
                  (send cust :ok))
                 
                 (t
                  (try cust retry-target args))
                 ))
     
          ((atag cust retry-target . args) when (eql atag tag-rollback)
           (try cust retry-target args))
          
          ((atag) when (eql atag tag-write)
           (send saver :save db))
          )))))

(defun nascent-database-beh (custs saver)
  (lambda* msg
    (with-sponsor base-sponsor
      (match msg
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
        ))))

;; -----------------------------------------------------------

(defconstant +db-id+  #/uuid/{6f896744-6472-11ec-8ecb-24f67702cdaa})

(defun save-database-beh (trans-gate path last-db)
  (lambda* msg
    (with-sponsor slow-sponsor
      (match msg
        ((:open db-path)
         (let ((db (maps:empty)))
           (ignore-errors
             (with-open-file (f db-path
                                :direction         :input
                                :if-does-not-exist :error
                                :element-type      '(unsigned-byte 8))
               (let* ((sig  (uuid:uuid-to-byte-array +db-id+))
                      (id   (make-array (length sig)
                                        :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
                 (read-sequence id f)
                 (when (equalp id sig)
                   (setf db (loenc:deserialize f)))
                 )))
           (become (save-database-beh trans-gate db-path db))
           (send trans-gate self :open db)))
        
        ((:save new-db) when (not (eql new-db last-db))
         (ensure-directories-exist path)
         (let ((trimmed (remove-unstorable new-db)))
           (with-open-file (f path
                              :direction         :output
                              :if-exists         :rename
                              :if-does-not-exist :create
                              :element-type      '(unsigned-byte 8))
             (write-sequence (uuid:uuid-to-byte-array +db-id+) f)
             (loenc:serialize trimmed f)
             (become (save-database-beh trans-gate path new-db)))))
        ))))

(defun remove-unstorable (map)
  (maps:fold map (lambda (key val accu)
                   (handler-case
                       (progn
                         (loenc:encode (list key val))
                         (maps:add accu key val))
                     (error ()
                       accu)))
             (maps:empty)))

(defun deep-copy (obj)
  (loenc:decode (loenc:encode obj)))

;; -----------------------------------------------------------

(defvar *db-path*  (merge-pathnames "LispActors/Actors Transactional Database.dat"
                                    (sys:get-folder-path :appdata)))

(def-singleton-actor db ()
  (actors ((trans  (nascent-database-beh nil saver))
           (saver  (save-database-beh trans nil nil)))
    (send saver :open *db-path*)
    trans))

;; -----------------------------------------------------------

(defun add-rec (cust key val)
  (send (db) cust :req
        (actor (db commit rollback &rest retry-info)
          (declare (ignore rollback))
          (send* commit db (maps:add db key val) retry-info))
        ))

(defun remove-rec (cust key)
  (send (db) cust :req
        (actor (db commit rollback &rest retry-info)
          (declare (ignore rollback))
          (send* commit db (maps:remove db key) retry-info))
        ))

(defun lookup (cust key &optional default)
  (send (db) cust :req
        (actor (db &rest ignored)
          (declare (ignore ignored))
          (send cust (maps:find db key default)))
        ))

(defun show-db ()
  (send (db) nil :req
        (actor (db &rest ignored)
          (declare (ignore ignored))
          (sets:view-set db))))

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
          
          ((cust :req action-actor)
           (repeat-send (db)))
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

(send (db) nil :req
      (actor (db . _)
        (maps:iter db (lambda (k v)
                        (send writeln (list k v))))))
|#               
