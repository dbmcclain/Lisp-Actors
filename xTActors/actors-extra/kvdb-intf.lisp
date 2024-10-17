;; kvdb-intf.lisp -- Interface to raw in-memory database structs
;; DM/RAL  2024/07/13 18:03:02 UTC
;; -----------------------------------------------------------

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
  
   [ Non-Liberal or Strict Persistence - Pure Lisp data structures
   can be persisted. But unnamed compiled functions,
   functional closures, and some system level objects, as well as data
   structures that contain them, cannot be persisted.
  
   So this excludes open streams, Actors, and many other low-level
   things.  Liberal Persistence substitutes proxy objects in their
   place.  Non-liberal Persistence throws an error.
  
   By special arrangement, Actors can be ferried across a network
   connection using substitution by special proxy Actor designators.
   But they cannot be persisted to disk.]
  
   If two persistable items are EQ when added to the KV database, they
   are restored to EQ upon retrieval in future sessions.
|#
;; -----------------------------------------------------------

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
  #+:LISPWORKS
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
  (apply #'fplht:make-fpl-hashtable
         :test 'equal
         #+:LISPWORKS
         '(:single-thread t)
         #-:LISPWORKS
         '()))

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

