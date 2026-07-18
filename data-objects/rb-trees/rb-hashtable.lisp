;; rb-hashtable.lisp - Purely Functional Hash Table
;;
;; DM/RAL  2026/07/15T18:15:16U
;;
;; Never needs resizing nor rebuilding. But probably slower than the
;; imperative hash tables in Lisp.
;;
;; SXHASH (and variants) are used on the data key, is used to derive a
;; secondary RB-MAP key which locates an A-List of actual (key . val)
;; pairs.  This is similar to the collision handling used by Lisp's
;; built-in Hash Tables.
;;
;; The secondary SXHASH keys are FIXNUM elements with well defined
;; ordering. So, even if the data keys have no well defined ordering,
;; all they now need is an equality predicate that can distinguish
;; between them.
;;
;; What you lose in this scheme, versus an RB-MAP if the data keys do
;; have an order relation, is the nicely ordered processing of mapping
;; elements in ITER and FOLD.
;;
;; This purely functional hash table is slower than the imperative
;; version in Lisp. But simultaneous access by competing threads is
;; possible without interfering with each other for
;; adds/changes/removals - provided that the threads continue with
;; their own table ancestry chains. Otherwise, at some point, you may
;; need to coordinate updates of shared a global hash table.
;;
;; With FP Hash Tables you also get immediate reversion to an earlier
;; version of the table, if you want it. The original table remains
;; intact as long as you hold a reference to it. Copies of tables with
;; resulting from alterations use mostly shared structure with the
;; previous version of the table.
;;
;; --------------------------------------------

(in-package :com.ral.rb-trees.hashtable)

(defstruct (hash-table
            (:constructor %make-hash-table))
  (test  nil :read-only t)
  (tree  nil :read-only t))

(defvar +default-hash-table-map-type+
  (maps:make-map-type :compare-fn   '-
                      :replace-p-fn 'maps:/eql))

(defun make-hash-table (&key (test 'eql) (map-type +default-hash-table-map-type+))
  (%make-hash-table
   :test  test
   :tree  (maps:make-map :map-type map-type)))

(defgeneric rbht-hash (x)
  (:method (x)
   (sxhash x))
  (:method ((x single-float))
   (cheat-to-string x))
  (:method ((x double-float))
   (cheat-to-string x))
  (:method ((x complex))
   (cheat-to-string x))
  (:method ((x uuid:uuid))
   (cheat-to-string x)))

(defun cheat-to-string (x)
  (with-standard-io-syntax
    (rbht-hash (princ-to-string x))))
  

(defun find-pair (tbl key)
  (with-slots (test tree) tbl
    (let* ((hkey  (rbht-hash key))
           (entry (maps:find tree hkey)))
      (values (assoc key entry :test test)
              hkey
              entry)
      )))
  
(defun gethash (key tbl &optional default)
  (let ((pair (find-pair tbl key)))
    (if pair
        (values (cdr pair) t)
      default)))

(defun add (tbl key val)
  (with-slots (tree) tbl
    (multiple-value-bind (pair hkey entry) (find-pair tbl key)
      (let ((new   (acons key val (remove pair entry))))
        (um:with tbl
             :tree  (maps:add tree hkey new))
        ))))

(define-modify-macro addf (key val)
  add)

(defsetf gethash (key tbl &optional default) (val)
  (declare (ignore default))
  `(add ,tbl ,key ,val))

#|
(incf (gethash (normalize-key "abc") tbl 0) 5)
(setf (gethash "abc" tbl 0) 5)
(addf tbl "abc" 15)
|#

(defun trim (tbl key)
  (with-slots (tree) tbl
    (multiple-value-bind (pair hkey entry) (find-pair tbl key)
      (if pair
          (let ((new   (remove pair entry)))
            (um:with tbl
                 :tree  (maps:add tree hkey new)))
        ;; else
        tbl))))

(define-modify-macro trimf (key)
  trim)

(defun remhash (key tbl)
  (trim tbl key))

(defun clrhash (tbl)
  (um:with tbl
    :tree (funcall (hash-table-tree tbl) :new-tree)))

(defun maphash (fn tbl)
  (with-slots (tree) tbl
    (maps:iter tree (lambda (_ v)
                      (declare (ignore _))
                      (dolist (pair v)
                        (funcall fn (car pair) (cdr pair)))
                      ))
    ))

(defun stats (tbl)
  (with-slots (tree) tbl
    (let* ((maxct   0)
           (buckets (mapcar (lambda (pair)
                              (let* ((ent (cdr pair))
                                     (nel (length ent)))
                                (when (> nel maxct)
                                  (setf maxct nel))
                                nel))
                            (maps:elements tree)))
           (ans     (make-array (1+ maxct)
                                :element-type 'fixnum
                                :initial-element 0)))
      (dolist (ct buckets)
        (incf (aref ans ct)))
      (list
       :buckets  (length buckets)
       :stats    ans)
      )))

(defun tst (&key (nel 1_000_000))
  (let ((tbl (make-hash-table)))
    (dotimes (ix nel)
      (addf tbl (princ-to-string (random 1f0)) t)) ;; (random 100_000_000_000) t)))
    (list (stats tbl) tbl)))
#|
(inspect (tst :nel 1_000))
|#
;; --------------------------------------------
#|
(let* ((tbl (make-hash-table)))
  (dolist (pair '((dog . cat)
                  (tom . jerry)
                  (abbot . costello)
                  (laurael . hardy)))
    (addf tbl (car pair) (cdr pair)))
  ;; (inspect tbl)
  (inspect (ser:decode (ser:encode tbl)))
  (list (gethash 'tom tbl)
        (coerce (map 'vector #'code-char (ser:encode tbl)) 'string )))
|#
;; -------------------------------------------------------------
#|
(defparameter *tst-coll*
  (let ((arr (make-array 1_000_000
                         :element-type 'single-float)))
    (dotimes (ix (length arr))
      (setf (aref arr ix) (random 1f0)))
    arr))

(defun tst ()
  (let ((tbl (make-hash-table)))
    (dotimes (ix (length *tst-coll*))
      (setf (gethash (aref *tst-coll* ix) tbl) t))
    (maps:cardinal (hash-table-tree tbl))))
(time (tst))

(defun tsth ()
  (let ((tbl (cl:make-hash-table)))
    (dotimes (ix (length *tst-coll*))
      (setf (cl:gethash (aref *tst-coll* ix) tbl) t))
    tbl))
(time (tsth))

;; Whoa!! Hashtable is 15x faster than RB-Trees, uses 20x less alloc,
;; and 10x less page faults
;;
;; Timing with (:TYPE VECTOR) NODE Struct:
;;               μs/add       Alloc bytes    Page Faults
;;               ------       -----------    -----------
;;    RB-Trees:  2.8              920           0.048
;; Hash-tables:  0.19              52           0.005
;;
;; Take away the (:TYPE VECTOR) in the NODE Struct:
;;    RB-Trees:  2.2             1075           0.031
;; Hash-tables:  0.13              53           0.004
|#
;; --------------------------------------------

#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((tbl hash-table) (mode (eql 'stats)))
  (declare (ignore mode))
  (um:nlet iter ((lst   (stats tbl))
                 (names nil)
                 (vals  nil))
    (if (endp lst)
        (values (nreverse names)
                (nreverse vals)
                nil nil 'hash-table)
      (go-iter (cddr lst)
               (cons (car lst)  names)
               (cons (cadr lst) vals))
      )))

