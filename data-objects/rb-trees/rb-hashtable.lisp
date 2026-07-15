
(in-package :com.ral.rb-trees.hashtable)

(defstruct hash-table
  (test   'eql)
  (tree   (maps:empty)))

(defmethod rbht-hash (x)
  (sxhash x))

(defun cheat-to-string (x)
  (with-standard-io-syntax
    (rbht-hash (princ-to-string x))))
  
(defmethod rbht-hash ((x single-float))
  (cheat-to-string x))

(defmethod rbht-hash ((x double-float))
  (cheat-to-string x))

(defmethod rbht-hash ((x complex))
  (cheat-to-string x))

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
  (list (lookup tbl 'tom)
        (coerce (map 'vector #'code-char (ser:encode tbl)) 'string )))
|#