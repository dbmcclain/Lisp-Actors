;; fpl-hashtable.lisp
;;
;; So, can we make a hashtable that behaves as a persistent FPL data
;; struct?  It would appear that we can! Isn't Lisp wonderful?!!
;;
;; DM/RAL  2022/11/14 05:10:41
;; ----------------------------------

(defpackage #:com.ral.useful-macros.fpl-hashtable
  (:use #:common-lisp)
  (:export
   #:make-fpl-hashtable
   #:fpl-gethash
   #:fpl-sethash
   #:fpl-gethash-or-add
   #:fpl-remhash
   #:rebuild-fpl-hashtable
   #:fpl-maphash
   #:fpl-get-keys
   ))

(in-package #:com.ral.useful-macros.fpl-hashtable)

;; ----------------------------------

(defstruct (fpl-hashtable
            (:constructor %make-fpl-hashtable))
  htbl alst create-args)

(defun make-fpl-hashtable (&rest args)
  (%make-fpl-hashtable
   :htbl        (apply #'make-hash-table args)
   :alst        nil
   :create-args args))

;; NOTE: We use local symbol '+KEY-REMOVED+ as the sentinel for a
;; removed key in the table. This is unique and serializes back to
;; itself on restore. Using a naked vector value would not deserialize
;; back to itself.

(defun fpl-gethash (tbl key &optional default)
  ;; -> val, present-p
  (with-accessors ((htbl  fpl-hashtable-htbl)
                   (alst  fpl-hashtable-alst)) tbl
    (let ((pair (assoc key alst
                       :test (hash-table-test htbl))))
      (if pair
          (let ((val (cdr pair)))
            (if (eq val '+key-removed+)
                (values default nil)
              (values val t)))
        (gethash key htbl default)
        ))))

(defun fpl-sethash (tbl key val)
  ;; -> new-tbl
  (let ((new-tbl (copy-fpl-hashtable tbl)))
    (push (cons key val) (fpl-hashtable-alst new-tbl))
    new-tbl))

(defsetf fpl-gethash fpl-sethash)

(defun fpl-gethash-or-add (tbl key val)
  ;; => val, new-tbl
  (multiple-value-bind (tbl-val present-p)
      (fpl-gethash tbl key)
    (if present-p
        (values tbl-val tbl)
      (values val (fpl-sethash tbl key val)))
    ))

(defun fpl-remhash (tbl key)
  ;; -> new-tbl
  (fpl-sethash tbl key '+key-removed+))

(defun rebuild-fpl-hashtable (tbl)
  ;; -> new-tbl
  (with-accessors ((alst        fpl-hashtable-alst)
                   (htbl        fpl-hashtable-htbl)
                   (create-args fpl-hashtable-create-args)) tbl
    (let* ((new-fpl-tbl (apply #'make-fpl-hashtable create-args))
           (new-htbl    (fpl-hashtable-htbl new-fpl-tbl)))
      (maphash (lambda (k v)
                 (setf (gethash k new-htbl) v))
               htbl)
      (dolist (pair (reverse alst))
        (destructuring-bind (key . val) pair
          (if (eq val '*+key-removed+)
              (remhash key new-htbl)
            (setf (gethash key new-htbl) val))
          ))
      new-fpl-tbl)))

(defun fpl-maphash (tbl fn)
  (let ((wrk (if (fpl-hashtable-alst tbl)
                 (rebuild-fpl-hashtable tbl)
               tbl)))
    (maphash fn (fpl-hashtable-htbl wrk))
    ))

(defun fpl-get-keys (tbl)
  (let (keys)
    (fpl-maphash tbl (lambda (k v)
                       (declare (ignore v))
                       (push k keys)))
    keys))

