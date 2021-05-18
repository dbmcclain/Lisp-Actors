;; Banker's Queue = A simple, purely functional, queue with head/tail insertion / extraction
;;
;; DM/RAL  05/21
;; ---------------------------------------------------

(defpackage #:bankers-queue
  (:use :common-lisp)
  (:export
   #:q-add
   #:q-pop
   #:q-push
   #:q-get))

(in-package #:bankers-queue)

;; ---------------------------------------------------
;; Purely Functional Banker's Queue
;; Empty Queue is NIL

(defun q-norm (q)
  (if (car q)
      q
    (when (cdr q)
      (list (reverse (cdr q))) )))

(defun q-add (q x)
  (q-norm (cons (car q) (cons x (cdr q)))))

(defun q-pop (q)
  (if q
      (values (caar q)
              (q-norm (cons (cdar q) (cdr q))))
    (error "Empty Queue")))

(defun q-push (q x)
  (cons (cons x (car q)) (cdr q)))

(defun q-get (q)
  (when q
    (if (cdr q)
        (values (cadr q)
                (cons (car q) (cddr q)))
      (let ((new-tl (reverse (car q))))
        (values (car new-tl)
                (qnorm (cons nil (cdr new-tl))))
        ))))

