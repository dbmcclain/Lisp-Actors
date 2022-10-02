
(in-package :com.ral.actors.base)

;; -------------------------------------------
;; A cheap FP Banker's queue
;; When all you need is ADDQ, PUSHQ, POPQ...

(defconstant +emptyq+ (list nil)) ;; strictly speaking, but NIL is okay in CL too.
(defconstant +doneq+  #())

(defun normq (q)
  (if (car q)
      q
    (list (reverse (cdr q)))))

(defun addq (q item)
  ;; add item to tail, return new queue
  (normq (cons (car q) (cons item (cdr q)))))

(defun pushq (q item)
  ;; add item to head, return new queue
  (cons (cons item (car q)) (cdr q)))

(defun popq (q)
  (if (car q)
      (values (caar q)
              (normq (cons (cdar q) (cdr q))))
    +doneq+))

(defun emptyq? (q)
  (null (car q)))

(defun iterq (q fn)
  (um:nlet iter ((q q))
    (unless (emptyq? q)
      (multiple-value-bind (item new-q)
          (popq q)
        (funcall fn item)
        (go-iter new-q))
      )))

(defmacro do-queue ((item q) &body body)
  `(iterq ,q (lambda (,item) ,@body)))

