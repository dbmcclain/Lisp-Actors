
(in-package :com.ral.actors.base)

;; -------------------------------------------
;; A cheap FP Banker's queue
;; When all you need is ADDQ, PUSHQ, POPQ...
;;
;; This implements a Pure Functional version of the Banker's Queue.
;; The Queue is represented by a single CONS cell. The CAR points to a
;; list of in-order items added to the queue. The CDR points to a
;; revesed list of tail items added to the queue.
;;
;; Queues are elemental to Actors. We looked at making Queues into
;; Actors themselves. But doing this somewhat complicates matters
;; because code that needs to use a Queue, e.g., Serializer, often
;; also needs to do something with other incoming requests - like
;; enqueue them? - while awaing a response from the Queue Actor. A
;; Chicken-and-Egg problem.
;;
;; It can be done, but race conditions develop that need to be handled
;; properly. And in doing so, it requires knowledge of how many items
;; are expected to already be in the Queue. This becomes a distasteful
;; separation of responsibilities, and one which could easily be
;; overlooked in future client code.
;;
;; So our conclusion is that Queues should remain primitive, FPL, and
;; terse, and be carried directly in client code that needs to use
;; them.

(defvar +emptyq+ (list nil)) ;; strictly speaking, but NIL is okay in CL too.
(defvar +doneq+  #())

(defun normq (q)
  ;; maintain a non-empty hd unless the queue is empty.
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
  ;; return item at head of queue, and the new queue without that
  ;; element.
  (if (car q)
      (values (caar q)
              (normq (cons (cdar q) (cdr q))))
    +doneq+))

(defun emptyq? (q)
  (null (car q)))

(defun iterq (q fn)
  ;; functional mapping of fn over all elements in the queue
  (um:nlet iter ((q q))
    (unless (emptyq? q)
      (multiple-value-bind (item new-q)
          (popq q)
        (funcall fn item)
        (go-iter new-q))
      )))

(defmacro do-queue ((item q) &body body)
  ;; perform a body of code over all elements in the queue
  `(iterq ,q (lambda (,item) ,@body)))

