;; propagators.lisp -- An implementation of Radul and Sussman Propagators using Actors
;; from "The Art of the Propagator", Alexey Radul and Gerald Jay Sussman, MIT-CSAIL-TR-2009-002, Jan 26, 2009.
;;
;; DM/RAL 02/22
;;
;; Actors represent an ideal platform for Propagators, since they are
;; inherently asynchronous. However, a network of propagators with
;; feedback faces the possibility of never-ending computation. So
;; asking for the results of a network computation may depend on when
;; you ask.
;;
;; --------------------------------------------------------------------------------

(in-package :cl-user)

(defpackage :propagators
  (:use :cl :ac)
  (:export
   ))

(in-package :propagators)

;; ---------------------------------------------

(deflex nothing sink)

(defun nothing? (thing)
  (eql nothing thing))

(defun interval-cell-beh (neighbors content)
  ;; cell's internal content is an interval. And this accommodates
  ;; both intervals and numbers.
  (alambda
   ((cust :new-neighbor! new-neighbor)
    (cond ((member new-neighbor neighbors)
           (send cust :ok))
          (t
           (become (interval-cell-beh (cons new-neighbor neighbors) content))
           (send cust :ok))
          ))

   ((cust :add-content increment)
    (cond ((nothing? increment)
           (send cust :ok))
          ((nothing? content)
           (become (interval-cell-beh neighbors (->interval increment)))
           (β _
               (send par β neighbors :propagate)
             (send cust :ok)))
          (t
           (let* ((interval-incr (->interval increment))
                  (new-range (intersect-intervals content interval-incr)))
             (cond ((interval-eql? new-range content)
                    (send cust :ok))
                   ((empty-interval? new-range)
                    (error "Ack! Inconsistency!"))
                   (t
                    (become (interval-cell-beh neighbors new-range))
                    (β _
                        (send par β neighbors :propagate)
                      (send cust :ok)))
                   )))
          ))

   ((cust :content)
    (send cust content))
   ))

(defun cell ()
  (make-actor (interval-cell-beh nil nothing)))

;; -----------------------------------------

(defmacro defcell (name)
  `(deflex ,name (cell)))

(defun add-content (cell val)
  (send cell nil :add-content val))

(defun content (cell)
  (ask cell :content))

;; -----------------------------------------

(defun propagator (to-do &rest neighbors)
  (β _
      (send par β neighbors :new-neighbor! to-do)
    (send to-do nil :propagate)))

(defun lift-to-cell-contents (f)
  (lambda (&rest args)
    (if (some 'nothing? args)
        nothing
      (apply f args))))

(defun function->propagator-constructor (f)
  (lambda (&rest cells)
    (let ((output   (car (last cells)))
          (inputs   (butlast cells))
          (lifted-f (lift-to-cell-contents f)))
      (apply 'propagator
             (make-actor
              (alambda
               ((cust :propagate)
                (β  args
                    (send par β inputs :content)
                  (send output cust :add-content (apply lifted-f args))))
               ))
             inputs)
      )))

#|
(let* ((a (cell))
       (b (cell))
       (c (cell))
       (adder (function->propagator-constructor '+)))
  (funcall adder a b c)
  (β _
      (send a β :add-content 15)
    ;; (send a println :content)
    (β _
        (send b β :add-content 2)
      ;; (send b println :content)
      (send c println :content))
    ))
|#

(defun sq (x)
  (* x x))

(defun konst (value)
  (function->propagator-constructor (constantly value)))

(defun switch (predicate if-true output)
  (conditional predicate if-true (cell) output))

(defun conditional (p if-true if-false output)
  (propagator
   (make-actor
    (alambda
     ((cust :propagate)
      (β (predicate)
          (send p β :content)
        (unless (nothing? predicate)
          (if predicate
              (β  (tval)
                  (send if-true β :content)
                (send output cust :add-content tval))
            (β (fval)
                (send if-false β :content)
              (send output cust :add-content fval))
            ))))
     ))
   p if-true if-false))

(defun compound-propagator (to-build &rest neighbors)
  (labels ((done-beh ()
             (alambda
              ((cust :propagate)
               (send cust :ok))))
           (not-done-beh ()
             (alambda
              ((cust :propagate)
               (become (done-beh))
               (funcall to-build))
              )))
    (apply 'propagator (make-actor (not-done-beh)) neighbors)))

;; ------------------------------------------------
;; Interval Arithmetic

(defstruct (interval
            (:constructor interval (lo hi)))
  lo hi)

(defun interval? (x)
  (interval-p x))

(defun abs-diff (a b)
  (abs (- a b)))

(defvar *max-abs-err*  1e-3)

(defun interval-eql? (a b)
  ;; we have to take care with floating point numbers, equality
  ;; testing is rarely useful - and in this case, with feedback, it
  ;; can lead to infinite loops on values that are essentially equal,
  ;; but not literally equal...
  (and (< (abs-diff (interval-lo a) (interval-lo b)) *max-abs-err*)
       (< (abs-diff (interval-hi a) (interval-hi b)) *max-abs-err*)))

(defun ->interval (x)
  (if (interval-p x)
      x
    (interval x x)))

(defun coercing (coercer f)
  (lambda (&rest args)
    (apply f (mapcar coercer args))))

(defun add-interval (x y)
  (interval (+ (interval-lo x) (interval-lo y))
            (+ (interval-hi x) (interval-hi y))))

(defun sub-interval (x y)
  (interval (- (interval-lo x) (interval-hi y))
            (- (interval-hi x) (interval-lo y))))

(defun mul-interval (x y)
  ;; assumes positive bounds
  (interval (* (interval-lo x) (interval-lo y))
            (* (interval-hi x) (interval-hi y))))

(defun div-interval (x y)
  ;; assumes y not (0 ymax) or (ymin 0)
  (mul-interval x
                (interval (/ (interval-hi y))
                          (/ (interval-lo y)))
                ))

(defun sq-interval (x)
  ;; assumes positive bounds
  (interval (sq (interval-lo x))
            (sq (interval-hi x))))

(defun sqrt-interval (x)
  (interval (sqrt (interval-lo x))
            (sqrt (interval-hi x))))

(defun empty-interval? (x)
  (> (interval-lo x) (interval-hi x)))

(defun intersect-intervals (x y)
  (interval (max (interval-lo x) (interval-lo y))
            (min (interval-hi x) (interval-hi y))))

(defmacro defprop (name op)
  `(deflex ,name (function->propagator-constructor ',op)))

(defprop adder      add-interval)
(defprop subtractor sub-interval)
(defprop multiplier mul-interval)
(defprop divider    div-interval)
(defprop squarer    sq-interval)
(defprop sqrter     sqrt-interval)

;; -------------------------------------------------------
;; Proceed to manual execution of section-3.lisp and section-4.lisp

