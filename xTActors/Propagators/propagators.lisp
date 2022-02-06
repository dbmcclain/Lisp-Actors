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
;; The public face of a Propagator Network is a collection of CELLS
;; (Actors), which contain values that can be queried and updated with
;; new information.
;;
;; Hidden PROPAGATORS (Actors) connect CELLS together with
;; transforming functions to form a Propagator Network. A PROPAGATOR
;; can have any number of input CELLS and just one output CELL.
;;
;; By conventon, PROPAGATORS are set up via oridinary functions
;; relating the CELL argumens. The last CELL argument is the output
;; CELL, and all the others are input CELLS.
;;
;; Whenever a CELL value is updated, it triggers the PROPAGATORS
;; connecting the CELL to other CELLS. The PROPAGATORS query each
;; input CELL for its value, and if the collection of input CELLS
;; altogether have useful values, then the PROPAGATOR computes a
;; function of these values and updates its output CELL. That may, in
;; turn, trigger other PROPAGATORS.
;;
;; --------------------------------------------------------------------------------

(in-package :cl-user)

(defpackage :propagators
  (:use :cl :ac :def*)
  (:export
   ))

(in-package :propagators)

;; ---------------------------------------------

(defconstant nothing (list 'the-nothing))

(defun nothing? (thing)
  (eql thing nothing))

(defun interval-cell-beh (propagators content)
  ;; cell's internal content is an interval. And this accommodates
  ;; both intervals and numbers.
  (alambda
   ((:new-propagator new-propagator)
    (unless (member new-propagator propagators)
      (become (interval-cell-beh (cons new-propagator propagators) content))))

   ((:add-content increment)
    (cond ((nothing? increment))
          ((nothing? content)
           (become (interval-cell-beh propagators (->interval increment)))
           (send-to-all propagators))
          (t
           (let* ((interval-incr (->interval increment))
                  (new-range (intersect-intervals content interval-incr)))
             (cond ((interval-eql? new-range content))
                   ((empty-interval? new-range)
                    (error "Ack! Inconsistency!"))
                   (t
                    (become (interval-cell-beh propagators new-range))
                    (send-to-all propagators))
                   )))
          ))

   ((cust :content)
    (send cust content))
   ))

(defun cell (&optional (value nothing))
  (make-actor (interval-cell-beh nil (->interval value))))

;; -----------------------------------------
;; User (REPL) Interface to CELLS. Actors use message sends.

(defmacro defcell (name &optional (value 'nothing))
  `(deflex ,name (cell ,value)))

(defun add-content (cell val)
  (send cell :add-content val))

(defun content (cell)
  (ask cell :content))

;; -----------------------------------------

(defmacro defprop (name fn)
  (lw:with-unique-names (cells)
    `(defun ,name (&rest ,cells)
       (attach-propagator-fn ',fn ,cells))
    ))

(defun attach-propagator (prop &rest cells)
  (send-to-all cells :new-propagator prop)
  (send prop))

(defun attach-propagator-fn (fn cells)
  (let* ((output (car (last cells)))
         (inputs (butlast cells))
         (prop   (make-actor
                  (lambda ()
                    (β  args
                        (send par β inputs :content)
                      (unless (some 'nothing? args)
                        (send output :add-content (apply fn args))))
                    ))))
    (apply 'attach-propagator prop inputs)))

;; ---------------------------------------------------------

(defun switch (predicate if-true output)
  (conditional predicate if-true (cell) output))

(defun conditional (p if-true if-false output)
  (let ((prop  (make-actor
                (lambda ()
                  (β  (pred)
                      (send p β :content)
                    (unless (nothing? pred)
                      (β  (val)
                          (send (if pred
                                    if-true
                                  if-false)
                                β :content)
                        (unless (nothing? val)
                          (send output :add-content val)))
                      )))
                )))
    (attach-propagator prop p if-true if-false)))

;; ------------------------------------------------
;; Interval Arithmetic

(defstruct (interval
            (:constructor interval (lo hi)))
  lo hi)

(defun interval? (x)
  (interval-p x))

(defun abs-diff (a b)
  (abs (- a b)))

(defvar *tolerance* 1e-3)

(defgeneric number-eql? (a b)
  (:method (a b)
   nil)
  (:method ((a rational) (b rational))
   (eql a b))
  (:method ((a number) (b number))
   (< (abs-diff a b) *tolerance*))
  )

(defun interval-eql? (a b)
  ;; we have to take care with floating point numbers, equality
  ;; testing is rarely useful - and in this case, with feedback, it
  ;; can lead to infinite loops on values that are essentially equal,
  ;; but not literally equal...
  (and (number-eql? (interval-lo a) (interval-lo b))
       (number-eql? (interval-hi a) (interval-hi b))))

(defgeneric ->interval (x)
  (:method ((x (eql nothing)))
   x)
  (:method ((x interval))
   x)
  (:method ((x number))
   (interval x x))
  )

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

(defun sq (x)
  (* x x))

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

(defprop adder      add-interval)
(defprop subtractor sub-interval)
(defprop multiplier mul-interval)
(defprop divider    div-interval)
(defprop squarer    sq-interval)
(defprop sqrter     sqrt-interval)

;; -------------------------------------------------------
;; Proceed to manual execution of section-3.lisp and section-4.lisp

