;; propagators.lisp -- An implementation of Radul and Sussman Propagators using Actors
;; from "The Art of the Propagator", Alexey Radul and Gerald Jay Sussman, MIT-CSAIL-TR-2009-002, Jan 26, 2009.
;;
;; DM/RAL 02/22
;; --------------------------------------------------------------------------------

(in-package :propagators)

;; ---------------------------------------------

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
             (cond ((default-equal? new-range content))
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

(defun konst (val)
  (make-actor (konst-beh nil (->interval val))))

;; ------------------------------------------------
;; Interval Arithmetic

(defstruct (interval
            (:constructor interval (lo hi)))
  lo hi)

(defun interval? (x)
  (interval-p x))

(defmethod default-equal? ((a interval) b)
  (default-equal-interval? b a))

(defgeneric default-equal-interval? (b a)
  (:method (b a)
   nil)
  (:method ((b number) a)
   (and (default-equal? (interval-lo a) b)
        (default-equal? (interval-hi a) b)))
  (:method ((b interval) a)
   (and (default-equal? (interval-lo a) (interval-lo b))
        (default-equal? (interval-hi a) (interval-hi b)))))

(defmethod default-equal-rational? ((b interval) a)
  (and (default-equal? a (interval-lo b))
       (default-equal? a (interval-hi b))))

(defmethod default-equal-number? ((b interval) a)
  (and (default-equal? a (interval-lo b))
       (default-equal? a (interval-hi b))))

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

(defun empty-interval? (x)
  (> (interval-lo x) (interval-hi x)))

(defun intersect-intervals (x y)
  (interval (max (interval-lo x) (interval-lo y))
            (min (interval-hi x) (interval-hi y))))

;; ------------------------------------------------

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

(defprop adder      add-interval)
(defprop subtractor sub-interval)
(defprop multiplier mul-interval)
(defprop divider    div-interval)
(defprop squarer    sq-interval)
(defprop sqrter     sqrt-interval)

;; -------------------------------------------------------
;; Proceed to manual execution of section-3.lisp and section-4.lisp

