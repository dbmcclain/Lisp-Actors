;; ball-cells.lisp -- Recast Interval Cells to use Ball Arithmetic
;;
;; DM/RAL 02/22
;;
;; Using Ball arithmetic instead of Interval arithmetic, for better
;; statistical information as knowledge is increased. There are no
;; longer contradictions, but variance of results can grow as a result
;; of poor data being admixed.
;;
;; Note: Our Balls use standard deviation measures for radii.
;; Operations combine uncertainties as incoherent sums of variance
;; terms, each scaled by the square of the partial derivative of the
;; function with respect to each operand. This corresponds to
;; independent uncorrelated measurements.
;; ------------------------------------------------
;; Load this file afer propagators.lisp, and rerun examples in section-3.lisp

(in-package :propagators)

(defun ball-cell-beh (propagators content)
  ;; cell's internal content is an interval. And this accommodates
  ;; both intervals and numbers.
  (alambda
   ((:new-propagator new-propagator)
    (unless (member new-propagator propagators)
      (become (ball-cell-beh (cons new-propagator propagators) content))
      ))

   ((:add-content increment)
    (cond ((nothing? increment))
          ((nothing? content)
           (become (ball-cell-beh propagators (->ball increment)))
           (send-to-all propagators))
          (t
           (let* ((ball-incr (->ball increment))
                  (new-range (merge-balls content ball-incr)))
             (cond ((default-equal? new-range content))
                   (t
                    (become (ball-cell-beh propagators new-range))
                    (send-to-all propagators))
                   )))
          ))
   
   ((cust :content)
    (send cust content))
   ))

(defun cell (&optional (value nothing))
  (make-actor (ball-cell-beh nil (->ball value))))

(defun konst (val)
  (make-actor (konst-beh nil (->ball val))))

;; ------------------------------------------------
;; Ball Numbers

(defstruct (ball
            (:constructor ball (ctr rad)))
  ctr rad)

(defun ball? (x)
  (ball-p x))

(defmethod default-equal? ((a ball) b)
  (default-equal-ball? b a))

(defgeneric default-equal-ball? (b a)
  (:method (b a)
   nil)
  (:method ((b number) a)
   t) ;; however unlikely...
  (:method ((b interval) a)
   t) ;l however unlikely...
  (:method ((b ball) a)
   (and (default-equal? (ball-ctr a) (ball-ctr b))
        (default-equal? (ball-rad a) (ball-rad b)))))

(defmethod default-equal-rational? ((b ball) a)
  t) ;; however unlikely...

(defmethod default-equal-number? ((b ball) a)
  t) ;; however unlikely...

(defmethod default-equal-interval? ((b ball) a)
  t) ;; however unlikely...
  
(defgeneric ->ball (x)
  (:method ((x (eql nothing)))
   x)
  (:method ((x ball))
   x)
  (:method ((x number))
   (ball x 0))
  (:method ((x interval))
   ;; assume interval bounds are 1-sigma values
   (let ((diam (- (interval-hi x) (interval-lo x))))
     (ball (+ (interval-lo x) (/ diam 2))
           (/ (abs diam) 2))
     ))
  )

(defmethod ->interval ((x ball))
  ;; This is *not* the unique inverse of ->ball for intervals in the complex domain
  ;; We have: ball == (->ball (->interval ball))
  ;; But we don't necessarily have: interval =?= (->interval (->ball interval))
  ;; Resulting interval will be aligned along the ray from origin to ball ctr.
  (let ((frac (/ (ball-rad x) (abs (ball-ctr x)))))
    (interval (* (ball-ctr x) (- 1 frac))
              (* (ball-ctr x) (+ 1 frac)))
    ))

;; -----------------------------------------------

(defun rss (a b)
  (abs (complex a b)))

(defun abs-rss (a b)
  (rss (ball-rad a) (ball-rad b)))

(defun rel-rss (a b)
  (rss (/ (ball-rad a) (ball-ctr a))
       (/ (ball-rad b) (ball-ctr b))))

(defun add-ball (a b)
  (ball (+ (ball-ctr a) (ball-ctr b))
        (abs-rss a b)))

(defun sub-ball (a b)
  (ball (- (ball-ctr a) (ball-ctr b))
        (abs-rss a b)))

(defun mul-ball (a b)
  (let ((prod (* (ball-ctr a) (ball-ctr b))))
    (ball prod
          (* prod (rel-rss a b)))
    ))

(defun div-ball (a b)
  (let ((quot (/ (ball-ctr a) (ball-ctr b))))
    (ball quot
          (* quot (rel-rss a b)))
    ))

(defun sq-ball (a)
  (ball (sq (ball-ctr a))
        (* 2 (ball-ctr a) (ball-rad a))))

(defun sqrt-ball (a)
  (let ((rt (sqrt (ball-ctr a))))
    (ball rt
          (/ (ball-rad a) rt 2))
    ))

(defun merge-balls (a b)
  ;; combine two Ball estimates to produce Ball at variance-weighted
  ;; ctr
  ;;
  ;; Ugh! This is bad... Imagine a CELL containing a BALL as its
  ;; content, given to it by some Propagator. Now imagine the same
  ;; Propagator gets redundantly executed again, and hands our CELL
  ;; the exact same BALL, again, to stuff into its content slot.
  ;;
  ;; What will our CELL do with this repeated information? It will
  ;; merge the "new" BALL with its own content BALL, giving it the
  ;; same BALL ctr, but with half the original variance.
  ;;
  ;; Our knowledge claims to be improving, when absolutely nothing
  ;; new was presented to it...
  ;;
  (cond ((zerop (ball-rad a))
         (assert (or (plusp (ball-rad b))
                     (equalp (ball-ctr a) (ball-ctr b))
                     ))
         a)
        ((zerop (ball-rad b)) b)
        (t
         (let* ((wa   (/ (sq (ball-rad a))))
                (wb   (/ (sq (ball-rad b))))
                (wtot (+ wa wb))
                (ctr  (/ (+ (* wa (ball-ctr a))
                            (* wb (ball-ctr b)))
                         wtot)))
           (ball ctr
                 (/ (sqrt wtot)))
           ))
        ))

(defprop adder      add-ball)
(defprop subtractor sub-ball)
(defprop multiplier mul-ball)
(defprop divider    div-ball)
(defprop squarer    sq-ball)
(defprop sqrter     sqrt-ball)

;; -------------------------------------------------------
;; Proceed to manual execution of section-3.lisp and section-4.lisp

