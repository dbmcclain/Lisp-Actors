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

(in-package :propagators)

(defun ball-cell-beh (neighbors content)
  ;; cell's internal content is an interval. And this accommodates
  ;; both intervals and numbers.
  (alambda
   ((cust :new-neighbor! new-neighbor)
    (cond ((member new-neighbor neighbors)
           (send cust :ok))
          (t
           (become (ball-cell-beh (cons new-neighbor neighbors) content))
           (send cust :ok))
          ))

   ((cust :add-content increment)
    (cond ((nothing? increment)
           (send cust :ok))
          ((nothing? content)
           (become (ball-cell-beh neighbors (->ball increment)))
           (β _
               (send par β neighbors :propagate)
             (send cust :ok)))
          (t
           (let* ((ball-incr (->ball increment))
                  (new-range (merge-balls content ball-incr)))
             (cond ((ball-eql? new-range content)
                    (send cust :ok))
                   #| ;; already handled with assertions in merge-balls
                   ((and (zerop (ball-rad content))
                         (zerop (ball-rad ball-incr)))
                    ;; (empty-interval? new-range)
                    (error "Ack! Inconsistency!"))
                   |#
                   (t
                    (become (ball-cell-beh neighbors new-range))
                    (β _
                        (send par β neighbors :propagate)
                      (send cust :ok)))
                   )))
          ))

   ((cust :content)
    (send cust content))
   ))

(defun cell ()
  (make-actor (ball-cell-beh nil nothing)))

;; ------------------------------------------------
;; Ball Numbers

(defstruct (ball
            (:constructor ball (ctr rad)))
  ctr rad)

(defun ball? (x)
  (ball-p x))

(defun ball-eql? (a b)
  (and (number-eql? (ball-ctr a) (ball-ctr b))
       (number-eql? (ball-rad a) (ball-rad b))))

(defgeneric ->ball (x)
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
  ;; combine two Ball estimates to 
  ;; produce Ball at variance-weighted ctr
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

