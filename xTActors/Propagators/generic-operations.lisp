;; generic-operations.lisp - Predicative Dispatch of Generic Arithmetic Ops
;;
;; DM/RAL  02/22
;; ---------------------------------------------------------------------------
;; Load this file after ball-cells.lisp, then execute contents of section-4.lisp

(in-package :propagators)

;; ---------------------------------------------------

(deflex the-contradiction (list 'contradiction))

(defgeneric  contradictory? (x)
  (:method (x)
   (eql x the-contradiction)))

;; ---------------------------------------------
;; Test for point within an interval

(defun collapse-to-real (x)
  (let ((re  (realpart x)))
    (if (number-eql? x (complex re 0))
        re
      x)))

(defgeneric ensure-inside (interval number)
  (:method ((interval interval) (number number))
   (cond ((number-eql? number (interval-lo interval)) number)
         ((number-eql? (interval-lo interval) (interval-hi interval))
          the-contradiction)
         (t
          ;; test for number inside ray from interval-lo to interval-hi
          (let* ((test-ray (- number (interval-lo interval)))
                 (diam     (- (interval-hi interval) (interval-lo interval)))
                 (frac     (collapse-to-real (/ test-ray diam))))
            (if (and (realp frac)
                     (<= 0 frac 1))
                number
              the-contradiction)))
         ))
  
  (:method ((interval ball) (number number))
   ;; test for number inside of ball
   (if (<= (abs (- number (ball-ctr interval))) (ball-rad interval))
       number
     the-contradiction))
  )

;; ------------------------------------------------------
;; Using staged computations to avoid Cartesian explosion of
;; cross-type comparisons

(defgeneric default-equal? (a b)
  (:method (a b)
   (eql a b))
  
  (:method ((a number) b)
   (default-equal-number? b a))
  
  (:method ((a interval) b)
   (default-equal-interval? b a))
  
  (:method ((a ball) b)
   (default-equal-ball? b a))
  )

(defgeneric default-equal-number? (b a)
  ;; here it is known that a is a number
  (:method (b a)
   nil)
  
  (:method ((b number) a)
   (number-eql? a b))

  (:method ((b interval) a)
   (interval-eql? (->interval a) b))

  (:method ((b ball) a)
   (ball-eql? (->ball a) b))
  )

(defgeneric default-equal-interval? (b a)
  ;; here it is known that a is an interval
  (:method (b a)
   nil)

  (:method ((b number) a)
   (interval-eql? a (->interval b)))

  (:method ((b interval) a)
   (interval-eql? a b))

  (:method ((b ball) a)
   (ball-eql? (->ball a) b))
  )

(defgeneric default-equal-ball? (b a)
  ;; here it is known that a is a ball
  (:method (b a)
   nil)

  (:method ((b number) a)
   (ball-eql? a (->ball b)))

  (:method ((b interval) a)
   (ball-eql? a (->ball b)))

  (:method ((b ball) a)
   (ball-eql? a b))
  )

;; ------------------------------------------------------

(defun unspec-merge (content increment)
  (if (default-equal? content increment)
      content
    the-contradiction))

(defgeneric merge-info (content increment)
  (:method (content increment)
   (unspec-merge content increment))

  (:method ((content (eql nothing)) increment)
   increment)

  (:method ((content number) increment)
   (merge-info-number increment content))

  (:method ((content interval) increment)
   (merge-info-interval increment content))

  (:method ((content ball) increment)
   (merge-info-ball increment content))
  )

(defgeneric merge-info-number (increment content)
  ;; here it is known that content is a number
  (:method (increment content)
   (unspec-merge content increment))

  (:method ((increment (eql nothing)) content)
   content)

  (:method ((increment interval) content)
   (ensure-inside increment content))

  (:method ((increment ball) content)
   content) ;; however unlikely...
  )

(defgeneric merge-info-interval (increment content)
  ;; here it is known that content is an interval
  (:method (increment content)
   (unspec-merge content increment))
  
  (:method ((increment (eql nothing)) content)
   content)

  (:method ((increment number) content)
   (ensure-inside content increment))

  (:method ((increment interval) content)
   (let ((new-range (intersect-intervals content increment)))
     (cond ((interval-eql? new-range content) content)
           ((interval-eql? new-range increment) increment)
           ((empty-interval? new-range) the-contradiction)
           (t new-range)
           )))

  (:method ((increment ball) content)
   (merge-balls (->ball content) increment))
  )

(defgeneric merge-info-ball (increment content) 
  ;; here it is known that content is a ball
  (:method (increment content)
   (unspec-merge content increment))
  
  (:method ((increment (eql nothing)) content)
   content)

  (:method ((increment number) content)
   increment) ;; no contradiction, however unlikely

  (:method ((increment interval) content)
   (merge-balls content (->ball increment)))

  (:method ((increment ball) content)
   (merge-balls content increment))
  )

;; ---------------------------------------------

(defun cell-beh (propagators content)
  (alambda
   ((:new-propagator new-propagator)
    (unless (member new-propagator propagators)
      (become (cell-beh (cons new-propagator propagators) content))))

   ((:add-content increment)
    (let ((ans (merge-info content increment)))
      (cond ((default-equal? ans content))
            ((contradictory? ans)
             (error "Ack! Inconsistency!"))
            (t
             (become (cell-beh propagators ans))
             (send-to-all propagators))
            )))

   ((cust :content)
    (send cust content))
   ))

(defun cell (&optional (value nothing))
  (make-actor (cell-beh nil value)))

(defun konst (val)
  (make-actor (konst-beh nil val)))

;; ----------------------------------------------

(defmacro gen-unop (name nbr-op interval-op ball-op)
  `(defgeneric ,name (x)
     (:method ((x number))
      (,nbr-op x))
     (:method ((x interval))
      (,interval-op x))
     (:method ((x ball))
      (,ball-op x))
     ))

(gen-unop generic-sq   sq   sq-interval   sq-ball)
(gen-unop generic-sqrt sqrt sqrt-interval sqrt-ball)

(defmacro gen-binop (name nbr-op interval-op ball-op)
  (let ((op-number   (um:symb name "-number"))
        (op-interval (um:symb name "-interval"))
        (op-ball     (um:symb name "-ball")))
    `(progn
       (defgeneric ,op-number (b a)
         ;; here it is known that a is number
         (:method ((b number) a)
          (,nbr-op a b))
         (:method ((b interval) a)
          (,interval-op (->interval a) b))
         (:method ((b ball) a)
          (,ball-op (->ball a) b)))
       (defgeneric ,op-interval (b a)
         ;; here it is known that a is interval
         (:method ((b number) a)
          (,interval-op a (->interval b)))
         (:method ((b interval) a)
          (,interval-op a b))
         (:method ((b ball) a)
          (,ball-op (->ball a) b)))
       (defgeneric ,op-ball (b a)
         ;; here it is known that a is ball
         (:method ((b number) a)
          (,ball-op a (->ball b)))
         (:method ((b interval) a)
          (,ball-op a (->ball b)))
         (:method ((b ball) a)
          (,ball-op a b)))
       (defgeneric ,name (a b)
         ;; the primary generic operator - first classify a
         (:method ((a number) b)
          (,op-number b a))
         (:method ((a interval) b)
          (,op-interval b a))
         (:method ((a ball) b)
          (,op-ball b a))))
    ))

(gen-binop generic-+ + add-interval add-ball)
(gen-binop generic-- - sub-interval sub-ball)
(gen-binop generic-* * mul-interval mul-ball)
(gen-binop generic-/ / div-interval div-ball)

(defprop adder      generic-+)
(defprop subtractor generic--)
(defprop multiplier generic-*)
(defprop divider    generic-/)
(defprop squarer    generic-sq)
(defprop sqrter     generic-sqrt)
  
