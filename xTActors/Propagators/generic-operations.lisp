;; generic-operations.lisp - Predicative Dispatch of Generic Arithmetic Ops
;;
;; DM/RAL  02/22
;; ---------------------------------------------------------------------------

(in-package :propagators)

;; ---------------------------------------------------

(deflex the-contradiction (list 'contradiction))

(defun contradictory? (x) (eq x the-contradiction))

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
          ;; test for number along ray from interval-lo to interval-hi
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

(defgeneric default-equal? (a b)
  (:method (a b)
   (eql a b))
  
  (:method ((a number) (b number))
   (number-eql? a b))
  
  (:method ((a interval) (b interval))
   (interval-eql? a b))
  
  (:method ((a ball) (b ball))
   (ball-eql? a b))
  
  (:method ((a interval) (b number))
   (interval-eql? a (->interval b)))
  
  (:method ((a number) (b interval))
   (interval-eql? (->interval a) b))
  
  (:method ((a ball) (b number))
   (ball-eql? a (->ball b)))
  
  (:method ((a number) (b ball))
   (ball-eql? (->ball a) b))
  
  (:method ((a ball) (b interval))
   (interval-eql? (->interval a) b))
  
  (:method ((a interval) (b ball))
   (interval-eql? a (->interval b)))
  )

;; ------------------------------------------------------

(defgeneric merge-info (content increment)

  (:method (content increment)
   (if (default-equal? content increment)
       content
     the-contradiction))
  
  (:method (content (increment (eql nothing)))
   content)
  
  (:method ((content (eql nothing)) increment)
   increment)
  
  (:method ((content interval) (increment interval))
   (let ((new-range (intersect-intervals content increment)))
     (cond ((interval-eql? new-range content) content)
           ((interval-eql? new-range increment) increment)
           ((empty-interval? new-range) the-contradiction)
           (t new-range)
           )))
  
  (:method ((content number) (increment interval))
   (ensure-inside increment content))
  
  (:method ((content interval) (increment number))
   (ensure-inside content increment))
  
  (:method ((content ball) (increment number))
   increment) ;; no ontradictions, however unlikely
  
  (:method ((content number) (increment ball))
   content) ;; no contradictions, however unlikely
  
  (:method ((content interval) (increment ball))
   (let ((new-ball (->ball content)))
     (merge-balls new-ball increment)))

  (:method ((content ball) (increment interval))
   (let ((new-ball (->ball increment)))
     (merge-balls new-ball content)))
  )

;; ---------------------------------------------

(defun cell-beh (neighbors content)
  (alambda
   ((cust :new-neighbor! new-neighbor)
    (cond ((member new-neighbor neighbors)
           (send cust :ok))
          (t
           (become (cell-beh (cons new-neighbor neighbors) content))
           (send cust :ok))
          ))

   ((cust :add-content increment)
    (let ((ans (merge-info content increment)))
      (cond ((default-equal? ans content)
             (send cust :ok))
            ((contradictory? ans)
             (error "Ack! Inconsistency!"))
            (t
             (become (cell-beh neighbors ans))
             (β _
                 (send par β neighbors :propagate)
               (send cust :ok)))
            )))

   ((cust :content)
    (send cust content))
   ))

(defun cell ()
  (make-actor (cell-beh nil nothing)))

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
  `(defgeneric ,name (a b)
     (:method ((a number) (b number))
      (,nbr-op a b))
     (:method ((a interval) (b interval))
      (,interval-op a b))
     (:method ((a interval) (b number))
      (,interval-op a (->interval b)))
     (:method ((a number) (b interval))
      (,interval-op (->interval a) b))
     (:method ((a ball) (b ball))
      (,ball-op a b))
     (:method ((a ball) (b interval))
      (,ball-op a (->ball b)))
     (:method ((a interval) (b ball))
      (,ball-op (->ball a) b))
     (:method ((a ball) (b number))
      (,ball-op a (->ball b)))
     (:method ((a number) (b ball))
      (,ball-op (->ball a) b))
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
  
