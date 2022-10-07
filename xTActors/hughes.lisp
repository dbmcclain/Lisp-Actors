;; hughes.lisp -- Higher order FPL in Actors?
;;
;; DM/RAL 02/22
;; --------------------------------------------------------------------
;; Do Actors bring anything to the table for higher order functional
;; programming? We use John Hughes's "Why Functional Programming
;; Matters" as our reference.
;;
;; So far, I find that using Actors seriously complicates matters over
;; plain functional programming with Call/Return semantics. But Actors
;; do allow a degree of parallelism that did not exist before. So
;; that's at least something.
;;
;; The main difference between simulating normal order eval with
;; Call/Return vs simuilation with Actors, is that in Call/Return we
;; have direct access to the components of Sequence structs, used for
;; infinite sequences.
;;
;; In Actor code you do not have direct access to this data, and must
;; use message passing and continuation Actors in place of struct
;; references. That is the seriously complicating matter. But that is
;; also what opens up possibilities for parallel execution. (c.f., all
;; the FORK operations below.)

(in-package :ac)

#|
(deflex nil-actor
  (create
   (alambda
    ((cust :repr)
     (send cust nil))
    ((cust :foldr fn x)
     (send cust x))
    )))

(defun cons-beh (car cdr)
  (alambda
   ((cust :car)
    (send cust car))

   ((cust :cdr)
    (send cust cdr))

   ((cust :repr)
    (β (ans)
        (send cdr β :repr)
      (send cust (cons car ans))))
   
   ((cust :foldr fn x)
    (β  (ans)
        (send cdr β :foldr fn x)
      (send cust (funcall fn car ans))))
   ))

(defun encons (car cdr)
  (create (cons-beh car cdr)))


(let ((lst (encons 1 (encons 2 (encons 3 nil-actor)))))
  (send lst println :foldr '+ 0)
  (send lst println :foldr '* 1)
  (β (ans)
      (send lst β :foldr 'encons nil-actor)
    (send ans println :repr)))

(defmacro compose (actor cust (&rest msg1) (&rest msg2))
  (lw:with-unique-names (ans)
    `(β (,ans)
         (send ,actor β ,@msg1)
       (send ,ans ,cust ,@msg2))
    ))

(let ((lst (encons 1 (encons 2 (encons 3 nil-actor)))))
  (compose lst println (:foldr 'encons nil-actor) (:repr)))


(defun heron-beh (x)
  (alambda
   ((cust n)
    (cond ((< (abs (- n (* x x))) 1e-6)
           (send cust x))
          (t
           (become (heron-beh (* 0.5 (+ x (/ n x)))))
           (send self cust n))
          ))
   ))

(send (create (heron-beh 1)) println 50)

;; --------------------------------------------

(defun next-guess-beh (test)
  (alambda
   ((n x)
    (let ((xx (* 0.5 (+ x (/ n x)))))
      (send test n xx)
      ))
   ))

(defun abserr-beh (xprev tol cust again)
  (alambda
   ((n x)
    (cond ((< (abs (- x xprev)) tol)
           (send cust (list n x)))
          (t
           (become (abserr-beh x tol cust again))
           (send again n x))
          ))))

(actors ((next      (create (next-guess-beh good-enuf)))
         (good-enuf (create (abserr-beh 0 1e-8 println next))))
  (send next 10001 1))


;; --------------------------------------------

(defun start-seq-beh (x0 fn)
  (alambda
   ((cust :nth n)
    (cond ((eql n 0)
           (send cust self))
          (t
           (let* ((x1   (funcall fn x0))
                  (next (create (start-seq-beh x1 fn))))
             (become (seq-beh x0 next))
             (send next cust :nth (1- n))))
          ))
   ((cust :get)
    (send cust x0))
   ))
   
(defun seq-beh (x next)
  (alambda
   ((cust :nth n)
    (cond ((eql n 0)
           (send cust self))
          (t
           (send next cust :nth (1- n)))
          ))
   ((cust :get)
    (send cust x))
   ))

(defun seq (x0 fn)
  (create (start-seq-beh x0 fn)))

(β (nth)
    (send (seq 0 '1+) β :nth 15)
  (send nth println :get))

(defun nth-cdr (seq cust n)
  (send seq cust :nth n))

(defun nth-car (seq cust n)
  (β (nth)
      (nth-cdr seq β n)
    (send nth cust :get)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------
;; Infinite Sequences

(def-beh resolved-seq-beh (a next)
  ((cust :hd)
   (send cust a))
  
  ((cust :tl)
   (send cust next))
  
  ((cust :nth n)
   (cond ((zerop n)
          (send cust a))
         (t
          (send next cust :nth (1- n)))
         ))
  
  ((cust :nthtl n)
   (cond ((zerop n)
          (send cust self))
         (t
          (send next cust :nthtl (1- n)))
         )))

(defun seq-beh (a next)
  (labels
      ((resolve-next (cust)
         (let ((me self))
           (β  (new-next)
               (send next β)
             (send me :resolve-next new-next)
             (send cust new-next))
           )))
    
    (alambda
     ((:resolve-next new-next)
      (become (resolved-seq-beh a new-next)))
     
     ((cust :hd)
      (send cust a))
     
     ((cust :tl)
      (resolve-next cust))
     
     ((cust :nth n)
      (cond ((zerop n)
             (send cust a))
            (t
             (β (new-next)
                 (resolve-next β)
               (send new-next cust :nth (1- n))
               ))
            ))
     
     ((cust :nthtl n)
      (cond ((zerop n)
             (send cust self))
            (t
             (β  (new-next)
                 (resolve-next β)
               (send new-next cust :nthtl (1- n))
               ))
            ))
     )))

(defun seq (a α-cust)
  (create (seq-beh a α-cust)))

;; -----------------------------------------------------
;; Operators - singleton Actors that act on other Actors to produce new Actors

(deflex stake
  (α (cust s n &optional ans)
    (if (zerop n)
        (send cust (reverse ans))
      (β  (a rb)
          (send (fork s s) β '(:hd) '(:tl))
        (send stake cust rb (1- n) (cons a ans))
        ))))

(deflex smap
  (α (cust fn s)
    (β  (a rb)
        (send (fork s s) β '(:hd) '(:tl))
      (send cust (seq (funcall fn a)
                      (α (acust)
                        (send smap acust fn rb))
                      ))
      )))∑

(deflex foldl
  (α (cust fn init s)
    (β  (a rb)
        (send (fork s s) β '(:hd) '(:tl))
      (let ((new-init (funcall fn init a)))
        (send cust (seq new-init
                        (α (acust)
                          (send foldl acust fn new-init rb)))
              ))
      )))

(deflex repeat
  (α (cust a f)
    (send cust (seq a (α (acust)
                        (send repeat acust (funcall f a) f))
                    ))))

(deflex order
  (α (cust seq)
    (β  (a rb)
        (send (fork seq seq) β '(:hd) '(:tl))
      (β  (b rc)
          (send (fork rb rb) β '(:hd) '(:tl))
        (β (c)
            (send rc β :hd)
          (let ((ord (or (ignore-errors
                           (round (log (- (/ (- a c)
                                             (- b c))
                                          1 )
                                       2 )))
                         100)))
            (send cust ord)
            )))
      )))

(deflex elimerror
  (α (cust n s)
    (β (a rb)
        (send (fork s s) β '(:hd) '(:tl))
      (β (b)
          (send rb β :hd)
        (let ((2^n (expt 2 n)))
          (send cust (seq (/ (- (* b 2^n) a) (- 2^n 1))
                          (α (acust)
                            (send elimerror acust n rb))))
          )))))

(deflex improve
  (α (cust s)
    (β (ord)
        (send order β s)
      (send elimerror cust ord s))
    ))

(deflex within
  (α (cust eps s)
    (β  (a rb)
        (send (fork s s) β '(:hd) '(:tl))
      (β (b)
          (send rb β :hd)
        (cond ((<= (abs (- a b)) eps)
               (send cust b))
              (t
               (send order println s)
               (send within cust eps rb))
              )))))

(deflex ssqrt
  (α (cust a0 eps n)
    (β  (s)
        (send repeat β
              a0
              (lambda (x)
                (/ (+ x (/ n x)) 2)))
      (send within cust eps s))
    ))


#|
(send ssqrt println 1.5 1e-8 0.1)
(sqrt 2.0)
(β (s)
    (send repeat β 1.5 (lambda (x) (/ (+ x (/ 0.1 x)) 2)))
  (send stake println s 5))

(send repeat println 1 (lambda (x)
                         (/ (+ x (/ 2 x)) 2)))

|#
