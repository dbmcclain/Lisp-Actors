;; funcallable-beh.lisp -- What happens if we make Behavior functions into Funcallable Classes?
;;
;; A new style of message format, and a different way of building up
;; Actor behaviors that can be extensible and inheritable. We can also
;; avoid the overhead of pattern matching on messages.
;;
;; DM/RAL 02/22
;; --------------------------------------------

(in-package #:com.ral.actors)

;; -----------------------------------------------------------

(defclass <behavior> ()
  ()
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((beh <behavior>) &key &allow-other-keys)
  ;; Watch Out! Unlike ALAMBDA behaviors, we don't ignore unrecognized messages
  (clos:set-funcallable-instance-function beh
                                          (lambda (fn &rest args)
                                            (apply fn beh args))
                                          ))

(defmacro defbeh (name supers &optional slots &rest options)
  `(defclass ,name
             ,(or supers
                  '(<behavior>))
     ,slots ,@options
     (:metaclass clos:funcallable-standard-class)))

#+:LISPWORKS
(editor:setup-indent "defbeh" 2)

;; -----------------------------------------------------------
;; Example - Infinite Sequences
;;
;;   shd  stl
;;  +---+----+
;;  | a | rb |  refs are initially LAZY Actors, become CONST ptrs
;;  +---+----+  (A LAZY Actor is only ever executed just once)
;;   fst   |
;;         v
;;       +---+----+
;;       | b | rc |
;;       +---+----+
;;        snd  |
;;             v
;;           +---+----+
;;           | c | rd |
;;           +---+----+
;;            thd  |
;;                 v
;;
;; NOTE: Notice that, in the following code, except for the relatively
;; infrequent computation using Lisp call/return function processing,
;; the vast majority of the code is simply a sequence of SEND
;; operations.
;;
;; You could think of Lisp call/return as microcode, and SEND as
;; higher level, Actor-land, GOTO.
;;
;; Also, the extensive use of Actors, in place of Lisp functions,
;; makes all the following code very similar to CPS-style coding. Here
;; the continuations are anonymous Actors, instead of anonymous Lambda
;; closures. But, except for this new funcallable behavior approach,
;; an Actor usually *is* a simply-encapsulated Lambda closure.

(defbeh seq-beh ()
  ((hd  :reader seq-hd  :initarg :hd)
   (tl  :reader seq-tl  :initarg :tl)))

(deflex seq
  ;; construct a sequence given first element, and Actor to produce
  ;; next element
  (λ (cust a α-cust)
    (send cust (create
                (make-instance 'seq-beh
                               :hd  a
                               :tl  (lazy α-cust))))
    ))

;; ----------------------------------------
;; SEQ Behavior Message Handlers

(defmethod shd ((beh seq-beh) cust)
  (send cust (seq-hd beh)))

(defmethod stl ((beh seq-beh) cust)
  (send (seq-tl beh) cust))

(defmethod spair ((beh seq-beh) cust)
  (let ((a  (seq-hd beh)))
    (β  (rb)
        (stl beh β)
      (send cust a rb)
      )))

(defmethod snth ((beh seq-beh) cust n)
  (cond ((zerop n)
         (send cust (seq-hd beh)))
        (t
         (β (next)
             (stl beh β)
           (send next 'snth cust (1- n))))
        ))

(defmethod snthtl ((beh seq-beh) cust n)
  (cond ((zerop n)
         (send cust self))
        (t
         (β (next)
             (stl beh β)
           (send next 'snthtl cust (1- n)))
         )))

;; -----------------------------------------------

(deflex repeat
  ;; construct a sequence: a, f(a), f(f(a)), ...
  (λ (cust a f)
    (send seq cust a
          (α (acust)
            (β (fa)
                (send f β a)
              (send repeat acust fa f))))
    ))

(deflex order
  ;; compute order of convergence of sequence
  (λ (cust seq)
    (β  (a rb)
        (send seq 'spair β)
      (β  (b rc)
          (send rb 'spair β)
        (β (c)
            (send rc 'shd β)
          (let ((ord (or (ignore-errors
                           (round (log (- (/ (- a c)
                                             (- b c))
                                          1 )
                                       2 )))
                         100)))
            (send cust ord)
            )))
      )))

(deflex within
  ;; recursive probing until two consecutive values differ by less
  ;; than eps
  (λ (cust eps s)
    (β  (a rb)
        (send s 'spair β)
      (β (b)
          (send rb 'shd β)
        (cond ((<= (abs (- a b)) eps)
               (send cust b))
              (t
               (send within cust eps rb))
              )))))

(deflex ssqrt
  ;; Heron's method for SQRT (= Newton's Method)
  (λ (cust a0 eps n)
    (β  (s)
        (send repeat β
              a0
              (α (acust x)
                (send acust (/ (+ x (/ n x)) 2))))
      (send within cust eps s))
    ))


#|
(send ssqrt println 1.5 1e-8 0.1)
(sqrt 2.0)
(β (s)
    (send repeat β 1.5 (lambda (x) (/ (+ x (/ 0.1 x)) 2)))
  (send stake println s 5))
|#
;; -----------------------------------------------

(deflex elimerror
  (λ (cust n s)
    (β (a rb)
        (send s 'spair β)
      (β (b)
          (send rb 'shd β)
        (let* ((2^n   (expt 2 n))
               (new-a (/ (- (* b 2^n) a) (- 2^n 1))))
          (send seq cust new-a
                (α (acust)
                  (send elimerror acust n rb)))
          )))))

(deflex improve
  (λ (cust s)
    (β (ord)
        (send order β s)
      (send elimerror cust ord s))
    ))

(deflex fst
  (λ (cust s)
    (send s 'shd cust)))

(deflex snd
  (λ (cust s)
    (β (rb)
        (send s 'stl β)
      (send rb 'shd cust)
      )))

(deflex thd
  (λ (cust s)
    (β  (rb)
        (send s 'stl β)
      (β  (rc)
          (send rb 'stl β)
        (send rc 'shd cust)
        ))))

(deflex smap
  (λ (cust afn s)
    (β  (a rb)
        (send s 'spair β)
      (β  (ma)
          (send afn β a)
        (send seq cust ma
              (α (acust)
                (send smap acust afn rb))
              )
        ))))

(deflex aitken
  (λ (cust s)
  ;; Aitken's delta-squared process
  (β  (a rb)
      (send s 'spair β)
    (β  (b rc)
        (send rb 'spair β)
      (β (c)
          (send rc 'shd β)
        (let* ((epsilon 1d-16)
               (c-b (- c b))
               (den (- c-b (- b a)))
               (c-new (if (< (abs den) epsilon)
                          c
                        (- c (/ (* c-b c-b) den)))))
          (declare (real a b c c-b den c-new))
          (send seq cust c-new
                (α (acust)
                  (send aitken acust rb)))
          )))
    )))

(deflex accelerate
  (λ (cust xform s)
    (β  (xs)
        (send repeat β s xform)
      (send smap cust snd xs)
      )))

(deflex fsecond
    (fn-actor-beh 'second))

;; ----------------------------------------------------------------------
;; cfrac-iter -- an evaluator for continued fraction approximations,
;; expressed in the form
;;
;;  f(x) = x0
;;         -------
;;         y0 + x1
;;              -------
;;              y1 + x2
;;                   -------
;;                   y2 + x3 .....
;;
;; could also be written as:
;;
;; f(x) = x0/(y0 + x1/(y1 + x2/(y2 + ...
;;
;; Caller supplies a function fnxy that, when furnished with the index ix,
;; returns the next numerator x[ix] and denominator y[ix], for index ix = 1, 2, ...
;;
;; Also required are the starting values x[0] and y[0].
;;
;; We use accelerated iteration with Aitken's method.
;; Iteration ceases when two successive iterations produce the same answer.
;; That answer is supplied as the result.
;;
;; This is made easier by the use of lazy-streams.
;;
;; --------------------------------------------------------------------

(defun cfrac-term (fnxy)
  (α (cust args)
    (destructuring-bind (ix v p1 q1 p2 q2) args
      (declare (ignore v))
      (destructuring-bind (x0 y0) (funcall fnxy ix)
        (let ((p0 (+ (* y0 p1) (* x0 p2)))
              (q0 (+ (* y0 q1) (* x0 q2))))
          (assert (not (zerop q0)))
          (send cust (list (1+ ix) (/ p0 q0) p0 q0 p1 q1))
          )))))

(deflex erf-stream
  ;; cfrac is: x|1-2*x^2|3+4*x^2|5-6*x^2|7+ ...
  ;; use when x <= 1.7
  (λ (cust x)
    (labels ((fnxy (ix)
                 (let ((sgn (- 1 (* 2 (logand ix 1)))))
                   (list (* sgn 2 ix x x) (+ 1 ix ix))
                   )))
      (β  (s)
          (send repeat β (list 1 x x 1 0 1) (cfrac-term #'fnxy))
        (send smap cust fsecond s)
        ))))

(deflex erfc-stream
  ;; cfrac is: 1|x+(1/2)|x+(2/2)|x+(3/2)|x+ ...
  ;; use when x > 1.7
  (λ (cust x)
    (labels ((fnxy (ix)
               (list (/ ix 2) x)))
      (β  (s)
          (send repeat β (list 1 (/ x) 1 x 0 1) (cfrac-term #'fnxy))
        (send smap cust fsecond s)
        ))))

(deflex erf-raw
  ;; use when x <= 1.7
  (λ (cust x eps)
    (β  (s)
        (send erf-stream β x)
      (β  (as)
          (send accelerate β aitken s)
        (β  (ans)
            (send within β eps as)
          (send cust (* ans
                        (/ 2.0d0 (sqrt pi) (exp (* x x)))))
          )))))

(deflex erfc-raw
  ;; use when x > 1.7
  (λ (cust x eps)
    (β  (s)
        (send erfc-stream β x)
      (β  (as)
          (send accelerate β aitken s)
        (β  (ans)
            (send within β eps as)
          (send cust (* ans
                        (/ 1.0d0 (sqrt pi) (exp (* x x)))))
          )))))

;; -------------------------------------------------------------
;; User callable entry points
;;
;; These entry points determine which of the two raw definitions to call,
;; based on the magnitude of the argument x. When abs(x) = 1.7 both raw
;; definitions require about the same number of accelerated iterations for convergence.
;;
(deflex erfc
  ;; 2/Sqrt(Pi)*Integral(Exp(-t^2), {t, x, inf}) = 1 - erf(x)
  (λ (cust x &optional (eps 1e-8))
    (let ((z  (abs (float x 1d0))))
      (cond ((> z 1.7d0)
             (β  (ans)
                 (send erfc-raw β z eps)
               (send cust (if (minusp x)
                              (- 2d0 ans)
                            ans))
               ))
            (t
             (β  (ans)
                 (send erf-raw β z eps)
               (let ((aans (- 1d0 ans)))
                 (send cust (if (minusp x)
                                (- 2d0 aans)
                              aans)))
               ))
            ))))

(deflex erf
  ;; 2/Sqrt(Pi)*Integral(Exp(-t^2), {t, 0, x}) = 1 - erfc(x)
  (λ (cust x &optional (eps 1d-8))
    (let ((z  (abs (float x 1.0d0))))
      (cond ((> z 1.7d0)
             (β  (ans)
                 (send erfc-raw β z eps)
               (let ((aans (- 1d0 ans)))
                 (send cust (if (minusp x)
                                (- aans)
                              aans))
                 )))

            (t
             (β  (ans)
                 (send erf-raw β z eps)
               (send cust (if (minusp x)
                              (- ans)
                            ans))
               ))
            ))))

;; ---------------------------------------
;; functional interface

(defun fn-erfc (x &optional (eps 1e-12))
  (ask erfc x eps))

(defun fn-erf (x &optional (eps 1e-12))
  (ask erf x eps))

;; ----------------------------------------
#| ;check it out...
(let ((domain '(-3.0d0 3.0d0)))
  (plt:fplot 1 domain #'fn-erfc
             :clear t
             :title "Erfc(x)")
  
  (plt:fplot 2 domain #'fn-erf
             :clear t
             :title "Erf(x)")
  
  (plt:fplot 3 domain (lambda (x)
                        (- (stocks::erfc x)
                           (fn-erfc x)))
             :clear t
             :title "Erfc Approximation Error"))

|#

        