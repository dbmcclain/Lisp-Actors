;; funcallable-beh.lisp -- What happens if we make Behavior functions into Funcallable Classes?
;;
;; A new style of message format, and a different way of building up
;; Actor behaviors that can be extensible and inheritable.
;;
;; DM/RAL 02/22
;; --------------------------------------------

(in-package :ac)

;; -----------------------------------------------------------

(defclass <behavior> ()
  ()
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((beh <behavior>) &key &allow-other-keys)
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

(defbeh seq-beh ()
  ((hd  :reader seq-hd  :initarg :hd)
   (tl  :reader seq-tl  :initarg :tl)))

(defmethod get-hd ((beh seq-beh) cust)
  (send cust (seq-hd beh)))

(defmethod get-tl ((beh seq-beh) cust)
  (send (seq-tl beh) cust))

(defmethod get-pair ((beh seq-beh) cust)
  (let ((a  (seq-hd beh)))
    (β  (rb)
        (get-tl beh β)
      (send cust a rb)
      )))

(defmethod snth ((beh seq-beh) cust n)
  (cond ((zerop n)
         (send cust (seq-hd beh)))
        (t
         (β (next)
             (get-tl beh β)
           (send next 'snth cust (1- n))))
        ))

(defmethod snthtl ((beh seq-beh) cust n)
  (cond ((zerop n)
         (send cust (seq-tl beh)))
        (t
         (β (next)
             (get-tl beh β)
           (send next 'snthtl cust (1- n)))
         )))

(defun seq (a α-cust)
  (make-actor (make-instance 'seq-beh
                             :hd  a
                             :tl  (lazy α-cust))))

;; -----------------------------------------------

(deflex repeat
  (α (cust a f)
    (send cust (seq a (α (acust)
                        (send repeat acust (funcall f a) f))
                    ))))

(deflex order
  (α (cust seq)
    (β  (a rb)
        (send seq 'get-pair β)
      (β  (b rc)
          (send rb 'get-pair β)
        (β (c)
            (send rc 'get-hd β)
          (let ((ord (or (ignore-errors
                           (round (log (max 1e-20
                                            (- (/ (- a c)
                                                  (- b c))
                                               1))
                                       2)))
                         100)))
            (send cust ord)
            )))
      )))

(deflex within
  (α (cust eps s)
    (β  (a rb)
        (send s 'get-pair β)
      (β (b)
          (send rb 'get-hd β)
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
|#


        