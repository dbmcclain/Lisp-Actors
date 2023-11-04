
(in-package :ac)

;; ----------------------------------------
;; Loosely modeled after McCarthy's AMB operator
;;
;; Simple Generators take a Customer and a Fail Actor, and send a
;; generated item to the Customer, or send to Fail if there are no
;; remaining items. They update their internal state so that the next
;; customer will get the next generated value.
;;
;; AMB is a generator pump. It sends each item from the generator,
;; along with a Next pump Actor, to the Cust. When the Generator runs
;; dry it sends to Fail.  The Cust Actor is expected to receive the
;; Next pump and an item. That Cust can send to Next to try again, or
;; not.

;; -----------------------------------
;; Generator for alternative choices
(defun alt (&rest alts)
  (labels ((alt-beh (&rest alts)
             (alambda
              ((cust fail)
               (cond (alts
                      (become (apply #'alt-beh (cdr alts)))
                      (send cust (car alts)))
                     (t
                      (send fail))
                     ))
              )))
    (create (apply #'alt-beh alts))
    ))
  
;; -------------------------
;; Generator for integer range
(defun int-between (lo hi)
  (labels ((int-between-beh (lo)
             (alambda
              ((cust fail)
               (cond ((<= lo hi)
                      (become (int-between-beh (1+ lo) ))
                      (send cust lo))
                     (t
                      (send fail))
                     )))
             ))
    (create (int-between-beh lo))
    ))

;; -----------------------------
;; The AMB generator pump
(defun amb (cust fail gen)
  ;; Gen furnishes successive items to Cust. If no more items, Gen
  ;; sends to Fail.
  (um:letrec ((next (create
                     (lambda ()
                       (β args
                           (send gen β fail)
                         (send* cust next args)))
                     )))
    (send next)))

;; ------------------------------
;; Generator for Pythagorean Triples
;; - uses AMB internally, but externally it appears to be a simple generator.
(defun pythagorean-triple (lo hi)
  (let ((gen (int-between lo hi)))
    (create
     (alambda
      ((cust fail)
       (β (next-i i)
           (amb β fail gen)
         (β (next-j j)
             (amb β next-i (int-between i hi))
           (β (next-k k)
               (amb β next-j (int-between j hi))
             (if (= (* k k)
                    (+ (* i i) (* j j)))
                 (send cust (list i j k))
               (send next-k))
             ))))
      ))))

#|
(β (next arg)
    (amb β sink
         ;; (alt 1 2 3)
         ;; (int-between 1 5)
         (pythagorean-triple 1 20)
         )
  (send println arg)
  (send next))
|#
;; --------------------------------------------------------
;; McCarthy's AMB is closely related to CALL/CC for reusing
;; continuations. Let's see what happens for Actors....
;;
;; Any particular Cust argument in a SEND message serves as a
;; continuation for the Actor network. By saving that Cust arg and
;; later re-using it in another message, we can end up re-running the
;; Actor network from that point forward with fresh args.
;;
;; So it appears we are already there. The Cust arg in a message is
;; operationally the same as a captured continuation. The convention
;; of sending along a Cust argument in messages is isomorphic to
;; continuation passing style.
;;
;; To the extent that this is useful behavior, it vindicates an
;; earlier design decision when β was constructed. Initial designs
;; held that sending to target β would be a ONCE operation. That was
;; later abandoned to place responsibility for one-timeness on the
;; programmer if needed. My original reason was that ONCE behavior
;; seemed unnecessarily burdensome at runtime for most uses of β.

