
(in-package #:actors)

(defpan warp-me (args) (pargs)
  (let ((old-this um:this))
    (become (:args args
             :pargs pargs)
      (dlambda
        (:op1 () (do-something))
        (t (&rest arg)
           (apply old-this args)))
      )))


(defun wrap-actor (actor args) (pargs)
  (let ((penv (actor-behavior actor)))
    (um:with-pandoric (this) penv
      (let ((old-this this))
        (become (:args  args
                 :pargs pargs)
          (um:dlambda 
            (:op1 () (do-something))
            (t (&rest msg)
               (apply old-this msg))
            ))))))

(defmacro blindly (&body body)
  ;; like CONSTANTLY, but does not pre-evaluate the body
  (let ((g!args (gensym)))
    `(lambda (&rest ,g!args)
       ,@body)))
    

(defun divulge (actor)
  ;; show the pandoric bindings inside an actor
  (um::pinspect (actor-behavior actor)))

(pprint
 (divulge (find-actor :rstkv)))

(um:with-pandoric (a b c) box
  body)