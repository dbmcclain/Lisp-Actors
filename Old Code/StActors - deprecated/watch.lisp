;; watch.lisp -- A monitor probe for Actors
;;
;; DM/RAL  04/21
;; ------------------------------------------

(in-package #:stactors/base)

;; ----------------------------------------
;; Watch incoming message traffic for Actor

#|
(defun do-watch (actor wr-fn title)
  (funcall wr-fn (make-actor
                  (um:dlambda
                    (stactors/internal-message:unwatch ()
                     (funcall wr-fn actor))
                    (t (&rest msg)
                       (declare (ignore msg))
                       (log-info :SYSTEM-LOG "~A: ~S" title (whole-message))
                       (repeat-send actor)
                       (signal 'no-immediate-answer))
                    ))
           ))

(defmacro watch (actor-place &optional (title "watch"))
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion actor-place)
    `(let* ,(mapcar #'list vars vals)
       (do-watch ,reader-form
                 (lambda (,(car store-vars))
                   ,writer-form)
                 ,title))
    ))
|#

(defun watch (actor &optional (title "watch"))
  (inject-into-actor actor
    (let ((prev-beh self-beh))
      (become (um:dlambda
                (actors/internal-message:unwatch ()
                   (become prev-beh))
                (t (&rest msg)
                   (log-info :SYSTEM-LOG "~A: ~S" title (whole-message))
                   (apply prev-beh msg))
                )))))

(defun unwatch (actor)
  (send actor 'actors/internal-message:unwatch))

               