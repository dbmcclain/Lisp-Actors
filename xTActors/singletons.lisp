
(in-package :com.ral.actors.base)

;; -----------------------------------------------

(defvar *singleton-actors*  nil)

(defmacro def-singleton-actor (name args &body body)
  ;; useful for defining Actors that behave as a global list
  (lw:with-unique-names (actor tmp-actor)
    `(progn
       (defvar ,actor  nil)
       (defun ,name ,args
         (or ,actor
             (let ((,tmp-actor (progn
                                 ,@body)))
               (assert (actor-p ,tmp-actor))
               (pushnew ',actor *singleton-actors*)
               (setf ,actor ,tmp-actor))
             )))
    ))

(defun reset-singleton-actors ()
  ;; useful for reset ahead of save-image
  (dolist (actor *singleton-actors*)
    (setf (symbol-value actor) nil)))

