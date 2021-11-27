
(in-package :actors)

;; -----------------------------------------------

(defvar *singleton-actors*  nil)

(defmacro def-singleton-actor (name args &body body)
  ;; useful for defining Actors that behave as a global list
  (lw:with-unique-names (actor)
    `(progn
       (defvar ,actor  nil)
       (defun ,name ,args
         (or ,actor
             (setf ,actor (progn
                            (pushnew ',actor *singleton-actors*)
                            ,@body)))))
    ))

(defun reset-singleton-actors ()
  ;; useful for resume after save-image
  (dolist (actor *singleton-actors*)
    (setf (symbol-value actor) nil)))

