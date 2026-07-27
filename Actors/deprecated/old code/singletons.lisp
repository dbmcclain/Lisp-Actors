
(in-package :com.ral.actors.base)

;; -----------------------------------------------

(defvar *singleton-actors*  nil)
(defvar *singleton-lock*    (mp:make-lock))

(defmacro def-singleton-actor (name &body body)
  ;; useful for defining Actors that behave as a global list
  (let ((backing (intern (format nil "*~A-backing-store*" (symbol-name name))))
        (fetch   (intern (format nil "fetch-~A-backing-store" (symbol-name name)))))
    `(progn
       (defvar ,backing nil)
       (defun ,fetch ()
         (or ,backing
             (singleton-actor ',backing (lambda () ,@body))
             ))
       (define-symbol-macro ,name (,fetch)))
    ))

(defun singleton-actor (sym fn)
  (mp:with-lock (*singleton-lock*)
    (or (symbol-value sym)
        (let ((tmp-actor (funcall fn)))
          (check-type tmp-actor actor)
          (pushnew sym *singleton-actors*)
          (setf (symbol-value sym) tmp-actor))
        )))

(defun reset-singleton-actors ()
  ;; useful for reset ahead of save-image
  (mp:with-lock (*singleton-lock*)
    (dolist (actor *singleton-actors*)
      (setf (symbol-value actor) nil))))

