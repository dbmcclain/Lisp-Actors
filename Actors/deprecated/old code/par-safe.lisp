
(in-package com.ral.actors.base)

(defmacro mutable-beh ((&optional preferred-spon) beh)
  (lw:with-unique-names (msg)
    `(lambda* ,msg
       (with-sponsor (or ,preferred-spon base-sponsor)
         (macrolet ((become (new-beh)
                      (setf (actor-beh self) ,new-beh)))
           (apply ,beh ,msg))))
    ))

(defun do-mutable-beh (lock msg beh)
  (if (sys:compare-and-swap (car lock) nil self-sponsor)
      (unwind-protect
          (apply beh msg)
        (setf (car lock) nil))
    ;; else
    (send* (or (car lock)
               self-sponsor)
           self msg)))

(defun beh-lock ()
  (list nil))

(defmacro mutable-alambda (lock &rest clauses)
  `(mutable-beh lock
                (alambda ,@clauses)))

(let (spon)
  (loop until (sys:compare-and-swap (cdr lock) nil t))
  (setf spon (shiftf (car lock)) self-sponsor)
  (when spon
    (setf (car lock) spon))
  (setf (cdr lock) nil)
)

(defmacro mutable-beh ((&optional preferred-sponsor) beh)
  (lw:with-unique-names (lock msg)
    `(let ((,lock (list nil)))
       (lambda* ,msg
         (with-sponsor (or ,preferred-sponsor self-sponsor)
           (if (sys:compare-and-swap (car ,lock) nil t)
               (unwind-protect
                   (apply (macrolet ((become (new-beh)
                                       `(setf (actor-beh self) ,new-beh)))
                            ,beh)
                          ,msg)
                 (setf (car ,lock) nil))
             (send* self ,msg)))
         ))
    ))
  

