
(in-package :um)

(eval-always
  ;; --------------------------------------
  ;; Reader macro for #f
  ;; sets compiler optimization levels
  ;; #0f .. #3f   #f defaults to #3f for fastest code
  (defun |reader-for-#F| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (unless *read-suppress*
      (setq numarg (or numarg 3))
      (unless (<= numarg 3)
        (error "Bad value for #f: ~a" numarg))
      `(declare (optimize (speed ,numarg)
                          (safety ,(- 3 numarg))
                          #+LISPWORKS (float ,(- 3 numarg))
                          ))))
  
  (set-dispatch-macro-character #\# #\f '|reader-for-#F|))

(defmacro fast-progn (&rest body)
  `(locally #f ,@body))

(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))

