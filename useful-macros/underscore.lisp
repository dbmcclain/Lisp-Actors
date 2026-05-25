;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package #:com.ral.useful-macros)
;; -------------------------------------------------------------------

(defun nbr-termination-char-p (ch)
  ;; tests for if char terminates a number accumulation
  (or (null ch)
      (whitespace-char-p ch)
      (multiple-value-bind (fn non-terminating)
          (get-macro-character ch)
        (and fn
             (not non-terminating))
        )))

(defun patched-numreader (stream subch pref prev-fn)
  (let ((tok  (make-array 16
                          :element-type 'character
                          :adjustable   t
                          :fill-pointer 0)))
    (um:with-vanilla-readtable
      (um:nlet iter ()
        (let ((ch  (peek-char nil stream nil nil t)))
          (unless (nbr-termination-char-p ch)
            (unless (char= #\_ ch)
              (vector-push-extend ch tok))
            (read-char stream nil nil t)
            (go-iter))
          ))
      (with-input-from-string (s (coerce tok 'string))
        (funcall prev-fn s subch pref))
      )))

;; --------------------------------------------

(defvar *sub-x-reader*
  (with-vanilla-readtable
    (um:rcurry #'patched-numreader
               (get-dispatch-macro-character #\# #\x))))
(defvar *sub-o-reader*
  (with-vanilla-readtable
    (um:rcurry #'patched-numreader
               (get-dispatch-macro-character #\# #\o))))
(defvar *sub-b-reader*
  (with-vanilla-readtable
    (um:rcurry #'patched-numreader
               (get-dispatch-macro-character #\# #\b))))
(defvar *sub-r-reader*
  (with-vanilla-readtable
    (um:rcurry #'patched-numreader
               (get-dispatch-macro-character #\# #\r))))

(defun install-radix-readers (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\x *sub-x-reader* readtable)
  (set-dispatch-macro-character #\# #\o *sub-o-reader* readtable)
  (set-dispatch-macro-character #\# #\b *sub-b-reader* readtable)
  (set-dispatch-macro-character #\# #\r *sub-r-reader* readtable))

(install-radix-readers)
