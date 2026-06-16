;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package #:com.ral.useful-macros)
;; -------------------------------------------------------------------

(defun patched-numreader (stream subch pref prev-fn)
  (um:with-vanilla-readtable
    (with-input-from-string (s (remove #\_ (read-chars-to-end-of-token stream nil)))
      (funcall prev-fn s subch pref))))

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
(update-ral-syntax)
