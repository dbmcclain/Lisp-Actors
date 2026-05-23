;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package :common-lisp-user)
;; -------------------------------------------------------------------

(defun skip-underscore (ans)
  (when (and (symbolp ans)
             (find #\_ (symbol-name ans)))
    (let ((num (ignore-errors
                 (read-from-string (remove #\_ (symbol-name ans))))))
      (when (realp num)
        (unintern ans)
        (setf ans num))
      ))
  ans)

;; --------------------------------------------

(defun read-base-number (stream subch pref test-fn reader-fn)
  (let ((str  (make-array 16
                          :element-type 'character
                          :adjustable   t
                          :fill-pointer 0)))
    (um:nlet iter ()
      (let ((ch  (read-char stream nil stream t)))
        (cond ((char= #\_ ch)
               (go-iter))
              ((funcall test-fn ch)
               (vector-push-extend ch str)
               (go-iter))
              (t
               (unless (eq ch stream)
                 (unread-char ch stream))
               (with-input-from-string (s str)
                 (funcall reader-fn s subch pref)))
              )))
    ))

(defun patch-reader-fn (subch &optional base)
  (let ((reader-fn  (get-dispatch-macro-character #\# subch)))
    (set-dispatch-macro-character #\# subch
                                  (if (char-equal subch #\r)
                                      (lambda (stream subch pref)
                                        (let ((test-fn  (um:rcurry #'digit-char-p pref)))
                                          (read-base-number stream subch pref test-fn reader-fn)))
                                    (let ((test-fn (um:rcurry #'digit-char-p base)))
                                      (lambda (stream subch pref)
                                        (read-base-number stream subch pref test-fn reader-fn)))
                                    ))
    ))

;; --------------------------------------------

#+:LISPWORKS
(progn
  (defadvice (read underscore-skipper :around)
      (&rest args)
    (skip-underscore (apply #'call-next-advice args)))
  (defadvice (read-preserving-whitespace underscore-skipper :around)
      (&rest args)
    (skip-underscore (apply #'call-next-advice args)))
  (patch-reader-fn #\x 16)
  (patch-reader-fn #\o  8)
  (patch-reader-fn #\b  2)
  (patch-reader-fn #\r))

;; --------------------------------------------

#+:SBCL
(sb-ext:with-unlocked-packages (:common-lisp)
  (cl-advice:make-advisable 'read)
  (cl-advice:make-advisable 'read-preserving-whitespace)
  (cl-advice:add-advice :around 'read
			(lambda (next-fn &rest args)
                          (skip-underscore (apply next-fn args))))
  (cl-advice:add-advice :around 'read-preserving-whitespace
                        (lambda (next-fn &rest args)
                          (skip-underscore (apply next-fn args))))
  (patch-reader-fn #\x 16)
  (patch-reader-fn #\o  8)
  (patch-reader-fn #\b  2)
  (patch-reader-fn #\r))

