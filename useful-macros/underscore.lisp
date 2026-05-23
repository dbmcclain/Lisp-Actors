;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package #:com.ral.useful-macros)
;; -------------------------------------------------------------------

(defun map-array (fn arr)
  (let ((new  (make-array (array-dimensions arr)
                          :element-type (array-element-type arr)
                          :adjustable   (adjustable-array-p arr)
                          :fill-pointer (and (adjustable-array-p arr)
                                             (fill-pointer arr)))
              ))
    (if (cdr (array-dimensions arr))
        (let ((rma-src (make-array (array-total-size arr)
                                   :element-type (array-element-type arr)
                                   :displaced-to arr
                                   :displaced-index-offset 0))
              (rma-dst (make-array (array-total-size arr)
                                   :element-type (array-element-type arr)
                                   :displaced-to new
                                   :displaced-index-offset 0)))
          (map-into rma-dst fn rma-src))
      ;; else
      (map-into new fn arr))
    new))

(defun skip-underscore (ans)
  (cond ((null ans)
         nil)
        ((consp ans)
         (cons (skip-underscore (car ans))
               (skip-underscore (cdr ans))))
        ((arrayp ans)
         (map-array #'skip-underscore ans))
        ((symbolp ans)
         (or (um:read-extended-number-syntax (symbol-name ans))
             ans))
        (t
         ans)
        ))

(defmacro with-extended-number-syntax (&body body)
  `(progn
     ,@(mapcar #'skip-underscore body)))


;; --------------------------------------------

(defun patched-numreader (stream subch pref test-fn prev-fn)
  (let ((tok  (make-array 16
                          :element-type 'character
                          :adjustable   t
                          :fill-pointer 0)))
    (um:nlet iter ()
      (let ((ch  (read-char stream nil stream)))
        (cond ((eq ch stream))
              ((char= #\_ ch)
               (go-iter))
              ((funcall test-fn ch)
               (vector-push-extend ch tok)
               (go-iter))
              (t
               (unread-char ch stream))
              )))
    (with-input-from-string (s tok)
      (funcall prev-fn s subch pref))
    ))

(defun patch-numreader (subch &optional base)
  (let ((prev-fn  (get-dispatch-macro-character #\# subch)))
    (set-dispatch-macro-character #\# subch
                                  (if (char-equal subch #\r)
                                      (lambda (stream subch pref)
                                        (let ((test-fn (um:rcurry #'digit-char-p pref)))
                                          (patched-numreader stream subch pref test-fn prev-fn)))
                                    (let ((test-fn  (um:rcurry #'digit-char-p base)))
                                      (lambda (stream subch pref)
                                        (patched-numreader stream subch pref test-fn prev-fn)))
                                    ))))

(unless (fboundp 'do-nothing)
  (defun do-nothing (&rest ignored)
    (declare (ignore ignored))))


(progn
  (patch-numreader #\x 16)
  (patch-numreader #\o  8)
  (patch-numreader #\b  2)
  (patch-numreader #\r)
  (setf (symbol-function 'patch-numreader) #'do-nothing))

#|
(print #x1_00)
|#
