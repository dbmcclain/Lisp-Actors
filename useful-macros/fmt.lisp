;; fmt.lisp -- a FORMAT with C-style control strings
;;
;; DM/RAL  2026/05/19 17:42:00 UTC
;; ----------------------------------

(in-package #:com.ral.useful-macros)

;; ----------------------------------

(defun pr-int (x stream &rest args &key (base 10.) (ndig 0) (sep #\,) (sepct 3) (sign #\-))
  (let ((strm  (if (eq stream t)
                   *standard-output*
                 stream)))
    (if *print-readably*
        (if strm
            (write x
                   :stream strm
                   :base   base
                   :radix  t)
          (with-output-to-string (s)
            (apply #'pr-int x s args)))
      
      (um:nlet iter ((ax  (abs x))
                     (dct ndig)
                     (sct (or (and sep sepct)
                              0))
                     (stk nil))
        (cond ((or (plusp ax)
                   (plusp dct)
                   (null stk))
               ;; true at very start - at least 1 digit
               ;; and while dct > 0
               ;; and while ax > 0
               (when (zerop sct)
                 (go-iter ax dct sepct (cons sep stk)))
               (multiple-value-bind (q r)
                   (truncate ax base)
                 (go-iter q (1- dct) (1- sct) (cons (digit-char r base) stk))
                 ))
              
              (t
               ;; finis - we finally exhausted all digits
               (let ((s  (coerce
                          (if (minusp x)
                              (cons #\- stk)
                            (if (eql sign #\-)
                                stk
                              (cons sign stk)))
                          'string)))
                 (if strm
                     (princ s strm)
                   s)))
              ))
      )))

#|
(let ((*print-readably* nil))
  (pr-int 15. t :ndig 6 :sign #\+ :sep #\_ :sepct 4  :base 16.))

(let ((*print-readably* t))
  (pr-int 15. nil :ndig 6 :sign #\+ :sep #\_ :sepct 4  :base 16.))
|#