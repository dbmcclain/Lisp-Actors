;; fdpl.lisp - Number objects that format themselves
;;
;; Usefully rounding floating point number to N decimal places while
;; still carrying its full value.
;;
;; Allows to display a convenient shortened version without needing to
;; FORMAT the number.
;;
;; Easy to extend by subclassing, to provide many custom display
;; formats. See astro/angle-output-suppl.lisp for examples.
;;
;;  DPL n x              - rounds to n dpl (and changes the value)
;;  FDPL n x {flags}     - rounds to n dpl on all displays, but retains the value.
;;                         Recover value with FLOAT.
;;  NFMT x [fmt]         - makes a self formatting value, and retains the value.
;;                         Recover value with FLOAT.
;;  SET-DEFAULT-NFMT fmt - does what it says...
;;  *DEFAULT-NFMT*       - defvar binding for current default NFMT format.
;;
;; Optional flags for FDPL - :SIGN         -- always show leading sign
;;                           :NOSEP        -- don't use any commas or separators
;;                           :SEP (comma-char, comma-interval, sep-char, sep-interval)
;;                           :DPL dpl-char -- change the decimal point character.
;;
;; DM/RAL  2024/10/15 03:54:40 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.fdpl
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.fdpl)

;; --------------------------------------------
;; Rounding to n decimal places - actually changes the number value

(defun dpl (n x)
  (let ((sf  (if (< n 5)
                 (aref #.#(1.0 10.0 100.0 1000.0 10000.0) n)
               (expt 10 n))))
    (/ (round (* sf x)) sf)
    ))

;; --------------------------------------------
;; worker function

(defun int-round (n x)
  ;; Split fp x to n dpl, into whole and frac integers
  (let ((sf (if (< n 5)
                (aref #.#(1 10 100 1000 1000) n)
              (expt 10 n))))
    (declare (integer sf))
    (truncate (round (* sf x)) sf)
    ))

;; --------------------------------------------
;; Base abstract class for self-reporting objects

(defclass self-formatting ()
  ((val  :reader sfmt-val  :initarg :val)))

(defgeneric self-formatting-p (x)
  (:method ((x self-formatting))
   t)
  (:method (x)
   nil))

#+:LISPWORKS
(lw:defadvice (float :self-formatting :around)
    (x &rest args)
  ;; Gets the original value of FDPL
  (apply #'lw:call-next-advice
         (if (self-formatting-p x)
             (sfmt-val x)
           x)
         args))

;; --------------------------------------------
;; Rounding to n decimal places for display, while retaining the
;; original value.

(defclass fdpl (self-formatting)
  ;; By making FDPL into a CLOS Class, we enable smoother subclassing
  ;; by user types that wish to extend and take advantage of what FDPL
  ;; offers.
  ((val   :reader fdpl-val)
   (n     :reader fdpl-n        :initarg :n)
   (w     :reader fdpl-w        :initarg :w)
   (f     :reader fdpl-f        :initarg :f)
   (flags :reader fdpl-flags    :initarg :flags))
  (:default-initargs
   :w     nil
   :flags '(:sep (#\, 3 #\_ 5))
   ))

(defmethod initialize-instance :after ((obj fdpl) &key &allow-other-keys)
  (with-slots (val n w f) obj
    (check-type n   (integer 0))
    (check-type val real)
    (unless w
      ;; Subclasses might provide their own W,F.
      (multiple-value-bind (q r)
          (int-round n val)
        (setf w q
              f r)
        ))))

(defun fdpl (n x &rest flags)
  ;; round x to n decimal places
  (make-instance 'fdpl
                 :n     n
                 :val   x
                 :flags flags))

;; --------------------------------------------

(defun fdpl-parse-flags (x)
  (let ((flags          (fdpl-flags x))
        (nosep          nil)
        (sep            '(#\, 3 #\_ 5))
        (sign           "D")
        (dpl            #\.)
        (comma-char     #\,)
        (comma-interval 3)
        (sep-char       #\_)
        (sep-interval   5))
    (when (member :sign flags)
      (setf sign "@D"))
    (setf nosep (member :nosep flags))
    (when-let (lst (member :sep flags))
      (when (consp (cadr lst))
        (setf sep (cadr lst))))
    (when-let (lst (member :dpl flags))
      (when (characterp (cadr lst))
        (setf dpl (cadr lst))))
    (setf comma-char     (or
                          (let ((ch (first sep)))
                            (and (characterp ch)
                                 ch))
                          #\,)
          comma-interval (max 1
                              (or
                               (let ((int (second sep)))
                                 (and (integerp int)
                                      int))
                               3))
          sep-char       (or
                          (let ((ch (third sep)))
                            (and (characterp ch)
                                 ch))
                          #\_)
          sep-interval   (max 1
                              (or
                               (let ((int (fourth sep)))
                                 (and (integerp int)
                                      int))
                               5)) )
    (values nosep      sign       dpl
            comma-char comma-interval
            sep-char   sep-interval)
    ))

(defun fdpl-format-frac (x nosep sep-char sep-interval)
  (let* ((frac  (format nil "~D" (abs (fdpl-f x))))
         (nfrac (length frac))
         (nz    (max 0 (- (fdpl-n x) nfrac)))
         (frac  (if (plusp nz)
                    (concatenate 'string
                                 (make-string nz :initial-element #\0)
                                 frac)
                  frac)))
    (if nosep
        frac
      (apply #'paste-strings sep-char
             (um:group frac sep-interval))
      )))
    
(defmethod print-object ((x fdpl) out-stream)
  (if *print-readably*
      (call-next-method)
    (without-abbrev
     (multiple-value-bind (nosep sign dpl comma-char comma-interval sep-char sep-interval)
         (fdpl-parse-flags x)
       (let* ((frac  (fdpl-format-frac x nosep sep-char sep-interval))
              (fmt    (concatenate 'string
                                   "~"
                                   (if nosep
                                       ""
                                     (format nil ",,'~c,~d:"
                                             comma-char
                                             comma-interval))
                                   sign))
              (whole  (format nil fmt (fdpl-w x))))
         (format out-stream "~A~c~A" whole dpl frac)
         )))
    ))

;; --------------------------------------------
#|
(let* ((lst '((:aegis . 971.4)
              (:hsa   .  20)
              (:schw  . 307.5)
              (:gold  . 115)
              (:chase .  46.4)
              (:bachb .  18.3)
              (:bacdm .  24.4)))
       (tot (reduce '+ (mapcar 'cdr lst))))
  `(list
    :total ,(fdpl 1 tot)
    :pcs   ',(mapcar (lambda (pair)
                       `(,(car pair)
                         .
                         ,(fdpl 1 (/ (* 100 (cdr pair)) tot))
                         ;; ,(* 0.1 (round (/ (* 100 (cdr pair)) tot) 0.1))
                         ))
                     lst)))


(let* ((lst '((:aegis . 971.4)
              (:hsa   .  20)
              (:schw  . 307.5)
              (:gold  . 115)
              (:chase .  46.4)
              (:bachb .  18.3)
              (:bacdm .  24.4)))
       (tot (reduce '+ (mapcar 'cdr lst))))
  `(list
    :total ,(dpl 1 tot)
    :pcs   ',(mapcar (lambda (pair)
                       `(,(car pair)
                         .
                         ,(dpl 1 (/ (* 100 (cdr pair)) tot))
                         ;; ,(/ (round (/ (* 100 (cdr pair)) tot) 0.1) 10.0)
                         ;; ,(* 0.1 (round (/ (* 100 (cdr pair)) tot) 0.1))
                         ))
                     lst)))

(defun dpl (n x)
  (let ((sf  (if (< n 5)
                 (aref #.#(1.0 10.0 100.0 1000.0 10000.0) n)
               (expt 10 n))))
    (/ (round (* sf x)) sf)
    ))

(defun bad-dpl (n x)
  (let ((sf  (if (< n 5)
                 (aref #.#(1.0 0.1 0.01 0.001 0.0001) n)
               (/ (expt 10 n)))))
    (* sf (round x sf))
    ))

(setf *print-base* 16.)
(setf *print-base* 10.)
(mapcar 'integer-decode-float `(10.0 0.1 ,(* (round 1.1 0.1) 0.1) ,(/ (round (* 10.0 1.0)) 10.0)))

(mapcar 'integer-decode-float `(,(dpl 1 (/ 971.4 1503.0)) ,(bad-dpl 1 (/ 971.4 1503.0))))
;; --------------------------------------------
|#

(defconstant +3g+ "~,3g")

(defvar *default-nfmt*  +3g+)

(defun set-default-nfmt (fmt)
  (setf *default-nfmt* fmt))

(defclass nfmt (self-formatting)
  ((val    :reader nfmt-val)
   (fmt    :reader nfmt-fmt  :initarg :fmt))
  (:default-initargs
   :fmt  *default-nfmt*))

(defun nfmt (x &optional (fmt *default-nfmt*))
  (make-instance 'nfmt
                 :val x
                 :fmt fmt))

(defmethod print-object ((x nfmt) out-stream)
  (if *print-readably*
      (call-next-method)
    (format out-stream (nfmt-fmt x) (nfmt-val x))
    ))

