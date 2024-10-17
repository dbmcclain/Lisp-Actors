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
;;                         Recover value with FLOAT, or VAL-OF.
;;  NFMT x [fmt]         - makes a self formatting value, and retains the value.
;;                         Recover value with FLOAT, or VAL-OF.
;;  SET-DEFAULT-NFMT fmt - does what it says...
;;  *DEFAULT-NFMT*       - defvar binding for current default NFMT format.
;;  WITH-NFMT fmt body   - changes the default NFMT in the body
;;
;;  FFMT fn x            - shows the result of calling function fn on x
;;                         whenever displayed.
;;  VAL-OF x             - shows the retained value for x
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
;; Base abstract class for augmented value objects
;;
;; Augmented values are a great generalization. Values can be
;; augmented by innate preferred formatting, physical units can be
;; assigned, etc. (See units.lisp)

(defgeneric val-of (x)
  (:method (x)
   x))

(defgeneric display-form (x)
  (:method (x)
   x))

(defclass augmented-value ()
  ((val  :reader val-of  :initarg :val)
   ))

#+:LISPWORKS
(lw:defadvice (float augmented-value :around)
    (x &rest args)
  ;; Gets the original value of FDPL.
  ;;
  ;; NOTE: Advice are not mappable - they only work when the advised
  ;; function is used in function position of a SEXPR.
  (apply #'lw:call-next-advice (val-of x) args))

(defmethod print-object ((obj augmented-value) out-stream)
  (if *print-readably*
      (call-next-method)
    (print-object (display-form obj) out-stream)
    ))

;; --------------------------------------------
;; Rounding to n decimal places for display, while retaining the
;; original value.

(defclass fdpl (augmented-value)
  ;; By making FDPL into a CLOS Class, we enable smoother subclassing
  ;; by user types that wish to extend and take advantage of what FDPL
  ;; offers.
  ((n     :reader fdpl-n        :initarg :n)
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
    ;; (check-type val real)
    (unless w
      ;; Subclasses might provide their own W,F.
      (multiple-value-bind (q r)
          (int-round n (val-of val))
        (setf w q
              f r)
        ))))

(defun fdpl (n x &rest flags)
  ;; round x to n decimal places
  (make-instance 'fdpl
                 :n     n
                 :val   x
                 :flags flags))

#|
FDPL 35 > (list :pi (fdpl 3 pi) :e (fdpl 3 (exp 1)))
(:PI 3.142 :E 2.718)

FDPL 36 > (mapcar 'val-of *)
(:PI 3.141592653589793 :E 2.7182818F0)

FDPL 37 > **
(:PI 3.142 :E 2.718)

FDPL 38 > (float (getf * :pi))
3.141592653589793
|#

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

(defun fdpl-parse-flags (x)
  (let ((flags          (fdpl-flags x))
        (nosep          nil)
        (sep            nil)
        (sign           "")
        (dpl            #\.)
        (comma-char     nil)
        (comma-interval nil)
        (sep-char       nil)
        (sep-interval   nil))
    (setf nosep (member :nosep flags))
    (when(member :sign flags)
      (setf sign "@"))
    (when-let (lst (member :sep flags))
      (when (consp (cadr lst))
        (setf sep (cadr lst))))
    (when-let (lst (member :dpl flags))
      (when (characterp (cadr lst))
        (setf dpl (cadr lst))))
    (setf comma-char     (let ((ch (first sep)))
                           (and (characterp ch)
                                ch))
          comma-interval (let ((int (second sep)))
                           (and (integerp int)
                                (max int 1)))
          sep-char       (let ((ch (third sep)))
                           (and (characterp ch)
                                ch))
          sep-interval   (let ((int (fourth sep)))
                           (and (integerp int)
                                (max int 1))))
    (when nosep
      (setf comma-char nil
            sep-char   nil))
    (values sign       dpl
            comma-char comma-interval
            sep-char   sep-interval)
    ))

(defun fdpl-format-frac (x sep-char sep-interval)
  (let* ((frac  (format nil "~D" (abs (fdpl-f x))))
         (nfrac (length frac))
         (nz    (max 0 (- (fdpl-n x) nfrac)))
         (frac  (if (plusp nz)
                    (concatenate 'string
                                 (make-string nz :initial-element #\0)
                                 frac)
                  frac)))
    (if (and sep-char sep-interval)
        (apply #'paste-strings sep-char
               (um:group frac sep-interval))
      frac)
    ))
    
(defmethod print-object ((x fdpl) out-stream)
  (if *print-readably*
      (call-next-method)
    (without-abbrev
     (multiple-value-bind (sign dpl comma-char comma-interval sep-char sep-interval)
         (fdpl-parse-flags x)
       (let* ((frac  (fdpl-format-frac x sep-char sep-interval))
              (fmt   (concatenate 'string
                                  "~"
                                  (if (and comma-char comma-interval)
                                      (format nil ",,'~c,~d:"
                                              comma-char
                                              comma-interval)
                                    "")
                                  sign
                                  "D"))
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


(let* ((lst '((:aegis . 970.8)
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

(with-nfmt "~,1f"
  (let* ((lst '((:aegis . 970.8)
                (:hsa   .  20)
                (:schw  . 307.5)
                (:gold  . 115)
                (:chase .  46.4)
                (:bachb .  18.3)
                (:bacdm .  24.4)))
         (tot (reduce '+ (mapcar 'cdr lst))))
  `(list
    :total ,tot
    :pcs   ',(mapcar (lambda (pair)
                       `(,(car pair)
                         .
                         ,(/ (* 100 (cdr pair)) tot)
                         ;; ,(/ (round (/ (* 100 (cdr pair)) tot) 0.1) 10.0)
                         ;; ,(* 0.1 (round (/ (* 100 (cdr pair)) tot) 0.1))
                         ))
                     lst))))

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
|#
;; --------------------------------------------
;; Default formatting for NFMT

(defconstant +3g+ "~,3g")

(defvar *default-nfmt*  +3g+)

(defun set-default-nfmt (fmt)
  (setf *default-nfmt* fmt))

(defmacro with-nfmt (fmt &body body)
  ;; Use fmt NIL for system default formatting. Outermost NFMT takes
  ;; precedence over inner, so you can easily override final display.
  `(let ((*default-nfmt*  ,fmt))
     (nfmt (progn
             ,@body))) )

;; --------------------------------------------
;; Uses a specified (or default) format when displaying the value
;; (nfmt = numeric formatter) Affects only floating point values.

(defclass nfmt (augmented-value)
  ((fmt    :reader nfmt-fmt  :initarg :fmt))
  (:default-initargs
   :fmt  *default-nfmt*))

(defun nfmt (x &optional (fmt *default-nfmt*))
  (make-instance 'nfmt
                 :val (val-of x)
                 :fmt fmt))

(defvar *in-nfmt* nil)

(lw:defadvice ((method print-object (float t))
               :nfmt-intercept :around)
    (flt out-stream)
  (if (stringp *in-nfmt*)
      (format out-stream *in-nfmt* flt)
    ;; else
    (lw:call-next-advice flt out-stream)))

(defmethod print-object ((x nfmt) out-stream)
  (if *print-readably*
      (call-next-method)
    (let ((*in-nfmt* 
           (or *in-nfmt*  ;; allows for outer override
               (nfmt-fmt x))))
      (print-object (val-of x) out-stream))
    ))

#|
FDPL 39 > (list :pi (nfmt pi) :e (nfmt (exp 1)))
(:PI 3.14     :E 2.72    )

FDPL 40 > (mapcar 'val-of *)
(:PI 3.141592653589793 :E 2.7182818F0)

FDPL 41 > **
(:PI 3.14     :E 2.72    )

FDPL 42 > (float (getf * :pi))
3.141592653589793

|#
;; --------------------------------------------
;; Shows the result of calling a function on the value whenever displayed.
;; (ffmt = function formatter)

(defclass ffmt (augmented-value)
  ((fn   :reader ffmt-fn  :initarg :fn))
  (:default-initargs
   :fn #'val-of))

(defun ffmt (fn x)
  (make-instance 'ffmt
                 :val (val-of x)
                 :fn  fn))

(defmethod display-form ((x ffmt))
  (funcall (ffmt-fn x) x))

#|
(defun siner (x)
  (ffmt (lambda (x)
          `(,(nfmt x) ,(nfmt (sin (val-of x)))))
        x))

(siner (/ pi 3))

;; --------------------------------------------

(defun labeler (lbl x)
  (ffmt (lambda (x)
          `(,lbl ,(nfmt x "~,3F")))
        x))

FDPL 51 > (list (labeler :pi pi) (labeler :e (exp 1)))
((:PI 3.142) (:E 2.718))

FDPL 52 > (mapcar 'val-of *)
(3.141592653589793 2.7182818F0)

FDPL 53 > **
((:PI 3.142) (:E 2.718))

|#
