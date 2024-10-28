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
;; Augmented numbers that display with N decimal places

(defclass fdpl (augmented-value)
  ((ndpl      :accessor fdpl-ndpl  :initarg :ndpl)
   (fmt       :accessor fdpl-fmt   :initarg :fmt)
   (flags     :accessor fdpl-flags :initarg :flags)
   (dsply     :accessor fdpl-dsply :initform nil))
  (:default-initargs
   :ndpl   2
   :fmt    '(sign ds dp nd)
   :flags  '(:dpchar         #\.
             :comma-char     #\,
             :comma-interval 3
             :sep-char       #\_
             :sep-interval   5)
   ))

(defgeneric fdpl-prepval (x)
  (:method (x)
   (val-of x)))

(let ((cache-lock  (mpc:make-lock))
      (cache-alist nil))
  
  (defun get-formatter-cache (fmt)
    (or (cdr (assoc fmt cache-alist :test 'equalp))
        (mpc:with-lock (cache-lock)
          (or (cdr (assoc fmt cache-alist :test 'equalp))
              (let* ((formatter (compile nil (eval `(picfmt:pic-formatter ,fmt))))
                     (new-cache (acons fmt formatter cache-alist)))
                (setf cache-alist new-cache)
                formatter))
          ))
    ))

(defmethod print-object ((x fdpl) out-stream)
  (if *print-readably*
      (call-next-method)
    (um:without-abbrev
      (with-accessors ((val       fdpl-prepval)
                       (fmt       fdpl-fmt)
                       (ndpl      fdpl-ndpl)
                       (flags     fdpl-flags)
                       (dsply     fdpl-dsply)) x
        (let* ((*print-base*   10.)
               (*print-escape* nil)
               (dsply  (or dsply
                           (setf dsply
                                 (apply (get-formatter-cache fmt)
                                        val :ndpl ndpl flags)
                                 ))))
          (print-object dsply out-stream)
          )))
    ))

(defun fdpl (n val &rest args)
  (apply #'make-instance 'fdpl
         :val  val
         :ndpl n
         args))

#|
(fdpl 3 pi)
|#
    
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
              (:schw  . 307.1)
              (:gold  . 115)
              (:chase .  44.0)
              (:bachb .  21.4)
              (:bacdm .  26.4)))
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

;; --------------------------------------------
;; --------------------------------------------
(with-nfmt "~,1f"
  (let* ((lst `((:aegis . 955.0)
                (:hsa   .  20.0)
                (:schw  . 307.1)
                (:gold  . ,(* 2720/1000 43.2))
                (:chase .  53.9)
                (:bachb .  21.4)
                (:bacdm .  26.4)))
         (tot (reduce '+ (mapcar 'cdr lst))))
    `(list
      :total ,tot
      :pcs   ,(mapcar (lambda (pair)
                        `(,(car pair)
                          ,(cdr pair)
                          ,(/ (* 100 (cdr pair)) tot)
                          ;; ,(/ (round (/ (* 100 (cdr pair)) tot) 0.1) 10.0)
                          ;; ,(* 0.1 (round (/ (* 100 (cdr pair)) tot) 0.1))
                          ))
                      lst))
    ))
;; --------------------------------------------
;; --------------------------------------------

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
(defmacro with-nfmt (fmt &body body)
  ;; Use fmt NIL for system default formatting. Outermost NFMT takes
  ;; precedence over inner, so you can easily override final display.
  `(let ((*default-nfmt*  ,fmt))
     (nfmt (progn
             ,@body))) )
|#
(defmacro with-nfmt (fmt &body body)
  ;; Use fmt NIL for system default formatting. Outermost NFMT takes
  ;; precedence over inner, so you can easily override final display.
  `(let* ((*in-nfmt*  ,fmt)
          (*default-nfmt* *in-nfmt*))
     (nfmt (progn
             ,@body))
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
