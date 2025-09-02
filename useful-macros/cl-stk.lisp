;; cl-stk.lisp
;;
;; DM/RAL  2024/10/31 08:12:14 UTC
;; ----------------------------------

(defpackage #:cl-stk
  (:use #:common-lisp :ac)
  (:export
   #:fdpl
   #:fdpl+
   #:ndpl
   #:ndpl+
   #:hms
   #:hms+
   #:hm
   #:hm+
   #:dms
   #:dms+
   #:dm
   #:dm+
   #:unipolar
   #:bipolar
   ))

(in-package #:cl-stk)

;; ----------------------------------
;;
;; The code here represents a facsimile of STC Forth.
;;
;; Watch out! Using Forth-like (un-parenthesized) syntax within Lisp
;; tagbodies is dangerous practice. Those un-parenthesized primitives
;; look like Tags to Lisp, and it will fail to compile in the function
;; call.
;;
;; DO, DOTIMES, DOLIST, etc, have implicit tagbodies. If there is any
;; doubt, place the unadorned symbols in a PROGN.

(defun make-pad ()
  (make-array 80
              :element-type 'character
              :adjustable   t
              :fill-pointer 0))

(defvar *default-env*
  '(:dp-char        #\.
    :colon-char     #\:
    :comma-char     #\,
    :comma-interval 3
    :sep-char       #\_
    :sep-interval   5
    :width          0
    :fill-char      #\space
    :ndpl           2))
    
(defstruct vm
  (sp   nil)
  (pad  (make-pad))
  (env  *default-env*))

(defvar *vm*  (make-vm))

(defmacro with-vm (args &body body)
  `(let ((*vm*  (make-vm
                 :env (append ,args *default-env*))
                ))
     ,@body))

(define-symbol-macro sp   (vm-sp  *vm*))
(define-symbol-macro pad  (vm-pad *vm*))
(define-symbol-macro env  (vm-env *vm*))
(define-symbol-macro base *print-base*)

(define-symbol-macro sp@+ (pop sp))
(defmacro sp-! (val)
  `(push ,val sp))
(define-symbol-macro tos  (car sp))
(define-symbol-macro nos  (cadr sp))

(define-symbol-macro dp-char        (getf env :dp-char #\.))
(define-symbol-macro colon-char     (getf env :colon-char #\:))
(define-symbol-macro comma-char     (getf env :comma-char #\,))
(define-symbol-macro comma-interval (getf env :comma-interval 3))
(define-symbol-macro sep-char       (getf env :sep-char #\_))
(define-symbol-macro sep-interval   (getf env :sep-interval 5))
(define-symbol-macro width          (getf env :width 0))
(define-symbol-macro pad-char       (getf env :pad-char #\Space))
(define-symbol-macro ndpl           (getf env :ndpl 0))

(defmacro defprim (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (define-symbol-macro ,name (,name))))

(defprim dup
  (sp-! tos))

(defprim swap
  (rotatef tos nos))

(defprim drop
  sp@+)

(defprim rot
  (destructuring-bind (a b c . rest) sp
    (setf sp `(,c ,a ,b ,@rest))))

(defprim -rot
  (destructuring-bind (a b c . rest) sp
    (setf sp `(,b ,c ,a ,@rest))))

;; --------------------------------------------

(defun lit (x)
  (sp-! x))

(defprim <pad ()
  (setf (fill-pointer pad) 0))

(defprim pad> ()
  (sp-! (nreverse pad))
  (padding-to-width))

(defmacro pad-! (ch)
  `(vector-push-extend ,ch pad))

(defprim digit
  (multiple-value-bind (q r)
      (truncate tos base)
    (setf tos q)
    (pad-! (digit-char r base))
    ))

(defprim digits
  (digit)
  (let ((interval  comma-interval))
    (if (and (characterp comma-char)
             (plusp interval))
        (do ((ix  1  (1+ ix)))
            ((zerop tos) (drop))
          (when (zerop (mod ix interval))
            (>comma))
          (digit))
      ;; else
      (do ()
          ((zerop tos) (drop))
        (digit)))
    ))

(defprim ndigits
  (dotimes (_ sp@+)
    (digit)
    ))

(defprim prep-to-int
  ;; Leave original number on the stack,
  ;; and push a scaled absolute value integer on the stack.
  (sp-! (abs
         (round
          (* tos
             (expt 10. ndpl) )))
        ))

(defprim sign
  (if (minusp sp@+)
      (pad-! #\-)))

(defprim sign+
  (pad-! (if (minusp sp@+) #\- #\+)))

(defprim >char
  (pad-! sp@+))

(defprim >colon
  (pad-! colon-char))

(defprim >dp
  (pad-! dp-char))

(defprim >sep
  (pad-! sep-char))

(defprim >comma
  (pad-! comma-char))

(defprim >padding
  (pad-! pad-char))

(defprim sexi-digit
  (let ((*print-base* 6.))
    digit))

(defprim sexi-pair
  (let ((*print-base* 10.))
    digit sexi-digit >colon))

(defprim >frac
  (let ((ndpl ndpl))
    (when (plusp ndpl)
      (let ((interval sep-interval))
        (if (and (characterp sep-char)
                 (plusp interval))
            (do ((off (- interval
                         (mod ndpl interval)))
                 (ix 0 (1+ ix)))
                ((>= ix ndpl))
              (when (and (plusp ix)
                         (zerop (mod (+ off ix) interval)))
                (>sep))
              (digit))
          ;; else
          (dotimes (_ ndpl)
            (digit)
            ))
        (>dp)
        ))))

(defprim padding-to-width
  (let ((pad-char  pad-char)
        (width     width)
        (len       (length tos)))
    (when (< len width)
      (setf tos
            (concatenate 'string
                          (make-string (- width len)
                                      :initial-element pad-char)
                          tos)
            ))))
        
;; --------------------------------------------

(defprim >hmsu
  (let ((*print-base* 10.))
    >frac sexi-pair sexi-pair digits drop))

(defprim >hms
  dup >hmsu sign)

(defprim >hms+
  dup >hmsu sign+)

;; --------------------------------------------

(defprim >hmu
  >frac sexi-pair digits drop)

(defprim >hm
  dup >hmu sign)

(defprim >hm+
  dup >hmu sign+)

;; --------------------------------------------

(defun unipolar (x)
  (mod x 1.0))

(defun bipolar (x)
  (- (unipolar (+ x 0.5)) 0.5))

;; --------------------------------------------

(defun %hms (fn turns env-args)
  (with-vm env-args
     (lit (* 86400. turns))
     prep-to-int
     <pad (funcall fn) pad>
     sp@+))

(defun hms (turns &rest env-args)
  (%hms '>hms (unipolar turns) env-args))

(defun hms+ (turns &rest env-args)
  (%hms '>hms+ (bipolar turns) env-args))

;; --------------------------------------------

(defun %hm (fn turns env-args)
  (with-vm env-args
     (lit (* 1440. turns))
     prep-to-int
     <pad (funcall fn) pad>
     sp@+))

(defun hm (turns &rest env-args)
  (%hm '>hm (unipolar turns) env-args))

(defun hm+ (turns &rest env-args)
  (%hm '>hm+ (bipolar turns) env-args))

;; --------------------------------------------

(defun %dms (fn turns env-args)
  (with-vm env-args
     (lit (* 1296000. turns))
     prep-to-int
     <pad (funcall fn) pad>
     sp@+))

(defun dms (turns &rest env-args)
  (%dms '>hms (unipolar turns) env-args))

(defun dms+ (turns &rest env-args)
  (%dms '>hms+ (bipolar turns) env-args))

;; --------------------------------------------

(defun %dm (fn turns env-args)
  (with-vm env-args
     (lit (* 21600. turns))
     prep-to-int
     <pad (funcall fn) pad>
     sp@+))

(defun dm (turns &rest env-args)
  (%dm '>hm (unipolar turns) env-args))

(defun dm+ (turns &rest env-args)
  (%dm '>hm+ (bipolar turns) env-args))

;; --------------------------------------------

(defun %ndpl (fn-sign ndpl x env-args)
  (with-vm (list* :ndpl ndpl env-args)
    (lit x)
    prep-to-int
    <pad >frac digits (funcall fn-sign) pad>
    sp@+))
  
(defun ndpl (ndpl x &rest env-args)
  (%ndpl #'sign ndpl x env-args))

(defun ndpl+ (ndpl x &rest env-args)
  (%ndpl #'sign+ ndpl x env-args))

;; --------------------------------------------

(defun %fdpl (fn-sign x env-args)
  (with-vm env-args
    (lit x)
    prep-to-int
    <pad >frac digits (funcall fn-sign) pad>
    sp@+))
  
(defun fdpl (x &rest env-args)
  (%fdpl #'sign x env-args))

(defun fdpl+ (x &rest env-args)
  (%fdpl #'sign+ x env-args))

;; --------------------------------------------
#|
(hms+ 0.79 :ndpl 1 :width 12)
(fmt:fmt* t (ndpl 3 (* 1e7 pi)) " sec/yr")
(/ (* 1e7 pi) 86400.)
(directory "*.txt")
|#
#|
(define-symbol-macro doit (doit))
(defun doit ()
  (print :doit))
(dotimes (ix 3)
  doit
  (print :ok))

|#
#|
(defclass thing ()
  ((tx :accessor thing-tx :initarg :tx :allocation :class)
   ))

(defclass subthing (thing)
  ((tx :accessor thing-tx :initarg :tx :allocation :class)
   ))

(let ((t1 (make-instance 'thing :tx 15))
      (t2 (make-instance 'subthing :tx 32)))
  (list (thing-tx t1)
        (thing-tx t2)))
|#
