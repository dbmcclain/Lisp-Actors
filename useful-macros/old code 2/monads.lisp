;; monads.lisp -- Monad functions (UNIT, BIND) in Lisp
;;
;; Adapted from Douglas Crockford's Javascript Monads
;;
;; DM/RAL  04/17
;; --------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defpackage #:monad
  (:use #:common-lisp)
  (:export
   #:monad
   #:unit
   #:bind
   #:maybe
   #:make-maybe-unit
   #:meth
   #:lift-value
   #:lift
   #:call
   ))

(in-package #:monad)

;; -------------------------------------------------------------------------

(defmethod do-with-spinlock ((cell cons) fn)
  (loop until (sys:compare-and-swap (car cell) nil mp:*current-process*))
  (prog1
      (funcall fn)
    (sys:compare-and-swap (car cell) mp:*current-process* nil)))

(defmacro with-spin-lock (spinor &body body)
  `(do-with-spinlock ,spinor (lambda () ,@body)))

(editor:setup-indent "with-spin-lock" 1)

;; -------------------------------------------------------------------------

(defclass unit ()
  ;; the unit-methods are a plist of prototype key/fn pairs for use in
  ;; constructing new monad objects
  ((methods  :accessor unit-methods  :initform nil)         ;; mutable plist
   (spinlock :accessor unit-lock     :initform (list nil))) ;; so we need the spinlock
  (:metaclass clos:funcallable-standard-class))

(defclass bind ()
  ;; the bind-methods are a plist of key/fn pairs
  ((methods  :reader bind-methods  :initarg :methods  :initform nil)) ;; immutable plist
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((b bind) &key bind-fn &allow-other-keys)
  (clos:set-funcallable-instance-function b bind-fn))

(defmethod initialize-instance :after ((u unit) &key modifier meth lift lift-value &allow-other-keys)
  (clos:set-funcallable-instance-function u (lambda (value)
                                              (make-instance 'bind
                                                             :methods (with-spin-lock (unit-lock u)
                                                                        (copy-list (unit-methods u)))
                                                             :bind-fn (or (and (functionp modifier)
                                                                               (funcall modifier value))
                                                                          (lambda (fn &rest args)
                                                                            (apply fn value args)))
                                                             )))
  (loop for (key fn) in meth do
        (unsafe-meth u key fn))
  (loop for (key fn) in lift-value do
        (unsafe-meth u key (lift-value-fn fn)))
  (loop for (key fn) in lift do
        (unsafe-meth u key (lift-fn u fn))))

(defun monad (&key modifier meth lift lift-value)
  ;; produce a monad unit function
  (make-instance 'unit
                 :modifier   modifier
                 :meth       meth
                 :lift       lift
                 :lift-value lift-value))

(defmethod unsafe-meth (unit name fn)
  (setf (getf (unit-methods unit) name) fn))

(defmethod meth ((unit unit) name fn)
  ;; fn should be an argument of a monad structure and possibly
  ;; additional args
  (with-spin-lock (unit-lock unit)
    (unsafe-meth unit name fn))
  unit)

(defun lift-value-fn (fn)
  (lambda (monad &rest args)
    (apply monad fn args)))

(defmethod lift-value ((unit unit) name fn)
  ;; add a method to the prototype that calls bind with the function.
  ;; This can be used for ajax mehtods that return values other than
  ;; monads
  (meth unit name (lift-value-fn fn)))

(defun lift-fn (unit fn)
  (lambda (monad &rest args)
    (let ((ans (apply monad fn args)))
      (if (typep ans 'bind)
          ans
        (funcall unit ans)))))

(defmethod lift ((unit unit) name fn)
  ;; add a method to the prototype that calls bind with the function.
  ;; If the value returned is not a monad, then make a monad
  (meth unit name (lift-fn unit fn)))

(defmethod unit ((unit unit) value)
  ;; apply the unit function to a value to produce a bind function
  (funcall unit value))


(defmethod bind ((monad bind) fn &rest args)
  ;; apply the bind function to a function and args to produce a value
  ;; value might be another monad unit function
  (apply monad fn args))

(defmethod call ((monad bind) name &rest args)
  ;; lookup the named function in the bind and apply it with the bind
  ;; function and args
  (um:if-let (fn (getf (bind-methods monad) name))
      (apply fn monad args)
    (error "No method: ~A" name)))

#|
(let* ((ajax (monad
              :lift `((:print ,#'print))))
       (mon  (funcall ajax "Hello world.")))
  (call mon :print))
|#

;; ----------------------------

(defun make-maybe-unit ()
  (monad
   :modifier (lambda (value)
               (unless value
                 #'lw:do-nothing))
   ))

(defun maybe (value)
  ;; generate a maybe monad
  (funcall (make-maybe-unit) value))

#|
(let ((mon (maybe nil)))
  (bind mon #'print))
|#

;; --------------------------------

(defun xmonad ()
  (let (unit)
    (setf unit
          (lambda (value)
            (um:dlambda
              (:result ()  value)
              (:bind   (f &rest args)
               (funcall unit (apply f value args)))
              (:unit   (value)
               (funcall unit value))))
          )))

(let* ((unit (xmonad))
       (mon  (funcall unit 15)))
  (funcall (funcall (funcall (xmonad) 15) :bind (lambda (x) (1+ x))) :result))
