
(fli:define-foreign-function (fegetround "fegetround" :source)
    ()
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (fesetround "fesetround" :source)
    ((rounding-mode-flag :int))
  :result-type :int
  :language :ansi-c)

(defconstant fe-round-to-nearest  #x0000)
(defconstant fe-round-toward-zero #x0C00)
(defconstant fe-round-upward      #x0800)
(defconstant fe-round-downward    #x0400)

(fli:define-c-struct fenv_t
  (__control (:unsigned :short))
  (__status  (:unsigned :short))
  (__mxcsr   (:unsigned :int))
  (__reserved (:foreign-array :byte (8))))

(fli:define-foreign-function (fegetenv "fegetenv" :source)
    ((envp (:pointer fenv_t)))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (fesetenv "fesetenv" :source)
    ((envp (:pointer fenv_t)))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (feholdexcept "feholdexcept" :source)
    ((envp (:pointer fenv_t)))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (feupdateenv "feupdateenv" :source)
    ((envp (:pointer fenv_t)))
  :result-type :int
  :language :ansi-c)


(fli:define-foreign-variable (_FE_DFL_ENV "_FE_DFL_ENV" :source)
  :type fenv_t
  :accessor :address-of)

(fli:define-foreign-variable (_FE_DFL_DISABLE_SSE_DENORMS_ENV
                              "_FE_DFL_DISABLE_SSE_DENORMS_ENV" :source)
  :type fenv_t
  :accessor :address-of)

(defun do-without-denormals (fn)
  (fli:with-dynamic-foreign-objects ()
    (let ((env (fli:allocate-dynamic-foreign-object
                :type 'fenv_t)))
      (fegetenv env)
      (fesetenv (_FE_DFL_DISABLE_SSE_DENORMS_ENV))
      (unwind-protect
          (funcall fn)
        (fesetenv env))
      )))

(defmacro without-denormals (&body body)
  `(do-without-denormals (lambda ()
                           ,@body)))


#|
(defun tst()
  (without-denormals
    (assert (zerop (* 1.2d0 least-positive-double-float)))))

(defun check-env ()
  (fli:with-dynamic-foreign-objects ()
    (let ((env (fli:allocate-dynamic-foreign-object
                :type 'fenv_t)))
      (fegetenv env)
      (inspect (fli:foreign-slot-value env '__mxcsr)))))

(defun tst (x)
  (declare (double-float x))
  (fli:with-dynamic-foreign-objects ()
    (let ((env (fli:allocate-dynamic-foreign-object
                :type 'fenv_t)))
      (fegetenv env)
      (fesetenv (_FE_DFL_DISABLE_SSE_DENORMS_ENV))
      (unwind-protect
          (* x least-positive-double-float)
        (fesetenv env))
      )))

(defun tst (x)
  (declare (double-float x))
  (vm:without-denormals
    (check-env)
    (assert (zerop (* x least-positive-double-float)))))

|#
