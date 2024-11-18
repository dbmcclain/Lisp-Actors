;; scigraph-for-mac-macros.lisp
;; 
(in-package #:com.ral.scigraph)

(defmacro by-proxy (&body body)
  `(sg::proxy-request #'(lambda ()
                          ,@body)))
  
(defmacro def-proxy-fli-function ((name &rest args)
                                  user-args &rest other-args)
  (let ((cname       (um:intern-symbol (format nil "_~A" name)))
        (caller-args (delete :constant 
                             (mapcar #'first user-args)))
        (coercions   (delete nil 
                             (mapcar #'um:coerce-fli-arg user-args))))
    `(progn
       (fli:define-foreign-function (,cname ,@args)
           ,user-args
         ,@other-args)
       (defun ,name ,caller-args
         (let ,coercions
           (sg::by-proxy (,cname ,@caller-args)))))
    ))


(defun rgb-int (v)
  (if (> v 1)
      v
    (round (* 255 v))))

(defun rgb (r g b)
  (let ((ri (rgb-int r))
        (gi (rgb-int g))
        (bi (rgb-int b)))
    (+ (* 256 (+ (* 256 bi) gi)) ri)))


;; ------------------------------------------------
;; Attempt to make SciGraph somewhat thread-safe...
;; Using Reppy Channels with a proxy thread works great,
;; quite a bit slower than simply locking the access to the
;; DLL. In this way, only one thread at a time can be inside
;; the DLL performing graphic operations.
;;
#|
(defvar *sg-process* nil)
(defvar *sg-channel* nil)

(defvar *error-token*  #())
(defvar *answer-token* #())

(defun sg-proxy-thread-fn ()
  (loop
   (destructuring-bind (replyCh fn)
       (rch:recv *sg-channel*)
     (multiple-value-bind (ans cond)
         (ignore-errors
           (multiple-value-list
            (funcall fn)))
       (if cond
           (rch:send replyCh (list *error-token* cond))
         (rch:send replyCh (list *answer-token* ans)))))
   ))
  
(defun sg-proxy-request (fn)
  (unless *sg-process*
	  (setf *sg-channel* (rch:make-channel)
		*sg-process* (mp:process-run-function
			       "IPLib Thread"
			       nil
			       #'sg-proxy-thread-fn)))
  (let ((replyCh (rch:make-channel)))
    (rch:send *sg-channel* (list replyCh fn))
    (destructuring-bind (reply-type val)
        (rch:recv replyCh)
      (cond ((eq reply-type *answer-token*) (apply #'values val))
            ((eq reply-type *error-token*)  (error val))
            (t (error "Unknown response to sg-proxy-request"))))
    ))


(defmacro sg-proxy (&body body)
  `(sg-proxy-request #'(lambda () 
                          ,@body)))
|#
#|
(defvar *sg-lock*
  (mp:make-lock :name "SciGraph Graphics Lock"))

(defmacro with-sg-lock (&body body)
  `(mp:with-lock (*sg-lock* "SG Plotting")
     ,@body))
|#
#| |#
(defmacro with-sg-lock (&body body)
  `(progn
     ,@body))
#| |#

;; ------------------------------------------------
;; Helper functions FOREIGN-SLOT-VALUES, SET-FOREIGN-SLOTS
;; and WITH-DYNAMIC-FOREIGN-STRINGS ---

(defun foreign-slot-values (pobj slotnames)
  (loop for slot in slotnames collect
        (fli:foreign-slot-value pobj slot)))

(defun %set-foreign-slots (pobj slotnames vals)
  (loop for slot in slotnames and val in vals do
	(setf (fli:foreign-slot-value pobj slot) val)))

(defmacro set-foreign-slots (pobj bindings)
  `(%set-foreign-slots ,pobj
                       ',(mapcar #'first bindings)
                       (list ,@(mapcar #'second bindings))))

(defmacro set-foreign-slots-x (ptr pairs)
  `(progn
     ,@(loop for pair in pairs collect
             (destructuring-bind (slot-name slot-value) pair
               `(progn
                  (format t "~%setting slot: ~A" ',slot-name)
                  (setf (fli:foreign-slot-value ,ptr ',slot-name) ,slot-value))
               ))))

;; ----------------------------------------------------------

(defmacro with-dynamic-foreign-strings (bindings &body body)
  (um:with-gensyms (str)
    `(let (,@(mapcar 
              #'(lambda (binding)
                  `(,(first binding)
                    (let ((,str ,(second binding)))
                      (if ,str
                          (fli:convert-to-dynamic-foreign-string ,str)
                        fli:*null-pointer*))))
              bindings))
       ,@body)))

;; --------------------------------------------------------------------
;; macro to ensure that we pass along a :float <carray> to the DLL
;; as an aside, we allow a null parameter to proceed... beware!

(defmethod do-with-float-carray ((c-arr ca:<float-carray>) fn)
  (funcall fn c-arr))

(defmethod do-with-float-carray ((c-arr null) fn)
  (funcall fn c-arr))

(defmethod do-with-float-carray ((c-arr ca:<carray>) fn)
  (let ((farr (ca:convert-to-float c-arr :type :float)))
    (unwind-protect
        (funcall fn farr)
      (ca:discard-carray farr))))

(defmethod do-with-float-carray ((arr array) fn)
  (ca:with-dynamic-float-carray (farr (array-dimensions arr))
    (ca:copy-lisp-array-to-float-carray arr farr)
    (funcall fn farr)))

(defmethod do-with-float-carray ((lst cons) fn)
  (do-with-float-carray (coerce lst 'vector) fn))

(defmacro with-float-carray ((f-name c-arr) &rest body)
  `(do-with-float-carray ,c-arr
                                #'(lambda (,f-name)
                                    ,@body)))

;; --------------------------------------------------------------------
;; macro to ensure that we pass along a 1-D :float <carray> to the DLL
;; as an aside, we allow a null parameter to proceed... beware!

(defmethod do-with-float-cvector ((c-arr ca:<float-carray>) fn)
  (if (= 1 (ca:carray-rank c-arr))
      (funcall fn c-arr)
    (ca:with-overlay-carray (c-vec :dims (ca:carray-total-size c-arr)) c-arr
      (funcall fn c-vec))
    ))

(defmethod do-with-float-cvector ((c-arr null) fn)
  (funcall fn c-arr))

(defmethod do-with-float-cvector ((c-arr ca:<carray>) fn)
  (ca:with-overlay-carray (c-vec :dims (ca:carray-total-size c-arr)) c-arr
    (let ((farr (ca:convert-to-float c-vec :type :float)))
      (unwind-protect
          (funcall fn farr)
        (ca:discard-carray farr)))))

(defmethod do-with-float-cvector ((arr array) fn)
  (ca:with-dynamic-float-carray (farr (array-total-size arr))
    (ca:copy-lisp-array-to-float-carray (vm:vector-of arr) farr)
    (funcall fn farr)))

(defmethod do-with-float-cvector ((lst cons) fn)
  (do-with-float-cvector (coerce lst 'vector) fn))

(defmacro with-float-cvector ((f-name arr) &rest body)
  `(do-with-float-cvector ,arr
                          #'(lambda (,f-name)
                              ,@body)))


;; ----------------------------------------------------------

(defmacro with-long-cvector ((f-name lst) &rest body)
  `(do-with-long-cvector ,lst
                         #'(lambda (,f-name)
                             ,@body)))

(defmethod do-with-long-cvector ((lst cons) fn)
  (ca:with-dynamic-carray (arr :int (length lst))
      (loop for item in lst and ix from 0 do
            (setf (ca:caref arr ix) (truncate item)))
      (funcall fn arr)))

;; ----------------------------------------------------------

(defmacro with-delayed-update (&body body)
  `(progn
     (delay-update)
     (unwind-protect
         (progn
           ,@body)
       (update))))

