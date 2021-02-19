;; DM/RAL 02/21
;;
;; Note: Use of DEFALIAS and REDIRECT affect all threads with a global
;; switch of functionality. If you want thread-safe behavior, you
;; should use AOP:DEFDYNFUN and AOP:DFLET.

(in-package :useful-macros)

(defclass aliasfn ()
  ((fn  :accessor alias-fn  :initarg :fn))
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((obj aliasfn) &key &allow-other-keys)
  (clos:set-funcallable-instance-function obj
                                          (lambda (&rest args)
                                            (apply (alias-fn obj) args))))

(defmethod redirect ((obj aliasfn) fn)
  (setf (alias-fn obj) fn))

(defmacro defalias (name fn)
  `(progn
     (defun ,name (&rest args)
       (declare (ignore args))
       nil)
     (setf (symbol-function ',name) (make-instance 'aliasfn
                                                   :fn ',fn))))

