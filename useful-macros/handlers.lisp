
(in-package :useful-macros)

;; --------------------------------------------------------

(defun expand-handler-bind* (bindings body)
  (if (null bindings)
      `(progn ,@body)
    `(handler-bind (,(car bindings))
       (handler-bind* ,(cdr bindings) ,@body))))

(defmacro handler-bind* (bindings &body body)
  (expand-handler-bind* bindings body))

;; ------------------------------------------------------------

(defun make-handler-case*-with-no-error-case (form cases)
  (let* ((no-error-case (assoc :no-error cases))
         (other-cases   (remove no-error-case cases)))
    (let ((normal-return (gensym "NORMAL-RETURN"))
          (error-return  (gensym "ERROR-RETURN")))
      `(block ,error-return
         (multiple-value-call (lambda ,@(cdr no-error-case))
           (block ,normal-return
             (return-from ,error-return
               (handler-case* (return-from ,normal-return ,form)
                              ,@other-cases)))))
      )))

(defun make-handler-case*-without-no-error-case (form cases)
  (if (null cases)
      form
    `(handler-case (handler-case* ,form ,@(cdr cases))
       ,(car cases))))

(defun expand-handler-case* (form cases)
  (let ((no-error-case-count (count :no-error cases :key #'car)))
    (case no-error-case-count
      (0 (make-handler-case*-without-no-error-case form cases))
      (1 (make-handler-case*-with-no-error-case form cases))
      (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE*."))
      )))

(defmacro handler-case* (form &rest cases)
  (expand-handler-case* form cases))

;; ----------------------------------------------------------

(defvar *call-with-handler-cache* (make-hash-table :test #'equal))
(defvar *call-with-restart-cache* (make-hash-table :test #'equal))

(defun ensure-call-with-handler-function (condition-type)
  (multiple-value-bind (value foundp) (gethash condition-type *call-with-handler-cache*)
    (if foundp
        value
      (let ((lambda-form
             `(lambda (handler thunk)
                (handler-bind ((,condition-type handler))
                  (funcall thunk)))
             ))
        (setf (gethash condition-type *call-with-handler-cache*)
              (compile nil lambda-form)))
      )))

(defun call-with-handler (thunk condition-type handler)
  (funcall (ensure-call-with-handler-function condition-type)
           handler thunk))

;; ---------------------------------

(defun ensure-call-with-restart-function (restart-name interactive-p report-p test-p)
  (let ((key (list restart-name interactive-p report-p test-p)))
    (multiple-value-bind (value foundp) (gethash key *call-with-restart-cache*)
      (if foundp
          value
        (let ((lambda-form `(lambda (restart-function thunk interactive report test)
                              (declare (ignorable interactive report test))
                              (restart-bind
                                  ((,restart-name
                                    restart-function
                                    ,@(when interactive-p `(:interactive-function interactive))
                                    ,@(when report-p      `(:report-function report))
                                    ,@(when test-p        `(:test-function test))))
                                (funcall thunk)))
                           ))
          (setf (gethash key *call-with-restart-cache*)
                (compile nil lambda-form))
          )))))

(defun call-with-restart (thunk restart-name restart-function
                                &key (interactive-function nil interactive-p)
                                (report-function nil report-p)
                                (test-function nil test-p))
  (let ((function (ensure-call-with-restart-function
                   restart-name (and interactive-p t) (and report-p t) (and test-p t))))
    (funcall function restart-function thunk
             interactive-function report-function test-function)))

;; -------------------------------------------

(defun make-handler-bind-case-with-no-error-case (form cases)
  (let* ((no-error-case (assoc :no-error cases))
         (other-cases   (remove no-error-case cases)))
    (let ((normal-return (gensym "NORMAL-RETURN"))
          (error-return  (gensym "ERROR-RETURN")))
      `(block ,error-return
         (multiple-value-call (lambda ,@(cdr no-error-case))
           (block ,normal-return
             (return-from ,error-return
               (handler-bind-case (return-from ,normal-return ,form)
                                  ,@other-cases)))))
      )))

(defun make-handler-bind-case-without-no-error-case (form cases)
  (let ((block-name (gensym "HANDLER-BIND-CASE-BLOCK")))
    (flet ((make-handler-binding (case)
             (destructuring-bind (type lambda-list . body) case
               `(,type (lambda ,lambda-list
                         (return-from ,block-name (locally ,@body))))
               )))
      (let ((bindings (mapcar #'make-handler-binding cases)))
        `(block ,block-name (handler-bind ,bindings ,form))
        ))))

(defun expand-handler-bind-case (form cases)
  (let ((no-error-case-count (count :no-error cases :key #'car)))
    (case no-error-case-count
      (0 (make-handler-bind-case-without-no-error-case form cases))
      (1 (make-handler-bind-case-with-no-error-case form cases))
      (t (error "Multiple :NO-ERROR cases found in HANDLER-BIND-CASE."))
      )))

(defmacro handler-bind-case (form &rest cases)
  (expand-handler-bind-case form cases))

#+LISPWORKS
(progn
  (editor:setup-indent "handler-bind*" 1 2 4)
  (editor:setup-indent "handler-case*" 1 2 4 'handler-case)
  (editor:setup-indent "handler-bind-case" 1 2 4 'handler-case))
