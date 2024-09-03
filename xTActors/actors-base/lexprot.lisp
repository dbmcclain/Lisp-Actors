;; lexprot.lisp -- Tim Bradshaw's solution to protecting globally
;; visible bindings from inadvertent mutation
;;
;; DM/RAL 09/24 -- Closure bindings in Actor behaviors must never be
;; mutated. They are concurrently accessible. Local bindings are not
;; concurrently accessible and can be safely mutated. Also - never use
;; DELETE - use REMOVE when trimming sequences in the environment.
;; ---------------------------------------------------------------

(in-package :com.ral.actors.base)

#|
;; (needs (:org.tfeb.hax.collecting :compile t :use t))

;;; Needs to be global for the compiler macro
;;;
(declaim (inline identically))
(defun identically (x) x)
(defun (setf identically) (new var)
 (declare (ignore new var))
 (error "protected"))

(define-compiler-macro identically (x)
  x)

(define-compiler-macro (setf identically) (new var)
 (declare (ignore new var))
 (error "protected"))

(defmacro collecting (form)
  `(um:accum collect
     ,form))

(defmacro protecting-variables ((&key (except '())) &body forms &environment e)
  (send writeln e)
 (let* ((varnames (collecting (system:map-environment
                               e :variable (lambda (name kind info)
                                             (declare (ignore info))
                                             (case kind
                                               (:lexical
                                                (unless (member name except)
                                                  (collect name))))))))
        (hidden-names (mapcar (lambda (name) (make-symbol (string name))) varnames)))
   `(let ,(mapcar #'list hidden-names varnames)
      (symbol-macrolet ,(mapcar (lambda (var hidden)
                                  `(,var (identically ,hidden)))
                                varnames hidden-names)
        (declare (ignorable ,@varnames))
        ,@forms))))
|#

;; -------------------------------------------------
#|
(defun foo (x)
  (protecting-variables (:except (x))
      (setf x 32)
      (values
       (lambda (v)
         (protecting-variables (:except (v))
           (setf x (+ v 32))))
       x)
      ))

(multiple-value-bind (fn val)
    (foo 15)
  (print val)
  (funcall fn 16))

;; ---------------------------------------

(defun foo (x)
  (protecting-variables (:except (x))
    (setf x (+ x 15))))

(foo 32)
|#


(declaim (inline env))
(defun env (x) x)

#|
(defun (setf env) (new var)
 (declare (ignore new var))
 (error "protected"))

(define-compiler-macro (setf env) (new var &environment e)
  (let (is-local)
    (system:map-environment e
                            :variable (lambda (name kind info)
                                        (declare (ignore info))
                                        (unless is-local
                                          (case kind
                                            (:lexical
                                             (cond ((eql name '%marker%)
                                                    (setf is-local :no))
                                                   ((eql name var)
                                                    (setf is-local :yes))
                                                   ))
                                            ))))
    (when (eq is-local :no)
      (error "protected"))
    `(setf ,var ,new)
    ))
|#

(defun root-sym (form)
  (if (consp form)
      (root-sym (cadr form))
    form))

(define-setf-expander env (var &environment e)
  (let ((sym (root-sym var)))
    (when (eq :no
              (block check
                (system:map-environment
                 e
                 :variable (lambda (name kind info)
                             (declare (ignore info))
                             (case kind
                               (:lexical
                                (cond ((eql name '%marker%)
                                       (return-from check :no))
                                      ((eql name sym)
                                       (return-from check :yes))
                                      ))
                               )))
                ))
      (error "protected ~A" sym)))
  (get-setf-expansion var e))

(defmacro prot-vars (&body body)
  `(let (%marker%)
     (declare (ignorable %marker%))
     ,@body))

(defmacro set! (&rest vars-exprs)
  (let ((ct nil))
    `(setf ,@(mapcar (lambda (item)
                       (if ct
                           (or (setf ct nil) item)
                         (and (setf ct t) `(env ,item))))
                     vars-exprs))))

;; --------------------------------
#|
(defun foo (x)
  (setf (env x) 15)
  (prot-vars
    (lambda (v)
      (setf (env x) (+ v x)))
    ))

(foo 32)

(get-setf-expansion 'x)
|#
