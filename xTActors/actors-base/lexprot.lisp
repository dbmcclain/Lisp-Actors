;; lexprot.lisp -- Tim Bradshaw's solution to protecting globally
;; visible bindings from inadvertent mutation
;;
;; DM/RAL 09/24 -- Closure bindings in Actor behaviors must never be
;; mutated. They are concurrently accessible. Local bindings are not
;; concurrently accessible and can be safely mutated. Also - never use
;; DELETE - use REMOVE when trimming sequences in the environment.
;; ---------------------------------------------------------------

(in-package :com.ral.actors.base)

;; (needs (:org.tfeb.hax.collecting :compile t :use t))

;;; Needs to be global for the compiler macro
;;;
(declaim (inline identically))
(defun identically (x) x)
(defun (setf identically) (new var)
 (declare (ignore new var))
 (error "protected"))

(define-compiler-macro (setf identically) (new var)
 (declare (ignore new var))
 (error "protected"))

(defmacro collecting (form)
  `(um:accum collect
     ,form))

(defmacro protecting-variables ((&key (except '())) &body forms &environment e)
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

(defmacro behav (form)
  `(protecting-variables ()
     ,form))

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
