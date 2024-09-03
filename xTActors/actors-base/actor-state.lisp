;; actor-state.lisp -- Alternative to BOA lists of state vars
;; DM/RAL  2023/12/07 10:08:39 UTC
;; ----------------------------------------------------------

(in-package :com.ral.actors.base)

;; ----------------------------------------------------------
;; What problem is this code trying to solve?
;;
;; Well, when you have a large number of state vars for an Actor
;; behavior, the use of BOA arglists (By Order of Argument) becomes
;; cumbersome to use. CommonLisp has keyword parameters in lambda
;; lists. But these suffer from two problems:
;;
;;   1. Any changes to arglists are usually done by pre-pending new
;;   keyword args in front of existing args. That leaves stale data
;;   laying around in addition to redefined values. -- and --
;;
;;   2. That stale data occupies memory preventing GC reclamation.
;;
;; So here we define ACTOR-STATE with clean augmentation semantics.
;;
;; Declare an ACTOR-STATE in your behavior code, stating the items by
;; name. Keep in mind that Actor state is visible to, and shared by,
;; all parallel executions.
;;
;; Use STATE-WITH to construct a fresh state from an existing state,
;; copying existing content, replacing old values by name, and
;; augmenting state with additonal values.
;;
;; Use STATE-WITHOUT to construct a fresh state from an existing
;; state, eliding keys and their values.
;;
;; These actions allows old values to be taken by GC.
;;
;; Pass existing state, in whole, to other functions, such as used in
;; CREATE for defining new behavior.
;;
;; We offer WITH-STATE-VALS as a convenient way to refer to the actual
;; state vars in the property list - much like WITH-ACCESSORS.
;;
;; You can also set default values in case the names were not already
;; in the plist. All of the accessor names are read-only.
;;
;; -------------------------------------------------------------------

(defstruct actor-state
  (plist nil :read-only t))

(defun validate-plist (plist)
  (loop for key in plist by #'cddr do
          (assert (symbolp key)))
  plist)

(defun actor-state (&rest plist)
  (make-actor-state
   :plist (validate-plist plist)))

(defun trim-plist (plist removals)
  (loop for key in removals do
          (remf plist key))
  plist)

(defmethod state-with ((state actor-state) &rest props)
  (let* ((new-plist (copy-list (actor-state-plist state)))
         (elisions  (getf props :without new-plist)))
    (unless (eq elisions new-plist)
      (setf new-plist (trim-plist new-plist (um:mklist elisions)))
      (remf props :without))
    (loop for (key val) on props by #'cddr do
            (setf (getf new-plist key) val))
    (apply #'actor-state new-plist)
    ))

(defmethod state-without ((state actor-state) &rest removals)
  (let ((new-plist  (copy-list (actor-state-plist state))))
    (setf new-plist (trim-plist new-plist removals))
    (apply #'actor-state new-plist)
    ))

(defmethod state-val ((state actor-state) (key symbol) &optional default)
  (getf (actor-state-plist state) key default))

(defmacro with-state-vals (bindings state &body body)
  ;; bindings can include a default val following the plist symbol
  (let ((gstate (gensym)))
    `(let ((,gstate ,state))
       (symbol-macrolet 
           ,(mapcar #`(,(car a1) (state-val ,gstate ,@(cdr a1))) bindings)
         ,@body))
    ))

(defmethod um:with ((state actor-state) &rest props)
  ;; WITH state props [:WITHOUT (prop-name | list-of-prop-names)] -> new-state
  (apply #'state-with state props))

