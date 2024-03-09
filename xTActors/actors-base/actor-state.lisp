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
;; in the plist. Take care not to mutate any of these symbol mappings.
;;
;; -------------------------------------------------------------------

(defstruct actor-state
  (plist nil :read-only t))

(defun validate-plist (plist)
  (um:nlet iter ((keys plist))
    (when keys
      (assert (symbolp (car keys)))
      (go-iter (cddr keys)))
    )
  plist)

(defun actor-state (&rest plist)
  (make-actor-state
   :plist (validate-plist plist)))

(defmethod state-with ((state actor-state) &rest props)
  (let ((new-plist  (copy-seq (actor-state-plist state))))
    (um:nlet iter ((keys props)
                   (vals (cdr props)))
      (when keys
        (let ((key  (car keys))
              (val  (car vals)))
          (assert (symbolp key))
          (setf (getf new-plist key) val)
          (go-iter (cddr keys) (cddr vals)))
        ))
    (make-actor-state
     :plist new-plist)
    ))

(defmethod state-without ((state actor-state) &rest removals)
  (let ((new-plist  (copy-seq (actor-state-plist state))))
    (um:nlet iter ((keys removals))
      (when keys
        (let ((key (car keys)))
          (assert (symbolp key))
          (remf new-plist key)
          (go-iter (cdr keys)))
        ))
    (make-actor-state
     :plist new-plist)
    ))

(defmethod state-val ((state actor-state) (key symbol) &optional default)
  (getf (actor-state-plist state) key default))

(defmacro with-state-vals (bindings state &body body)
  (let ((glist (gensym)))
    `(symbol-macrolet 
         ((,glist (actor-state-plist ,state))
          ,@(mapcar #`(,(car a1) (getf ,glist ,(cadr a1) ,@(cddr a1))) bindings))
       ,@body)
    ))

(defmacro with (state &rest props)
  (if (member :without props)
      (let ((rems (getf props :without)))
        (remf props :without)
        `(state-with
          (state-without ,state ,@(um:mklist rems))
          ,@props))
    `(state-with ,state ,@props)))
