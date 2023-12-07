;; actor-state.lisp -- Alternative to BOA lists of state vars
;; DM/RAL  2023/12/07 10:08:39 UTC

(in-package :com.ral.actors.base)

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
     :plist new-plist
     )))

(defmethod state-val ((state actor-state) (key symbol) &optional default)
  (getf (actor-state-plist state) key default))

(defmacro with-state-vals (bindings state &body body)
  (let ((glist (gensym)))
    `(let* ((,glist (actor-state-plist ,state))
            ,@(mapcar #`(,(car a1) (getf ,glist ,(cadr a1) ,@(cddr a1))) bindings))
       ,@body)
    ))

