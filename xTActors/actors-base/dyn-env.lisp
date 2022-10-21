;; dyn-env.lisp - Dynamic Environments for Actors
;;
;; Provides a rough equivalent for dynamic bindings, unwind actions,
;; error handlers, restarts, and go labels. Not quite the same because
;; Actors are not restricted to a stack execution protocol. But better
;; than nothing.
;;
;; To be useful, a dynamic env arg must become part of the Actor's
;; constructor, and be passed along as augmented by the Actor. Also
;; must perform exit of dynamic levels at suitable times. Normal exit
;; probably most easily done using :GO to known labels that represent
;; starting states.
;;
;; DM/RAL  2022/10/21 05:27:06
;; ----------------------------------

(in-package #:com.ral.actors.base)

;; ----------------------------------------------
;; Handlers convention:
;;   constructed with current dynamic env, then new-level constructed.
;;   So dyn env of handlers will be one higher than new current level.
;;
;;   Handlers receive cust and condition. The dynamic env will have
;;   been unwound before the handler is invoked.
;;
;; DynEnv's provide for unwind actions, error handlers, and dynamic
;; bindings. It is not exactly equiv to Lisp since we don't have a
;; stack execution protocol. When an env is unwound it discards all
;; dyn bindings, handlers, and becomes an empty env, but retains the
;; chain of command, so future uses may find earlier bindings.
;;
;; Props list contains one or more of :BINDINGS, :HANDLERS, :UNWIND,
;; :RESTARTS.

(defvar +not-found+ (cons :not :found))

(defun dyn-env-beh (next kind bindings)
  ;; next points to prior env, bindings is alternating keys and values.
  (alambda
   ((cust :lookup name)
    ;; lookup name in our current env. Return its value and our
    ;; dyn-env. Otherwise, pass request to next level. If no next
    ;; level then not found, and so return nil and nil.
    (flet ((not-found ()
             (if next
                 (repeat-send next)
               (send cust nil nil))))
      (if (eql kind :bindings)
          (let ((val (getf bindings name +not-found+)))
            (if (eql val +not-found+)
                (not-found)
              (send cust val self)))
        (not-found))))

   ((cust :add-binding name val)
    (send self cust :new-level :bindings (list name val)))

   ((cust :add-bindings . new-binds)
    (send self cust :new-level :bindings new-binds))

   ((cust :add-handlers . handlers)
    ;; automatically construct a new dyn-env with the handlers
    ;; installed. Sends new env to cust.  Handlers should include any
    ;; error label handlers and :UNWIND handler.
    (send self cust :new-level :handlers handlers))

   ((cust :add-restarts . restarts)
    (send self cust :new-level :restarts restarts))

   ((cust :add-unwind action)
    (send self cust :new-level :uwind action))

   ((cust :add-label label)
    (send self cust :new-level :label label))
   
   ((cust :new-level kind initial)
    (send cust (create (dyn-env-beh self kind initial))))

   ((cust :exit-level ans)
    ;; perform any unwinds then send ans to cust
    (send self cust :unwind next ans))

   ((cust :go label ans)
    ;; search for label in dyn env, unwinding as we go. If not found,
    ;; pass request along to next level. Once we find label, send ans
    ;; to cust.
    (β _
        (send self β :exit-level nil)
      (if (and (eql kind :label)
               (eql bindings label))
          (send cust ans)
        (repeat-send next))))
   
   ((cust :unwind to-env ans)
    ;; keep unwinding until to-env matches our level, then send cust
    ;; the answer ans.  Unwinding performs any :UNWIND action, and
    ;; discards other handlers and dynamic bindings.
    (if (eql to-env self)
        (send cust ans)
      (progn
        (become (dyn-env-beh next :discarded nil))
        (if (eql kind :unwind)
            (β _
                (send bindings β nil)
              (repeat-send next))
          (repeat-send next)))
      ))

   ((cust :handle kind cx)
    ;; perform unwind to next level then invoke handler if we have
    ;; one. Else pass message along to next level.
    (β _
        (send self β :exit-level nil)
      (let (handler)
        (if (and (eql kind :handlers)
                 (setf handler (getf bindings kind)))
            (send handler cust cx)
          (repeat-send next)))
      ))
   ))

(defun dyn-env (next kind arg)
  (create (dyn-env-beh next kind arg)))


