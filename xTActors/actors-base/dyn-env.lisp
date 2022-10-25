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

(def-actor base-dyn-env
  (create
   (alambda
    ((:throw label _)
     (error "Throw target not found: ~S" label))
    
    ((cust :lookup _ . default)
     (send cust (car default)))

    ((cust :handle . _)
     (send cust :ok))
    
    ((cust :find-handler _)
     (send cust nil self))
    
    ((cust :unwind _ ans)
     (send cust ans))
    
    ((cust :show . lst)
     (send cust (reverse lst)))
    )))

(setf *current-env* base-dyn-env)

;; ----------------------------------------------

(defun dyn-env-beh (next kind bindings)
  ;; next points to prior env, bindings is alternating keys and values.
  (alambda
   ((:throw label ans)
    ;; Unconditional unwind back to catch frame.
    ;; Search for label in dyn env, unwinding as we go. If not found,
    ;; pass request along to next level. Once we find label, send ans
    ;; to continuation Actor. Cust will never be sent anything. End of the line...
    ;; It is an error to throw to an undefined catch label.
    (let ((msg  self-msg))
      (β _
          (send self β :unwind next nil)
        (if (and (eql kind :catch)
                 (eql (car bindings) label))
            (send (cdr bindings) ans)
          (send* next msg)))
      ))
   
   ((cust :handle akind cx)
    ;; Search for handler. If found, unwind to its dyn env, then
    ;; invoke handler. Otherwise, just send a response to the waiting
    ;; customer.  Handlers should respond to their customer.
    (β  (handler level)
        (send self β :find-handler akind)
      (if handler
          (β _
              (send self β :unwind level nil)
            (send handler cust cx)) ;; handler should respond to cust
        (send cust :ok))
      ))
          
   ((cust :find-handler akind)
    (let (handler)
      (if (and (eql kind :HANDLER)
               (setf handler (getf bindings akind)))
          (send cust handler next)
        (repeat-send next))))
   
   ((cust :lookup name . default)
    ;; Lookup name in our current env. If found, return its value.
    ;; Else send request to next deeper level.  If never found, return
    ;; default answer.
    (if (eql kind :bindings)
        (let ((val (getf bindings name +not-found+)))
          (if (eql val +not-found+)
              (repeat-send next)
            (send cust val)))
      (repeat-send next)))

   ((cust :unwind to-env ans)
    ;; Keep unwinding until to-env matches our level, then send cust
    ;; the answer ans.  Unwinding performs any :UNWIND action, and
    ;; discards other handlers and dynamic bindings.
    ;;
    ;; Unwind handlers must reply to cust.
    ;;
    (if (eql to-env self)
        (send cust ans)
      (progn
        (become (dyn-env-beh next :discarded nil))
        (let ((*current-env* next)
              (msg           self-msg))
          (if (eql kind :unwind)
              (β _
                  (send bindings β) ;; bindings, here, is an unwind Actor
                (send* next msg))
            (send* next msg)))
        )))

   ((cust :show . accum)
    (let ((lst (cons (list kind bindings) (car accum))))
      (send next cust :show lst)
      ))
   ))

(defun dyn-env (next kind arg)
  ;; kind should be one of :LABEL, :UNWIND, :HANDLERS, or :BINDNGS
  (create (dyn-env-beh next kind arg)))

(defmacro %with-env ((kind arg) &body body)
  ;; (:CATCH  (label . cont)) ;; cont will be send a message on (GO label)
  ;; (:UNWIND actor)
  ;; (:HANDLERS plist) - pllist is keyword args list of keys and handler Actors
  ;; (:BINDINGS bindings) -- bindings is bindings list of keys and values
  `(let ((*current-env* (dyn-env self-env ,kind ,arg)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "%with-env" 1)

(defmacro catch-β ((label (ans) &body catcher-body) &body body)
  `(%with-env (:catch (cons ,label (create
                                    (lambda (,ans)
                                      ,@catcher-body))))
     ,@body))

(defun send-throw (label val)
  (send self-env :throw label val))

(defmacro unwind-β (form unwind-form)
  (lw:with-unique-names (cust)
    `(%with-env (:unwind (create
                          (lambda (,cust)
                            (send ,cust)
                            ,unwind-form)))
       ,form)))

(defun bindings-to-plist (bindings)
  `(list ,@(mapcan (lambda (binding)
                     `(',(car binding) ,(cadr binding)))
                   bindings)))
  
(defmacro with-handlers (handler-bindings &rest body)
  `(%with-env (:handlers ,(bindings-to-plist handler-bindings))
     ,@body))

(defmacro send-to-handler (cust handler-kind cx)
  `(send self-env ,cust :handle ,handler-kind ,cx))

(defmacro with-env (bindings &body body)
  `(%with-env (:bindings ,(bindings-to-plist bindings))
     ,@body))

(defmacro with-binding-β ((var name &optional default) &body body)
  `(β (,var)
       (if self-env
           (send self-env β :lookup ,name ,default)
         (send β ,default))
     ,@body))

#+:LISPWORKS
(progn
  (editor:setup-indent "catch-β"        1)
  (editor:setup-indent "unwind-β"       2 2 4)
  (editor:setup-indent "with-handlers"  1)
  (editor:setup-indent "with-env"       1)
  (editor:setup-indent "with-binding-β" 1))

;;-------------------------------------------------------
;; To be effective, we need to pass along the dynamic env with every
;; send target for messages. The dispatcher needs to strip out
;; that env for use in a handler-bind or handler-case surrounding the
;; body of message handling.
;;
;; Then we need to supply the env on any sends, but also allow for an
;; extension of env to be passed in lieu of the incoming env.
;;
;; Ideally, this all occurs under the table, unseen by client user
;; code.  Perhaps we can use Lisp dynamic-binding mechanism and
;; special binding *CURRENT-ENV* to represent incoming env and to be
;; modified by some macro for use in outbound messaging with agumented
;; env...

#|
(defun tst ()
  (β _
      (send β)
    (catch-β (:bottom (ans)
              (send fmt-println "Caught :BOTTOM: ~S" ans))
      
      (unwind-β
          (progn
            (send println "We should be seeing this...")
            (with-env ((a 1)
                       (b 2)
                       (c 3))
              (send (α ()
                      (send self-env writeln :show)
                      (send-throw :bottom 15)))
              (send println "...and we should also be seeing this.")))
          (send println :unwinding)))
    ))
(tst)
 |#
