;; dyn-env.lisp - Dynamic Environments for Actors
;;
;; Provides a rough equivalent for dynamic bindings, unwind actions,
;; error handlers, restarts, and go labels. Not quite the same because
;; Actors are not restricted to a stack execution protocol. But better
;; than nothing.
;;
;; To be useful, a dynamic env arg must become part of every message
;; event. An Actor that isn't running has no need for dyn env. Only
;; Actors running could have or need them. And every message
;; represents a potentially different dynamic envionment. So dyn envs
;; are associated with messages. And the running Actor inherits the
;; dyn-env arriving with the message being processed.
;;
;; All logical processing chains of Actors start out from the base
;; environment. Actors can augment the environment along the way. And
;; any new message SENDS will carry the dyn env in effect at time of
;; message creation, i.e., in the SEND itself.
;;
;; A first cut at this had dyn envs that were Actors. All comms with
;; the env were via message passing. But then we realize that the
;; chain of dyn envs pointing back to the base dyn env, can only
;; belong to a single logical Actor thread.
;;
;; So there is no possibility of different logical threads smashing
;; the dyn env of another logical thread. Any one logical activity
;; chain is started with a message SEND, and continued so long as that
;; target Actor also performs just one SEND. As soon as an Actor performs
;; multiple SENDs, new logical threads are started in parallel with
;; the original one.
;;
;; The trick might be one of ascertaining which SEND should be
;; considered the continuation of a logical thread, and which are side
;; ventures with independent character.
;;
;; Every running Actor instance inherits the dyn-env associated with
;; the incoming message. New dyn-envs extend the chain of dyn-envs
;; from that point.
;;
;; So all the dyn-envs form a tree with branches for each new logical
;; thread. Many different logical threads will share some trunk in
;; common, and then have just their own unique branch coming off the
;; shared trunk.
;;
;; So long as there are no CATCH handlers, or Error Handlers, located
;; below the level of the dyn env where these side ventures were
;; launched (using SEND), then we should be just fine.
;;
;; Then, again, CATCH and Error Handlers, and UNWIND handlers, are
;; inherently once-only. So perhaps by making sure to BECOME-SINK
;; after first use, they can be left in the dyn-env chain without
;; causing any harm.
;;
;; The code here ensures that once-only character by leaving dyn-env
;; members in the chain, but neutralizing them before firing the
;; associated handlers. On next look, they will not be seen as handler
;; nodes.
;;
;; But there, we could have a problem - shared trunk members being
;; neutralized in a parallel processing environment... a solid case
;; for MPX Locks... (or Actor BECOME)
;;
;; DM/RAL 2022/10/21 05:27:06
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

;; --------------------------------------------------------------------

(def-actor base-dyn-env
  (create
   (alambda
    ((:throw label _)
     (β _
         (send self-env β :do-unwind self)
       (error "Throw target not found: ~S" label)))

    ((cust :do-unwind to-env)
     (setf *current-env* to-env)
     (send cust :ok))

    ((cust :lookup _ . default)
     (send cust (car default)))
    
    ((cust :handle . _)
     (send cust :ok))

    ((cust :find-env to-env)
     (send cust (eql self to-env)))
    
    ((cust :unwind to-env)
     ;; If we end up here, we are either already at base level, or
     ;; else the sender's environment was established from a message
     ;; sent from outside the Actor system - as from an interrupt
     ;; routine.
     ;;
     ;; Either way, there is nothing we can do, so just tell him he is
     ;; where he wants to be already.
     (setf *current-env* to-env)
     (send cust :ok))
    
    ((cust :show . lst)
     (send cust (reverse (cons self lst))))
    )))

(setf *current-env* base-dyn-env)

;; ----------------------------------------------

(defun default-env-proc (next unw-handler msg)
  (match msg
    ((:throw label _)
     (repeat-send next))

    ((cust :do-unwind to-env)
     (if (eql to-env self)
         (send cust :ok)
       (progn
         (setf *current-env* next)
         (become (dyn-env-beh next))
         (if unw-handler
             (let ((my-msg self-msg))
               (β _
                   (send unw-handler β)
                 (send* next my-msg)))
           (repeat-send next))
         )))

    ((cust :lookup _ . default)
     (repeat-send next))
    
    ((cust :handle . _)
     (repeat-send next))

    ((cust :find-env to-env)
     (if (eql to-env self)
         (send cust t)
       (repeat-send next)))
    
    ((cust :unwind to-env)
     ;; If we end up here, we are either already at base level, or
     ;; else the sender's environment was established from a message
     ;; sent from outside the Actor system - as from an interrupt
     ;; routine.
     ;;
     ;; Either way, there is nothing we can do, so just tell him he is
     ;; where he wants to be already.
     (if-β (service self-env :find-env to-env)
         (send self-env cust :do-unwind to-env)
       (progn
         (setf *current-env* to-env)
         (send cust :ok))
       ))
    
    ((cust :show . lst)
     (send next cust (cons self (car lst))))
    ))
    
(defun dyn-env-beh (next)
  (lambda (&rest msg)
    (default-env-proc next nil msg)))

;; ----------------------------------------------

(defun catch-dyn-env-beh (next tag handler)
  (alambda
   ((:throw a-tag . msg)
    (if (eql a-tag tag)
        (β _
            (send self-env β :do-unwind next)
          (send* handler msg))
      (repeat-send next)))

   (msg
    (default-env-proc next nil msg))
   ))

;; ----------------------------------------------

(defun unwind-dyn-env-beh (next handler)
  (lambda (&rest msg)
    (default-env-proc next handler msg)))

;; ----------------------------------------------

(defun handlers-dyn-env-beh (next plist)
  (alambda
   ((cust :handle tag cx)
    (let ((handler (getf plist tag)))
      (cond (handler
             (become (dyn-env-beh next))
             (β _
                 (send self-env β :do-unwind next)
               (send handler cust cx)))
            (t
             (repeat-send next))
            )))

   (msg
    (default-env-proc next nil msg))
   ))

;; ----------------------------------------------

(defun bindings-dyn-env-beh (next plist)
  (alambda
   ((cust :lookup key . default)
    (let ((val (getf plist key +not-found+)))
      (if (eql val +not-found+)
          (repeat-send next)
        (send cust val))
      ))

   (msg
    (default-env-proc next nil msg))
   ))

;; ----------------------------------------------

(defun make-dyn-env (kind arg)
  (create
   (ecase kind
     (:CATCH    (catch-dyn-env-beh self-env (car arg) (cdr arg)))
     (:UNWIND   (unwind-dyn-env-beh self-env arg))
     (:HANDLERS (handlers-dyn-env-beh self-env arg))
     (:BINDINGS (bindings-dyn-env-beh self-env arg))
     )))

(defmacro %with-env ((kind arg) &body body)
  ;; (:CATCH  (label . cont)) ;; cont will be send a message on (GO label)
  ;; (:UNWIND actor)
  ;; (:HANDLERS plist) - pllist is keyword args list of keys and handler Actors
  ;; (:BINDINGS bindings) -- bindings is bindings list of keys and values
  `(let ((*current-env* (make-dyn-env ,kind ,arg)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "%with-env" 1)

;; ------------------------------------

(defmacro catch-β ((label args &body catcher-body) &body body)
  `(%with-env (:catch (cons ,label (create
                                    (lambda* ,args
                                      ,@catcher-body))))
     ,@body))

(defun send-throw (label &rest msg)
  (send* self-env :throw label msg))

;; ------------------------------------

(defmacro unwind-protect-β (form unwind-form)
  (lw:with-unique-names (cust)
    `(%with-env (:unwind (create
                          (lambda (,cust)
                            (send ,cust)
                            ,unwind-form)))
       ,form)))

(defmacro unwind-to-β (env &body body)
  `(β _
       (send self-env β :unwind ,env)
     ,@body))

;; ------------------------------------

(defun bindings-to-plist (bindings)
  `(list ,@(mapcan (lambda (binding)
                     `(',(car binding) ,(cadr binding)))
                   bindings)))
  
(defmacro with-handlers (handler-bindings &rest body)
  `(%with-env (:handlers ,(bindings-to-plist handler-bindings))
     ,@body))

(defun send-to-handler (cust handler-kind cx)
  (send self-env cust :handle handler-kind cx))

;; ------------------------------------

(defmacro with-env (bindings &body body)
  (if (consp bindings)
      `(%with-env (:bindings ,(bindings-to-plist bindings))
         ,@body)
    `(let ((*current-env* ,bindings)) ;; should be an env Actor here
       ,@body)))

(defmacro with-binding-β ((var name &optional default) &body body)
  `(β (,var)
       (send self-env β :lookup ,name ,default)
     ,@body))

;; ----------------------------------------------
;; From CPS - form an Actor that captures/restores the current Lisp
;; dynamic environment.  All Lisp =HANDLERS and =LET bindings.

(defun =act (fn)
  (create
   (let ((dyn-env (capture-dynamic-environment)))
     (lambda (&rest args)
       (apply #'call-with-dynamic-environment dyn-env fn args))
     )))

(defmacro =β (args send-form &rest body)
  `(let ((=β  (=act (lambda* ,args ,@body))))
     ,send-form))

;; ----------------------------------------------

(defun unwinding-fwd (env)
  ;; Useful to reset env when you no longer need handlers in place.
  ;; Send your message to cust via unwinding-fwd. This can be
  ;; established in the env before even knowing what cust will be
  ;; needed. Compare with FWD.
  ;;
  ;; This serves as a FILTER block.
  (create
   (lambda (cust &rest msg)
     (unwind-to-β env
       (send* cust msg)))
   ))

;; ----------------------------------------------

#+:LISPWORKS
(progn
  (editor:setup-indent "catch-β"          1)
  (editor:setup-indent "unwind-protect-β" 2 2 4)
  (editor:setup-indent "with-handlers"    1)
  (editor:setup-indent "with-env"         1)
  (editor:setup-indent "with-binding-β"   1)
  (editor:setup-indent "unwind-to-β"      1)
  (editor:indent-like  "=β" 'destructuring-bind))

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
      
      (unwind-protect-β
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
