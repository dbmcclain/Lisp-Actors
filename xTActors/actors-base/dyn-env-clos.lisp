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

;; --------------------------------------------
;; CLOS Implementation of Dynamic Environments - faster, more direct,
;; communication with dynamic-envs, but now we need MPX Locks.
;;
;; An alternative implementation would use Actors to represent dyn-env
;; nodes, and provide for slower, indirect communication with them,
;; but greater concurrency, and no need for locks.

(defclass base-dyn-env ()
  ())

(defmethod throw-β ((env base-dyn-env) tag &rest msg)
  (declare (ignore msg))
  (β _
      (do-unwind-β self-env β env)
    (error "Throw target not found: ~S" tag)))

(defmethod lookup-β ((env base-dyn-env) tag &optional default)
  (declare (ignore tag))
  default)

(defmethod handle-β ((env base-dyn-env) cust tag cx)
  (declare (ignore tag cx))
  (send cust :ok))

(defmethod find-env-β ((env base-dyn-env) to-env)
  (eql env to-env))

(defmethod do-unwind-β ((env base-dyn-env) cust to-env)
  (setf *current-env* to-env)
  (send cust :ok))

(defmethod unwind-β ((env base-dyn-env) cust to-env)
  ;; If we make it here, then caller was already in the base
  ;; environment and message arrrived from ether the base environment,
  ;; or from somewhere up in the tree.
  ;;
  ;; If from up in the tree, there is no unwinding that we could do,
  ;; so just give him his asked-for environment.
  ;;
  (setf *current-env* to-env)
  (send cust :ok))

(defmethod show-β ((env base-dyn-env) cust &optional lst)
  (send cust (nreverse (cons env lst))))

(setf *current-env* (make-instance 'base-dyn-env))

;; -------------------------------------------------------------

(defclass dyn-env (base-dyn-env)
  ((lock   :reader env-lock  :initform (mpc:make-lock))
   (next   :reader next-env  :initarg :next)))

(defmethod throw-β ((env dyn-env) tag &rest msg)
  (apply #'throw-β (next-env env) tag msg))

(defmethod lookup-β ((env dyn-env) tag &optional default)
  (lookup-β (next-env env) tag default))

(defmethod handle-β ((env dyn-env) cust tag cx)
  (handle-β (next-env) cust tag cx))

(defmethod find-env-β ((env dyn-env) to-env)
  (or (eql env to-env)
      (find-env-β (next-env env) to-env)))

(defmethod unwind-handler ((env dyn-env))
  nil)

(defmethod do-unwind-β ((env dyn-env) cust to-env)
  (if (eql env to-env)
      (send cust :ok)
    (let ((next     (next-env env))
          (handler  (mpc:with-lock ((env-lock env))
                      (prog1
                          (unwind-handler env)
                        (change-class env 'dyn-env)))
                    ))
      (setf *current-env* next)
      (if handler
          (β _
              (send handler β)
            (do-unwind-β next cust to-env))
        (do-unwind-β next cust to-env))
      )))

(defmethod unwind-β ((env dyn-env) cust to-env)
  (if (find-env-β env to-env)
      (do-unwind-β env cust to-env)
    (progn
      (setf *current-env* to-env)
      (send cust :ok))
    ))

(defmethod show-β ((env dyn-env) cust &optional lst)
  (show-β (next-env env) cust (cons env lst)))
                               
;; ----------------

(defclass catch-dyn-env (dyn-env)
  ((catch-tag     :reader   catch-tag     :initarg :tag)
   (catch-handler :accessor catch-handler :initarg :handler)))

(defmethod throw-β ((env catch-dyn-env) tag &rest msg)
  (if (eql tag (catch-tag env))
      (let ((handler (mpc:with-lock ((env-lock env))
                       (shiftf (catch-handler env) nil))))
        (cond (handler
               (β _
                   (do-unwind-β self-env β (next-env env))
                 (send* handler msg)))
              (t
               (call-next-method))
              ))
    (call-next-method)))

;; ----------------

(defclass handler-dyn-env (dyn-env)
  ((handler-plist :accessor handler-plist :initarg :handlers)))

(defmethod handle-β ((env handler-dyn-env) cust kind cx)
  (let ((handler (getf (handler-plist env) kind)))
    (cond (handler
           (mpc:with-lock ((env-lock env))
             (setf handler (getf (shiftf (handler-plist env) nil) kind)))
           (cond (handler
                  (β _
                      (do-unwind-β self-env β (next-env env))
                    (send handler cust)
                    ))
                 (t
                  (call-next-method))
                 ))
          (t
           (call-next-method))
          )))

;; ----------------

(defclass unwind-dyn-env (dyn-env)
  ((handler :initarg :handler)))

(defmethod unwind-handler ((env unwind-dyn-env))
  (shiftf (slot-value env 'handler) nil)) ;; look-once, callled only from within lock


;; ----------------

(defclass bindings-dyn-env (dyn-env)
  ((plist  :reader bindings-plist :initarg :bindings)))

(defmethod lookup-β ((env bindings-dyn-env) tag &optional default)
  (let ((ans (getf (bindings-plist env) tag +not-found+)))
    (if (eql ans +not-found+)
        (call-next-method)
      ans)))

;; ---------------------------

(defun make-dyn-env (kind arg)
  (ecase kind
    (:CATCH    (make-instance 'catch-dyn-env
                              :next     self-env
                              :tag      (car arg)
                              :handler  (cdr arg)))
    (:UNWIND   (make-instance 'unwind-dyn-env
                              :next     self-env
                              :handler  arg))
    (:HANDLERS (make-instance 'handler-dyn-env
                              :next     self-env
                              :handlers arg))
    (:bindings (make-instance 'bindings-dyn-env
                              :next     self-env
                              :bindings arg))
    ))

(defmacro %with-env ((kind arg) &body body)
  ;; (:CATCH  (label . cont)) ;; cont will be send a message on (GO label)
  ;; (:UNWIND actor)
  ;; (:HANDLERS plist) - pllist is keyword args list of keys and handler Actors
  ;; (:BINDINGS bindings) -- bindings is bindings list of keys and values
  `(let ((*current-env* (make-dyn-env ,kind ,arg)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "%with-env" 1)

;; ----------------------------------------

(defmacro catch-β ((label args &body catcher-body) &body body)
  `(%with-env (:catch (cons ,label (create
                                    (lambda* ,args
                                      ,@catcher-body))))
     ,@body))

(defun send-throw (tag &rest msg)
  (apply #'throw-β self-env tag msg))

;; ----------------------------------------

(defmacro unwind-protect-β (form unwind-form)
  (lw:with-unique-names (cust)
    `(%with-env (:unwind (create
                          (lambda (,cust)
                            (send ,cust)
                            ,unwind-form)))
       ,form)))

(defmacro unwind-to-β (env &body body)
  `(progn
     (β _
         (unwind-β self-env β ,env)
       ,@body)))

;; ----------------------------------------

(defun bindings-to-plist (bindings)
  `(list ,@(mapcan (lambda (binding)
                     `(',(car binding) ,(cadr binding)))
                   bindings)))
  
(defmacro with-handlers (handler-bindings &rest body)
  `(%with-env (:handlers ,(bindings-to-plist handler-bindings))
     ,@body))

(defun send-to-handler (cust handler-kind cx)
  (handle-β self-env cust handler-kind cx))

;; ----------------------------------------

(defmacro with-env (bindings &body body)
  (if (consp bindings)
      `(%with-env (:bindings ,(bindings-to-plist bindings))
         ,@body)
    `(let ((*current-env* ,bindings)) ;; should be an env Actor here
       ,@body)))

(defmacro with-binding-β ((var name &optional default) &body body)
  `(let ((,var (lookup-β self-env ,name ,default)))
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
