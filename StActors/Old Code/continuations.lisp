
    (:continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} (fn &rest vals)
     ;; Used for callbacks into the Actor
     (apply fn vals))
    

;; ------------------------------------------
;; Create a callback on the function argument

(defclass callback-function ()
  ()
  (:metaclass #+:LISPWORKS clos:funcallable-standard-class
              #+:ALLEGRO   mop:funcallable-standard-class
              #+:CLOZURE   ccl:funcallable-standard-class
              #+:SBCL      sb-mop:funcallable-standard-class
              ))

(defmethod initialize-instance :after ((obj callback-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   #+:CLOZURE   ccl:set-funcallable-instance-function
   #+:SBCL      sb-mop:set-funcallable-instance-function
   obj behavior))

(defmethod =cont ((contfn callback-function))
  contfn)

(defmethod =cont ((contfn symbol))
  (=cont (symbol-function contfn)))

(defmethod =cont ((contfn function))
  (if-let (self (current-actor))
      ;;
      ;; If the callback originated from inside an Actor, we ensure
      ;; that it will later execute inside that Actor only when that
      ;; Actor is alive.
      ;;
      ;; Code inside an Actor should only be executing on one thread
      ;; at a time, in order to preserve SMP single-thread semantics.
      ;;
      (make-instance 'callback-function
                     :behavior (lambda (&rest args)
                                 (if (eq self (current-actor))
                                     (apply contfn args)
                                   (apply 'send self :continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} contfn args))))
                     
    ;; else - not originating from inside of an Actor, just return the
    ;; function unchanged
    contfn))

;; ------------------------------------------

(defmacro without-actor-status (&body body)
  ;;
  ;; Used to avoid deadlocking an Actor. This should be quite rare.
  ;;
  ;; In general, if you will have the Actor hang waiting on a resource
  ;; (e.g. a local mailbox), and the only way to release that resource
  ;; is to perform a callback function, that callback function would
  ;; ordinarily be redirected as a continuation message to the Actor.
  ;; The Actor would have to respond to the message, and you will have
  ;; induced a classic deadlock.
  ;;
  ;; You need to surround that action with WITHOUT-ACTOR-STATUS so
  ;; that embedded =CONT calls will become identity operations instead
  ;; of setups to send continuation messages back to the Actor.
  ;;
  ;; The Actor will be hung waiting on the resource, so there is no
  ;; danger of multiple thread access to Actor internals, until the
  ;; resource is released, if code in callback functions access Actor
  ;; internals from a foreign thread prior to that release. When in
  ;; doubt, use a lock.
  ;;
  ;; NOTE:  (without-actor-status (=cont (lambda () ...))) = (lambda () ...)
  ;;
  ;;  In other words, (without-actor-status (=cont fn)) is an identity
  ;;  operation on functions.
  ;;
  `(let ((*current-actor* nil))
     ,@body))

