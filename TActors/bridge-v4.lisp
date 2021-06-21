;; actor-bridge.lisp -- The bridge needed to allow Actors to remotely interact
;;
;; When an Actor target is a string specified as
;; "service@ip-addr:port" this package re-routes the SEND/ASK to a
;; socket interface, connecting first to a remote server if needed.
;;
;; Keeps a local record of pending callbacks, and serializes them by
;; using a unique string for remote reference.
;;
;; DM/RAL 10/20
;; -----------------------------------------------------------------------------

(in-package :actors/bridge)

(um:eval-always
  (import '(
            actors/network:open-connection
            actors/network:socket-send
            )))

;; ---------------------------------------------------------------------------------
;; The "Bridge" - a resident agent for transparent SEND/ASK across a
;; network connection to foreign Actors. A foreign Actor is identified
;; by IP Addr, Port, and Service name - aka PROXY,
;;
;; When a network connection becomes established, an entry is made in
;; the DESTS map, relating the stated IP Addr with the Actor
;; responsible for comms across that connection.
;;
;; When a caller wants to send a message across the network
;; connection, and include a reply-to callback, it must represent that
;; callback closure using a UUID. The foreign system can forward reply
;; SENDS to that closure using the UUID. Back at the original sender
;; side, that UUID can be translated back into the actual closure code
;; to perform.
;;
;; Users can produce UUID -> Continuation mappings for other purposes
;; than a reply-to address. In that case, the Handler will be NIL.
;;
;; These UUID -> Continuation mappings are ephemeral, being removed
;; from the map upon invocation.
;;
;; --------------------------------------------------------------------------------
(defun same-ip? (ip1 ip2)
  (string-equal (string ip1) (string ip2)))

;; -------------------------------------------
;; Bridge IP/Intf mapper

(defvar *allowable-pending-delay* 5)

(defun make-pending-intf-beh (ip tag pend next)
  ;; On first request, one of these Actors holds the info for a
  ;; pending connection. Additional subscribers are enqueued in the
  ;; pending list.
  (alambda
    ((:attach an-ip intf) when (same-ip? an-ip ip)
     ;; successful attachment, change state to normal table lookup
     ;;; (send println "Attach from pending-intf-beh")
     (send-to-all pend intf)
     (become (make-intf-beh ip intf next)))

    ((a-tag :fail) when (eq a-tag tag)
     ;; we timed out waiting for attachment
     ;;; (send println "Fail from pending-intf-beh")
     (prune-self next))

    ((prev :prune)
     (send prev :pruned self-beh))
    
    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((cust :pre-register an-ip intf) when (same-ip? an-ip ip)
     ;; seen by the client side while awaiting attachment. We hold on
     ;; to the pending interface to protect against GC.
     ;;; (send println "Pre-Register from pending-intf-beh")
     (become (make-prereg-intf-beh ip intf tag pend next))
     (send cust))

    ((cust :get-intf an-ip _) when (same-ip? an-ip ip)
     ;; new incoming request for a pending socket connection
     ;;; (send println "get-intf from pending-intf-beh")
     (become (make-pending-intf-beh ip tag (cons cust pend) next)))

    ( _
       (repeat-send next))))

(defun make-prereg-intf-beh (ip intf tag pend next)
  ;; Client-side requests are pre-registered when the physical
  ;; interface is successfuly constructed. But until the interface
  ;; completes its handshake with the server, it remains in pending
  ;; status. This Actor simply anchors the intf against GC.
  (alambda
   #|
    ((:attach an-ip an-intf) when (same-ip? an-ip ip)
     ;; successful attachment, change state to normal table lookup.
     (send-to-all pend an-intf)
     (become (make-intf-beh ip an-intf next)))
    |#
    ((:attach _ an-intf) when (eql an-intf intf)
     ;;; (send println "Attach from prereg-intf-beh")
     ;; successful attachment, change state to normal table lookup.
     (send-to-all pend intf)
     (become (make-intf-beh ip intf next)))

    ((:detach an-intf) when (eq an-intf intf)
     ;; seen on unsuccessful connection negotiation
     ;;; (send println "detach from prereg-intf-beh")
     (repeat-send next)
     (prune-self next))

    ((a-tag :fail) when (eq a-tag tag)
     ;; we timed out waiting for attachment
     ;;; (send println "fail from prereg-intf-beh")
     (prune-self next))

    ((prev :prune)
     (send prev :pruned self-beh))

    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((cust :pre-register an-ip an-intf) when (same-ip? an-ip ip)
     ;; this should not orinarily be seen from this state
     ;;; (send println "Pre-Register from prereg-intf-beh")
     (become (make-prereg-intf-beh ip an-intf tag pend next))
     (send cust))

    ((cust :get-intf an-ip _) when (same-ip? an-ip ip)
     ;; new incoming request for a pending socket connection
     ;;; (send println "get-intf from preref-intf-beh")
     (become (make-prereg-intf-beh ip intf tag (cons cust pend) next)))

    ( _
       (repeat-send next))))
    
(defun make-intf-beh (ip intf next)
  ;; A fully live interface connection.
  (alambda
    ((:attach an-ip an-intf) when (same-ip? an-ip ip)
     ;; Called by the network interface upon successful connection.
     ;; There may be multiple ip-addr (strings, keywords, etc) assoc
     ;; to each intf. But each ip-addr points to only one intf.
     ;;; (send println "Attach from intf-beh")
     (become (make-intf-beh ip an-intf next)))
    
    ((:detach an-intf) when (eq an-intf intf)
     ;; Called by the network intf when it shuts down
     ;; NOTE: multiple ip-addr may correspond to one intf
     ;;; (send println "detach from intf-beh")
     (repeat-send next)
     (prune-self next))

    ((prev :prune)
     (send prev :pruned self-beh))
    
    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((cust :get-intf an-ip _) when (same-ip? an-ip ip)
     ;; Called by SEND on our side to find the network intf to use for
     ;; message forwarding.  TODO - clean up re ports
     ;;; (send println "get-intf from intf-beh")
     (send cust intf))

    ( _
       (repeat-send next))
    ))

(defun make-empty-intf-beh ()
  ;; The state of the interface table when no scokets are open.  When
  ;; a connection attempt is begun, we go to pending state. We have up
  ;; to *allowable-pending-delay* to see an :ATTACH or :DETACH message
  ;; to cause a state change. Otherwise the pending state becomes
  ;; removed from the mapping list.
  (labels ((schedule-timeout ()
             (let ((tag (tag self)))
               (send-after *allowable-pending-delay* tag :fail)
               tag)))
    (alambda
      ((:attach ip intf)
       ;; server side sees this message from this state, but client side
       ;; ordinarily pre-registers before the attach.
       ;;; (send println "Attach from empty-intf-beh")
       (become (make-intf-beh ip intf
                              (make-actor self-beh))))
      
      ((cust :pre-register ip intf)
       ;; we should never ordinarily see this message from empty state.
       ;; But go to pre-register state and give us a timeout in case we
       ;; fail to attach.
       ;;; (send println "Pre-Register from emtpy-intf-beh")
       (become (make-prereg-intf-beh ip intf
                                     (schedule-timeout)
                                     nil
                                     (make-actor self-beh)))
       (send cust))
      
      ((cust :get-intf ip port)
       ;; this is the place where a new connection is attempted. We now
       ;; go to pending state, with a timeout.
       ;;; (send println "get-intf from empty-intf-beh")
       (become (make-pending-intf-beh ip
                                      (schedule-timeout)
                                      (list cust)
                                      (make-actor self-beh)))
       (with-worker
         (open-connection ip port)))
      
      ((prev :prune)
       (send prev :pruned (make-empty-intf-beh)))
      )))

(defvar *intf-map* (make-actor (make-empty-intf-beh)))

;; -------------------------------------------
;; Register / Connect to socket handler

(defun bridge-pre-register (cust ip-addr intf)
  (send *intf-map* cust :pre-register ip-addr intf))

(defun bridge-register (ip-addr handler)
  (send *intf-map* :attach ip-addr handler))

(defun bridge-unregister (handler)
  (send *intf-map* :detach handler))

(defun bridge-reset ()
  ;; called when *all* socket I/O is shutdown
  (send *intf-map* :reset))

;; -----------------------------------------------------------------------
;; Default services: ECHO and EVAL

(defun cmpfn (&rest args)
  (compile nil `(lambda ()
                  ,@args)))

(defun make-basic-services ()
  (register-actor :echo
                  (actor (cust &rest msg)
                    (send cust msg)))
  (register-actor :eval
                  (actor (cust &rest msg)
                    (send cust (funcall (apply #'cmpfn msg))))))

;; ----------------------------------------------------------------------
;; SEND across network connections - never blocks.

(defvar *machines*
  ;; (MACHINE-INSTANCE  IP-ADDR  PORT)
  '(("Arroyo.local" "Arroyo.local" :default)
    ("Rincon.local" "Rincon.local" :default)
    ("Rambo"        "10.0.0.142"   :default)))

(defstruct hosted-actor
  mach
  act)

(defmethod hosted-actor (mach (act hosted-actor))
  act)

(defvar *hosted-lock*            (mp:make-lock))
(defvar *hosted-index*           0)
(defvar *hosted-actors-by-actor* (make-hash-table :weak-kind :key))
(defvar *hosted-actors-by-name*  (make-hash-table :weak-kind :value))

(defmethod hosted-actor (mach act)
  (cond ((string-equal (string mach) (machine-instance))
         (let ((sym (gethash act *hosted-actors-by-actor*)))
           (unless sym
             (mp:with-lock (*hosted-lock*)
               (unless (setf sym (gethash act *hosted-actors-by-actor*))
                 (setf sym (incf *hosted-index*)
                       (gethash act *hosted-actors-by-actor*) sym
                       (gethash sym *hosted-actors-by-name*)  act))
               ))
           (make-hosted-actor
            :mach (string mach)
            :act  sym)))

        (t
         (make-hosted-actor
          :mach (string mach)
          :act  act))
        ))

(defmethod send ((ha hosted-actor) &rest msg)
  (cond ((string-equal (hosted-actor-mach ha) (machine-instance))
         (let ((act (gethash (hosted-actor-act ha) *hosted-actors-by-name*)))
           (if act
               (send* act msg)
             (send* (hosted-actor-act ha) msg))))
        (t
         (destructuring-bind (_ ip port) (find (hosted-actor-mach ha) *machines*
                                               :test #'string-equal
                                               :key  #'first)
           (declare (ignore _ port))
           (beta (intf)
               (send *intf-map* beta :get-intf ip nil)
             (apply #'socket-send intf :forwarding-send ha
                    (mapcar (lambda (elt)
                              (if (actor-p elt)
                                  (hosted-actor (machine-instance) elt)
                                elt))
                            msg)))
             ))
        ))

#|
(loop repeat 5 do
      (send (hosted-actor :rincon.local :eval) println '(machine-instance)))
(loop repeat 5 do
      (send (hosted-actor :arroyo.local :eval) println '(machine-instance)))
(loop repeat 5 do
      (send (hosted-actor :rambo :eval) println '(machine-instance)))
|#


