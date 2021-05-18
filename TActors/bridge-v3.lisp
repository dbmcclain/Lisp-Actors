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
            um:if-let
            um:when-let
            um:recover-ans-or-exn
             
            actors/base:retry-send
            
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
;; Actor PROXY addresses

(defun parse-destination (dest)
  (check-type dest string)
  (let ((pos-at    (position #\@ dest))
        (pos-colon (position #\: dest))
        start
        service
        ip-addr
        ip-port)
    (if pos-at
        (setf service (string-upcase (subseq dest 0 pos-at))
              start   (1+ pos-at))
      (setf start 0))
    (if pos-colon
        (setf ip-addr (string-upcase (subseq dest start (- pos-colon start)))
              ip-port (parse-integer (subseq dest (1+ pos-colon))))
      (setf ip-addr (string-upcase (subseq dest start))))
    (values service ip-addr ip-port)))

(defstruct (proxy
            (:constructor %make-proxy))
  (ip      nil :read-only t)
  (port    nil :read-only t)
  (service nil :read-only t))

(defun make-proxy (&key ip port service addr)
  (when addr
    (multiple-value-bind (aservice aip aport)
        (parse-destination addr)
      (setf ip      aip
            port    aport
            service aservice)))
  (%make-proxy
   :ip      (string-upcase (or ip
                               (machine-instance)))
   :port    (etypecase port
              (null    nil)
              (integer port)
              (string  (parse-integer port)))
   :service (etypecase service
              (null      (error "Service must be provided"))
              (symbol    (string service))
              (string    (string-upcase service))
              (uuid:uuid service))))

(defgeneric normalize-ip (ip)
  (:method ((ip symbol))
   (make-proxy :ip ip :service :none))
  (:method ((ip string))
   (if (find #\@ ip)
       (make-proxy :addr ip)
     (make-proxy :ip ip :service :none)))
  (:method ((ip proxy))
   ip))

(defun same-ip? (ip1 ip2)
  ;; TBD - clean this up re: ports
  (let ((ip1 (normalize-ip ip1))
        (ip2 (normalize-ip ip2)))
    (equalp (proxy-ip ip1) (proxy-ip ip2))))

;; -------------------------------------------
;; Bridge Intf/Continuations mapper

(defun make-pruned-beh (next)
  (alambda
    ((:pruned beh)
     (become beh))

    ( _
       (repeat-send next))
    ))

(defun prune-self (next)
  (become (make-pruned-beh next))
  (send next :prune self))

(defun make-cont-entry (cont intf)
  ;; We hold cont objects in a weak vector so that if the cont object
  ;; disappears, we have a way of knowing to purge the entry for it.
  (make-array 1 :initial-element cont :weak (null intf)))

(defun make-cont-beh (usti contv intf next)
  (alambda
    ((:attach a-usti a-cont an-intf)
     (send next :check-purge)
     (become (make-cont-beh a-usti (make-cont-entry a-cont an-intf)
                            an-intf (make-actor self-beh))))
    
    ((:detach an-intf) when (eq an-intf intf)
     (declare (ignore an-intf))
     (repeat-send next)
     (prune-self next))
    
    ((:prune prev)
     (send prev :pruned self-beh))
    
    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((:deliver-message a-usti if-cant-send . msg) when (uuid:uuid= a-usti usti)
     (declare (ignore a-usti))
     (prune-self next)
     (handler-case
         (send* (aref contv 0) msg)
       (error ()
         (send if-cant-send))
       ))
    ( _
       (unless (aref contv 0)
         (prune-self next))
       (repeat-send next))
    ))
    
(defun make-empty-cont-beh ()
  (alambda
    ((:attach usti cont intf)
     (become (make-cont-beh usti (make-cont-entry cont intf)
                            intf (make-actor self-beh))))

    ((:prune prev)
     (send prev :pruned self-beh))
    
    ((:deliver-message _ if-cant-send . _)
     (send if-cant-send))
    ))

(defvar *cont-map* (make-actor (make-empty-cont-beh)))

;; ------------------------------------------

(defun create-and-add-usti (obj &optional handler)
  ;; construct an ephemeral UUID -> Continuation mapping, including
  ;; the handler if specified. Return the UUID representing the
  ;; continuation for the other side of the connection.
  (let ((usti (uuid:make-v1-uuid)))
    ;; use sync send to prevent race with response
    (send *cont-map* :attach usti obj handler)
    usti))

;; -------------------------------------------
;; Bridge IP/Intf mapper

(defvar *undefined-actor*  
  (actor (&rest msg)
    (error "Send to Undefined Actor: ~S" msg)))
       
(defun undefined ()
  *undefined-actor*)

(defvar *allowable-pending-delay* 5)

(defun make-pending-intf-beh (ip tok pend next)
  ;; On first request, one of these Actors holds the info for a
  ;; pending connection. Additional subscribers are enqueued in the
  ;; pending list.
  (alambda
    ((:attach an-ip intf) when (same-ip? an-ip ip)
     ;; successful attachment, change state to normal table lookup
     (send-to-all pend intf)
     (become (make-intf-beh ip intf next)))

    ((:fail a-tok) when (eq a-tok tok)
     ;; we timed out waiting for attachment
     (prune-self next))

    ((:prune prev)
     (send prev :pruned self-beh))
    
    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((:pre-regiser an-ip intf) when (same-ip? an-ip ip)
     ;; seen by the client side while awaiting attachment. We hold on
     ;; to the pending interface to protect against GC.
     (become (make-prereg-intf-beh ip intf tok pend next)))

    ((:call-with-intf cust an-ip _) when (same-ip? an-ip ip)
     ;; new incoming request for a pending socket connection
     (become (make-pending-intf-beh ip tok (cons cust pend) next)))

    ( _
       (repeat-send next))))

(defun make-prereg-intf-beh (ip intf tok pend next)
  ;; Client-side requests are pre-registered when the physical
  ;; interface is successfuly constructed. But until the interface
  ;; completes its handshake with the server, it remains in pending
  ;; status. This Actor simply anchors the intf against GC.
  (alambda
    ((:attach an-ip an-intf) when (same-ip? an-ip ip)
     ;; successful attachment, change state to normal table lookup.
     (send-to-all pend an-intf)
     (become (make-intf-beh ip an-intf next)))

    ((:detach an-intf) when (eq an-intf intf)
     ;; seen on unsuccessful connection negotiation
     (repeat-send next)
     (prune-self next))

    ((:fail a-tok) when (eq a-tok tok)
     ;; we timed out waiting for attachment
     (prune-self next))

    ((:prune prev)
     (send prev :pruned self-beh))

    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((:pre-register an-ip an-intf) when (same-ip? an-ip ip)
     ;; this should not orinarily be seen from this state
     (become (make-prereg-intf-beh ip an-intf tok pend next)))

    ((:call-with-intf cust an-ip _) when (same-ip? an-ip ip)
     ;; new incoming request for a pending socket connection
     (become (make-prereg-intf-beh ip intf tok (cons cust pend) next)))

    ( _
       (repeat-send next))))
    
(defun make-intf-beh (ip intf next)
  ;; A fully live interface connection.
  (alambda
    ((:attach an-ip an-intf) when (same-ip? an-ip ip)
     ;; Called by the network interface upon successful connection.
     ;; There may be multiple ip-addr (strings, keywords, etc) assoc
     ;; to each intf. But each ip-addr points to only one intf.
     (become (make-intf-beh ip an-intf next)))
    
    ((:detach an-intf) when (eq an-intf intf)
     ;; Called by the network intf when it shuts down
     ;; NOTE: multiple ip-addr may correspond to one intf
     (repeat-send next)
     (prune-self next))

    ((:prune prev)
     (send prev :pruned self-beh))
    
    ((:reset)
     (repeat-send next)
     (prune-self next))
    
    ((:call-with-intf cust an-ip _) when (same-ip? an-ip ip)
     ;; Called by SEND on our side to find the network intf to use for
     ;; message forwarding.  TODO - clean up re ports
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
             (actors ((sched (make-scheduled-message-beh self
                                                         *allowable-pending-delay*
                                                         :fail sched)))
               (send sched :go)
               sched)))
    (alambda
      ((:attach ip intf)
       ;; server side sees this message from this state, but client side
       ;; ordinarily pre-registers before the attach.
       (become (make-intf-beh ip intf (make-actor self-beh))))
      
      ((:pre-regiser ip intf)
       ;; we should never ordinarily see this message from empty state.
       ;; But go to pre-register state and give us a timeout in case we
       ;; fail to attach.
       (become (make-prereg-intf-beh ip intf
                                     (schedule-timeout)
                                     nil
                                     (make-actor self-beh))))
      
      ((:call-with-intf cust ip port)
       ;; this is the place where a new connection is attempted. We now
       ;; go to pending state, with a timeout.
       (become (make-pending-intf-beh ip
                                      (schedule-timeout)
                                      (list cust)
                                      (make-actor self-beh)))
       (with-worker
         (open-connection ip port)))
      
      ((:prune prev)
       (send prev :pruned self-beh))
      )))

(defvar *intf-map* (make-actor (make-empty-intf-beh)))

(defun call-with-valid-ip (service ip port fn)
  ;; Find or create a connection Actor for the IP Addr. If successful,
  ;; then execute the fn with the service and connection handler Actor
  ;; as arguments.
  (when (and service
             ip)
    (send *intf-map* :call-with-intf
          (make-actor (um:curry fn service))
          ip port)
    ))

;; -------------------------------------------
;; Register / Connect to socket handler

(defun bridge-pre-register (ip-addr intf)
  (send *intf-map* :pre-register ip-addr intf))

(defun bridge-register (ip-addr handler)
  (send *intf-map* :attach ip-addr handler))

(defun bridge-unregister (handler)
  (send *intf-map* :detach handler)
  (send *cont-map* :detach handler))

(defun bridge-reset ()
  ;; called when *all* socket I/O is shutdown
  (send *intf-map* :reset)
  (send *cont-map* :reset))

;; --------------------------------------------

(defgeneric call-with-valid-dest (dest fn)
  (:method ((dest string) fn)
   (multiple-value-bind (service dest-ip dest-port)
       (parse-destination dest)
     (call-with-valid-dest (make-proxy
                            :service service
                            :ip      dest-ip
                            :port    dest-port)
                         fn)))
  (:method ((dest proxy) fn)
   (with-slots (ip port) dest
     (call-with-valid-ip dest ip port fn))))

(defmacro with-valid-dest ((service handler) dest &body body)
  `(call-with-valid-dest ,dest (lambda (,service ,handler)
                                 ,@body)))

;; -----------------------------------------------------------------------

(defun bridge-forward-message (dest &rest msg)
  ;; called by SEND as a last resort
  (with-valid-dest (service handler) dest
    (apply #'socket-send handler :forwarding-send service msg)))

(defun my-node? (proxy)
  (string-equal (machine-instance) (proxy-ip proxy)))

(defmethod bridge-deliver-message ((dest proxy) if-cant-send &rest msg)
  ;; a message arrived from across the network. try to dispatch.
  (if (my-node? dest)
      (apply #'bridge-deliver-message (proxy-service dest) if-cant-send msg)
    (apply #'bridge-forward-message dest msg)))

(defmethod bridge-deliver-message ((dest uuid:uuid) if-cant-send &rest msg)
  (send* *cont-map* :deliver-message dest if-cant-send msg))

(defmethod bridge-deliver-message (dest if-cant-send &rest msg)
  (handler-case
      (apply #'send dest msg)
    (error ()
      (send if-cant-send))))

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

(defmethod send ((usti uuid:uuid) &rest message)
  (bridge-deliver-message usti
                          (actor ()
                            (error "No service: ~S" usti))
                          message))

(defmethod send ((proxy proxy) &rest message)
  (if (my-node? proxy)
      (send* (proxy-service proxy) message)
    (apply #'bridge-forward-message proxy message)))

(defmethod retry-send ((proxy proxy) &rest message)
  (send* proxy message))

(defmethod retry-send ((usti uuid:uuid) &rest message)
  (send* usti message))

;; -------------------------------------------
;; USTI - transient identifiers for possible send targets
;;
;; Functions, Actors, continuations, etc. cannot be transported across
;; the network. So we need to translate them to a USTI for transport.
;;
;; USTI live in the cache until used. They have single-use semantics.
;; But if never used they just linger... so... we hold onto these USTI
;; entries with a weak-vector, so that periodic scanning of the cache
;; can identify objects that have been discarded.

(defun usti (obj)
  (make-proxy
   :service (create-and-add-usti obj)))

(defmethod find-actor ((cust actor) (proxy proxy))
  (if (my-node? proxy)
      (find-actor cust (proxy-service proxy))
    (call-next-method)))

#|
(defun test-usti ()
  (=wait ((ans))
      (send "eval@rincon.local"
            `(send ,(usti =wait-cont) 15))
    ans))
(test-usti)
 |#

;; ------------------------------------------------------

(defun make-remote-actor (remote-addr &key register)
  (let ((actor (make-actor (make-remote-actor-beh remote-addr))))
    (when register
      (register-actor register actor))
    actor))

(defun make-remote-actor-beh (remote-addr)
  (let ((remote-addr (if (stringp remote-addr)
                         (make-proxy :addr remote-addr)
                       remote-addr)))
    (lambda (&rest msg)
      (apply #'bridge-forward-message remote-addr msg))
    ))
  
#|
;; e.g.,
(make-remote-actor "eval@rincon.local"
                   :register :rincon-eval)
(ask :rincon-eval '(get-actor-names))
(ask :rincon-eval '(get-actors)) ;; should present an error

(become-remote :eval "eval@rincon.local")
(=wait ((ans) :timeout 5)
    (send :eval `(send ,(usti =wait-cont) :hello))
  (pr ans))
 |#
