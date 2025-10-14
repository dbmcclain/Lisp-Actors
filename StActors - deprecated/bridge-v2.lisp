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

(in-package :stactors/bridge)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            um:if-let
            um:when-let
            um:recover-ans-or-exn

            stactors/network:open-connection
            stactors/network:socket-send

            stactors/base:no-immediate-answer
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

(defun make-new-cont-entry (usti cont intf)
  ;; We hold cont objects in a weak vector so that if the cont object
  ;; disappears, we have a way of knowing to purge the entry for it.
  (let ((next  (make-actor self-beh))
        (contv (make-array 1 :initial-element cont :weak (null intf))))
    (become (make-cont-beh usti contv intf next))
    ))

(defparameter *nocont-beh*
   (um:dlambda
     (:attach (usti cont intf)
      ;; hand off our beh to new next in line,
      ;; assume response for the new entry
      (make-new-cont-entry usti cont intf))

     (:prune (prev)
      (respond-to-prune prev))

     (:deliver-message (a-usti if-cant-send-fn &rest msg)
      (declare (ignore a-usti msg))
      (funcall if-cant-send-fn))
     ))

(defparameter *cont-gateway*
  (let ((next (make-actor *nocont-beh*)))
    (um:dlambda
      (t (&rest msg)
         (declare (ignore msg))
         (repeat-send next)))))

(defvar *cont-map*
  (make-actor *cont-gateway*))

(defun make-cont-beh (usti contv intf next)
  ;; We hold cont objects in a weak vector so that if the cont object
  ;; disappears, we have a way of knowing to purge the entry for it.
  (flet ((check-purge ()
           (unless (aref contv 0)
             (detach-myself next))))
    (um:dlambda
      (:attach (a-usti a-cont an-intf)
       ;; hand off our beh to new next in line,
       ;; assume response for the new entry
       (make-new-cont-entry a-usti a-cont an-intf))
      
      (:detach (an-intf)
       ;; called when an interface is retiring
       ;; remove all continuations registered to that intf
       (repeat-send next)
       (if (eq an-intf intf)
           (detach-myself next)
         (check-purge)))
      
      (:prune (prev)
       (respond-to-prune prev))

      (:deliver-message (a-usti if-cant-send-fn &rest msg)
       (cond ((uuid:uuid= a-usti usti)
              (detach-myself next)
              (handler-case
                  (apply #'send (aref contv 0) msg)
                (error ()
                  (funcall if-cant-send-fn))))
             (t
              (repeat-send next)
              (check-purge))
             ))
      )))

(defun make-detached-beh (next)
  (um:dlambda
    (:pruned (beh)
     (become beh))

    (t (&rest _)
       (declare (ignore _))
       (repeat-send next))
    ))

(defun detach-myself (next)
  (become (make-detached-beh next))
  (send next :prune self))

(defun respond-to-prune (prev)
  (send prev :pruned self-beh))

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

(defparameter *nodev-beh*
   (um:dlambda
     (:attach (ip-addr intf)
      ;; called by the network interface upon successful connection
      (let ((next (make-actor self-beh)))
        (become (make-dest-beh ip-addr intf next))
        ))
     (:prune (prev)
      (respond-to-prune prev))
     
     (:get-intf (dest-ip dest-port reply-to)
      ;; no interface found - try to connect and create a new interface
      (open-connection reply-to dest-ip dest-port))
     ))

(defvar *intf-map*
  (make-actor *nodev-beh*))

(defun make-dest-beh (ip-addr intf next)
  (um:dlambda
    (:attach (an-ip-addr an-intf)
      ;; Called by the network interface upon successful connection.
      ;; There may be multiple ip-addr (strings, keywords, etc) assoc
      ;; to each intf. But each ip-addr points to only one intf.
     (cond ((same-ip? ip-addr an-ip-addr)
            (setf intf an-intf))
           (t
            (repeat-send next))
           ))
    (:detach (an-intf)
     ;; Called by the network intf when it shuts down
     ;; NOTE: multiple ip-addr may correspond to one intf
     (repeat-send next)
     (when (eq an-intf intf)
       (detach-myself next)))

    (:prune (prev)
     (respond-to-prune prev))

    (:get-intf (an-ip-addr a-port reply-to)
     ;; Called by SEND on our side to find the network intf to use for
     ;; message forwarding.  TODO - clean up re ports
     (declare (ignore a-port))
     (if (same-ip? ip-addr an-ip-addr)
         (send reply-to intf)
       (repeat-send next)))
    ))

(defun call-with-valid-ip (service ip port fn)
  ;; Find or create a connection Actor for the IP Addr. If successful,
  ;; then execute the fn with the service and connection handler Actor
  ;; as arguments.
  (when (and service
             ip)
    (=bind (intf)
        (send *intf-map* :get-intf ip port =bind-cont)
      (when intf
        (funcall fn service intf)))
    ))

;; -------------------------------------------
;; Register / Connect to socket handler

(defun bridge-register (ip-addr handler)
  (send *intf-map* :attach ip-addr handler))

(defun bridge-unregister (handler)
  (send *intf-map* :detach handler)
  (send *cont-map* :detach handler))

(defun bridge-reset ()
  ;; called when *all* socket I/O is shutdown
  (setf *intf-map* (make-actor *nodev-beh*)
        *cont-map* (make-actor *nocont-beh*)))

;; --------------------------------------------

(defgeneric call-with-valid-dest (dest fn)
  (:method ((dest string) fn)
   (multiple-value-bind (service dest-ip dest-port)
       (parse-destination dest)
     (call-with-valid-ip (make-proxy
                          :service service
                          :ip      dest-ip
                          :port    dest-port)
                         dest-ip dest-port fn)))
  (:method ((dest proxy) fn)
   (with-slots (ip port) dest
     (call-with-valid-ip dest ip port fn))))

(defmacro with-valid-dest ((service handler) dest &body body)
  `(call-with-valid-dest ,dest (lambda (,service ,handler)
                                 ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-valid-dest" 1)

;; -----------------------------------------------------------------------

(defun bridge-forward-message (dest &rest msg)
  ;; called by SEND as a last resort
  (with-valid-dest (service handler) dest
    (apply #'socket-send handler 'actors/internal-message/bridge:forwarding-send service msg)))

(defmethod bridge-deliver-message ((dest proxy) if-cant-send-fn &rest msg)
  (if (string-equal (machine-instance) (proxy-ip dest))
      (apply #'bridge-deliver-message (proxy-service dest) if-cant-send-fn msg)
    (apply #'bridge-forward-message dest msg)))

(defmethod bridge-deliver-message ((dest uuid:uuid) if-cant-send-fn &rest msg)
  (apply #'send *cont-map* :deliver-message dest if-cant-send-fn msg))

(defmethod bridge-deliver-message (dest if-cant-send-fn &rest msg)
  (handler-case
      (apply #'send dest msg)
    (error ()
      (funcall if-cant-send-fn))))
  
;; -----------------------------------------------------------------------
;; Default services: ECHO and EVAL

(register-actor :echo
                (make-actor
                 (lambda (&rest msg)
                   msg)))

;; -----------------------------------------------------------------

(defun cmpfn (&rest args)
  (compile nil `(lambda ()
                  ,@args)))

(register-actor :eval
                (make-actor
                 (lambda (&rest msg)
                   (funcall (apply #'cmpfn msg)))))

;; ----------------------------------------------------------------------
;; SEND across network connections - never blocks.

(defmethod send ((str string) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor str))
      (apply #'send actor message))
     
     ((find #\@ str)
      (apply #'bridge-forward-message str message))

     (t
      (call-next-method))
     )))

(defmethod send ((usti uuid:uuid) &rest message)
  (if-let (actor (find-actor usti))
      (apply #'send actor message)
    (call-next-method)))

(defmethod send ((proxy proxy) &rest message)
  (if (string-equal (machine-instance) (proxy-ip proxy))
      (apply #'send (proxy-service proxy) message)
    (apply #'bridge-forward-message proxy message)))

;; -------------------------------------------
;; USTI - transient identifiers for possible send targets
;;
;; Functions, Actors, continuations, etc. cannot be transported across
;; the network. So we need to translate them to a USTI for transport.
;;
;; For ASK, the recipient continuation is automatically translated.
;; Any other SEND targets in a transmitted message must be manually
;; translated before being sent in a message.
;;
;; USTI live in the cache until used. They have single-use semantics.
;; But if never used they just linger... so... we hold onto these USTI
;; entries with a weak-vector, so that periodic scanning of the cache
;; can identify objects that have been discarded.

(defun usti (obj)
  (make-proxy
   :service (create-and-add-usti obj)))

(defmethod find-actor ((proxy proxy) &key directory)
  (if (string-equal (machine-instance) (proxy-ip proxy))
      (find-actor (proxy-service proxy) :directory directory)
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

(defun make-remote-actor (remote-addr &key register directory)
  (let ((actor (make-actor)))
    (become-remote actor remote-addr)
    (when register
      (register-actor register actor :directory directory))
    actor))

(defun become-remote (actor remote-addr)
  (inject-into-actor actor
    (let ((prev-beh    self-beh)
          (remote-addr (if (stringp remote-addr)
                           (make-proxy :addr remote-addr)
                         remote-addr)))
      (become (um:dlambda
                (actors/internal-message:become-local ()
                   (become prev-beh))
                (t (&rest msg)
                   (declare (ignore msg))
                   (apply #'stactors/bridge:bridge-forward-message remote-addr (whole-message))
                   ))
              ))))

(defun become-local (actor)
  (send actor 'actors/internal-message:become-local))
  
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
