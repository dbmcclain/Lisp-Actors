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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            um:if-let
            um:when-let
            um:recover-ans-or-exn

            actors/network:open-connection
            actors/network:socket-send

            actors/base:*in-ask*
            actors/base:no-immediate-answer
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

#+diddly
(defun dbg (from)
  (log-info :SYSTEM-LOG "~A: ~S" from (whole-message)))

#-diddly
(defmacro dbg (from)
  (declare (ignore from)))

;; -------------------------------------------
;; Bridge Intf/Continuations mapper

(defparameter *nocont-beh*
   (um:dlambda
     (:attach (usti cont intf)
      ;; hand off our beh to new next in line,
      ;; assume response for the new entry
      (let ((next (make-actor (current-behavior))))
        (become (make-cont-beh usti cont intf next))
        ))

     (:prune (prev)
      (respond-to-prune prev))

     (:find-and-remove (usti reply-to)
      ;; Called by FIND-ACTOR.
      ;; End of line response - nil
      (declare (ignore usti))
      (send reply-to nil))
     ))

(defvar *cont-map*
  (make-actor *nocont-beh*))

(defun make-cont-beh (usti cont intf next)
  (um:dlambda
    (:attach (a-usti a-cont an-intf)
     ;; hand off our beh to new next in line,
     ;; assume response for the new entry
     (let ((new-next (make-actor (current-behavior))))
       (become (make-cont-beh a-usti a-cont an-intf new-next))
       ))

    (:detach (an-intf)
     ;; called when an interface is retiring
     ;; remove all continuations registered to that intf
     (repeat-send next)
     (when (eq an-intf intf)
       (detach-myself next)))

    (:prune (prev)
     (respond-to-prune prev))

    (:handle-reply (a-usti &rest reply)
     ;; if we match the USTI, perform the continuation with the reply
     ;; and remove ourselves. Mappings are one-time use.
     (cond ((uuid:uuid= a-usti usti)
            (detach-myself next)
            ;; Spawn...just in case cont is a brute closure and not an
            ;; Actor continuation. Happens when =BIND is called from a
            ;; non-Actor.  Possibly indefinite work load.
            (spawn-worker #'apply cont reply))
           (t
            (repeat-send next))))

    (:find-and-remove (a-usti reply-to)
     ;; Called by FIND-ACTOR
     ;; If we match the USTI, send it along to reply-to and remove
     ;; ourselves. Mappings are one-time use only.
     (cond ((uuid:uuid= a-usti usti)
            (detach-myself next)
            (send reply-to cont))
           (t
            (repeat-send next))
           ))
    ))

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
  (send prev :pruned (current-behavior)))

;; ------------------------------------------

(defun create-and-add-usti (obj &optional handler)
  ;; construct an ephemeral UUID -> Continuation mapping, including
  ;; the handler if specified. Return the UUID representing the
  ;; continuation for the other side of the connection.
  (let ((usti (uuid:make-v1-uuid)))
    ;; use sync send to prevent race with response
    (send *cont-map* :attach usti obj handler)
    usti))

(defun bridge-handle-reply (usti &rest reply)
  ;; called by socket handler when an ASK reply arrives
  (apply #'send *cont-map* :handle-reply usti reply))

;; -------------------------------------------
;; Bridge IP/Intf mapper

(defparameter *nodev-beh*
   (um:dlambda
     (:attach (ip-addr intf)
      ;; called by the network interface upon successful connection
      (let ((next (make-actor (current-behavior))))
        (become (make-dest-beh ip-addr intf next))
        ))
     (:prune (prev)
      (respond-to-prune prev))
     
     (:get-intf (dest-ip dest-port reply-to)
      ;; no interface found - try to connect and create a new interface
      (with-worker ()
        ;; spawned since there may be significant delay
        (send reply-to (open-connection dest-ip dest-port))))
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
     (call-with-valid-ip service dest-ip dest-port fn)))
  (:method ((dest proxy) fn)
   (with-slots (service ip port) dest
     (call-with-valid-ip service ip port fn))))

(defmacro with-valid-dest ((service handler) dest &body body)
  `(call-with-valid-dest ,dest (lambda (,service ,handler)
                                 ,@body)))

;; -----------------------------------------------------------------------

(defun forward-query (handler service cont &rest msg)
  (let ((usti (create-and-add-usti cont handler)))
    (apply #'socket-send handler 'actors/internal-message/bridge:forwarding-ask service usti msg)))

(defun bridge-forward-message (dest &rest msg)
  ;; called by SEND as a last resort
  (with-valid-dest (service handler) dest
    (case (car msg)
      ((actors/internal-message:ask)
       ;; form was (ASK reply-to &rest actual-message)
       (apply #'forward-query handler service (cdr msg))
       (when *in-ask*
         (signal 'no-immediate-answer)))
      
      (otherwise
       (apply #'socket-send handler 'actors/internal-message/bridge:forwarding-send service msg))
      )))

(=defun bridge-ask-query (dest &rest msg)
  ;; called by ASK as a last resort
  (with-valid-dest (service handler) dest
    (apply #'forward-query handler service =bind-cont msg)
    ))

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
  (apply #'bridge-forward-message proxy message))

;; ------------------------------------------
;; Blocking ASK across network connections

(defun network-ask (dest &rest message)
  ;; Blocking ASK across a network connection
  (=wait ((ans)
          :timeout *timeout*
          :errorp  t)
      (=apply #'bridge-ask-query dest message)
    (recover-ans-or-exn ans)))

(defmethod ask ((str string) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor str))
      (apply #'ask actor message))
     
     ((find #\@ str)
      (apply #'network-ask str message))
     
     (t
      (call-next-method))
     )))

(defmethod ask ((usti uuid:uuid) &rest message)
  (if-let (actor (find-actor usti))
      (apply #'ask actor message)
    (call-next-method)))

(defmethod ask ((proxy proxy) &rest message)
  (apply #'network-ask proxy message))

;; -----------------------------------------------
;; Non-blocking ASK across network connections

(=defun network-ask-nb (dest &rest message)
  ;; Non-blocking ASK across a network connection
  (=bind (ans)
      (=apply #'bridge-ask-query dest message)
    (=values (recover-ans-or-exn ans))))

(=defmethod =ask ((str string) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor str))
      (=apply #'=ask actor message))
     
     ((find #\@ str)
      (=apply #'network-ask-nb str message))
     
     (t
      (call-next-method))
     )))

(=defmethod =ask ((usti uuid:uuid) &rest message)
  (if-let (actor (find-actor usti))
      (=apply #'=ask actor message)
    (call-next-method)))

(=defmethod =ask ((proxy proxy) &rest message)
  (=apply #'network-ask-nb proxy message))

;; --------------------------------------------------------------------------------
;; USTI - Universal Send-Target Identifier
;;
;; While ASK uses a known syntax, where the reply-to field is in a
;; known position of the message, and the Bridge takes care of
;; translating that into a USTI for network transmission, we need
;; something else for general messages that may contain a callback to
;; some closure or mailbox.
;;
;; In that case, it is up to the caller to form an explicit USTI in
;; the message, which will be looked up on return.
;;
;; Here we use UUID's for USTI's. UUID's have a very short network
;; encoding, compared to some struct that would enclose a UUID to
;; serve as a USTI type envelope. Since USTI's are only used as
;; targets of SEND across a network connection, it seems reasonable to
;; just use UUID's here.

(defgeneric usti (obj)
  (:method ((obj proxy))
   obj)
  (:method ((obj string))
   (make-proxy
    :service obj))
  (:method ((obj symbol))
   (make-proxy
    :service obj))
  (:method ((obj uuid:uuid))
   (make-proxy
    :service obj))
  (:method ((obj actor))
   (make-proxy
    :service (ensured-identifier obj)))
  (:method (obj)
   (make-proxy
    :service (create-and-add-usti obj)
    )))

(defmethod find-actor ((usti uuid:uuid) &key directory)
  (declare (ignore directory))
  (or (when (uuid:one-of-mine? usti)
        (=wait ((cont))
            (send *cont-map* :find-and-remove usti =wait-cont)
          cont))
      (call-next-method)))

#|
(defun test-usti ()
  (=wait ((ans))
      (send "eval@rincon.local"
            `(send ,(usti =wait-cont) 15))
    (print ans)))
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
    (let ((prev-beh (current-behavior))
          (remote-addr (if (stringp remote-addr)
                           (make-proxy :addr remote-addr)
                         remote-addr)))
      (become (um:dlambda
                (actors/internal-message:become-local ()
                   (become prev-beh))
                (t (&rest msg)
                   (declare (ignore msg))
                   (apply #'actors/bridge:bridge-forward-message remote-addr (whole-message))
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
 |#
