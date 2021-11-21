;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;
;; Uses ECDH secret key exchange for encryption. All ECC points are
;; relayed as integers representing compressed points. All messages
;; and replies are encrypted and authenticatd with Schnorr signatures.
;; Encryption keying is roving for each message/reply interchange.
;;
;; After initial connection is established between client and server,
;; each of them knows the other's public key and mutal encryption key.
;; No need to send along public key info for signatures. It is assumed
;; that the public key that requested the connection is the one
;; performing signature generation.
;;
;; It is presumed that the server has a well known public key, and its
;; gateway Actor is known to the outside world. That is the only
;; information the outside world needs to know. Clients can perfom
;; only a limited list of services, as provided by a menu of
;; offerings. The verbs for the services can be obtained by sending
;; verb :available-services.
;;
;; The client and server connection portals are also controlled by an
;; admin-tag to provide for immediate shutdown, and for
;; augmenting/trimming the list of available services at the server.
;;
;; This code is the working guts for a secure comm system. No
;; provision here for comm channels, e.g., sockets. It is assumed that
;; there are proxy Actors resident in the machine to which the Actors
;; send messages. Outboard communications is a separate layer. With
;; the exception of symbolic verbs representing server services, all
;; sends are between Actors.
;;
;; DM/RAL 11/21
;; --------------------------------------------------------------------------

(in-package :ac-secure-comm)

(defconstant *server-id*    "7a1efb26-bc60-123a-a2d6-24f67702cdaa")
(defconstant *server-skey*  #x4504E460D7822B3B0E6E3774F07F85698E0EBEFFDAA35180D19D758C2DEF09)
(defconstant *server-pkey*  #x7EBC0A8D8FFC77F24E7F271F12FC827415F0B66CC6A4C1144070A32133455F1)

;; ----------------------------------------------------------------
;; Self-organizing list of services for Server and connection Actors

(defun null-service-list-beh ()
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :available-services lst)
    (send cust (reverse lst)))

   ((cust :add-service name handler)
    (let ((next (make-actor self-beh)))
      (become (service-list-beh name handler next))
      (send cust :ok)))

   ((cust :list lst)
    (send cust lst))
   ))

(defun service-list-beh (name handler next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :send verb . msg) when (eql verb name)
    (send* handler cust msg))

   ((cust :add-service aname new-handler) when (eql aname name)
    (become (service-list-beh name new-handler next))
    (send cust :ok))
   
   ((cust :remove-service aname) when (eql aname name)
    (prune-self next)
    (send cust :ok))

   ((cust :available-services lst)
    (send next cust :available-services (cons name lst)))

   ((cust :list lst)
    (send next cust :list (cons name lst)))

   ( _
     (repeat-send next))
   ))

(defvar *global-services*  nil)

(defun global-services ()
  (or *global-services*
      (setf *global-services* (make-actor (null-service-list-beh)))
      ))

;; ------------------------------------------------------------
;; When the socket connection (server or client side) receives an
;; incoming message, the cust field of the message will contain a
;; symbolic reference to a customer on the other side of the socket
;; connection.
;;
;; We need to manufacture a local proxy for that customer and pass it
;; along as the cust field of the message being sent to a local
;; service. That service will use the proxy for any replies.
;;
;; We want to avoid inventing subtypes of Actors for this. Instead, we
;; manufacture stand-in Actors.

(defun client-side-server-proxy (server-id socket)
  ;; Used to setup a server proxy, on the client, for sending queries
  ;; to server.
  (actor (&rest msg)
    ;; (send println (format nil "c/query: ~S" msg))
    (send* socket server-id :send msg)))

(defun server-side-client-proxy (client-id socket)
  ;; Used to setup a target proxy, on the server, for sending replies
  ;; back to client.
  (when client-id
    (actor (&rest msg)
      ;; (send println (format nil "s/reply: ~S" msg))
      (send* socket :reply client-id msg))))

;; Similarly, on the sending side, we can't just send along a cust
;; field of a message because it is an Actor, and contains a
;; non-marshalable functional closure.
;;
;; We need to manufacture a symbolic name for sending across, and give
;; us a way to translate back to an Actor for any messages sent back
;; to us on its behalf.
;;
;; Unlike the previous case, this situation more resembles a service
;; since it may become the direct target of a send. But unlike server
;; services, each of these local services survives only for one
;; message send. And in case that never happens, they are given a
;; time-to-live, after which they become purged from the list of local
;; ephemeral services.

(defvar *dbg* nil)

(defmacro dbg (&body body)
  `(when *dbg*
     ,@body))

(defvar *default-ephemeral-ttl*  10)
(defvar *default-services-ttl*   20)

(defun empty-local-service-beh ()
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :add-service actor)
    ;; used for connection handlers
    (let ((next  (make-actor self-beh))
          (tag   (tag (local-services)))
          (id    (uuid:make-v1-uuid)))
      (become (local-service-beh actor id tag next))
      (send-after *default-services-ttl* tag :lease-expired)
      (dbg (send println (format nil "Service added: ~A" id)))
      (send cust id)
      ))
   
   ((cust :add-ephemeral-client actor ttl)
    ;; used for transient customer proxies
    (let ((next (make-actor self-beh))
          (id   (uuid:make-v1-uuid)))
      (become (local-ephemeral-client-beh actor id next))
      (send cust id)
      (when ttl
        (send-after ttl (local-services) sink :remove-service id))
      (dbg (send println (format nil "Ephemeral service added: ~A" id)))
      ))

   ((cust :list lst)
    (send cust lst))
   ))

(defun local-ephemeral-client-beh (actor id next)
  ;; used by clients to hold ephemeral reply proxies
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((:reply client-id . msg) when (uuid:uuid= client-id id)
    ;; Server replies are directed here via the client proxy id, to
    ;; find the actual client channel. Once a reply is received, this
    ;; proxy is destroyed. It is also removed after a timeout and no
    ;; reply forthcoming.
    (dbg (send println (format nil "Ephemeral service used: ~A" id)))
    (send* actor msg)
    (prune-self next))
    
   ((cust :remove-service an-id) when (uuid:uuid= an-id id)
    (send cust :ok)
    (dbg (send println (format nil "Ephemeral service removed: ~A" id)))
    (prune-self next))

   ((cust :list lst)
    (send next cust :list (cons (list :ephemeral id actor) lst)))
   
   (_
    (repeat-send next))
   ))

(defun local-service-beh (actor id tag next)
  ;; used by servers to hold proxies for local service channels
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((serv-id :send client-id . msg) when (uuid:uuid= serv-id id)
    ;; We do not automatically remove this entry once used. Instead,
    ;; we renew the lease. Client messages are directed here via proxy
    ;; serv-id, to find the actual target channel.
    (let ((tag  (tag (local-services))))
      (dbg (send println (format nil "Service used: ~A" id)))
      (become (local-service-beh actor id tag next))
      (send-after *default-services-ttl* tag :lease-expired)
      (send* actor client-id msg)))

   ((a-tag :lease-expired) when (eq a-tag tag)
    (dbg (send println (format nil "Service lease expired: ~A" id)))
    (prune-self next))
   
   ((cust :remove-service an-id) when (uuid:uuid= an-id id)
    (send cust :ok)
    (prune-self next))

   ((cust :list lst)
    (send next cust :list (cons (list :service id actor) lst)))
   
   (_
    (repeat-send next))
   ))

(defvar *local-services* nil)

(defun local-services ()
  (or *local-services*
      (setf *local-services* (make-actor (empty-local-service-beh)))
      ))

(defun create-ephemeral-client-proxy (cust svc &key (ttl *default-ephemeral-ttl*))
  ;; used by client side
  (send (local-services) cust :add-ephemeral-client svc ttl))

(defun create-service-proxy (cust svc)
  ;; used by server side
  (send (local-services) cust :add-service svc))

;; ---------------------------------------------------
;; Composite Actor pipes

(defun secure-sender (ekey skey)
  (pipe (marshal-encoder)
        (encryptor ekey)
        (signing   skey)))

(defun secure-reader (ekey pkey)
  (pipe (signature-validation pkey)
        (decryptor ekey)
        (marshal-decoder)))

