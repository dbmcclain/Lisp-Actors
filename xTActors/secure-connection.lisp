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

(defpackage :ac-secure-comm
  (:use #:common-lisp #:actors #:core-crypto #:edec)
  (:export
   ))

(in-package :ac-secure-comm)

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

(defun create-cust-proxy (cust socket)
  ;; cust here is a symbolic reference
  (actor (&rest msg)
    (send* socket cust :send msg)))

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

(defun empty-local-service-beh (top)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :add-service actor)
    ;; used for connection handlers
    (let ((next  (make-actor self-beh))
          (tag   (tag top))
          (id    (uuid:make-v1-uuid)))
      (become (local-service-beh actor id tag top next))
      (send-after *default-services-ttl* tag :lease-expired)
      (dbg (send println (format nil "Service added: ~A" id)))
      (send cust id)
      ))
   
   ((cust :add-ephemeral-service actor ttl)
    ;; used for transient customer proxies
    (let ((next (make-actor self-beh))
          (id   (uuid:make-v1-uuid)))
      (become (local-ephemeral-service-beh actor id next))
      (send cust id)
      (when ttl
        (send-after ttl top sink :remove-service id))
      (dbg (send println (format nil "Ephemeral service added: ~A" id)))
      ))

   ((cust :list lst)
    (send cust lst))
   ))

(defun local-ephemeral-service-beh (actor id next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :send an-id . msg) when (uuid:uuid= an-id id)
    (dbg (send println (format nil "Ephemeral service used: ~A" id)))
    (send* actor msg)
    (send cust :ok)
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

(defun local-service-beh (actor id tag top next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :send an-id . msg) when (uuid:uuid= an-id id)
    ;; we do not automatically remove this entry once used.
    (let ((tag  (tag top)))
      (dbg (send println (format nil "Service used: ~A" id)))
      (become (local-service-beh actor id tag top next))
      (send-after *default-services-ttl* tag :lease-expired)
      (send* actor msg)
      (send cust :ok)))

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

(defvar *local-services*
  (actors ((svcs (empty-local-service-beh svcs)))
    svcs))

(defun create-ephemeral-service-proxy (cust svc &key (ttl *default-ephemeral-ttl*))
  (send *local-services* cust :add-ephemeral-service svc ttl))

(defun create-service-proxy (cust svc)
  (send *local-services* cust :add-service svc))

;; ---------------------------------------------------
;; Composite Actor pipes

(defun secure-sender (ekey skey)
  (pipe (marshal-encoder)
        (encryptor ekey)
        (signing   skey)
        (marshal-encoder)))

(defun secure-reader (ekey pkey)
  (pipe (marshal-decoder)
        (signature-validation pkey)
        (decryptor ekey)
        (marshal-decoder)))

;; --------------------------------------------------------------------
;; Client side

(defun client-crypto-gate-beh (&rest args &key client-skey admin-tag cnxs)
  ;; This is the main local client service used to initiate
  ;; connections with foreign servers. We develop a DHE shared secret
  ;; encryption key for use across a private connection portal with
  ;; the server.
  ;;
  ;; One of these is constructed for each different key set on the
  ;; client. That way we avoid sending messages that contain a secret
  ;; key.
  (alambda
   ((cust :connect socket server-pkey)
    (let* ((arand       (int (ctr-drbg 256)))
           (apt         (ed-mul *ed-gen* arand))
           (client-pkey (ed-mul *ed-gen* client-skey))
           (me          self)
           (responder
            (actor (server-cnx bpt)
              (let* ((ekey  (hash/256 (ed-mul (ed-decompress-pt bpt) arand)))
                     (client-cnx (make-actor (client-connect-beh
                                              :encryptor   (pipe
                                                            (secure-sender ekey client-skey)
                                                            (create-cust-proxy server-cnx socket))
                                              :decryptor   (secure-reader ekey (ed-decompress-pt server-pkey))
                                              :admin       me))))
                (send cust (secure-send client-cnx)) ;; our local customer
                (send admin-tag :add-cnx client-cnx)
                ))))
      (beta (id)
          (create-ephemeral-service-proxy beta responder)
        (send socket id :connect server-pkey (int client-pkey) (int apt))
        )))

   ((tag :add-cnx cnx) when (eq tag admin-tag)
    (become (um:reapply #'client-crypto-gate-beh () args
                        :cnxs (cons cnx cnxs))))
   
   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh))
    (send-to-all cnxs admin-tag :shutdown))
   ))

(defun make-client-crypto-gate (client-skey)
  (actors ((gate      (client-crypto-gate-beh
                       :client-skey client-skey
                       :admin-tag   admin-tag))
           (admin-tag (tag-beh gate)))
    (values gate admin-tag)
    ))

(defun secure-send (cnx)
  ;; Make a user freindly interface to a remote connection.
  ;; Instead of writing:     (send cnx cust :send :verb . data)
  ;; we can do like always:  (send cnx cust :verb . data)
  (actor (cust service-name &rest msg)
    (send* cnx cust :send service-name msg)))

;; ---------------------------------------------------

(defun client-connect-beh (&key
                           encryptor
                           decryptor
                           admin)
  ;; One of these serves as a client-local private portal with an
  ;; established connection to a server. It encrypts and signs a
  ;; request message for a server service. It also instantiates a
  ;; private decryptor for any returning replies being sent back to
  ;; the customer on this client.
  ;;
  ;; Every request to the server sent from here carries an
  ;; incrementing sequence number to ensure a change of encryption
  ;; keying for every new message.
  (alambda
   ((cust :send verb . msg) ;; client send to a server service by name
    (beta (id)
        (create-ephemeral-service-proxy beta (pipe decryptor cust))
      (send* encryptor id verb msg)))

   ((cust :shutdown) when (eq cust admin)
    (become (sink-beh)))
   ))

;; ------------------------------------------------------------------
;; Server side

(defun server-crypto-gate-beh (&rest args &key
                                     socket
                                     server-skey
                                     services
                                     admin-tag
                                     cnxs)
  ;; Foreign clients first make contact with us here. They send us
  ;; their public key and a random ECC point. We develop a unique DHE
  ;; encryption key shared secretly between us and furnish a private handler
  ;; for encrypted requests along with our own random ECC point.
  (alambda
   ((cust :connect server-pkey client-pkey apt)
    (let ((my-pkey     (ed-mul *ed-gen* server-skey))
          (server-pkey (ed-decompress-pt server-pkey)))
      (when (ed-pt= my-pkey server-pkey) ;; did client have correct server-pkey?
        (let* ((brand     (int (ctr-drbg 256)))
               (bpt       (ed-mul *ed-gen* brand))
               (ekey      (hash/256 (ed-mul (ed-decompress-pt apt) brand)))
               (encryptor (secure-sender ekey server-skey))
               (cnx       (make-actor (server-connect-beh
                                       :socket      socket
                                       :encryptor   encryptor
                                       :services    services
                                       :admin       self)))
               (decryptor (pipe
                           (secure-reader ekey (ed-decompress-pt client-pkey))
                           cnx)))
          (beta (id)
              (create-service-proxy beta decryptor)
            (send (create-cust-proxy cust socket)  ;; remote client cust
                  id (int bpt))
            (send admin-tag :add-cnx cnx))
          ))))

   ((tag :add-cnx cnx) when (eq tag admin-tag)
    (become (um:reapply #'server-crypto-gate-beh nil args
                        :cnxs (cons cnx cnxs))))

   ((tag :add-service . _) when (eq tag admin-tag)
    (repeat-send services))

   ((tag :remove-service . _) when (eq tag admin-tag)
    (repeat-send services))

   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh))
    (send-to-all cnxs self :shutdown))
   ))

(defun server-connect-beh (&key
                           socket
                           encryptor
                           services
                           admin)
  ;; This is a private portal for exchanges with a foreign client.
  ;; One of these exist for each connection established through the
  ;; main crypto gate.
  ;;
  ;; We authenticate requests as coming from the client public key,
  ;; decrypt the requests, and pass along to a local service. For each
  ;; request we make an encrypting forwarder back to the client
  ;; customer, and pass that along as the local customer for the
  ;; request to the local service.
  (alambda
   ((tag :shutdown) when (eq tag admin)
    (become (sink-beh)))

   ((cust :available-services)
    (let ((proxy (create-cust-proxy cust socket)))
      (send services (pipe encryptor proxy) :available-services nil)))

   ((cust verb . msg) ;; remote client cust
    (let ((proxy (create-cust-proxy cust socket)))
      (send* services (pipe encryptor proxy) :send verb msg)))
   ))

;; ---------------------------------------------------------------

(defun make-server-crypto-gate (socket server-skey services)
  ;; Make a server side crypto gate. Services is an ALIST of service
  ;; Actors on the server side, associating a name with the actual
  ;; service. Names can be marshalled across a network connection,
  ;; service handlers cannot.
  ;;
  ;; We add one more service to the list, which enables a client to
  ;; ask for available services to get their names.
  (let ((service-list (make-actor (null-service-list-beh))))
    (dolist (pair services)
      (send service-list :add-service (car pair) (cdr pair)))
    (actors ((server    (server-crypto-gate-beh
                         :socket      socket
                         :server-skey server-skey
                         :services    service-list
                         :admin-tag   admin-tag))
             (admin-tag (tag-beh server)))
      (values server admin-tag)
      )))

(defvar *server-gateway* nil) ;; this can be shared
(defvar *server-admin*   nil) ;; this cannot be shared

(defconstant *server-id*    "7a1efb26-bc60-123a-a2d6-24f67702cdaa")
(defconstant *server-pkey*  #x7EBC0A8D8FFC77F24E7F271F12FC827415F0B66CC6A4C1144070A32133455F1)

#|
(multiple-value-bind (skey pkey)
    (make-deterministic-keys *server-id*)
  (with-standard-io-syntax
    (format t "~%skey: #x~x" skey)
    (format t "~%pkey: #x~x" (int pkey))))
|#

(defun start-server (socket)
  (multiple-value-bind (gateway admin)
      (make-server-crypto-gate socket #x4504E460D7822B3B0E6E3774F07F85698E0EBEFFDAA35180D19D758C2DEF09 nil)
    (setf *server-gateway* gateway
          *server-admin*   admin)))
  
;; ----------------------------------------------------------------
;; Self-organizing list of services for Server and connection Actors

(defun null-service-list-beh ()
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :available-services lst)
    (send cust (reverse lst)))

   ((_ :add-service name handler)
    (let ((next (make-actor self-beh)))
      (become (service-list-beh name handler next))))
   ))

(defun service-list-beh (name handler next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :send verb . msg) when (eql verb name)
    (send* handler cust msg))

   ((_ :add-service aname new-handler) when (eql aname name)
    (become (service-list-beh name new-handler next)))
   
   ((_ :remove-service aname) when (eql aname name)
    (prune-self next))

   ((cust :available-services lst)
    (send next cust :available-services (cons name lst)))

   ( _
     (repeat-send next))
   ))
    
;; ------------------------------------------------------------

#|
(let* ((msg :diddly)
       (seq 1)
       (ekey (ctr-drbg 256)))
  (multiple-value-bind (skey pkey) (ed-random-pair)
    (let* ((emsg (encrypt ekey seq msg))
           (sig  (make-signature emsg skey)))
       (values emsg
               sig
               (check-signature emsg sig pkey)
               (decrypt ekey seq emsg))
       )))

(defun tst-beh (&rest args &key a b c)
  ;; show the need to trim away prior garbage
  (alambda
   ((:show)
    (send writeln args)
    (when (eql a 1)
      (become (apply #'tst-beh
                     :a 2
                     args))
      (send self :show)))
   ))

(send (make-actor (tst-beh :a 1 :b 2 :c 3)) :show)

;; ---------------------------------
;; try it out...

(defun fake-socket ()
  ;; Uses a shared *local-services* between fake client and fake
  ;; server.  But that's okay because all entries have unique ID's.
  (make-actor
   (alambda
    ((cust :connect . msg)
     (send* *server-gateway* cust :connect msg))
     
    ((cust :send . msg)
     (send* *local-services* sink :send cust msg))
    )))
     
(defun make-mock-service ()
  (actor ()
    (let ((socket (fake-socket)))
      (start-server socket)
      (send *server-admin* :add-service :println println)
      (send *server-admin* :add-service :writeln writeln)
      (send (make-connection socket)))))
    
(defun make-connection (socket)
  (actor ()
    (multiple-value-bind (client-skey client-pkey)
        (make-deterministic-keys :client)
      
      (multiple-value-bind (client-gate client-admin-tag)
          (make-client-crypto-gate client-skey)
        
        (beta (cnx)
            (send client-gate beta :connect socket *server-pkey*)
          (beta (ans)
              (send cnx beta :available-services)
            (send println (format nil "from server: Services: ~S" ans))
            (send *server-admin* :remove-service :println)
            (send *server-admin* :remove-service :writeln))
          )))))

(defun tst ()
  (send (make-mock-service)))

(send (logged (make-mock-service)))

(atrace)
(atrace nil)
(tst)
(send *local-services* writeln :list nil)
(setf *dbg* t)
(setf *dbg* nil)

 |#

