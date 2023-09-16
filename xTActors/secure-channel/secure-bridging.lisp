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

(in-package :com.ral.actors.secure-comm)

(um:eval-always
  (import '(vec-repr:ub8
            vec-repr:ub8-vector)))

;; ------------------------------------------------------

(defconstant +server-connect-id+  {66895052-c57f-123a-9571-0a2cb67da316})

;; ------------------------------------------------------

#|
(defgeneric wipe (x)
  (:method (x)
   x)
  (:method ((x stream::ef-file-stream))
   (wipe (slot-value x 'stream::buffer-state)))
  (:method ((x stream::stream-buffer-state))
   (wipe (slot-value x 'stream::input-buffer))
   (wipe (slot-value x 'stream::output-buffer)))
  (:method ((x string))
   (fill x #\0))
  (:method ((x vector))
   (fill x 0))
  (:method ((x vec-repr:ub8v-obj))
   (wipe (vec-repr:ub8v-val x)))
  (:method ((x vec-repr:ub8v-as-str))
   (wipe (vec-repr:ub8v-str x)))
  )

(defvar *actors-node* nil)

(defun actors-skey ()
  (apply #'edec:skey (or *actors-node*
                         (setf *actors-node*
                               (let ((x (with-open-file (f "~/.actors-node")
                                          (prog1
                                              (read f)
                                            (wipe f)))))
                                 (multiple-value-bind (pkey shares)
                                     (edec:gen-shares x (ed-nth-pt x) 3)
                                   (declare (ignore pkey))
                                   shares))
                               ))))
|#
#|
(defun actors-pkey ()
  (ed-compress-pt (ed-nth-pt (actors-skey))))
|#

#|
(defun actors-smult (pt)
  (let* ((txt (with-output-to-string (s)
                (sys:call-system-showing-output (list "~/bin/edwards-tpm"
                                                      (with-standard-io-syntax
                                                        (format nil "#x~X" (int pt))))
                                                :output-stream s)
                ))
         (lines (um:split-string txt :delims '(#\newline))))
    
    (print lines)
    (ed-decompress-pt (read-from-string (subseq (cadr lines) 2)))
    ))
|#
#-:lattice-crypto
(defun actors-smult (pt)
  (let* ((txt (with-output-to-string (s)
                (sys:call-system-showing-output
                 (with-standard-io-syntax
                   (format nil "~~/bin/edwards-tpm \"#x~X\"" (int pt)))
                 :output-stream s)
                ))
         (lines (um:split-string txt :delims '(#\newline))))
    
    ;; (print lines)
    (ed-decompress-pt (read-from-string (subseq (cadr lines) 2)))
    ))

#|
(defun actors-smult (pt)
  (let* ((paths (directory "~/bin/edwards-tpm"))
         (txt (with-output-to-string (s)
                (sys:call-system-showing-output (list (namestring (car paths))
                                                      (with-standard-io-syntax
                                                        (format nil "#x~X" (int pt))))
                                                :output-stream s)
                ))
         (lines (um:split-string txt :delims '(#\newline))))
    
    (print lines)
    (ed-decompress-pt (read-from-string (subseq (cadr lines) 2)))
    ))
|#

#-:lattice-crypto
(defun actors-pkey ()
  ;; (edec:smult edec:*ed-gen*)
  (actors-smult edec:*ed-gen*))

;; ----------------------------------------------------------------
;; Group Membership Verification
;;
;; A collection of public keys that are permitted to use our services.
;; Anyone of the group can act as a Server and as a Client.
;;
;; No need for passwords, etc. If someone in the list doesn't actually
;; control the associated secret key, then they won't be able to
;; communicate across a connection. Problem solved.

#-:lattice-crypto
(defvar *allowed-members*
  (let ((s (sets:empty)))
    (dolist (pkey (with-open-file (f "~/.actor-nodes")
                    (read f)))
      (sets:addf s pkey))
    s))

#|
(let ((lst nil))
  (dolist (skey '())
    (push (int (ed-compress-pt (ed-nth-pt skey))) lst))
  (with-standard-io-syntax
    (let ((*print-base* 16.))
      (print lst))))
|#

;; ----------------------------------------------------------------
;; Self-organizing list of services for Server and connection Actors

(defun service-list-beh (lst)
  (alambda
   ((cust :available-services)
    (send cust (mapcar #'car lst)))
   
   ((cust :add-service name handler)
    ;; replace or add
    (become (service-list-beh (acons name handler
                                     (remove (assoc name lst) lst))))
    (send cust :ok))
   
   ((cust :get-service name)
    (send cust (cdr (assoc name lst))))
   
   ((cust :remove-service name)
    (become (service-list-beh (remove (assoc name lst) lst)))
    (send cust :ok))
   
   ((rem-cust verb . msg)
    (let ((pair (assoc verb lst)))
      (when pair
        (send* (cdr pair) rem-cust msg))
      ))))

;; -----------------------------------------------
;; Simple Services

(deflex remote-echo
  (create
   (lambda (cust &rest msg)
     (send* cust msg)) ))

(defun cmpfn (&rest args)
  (compile nil `(lambda ()
                  ,@args)))

(deflex remote-eval
  (create
   (lambda (cust form)
     (send cust (funcall (cmpfn form)))) ))

;; -----------------------------------------------

(def-actor global-services
  (create (service-list-beh
           `((:echo . ,remote-echo)
             (:eval . ,remote-eval))
           )))

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
;;
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

;; -------------------------------------------------------------------

(defstruct (local-service
            (:constructor local-service (handler)))
  handler)

(defstruct (ephem-service
            (:include local-service)
            (:constructor ephem-service (handler &optional (ttl *default-ephemeral-ttl*))))
  ttl)

;; -------------------------------------------------------------------

(defun local-services-beh (&optional svcs encryptor decryptor)
  (alambda
   ((cust :add-service-with-id id actor)
    ;; insert ahead of any with same id
    (let ((new-svcs (acons id (local-service actor) svcs)))
      (become (local-services-beh new-svcs encryptor decryptor))
      (send cust id)))
   
   ((cust :add-ephemeral-client-with-id id actor ttl)
    (let ((new-svcs (acons id (ephem-service actor ttl) svcs)))
      (become (local-services-beh new-svcs encryptor decryptor))
      (send cust id)
      (when ttl
        (send-after ttl self sink :remove-service id))))
   
   ((cust :add-service actor)
    ;; used for connection handlers
    (send self cust :add-service-with-id (uuid:make-v1-uuid) actor))

   ((cust :add-single-use-service id actor)
    (let ((new-svcs (acons id (ephem-service actor) svcs)))
      (become (local-services-beh new-svcs encryptor decryptor))
      (send-after *default-ephemeral-ttl* self sink :remove-service id)
      (send cust id)))
   
   ((cust :add-ephemeral-client actor ttl)
    ;; used for transient customer proxies
    (send self cust :add-ephemeral-client-with-id (uuid:make-v1-uuid) actor ttl))

   ((cust :add-ephemeral-clients clients ttl)
    (if clients
        (let+ ((me  self)
               ( ((id . ac) . rest) clients))
            (β _
                (send self β :add-ephemeral-client-with-id id ac ttl)
              (send me cust :add-ephemeral-clients rest ttl) ))
      ;; else
      (send cust :ok)))
   
   ((cust :remove-service id)
    (let ((new-svcs (remove (assoc id svcs :test #'uuid:uuid=) svcs :count 1)))
      (become (local-services-beh new-svcs encryptor decryptor))
      (send cust :ok)))
   
   ((cust :set-crypto ekey socket)
    ;; after this we promptly forget ekey...
    (let ((encryptor (sink-pipe (secure-sender ekey self)
                                socket))
          (decryptor (sink-pipe (secure-reader ekey self)
                                self)))
      (become (local-services-beh svcs encryptor decryptor))
      (send cust :ok)))
   
   ;; -------------------------------------------------------------------
   ;; encrytped socket send - proxy Actors send to here...  The entire
   ;; message, including UUID target, is encrypted. The only thing
   ;; appearing on the wire are the (SEQ CTXT AUTH)
   ((:ssend . msg) / encryptor
    (send* encryptor msg))
   
   ;; -------------------------------------------------------------------
   ;; unencrypted socket delivery
   ((service-id . msg) / (typep service-id 'uuid:uuid)
    (let ((pair (assoc service-id svcs :test #'uuid:uuid=)))
      (when pair
        (let ((svc (cdr pair)))
          (send* (local-service-handler svc) msg)
          (when (ephem-service-p svc)
            (cond ((ephem-service-ttl svc)
                   ;; possibly counterintuitive... if we have traffic on this
                   ;; ephemeral connection, keep it alive a bit longer in case
                   ;; it gets reused. Removal only removes one copy of the
                   ;; pairing in the services list. Since a removal has already
                   ;; been scheduled, we insert an extra one for it to work
                   ;; against.
                   (become (local-services-beh (cons pair svcs) encryptor decryptor))
                   (send-after (ephem-service-ttl svc) self sink :remove-service (car pair)))
                  (t
                   ;; no TTL specified, so just remove it
                   (become (local-services-beh (remove pair svcs :count 1) encryptor decryptor)))
                  ))
          ))))

   ;; -------------------------------------------------------------------
   ;; encrypted handshake pair
   
   #+:lattice-crypto
   ((latcrypt aescrypt) / (and (null decryptor) ;; i.e., only valid during initial handshake dance
                               (typep latcrypt 'vector)
                               (consp aescrypt))
    (β (rkey info)
        (send lattice-ke:cnx-packet-decoder β latcrypt aescrypt)
      (when (typep (car info) 'uuid:uuid)
        (let ((pair (assoc (car info) svcs :test #'uuid:uuid=)))
          (when pair
            (let ((svc  (cdr pair)))
              (send* (local-service-handler svc) rkey (cdr info))
              ))))
      ))

   #-:lattice-crypto
   ((rand-pt aescrypt) / (and (null decryptor)
                              (typep rand-pt 'edec:ecc-pt)
                              (consp aescrypt))
    (β (info)
        (send eccke:ecc-cnx-decrypt β rand-pt aescrypt)
      (when (typep (car info) 'uuid:uuid)
        (let ((pair (assoc (car info) svcs :test #'uuid:uuid=)))
          (when pair
            (let ((svc  (cdr pair)))
              (send* (local-service-handler svc) rand-pt (cdr info))
              ))))
      ))
        
   ;; -------------------------------------------------------------------
   ;; encrypted socket delivery -- decryptor decodes the message and
   ;; sends back to us as an unencrypted socket delivery (see previous
   ;; clause)
   ((seq ctxt auth) / (and decryptor
                           ;; (integerp seq)
                           (typep seq  'ub8-vector)
                           (typep ctxt 'ub8-vector)
                           (typep auth 'ub8-vector))
    (send decryptor seq ctxt auth))
   ))

(defun make-local-services ()
  (create (local-services-beh)))


(defun create-ephemeral-client-proxy (cust local-services svc &key (ttl *default-ephemeral-ttl*))
  ;; used by client side
  (send local-services cust :add-ephemeral-client svc ttl))

(defun create-ephemeral-client-proxy-with-id (cust client-id local-services svc &key (ttl *default-ephemeral-ttl*))
  ;; used by client side
  (send local-services cust :add-ephemeral-client-with-id client-id svc ttl))

(defun create-service-proxy (cust local-services svc)
  ;; used by server side
  (send local-services cust :add-service svc))

#| ---------------------------------------------------
   Marshaling with Actor conversions
  
   On the way out, any Actors in message args are replaced by
   CLIENT-PROXY structs carrying a UUID as the identifier for the
   Actor on this side.
  
   On the way in, any CLIENT-PROXY structs are replaced by local proxy
   Actors which arrange to send back to the UUID client.
  
   We perform this substitution down inside of marshaling so that
   Actors can be buried to any depth anywhere among the message args
   and they will receive the translation.
  
   When the marshaler in SDLE-STORE is about to serialize a structure
   object, it first calls BEFORE-STORE and then serializes whatever
   that function returns. Nominally it just returns the argument
   struct object.
  
   After the marshaler deserializes a struct object, it returns
   whatever the result of AFTER-RETRIEVE returns. Normally it just
   returns its argument object.
  
   So rather than inventing whole new type encodings for Actors, we
   can use AOP and dynamic functions to effect the added translations
   just while we are encoding and decoding for socket transmission.
   --------------------------------------------------- |#

(defstruct (client-proxy
            (:constructor client-proxy (&optional (id (uuid:make-v1-uuid)))))
  id)

(aop:defdynfun translate-actor-to-proxy (ac)
  ac)

(defmethod sdle-store:backend-store-object :around (backend (obj actor) stream)
  (let ((xobj (translate-actor-to-proxy obj)))
    (call-next-method backend xobj stream)))
  
(defun client-marshal-encoder (local-services)
  ;; serialize an outgoing message, translating all embedded Actors
  ;; into client proxies and planting corresponding ephemeral
  ;; forwarding receiver Actors.
  (create
   (lambda (cust &rest msg)
     (let (rcvrs)
       (aop:dflet ((translate-actor-to-proxy (ac)
                     (if (is-pure-sink? ac)
                         nil
                       (let ((id (uuid:make-v1-uuid)))
                         (push (cons id ac) rcvrs)
                         (client-proxy id))
                       )))
         (let ((enc (loenc:encode (coerce msg 'vector))))
           (β _
               (send local-services β :add-ephemeral-clients rcvrs *default-ephemeral-ttl*)
             (send cust enc))
           ))))
   ))

#|
(def-actor echo
  (lambda (cust &rest msg)
    (send cust msg)))

(let ((encoder (client-marshal-encoder echo)))
  (let ((obj  println))
    (β (enc)
        (send encoder β obj)
      (send fmt-println"Encoding: ~S" enc)
      (send fmt-println "Decoding: ~S" (loenc:decode enc))
      )))
|#

;; -------------------------------------------------------

(aop:defdynfun translate-proxy-to-actor (proxy)
  proxy)

(defgeneric after-restore (obj)
  (:method (obj)
   obj)
  (:method ((obj client-proxy))
   (translate-proxy-to-actor obj)))

(defmethod sdle-store:backend-restore-object :around (backend place)
  (after-restore (call-next-method)))
      
(defun server-marshal-decoder (local-services)
  ;; deserialize an incoming message, translating all client Actor
  ;; proxies to server local proxie Actors aimed back at client.
  (create
   (alambda
    ((cust vec) / (typep vec 'ub8-vector)
     (aop:dflet ((translate-proxy-to-actor (proxy)
                   (let ((id (client-proxy-id proxy)))
                     (create
                      (lambda (&rest msg)
                        (send* local-services :ssend id msg))
                      ))))
       (let ((dec (ignore-errors
                    (loenc:decode vec))))
         (when (and dec
                    (vectorp dec))
           (send* cust (coerce dec 'list)))
         )))
    )))

;; ---------------------------------------------------
;; Composite Actor pipes - used by both clients and servers. Both
;; receive the exact same treatment. The only distinction between
;; client and server is that client begins the channel connection, and
;; server offers to supply global services.

(defun secure-sender (ekey local-services)
  (pipe (client-marshal-encoder local-services) ;; translate Actors to CLIENT-PROXY's
        (marshal-compressor)
        (chunker 65000)
        (marshal-encoder)
        (encryptor ekey)
        (authentication ekey)
        ))
  
(defun secure-reader (ekey local-services)
  (pipe (check-authentication ekey)
        (decryptor ekey)
        (fail-silent-marshal-decoder)
        (dechunker)
        (fail-silent-marshal-decompressor)
        (server-marshal-decoder local-services) ;; translate CLIENT-PROXY's back to proxy Actors
        ))

;; ----------------------------------------------
