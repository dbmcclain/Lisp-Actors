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

;; ------------------------------------------------------------------

(defun reapply (fn reqd restargs &rest parms)
  ;; Like APPLY, but used to substitute new keyword args for old,
  ;; removing all the old kw args to prevent accumulation of old stuff
  ;; and prevent its GC.
  (apply fn (append reqd
                    (append parms
                            (alexandria:alist-plist
                             (set-difference (alexandria:plist-alist restargs)
                                             (alexandria:plist-alist parms)
                                             :key #'car))))))
#|
(defun reapply (fn reqd restargs &rest parms)
  ;; Like APPLY, but used to substitute new keyword args for old,
  ;; removing all the old kw args to prevent accumulation of old stuff
  ;; and prevent its GC.
  (let* ((keys (um:nlet iter ((lst parms)
                              (ans nil))
                 (if lst
                     (go-iter (cddr lst) (cons (car lst) ans))
                   ans)))
        (trimmed (um:nlet iter ((lst restargs)
                                (ans parms))
                   (if lst
                       (go-iter (cddr lst)
                                (if (find (car lst) keys)
                                    ans
                                  (cons (car lst) (cons (cadr lst) ans))))
                     ans))))
    (apply fn (append reqd trimmed))
    ))
|#

(defun pt->int (ecc-pt)
  (int (ed-compress-pt ecc-pt)))

(defun int->pt (int)
  (ed-decompress-pt int))

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
   ((cust :connect server server-pkey)
    (let* ((arand       (int (ctr-drbg 32)))
           (apt         (ed-mul *ed-gen* arand))
           (client-pkey (ed-mul *ed-gen* client-skey)))
      (beta (server-cnx bpt)
          (send server beta :connect
                server-pkey
                (pt->int client-pkey)
                (pt->int apt))
        (let* ((ekey  (hash/256 (ed-mul (int->pt bpt) arand)))
               (client-cnx (make-actor (client-connect-beh
                                        :ekey        ekey
                                        :client-skey client-skey
                                        :server-pkey (int->pt server-pkey)
                                        :server-cnx  server-cnx
                                        :admin-tag   admin-tag))))
          (send cust (secure-send client-cnx))
          (become (reapply #'client-crypto-gate-beh () args
                           :cnxs (cons client-cnx cnxs)))
          ))))
   
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

(defun client-connect-beh (&rest args &key
                                 ekey
                                 client-skey
                                 server-pkey
                                 server-cnx
                                 (seq 0)
                                 admin-tag
                                 decs)
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
   ((cust :send service-name . msg) ;; client send to a server service by name
    (let* ((full-msg (cons service-name msg))
           (emsg     (encrypt ekey seq full-msg))
           (sig      (make-signature seq emsg client-skey))
           (dec      (make-actor (decryptor-beh
                                  :cust        cust
                                  :ekey        ekey
                                  :seq         seq
                                  :server-pkey server-pkey
                                  :admin       self))))
      (become (reapply #'client-connect-beh nil args
                     :seq  (1+ seq)
                     :decs (cons dec decs)))
      (send server-cnx dec seq emsg sig)
      ))

   ((dec :forward . msg) when (find dec decs)
    (become (reapply #'client-connect-beh nil args
                   :decs (remove dec decs)))
    (send* msg))

   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh))
    (send-to-all decs self :shutdown))
   ))

(defun decryptor-beh (&key cust
                           ekey
                           seq
                           server-pkey
                           admin)
  ;; This is a private decrypting customer, sent along to the server
  ;; as the customer for our request. One of these for every request
  ;; made through our private connection.
  ;;
  ;; It authenticates a reply from the server, decrypts it with the
  ;; roving encryption key, and forwards the reply to the local
  ;; customer on the client.
  (alambda
   ((tag :shutdown) when (eq tag admin)
    (become (sink-beh)))

   ((aseq ereply sig)
    (when (and (eql seq aseq)
               (check-signature seq ereply sig server-pkey))
      (let ((reply (decrypt ekey seq ereply)))
        (send* admin self :forward cust reply)
        (become (sink-beh))
        )))
   ))

;; ------------------------------------------------------------------
;; Server side

(defun server-crypto-gate-beh (&rest args &key
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
          (server-pkey (int->pt server-pkey)))
      (when (ed-pt= my-pkey server-pkey) ;; did client have correct server-pkey?
        (let* ((brand  (int (ctr-drbg 32)))
               (bpt    (ed-mul *ed-gen* brand))
               (ekey   (hash/256 (ed-mul (int->pt apt) brand)))
               (cnx    (make-actor (server-connect-beh
                                    :ekey        ekey
                                    :client-pkey (int->pt client-pkey)
                                    :server-skey server-skey
                                    :services    services
                                    :admin-tag   admin-tag))))
          (send cust cnx (pt->int bpt))
          (become (reapply #'server-crypto-gate-beh nil args
                           :cnxs (cons cnx cnxs)))
          ))))

   ((tag :add-service name handler) when (eq tag admin-tag)
    (let ((new-services (list* (car services)
                               (cons name handler)
                               (cdr services))))
      (become (reapply #'server-crypto-gate-beh nil args
                     :services new-services))
      (send-to-all cnxs admin-tag :update-services new-services)
      ))

   ((tag :remove-service name) when (eq tag admin-tag)
    (let ((new-services (list* (car services)
                               (remove name (cdr services) :key #'car))
                        ))
      (become (reapply #'server-crypto-gate-beh nil args
                     :services new-services))
      (send-to-all cnxs admin-tag :update-services new-services)
      ))

   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh))
    (send-to-all cnxs self :shutdown))
   ))

(defun make-server-crypto-gate (server-skey services)
  ;; Make a server side crypto gate. Services is an ALIST of service
  ;; Actors on the server side, associating a name with the actual
  ;; service. Names can be marshalled across a network connection,
  ;; service handlers cannot.
  ;;
  ;; We add one more service to the list, which enables a client to
  ;; ask for available services to get their names.
  (let ((avail (actor (cust)
                 (send cust (mapcar #'car services)))))
    (actors ((server    (server-crypto-gate-beh
                         :server-skey server-skey
                         :services    (acons :available-services avail services)
                         :admin-tag   admin-tag))
             (admin-tag (tag-beh server)))
      (values server admin-tag)
      )))

;; ----------------------------------------------------------------

(defun server-connect-beh (&rest args &key
                                 ekey
                                 client-pkey
                                 server-skey
                                 services
                                 admin-tag
                                 encs)
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
   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh))
    (send-to-all encs self :shutdown))

   ((tag :update-services new-services) when (eq tag admin-tag)
    (become (reapply #'server-connect-beh nil args
                   :services new-services)))

   ((enc :forward . msg) when (find enc encs)
    (become (reapply #'server-connect-beh nil args
                     :encs (remove enc encs)))
    (send* msg))
   
   ((cust seq emsg sig)
    (when (check-signature seq emsg sig client-pkey)
      (let* ((msg  (decrypt ekey seq emsg))
             (svc  (sys:cdr-assoc (car msg) services)))
        (when svc
          (let ((enc  (make-actor (encryptor-beh
                                   :cust        cust
                                   :ekey        ekey
                                   :seq         seq
                                   :server-skey server-skey
                                   :admin       self))))
            (send* svc enc (cdr msg))
            (become (reapply #'server-connect-beh nil args
                           :encs (cons enc encs)))
            )))
      ))
   ))

(defun encryptor-beh (&key cust
                           ekey
                           seq
                           server-skey
                           admin)
  ;; One of these is instantiated for each request arriving through
  ;; our private channel with the client. It serves as an encrypting
  ;; forwarding customer, back to the client, carrying replies from
  ;; the local service request.
  (alambda
   ((tag :shutdown) when (eq tag admin)
    (become (sink-beh)))
   
   (reply
    (let* ((ereply (encrypt ekey seq reply))
           (sig    (make-signature seq ereply server-skey)))
      (send admin self :forward cust seq ereply sig)
      (become (sink-beh))
      ))
   ))

;; ------------------------------------------------------------

(defun encrypt (ekey seq msg)
  (let* ((enc  (loenc:encode msg))
         (mask (vec (get-hash-nbytes (length enc) ekey seq))))
    (map 'vector #'logxor enc mask)))

(defun decrypt (ekey seq emsg)
  (let ((mask  (vec (get-hash-nbytes (length emsg) ekey seq))))
    (loenc:decode (map 'vector #'logxor emsg mask))))

(defun make-signature (seq emsg skey)
  (let* ((pkey  (ed-mul *ed-gen* skey))
         (krand (int (hash/256 seq emsg skey pkey)))
         (kpt   (ed-mul *ed-gen* krand))
         (h     (int (hash/256 seq emsg kpt pkey)))
         (u     (with-mod *ed-r*
                  (m+ krand (m* h skey))))
         (upt   (ed-mul *ed-gen* u)))
    (list (pt->int upt) krand)
    ))

(defun check-signature (seq emsg auth pkey)
  (destructuring-bind (upt krand) auth
    (let* ((kpt  (ed-mul *ed-gen* krand))
           (h    (int (hash/256 seq emsg kpt pkey))))
      (ed-pt= (int->pt upt) (ed-add kpt (ed-mul pkey h)))
      )))

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

(defun tst ()
  (multiple-value-bind (server-skey server-pkey)
      (make-deterministic-keys :server)
    
    (multiple-value-bind (client-skey client-pkey)
        (make-deterministic-keys :client)

      (multiple-value-bind (server-gate server-admin-tag)
          (make-server-crypto-gate server-skey
                                   `((:println ,println)
                                     (:logged  #'logged)))

        (multiple-value-bind (client-gate client-admin-tag)
            (make-client-crypto-gate client-skey)

          (beta (cnx)
              (send client-gate beta :connect server-gate (pt->int server-pkey))
            (send cnx writeln :available-services)
            ))))))

(tst)
 

 |#