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

(defun addnew-to-plist (plist-start plist-adds)
  (let ((unique #()))
    (do ((pa  plist-adds  (cddr pa))
         (pd  plist-start))
        ((endp pa) pd)
      (when (eq unique (getf pd (car pa) unique))
        (setf pd (list* (car pa) (cadr pa) pd)))
      )))

(defun reapply (fn reqd restargs &rest parms)
  ;; Like APPLY, but used to substitute new keyword args for old,
  ;; removing all the old kw args to prevent accumulation of old stuff
  ;; and allow their GC.
  (declare (list reqd restargs parms))
  (multiple-value-call fn (values-list reqd)
    (values-list (addnew-to-plist parms restargs))
    ))

(defun pt->int (ecc-pt)
  (int (ed-compress-pt ecc-pt)))

(defun int->pt (int)
  (ed-decompress-pt int))

;; --------------------------------------------------------------------
;; Crypto Primitives

(defun get-random-seq ()
  (ecc-crypto-b571:ctr-drbg 256))

(defun encrypt (ekey seq bytevec)
  ;; takes a bytevec and produces an encrypted bytevec
  ;;
  ;; One-time-pad encryption via XOR with random mask. Take care to
  ;; never re-use the same mask.
  (let ((mask (vec (get-hash-nbytes (length bytevec) ekey seq))))
    (map 'vector #'logxor bytevec mask)))

(defun decrypt (ekey seq emsg)
  ;; takes an encrypted bytevec and produces a bytevec
  (let ((mask  (vec (get-hash-nbytes (length emsg) ekey seq))))
    (map 'vector #'logxor emsg mask)))

(defun make-signature (seq emsg skey)
  ;; Generate and append a Schnorr signature - signature includes seq
  ;; and emsg.
  ;;
  ;; We take care to use deterministic hashing so that the same
  ;; message and seq always produces the same signature, for the given
  ;; secret key, skey. This is doubly cautious here, since seq is only
  ;; ever supposed to be used just once.
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
  ;; takes seq, emsg, and sig (a Schnorr signature on seq+emsg), and
  ;; produce t/f on signature as having come from pkey.
  (destructuring-bind (upt krand) auth
    (let* ((kpt  (ed-mul *ed-gen* krand))
           (h    (int (hash/256 seq emsg kpt pkey))))
      (ed-pt= (int->pt upt) (ed-add kpt (ed-mul pkey h)))
      )))

;; ----------------------------------------------
;; Actor crypto component blocks
;;
;; The term "Arbitrary Objects" here refers to serializable objects -
;; just about anything, except compiled closures.

(defun marshal-encoder ()
  ;; takes arbitrary objects and produces an encoded bytevec
  (actor (cust &rest msg)
    (send cust (loenc:encode msg))))

(defun marshal-decoder ()
  ;; takes an encoded bytevec and produces arbitrary objects
  (actor (cust msg)
    (send* cust (loenc:decode msg))))

(defun encryptor (ekey)
  ;; Takes a bytevec and produces an encrypted bytevec
  ;;
  ;; Since we are encrypting via XOR with a random mask, we must
  ;; ensure that no two messages are ever encrypted with the same
  ;; keying. We do that by ensuring that every encryption is by way of
  ;; a new mask chosen from a PRF. This is a one-time-pad encryption.
  ;;
  ;; We use SHA3/256 as that PRF, and compute the next seq as the hash
  ;; of the current one. The likelihood of seeing a same key arise is
  ;; the same as the likelihood of finding a SHA3/256 hash collision.
  ;; Not very likely...
  ;;
  ;; The initial seq is chosen randomly over the field of 256 bit
  ;; integers, using NIST Hash DRBG.
  ;;
  ;; The master key never changes, but the keying used for any message
  ;; is the hash of the master key concatenated with the seq. The seq
  ;; is sent along as part of the message so that someone sharing the
  ;; same master key will be able to decrypt the message.
  ;;
  (labels ((encryptor-beh (ekey seq)
             (lambda (cust bytevec)
               (let ((emsg (encrypt ekey seq bytevec)))
                 (send cust seq emsg)
                 (let ((new-seq (hash:hash/256 seq)))
                   (become (encryptor-beh ekey new-seq)))
                 ))))
    (make-actor (encryptor-beh ekey (get-random-seq)))
    ))

(defun decryptor (ekey)
  ;; takes an encrypted bytevec and produces a bytevec
  (actor (cust seq emsg)
    (let ((ans (decrypt ekey seq emsg)))
      (send cust ans))))

(defun signing (skey)
  ;; takes seq, enc msg, and produces seq, enc msg, sig
  (actor (cust seq emsg)
    (let ((sig (make-signature seq emsg skey)))
      (send cust seq emsg sig))))

(defun signature-validation (pkey)
  ;; takes seq, enc msg, sig and produces seq, enc msg
  (actor (cust seq emsg sig)
    (when (check-signature seq emsg sig pkey)
      (send cust seq emsg))))

;; ---------------------------------------------------
;; Composite Actor chains

(defun secure-sender (ekey skey)
  (chain (marshal-encoder)
         (encryptor ekey)
         (signing   skey)
         (marshal-encoder)))

(defun secure-reader (ekey pkey)
  (chain (marshal-decoder)
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
   ((cust :connect server server-pkey)
    (let* ((arand       (int (ctr-drbg 256)))
           (apt         (ed-mul *ed-gen* arand))
           (client-pkey (ed-mul *ed-gen* client-skey)))
      (beta (server-cnx bpt)
          (send server beta :connect
                server-pkey
                (pt->int client-pkey)
                (pt->int apt))
        (let* ((ekey  (hash/256 (ed-mul (int->pt bpt) arand)))
               (client-cnx (make-actor (client-connect-beh
                                        :encryptor   (chain
                                                      (secure-sender ekey client-skey)
                                                      server-cnx)
                                        :decryptor   (secure-reader ekey (int->pt server-pkey))
                                        :admin       self))))
          (send cust (secure-send client-cnx)) ;; our local customer
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
    (send* encryptor (chain decryptor cust) verb msg))

   ((cust :shutdown) when (eq cust admin)
    (become (sink-beh)))
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
        (let* ((brand     (int (ctr-drbg 256)))
               (bpt       (ed-mul *ed-gen* brand))
               (ekey      (hash/256 (ed-mul (int->pt apt) brand)))
               (encryptor (secure-sender ekey server-skey))
               (decryptor (secure-reader ekey (int->pt client-pkey)))
               (cnx       (make-actor (server-connect-beh
                                       :encryptor   encryptor
                                       :services    services
                                       :admin       self))))
          (send cust (chain decryptor cnx) (pt->int bpt)) ;; remote client cust
          (become (reapply #'server-crypto-gate-beh nil args
                           :cnxs (cons cnx cnxs)))
          ))))

   ((tag :add-service . _) when (eq tag admin-tag)
    (repeat-send services))

   ((tag :remove-service . _) when (eq tag admin-tag)
    (repeat-send services))

   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh))
    (send-to-all cnxs self :shutdown))
   ))

(defun server-connect-beh (&key
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
    (send services (chain encryptor cust) :available-services nil))

   ((cust verb . msg) ;; remote client cust
    (send* services (chain encryptor cust) :send verb msg))
   ))

;; ---------------------------------------------------------------

(defun make-server-crypto-gate (server-skey services)
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
    (format t "~%pkey: #x~x" (pt->int pkey))))
|#

(defun start-server ()
  (multiple-value-bind (gateway admin)
      (make-server-crypto-gate #x4504E460D7822B3B0E6E3774F07F85698E0EBEFFDAA35180D19D758C2DEF09 nil)
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

(defun make-mock-service ()
  (actor ()
    (start-server)
    (send *server-admin* :add-service :println println)
    (send *server-admin* :add-service :writeln writeln)
    (send (make-connection))))
    
(defun make-connection ()
  (actor ()
    (multiple-value-bind (client-skey client-pkey)
        (make-deterministic-keys :client)
      
      (multiple-value-bind (client-gate client-admin-tag)
          (make-client-crypto-gate client-skey)
        
        (beta (cnx)
            (send client-gate beta :connect *server-gateway* *server-pkey*)
          (beta (ans)
              (send cnx beta :available-services)
            (send println (format nil "  from server: Services: ~S" ans))
            (send *server-admin* :remove-service :println)
            (send *server-admin* :remove-service :writeln))
          )))))

(defun tst ()
  (send (make-mock-service)))

(tst)
 

 |#