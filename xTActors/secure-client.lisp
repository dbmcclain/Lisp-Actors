;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :ac-secure-comm)

;; --------------------------------------------------------------------
;; Client side

(defvar *client-gateway*  nil)
(defvar *client-admin*    nil)

(defun client-gateway ()
  (or *client-gateway*
      (setf *client-gateway*
            (let ((skey  (make-deterministic-keys (machine-instance) (uuid:make-v1-uuid))))
              (actors ((gate  (client-crypto-gate-beh
                               :client-skey  skey
                               :admin-tag    admin))
                       (admin (tag-beh gate)))
                (setf *client-admin* admin)
                gate))
            )))

(defun show-client-outbound (socket)
  (actor (&rest msg)
    (send println (format nil "c/out: ~S" msg))
    (send* socket msg)))

(defun show-client-inbound ()
  (actor (cust &rest msg)
    (send println (format nil "c/in: ~S" msg))
    (send* cust msg)))

(defun client-crypto-gate-beh (&key client-skey admin-tag)
  ;; This is the main local client service used to initiate
  ;; connections with foreign servers. We develop a DHE shared secret
  ;; encryption key for use across a private connection portal with
  ;; the server.
  ;;
  ;; One of these is constructed for each different key set on the
  ;; client. That way we avoid sending messages that contain a secret
  ;; key.
  (alambda
   ((cust :connect host-ip-addr server-pkey)
    (beta (socket)
        (send (actors/network:client-connector) beta host-ip-addr)
      (let* ((arand       (int (ctr-drbg 256)))
             (apt         (ed-mul *ed-gen* arand))
             (client-pkey (ed-mul *ed-gen* client-skey))
             (me          self)
             ;; (socket      (show-client-outbound socket)) ;; ***
             (responder
              (actor (server-id bpt)
                (let* ((ekey  (hash/256 (ed-mul (ed-decompress-pt bpt) arand)))
                       (client-cnx (make-actor (client-connect-beh
                                                :encryptor   (sink-pipe
                                                              ;; (pass)
                                                              (secure-sender ekey client-skey)
                                                              (client-side-server-proxy server-id socket))
                                                :decryptor   (pipe
                                                              ;; (pass)
                                                              (secure-reader ekey (ed-decompress-pt server-pkey))
                                                              ;; (show-client-inbound)
                                                              )
                                                :admin       me))))
                  (send cust (secure-send client-cnx)) ;; our local customer
                  ))))
        (beta (client-id)
            (create-ephemeral-service-proxy beta responder)
          (send socket :connect client-id server-pkey (int client-pkey) (int apt))
          ))))

   ((tag :shutdown) when (eq tag admin-tag)
    (become (sink-beh)))
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
    ;; (send println (format nil "trying to send: ~S" actors:*whole-message*))
    (beta (cust-id)
        (create-ephemeral-service-proxy beta (sink-pipe decryptor cust))
      (send* encryptor cust-id verb msg)))

   ((cust :shutdown) when (eq cust admin)
    (become (sink-beh)))
   ))

;; ------------------------------------------------------------------
;; User side of Client Interface

(defun resolved-remote-service-beh (name sender)
  (lambda (cust &rest msg)
    ;; (send println (format nil "rsend message was: ~S" (cons cust msg)))
    (send* sender cust name msg)))

(defun unresolved-remote-service-beh (name host-ip-addr)
  (alambda
   ((cust :xform beh msg) when (eq cust self)
    (become beh)
    (send* self msg))
   
   (msg
    (let ((me self))
      (beta (sender)
          (send (client-gateway) beta :connect host-ip-addr *server-pkey*)
        (send me me :xform (resolved-remote-service-beh name sender) msg)
        )))
   ))

(defun remote-service (name host-ip)
  (make-actor (unresolved-remote-service-beh name host-ip)))

;; ------------------------------------------------------------
#|
(defun tst ()
  (let ((recho (remote-service :echo
                               ;; "localhost"
                               ;; "arroyo.local"
                               "rincon.local"
                               )))
    (start-server-gateway)
    (sleep 1)
    (beta (ans)
        (send recho beta :hello)
      (send println (format nil "(send recho println ~S) sez: ~S" :hello ans)))))
(tst)
(atrace)
(atrace nil)


(defun tst ()
  (let ((reval (remote-service :eval
                               ;; "localhost"
                               ;; "arroyo.local"
                               "rincon.local"
                               )))
    (start-server-gateway)
    (sleep 1)
    (beta (ans)
        ;; (send reval beta '(list (get-universal-time) (machine-instance)))
        (send reval beta '(um:capture-ans-or-exn
                            (error "test-error")))
      (send println (format nil "reval sez: ~S" (um:recover-ans-or-exn ans))))
    ))
(tst)
|#
