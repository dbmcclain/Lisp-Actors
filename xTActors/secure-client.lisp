;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :ac-secure-comm)

;; --------------------------------------------------------------------
;; Client side

(defvar *client-gateway*  nil)

(defun client-gateway ()
  (or *client-gateway*
      (setf *client-gateway*
            (client-crypto-gate (make-deterministic-keys (uuid:make-v1-uuid))))
      ))

#| ;; for debugging
(defun show-client-outbound (socket)
  (actor (&rest msg)
    (send println (format nil "c/out: ~S" msg))
    (send* socket msg)))

(defun show-client-inbound ()
  (actor (cust &rest msg)
    (send println (format nil "c/in: ~S" msg))
    (send* cust msg)))
|#

(defun client-crypto-gate (client-skey)
  ;; This is the main local client service used to initiate
  ;; connections with foreign servers. We develop a DHE shared secret
  ;; encryption key for use across a private connection portal with
  ;; the server.
  ;;
  ;; A Connection consists of a Socket and a Channel. A Socket merely
  ;; supports the tranport of data across a network. It does not
  ;; specify any data protocol. It may marshal objects and compress
  ;; the resulting byte stream before sending. A Channel is an
  ;; encryptor/decryptor married to a Socket.
  (serializer
   (actor (cust host-ip-addr)
     (beta (socket chan)
         (send (with-timeout 6 (actors/network:client-connector)
                             (actor _
                               (serializer-abort cust)))
               beta host-ip-addr)
       (if chan
           (send cust chan)
         ;; else - negotiate a connection with the server
         (let* ((arand       (int (ctr-drbg 256)))
                (apt         (ed-nth-pt arand))
                (client-pkey (ed-nth-pt client-skey))
                ;; (socket      (show-client-outbound socket)) ;; ***
                (responder
                 (actor (server-id bpt)
                   (let* ((ekey  (hash/256 (ed-mul (ed-decompress-pt bpt) arand)))
                          (chan  (client-channel
                                       :encryptor   (sink-pipe
                                                     (secure-sender ekey client-skey)
                                                     (client-side-server-proxy server-id socket))
                                       :decryptor   (secure-reader ekey (ed-decompress-pt *server-pkey*))
                                       )))
                     (beta _
                         (send (actors/network:connections) beta :add-channel socket chan)
                       (send cust chan)) ;; to our local customer
                     ))))
           (beta (client-id)
               (create-ephemeral-client-proxy beta responder)
             (send socket :connect client-id *server-pkey* (int client-pkey) (int apt))
             )))))
   ))

;; ---------------------------------------------------

(defun client-channel (&key
                       encryptor
                       decryptor)
  ;; One of these serves as a client-local private portal with an
  ;; established connection to a server. It encrypts and signs a
  ;; request message for a server service. It also instantiates a
  ;; private decryptor for any returning replies being sent back to
  ;; the customer on this client.
  ;;
  ;; Every request to the server sent from here carries an
  ;; incrementing sequence number to ensure a change of encryption
  ;; keying for every new message.
  (actor (cust verb &rest msg)
    ;; (send println (format nil "trying to send: ~S" self-msg))
    (beta (cust-id)
        (create-ephemeral-client-proxy beta (sink-pipe decryptor cust))
      (send* encryptor cust-id verb msg))))

;; ------------------------------------------------------------------
;; User side of Client Interface

(defun remote-service (name host-ip-addr)
  (actor (cust &rest msg)
    (beta (chan)
        (send (client-gateway) beta host-ip-addr)
      (send* chan cust name msg))))

;; ------------------------------------------------------------
#|
(defun tst (host)
  (let ((recho (remote-service :echo host)))
    (beta (ans)
        (send recho beta :hello)
      (send println (format nil "(send recho println ~S) sez: ~S" :hello ans)))))
(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "rambo.local")
(atrace)
(atrace nil)


(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (beta (ans)
        (send reval beta '(list (get-universal-time) (machine-instance)))
      #|
        (send reval beta '(um:capture-ans-or-exn
                            (error "test-error")))
        |#
      (send println (format nil "reval sez: ~S" (um:recover-ans-or-exn ans))))
    ))
(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "rambo.local")
|#
