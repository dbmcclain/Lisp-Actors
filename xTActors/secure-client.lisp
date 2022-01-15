;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :com.ral.actors.secure-comm)

;; --------------------------------------------------------------------
;; Client side

(defun client-connect-beh (pending-cx)
  (λ (cust host-ip-addr)
    (β (socket chan local-services)
        (send com.ral.actors.network:client-connector β host-ip-addr)
      (if (eq chan socket)
          (send pending-cx cust :get-chan socket local-services)
        (send cust chan))
      )))

#| ;; for debugging
(defun show-client-outbound (socket)
  (α (&rest msg)
    (send println (format nil "c/out: ~S" msg))
    (send* socket msg)))

(defun show-client-inbound ()
  (α (cust &rest msg)
    (send println (format nil "c/in: ~S" msg))
    (send* cust msg)))
|#

(defun empty-pending-negotiations-beh (top client-skey)
  (prunable-alambda

   ((cust :get-chan socket local-services)
    (let ((next (make-actor self-beh)))
      (become (pending-negotiation-beh socket (list cust) next))
      (send (negotiate-secure-channel client-skey socket local-services) top)))
   ))

(defun init-pending-negotiations-beh (client-skey)
  (λ _
    (become (empty-pending-negotiations-beh self client-skey))
    (repeat-send self)))

(defun pending-negotiation-beh (socket custs next)
  (prunable-alambda

   ((cust :get-chan a-socket . _) when (eq a-socket socket)
    (become (pending-negotiation-beh socket (cons cust custs) next)))

   ((:use-chan a-socket chan) when (eq a-socket socket)
    (prune-self next)
    (send-to-all custs chan))

   (_
    (repeat-send next))
   ))

(defun negotiate-secure-channel (client-skey socket local-services)
  (α (cust)
    (let* ((arand       (int (ctr-drbg 256)))
           (apt         (ed-nth-pt arand))
           (client-pkey (ed-nth-pt client-skey))
           (srv-pkey    (server-pkey))
           ;; (socket      (show-client-outbound socket)) ;; ***
           (responder
            (α (server-id bpt)
              (let* ((ekey  (hash/256 (ed-mul (ed-decompress-pt bpt) arand)))
                     (chan  (client-channel
                             :local-services  local-services
                             :encryptor       (sink-pipe
                                               (secure-sender ekey client-skey)
                                               (remote-actor-proxy server-id socket))
                             :decryptor       (secure-reader ekey (ed-decompress-pt srv-pkey))
                             )))
                (β _
                    (send com.ral.actors.network:connections β :set-channel socket chan)
                  (send cust :use-chan socket chan)) ;; to our local customer
                ))))
      (β (client-id)
          (create-ephemeral-client-proxy β local-services responder)
        (send (remote-actor-proxy +server-connect-id+ socket)
              client-id srv-pkey (int client-pkey) (int apt))
        ))
    ))

(defun init-gateway-beh ()
  (λ _
    (let* ((skey (make-deterministic-keys (uuid:make-v1-uuid)))
           (pend (make-actor (init-pending-negotiations-beh skey))))
      (become (client-connect-beh pend))
      (repeat-send self))
    ))
    
(deflex client-gateway
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
  (make-actor (init-gateway-beh)))
    
;; ---------------------------------------------------

(defun client-channel (&key
                       local-services
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
  (α (cust verb &rest msg)
    ;; (send println (format nil "trying to send: ~S" self-msg))
    (if (is-pure-sink? cust)
        (send* encryptor nil verb msg)
      (β (cust-id)
          (create-ephemeral-client-proxy β local-services (sink-pipe decryptor cust))
        (send* encryptor cust-id verb msg))
      )))

;; ------------------------------------------------------------------
;; User side of Client Interface

(defun remote-service (name host-ip-addr)
  ;; An Actor and send target. Connection to remote service
  ;; established on demand.
  (α (cust &rest msg)
    (β (chan)
        (send client-gateway β host-ip-addr)
      (send* chan cust name msg))))

;; ------------------------------------------------------------
#|
(defun tst (host)
  (let ((recho (remote-service :echo host)))
    (β (ans)
        (send recho β :hello)
      (send println (format nil "(send recho println ~S) sez: ~S" :hello ans)))))
(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "rambo.local")
(atrace)
(atrace nil)


(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (ans)
        (send reval β '(list (get-universal-time) (machine-instance)))
      #|
        (send reval β '(um:capture-ans-or-exn
                            (error "test-error")))
        |#
      (send println (format nil "reval sez: ~S" (um:recover-ans-or-exn ans))))
    ))
(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "rambo.local")
(tst "dachshund.local")
(tst "honeypot.local")
(tst "david-pc.local")
(tst "umbra.local")
(tst "zircon.local")
|#
