;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :ac-secure-comm)

;; ------------------------------------------------------------------
;; Server side

#| ;; for debugging
(defun show-server-outbound (socket)
  (actor (&rest msg)
    (send println (format nil "s/out: ~S" msg))
    (send* socket msg)))

(defun show-server-inbound ()
  (actor (cust &rest msg)
    (send println (format nil "s/in: ~S" msg))
    (send* cust msg)))
|#

(defun server-crypto-gateway (server-skey socket local-services)
  ;; Foreign clients first make contact with us here. They send us
  ;; their public key and a random ECC point. We develop a unique DHE
  ;; encryption key shared secretly between us and furnish a private handler
  ;; for encrypted requests along with our own random ECC point.
  (actor (cust-id server-pkey client-pkey apt)
    (let ((my-pkey     (ed-nth-pt server-skey))
          (server-pkey (ed-decompress-pt server-pkey)))
      (when (ed-pt= my-pkey server-pkey) ;; did client have correct server-pkey?
        (let* ((brand     (int (ctr-drbg 256)))
               (bpt       (ed-nth-pt brand))
               (ekey      (hash/256 (ed-mul (ed-decompress-pt apt) brand)))
               ;; (socket    (show-server-outbound socket))  ;; ***
               (encryptor (secure-sender ekey server-skey))
               (chan      (server-channel
                           :socket      socket
                           :encryptor   encryptor))
               (decryptor (sink-pipe
                           (secure-reader ekey (ed-decompress-pt client-pkey))
                           ;; (show-server-inbound) ;; ***
                           chan)))
          (beta (id)
              (create-service-proxy beta local-services decryptor)
            (send (remote-actor-proxy cust-id socket)  ;; remote client cust
                  id (int bpt)))
          )))))

(defun server-channel (&key
                       socket
                       encryptor)
  ;; This is a private portal for exchanges with a foreign client.
  ;; One of these exist for each connection established through the
  ;; main crypto gate.
  ;;
  ;; We authenticate requests as coming from the client public key,
  ;; decrypt the requests, and pass along to a local service. For each
  ;; request we make an encrypting forwarder back to the client
  ;; customer, and pass that along as the local customer for the
  ;; request to the local service.
  (make-actor
   (alambda
    ;; A significant difference between LAMBDA and ALAMBDA - if an
    ;; incoming arg list does not match what LAMBDA expects, it
    ;; produces an error. ALAMBDA uses pattern matching, and anything
    ;; arriving that does not match is simply ignored.
    ((cust-id verb . msg) ;; remote client cust
     ;; (send println (format nil "server rec'd req: ~S" self-msg))
     (let ((proxy (when cust-id
                    (sink-pipe encryptor (remote-actor-proxy cust-id socket)))))
       (send* (global-services) proxy :send verb msg)))
    )))

;; ---------------------------------------------------------------

#|
(multiple-value-bind (skey pkey)
    (make-deterministic-keys +server-id+)
  (with-standard-io-syntax
    (format t "~%skey: #x~x" skey)
    (format t "~%pkey: #x~x" (int pkey))))
|#

;; -----------------------------------------------
;; Simple Services

(defun make-echo ()
  (actor (cust msg)
    ;; (send println (format nil "echo got: ~S" msg))
    (send cust msg)))

(defun cmpfn (&rest args)
  (compile nil `(lambda ()
                  ,@args)))

(defun make-eval ()
  (actor (cust form)
    (send cust (funcall (cmpfn form)))))

(defun make-avail ()
  (actor (cust)
    (send (global-services) cust :available-services nil)))

;; -----------------------------------------------

(defun make-initial-global-services ()
  (send* (actor (&rest svcs)
           (when svcs
             (let ((me  self))
               (beta _
                   (send* (global-services) beta :add-service (car svcs))
                 (send* me (cdr svcs))
                 ))))
         `((:echo               ,(make-echo))
           (:eval               ,(make-eval))
           (:available-services ,(make-avail)))
         ))

(defun start-server-gateway ()
  (setf *client-gateway*  nil
        *global-services* nil)
  (make-initial-global-services))
  
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

 |#

