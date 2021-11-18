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

(defun server-crypto-gate (server-skey)
  ;; Foreign clients first make contact with us here. They send us
  ;; their public key and a random ECC point. We develop a unique DHE
  ;; encryption key shared secretly between us and furnish a private handler
  ;; for encrypted requests along with our own random ECC point.
  (make-actor
   (alambda
    ((:connect cust-id socket server-pkey client-pkey apt)
     (let ((my-pkey     (ed-mul *ed-gen* server-skey))
           (server-pkey (ed-decompress-pt server-pkey)))
       (when (ed-pt= my-pkey server-pkey) ;; did client have correct server-pkey?
         (let* ((brand     (int (ctr-drbg 256)))
                (bpt       (ed-mul *ed-gen* brand))
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
               (create-service-proxy beta decryptor)
             (send (server-side-client-proxy cust-id socket)  ;; remote client cust
                   id (int bpt)))
           ))))
    )))

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
    ((cust-id :available-services)
     (let ((proxy (server-side-client-proxy cust-id socket)))
       (send (global-services) (sink-pipe encryptor proxy) :available-services nil)))
    
    ((cust-id verb . msg) ;; remote client cust
     ;; (send println (format nil "server rec'd req: ~S" self-msg))
     (let ((proxy (server-side-client-proxy cust-id socket)))
       (send* (global-services) (sink-pipe encryptor proxy) :send verb msg)))
    )))

;; ---------------------------------------------------------------

(defvar *server-gateway* nil) ;; this can be shared

#|
(multiple-value-bind (skey pkey)
    (make-deterministic-keys *server-id*)
  (with-standard-io-syntax
    (format t "~%skey: #x~x" skey)
    (format t "~%pkey: #x~x" (int pkey))))
|#

(defun server-gateway ()
  (or *server-gateway*
      (setf *server-gateway*
            (server-crypto-gate *server-skey*))
      ))

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

;; -----------------------------------------------

(defun make-initial-global-services ()
  (send* (actor (&rest svcs)
           (when svcs
             (let ((me  self))
               (beta _
                   (send* (global-services) beta :add-service (car svcs))
                 (send* me (cdr svcs))
                 ))))
         `((:echo ,(make-echo))
           (:eval ,(make-eval)))
         ))

(defun start-server-gateway ()
  (setf *server-gateway*  nil
        *client-gateway*  nil
        *local-services*  nil
        *global-services* nil)
  (make-initial-global-services)
  (server-gateway))
  
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

(defun chunker-outp ()
  ;; expects general objects
  ;; produces a byte stream
  (pipe (marshal-encoder)
        (chunker :max-size 65000)
        (marshal-encoder)))

(defun dechunker-inp ()
  ;; expects a byte stream
  ;; supplies general objecst
  (pipe (marshal-decoder)
        (dechunker)
        (marshal-decoder)))

(defun ether ()
  (actor (cust &rest msg)
    (send* cust msg)))

(defun channel ()
  (pipe (chunker-outp)
        (ether)
        (dechunker-inp)))

(defun fake-client-to-server ()
  (sink-pipe (channel)
             (fake-server)))

(defun fake-server-to-client ()
  (sink-pipe (channel)
             (fake-client)))

(defun fake-server ()
  ;; Uses a shared *local-services* between fake client and fake
  ;; server.  But that's okay because all entries have unique ID's.
  (make-actor
   (alambda
    ((cust :connect . msg)
     (send* (server-gateway) cust :connect self msg))
    
    ((cust :send . msg)
     (send* (local-services) cust :send msg))
    
    ((cust :reply . msg)
     (send* (fake-server-to-client) cust :reply msg))
    )))

(defun fake-client ()
  ;; Uses a shared *local-services* between fake client and fake
  ;; server.  But that's okay because all entries have unique ID's.
  (let ((fake-cnx (fake-client-to-server)))
    (make-actor
     (alambda
      ((cust :connect . msg)
       (send* fake-cnx cust :connect msg))
       
      ((cust :send . msg)
       (send* fake-cnx cust :send msg))

      ((cust :reply . msg)
       (send* (local-services) cust :send msg))
      ))))
     
(defun make-mock-service ()
  (actor ()
    (let ((client-socket (fake-client)))
      (start-server-gateway)
      (beta _
          (send (global-services) beta :add-service :println println)
        (beta _
            (send (global-services) beta :add-service :writeln writeln)
          (send (make-connection client-socket)))))
    ))
    
(defun make-connection (client-socket)
  (actor ()
    (multiple-value-bind (client-skey client-pkey)
        (make-deterministic-keys :client)
      
      (beta (cnx)
          (send (client-gateway) beta :connect client-socket *server-pkey*)
        (beta (ans)
            (send cnx beta :available-services)
          (send println (format nil "from server: Services: ~S" ans))
          (beta _
              (send (global-services) beta :remove-service :println)
            (beta _
                (send (global-services) beta :remove-service :writeln))
            )))
      )))

(defun tst ()
  (send (make-mock-service)))

(send (logged (make-mock-service)))

(atrace)
(atrace nil)
(tst)
(send (local-services) writeln :list nil)
(send (global-services) writeln :list nil)
(setf *dbg* t)
(setf *dbg* nil)

 |#

