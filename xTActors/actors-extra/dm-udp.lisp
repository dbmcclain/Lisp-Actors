;; dm-udp.lisp
;;
;; Async UDP Connections in Actors - initial coding kindly provided by
;; Camille Troillard. Extended to support typical client/server
;; architectures, and self-sync encoding, by DM/RAL 2022/11/21
;; 10:18:20.
;; -- DM/RAL  2022/11/22 18:44:52 Remove Self-Sync encode/decode
;;    Interface transmits and receives UB8 vectors.
;;
;; DM/RAL  2022/11/21 06:36:03
;; ----------------------------------
;;
;; Unlike TCP/IP Sockets, UDP has a clear distinction between Client
;; and Server roles. Clients send messages of limited size to a UDP
;; server. Any number of different clients can do so, to the same
;; Server socket. On the Server side, we have to know who sent the
;; message so we can respond to them.
;;
;; So there becomes a fundamental asymmetry in the connection. Clients
;; already know who they are sending UDP messages to. Servers can
;; receive from multiple Clients. And each message carries with it the
;; identity (ip-addr, port) of the sender of the message.
;;
;; This UDP interface expects both Client and Server to send messages
;; that are 8-bit simple vectors. The "safe" size limit is around 512
;; bytes. This particular interace allows up to 1500 bytes total
;; message size. Apple Mac OSX sets an absolute max size of 9216
;; bytes. But the code here limits it to 1500 bytes.
;;

(defpackage #:dm-udp
  (:use #:common-lisp #:ac)
  (:import-from #:vec-repr
   #:ub8-vector
   #:make-ub8-vector)
  (:export
   #:udp-srv-port-beh
   #:udp-cli-connected-port-beh
   ))

(in-package #:dm-udp)

;; ----------------------------------
#|
(defvar *default-wait-state-collection* nil)
(defvar *ws-coll-lock*                  (mpc:make-lock))

(defun get-ws-coll ()
  (or *default-wait-state-collection*
      (mpc:with-lock (*ws-coll-lock*)
        (or *default-wait-state-collection*
            (setf *default-wait-state-collection*
                  (comm:create-and-run-wait-state-collection (package-name *package*)))
            ))
      ))
|#

(defparameter +udp-max-receive-size+ 1500)

(defun ws-coll-beh ()
  (lambda (cust)
    (let ((coll (comm:create-and-run-wait-state-collection "UDP")))
      (become (const-beh coll))
      (send cust coll))
    ))

(deflex ws-coll  (create (ws-coll-beh)))
  
;; -----------------------------------------------------------------------
;; Server Side using raw socket

(defun udp-srv-port-beh (&key
                         (ipv6 nil)
                         local-address local-port
                         (receive-actor (error "RECEIVE-ACTOR must not be NIL")))
  (alambda
   ((cust :become-srv beh)
    (become beh)
    (send cust :ok))

   ((cust :init)
    (let++ ((me       self)
            (:β  coll ws-coll)
            (io-state (comm:create-async-io-state-and-udp-socket
                       coll
                       :local-address local-address
                       :local-port    local-port
                       :ipv6          ipv6)))
      (labels
          ((udp-srv-port-receive-next (buffer)
             (comm:async-io-state-receive-message io-state
                                                  buffer
                                                  #'udp-srv-port-process-message
                                                  :needs-address t))
           
           (udp-srv-port-process-message (io-state buffer number-of-bytes-read from-ip from-port)
             (declare (ignore io-state))
             (send receive-actor from-ip from-port (subseq buffer 0 number-of-bytes-read))
             (udp-srv-port-receive-next buffer)))
        
        (udp-srv-port-receive-next (make-ub8-vector +udp-max-receive-size+))
        (send me cust :become-srv
              (alambda
               ((cust :close)
                (comm:async-io-state-abort-and-close io-state
                                                     :close-callback (λ _ (send cust :closed))))
               ((cust to-ip to-port message)
                (comm:async-io-state-send-message-to-address io-state
                                                             to-ip to-port message
                                                             (λ _ (send cust :sent to-ip to-port message))))
               ))
        )))
   ))

;; -----------------------------------------------------------------------
;; Client Side using connected socket - uses existing Self-Sync stream
;; interface.

(defun udp-cli-connected-port-beh (hostspec service &key
                                            (ipv6 nil)
                                            local-address local-port
                                            receive-actor)
  (alambda
   ((cust :become-cli msg beh)
    (become beh)
    (send* self msg)
    (send cust :ok))
   
   (msg
    (let++ ((me       self)
            (:β coll  ws-coll)
            (io-state (comm:create-async-io-state-and-connected-udp-socket
                       coll
                       hostspec service
                       :local-address local-address
                       :local-port    local-port
                       :ipv6          ipv6)))
      (labels
          ((udp-cli-port-receive-next (buffer)
             (comm:async-io-state-receive-message io-state
                                                  buffer
                                                  #'udp-cli-port-process-message))
           (udp-cli-port-process-message (io-state buffer number-of-bytes-read)
             (declare (ignore io-state))
             (send receive-actor (subseq buffer 0 number-of-bytes-read))
             (udp-cli-port-receive-next buffer)))
        
        (when receive-actor
          ;; how useful would it be to elide receive-actor?
          (udp-cli-port-receive-next (make-ub8-vector +udp-max-receive-size+)))
        (send me sink :become-cli msg 
              ;; provide sender beh
              (alambda
               ((cust :close)
                (comm:async-io-state-abort-and-close io-state
                                                     :close-callback (λ _ (send cust :closed))))
               ((cust message)
                (comm:async-io-state-send-message io-state message
                                                  (λ _ (send cust :sent message))))
               ))
        )))
   ))

;; -------------------------------------------------------------
#|
;; --- Example UDP Server ---
(deflex srv-sender
  (actors
      ((sender   (udp-srv-port-beh :local-port 65002
                                   :receive-actor handler))
       (handler  (lambda (from-ip from-port message)
                   (send fmt-println "Srv: from: ~S @ ~S (~D) ~S"
                         from-port from-ip (length message) message)
                   (send sender println from-ip from-port
                         (map 'ub8-vector #'char-code "OK")))))
    (send sender sink :init)
    sender))

;; Terminate the connection from Server side
(send srv-sender println :close)

;; -------------------------------------

;; --- Example UDB Client ---
(deflex cli-sender
  (actors ((sender  (udp-cli-connected-port-beh "fornax.local" #|"localhost"|# 65002
                                            :receive-actor handler))
           (handler (lambda (message)
                      (send fmt-println "Client RX: (~D) ~S" (length message) message))))
    sender))

(send cli-sender println (make-ub8-vector 3
                                          :initial-contents '(1 2 3)))

;; Terminate the connection from Client side
(send cli-sender println :close)

;; ------------------------------------------------------------
;; Max transmissible message appears to be 1500 bytes.

(send cli-sender println
      (subseq (map 'ub8-vector #'char-code (hcl:file-string "port-server.lisp"))
              0 1500))



;; By increasing max size to 64kB we can reach 9216 byte messages,
;; This appears to be a Mac limit.
(send cli-sender println
      (subseq (map 'ub8-vector #'char-code (hcl:file-string "port-server.lisp"))
              0 9202))

;; ------------------------------------------------------------
;;
(deflex cli-sender
  (actors ((sender  (udp-cli-connected-port-beh "fornax.local" 65301
                                            :receive-actor handler))
           (handler (lambda (message)
                      (send fmt-println "Client RX: (~D) ~S"
                            (length message) message))))
    sender))

(send cli-sender println (make-ub8-vector 1
                                          :initial-contents '(1)))
(send cli-sender println :close)
|#

(with-open-stream (http (comm:open-tcp-stream 
                         "fornax.local" 5555)) 
  (write-byte 5 http)
  (format http "Hello")
  (force-output http)
  (write-string "Waiting for reply...")
  (loop for ch = (read-char-no-hang http nil :eof)
        until ch
        do (write-char #\.)
           (sleep 0.25)
        finally (unless (eq ch :eof)
                  (unread-char ch http))) 
  (terpri)
  (loop for line = (read-line http nil nil)
        while line
        do (write-line line)))
