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
  (:use #:common-lisp #:ac))

(in-package #:dm-udp)

(um:eval-always
  (import '(vec-repr:ub8-vector
            vec-repr:make-ub8-vector
            )))

;; ----------------------------------

(defvar *default-wait-state-collection*
 (comm:create-and-run-wait-state-collection (package-name *package*)))

(defparameter +udp-max-receive-size+ 1500)

;; -----------------------------------------------------------------------
;; Server Side using raw socket

(defun udp-srv-port-process-message (io-state buffer number-of-bytes-read from-ip from-port)
  (let ((actor (comm:async-io-state-user-info io-state)))
    (send actor
          from-ip from-port
          (subseq buffer 0 number-of-bytes-read))
    (udp-srv-port-receive-next io-state buffer)))

(defun udp-srv-port-receive-next (io-state buffer)
 (comm:async-io-state-receive-message io-state
                                      buffer
                                      #'udp-srv-port-process-message
                                      :needs-address t))

(defun udp-srv-port-start-receive (io-state)
 (udp-srv-port-receive-next io-state
                            (make-array +udp-max-receive-size+
                                        :element-type '(unsigned-byte 8))))

(defun udp-srv-port-beh (io-state)
  ;; Sending side for Server
  (alambda
   ((cust :dispose)
    (comm:async-io-state-abort-and-close io-state
                                         :close-callback (λ _ (send cust :closed))))
   ((cust to-ip to-port message)
    (comm:async-io-state-send-message-to-address io-state
                                                 to-ip to-port message
                                                 (λ _ (send cust :sent to-ip to-port message))))
   ))

(defun udp-srv-port (&key
                (ipv6 nil)
                local-address local-port
                (receive-actor (error "RECEIVE-ACTOR must not be NIL")))
 (let ((io-state (comm:create-async-io-state-and-udp-socket
                  *default-wait-state-collection*
                  :local-address local-address
                  :local-port local-port
                  :ipv6 ipv6
                  :user-info receive-actor)))
   (udp-srv-port-start-receive io-state)
   (create (udp-srv-port-beh io-state))
   ))

;; -----------------------------------------------------------------------
;; Client Side using connected socket - uses existing Self-Sync stream
;; interface.

(defun udp-cli-port-process-message (io-state buffer number-of-bytes-read)
  (let ((actor (comm:async-io-state-user-info io-state)))
    (send actor (subseq buffer 0 number-of-bytes-read))
   (udp-cli-port-receive-next io-state buffer)))

(defun udp-cli-port-receive-next (io-state buffer)
 (comm:async-io-state-receive-message io-state
                                      buffer
                                      #'udp-cli-port-process-message))

(defun udp-cli-port-start-receive (io-state)
 (udp-cli-port-receive-next io-state
                            (make-array +udp-max-receive-size+
                                        :element-type '(unsigned-byte 8))))

(defun udp-cli-port-beh (io-state)
  ;; Sending side for Client
 (alambda
  ((cust :dispose)
   (comm:async-io-state-abort-and-close io-state
                                        :close-callback (λ _ (send cust :closed))))
  ((cust message)
   (comm:async-io-state-send-message io-state message
                                     (λ _ (send cust :sent message))))
  ))

(defun udp-cli-connected-port (hostspec service &key
                                   (ipv6 nil)
                                   local-address local-port
                                   receive-actor)
 (let ((io-state (comm:create-async-io-state-and-connected-udp-socket
                  *default-wait-state-collection*
                  hostspec service
                  :local-address local-address
                  :local-port local-port
                  :ipv6 ipv6
                  :user-info receive-actor)))
   (when receive-actor
     (udp-cli-port-start-receive io-state))
   (create (udp-cli-port-beh io-state))
   ))

;; -------------------------------------------------------------
#|
(progn
  ;; --- Example UDP Server ---
  (deflex srv-sender (create))
  
  (deflex srv-handler
    ;; Receiving side for Server
    (create
     (lambda (from-ip from-port message)
       (send fmt-println "Srv: from: ~S @ ~S (~D) ~S"
             from-port from-ip (length message) message)
       (send srv-sender println from-ip from-port (map 'ub8-vector #'char-code "OK")))
     ))

  (define-behavior srv-sender
    ;; Sending side for Server
    (actor-beh
     (udp-srv-port :local-port    65200
                   :receive-actor srv-handler))))

;; Terminate the connection from Server side
(send srv-sender println :dispose)

;; -------------------------------------

(progn
  ;; --- Example UDB Client ---
  (deflex cli-handler
    ;; Receiving side for Client
    (create
     (lambda (message)
       (send fmt-println "Client RX: ~S" message))
     ))
  
  (deflex cli-sender
    ;; Sending side for Client
    (udp-cli-connected-port "localhost" 65200
                            :receive-actor cli-handler)))
  
(send cli-sender println (make-ub8-vector 3
                                   :initial-contents '(1 2 3)))

;; Terminate the connection from Client side
(send cli-sender println :dispose)

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

|#
