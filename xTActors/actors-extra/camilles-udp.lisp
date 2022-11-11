;; camilles-udp.lisp
;;
;; Async UDP Connections in Actors - kindly provided by Camille Troillard.
;;
;; DM/RAL  2022/11/10 18:26:35
;; ----------------------------------

(defpackage #:camilles-udp
  (:use #:common-lisp #:ac))

(in-package #:camilles-udp)

;; ----------------------------------

(defvar *default-wait-state-collection*
 (comm:create-and-run-wait-state-collection (package-name *package*)))

(defparameter +udp-max-receive-size+ 1500)

(defun udp-port-process-message (io-state buffer number-of-bytes-read)
 (let ((actor (comm:async-io-state-user-info io-state)))
   (send actor :message (subseq buffer 0 number-of-bytes-read))
   (udp-port-receive-next io-state buffer)))

(defun udp-port-receive-next (io-state buffer)
 (comm:async-io-state-receive-message io-state
                                      buffer
                                      #'udp-port-process-message))

(defun udp-port-beh (io-state encoder)
 (alambda
  ((cust :dispose)
   (comm:async-io-state-abort-and-close io-state
                                        :close-callback (λ _ (send cust :closed))))
  ((cust message)
   (comm:async-io-state-send-message io-state
                                     (funcall encoder message)
                                     (λ _ (send cust :sent message))))))

(defun udp-port-start-receive (io-state)
 (udp-port-receive-next io-state
                        (make-array +udp-max-receive-size+
                                    :element-type '(unsigned-byte 8))))

(defun udp-port (&key
                (ipv6 nil)
                local-address local-port
                (receive-actor (error "RECEIVE-ACTOR must not be NIL")))
 (let ((io-state (comm:create-async-io-state-and-udp-socket
                  *default-wait-state-collection*
                  :local-address local-address
                  :local-port local-port
                  :ipv6 ipv6
                  :user-info receive-actor)))
   (udp-port-start-receive io-state)
   (create (udp-port-beh io-state #'identity))))

(defun udp-connected-port (hostspec service &key
                                   (ipv6 nil)
                                   (encoder #'identity)
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
     (udp-port-start-receive io-state))
   (create (udp-port-beh io-state encoder))))

