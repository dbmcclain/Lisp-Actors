;; dm-udp.lisp
;;
;; Async UDP Connections in Actors - initial coding kindly provided by
;; Camille Troillard. Extended to support typical client/server
;; architectures, and self-sync encoding, by DM/RAL 2022/11/21
;; 10:18:20.
;;
;; DM/RAL  2022/11/21 06:36:03
;; ----------------------------------

(defpackage #:dm-udp
  (:use #:common-lisp #:ac))

(in-package #:dm-udp)

(um:eval-always
  (import '(vec-repr:ub8
            vec-repr:ub8-vector
            vec-repr:make-ub8-vector

            self-synca:+long-count-base+
            self-synca:+max-short-count+
            self-synca:+max-long-count+
            self-synca:vec-le4-to-int
            self-synca:crc32
            )))

;; ----------------------------------

(defvar *default-wait-state-collection*
 (comm:create-and-run-wait-state-collection (package-name *package*)))

(defparameter +udp-max-receive-size+ 1500)

(defstruct user-cnx-state
  actor
  (in-counter 0))

;; -----------------------------------------------------------------------
;; Self-Sync Decoding for UDP Server Ports
;;
;; These have incoming message, serialized at the receiver, but also
;; augmented with ip-addr and ip-port of sender. We need to pass that
;; identifying information along to the ultimate handler.

(defun udp-srv-ssfsm-beh (dest aout stuff-fn)
  #F
  (declare (function stuff-fn)
           ((ub8-vector *) aout))
  (let (state          ;; machine state function
        need-fefd      ;; when T we may need to insert #xFEFD
        (remct 0)      ;; segment bytes remaining
        crcv           ;; copy of 4-byte CRC field
        lenv           ;; copy of 4-byte length field
        senders-ip     ;; sender's ip-addr
        senders-port   ;; sender's ip-port
        (nel   -1))    ;; expected message length
    (declare (fixnum remct nel))
    (macrolet ((new-state (fn)
                 `(setf state #',fn)))
      (labels (;; --------------------
               ;; Utility Functions
               (subrange-code? (b)
                 (declare (ub8 b))
                 (<= 0 b #xFC))
               
               (stuffer-init (ct)
                 (declare (fixnum ct))
                 (setf nel       -1
                       remct     ct
                       need-fefd (< ct +max-short-count+)
                       (fill-pointer aout) 0)
                 (new-state read-segm))

               (segm-init (ct)
                 (declare (fixnum ct))
                 (setf remct     ct
                       need-fefd (< ct +max-long-count+))
                 (new-state read-segm)
                 (check-finish))

               (raw-stuff (b)
                 (funcall stuff-fn b aout))
               
               (stuff (b)
                 (raw-stuff b)
                 (decf remct)
                 (check-finish))

               (check-finish ()
                 (when (zerop remct)
                   (when (minusp nel)
                     (setf crcv (subseq aout 0 4)
                           lenv (subseq aout 4 8)
                           nel  (+ 8 (vec-le4-to-int lenv))))
                   (let ((nbuf  (length aout)))
                     (declare (fixnum nbuf))
                     (when (and need-fefd
                                (< nbuf nel))
                       (raw-stuff #xFE)
                       (raw-stuff #xFD)
                       (incf nbuf 2))
                     (cond ((< nbuf nel)
                            (new-state read-long-count))
                           (t
                            (new-state start)
                            (let* ((ans (subseq aout 8))
                                   (chk (crc32 lenv ans)))
                              (when (equalp crcv chk)
                                (send dest senders-ip senders-port ans))
                              ))
                           ))))

               (restart (b)
                 (new-state start)
                 (start b))

               (inhale (b)
                 (funcall state b))
               
               ;; ----------------
               ;; Machine States
               (start (b)
                 (declare (ub8 b))
                 (when (eql b #xFE)
                   (new-state check-start-fd)))
               
               (check-start-fd (b)
                 (declare (ub8 b))
                 (if (eql b #xFD)
                     (new-state check-version)
                   (restart b)))

               (check-version (b)
                 (declare (ub8 b))
                 (if (eql b #x01)
                     (new-state read-short-count)
                   (restart b)))

               (read-short-count (b)
                 (if (subrange-code? b)
                     (stuffer-init b)
                   (restart b)))

               (read-segm (b)
                 (declare (ub8 b))
                 (if (and (eql b #xFE)
                          (> remct 1))
                     (new-state check-segm-fd)
                   (stuff b)))

               (check-segm-fd (b)
                 (declare (ub8 b))
                 (cond ((eql b #xFD) ;; we just saw a start pattern #xFE #xFD
                        (new-state check-version))
                       (t
                        (stuff #xFE)
                        (new-state read-segm)
                        (read-segm b))
                       ))

               (read-long-count (b)
                 (declare (ub8 b))
                 (cond ((subrange-code? b)
                        (setf remct b)
                        (new-state read-long-count-2))
                       (t
                        (restart b))
                       ))

               (read-long-count-2 (b)
                 (declare (ub8 b))
                 (if (subrange-code? b)
                     (segm-init (+ remct (* b +long-count-base+)))
                   (restart b))))
        
        (new-state check-version) ;; initialize state
        
        ;; finally... we get to the Actor behavior function
        (lambda (cust from-ip from-port buf)
          (declare ((array ub8 *) buf))
          (setf senders-ip   from-ip
                senders-port from-port)
          (map nil #'inhale buf)
          (send cust :next))
        ))))

(defun udp-srv-decoder-fsm (dest)
  (create (udp-srv-ssfsm-beh dest (make-ub8-vector 256
                                                   :fill-pointer 0
                                                   :adjustable   t)
                             #'vector-push-extend)))

;; ------------------------------------------------------

(defun udp-srv-stream-decoder-beh (fsm wait-ix queue)
  (alambda
   ((:deliver bufix from-ip from-port message)
    (cond ((eql bufix wait-ix)
           (send fsm self from-ip from-port message)
           (become (udp-srv-busy-stream-decoder-beh fsm (1+ bufix) queue)))
          
          (t
           (become (udp-srv-stream-decoder-beh
                    fsm wait-ix
                    (cons (list bufix from-ip from-port message) queue))))
          ))
   ))

(defun udp-srv-busy-stream-decoder-beh (fsm wait-ix queue)
  (alambda
   ((:next)
    (let ((lst (assoc wait-ix queue)))
      (cond (lst
             (destructuring-bind (bufix from-ip from-port message)
                 lst
               (declare (ignore bufix))
               (send fsm self from-ip from-port message)
               (become (udp-srv-busy-stream-decoder-beh fsm (1+ wait-ix) (remove lst queue)))
               ))
            (t
             (become (udp-srv-stream-decoder-beh fsm wait-ix queue)))
            )))

   ((:deliver bufix from-ip from-port message)
    (become (udp-srv-busy-stream-decoder-beh fsm wait-ix
                                             (cons (list bufix from-ip from-port message) queue))))
   ))

(defun udp-srv-self-sync-stream-decoder (dest)
  (let ((fsm (udp-srv-decoder-fsm dest)))
    (create (udp-srv-stream-decoder-beh fsm 1 nil))))

;; -----------------------------------------------------------------------
;; Server Side using raw socket

(defun udp-srv-port-process-message (io-state buffer number-of-bytes-read from-ip from-port)
  (let ((cnx-state (comm:async-io-state-user-info io-state)))
    (send (user-cnx-state-actor cnx-state)
          :deliver
          (incf (user-cnx-state-in-counter cnx-state))
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

(defun udp-srv-port-beh (io-state encoder)
  ;; Sending side for Server
  (alambda
   ((cust :dispose)
    (comm:async-io-state-abort-and-close io-state
                                         :close-callback (λ _ (send cust :closed))))
   ((cust to-ip to-port message)
    (comm:async-io-state-send-message-to-address io-state
                                                 to-ip to-port
                                                 (funcall encoder message)
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
                  :user-info (make-user-cnx-state
                              :actor (udp-srv-self-sync-stream-decoder receive-actor)))))
   (udp-srv-port-start-receive io-state)
   (create (udp-srv-port-beh io-state #'self-synca:encode))))

;; -----------------------------------------------------------------------
;; Client Side using connected socket

(defun udp-cli-port-process-message (io-state buffer number-of-bytes-read)
  (let ((cnx-state (comm:async-io-state-user-info io-state)))
    (send (user-cnx-state-actor cnx-state)
          :deliver
          (incf (user-cnx-state-in-counter cnx-state))
          (subseq buffer 0 number-of-bytes-read))
   (udp-cli-port-receive-next io-state buffer)))

(defun udp-cli-port-receive-next (io-state buffer)
 (comm:async-io-state-receive-message io-state
                                      buffer
                                      #'udp-cli-port-process-message))

(defun udp-cli-port-start-receive (io-state)
 (udp-cli-port-receive-next io-state
                            (make-array +udp-max-receive-size+
                                        :element-type '(unsigned-byte 8))))

(defun udp-cli-port-beh (io-state encoder)
  ;; Sending side for Client
 (alambda
  ((cust :dispose)
   (comm:async-io-state-abort-and-close io-state
                                        :close-callback (λ _ (send cust :closed))))
  ((cust message)
   (comm:async-io-state-send-message io-state
                                     (funcall encoder message)
                                     (λ _ (send cust :sent message))))
  ))

(defun udp-cli-connected-port (hostspec service &key
                                   (ipv6 nil)
                                   (encoder #'self-synca:encode)
                                   local-address local-port
                                   receive-actor)
 (let ((io-state (comm:create-async-io-state-and-connected-udp-socket
                  *default-wait-state-collection*
                  hostspec service
                  :local-address local-address
                  :local-port local-port
                  :ipv6 ipv6
                  :user-info (make-user-cnx-state
                              :actor (self-synca:stream-decoder receive-actor)))))
   (when receive-actor
     (udp-cli-port-start-receive io-state))
   (create (udp-cli-port-beh io-state encoder))))

;; -------------------------------------------------------------
#|
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
                 :receive-actor srv-handler)))

(send srv-sender println :dispose)

;; -------------------------------------

(deflex cli-handler
  ;; Receiving side for Client
  (create
   (lambda (message)
     (send fmt-println "Client RX: ~S" message))
   ))

(deflex cli-sender
  ;; Sending side for Client
  (udp-cli-connected-port "localhost" 65200
                          :receive-actor cli-handler))

(send cli-sender println (make-ub8-vector 3
                                   :initial-contents '(1 2 3)))

(send cli-sender println :dispose)

;; ------------------------------------------------------------
;; Max transmissible message appears to be 1485 bytes, which has a
;; Self-Sync encoding length of 1499 bytes.
(send cli println
      (subseq (map 'ub8-vector #'char-code (hcl:file-string "port-server.lisp"))
              0 1485))

(length (self-synca:encode
         (subseq (map 'ub8-vector #'char-code (hcl:file-string "port-server.lisp"))
                 0 1485)))


;; By increasing max size to 64kB we can reach 9202 byte messages,
;; with self-sync encoding length of 9216 bytes. This appears to be a
;; Mac limit.
(send cli println
      (subseq (map 'ub8-vector #'char-code (hcl:file-string "port-server.lisp"))
              0 9202))

(length (self-synca:encode
         (subseq (map 'ub8-vector #'char-code (hcl:file-string "port-server.lisp"))
                 0 9202)))


|#
