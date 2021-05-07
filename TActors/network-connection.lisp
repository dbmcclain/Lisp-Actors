;; bfly-socket.lisp
;; --------------------------------------------------------------------------------------
;; Butterfly -- a system for easy distributed computing, going beyond what is available
;; in Erlang with the full power of Common Lisp.
;;
;; Copyright (C) 2008,2009 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08, 06-12/09
;; --------------------------------------------------------------------------------------

(in-package #:actors/network)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import '(actors/security:random))
  (import '(um:if-let
            um:when-let
            um:dlambda
            um:dlambda*
            um:dcase
            um:dcase*
            um:nlet
            um:capture-ans-or-exn
            um:wr
            
            actors/security:secure-encoding
            actors/security:secure-decoding
            actors/security:byte-decode-obj
            actors/security:crypto
            actors/security:make-u8-vector
            actors/security:convert-vector-to-integer
            actors/security:+MAX-FRAGMENT-SIZE+
            actors/security:assemble-sks
            actors/security:time-to-renegotiate?
            
            actors/bridge:bridge-register
            actors/bridge:bridge-pre-register
            actors/bridge:bridge-unregister
            ;; actors/bridge:bridge-handle-reply
            actors/bridge:bridge-reset
            actors/bridge:bridge-deliver-message

            actors/lfm:ensure-system-logger
            actors/lfm:kill-system-logger

            scatter-vec:scatter-vector
            scatter-vec:add-fragment

            finger-tree:addq
            finger-tree:popq
            finger-tree:pushq
            #|
            #-:USING-ECC-CRYPTO actors/srp6-rsa:server-negotiate-security-rsa
            #-:USING-ECC-CRYPTO actors/srp6-rsa:client-negotiate-security-rsa

            #+:USING-ECC-CRYPTO actors/srp6-ecc:client-negotiate-security-ecc
            #+:USING-ECC-CRYPTO actors/srp6-ecc:server-negotiate-security-ecc
            |#
            )))

;; -----------------------------------------------------------------------

(defvar *default-port*            65001)
(defvar *socket-timeout-period*   20)
(defvar *ws-collection*           nil)
(defvar *aio-accepting-handle*    nil)

(defconstant +using-ssl+          t)

;; -------------------------------------------------------------

(defstruct intf-state
  title
  io-state
  accepting-handle
  crypto
  writer
  dispatcher
  kill-timer
  (io-running (list 1))
  decr-io-count-fn
  intf)

;; -------------------------------------------------------------
;; For link debugging...

(defvar *watch-input*  nil)
(defvar *watch-output* nil)

(defun watch-io (t/f)
  (setf *watch-input*  t/f
        *watch-output* t/f))

(defun watch-input (data)
  (when *watch-input*
    (let ((*watch-input* nil)) ;; in case logging is remote
      (log-info :SYSTEM-LOG (format nil "INP: ~A" data)))))

(defun watch-output (data)
  (when *watch-output*
    (let ((*watch-output* nil)) ;; in case logging is remote
      (log-info :SYSTEM-LOG (format nil "OUT: ~A" data)))))

;; -------------------------------------------------------------
;; Channel Handler
;; ----------------------------------------------------------------

(defmacro expect (intf &rest clauses)
  `(do-expect ,intf (make-expect-handler ,@clauses)))

(defun unexpected (msg)
  (error "Unexpected Message: ~A" msg))

(defmacro make-expect-handler (&rest clauses)
  `(dlambda*
     ,@clauses
     ,@(unless (find 't clauses
                     :key 'first)
         `((t (&rest msg)
              (unexpected msg)))
         )))
  
#+:LISPWORKS
(editor:setup-indent "expect" 1)

;; -----------------------------------------------------------
;; Input Buffer Control

(defun extract-bytes (queue buf start end)
  ;; extract fragments as needed to fill a buffer
  (if (null queue)
      (values start nil)
    (multiple-value-bind (frag new-queue) (popq queue)
      (destructuring-bind (frag-start frag-end . frag-bytes) frag
        ;; while there are still some fragments
        (let ((nb   (- frag-end frag-start))
              (need (- end start)))
          (cond
           ((< nb need)
            ;; fragment is short of what we need
            ;; so take what it has and ask for more
            (when (plusp nb)
              (replace buf frag-bytes
                       :start1 start
                       :start2 frag-start)
              (extract-bytes new-queue buf (+ start nb) end)))
             
           ((= nb need)
            ;; fragment has exactly what we need
            (when (plusp need)
              (replace buf frag-bytes
                       :start1 start
                       :start2 frag-start))
            (values end new-queue))
             
           (t ;; (> nb need)
              ;; fragment has more than needed. Take what we need
              ;; and put back the rest.
              (when (plusp need)
                (replace buf frag-bytes
                         :start1 start
                         :start2 frag-start
                         :end1   end)
                (values end
                        (pushq new-queue
                               (list* (+ frag-start need)
                                      frag-end frag-bytes))
                        )))
           ))))
    ))
       
(defun drain-buffer (custs frags)
  (cond (custs
         (multiple-value-bind (rec new-custs) (popq custs)
           (destructuring-bind (cust buf pos limit) rec
             (multiple-value-bind (new-pos new-frags)
                 (extract-bytes frags buf pos limit)
               (if (>= new-pos limit)
                   (progn
                     (send cust)
                     (drain-buffer new-custs new-frags))
                 ;; else
                 (become (make-empty-buffer-beh
                          (pushq new-custs
                                 (list cust buf new-pos limit))))
                 )))))
        (t
         (if frags
             (become (make-nonempty-buffer-beh nil frags))
           (become (make-empty-buffer-beh nil))))
        ))

(defun make-empty-buffer-beh (custs)
  (lambda (cust &rest msg)
    (um:dcase msg
      (:add (frag)
       (send cust)
       (drain-buffer custs (addq nil frag)))
      (:get (&rest msg)
       (become (make-empty-buffer-beh (addq custs msg))))
    )))

(defun make-nonempty-buffer-beh (custs frags)
  (lambda (cust &rest msg)
    (um:dcase msg
    (:add (frag)
     (send cust)
     (drain-buffer custs (addq frags frag)))
    (:get (&rest msg)
     (drain-buffer (addq custs msg) frags))
    )))
    
;; -------------------------------------------------------------------------
;; Socket Reader
;;          +-------+    +------------+
;;  ISR --->| Entry |--->| Buffer Mgr |
;;          +-------+    +------------+
;;                             ^
;;                             |
;;                             v  cyclic
;;                     +-----------------+
;;        Prefix Len   | +-----------------+
;;         Encr Data   +-| +-----------------+    +---------------+
;;            HMAC       +-| Packet Assembly |--->| Frag Assembly |---> Msg Dispatcher
;;                         +-----------------+    +---------------+
;;

(defconstant +len-prefix-length+  4)
(defconstant +hmac-length+       32)

(defun make-reader (state mbox)
  ;; An entire subsystem to respond to incoming socket data, assemble
  ;; bytes into packets (len, data, hmac) decode packets into
  ;; messages, then forward decoded messages on to dispatcher.
  (with-accessors ((crypto     intf-state-crypto)
                   (dispatcher intf-state-dispatcher)
                   (intf       intf-state-intf)) state
    (let ((len-buf  (make-u8-vector +len-prefix-length+))
          (hmac-buf (make-u8-vector +hmac-length+))
          buf-actor
          rdr-actor
          assembler)
      (labels
          ((make-rd-lenbuf-beh ()
             (lambda ()
               (let ((ndata (convert-vector-to-integer len-buf)))
                 (cond ((> ndata +MAX-FRAGMENT-SIZE+)
                        ;; possible DOS attack - and we are done... just hang up.
                        (log-error :SYSTEM-LOG "NData = ~D" ndata)
                        (send assembler 'actors/internal-message/network:discard :E-NDATA))
                       (t
                        (let ((enc-buf (make-u8-vector ndata)))
                          (send buf-actor (sink)
                                :get (make-actor (make-rd-data-beh enc-buf ndata))
                                enc-buf 0 ndata)
                          ))
                       ))))
           (make-rd-data-beh (enc-buf ndata)
             (lambda ()
               (send buf-actor (sink) 
                     :get (make-actor (make-rd-hmac-beh enc-buf ndata))
                     hmac-buf 0 +hmac-length+)))
           (make-rd-hmac-beh (enc-buf ndata)
             (lambda ()
               (send* assembler
                      (secure-decoding crypto ndata len-buf enc-buf hmac-buf))
               (send buf-actor (sink) 
                     :get rdr-actor len-buf 0 +len-prefix-length+)
               ))

           (make-packet-assembler-beh (accum)
             (um:dlambda
               (actors/internal-message/network:discard (err)
                  ;; something went wrong, kill the connection
                  (log-error :SYSTEM-LOG "Data framing error: ~A" err)
                  (shutdown intf))
      
               (actors/internal-message/network:frag (frag)
                  ;; a partial buffer of a complete message
                  (become (make-packet-assembler-beh (cons frag accum))))
        
               (actors/internal-message/network:last-frag (frag)
                  ;; the last buffer of a complete message
                  (become (make-packet-assembler-beh nil))
                  (let* ((frags (reverse (cons frag accum)))
                         (vec   (make-instance 'scatter-vector)))
                    (dolist (frag frags)
                      (add-fragment vec frag))
                    (send dispatcher (byte-decode-obj vec))))
               ))
           (make-reader-beh ()
             (lambda ()
               (if (mp:mailbox-empty-p mbox)
                   (send self)
                 (um:dcase (mp:mailbox-read mbox)
                   
                   (actors/internal-message/network:rd-incoming (frag)
                      (send buf-actor self :add frag))
                 
                   (actors/internal-message/network:rd-error () )
                   )))))

        (setf buf-actor (make-actor (make-empty-buffer-beh nil))
              rdr-actor (make-actor (make-rd-lenbuf-beh))
              assembler (make-actor (make-packet-assembler-beh nil)))
        (send buf-actor (sink)
              :get rdr-actor len-buf 0 +len-prefix-length+)

        (send (make-actor (make-reader-beh)))
        ))))

;; -------------------------------------------------------------------------
;; Socket writer
;;                                          ISR
;;      +-------+     +------------+    +---------+    +---------+
;;  --->| Entry |<--->| Serializer |--->| Starter |--->| EndSync |--+
;;      +-------+     +------------+    +---------+    +---------+  |
;;                          ^                                       |
;;                          |                                       |
;;                          +---------------------------------------+
;;


(defun make-write-starter-beh (state write-end)
  (lambda (cust buffers)
    (with-accessors ((io-state   intf-state-io-state)
                     (io-running intf-state-io-running)) state
      (labels
          ((transmit-next-buffer (state)
             (comm:async-io-state-write-buffer state (pop buffers) #'write-next-buffer))
           
           (write-next-buffer (state &rest ignored)
             ;; this is a callback routine, executed in the thread of
             ;; the async collection
             (declare (ignore ignored))
             (cond ((comm:async-io-state-write-status state)
                    (send write-end 'actors/internal-message/network:wr-fail cust))
                   (buffers
                    (transmit-next-buffer state))
                   (t
                    (send write-end 'actors/internal-message/network:wr-done cust))
                   )))
        (cond
         ((eq cust self)
          (become (make-sink-beh)))

         ((sys:compare-and-swap (car io-running) 1 2) ;; still running recieve?
          (transmit-next-buffer io-state))

         (t
          (send cust :fail self)
          (become (make-sink-beh)))
         )))))

(defun make-write-end-beh (state starter)
  (with-accessors ((io-state         intf-state-io-state)
                   (decr-io-count-fn intf-state-decr-io-count-fn)) state
    (um:dlambda
      (actors/internal-message/network:wr-done (cust)
         (if (zerop (funcall decr-io-count-fn io-state))
             (progn
               (send cust :fail starter)
               (send starter starter))
           (send cust :ok starter)))
    
      (actors/internal-message/network:wr-fail (cust)
         (funcall decr-io-count-fn io-state)
         (send cust :fail starter)
         (send starter starter))
      )))

(defun make-write-entry-beh (serial starter)
  (lambda (item &rest msg)
    (cond ((consp item)
           ;; A list of buffers to write
           (send serial self item))
          
          ((and (eql item :fail)
                (eq (car msg) starter))
           ;; Error condition flagged by :FAIL from Starter
           (become (make-sink-beh)))
          )))

(defun make-writer (state)
  (actors ((starter  (make-write-starter-beh state sync-end))
           (sync-end (make-write-end-beh state starter))
           (serial   (make-serializer-beh starter))
           (entry    (make-write-entry-beh serial starter)))
    entry))
      
;; -------------------------------------------------------------------------

(defun make-kill-timer (timer-fn)
  (let ((timer (mp:make-timer #'mp:funcall-async timer-fn)))
    (actor (&rest msg)
      (um:dcase msg
        (:resched ()
         (mp:schedule-timer-relative timer *socket-timeout-period*))
        (:discard ()
         (mp:unschedule-timer timer)
         (become (make-sink-beh)))
        ))))

;; ------------------------------------------------------------------------
;; Simple to add a session crypto renegotiation monitor using Actors...
;;
;; Running on each end of the connection.
;; Checks for duration and data transfer limits reached,
;; then initiates a crypto renegotiation for session.
;; Each end of the connection has randomized time and data transfer limits.
;; Renegotiation can be initiated by either end.

#|
(defconstant +monitor-interval+ 60) ;; check every minute while session alive

(define-actor-class crypto-monitor ()
  ((crypto :reader   notification-crypto  :initarg :crypto)
   (intf   :reader   notification-intf    :initarg :intf)
   (timer)))

(defmethod initialize-instance :after ((mon crypto-monitor) &key &allow-other-keys)
  (with-slots (timer) mon
    (setf timer (mp:make-timer (lambda ()
                                 (check-reneg mon))))
    (resched-random-timeout timer)))

(defun resched-random-timeout (timer)
  (mp:schedule-timer-relative timer (+ 10 (random (* 2 +monitor-interval+)))))

(defmethod check-reneg ((mon crypto-monitor))
  (with-slots (crypto intf timer) mon
    (perform-in-actor mon
      (when (time-to-renegotiate? crypto)
        (inject-into-actor intf
          (handler-bind ((error (lambda* _
                                  ;; if any negotiation errors we shut down immediately
                                  (log-error :SYSTEM-LOG "Renegotiation failure")
                                  (shutdown intf))
                                ))
            #+:USING-ECC-CRYPTO (client-negotiate-security-ecc crypto intf)
            #-:USING-ECC-CRYPTO (client-negotiate-security-rsa crypto intf)
            ))
        (resched-random-timeout timer))
      )))

(defmethod kill-monitor ((mon crypto-monitor))
  (with-slots (timer) mon
    (perform-in-actor mon
      (mp:unschedule-timer timer))))
|#          
;; ------------------------------------------------------------------------
;; Once a buffer fragment has been completely received, we examine what we have

(defun make-dispatcher (state)
  (actor (whole-msg)
    (with-accessors ((kill-timer  intf-state-kill-timer)
                     (intf        intf-state-intf)) state
      (send kill-timer :resched)
      (dcase* whole-msg
          #|
          #-:USING-ECC-CRYPTO
          (actors/internal-message/security:srp-node-id-rsa (node-id)
              ;; Client is requesting security negotiation
              (inject-into-actor intf
                (handler-bind ((error (lambda* _
                                        ;; if any negotiation errors we shut down immediately
                                        (log-error :SYSTEM-LOG "Can't connect")
                                        (shutdown intf))
                                      ))
                  (server-negotiate-security-rsa crypto intf node-id)))
              ;; (spawn-worker 'server-negotiate-security-rsa crypto intf node-id)
              )

          #+:USING-ECC-CRYPTO
          (actors/internal-message/security:srp-node-id-ecc (node-id)
              ;; Client is requesting security negotiation
              (inject-into-actor intf
                (handler-bind ((error (lambda* _
                                        ;; if any negotiation errors we shut down immediately
                                        (log-error :SYSTEM-LOG "Can't connect")
                                        (shutdown intf))
                                      ))
                  (server-negotiate-security-ecc crypto intf node-id)))
              ;; (spawn-worker 'server-negotiate-security-ecc crypto intf node-id)
              )
          |#
          
        (actors/internal-message/bridge:forwarding-send (service &rest msg)
             ;; the bridge from the other end has forwarded a message to
             ;; an actor on this side
             (apply #'bridge-deliver-message service
                    (lambda ()
                      (socket-send intf 'actors/internal-message/bridge:no-service
                                   service (machine-instance)))
                    msg))

        (actors/internal-message/bridge:no-service (service node)
             ;; sent to us from the other end on our send to
             ;; non-existent service
             (mp:funcall-async #'no-service-alert service node))
          
          #|
          (actors/internal-message/bridge:forwarding-ask (service usti &rest msg)
             ;; the bridge on the other end has relayed an ASK to our
             ;; side
             (=bind (&rest ans)
                 (if-let (actor (find-actor service))                                    
                     (send* actor (apply 'assemble-ask-message =bind-cont msg))
                   (=values (capture-ans-or-exn
                              (no-service-alert service (machine-instance)))))
               (apply #'socket-send-reply intf usti ans)))
           
          (actors/internal-message/bridge:forwarding-reply (usti &rest ans)
              ;; An Actor from the other side is replying to an ASK from our side.
              (apply #'bridge-handle-reply usti ans))
          |#
        
        (t (&rest msg)
             ;; other out-of-band messages - part of a private
             ;; conversation between the two network interfaces
             #|
             (log-info :SYSTEM-LOG
                       "Incoming ~A Msg: ~A" title msg)
             |#
             (send* intf 'actors/internal-message/network:incoming-msg msg))
        ))))

(defun no-service-alert (service node)
  (error "No Service ~S on Node ~A" service node))

;; ------------------------------------------------------------------------
;; The main user-visible portion of a network interface

(defun create-socket-intf (make-beh-fn &key io-state crypto title accepting-handle)
  (let* ((state (make-intf-state
                 :title    title
                 :crypto   crypto
                 :io-state io-state
                 :accepting-handle accepting-handle))
         (intf  (make-actor (funcall make-beh-fn state))))
    (with-accessors ((title            intf-state-title)
                     (io-state         intf-state-io-state)
                     (kill-timer       intf-state-kill-timer)
                     (writer           intf-state-writer)
                     (dispatcher       intf-state-dispatcher)
                     (io-running       intf-state-io-running)
                     (decr-io-count-fn intf-state-decr-io-count-fn)) state
      (setf (intf-state-intf state) intf)
      (flet
          (#|
           #-:USING-ECC-CRYPTO
           (start-phase2-rsa (cont p-key g-key salt bb)
             ;; Called by server in response to request for crypto negotiation
             (prio-socket-send intf 'actors/internal-message/security:srp-phase2-rsa p-key g-key salt bb)
             (expect intf
               (actors/internal-message/security:srp-phase2-reply (aa m1)
                    (funcall cont aa m1))
               ))

           #+:USING-ECC-CRYPTO
           (start-phase2-ecc (cont bb)
             ;; Called by server in response to request for crypto negotiation
             (prio-socket-send intf 'actors/internal-message/security:srp-phase2-ecc bb)
             (expect intf
               (actors/internal-message/security:srp-phase2-reply (aa m1)
                                                        (funcall cont aa m1))
               ))

           (phase2-reply (cont aa m1)
             ;; Called by client after receiving server ack on crypto renegotiation
             (prio-socket-send intf 'actors/internal-message/security:srp-phase2-reply aa m1)
             (expect intf
               (actors/internal-message/security:srp-phase3 (m2)
                  (funcall cont m2))
               ))
           
           (start-phase3 (m2 final-fn)
             ;; sent by server as last message sent under old crypto during crypto negotiation
             ;; encrypt, set new crypto, then send - to avoid race conditions
             (let ((enc (secure-encoding crypto `(actors/internal-message/security:srp-phase3 ,m2))))
               ;; init new crypto for incoming messages
               (funcall final-fn)
               ;; send old-encr message
               (write-message writer enc)))
           |#)
        #|
        #+:USING-ECC-CRYPTO
        (setf srp-ph2-begin-ecc (=cont #'start-phase2-ecc))
        #-:USING-ECC-CRYPTO
        (setf srp-ph2-begin-rsa (=cont #'start-phase2-rsa))
        
        (setf srp-ph2-reply     (=cont #'phase2-reply)
              srp-ph3-begin     (=cont #'start-phase3))
        |#
        (setf kill-timer (make-kill-timer
                          #'(lambda ()
                              (log-info :SYSTEM-LOG "Inactivity shutdown request")
                              (shutdown intf)))
              dispatcher (make-dispatcher state))
        
        (let ((mbox  (mp:make-mailbox)))
          (make-reader state mbox)
          (labels
              ((rd-callback-fn (state buffer end)
                 ;; callback for I/O thread - on continuous async read
                 #|
                 (log-info :SYSTEM-LOG "Socket Reader Callback (STATUS = ~A, END = ~A)"
                           (comm:async-io-state-read-status state)
                           end)
                 |#
                 (let (err-too-large)
                   (when (plusp end)
                     ;; (log-info :SYSTEM-LOG "~A Incoming bytes: ~A" title buffer)
                     (if (> end +max-fragment-size+)
                         (setf err-too-large "Incoming packet too large")
                       (mp:mailbox-send mbox `(actors/internal-message/network:rd-incoming
                                               ,(list* 0 end (subseq buffer 0 end)))))
                     (comm:async-io-state-discard state end))
                   (when-let (status (or (comm:async-io-state-read-status state)
                                         err-too-large))
                     ;; terminate on any error
                     (comm:async-io-state-finish state)
                     (log-error :SYSTEM-LOG "~A Incoming error state: ~A" title status)
                     (mp:mailbox-send mbox `(actors/internal-message/network:rd-error))
                     (decr-io-count state))
                   ))
               
               (decr-io-count (io-state)
                 (let ((ct (sys:atomic-fixnum-decf (car io-running))))
                   (when (zerop ct) ;; >0 is running
                     (comm:close-async-io-state io-state)
                     (log-info :SYSTEM-LOG "Connection Shutdown")
                     (shutdown intf))
                   ct)))
            
            (setf writer           (make-writer state)
                  decr-io-count-fn #'decr-io-count)
            
            #|
            (setf monitor
                  (make-instance 'crypto-monitor
                                 :intf   intf
                                 :crypto crypto))
            |#
            (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                    :element-type '(unsigned-byte 8))
            (send kill-timer :resched)
            intf
            ))))))

(defun socket-send (intf &rest msg)
  (send* intf :send msg))

(defun %socket-send (state &rest msg)
  (with-accessors ((crypto     intf-state-crypto)
                   (writer     intf-state-writer)
                   (kill-timer intf-state-kill-timer)) state
    
    (send kill-timer :resched)
    (send writer (secure-encoding crypto msg))
    ))

(defun shutdown (intf)
  (send intf :shutdown))

(defun %shutdown (state)
  ;; define as a Continuation to get past any active RECV
  (with-accessors ((kill-timer       intf-state-kill-timer)
                   (io-running       intf-state-io-running)
                   (io-state         intf-state-io-state)
                   (accepting-handle intf-state-accepting-handle)
                   (title            intf-state-title)) state
    (send kill-timer :discard)
    ;; (kill-monitor monitor)
    (wr (car io-running) 0)
    (comm:async-io-state-abort-and-close io-state)
    (when accepting-handle
      (um:deletef (comm:accepting-handle-user-info accepting-handle) self))
    (bridge-unregister self)
    (log-info :SYSTEM-LOG "Socket ~A shutting down: ~A" title self)
    (become (make-sink-beh))
    ))

#|
  
(define-actor-class socket-interface ()
  ((title    :initarg :title)
   (io-state :initarg :io-state)
   (crypto   :initarg :crypto)
   #|
   #-:USING-ECC-CRYPTO (srp-ph2-begin-rsa :reader intf-srp-ph2-begin-rsa)
   #+:USING-ECC-CRYPTO (srp-ph2-begin-ecc :reader intf-srp-ph2-begin-ecc)
   (srp-ph2-reply     :reader intf-srp-ph2-reply)
   (srp-ph3-begin     :reader intf-srp-ph3-begin)
   |#
   writer
   kill-timer
   ;; monitor
   (stopped    :initform nil)
   (io-running :initform (list 1))))

#|
(defmethod do-expect ((intf socket-interface) handler)
  (inject-into-actor intf
    (recv ()
      (actors/internal-message/network:incoming-msg (&rest msg)
         (handler-bind ((error (lambda* _
                                 (log-error :SYSTEM-LOG "Expect Failure: ~A" msg)
                                 (shutdown intf))
                               ))
           (apply handler msg))
         ))))

(defmethod prio-socket-send ((intf socket-interface) &rest msg)
  (with-slots (crypto writer kill-timer) intf
    (inject-into-actor intf
      (resched kill-timer)
      (write-message writer (secure-encoding crypto msg)))))
|#

(defmethod socket-send ((intf socket-interface) &rest msg)
  (send* intf :send msg))

(defun %socket-send (intf &rest msg)
  (with-slots (crypto writer kill-timer) intf
    (send kill-timer :resched)
    (send writer (secure-encoding crypto msg))))

#|
(defun socket-send-reply (intf usti &rest ans)
  (with-slots (crypto writer kill-timer) intf
    (perform-in-actor intf
      (resched kill-timer)
      (write-message writer
                     (ensure-safe-answer crypto
                                         'actors/internal-message/bridge:forwarding-reply usti ans)))))

(defun ensure-safe-answer (crypto msg-kind usti ans)
  (handler-case
      (secure-encoding crypto (list* msg-kind usti ans)) ;; tickle error
    (error (e)
      (secure-encoding crypto (list msg-kind usti
                                    (capture-ans-or-exn
                                      (error (um:format-error e))))
                       ))
    ))
|#

(defmethod shutdown ((intf socket-interface))
  ;; define as a Continuation to get past any active RECV
  (with-slots (kill-timer #|monitor|# io-running io-state title stopped) intf
    (inject-into-actor intf ;; as a continuation, preempting RECV filtering
      (unless (shiftf stopped t)
        (send kill-timer :discard)
        ;; (kill-monitor monitor)
        (wr (car io-running) 0)
        (comm:async-io-state-abort-and-close io-state)
        (bridge-unregister intf)
        (log-info :SYSTEM-LOG "Socket ~A shutting down: ~A" title intf)
        (become-null-monitor :socket-interface)))))

#|
#-:USING-ECC-CRYPTO
(defmethod client-request-negotiation-rsa ((intf socket-interface) cont node-id)
  ;; Called by Client for crypto negotiation. Make it a continuation so
  ;; it can be initiated by message reader when deemed appropriate.
  (inject-into-actor intf
    (%socket-send intf 'actors/internal-message/security:srp-node-id-rsa node-id)
    (expect intf
      (actors/internal-message/security:srp-phase2-rsa (p-key g-key salt bb)
         (funcall cont p-key g-key salt bb))
      )))

#+:USING-ECC-CRYPTO
(defmethod client-request-negotiation-ecc ((intf socket-interface) cont node-id)
  ;; Called by Client for crypto negotiation. Make it a continuation so
  ;; it can be initiated by message reader when deemed appropriate.
  (inject-into-actor intf
    (%socket-send intf 'actors/internal-message/security:srp-node-id-ecc node-id)
    (expect intf
      (actors/internal-message/security:srp-phase2-ecc (bb)
         (funcall cont bb))
      )))
|#

(defmethod initialize-instance :after ((intf socket-interface) &key &allow-other-keys)
  (with-slots (title
               io-state
               crypto
               kill-timer
               writer
               ;; monitor
               io-running
               #|
               #-:USING-ECC-CRYPTO srp-ph2-begin-rsa
               #+:USING-ECC-CRYPTO srp-ph2-begin-ecc
               srp-ph2-reply
               srp-ph3-begin
               |#
               ) intf
    (progn ;; inject-into-actor intf ;; for =cont
      (flet
          (#|
           #-:USING-ECC-CRYPTO
           (start-phase2-rsa (cont p-key g-key salt bb)
             ;; Called by server in response to request for crypto negotiation
             (prio-socket-send intf 'actors/internal-message/security:srp-phase2-rsa p-key g-key salt bb)
             (expect intf
               (actors/internal-message/security:srp-phase2-reply (aa m1)
                    (funcall cont aa m1))
               ))

           #+:USING-ECC-CRYPTO
           (start-phase2-ecc (cont bb)
             ;; Called by server in response to request for crypto negotiation
             (prio-socket-send intf 'actors/internal-message/security:srp-phase2-ecc bb)
             (expect intf
               (actors/internal-message/security:srp-phase2-reply (aa m1)
                                                        (funcall cont aa m1))
               ))

           (phase2-reply (cont aa m1)
             ;; Called by client after receiving server ack on crypto renegotiation
             (prio-socket-send intf 'actors/internal-message/security:srp-phase2-reply aa m1)
             (expect intf
               (actors/internal-message/security:srp-phase3 (m2)
                  (funcall cont m2))
               ))
           
           (start-phase3 (m2 final-fn)
             ;; sent by server as last message sent under old crypto during crypto negotiation
             ;; encrypt, set new crypto, then send - to avoid race conditions
             (let ((enc (secure-encoding crypto `(actors/internal-message/security:srp-phase3 ,m2))))
               ;; init new crypto for incoming messages
               (funcall final-fn)
               ;; send old-encr message
               (write-message writer enc)))
           |#)
        #|
        #+:USING-ECC-CRYPTO
        (setf srp-ph2-begin-ecc (=cont #'start-phase2-ecc))
        #-:USING-ECC-CRYPTO
        (setf srp-ph2-begin-rsa (=cont #'start-phase2-rsa))
        
        (setf srp-ph2-reply     (=cont #'phase2-reply)
              srp-ph3-begin     (=cont #'start-phase3))
        |#
        (setf kill-timer        (make-actor (make-kill-timer-beh
                                             #'(lambda ()
                                                 (mp:funcall-async
                                                  #'(lambda ()
                                                      (log-info :SYSTEM-LOG "Inactivity shutdown request")
                                                      (shutdown intf))))
                                             )))
        
        (let* ((dispatcher (make-actor (make-dispatcher-beh
                                      (make-dispatcher-state
                                       :crypto crypto
                                       :kill-timer kill-timer
                                       :intf   intf
                                       :title  title)
                                      nil)))
               (reader     (make-actor (make-reader-beh crypto dispatcher))))
          (labels
              ((rd-callback-fn (state buffer end)
                 ;; callback for I/O thread - on continuous async read
                 #|
                 (log-info :SYSTEM-LOG "Socket Reader Callback (STATUS = ~A, END = ~A)"
                           (comm:async-io-state-read-status state)
                           end)
                 |#
                 (let (err-too-large)
                   (when (plusp end)
                     ;; (log-info :SYSTEM-LOG "~A Incoming bytes: ~A" title buffer)
                     (if (> end +max-fragment-size+)
                         (setf err-too-large "Incoming packet too large")
                       (send reader 'actors/internal-message/network:rd-incoming
                             (list* 0 end (subseq buffer 0 end))))
                     (comm:async-io-state-discard state end))
                   (when-let (status (or (comm:async-io-state-read-status state)
                                         err-too-large))
                     ;; terminate on any error
                     (comm:async-io-state-finish state)
                     (log-error :SYSTEM-LOG "~A Incoming error state: ~A" title status)
                     (send reader 'actors/internal-message/network:rd-error)
                     (decr-io-count state))
                   ))
               
               (decr-io-count (io-state)
                 (let ((ct (sys:atomic-fixnum-decf (car io-running))))
                   (when (zerop ct) ;; >0 is running
                     (comm:close-async-io-state io-state)
                     (log-info :SYSTEM-LOG "Connection Shutdown")
                     (shutdown intf))
                   ct)))
            
            (setf writer (make-actor (make-writer-beh
                                      (make-writer-state
                                       :io-state      io-state
                                       :io-running    io-running
                                       :decr-io-count #'decr-io-count))
                                     ))
            
            #|
            (setf monitor
                  (make-instance 'crypto-monitor
                                 :intf   intf
                                 :crypto crypto))
            |#
            (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                    :element-type '(unsigned-byte 8))
            (send kill-timer :resched)
            ))))))
|#
;; ------------------------------------------------------------------------
#|
(defun tst ()
  (bfly:!? "eval@malachite.local" '(get-universal-time))
  (bfly:!? "eval@10.0.1.13"       '(get-universal-time))
  (bfly:!? "eval@dachshund.local" '(get-universal-time)))
(tst)

(time
 (bfly:!? "eval@rincon.local" '(list "**** RESPONSE ****" (machine-instance) (get-universal-time))))
(bfly:!? "eval@arroyo.local" '(list "**** RESPONSE ****" (machine-instance) (get-universal-time)))

(bfly:!  "eval@rincon.local" '(bfly:log-info :SYSTEM-LOG
                                             (list :machine (machine-instance)
                                                   :time    (get-universal-time))))

(defun tst ()
  (let (ans)
    (log-info :SYSTEM-LOG "TIMING: ~A :VALUE ~A"
              (with-output-to-string (*trace-output*)
                (time
                 (setf ans (bfly:!? "eval@rincon.local"
                                    `(list "**** RESPONSE ****" (machine-instance) (get-universal-time)))
                       )))
              ans)))
(tst)
(ac:spawn 'tst) ;; no go...
(ac:spawn-worker 'tst)
(mp:funcall-async 'tst)

(..rem:init-messenger-mapper)
(com.sd.butterfly.glbs:show-maps)

(defun tst ()
  (let (ans)
    (log-info :SYSTEM-LOG "TIMING: ~A VALUE: ~A" 
              (with-output-to-string (*trace-output*)
                (time
                 (setf ans (start-client-messenger (bfly:make-node "rincon.local")))))
              ans)))

(tst)
(ac:spawn 'tst)
(ac:spawn-worker 'tst)
(mp:funcall-async 'tst)


(ac:spawn
 (lambda ()
   (ac:=wait* ()
       (lw:do-nothing)
     (ac:pr :got-it!))))


|#
#||# ;; -------------------------------

(defun make-deaf-socket-beh (state)
  (um:dlambda
    (:send (&rest msg)
     (apply #'%socket-send state msg))
    (:shutdown ()
     (%shutdown state))
    (t (&rest msg)
       (error "Unknown message: ~S" msg)
       (apply #'funcall msg))
    ))

(defun make-client-beh (state)
  (let ((deaf-actor (make-actor (make-deaf-socket-beh state))))
    (um:dlambda
      (actors/internal-message/network:incoming-msg (&rest msg)
         (um:dcase msg
           (actors/internal-message/network:server-info (server-node)
              (bridge-register server-node self)
              (log-info :SYSTEM-LOG "Socket client starting up: ~A" self)
              (become (make-deaf-socket-beh state)))
           ))
      (t (&rest msg)
         (send* deaf-actor msg))
      )))

(defun open-connection (ip-addr &optional ip-port)
  ;; Called from client side wishing to connect to a server
  (let ((k-start (actor (io-state)
                   (if io-state
                       (let* ((crypto  (make-instance 'crypto))
                              (intf    (create-socket-intf
                                        #'make-client-beh
                                        :title    "Client"
                                        :io-state io-state
                                        :crypto   crypto)))
                         (bridge-pre-register ip-addr intf) ;; anchor for GC
                         (socket-send intf 'actors/internal-message/network:client-info
                                      (machine-instance)))
                     ;; else
                     (error "Can't connect to: ~A" ip-addr))
                   )))
    (comm:create-async-io-state-and-connected-tcp-socket
     *ws-collection*
     ip-addr
     (or ip-port *default-port*)
     (lambda (state args)
       (when args
             (apply 'log-error :SYSTEM-LOG args))
       (send k-start (if args nil state)))
     #||#
     :ssl-ctx (when +using-ssl+ :tls-v1)
     :ctx-configure-callback (when +using-ssl+
                               (lambda (ctx)
                                 (comm:set-ssl-ctx-cert-cb ctx 'my-find-certificate)))
     #||#
     :handshake-timeout 5
     #-:WINDOWS :ipv6    #-:WINDOWS nil)))

;; -------------------------------------------------------------

(defun make-server-beh (state)
  (let ((deaf-actor (make-actor (make-deaf-socket-beh state))))
    (um:dlambda
      (actors/internal-message/network:incoming-msg (&rest msg)
         (um:dcase msg
           (actors/internal-message/network:client-info (client-node)
              (log-info :SYSTEM-LOG "Socket server starting up: ~A" self)
              (socket-send self 'actors/internal-message/network:server-info (machine-instance))
              (bridge-register client-node self)
              (become (make-deaf-socket-beh state)))
           ))
      (t (&rest msg)
         (send* deaf-actor msg))
      )))

(defun start-server-messenger (accepting-handle io-state)
  "Internal routine to start a network interface from the server side.
The interface is identical to that on the client side, but the
connection handshake differs from this side.

See the discussion under START-CLIENT-MESSENGER for details."

  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (let* ((crypto  (make-instance 'crypto))
         (intf    (create-socket-intf #'make-server-beh
                                      :title    "Server"
                                      :io-state io-state
                                      :accepting-handle accepting-handle
                                      :crypto   crypto)))
    ;; for server side, this user-info is the only reference to intf
    ;; until we get registered into the ip-mapping table.
    (push intf (comm:accepting-handle-user-info accepting-handle))
    ))

;; --------------------------------------------------------------
;;; The certificate and private key files in this directory were generated
;;; by running gen-certs.sh

(defvar *ssl-context*  nil)
(defvar *sks*
  '(("appear"  "learn"  "mean"  "diagram"  "off"  "average"  "aerobic"  "rose"
     "similar"  "notice"  "hill"  "accident"  "hammer"  "vicious"  "exercise"
     "kind"  "bacon"  "fossil"  "convince"  "tent"  "spare"  "old"  "news"  "march")
    ("art"  "gaze"  "toss"  "total"  "donor"  "melody"  "segment"  "vessel"
     "inspire"  "prefer"  "glass"  "worth"  "cargo"  "raise"  "soup"  "road"
     "topple"  "latin"  "rotate"  "mixture"  "toddler"  "bright"  "kind"  "board")
    ("allow"  "place"  "because"  "whip"  "wealth"  "identify"  "dial"  "pilot"
     "ahead"  "decade"  "toward"  "voice"  "control"  "company"  "gaze"  "wedding"
     "game"  "average"  "shoe"  "worth"  "scene"  "ensure"  "test"  "blur")))
  
(define-symbol-macro *actors-version* (assemble-sks *sks*))

(defun filename-in-ssl-server-directory (name)
  (namestring (merge-pathnames name
                               (merge-pathnames "Butterfly/"
                                                (sys:get-folder-path :appdata))
                               )))
              
(defun verify-client-certificate (ok-p xsc)
  (format (or mp:*background-standard-output* t)
          "Current certificate issuer : ~a [~a]~%"
          (comm:x509-name-field-string 
           (comm:x509-get-issuer-name
            (comm:x509-store-ctx-get-current-cert xsc))
           "organizationName")
          ok-p)
  t)

(defun my-configure-ssl-ctx (ssl-ctx ask-for-certificate)
  (comm:set-ssl-ctx-password-callback
   ssl-ctx
   :password *actors-version*)
  (comm:ssl-ctx-use-certificate-chain-file
   ssl-ctx
   (filename-in-ssl-server-directory "newcert.pem" ))
  (comm:ssl-ctx-use-rsaprivatekey-file
   ssl-ctx
   (filename-in-ssl-server-directory "newreq.pem")
   comm:ssl_filetype_pem)
  (comm:set-ssl-ctx-dh
   ssl-ctx
   :filename (filename-in-ssl-server-directory "dh_param_1024.pem"))

  (when ask-for-certificate
    (comm:set-verification-mode
     ssl-ctx
     :server :always
     'verify-client-certificate)
    (comm:set-verification-depth
     ssl-ctx
     1)))

(defun initialize-the-ctx (symbol ask-for-certificate)
  (when-let (old (symbol-value symbol))
    (comm:destroy-ssl-ctx old)
    (set symbol nil))
  (let ((new (comm:make-ssl-ctx)))
    (set symbol new)
    (my-configure-ssl-ctx new ask-for-certificate)))

(defvar *cert-key-pairs* nil)

(defun my-find-certificate (ssl-pointer)
  (declare (ignorable ssl-pointer))
  (let ((pair (or *cert-key-pairs* 
                  (setq *cert-key-pairs* 
                        (comm:read-certificate-key-pairs
                         (filename-in-ssl-server-directory "cert-and-key.pem")
                        :pass-phrase *actors-version*)))))
    (values (caar pair) (second (car pair)))))

;; --------------------------------------------------------------

(defun terminate-server (reply-to)
  (if *aio-accepting-handle*
      (progn
        (setf (comm:accepting-handle-user-info *aio-accepting-handle*) nil)
        (comm:close-accepting-handle *aio-accepting-handle*
                                     (lambda (coll)
                                       ;; we are operating in the collection process
                                       (comm:close-wait-state-collection coll)
                                       (when +using-ssl+
                                         (comm:destroy-ssl-ctx *ssl-context*)
                                         (setf *ssl-context* nil))
                                       (setf *aio-accepting-handle* nil
                                             *ws-collection*        nil)
                                       (unwind-protect
                                           (mp:process-terminate (mp:get-current-process))
                                         (send reply-to :ok)))))
    ;; else
    (send reply-to :ok)))

(defun start-tcp-server (&optional (tcp-port-number *default-port*))
  "An internal routine to start up a server listener socket on the
indicated port number."
  (let ((starter (make-actor
                  (lambda* _
                    (when +using-ssl+
                      (initialize-the-ctx '*ssl-context* t))
                    (setq *ws-collection*
                          (comm:create-and-run-wait-state-collection "Actors Server"))
                    (setq *aio-accepting-handle* 
                          (comm:accept-tcp-connections-creating-async-io-states
                           *ws-collection*
                           tcp-port-number
                           #'start-server-messenger
                           :ssl-ctx (when +using-ssl+
                                      *ssl-context*)
                           :ipv6    nil
                           ))
                    (log-info :SYSTEM-LOG "Actors service started on port ~A" tcp-port-number)))))
    (terminate-server starter)))

;; --------------------------------------------------
;;

(defun reset-global-state ()
  (when *ssl-context*
    (comm:destroy-ssl-ctx *ssl-context*)
    (setf *ssl-context* nil))
  (setf *ws-collection*        nil
        *aio-accepting-handle* nil
        *cert-key-pairs*       nil))

(defun* lw-start-tcp-server _
  ;; called by Action list with junk args
  ;;
  ;; We need to delay the construction of the system logger till this
  ;; time so that we get a proper background-error-stream.  Cannot be
  ;; performed on initial load of the LFM.
  (assert (null *ws-collection*))
  (assert (null *aio-accepting-handle*))
  (assert (null *ssl-context*))
  (assert (null *cert-key-pairs*))
  (actors/base::start-actors-system)
  (ensure-system-logger)
  (start-tcp-server))

(defun* lw-reset-actor-system _
  (terminate-server (sink))
  (bridge-reset)
  (kill-system-logger)
  (kill-executives)
  (reset-global-state)
  (print "Actors and Network has been shut down."))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor Server"
                    'lw-start-tcp-server
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Reset Actors"
                    'lw-reset-actor-system)

  (lw:define-action "Save Session After"
                    "Restart Actor System"
                    'lw-start-tcp-server)
  )

(defun ac:start ()
  (lw-start-tcp-server))
