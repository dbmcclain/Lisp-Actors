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
            actors/bridge:bridge-unregister
            actors/bridge:bridge-handle-reply
            actors/bridge:bridge-reset

            actors/lfm:ensure-system-logger
            actors/lfm:kill-system-logger
            
            actors/base:assemble-ask-message

            scatter-vec:scatter-vector
            scatter-vec:add-fragment

            #-:USING-ECC-CRYPTO actors/srp6-rsa:server-negotiate-security-rsa
            #-:USING-ECC-CRYPTO actors/srp6-rsa:client-negotiate-security-rsa

            #+:USING-ECC-CRYPTO actors/srp6-ecc:client-negotiate-security-ecc
            #+:USING-ECC-CRYPTO actors/srp6-ecc:server-negotiate-security-ecc
            )))

;; -----------------------------------------------------------------------

(defvar *default-port*            65001)
(defvar *socket-timeout-period*   20)
(defvar *ws-collection*           nil)
(defvar *aio-accepting-handle*    nil)

(defconstant +using-ssl+          t)

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

(defun extract-bytes (queue buf start end)
  ;; extract fragments as needed to fill a buffer
  (prog ()
    again
    (destructuring-bind
        (&whole frag &optional frag-start frag-end . frag-bytes)
        (finger-tree:popq queue)
      (if frag
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
                (incf start nb))
              (go again))
             
             ((= nb need)
              ;; fragment has exactly what we need
              (when (plusp need)
                (replace buf frag-bytes
                         :start1 start
                         :start2 frag-start))
              (return end))
             
             (t ;; (> nb need)
                ;; fragment has more than needed. Take what we need
                ;; and put back the rest.
                (when (plusp need)
                  (replace buf frag-bytes
                           :start1 start
                           :start2 frag-start
                           :end1   end)
                  (incf (car frag) need))
                (finger-tree:pushq queue frag)
                (return end))
             ))
        ;; else - no more fragments. Return the count extracted so far
        (return start)))
    ))
       
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

(defun become-null-monitor (name)
  (become (lambda (&rest msg)
            (log-info :SYSTEM-LOG
                      "Null Monitor ~A Msg: ~A" name msg))))

;; -------------------------------------------------------------------------

(define-actor-class message-reader ()
  ())

(defmethod initialize-instance :after ((reader message-reader)
                                       &key crypto dispatcher &allow-other-keys)
  (let ((queue        (finger-tree:make-unshared-queue))
        (len-buf      (make-u8-vector 4))
        (hmac-buf     (make-u8-vector 32))
        (len          0)
        (pos          0)
        (enc-buf      nil)
        (ndata        nil)
        (statefn      nil))
    (macrolet ((check ()
                 `(funcall statefn))
               (state (fn)
                 `(progn
                    (setf statefn #',fn
                          pos     0)
                    (check))))
      (labels
          ((rd-len ()
             (setf pos (extract-bytes queue len-buf pos len))
             (when (= pos len)
               (setf ndata (convert-vector-to-integer len-buf))
               (cond ((> ndata +MAX-FRAGMENT-SIZE+)
                      ;; and we are done... just hang up.
                      (handle-message dispatcher '(actors/internal-message/network:discard)))
                     (t
                      (setf enc-buf (make-u8-vector ndata))
                      (state rd-data))
                     )))
           (rd-data ()
             (setf pos (extract-bytes queue enc-buf pos ndata))
             (when (= pos ndata)
               (setf len (length hmac-buf))
               (state rd-hmac)))
           (rd-hmac ()
             (setf pos (extract-bytes queue hmac-buf pos len))
             (when (= pos len)
               (handle-message dispatcher (secure-decoding crypto ndata len-buf enc-buf hmac-buf))
               (setf len (length len-buf))
               (state rd-len))))
        (setf len     (length len-buf)
              statefn #'rd-len
              (actor-beh reader)
              (um:dlambda
                (actors/internal-message/network:rd-incoming (frag)
                  (finger-tree:addq queue frag)
                  (check))
                (actors/internal-message/network:rd-error ()
                  (become-null-monitor :rd-actor)))
              )))))

;; -------------------------------------------------------------------------
#|
(define-actor-class message-reader ()
  ((crypto     :initarg :crypto)
   (dispatcher :initarg :dispatcher)
   (queue      :initform (finger-tree:make-shared-queue))
   (len-buf    :initform (make-u8-vector 4))
   (hmac-buf   :initform (make-u8-vector 32))))

(defmethod initialize-instance :after ((reader message-reader) &key &allow-other-keys)
  (with-slots (crypto dispatcher queue len-buf hmac-buf) reader
    (=flet
        ((read-buf (buf)
           ;; incoming byte stuffer and buffer reader
           (let ((len (length buf)))
             (nlet rd-wait ((start 0))
               (let ((pos (extract-bytes queue buf start len)))
                 (cond
                  ((= pos len)
                   (=values))
                  
                  (t
                   (recv ()
                     (actors/internal-message/network:rd-incoming (frag)
                        ;; (add-queue queue frag)
                        (finger-tree:addq queue frag)
                        (rd-wait pos))
                     
                     (actors/internal-message/network:rd-error ()
                        (become-null-monitor :rd-actor))
                     ))
                  )))
             )))
      
      ;; message asssembly and decoding from (len, payload, hmac)
      (inject-into-actor reader
        (nlet read-next-message ()
          (=bind ()
              (read-buf len-buf)
            (let ((len (convert-vector-to-integer len-buf)))
              (if (> len +MAX-FRAGMENT-SIZE+)
                  ;; and we are done... just hang up.
                  (handle-message dispatcher '(actors/internal-message/network:discard))
                ;; else
                (let ((enc-buf  (make-u8-vector len)))
                  (=bind ()
                      (read-buf enc-buf)
                    (=bind ()
                        (read-buf hmac-buf)
                      (handle-message dispatcher (secure-decoding crypto len len-buf enc-buf hmac-buf))
                      (read-next-message))
                    ))
                ))))))))
|#
;; -------------------------------------------------------------------------

(define-actor-class message-writer ()
  ((io-state      :initarg :io-state)
   (io-running    :initarg :io-running)
   (decr-io-count :initarg :decr-io-count)))

(defmethod write-message ((writer message-writer) buffers)
  (with-slots (io-state io-running decr-io-count) writer
    (labels
        ((we-are-done ()
           (become-null-monitor :wr-actor))

         (transmit-next-buffer (state)
           (comm:async-io-state-write-buffer state (pop buffers) #'write-next-buffer))
           
         (write-next-buffer (state &rest ignored)
           ;; this is a callback routine, executed in the thread of
           ;; the async collection
           (declare (ignore ignored))
           (cond ((comm:async-io-state-write-status state)
                  (send writer 'actors/internal-message/network:wr-fail))
                 (buffers
                  (transmit-next-buffer state))
                 (t
                  (send writer 'actors/internal-message/network:wr-done))
                 )))
      
      (perform-in-actor writer
        (cond
         ((sys:compare-and-swap (car io-running) 1 2) ;; still running recieve?
          (transmit-next-buffer io-state)
          (recv ()
            (actors/internal-message/network:wr-done ()
              (when (zerop (funcall decr-io-count io-state))
                (we-are-done)))
            (actors/internal-message/network:wr-fail ()
              (funcall decr-io-count io-state)
              (we-are-done))
            ))
         (t
          (we-are-done))
         )))))

;; -------------------------------------------------------------------------

(define-actor-class kill-timer ()
  (timer))

(defmethod initialize-instance :after ((kt kill-timer) &key timer-fn &allow-other-keys)
  (with-slots (timer) kt
    (setf timer (mp:make-timer timer-fn))))

(defmethod resched ((kt kill-timer))
  (with-slots (timer) kt
    (perform-in-actor kt
      (when timer
        (mp:schedule-timer-relative timer *socket-timeout-period*)))))

(defmethod discard ((kt kill-timer))
  (with-slots (timer) kt
    (perform-in-actor kt
      (when timer
        (mp:unschedule-timer timer)
        (setf timer nil)))))

;; ------------------------------------------------------------------------
;; Simple to add a session crypto renegotiation monitor using Actors...
;;
;; Running on each end of the connection.
;; Checks for duration and data transfer limits reached,
;; then initiates a crypto renegotiation for session.
;; Each end of the connection has randomized time and data transfer limits.
;; Renegotiation can be initiated by either end.

(defconstant +monitor-interval+ 60) ;; check every minute while session alive

(define-actor-class crypto-monitor ()
  ((crypto :reader   notification-crypto  :initarg :crypto)
   (intf   :reader   notification-intf    :initarg :intf)
   (timer)))

(defmethod initialize-instance :after ((mon crypto-monitor) &key &allow-other-keys)
  (with-slots (timer) mon
    (setf timer (mp:make-timer (lambda ()
                                 (check-reneg mon))))
    (mp:schedule-timer-relative timer (random (* 2 +monitor-interval+)))
    ))

(defmethod check-reneg ((mon crypto-monitor))
  (with-slots (crypto intf timer) mon
    (perform-in-actor mon
      (when (time-to-renegotiate? crypto)
        (handler-bind ((error (lambda (c)
                                ;; if any negotiation errors we shut down immediately
                                (declare (ignore c))
                                (log-error :SYSTEM-LOG "Renegotiation failure")
                                (shutdown intf))
                              ))
          #+:USING-ECC-CRYPTO (client-negotiate-security-ecc crypto intf)
          #-:USING-ECC-CRYPTO (client-negotiate-security-rsa crypto intf)
          ))
      (mp:schedule-timer-relative timer (random (* 2 +monitor-interval+))))
    ))

(defmethod kill-monitor ((mon crypto-monitor))
  (with-slots (timer) mon
    (perform-in-actor mon
      (mp:unschedule-timer timer))))
          
;; ------------------------------------------------------------------------
;; Once a buffer fragment has been completely received, we examine what we have

(define-actor-class message-dispatcher ()
  ((accum      :initform (make-instance 'scatter-vector))
   (kill-timer :initarg :kill-timer)
   (intf       :initarg :intf)
   (title      :initarg :title)
   (crypto     :initarg :crypto)))

(defmethod handle-message ((dispatcher message-dispatcher) whole-msg)
  (with-slots (accum kill-timer intf #|title|# crypto) dispatcher
    (resched kill-timer)
    (perform-in-actor dispatcher
      (dcase* whole-msg
        
        (actors/internal-message/network:discard (&rest msg)
          ;; something went wrong, kill the connection
          (declare (ignore msg))
          (log-error :SYSTEM-LOG "Data framing error")
          (shutdown intf))
        
        (actors/internal-message/network:frag (frag)
          ;; a partial buffer of a complete message
          (add-fragment accum frag))
        
        (actors/internal-message/network:last-frag (frag)
           ;; the last buffer of a complete message
           (add-fragment accum frag)
           (handle-message dispatcher (byte-decode-obj
                                       (shiftf accum
                                               (make-instance 'scatter-vector)))
                           ))

        #-:USING-ECC-CRYPTO
        (actors/internal-message/security:srp-node-id-rsa (node-id)
           ;; Client is requesting security negotiation
           (inject-into-actor intf
             (handler-bind ((error (lambda (c)
                                     ;; if any negotiation errors we shut down immediately
                                     (declare (ignore c))
                                     (log-error :SYSTEM-LOG "Can't connect")
                                     (shutdown intf))
                                   ))
               (Server-negotiate-security-rsa crypto intf node-id)))
           ;; (spawn-worker 'server-negotiate-security-rsa crypto intf node-id)
           )

        #+:USING-ECC-CRYPTO
        (actors/internal-message/security:srp-node-id-ecc (node-id)
           ;; Client is requesting security negotiation
           (inject-into-actor intf
             (handler-bind ((error (lambda (c)
                                     ;; if any negotiation errors we shut down immediately
                                     (declare (ignore c))
                                     (log-error :SYSTEM-LOG "Can't connect")
                                     (shutdown intf))
                                   ))
               (server-negotiate-security-ecc crypto intf node-id)))
           ;; (spawn-worker 'server-negotiate-security-ecc crypto intf node-id)
           )

        (actors/internal-message/bridge:forwarding-send (service &rest msg)
           ;; the bridge from the other end has forwarded a message to
           ;; an actor on this side
           (=bind ()
               (apply #'bridge-deliver-message service =bind-cont msg)
             (socket-send intf 'actors/internal-message/bridge:no-service
                          service (machine-instance)))
           #|
           (if-let (actor (find-actor service))
               (apply 'send actor msg)
             (socket-send intf 'actors/internal-message/bridge:no-service service (machine-instance)))
           |#
           )

        (actors/internal-message/bridge:no-service (service node)
           ;; sent to us from the other end on our send to
           ;; non-existent service
           (mp:funcall-async 'no-service-alert service node))
        
        (actors/internal-message/bridge:forwarding-ask (service usti &rest msg)
           ;; the bridge on the other end has relayed an ASK to our
           ;; side
           (=bind (&rest ans)
               (if-let (actor (find-actor service))                                    
                   (apply 'send actor (apply 'assemble-ask-message =bind-cont msg))
                 (=values (capture-ans-or-exn
                            (no-service-alert service (machine-instance)))))
             (apply 'socket-send-reply intf usti ans)))
           
        (actors/internal-message/bridge:forwarding-reply (usti &rest ans)
           ;; An Actor from the other side is replying to an ASK from our side.
           (apply 'bridge-handle-reply usti ans))

        (t (&rest msg)
           ;; other out-of-band messages - part of a private
           ;; conversation between the two network interfaces
           #|
            (log-info :SYSTEM-LOG
                      "Incoming ~A Msg: ~A" title msg)
            |#
           (apply 'send intf 'actors/internal-message/network:incoming-msg msg))
        ))))

(defun no-service-alert (service node)
  (error "No Service ~A on Node ~A" service node))

;; ------------------------------------------------------------------------
;; The main user-visible portion of a network interface

(define-actor-class socket-interface ()
  ((title    :initarg :title)
   (io-state :initarg :io-state)
   (crypto   :initarg :crypto)
   #-:USING-ECC-CRYPTO (srp-ph2-begin-rsa :reader intf-srp-ph2-begin-rsa)
   #+:USING-ECC-CRYPTO (srp-ph2-begin-ecc :reader intf-srp-ph2-begin-ecc)
   (srp-ph2-reply     :reader intf-srp-ph2-reply)
   (srp-ph3-begin     :reader intf-srp-ph3-begin)
   writer
   kill-timer
   monitor
   (stopped    :initform nil)
   (io-running :initform (list 1))))

(defmethod do-expect ((intf socket-interface) handler)
  (perform-in-actor intf
    (recv ()
      (actors/internal-message/network:incoming-msg (&rest msg)
         (handler-bind ((error (lambda (err)
                                 (declare (ignore err))
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

(defmethod socket-send ((intf socket-interface) &rest msg)
  (with-slots (crypto writer kill-timer) intf
    (perform-in-actor intf
      (resched kill-timer)
      (write-message writer (secure-encoding crypto msg)))))

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

(defmethod shutdown ((intf socket-interface))
  ;; define as a Continuation to get past any active RECV
  (with-slots (kill-timer monitor io-running io-state title stopped) intf
    (inject-into-actor intf ;; as a continuation, preempting RECV filtering
      (unless (shiftf stopped t)
        (discard kill-timer)
        (kill-monitor monitor)
        (wr (car io-running) 0)
        (comm:async-io-state-abort-and-close io-state)
        (bridge-unregister intf)
        (log-info :SYSTEM-LOG "Socket ~A shutting down: ~A" title intf)
        (become-null-monitor :socket-interface)))))

#-:USING-ECC-CRYPTO
(defmethod client-request-negotiation-rsa ((intf socket-interface) cont node-id)
  ;; Called by Client for crypto negotiation. Make it a continuation so
  ;; it can be initiated by message reader when deemed appropriate.
  (inject-into-actor intf
    (socket-send intf 'actors/internal-message/security:srp-node-id-rsa node-id)
    (expect intf
      (actors/internal-message/security:srp-phase2-rsa (p-key g-key salt bb)
         (funcall cont p-key g-key salt bb))
      )))

#+:USING-ECC-CRYPTO
(defmethod client-request-negotiation-ecc ((intf socket-interface) cont node-id)
  ;; Called by Client for crypto negotiation. Make it a continuation so
  ;; it can be initiated by message reader when deemed appropriate.
  (inject-into-actor intf
    (socket-send intf 'actors/internal-message/security:srp-node-id-ecc node-id)
    (expect intf
      (actors/internal-message/security:srp-phase2-ecc (bb)
         (funcall cont bb))
      )))

(defmethod initialize-instance :after ((intf socket-interface) &key &allow-other-keys)
  (with-slots (title
               io-state
               crypto
               kill-timer
               writer
               monitor
               io-running
               #-:USING-ECC-CRYPTO srp-ph2-begin-rsa
               #+:USING-ECC-CRYPTO srp-ph2-begin-ecc
               srp-ph2-reply
               srp-ph3-begin) intf
    (inject-into-actor intf ;; for =cont
      (flet
          (#-:USING-ECC-CRYPTO
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
               (write-message writer enc))))
        
        #+:USING-ECC-CRYPTO
        (setf srp-ph2-begin-ecc (=cont #'start-phase2-ecc))
        #-:USING-ECC-CRYPTO
        (setf srp-ph2-begin-rsa (=cont #'start-phase2-rsa))
        
        (setf srp-ph2-reply     (=cont #'phase2-reply)
              srp-ph3-begin     (=cont #'start-phase3)
              kill-timer        (make-instance 'kill-timer
                                               :timer-fn #'(lambda ()
                                                             (mp:funcall-async
                                                              (lambda ()
                                                                (log-info :SYSTEM-LOG "Inactivity shutdown request")
                                                                (shutdown intf))))
                                               )))
    
      (let ((reader (make-instance 'message-reader
                                   :crypto     crypto
                                   :dispatcher (make-instance 'message-dispatcher
                                                              :title      title
                                                              :intf       intf
                                                              :crypto     crypto
                                                              :kill-timer kill-timer)
                                   )))
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
                     (send reader 'actors/internal-message/network:rd-incoming (list* 0 end (subseq buffer 0 end))))
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
          
          (setf writer
                (make-instance 'message-writer
                               :io-state      io-state
                               :io-running    io-running
                               :decr-io-count #'decr-io-count)
                monitor
                (make-instance 'crypto-monitor
                               :intf   intf
                               :crypto crypto))
          (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                  :element-type '(unsigned-byte 8))
          (resched kill-timer)
          )))))

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

(defun open-connection (reply-to ip-addr &optional ip-port)
  ;; Called from client side wishing to connect to a server
  (=bind (io-state)
      (comm:create-async-io-state-and-connected-tcp-socket
         *ws-collection*
         ip-addr
         (or ip-port *default-port*)
         (lambda (state args)
           (when args
             (apply 'log-error :SYSTEM-LOG args))
           (=values (if args nil state)))
         #||#
         :ssl-ctx (when +using-ssl+ :tls-v1)
         :ctx-configure-callback (when +using-ssl+
                                   (lambda (ctx)
                                     (comm:set-ssl-ctx-cert-cb ctx 'my-find-certificate)))
         #||#
         :handshake-timeout 5
         #-:WINDOWS :ipv6    #-:WINDOWS nil)
    (if io-state
          (let* ((crypto  (make-instance 'crypto))
                 (intf    (make-instance 'socket-interface
                                         :title    "Client"
                                         :io-state io-state
                                         :crypto   crypto)))
            (inject-into-actor intf
            (handler-bind ((error (lambda (c)
                                    ;; if any negotiation errors we shut down immediately
                                    (declare (ignore c))
                                    (log-error :SYSTEM-LOG "Can't connect")
                                    (shutdown intf))
                                  ))
              (=bind ()
                ;; connection will be authenticated/encrypted regardless of using SSL/TLS or not.
                #+:USING-ECC-CRYPTO (client-negotiate-security-ecc crypto intf =bind-cont)
                #-:USING-ECC-CRYPTO (client-negotiate-security-rsa crypto intf =bind-cont)
                (prio-socket-send intf 'actors/internal-message/network:client-info (machine-instance))
                (progn
                    (expect intf
                      (actors/internal-message/network:server-info (server-node)
                          (bridge-register server-node intf)
                          (bridge-register ip-addr intf)
                          (log-info :SYSTEM-LOG "Socket client starting up: ~A" intf)
                          (send reply-to intf)
                          )))))))
      ;; else
      (error "Can't connect to: ~A" ip-addr))))

;; -------------------------------------------------------------

(defun start-server-messenger (accepting-handle io-state)
  "Internal routine to start a network interface from the server side.
The interface is identical to that on the client side, but the
connection handshake differs from this side.

See the discussion under START-CLIENT-MESSENGER for details."

  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (declare (ignore accepting-handle))
  (let* ((crypto  (make-instance 'crypto))
         (intf    (make-instance 'socket-interface
                                 :title    "Server"
                                 :io-state io-state
                                 :crypto   crypto)))

    (expect intf
      (actors/internal-message/network:client-info (client-node)
          (log-info :SYSTEM-LOG "Socket server starting up: ~A" intf)
          (socket-send intf 'actors/internal-message/network:server-info (machine-instance))
          (bridge-register client-node intf))
      )))

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

(=defun %terminate-server ()
  (if *aio-accepting-handle*
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
                                       (=values))))
    ;; else
    (=values)))

(defun terminate-server ()
  (=wait (())
      (%terminate-server)))

(defun start-tcp-server (&optional (tcp-port-number *default-port*))
  "An internal routine to start up a server listener socket on the
indicated port number."

  (terminate-server)
  (when +using-ssl+
    (initialize-the-ctx '*ssl-context* t))
  (setq *ws-collection*
        (comm:create-and-run-wait-state-collection "Actors Server"))
  (setq *aio-accepting-handle* 
        (comm:accept-tcp-connections-creating-async-io-states
         *ws-collection*
         tcp-port-number
         'start-server-messenger
         :ssl-ctx (when +using-ssl+
                    *ssl-context*)
         :ipv6    nil
         ))
  (log-info :SYSTEM-LOG "Actors service started on port ~A" tcp-port-number))

;; --------------------------------------------------
;;

(defun reset-global-state ()
  (when *ssl-context*
    (comm:destroy-ssl-ctx *ssl-context*)
    (setf *ssl-context* nil))
  (setf *ws-collection*        nil
        *aio-accepting-handle* nil
        *cert-key-pairs*       nil))

(defun lw-start-tcp-server (&rest ignored)
  ;; called by Action list with junk args
  (declare (ignore ignored))
  ;; We need to delay the construction of the system logger till this
  ;; time so that we get a proper background-error-stream.  Cannot be
  ;; performed on initial load of the LFM.
  (assert (null *ws-collection*))
  (assert (null *aio-accepting-handle*))
  (assert (null *ssl-context*))
  (assert (null *cert-key-pairs*))
  (ensure-system-logger)
  (start-tcp-server))

(defun lw-reset-actor-system (&rest ignored)
  (declare (ignore ignored))
  (terminate-server)
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
