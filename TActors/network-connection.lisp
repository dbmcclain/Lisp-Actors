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
  (import '(um:when-let
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
            actors/bridge:bridge-reset
            actors/bridge:bridge-deliver-message

            actors/lfm:ensure-system-logger
            actors/lfm:kill-system-logger

            scatter-vec:scatter-vector
            scatter-vec:add-fragment

            finger-tree:addq
            finger-tree:popq
            finger-tree:pushq

            actors/srp6-ecc:client-negotiate-security-ecc
            actors/srp6-ecc:server-negotiate-security-ecc
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
       
(defun drain-buffer (custs frags new-ctr)
  (cond (custs
         (multiple-value-bind (rec new-custs) (popq custs)
           (destructuring-bind (cust buf pos limit) rec
             (multiple-value-bind (new-pos new-frags)
                 (extract-bytes frags buf pos limit)
               (if (>= new-pos limit)
                   (progn
                     (send cust)
                     (drain-buffer new-custs new-frags new-ctr))
                 ;; else
                 (become (make-empty-buffer-beh
                          (pushq new-custs
                                 (list cust buf new-pos limit))
                          new-ctr))
                 )))))
        (t
         (if frags
             (become (make-nonempty-buffer-beh nil frags new-ctr))
           (become (make-empty-buffer-beh nil new-ctr))))
        ))

(defun make-empty-buffer-beh (custs ctr)
  (alambda
    ((:add-bytes lbl frag)
     (cond ((= ctr lbl)
            (drain-buffer custs (addq nil frag) (1+ ctr)))
           (t
            (repeat-send self))
           ))
    ((:get . msg)
     (become (make-empty-buffer-beh (addq custs msg) ctr)))
    ))

(defun make-nonempty-buffer-beh (custs frags ctr)
  (alambda
    ((:add-bytes lbl frag)
     (cond ((= ctr lbl)
            (drain-buffer custs (addq frags frag) (1+ ctr)))
           (t
            (repeat-send self))
           ))
    ((:get . msg)
     (drain-buffer (addq custs msg) frags ctr))
    ))

(defun make-buffer-manager ()
  (make-actor
   (make-empty-buffer-beh nil 1)))

;; -------------------------------------------------------------------------
;; Socket Reader
;;                 +------------+
;;         ISR --->| Buffer Mgr |
;;                 +------------+
;;                       ^
;;                       |
;;                       v  cyclic
;;                     +-----------------+
;;        Prefix Len   | +-----------------+
;;         Encr Data   +-| +-----------------+    +---------------+
;;            HMAC       +-| Packet Assembly |--->| Frag Assembly |---> Msg Dispatcher
;;                         +-----------------+    +---------------+
;;

(defconstant +len-prefix-length+  4)
(defconstant +hmac-length+       32)

(defun make-frag-assembler-beh (state ctr frags)
  (with-accessors ((dispatcher intf-state-dispatcher)
                   (intf       intf-state-intf)) state
    (alambda
     
     ((_ :discard err)
      ;; something went wrong, kill the connection
      (log-error :SYSTEM-LOG "Data framing error: ~A" err)
      (become (make-sink-beh))
      (shutdown intf))
     
     ((in-ctr :frag frag)
      (cond ((= in-ctr ctr)
             (become (make-frag-assembler-beh state (1+ ctr) (cons frag frags))))
            (t
             (repeat-send self))
            ))
     
     ((in-ctr :last-frag frag)
      (cond ((= in-ctr ctr)
             (become (make-frag-assembler-beh state (1+ ctr) nil))
             (with-worker
               (let ((fragv (make-instance 'scatter-vector)))
                 (dolist (frag (reverse (cons frag frags)))
                   (add-fragment fragv frag))
                 (send* dispatcher (byte-decode-obj fragv)))))
            (t
             ;; ctr out of sync, go around again
             (repeat-send self))
            ))
     )))

(defun make-frag-assembler (state)
  (make-actor (make-frag-assembler-beh state 0 nil)))

(defun make-reader (state)
  ;; An entire subsystem to respond to incoming socket data, assemble
  ;; bytes into packets (len, data, hmac) decode packets into
  ;; messages, then forward decoded messages on to dispatcher.
  (let ((len-buf  (make-u8-vector +len-prefix-length+))
        (hmac-buf (make-u8-vector +hmac-length+))
        (buf-mgr  (make-buffer-manager))
        (frag-asm (make-frag-assembler state)))
    (with-accessors ((crypto  intf-state-crypto)) state
      (beta (ctr / )
          (send buf-mgr
                :get (beta 0) len-buf 0 +len-prefix-length+)
        (let ((ndata (convert-vector-to-integer len-buf)))
          (cond
           ((> ndata +MAX-FRAGMENT-SIZE+)
            ;; possible DOS attack - and we are done... just hang up.
            (log-error :SYSTEM-LOG "NData = ~D" ndata)
            (send frag-asm :discard :E-NDATA))
           
           (t
            ;; Actor Continuation Style (ACS)
            (let ((beta-len self)
                  (enc-buf  (make-u8-vector ndata)))
              (become (beta-beh (1+ ctr)))
              (beta ()
                  (send buf-mgr
                        :get beta enc-buf 0 ndata)
                (beta ()
                    (send buf-mgr
                          :get beta hmac-buf 0 +hmac-length+)
                  (send* frag-asm ctr
                         (secure-decoding crypto ndata
                                          len-buf enc-buf hmac-buf))
                  (send buf-mgr
                        :get beta-len len-buf 0 +len-prefix-length+)
                  ))))
           )))
      buf-mgr)))  ;; return buf-mgr to caller as ISR interface

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
                    (send write-end :wr-fail cust))
                   (buffers
                    (transmit-next-buffer state))
                   (t
                    (send write-end :wr-done cust))
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
    (flet ((send-fail (cust)
             (send cust :fail starter)
             (send starter starter)))
      (alambda
       ((:wr-done cust)
        (if (zerop (funcall decr-io-count-fn io-state))
            (send-fail cust)
          (send cust :ok starter)))
    
       ((:wr-fail cust)
        (funcall decr-io-count-fn io-state)
        (send-fail cust))
       ))))

(defun make-write-entry-beh (serial starter)
  (alambda

   ((:fail tag) when (eq tag starter)
    ;; Error condition flagged by :FAIL from Starter
    (become (make-sink-beh)))

   ((buffers)
    ;; A list of buffers to write
    (send serial self buffers))
   ))

(defun make-writer (state)
  (actors ((starter  (make-write-starter-beh state ender))
           (ender    (make-write-end-beh state starter))
           (serial   (make-serializer-beh starter))
           (entry    (make-write-entry-beh serial starter)))
    entry))
      
;; -------------------------------------------------------------------------
;; Watchdog Timer - shuts down interface after prologned inactivity

(defun make-kill-timer (timer-fn)
  (let ((timer (mp:make-timer #'mp:funcall-async timer-fn)))
    (make-actor
     (alambda
      ((:resched)
       (mp:schedule-timer-relative timer *socket-timeout-period*))
      ((:discard)
       (mp:unschedule-timer timer)
       (become (make-sink-beh)))
      ))))

;; ------------------------------------------------------------------------
;; Once a buffer fragment has been completely received, we examine what we have

(defun no-service-alert (service node)
  (error "No Service ~S on Node ~A" service node))

(defun make-dispatcher (state)
  (with-accessors ((kill-timer  intf-state-kill-timer)
                   (intf        intf-state-intf)) state
    (make-actor
     (alambda
      
      ((:forwarding-send service . msg)
       ;; the bridge from the other end has forwarded a message to
       ;; an actor on this side
       (send kill-timer :resched)
       (apply #'bridge-deliver-message service
              (make-actor (lambda ()
                            (socket-send intf :no-service
                                         service (machine-instance))
                            ))
              msg))
      
      ((:no-service service node)
       ;; sent to us from the other end on our send to
       ;; non-existent service
       (send kill-timer :resched)
       (mp:funcall-async #'no-service-alert service node))
      
      (msg
       ;; other out-of-band messages - part of a private
       ;; conversation between the two network interfaces
       #|
       (log-info :SYSTEM-LOG
                 "Incoming ~A Msg: ~A" title msg)
       |#
       (send kill-timer :resched)
       (send* intf :incoming-msg msg))
      )))) 

;; ------------------------------------------------------------------------
;; The main user-visible portion of a network interface

(defun create-socket-intf (make-beh-fn &key io-state crypto title accepting-handle)
  (let* ((state (make-intf-state
                 :title    title
                 :crypto   crypto
                 :io-state io-state
                 :accepting-handle accepting-handle))
         (intf  (make-actor (funcall make-beh-fn
                                     (make-socket-beh state)))))
    (with-accessors ((title            intf-state-title)
                     (io-state         intf-state-io-state)
                     (kill-timer       intf-state-kill-timer)
                     (writer           intf-state-writer)
                     (dispatcher       intf-state-dispatcher)
                     (io-running       intf-state-io-running)
                     (decr-io-count-fn intf-state-decr-io-count-fn)) state
      (setf (intf-state-intf state) intf
            kill-timer (make-kill-timer
                        #'(lambda ()
                            (log-info :SYSTEM-LOG "Inactivity shutdown request")
                            (shutdown intf)))
            dispatcher (make-dispatcher state))
        
      (let ((lbl    0)
            (reader (make-reader state)))
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
                     (send reader :add-bytes (incf lbl)
                           (list* 0 end (subseq buffer 0 end))))
                   (comm:async-io-state-discard state end))
                 (when-let (status (or (comm:async-io-state-read-status state)
                                       err-too-large))
                   ;; terminate on any error
                   (comm:async-io-state-finish state)
                   (log-error :SYSTEM-LOG "~A Incoming error state: ~A" title status)
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
          
          (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                  :element-type '(unsigned-byte 8))
          (send kill-timer :resched)
          intf
          )))))

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

;; ------------------------------------------------------------------------

(defun make-sec-beh (state prev-beh tbl msgs)
  (alambda
   ((:send . msg)
    (become (make-sec-beh state prev-beh tbl (cons msg msgs))))
      
   ((:shutdown)
    (%shutdown state))
      
   ((:sec-send @rcust @cust . msg)
    (let ((usti (uuid:make-v1-uuid)))
      (become (make-sec-beh state prev-beh (acons usti @cust tbl) msgs))
      (apply #'%socket-send state :sec-send @rcust usti msg)))
   
   ((:request-srp-negotiation @cust node-id)
    ;; send from client
    (let ((usti (uuid:make-v1-uuid)))
      (become (make-sec-beh state prev-beh (acons usti @cust tbl) msgs))
      (%socket-send state :request-srp-negotiation usti node-id)))
   
   ((:srp-ph3-begin @rcust @cust m2)
    ;; send from server
    (%socket-send state :sec-send @rcust m2)
    (send @cust))
      
   ((:srp-done)
    (dolist (msg msgs)
      (apply #'%socket-send state msg))
    (become prev-beh))
   
   ((:incoming-msg msgkind . msg) when (eq msgkind :SEC-SEND)
    (destructuring-bind (@cust . submsg) msg
      (let ((actor (cdr (assoc @cust tbl :test #'uuid:uuid=))))
        (send* actor submsg))))
   ))

(defun client-request-negotiation-ecc ()
  )

(defun make-socket-beh (state)
  (with-accessors ((crypto  intf-state-crypto)) state
    (alambda
      ((:send . msg)
       (apply #'%socket-send state msg))
    
      ((:shutdown)
       (%shutdown state))
      
      ((:client-request-srp cust)
       (become (make-sec-beh state self-beh nil nil))
       (client-negotiate-security-ecc crypto self cust))

      ((:incoming-msg msgkind . msg) when (eq msgkind :request-srp-negotiation)
       (destructuring-bind (@rcust sender-id) msg
         (become (make-sec-beh state self-beh nil nil))
         (server-negotiate-security-ecc crypto self @rcust sender-id)))
      )))

;; -------------------------------------------------------------

(defun make-client-beh (nom-socket-beh)
  (alambda
   ((:incoming-msg msgkind . msg) when (eq msgkind :SERVER-INFO)
    (let ((server-node (car msg)))
      (bridge-register server-node self)
      (log-info :SYSTEM-LOG "Socket client starting up: ~A" self)
      (become nom-socket-beh)))
   ( msg
     (apply nom-socket-beh msg))
   ))

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
                         (beta ()
                             (send intf :client-request-srp beta)
                           (bridge-pre-register ip-addr intf) ;; anchor for GC
                           (socket-send intf :client-info (machine-instance))))
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

(defun make-server-beh (nom-socket-beh)
  (alambda
   ((:incoming-msg msgkind . msg) when (eq msgkind :CLIENT-INFO)
    (let ((client-node (car msg)))
      (log-info :SYSTEM-LOG "Socket server starting up: ~A" self)
      (socket-send self :server-info (machine-instance))
      (bridge-register client-node self)
      (become nom-socket-beh)))
   ( msg
     (apply nom-socket-beh msg))
   ))

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
  (actors/bridge::make-basic-services)
  (start-tcp-server))

(defun* lw-reset-actor-system _
  (terminate-server sink)
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
