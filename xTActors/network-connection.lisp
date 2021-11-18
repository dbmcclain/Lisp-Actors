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

(um:eval-always
  (import '(um:when-let
            um:wr

             ac-secure-comm:local-services
             ac-secure-comm:global-services
             ac-secure-comm:server-gateway
             ac-secure-comm:start-server-gateway
             )))

;; -----------------------------------------------------------------------

(defvar *default-port*            65001.)
(defvar *socket-timeout-period*   20.)
(defvar *ws-collection*           nil)
(defvar *aio-accepting-handle*    nil)

(defconstant +MAX-FRAGMENT-SIZE+ 65536.)

;; -------------------------------------------------------------

(defstruct intf-state
  title
  ip-addr
  io-state
  accepting-handle
  kill-timer
  (io-running (list 1))
  decr-io-count-fn
  shutdown)

;; -------------------------------------------------------------------------
;; Socket writer

(defun physical-writer-beh (state)
  (with-accessors ((decr-io-count  intf-state-decr-io-count-fn)
                   (state-io-state intf-state-io-state)
                   (io-running     intf-state-io-running)
                   (kill-timer     intf-state-kill-timer)) state
    (lambda (cust byte-vec)
      (let ((me  self))
        (labels
            ((finish-fail (io-state)
               (funcall decr-io-count io-state)
               (send cust me :fail))
             (finish-ok (io-state)
               (send cust me
                     (if (zerop (funcall decr-io-count io-state))
                         :fail
                       :ok)))
             (write-done (io-state &rest ignored)
               ;; this is a callback routine, executed in the thread of
               ;; the async collection
               (declare (ignore ignored))
               (cond ((comm:async-io-state-write-status io-state)
                      (finish-fail io-state))
                     (t
                      (finish-ok io-state))
                     )))
          (cond
           ((sys:compare-and-swap (car io-running) 1 2) ;; still running recieve?
            (comm:async-io-state-write-buffer state-io-state
                                              byte-vec #'write-done)
            (send kill-timer :resched))
           
           (t
            (send cust self :fail))
           ))))))

(defun writer-beh (state phys-write)
  (lambda (byte-vec)
    (send phys-write self byte-vec)
    (become (pending-writer-beh state phys-write +emptyq+))
    ))

(defun pending-writer-beh (state phys-write pend)
  (alambda
   ((:send byte-vec)
    (become (pending-writer-beh state phys-write (addq pend byte-vec))))

   ((a-cust :ok) when (eq a-cust phys-write)
    (if (emptyq? pend)
        (become (writer-beh state phys-write))
      (multiple-value-bind (byte-vec new-queue) (popq pend)
        (send phys-write self byte-vec)
        (become (pending-writer-beh state phys-write new-queue))
        )))

   ((a-cust :fail) when (eq a-cust phys-write)
    (send (intf-state-shutdown state))
    (become (sink-beh)))
   ))

(defun make-writer (state)
  (actors ((writer     (writer-beh state phys-write))
           (phys-write (physical-writer-beh state)))
    writer))

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
       (become (sink-beh)))
      ))))

;; ------------------------------------------------------------------------
;; The main user-visible portion of a network interface

(defun make-server-handler (sender)
  (make-actor
   (alambda
    ((:connect cust-id . msg)
     (send* (server-gateway) :connect cust-id sender msg))

    ((_ :send . _)
     (send* (local-services) self-msg))
    )))

(defun create-socket-intf (&key kind ip-addr io-state accepting-handle)
  (let* ((title (if (eq kind :client) "Client" "Server"))
         (state (make-intf-state
                 :title            title
                 :ip-addr          ip-addr
                 :io-state         io-state
                 :accepting-handle accepting-handle))
         (encoder (sink-pipe (marshal-encoder)
                             (chunker :max-size (- +max-fragment-size+ 500))
                             (marshal-encoder)
                             (make-writer state)))
         (handler (if (eq kind :client)
                      (local-services)  ; clients only hear :reply
                    (make-server-handler encoder)))
         (decoder (sink-pipe (marshal-decoder)
                             (dechunker)
                             (marshal-decoder)
                             handler))
         (shutdown (make-socket-shutdown state)))

    (with-accessors ((title            intf-state-title)
                     (io-state         intf-state-io-state)
                     (kill-timer       intf-state-kill-timer)
                     (io-running       intf-state-io-running)
                     (decr-io-count-fn intf-state-decr-io-count-fn)) state
      
      (setf kill-timer (make-kill-timer
                        #'(lambda ()
                            (send println "Inactivity shutdown request")
                            (send shutdown)))
            (intf-state-shutdown state) shutdown)
        
      (labels
          ((rd-callback-fn (state buffer end)
             ;; callback for I/O thread - on continuous async read
             #|
             (send println (format nil "Socket Reader Callback (STATUS = ~A, END = ~A)"
                                   (comm:async-io-state-read-status state)
                                   end))
             |#
             (let (err-too-large)
               (when (plusp end)
                 ;; (send println (format nil "~A Incoming bytes: ~A" title buffer))
                 (if (> end +max-fragment-size+)
                     (setf err-too-large "Incoming packet too large")
                   (progn
                     (send decoder (subseq buffer 0 end))
                     (send kill-timer :resched)))
                 (comm:async-io-state-discard state end))
               (um:when-let (status (or (comm:async-io-state-read-status state)
                                     err-too-large))
                 ;; terminate on any error
                 (comm:async-io-state-finish state)
                 (send println (format nil "~A Incoming error state: ~A" title status))
                 (decr-io-count state))
               ))
               
           (decr-io-count (io-state)
             (let ((ct (sys:atomic-fixnum-decf (car io-running))))
               (when (zerop ct) ;; >0 is running
                 (comm:close-async-io-state io-state)
                 (send println "Connection Shutdown")
                 (send shutdown))
               ct)))

        (setf decr-io-count-fn #'decr-io-count)
        
        (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                :element-type '(unsigned-byte 8))
        (send kill-timer :resched)
        (values state encoder)
        ))))

;; -------------------------------------------------------------

(defun make-socket-shutdown (state)
  (actor ()
    (with-accessors ((kill-timer       intf-state-kill-timer)
                     (io-running       intf-state-io-running)
                     (io-state         intf-state-io-state)
                     (accepting-handle intf-state-accepting-handle)
                     (title            intf-state-title)
                     (ip-addr          intf-state-ip-addr)) state
      (send kill-timer :discard)
      (wr (car io-running) 0)
      (comm:async-io-state-abort-and-close io-state)
      (when accepting-handle
        (um:deletef (comm:accepting-handle-user-info accepting-handle) state))
      (send println (format nil "~A Socket (~S) shutting down"
                            title ip-addr))
      (send (connections) sink :remove state)
      (become (sink-beh))
      )))

;; -------------------------------------------------------------

(defun empty-connections-list ()
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :add-socket ip-addr ip-port state sender)
    (let ((next (make-actor self-beh)))
      (become (connection-node ip-addr ip-port state sender nil next))
      (send cust :ok)))

   ((:on-find-sender _ _ _ if-not-found)
    (send if-not-found))

   ((cust :remove . _)
    (send cust :ok))
   ))

(defun connection-node (ip-addr ip-port state sender cnx next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :add-socket an-ip-addr an-ip-port new-state new-sender) when (and (eql an-ip-addr ip-addr)
                                                                                (eql an-ip-port ip-port))
    (become (connection-node ip-addr ip-port new-state new-sender cnx next))
    (send cust :ok))

   ((:on-find-sender an-ip-addr an-ip-port cust . _) when (and (eql an-ip-addr ip-addr)
                                                               (eql an-ip-port ip-port))
    (send cust sender cnx))

   ((cust :add-connection a-sender new-cnx) when (eq a-sender sender)
    (become (connection-node ip-addr ip-port state sender new-cnx next))
    (send cust :ok))

   ((cust :remove a-state) when (eq a-state state)
    (prune-self next)
    (send cust :ok))
   
   (_
    (repeat-send next))
   ))

(defvar *connections* nil)

(defun connections ()
  (or *connections*
      (setf *connections* (make-actor (empty-connections-list)))))

;; -------------------------------------------------------------

(defun canon-ip-addr (ip-addr)
  (comm:get-host-entry ip-addr :fields '(:address)))

(defvar *client-connector* nil)

(defun client-connector ()
  (or *client-connector*
      (setf *client-connector* (make-client-connector))
      ))

(defun make-client-connector ()
  (serializer
   (actor (cust ip-addr &optional (ip-port *default-port*))
     ;; Called from client side wishing to connect to a server.
     ;;
     ;; Because we are serialized, one way or another, we have to exit
     ;; by sending to cust, or execute serializer-abort.
     (let ((clean-ip-addr (canon-ip-addr ip-addr)))
       (cond ((null clean-ip-addr)
              (serializer-abort cust)
              (err "Unknown host: ~A" ip-addr))
             (t
              (send (connections) :on-find-sender clean-ip-addr ip-port
                    cust      ;; send to cust if found
                    (actor () ;; else send to this if not found
                      (let ((k-stop  (actor ()
                                       (serializer-abort cust)
                                       (err "Can't connect to: ~A" ip-addr)))
                            (k-start (actor (io-state)
                                       (multiple-value-bind (state sender)
                                           (create-socket-intf :kind     :client
                                                               :ip-addr  ip-addr
                                                               :io-state io-state)
                                         (beta _
                                             (send (connections) beta :add-socket
                                                   clean-ip-addr ip-port state sender)
                                           (send println
                                                 (format nil "Client Socket (~S) starting up"
                                                         (intf-state-ip-addr state)))
                                           (send cust sender nil)))
                                       )))
                        (mp:funcall-async
                         (lambda ()
                           (comm:create-async-io-state-and-connected-tcp-socket
                            *ws-collection*
                            clean-ip-addr ip-port
                            (lambda (state args)
                              (cond (args
                                     (send println :CONNECTION-ERROR
                                           (apply #'format nil args))
                                     (send k-stop))
                                    (t
                                     (send k-start state))
                                    ))
                            :handshake-timeout 5
                            #-:WINDOWS :ipv6    #-:WINDOWS nil)))
                        ))))
             )))))

;; -------------------------------------------------------------

(defvar *server-count* 0)

(defun start-server-messenger (accepting-handle io-state)
  "Internal routine to start a network interface from the server side.
The interface is identical to that on the client side, but the
connection handshake differs from this side.

See the discussion under START-CLIENT-MESSENGER for details."

  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (multiple-value-bind (state sender)
      (create-socket-intf :kind             :server
                          :ip-addr          (format nil "~A#~D" (machine-instance) (incf *server-count*))
                          :io-state         io-state
                          :accepting-handle accepting-handle)
    ;; for server side, this user-info is the only reference to intf
    ;; until we get registered into the ip-mapping table.
    (send (connections) :add-socket nil nil state sender)
    (push state (comm:accepting-handle-user-info accepting-handle))
    (send println
          (format nil "Server Socket (~S) starting up"
                  (intf-state-ip-addr state)))
    ))

;; --------------------------------------------------------------

(defun terminate-server (reply-to)
  (if *aio-accepting-handle*
      (progn
        (setf (comm:accepting-handle-user-info *aio-accepting-handle*) nil)
        (comm:close-accepting-handle *aio-accepting-handle*
                                     (lambda (coll)
                                       ;; we are operating in the collection process
                                       (comm:close-wait-state-collection coll)
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
  (let ((starter (actor _
                   (setq *ws-collection*
                         (comm:create-and-run-wait-state-collection "Actor Server"))
                   (setq *aio-accepting-handle* 
                         (comm:accept-tcp-connections-creating-async-io-states
                          *ws-collection*
                          tcp-port-number
                          #'start-server-messenger
                          :ipv6    nil
                          ))
                   (send println (format nil "Actor Server started on port ~A" tcp-port-number)))))
    (terminate-server starter)))

;; --------------------------------------------------
;;

(defun reset-global-state ()
  (setf *ws-collection*        nil
        *aio-accepting-handle* nil
        *connections*          nil
        *client-connector*     nil))

(defun* lw-start-tcp-server _
  ;; called by Action list with junk args
  ;;
  ;; We need to delay the construction of the system logger till this
  ;; time so that we get a proper background-error-stream.  Cannot be
  ;; performed on initial load of the LFM.
  (assert (null *ws-collection*))
  (assert (null *aio-accepting-handle*))
  (start-server-gateway)
  (start-tcp-server))

(defun* lw-reset-actor-system _
  (terminate-server sink)
  (reset-global-state)
  (print "Actor Server has been shut down."))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor Server"
                    'lw-start-tcp-server
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor Server"
                    'lw-reset-actor-system)

  (lw:define-action "Save Session After"
                    "Restart Actor Server"
                    'lw-start-tcp-server)
  )

(defun ac:start ()
  (lw-start-tcp-server))

(if (mp:get-current-process)
    (unless *ws-collection*
      (lw-start-tcp-server))
  (pushnew '("Start Actor Server" () lw-start-tcp-server) mp:*initial-processes*
           :key #'third))
