;; actor-bridge.lisp -- The bridge needed to allow Actors to remotely interact
;;
;; When an Actor target is a string specified as
;; "service.ip-addr:port" this package re-routes the SEND/ASK to a
;; socket interface, connecting first to a remote server if needed.
;;
;; Keeps a local record of pending callbacks, and serializes them by
;; using a unique string for remote reference.
;;
;; DM/RAL 10/20
;; -----------------------------------------------------------------------------

(in-package :actors.bridge)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            um:if-let
            um:when-let
            um:recover-ans-or-exn

            actors.network:open-connection
            actors.network:socket-send
            )))

;; ---------------------------------------------------------------------------------

(defclass actor-bridge (actor)
  ((conts :accessor actor-bridge-conts :initform (maps:empty))
   (dests :accessor actor-bridge-dests :initform (maps:empty))
   ))

(defvar *bridge* (make-instance 'actor-bridge))

(defun actor-bridge ()
  ;; Return the singleton instance of BRIDGE. Don't bother
  ;; registering, because it is not generally useful to end users.
  *bridge*)

(defstruct ip-dest
  service ip-addr port)

;; --------------------------------------------------------------------------------
;; Helper Functions

(defmethod parse-destination ((dest ip-dest))
  (with-slots (service ip-addr port) dest
    (values service ip-addr port)))

(defmethod parse-destination ((dest string))
  (let ((pos-at    (position #\@ dest))
        (pos-colon (position #\: dest))
        start
        service
        ip-addr
        ip-port)
    (if pos-at
        (setf service (string-upcase (subseq dest 0 pos-at))
              start   (1+ pos-at))
      (setf start 0))
    (if pos-colon
        (setf ip-addr (string-upcase (subseq dest start (- pos-colon start)))
              ip-port (subseq dest (1+ pos-colon)))
      (setf ip-addr (string-upcase (subseq dest start))))
    (values service ip-addr ip-port)))

(defun find-handler (dest-ip dest-port)
  (or (maps:find (actor-bridge-dests (current-actor)) dest-ip)
      (open-connection dest-ip dest-port)))
        
(defun call-with-valid-dest (dest fn)
  (multiple-value-bind (service dest-ip dest-port)
      (parse-destination dest)
    (when (and service
               dest-ip)
      (when-let (handler (find-handler dest-ip dest-port))
        (funcall fn service handler)))
    ))

(defmacro with-valid-dest ((service handler dest) &body body)
  `(call-with-valid-dest ,dest (lambda (,service ,handler)
                                 ,@body)))

;; ---------------------------------------------------------------------
;; Register / Connect to socket handler

(defun bridge-register (ip-addr handler)
  ;; called by socket handler to register a new connection
  (with-slots (dests) *bridge*
    (perform-in-actor *bridge*
      (maps:addf dests (string-upcase ip-addr) handler)
      )))

(defun bridge-unregister (handler)
  ;; called by socket handler on socket shutdown
  (with-slots (dests conts) *bridge*
    (perform-in-actor *bridge*
      (maps:iter dests
                 (lambda (k v)
                   (when (eq handler v)
                     (maps:removef dests k))))
      (maps:iter conts
                 (lambda (k v)
                   (when (eq handler (first v))
                     (maps:removef conts k))))
      )))
        
(defun bridge-reset ()
  ;; called when all socket I/O is shutdown
  (with-slots (dests conts) *bridge*
    (perform-in-actor *bridge*
      (setf conts (maps:empty)
            dests (maps:empty))
      )))

;; -----------------------------------------------------------------------

(defun forward-query (handler service cont &rest msg)
  (with-slots (conts) *bridge*
    (let ((id (uuid:make-v1-uuid)))
      (maps:addf conts id (list handler cont))
      (apply 'socket-send handler 'actor-internal-message:forwarding-ask service id msg))
    ))

(defun bridge-forward-message (dest &rest msg)
  ;; called by SEND as a last resort
  (perform-in-actor *bridge*
    (with-valid-dest (service handler dest)
      (case (car msg)
        ((actor-internal-message:ask)
         (apply 'forward-query handler service (cadr msg) (cddr msg)))
        
        (otherwise
         (apply 'socket-send handler 'actor-internal-message:forwarding-send service msg))
        ))))

(=defun bridge-ask-query (dest &rest msg)
  ;; called by ASK as a last resort
  (perform-in-actor *bridge*
    (with-valid-dest (service handler dest)
      (apply 'forward-query handler service =bind-cont msg)
      )))

(defun bridge-handle-reply (id &rest reply)
  ;; called by socket handler when a reply arrives
  (with-slots (conts) *bridge*
    (perform-in-actor *bridge*
      (when-let (cont (second (maps:find conts id)))
        (maps:removef conts id)
        (apply cont reply))
      )))

;; -----------------------------------------------------------------------

(register-actor :echo
                (make-actor
                 (lambda (&rest msg)
                   msg)))

;; -----------------------------------------------------------------

(defun cmpfn (&rest args)
  (compile nil `(lambda ()
                  ,@args)))

(register-actor :eval
                (make-actor
                 (lambda (&rest msg)
                   (funcall (apply #'cmpfn msg)))))

;; ----------------------------------------------------------------------

(defmethod send ((str string) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor str))
      (apply 'send actor message))
     
     ((find #\@ str)
      (apply 'bridge-forward-message str message))

     (t
      (call-next-method))
     )))

(defmethod ask ((str string) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor str))
      (apply 'ask actor message))
     
     ((find #\@ str)
      (=wait (ans) (:timeout *timeout* :errorp t)
          (=apply 'bridge-ask-query str message)
        (recover-ans-or-exn ans)))
     
     (t
      (call-next-method))
     )))

(=defmethod =ask ((str string) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor str))
      (=apply '=ask actor message))
     
     ((find #\@ str)
      (=bind (ans)
          (=apply 'bridge-ask-query str message)
        (=values (recover-ans-or-exn ans))))
     
     (t
      (call-next-method))
     )))
