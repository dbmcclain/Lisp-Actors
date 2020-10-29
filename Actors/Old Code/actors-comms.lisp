
(in-package #:actors-base)

;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
;; This really ought to be an Actor-based manager! The directory is a
;; non-essential service during Actor base startup, so we will make it
;; an Actor-based service after all the base code is in place.

(defun directory-manager-p ()
  (typep *actor-directory-manager* 'Actor))

        ;;; =========== ;;;

(defmethod acceptable-key (name)
  nil)

(defmethod acceptable-key ((name (eql nil)))
  nil)

(defmethod acceptable-key ((name symbol))
  (and (symbol-package name)
       (acceptable-key (string name))))

(defmethod acceptable-key ((name string))
  (string-upcase name))

        ;;; =========== ;;;

(defmethod register-actor ((actor actor) name)
  (when (acceptable-key name)
    (send *actor-directory-manager* :register actor name)))
  
(defun unregister-actor (name-or-actor)
  (send *actor-directory-manager* :unregister name-or-actor))

(defun get-recorded-actors ()
  (when (directory-manager-p)
    (ask *actor-directory-manager* :get-all)))

(defun find-actor-in-directory (name)
  (when (and (directory-manager-p)
             (acceptable-key name))
    (ask *actor-directory-manager* :find name)))

(defmethod find-actor-name ((actor actor))
  (when (directory-manager-p)
    (ask *actor-directory-manager* :reverse-lookup actor)))

;; --------------------------------------------------------
;; Shared printer driver... another instance of something better
;; placed into an Actor

(defun pr (&rest things-to-print)
  (apply #'send *shared-printer-actor* :print things-to-print))

;; --------------------------------------------------------------------
;; External communication with an Actor

(defmethod priority-send ((actor actor) prio &rest message)
  (mailbox-send (actor-messages actor) message :prio prio)
  (add-to-ready-queue actor))

;; ---------------------------------------------------------

(defmethod send ((actor actor) &rest message)
  (apply #'priority-send actor 0 message)
  (values))

(defmethod send ((mbox mp:mailbox) &rest message)
  (mp:mailbox-send mbox message))

(defmethod send ((mbox prio-mailbox) &rest message)
  (mailbox-send mbox message))

(defmethod send ((ch rch:channel) &rest message)
  (rch:poke ch message))

(defmethod send ((fn function) &rest message)
  (apply fn message))

(defmethod send ((sym symbol) &rest message)
  (if-let (actor (find-actor sym))
      (apply #'send actor message)
    (if (fboundp sym)
        (apply sym message)
      (call-next-method))))

(defmethod send ((str string) &rest message)
  (if-let (actor (find-actor str))
      (apply #'send actor message)
    (call-next-method)))

(defun funcallable-p (obj)
  (or (functionp obj)
      (and (symbolp obj)
           (fboundp obj))))

(defmethod send (other-obj &rest message)
  (let ((mfn (car message)))
    (when (funcallable-p mfn)
      (apply mfn other-obj (cdr message))
      )))

;; ---------------------------------------------------------

(defstruct rpc-query
  replyTo query)

(defun huh!? ()
  (error "Huh!?"))

(defmethod ask (obj &rest message)
  (let ((mfn (car message)))
    (if (funcallable-p mfn)
        (apply mfn obj (cdr message))
      (Huh!?))))

(defmethod ask ((fn function) &rest message)
  (apply fn message))

(defmethod ask ((sym symbol) &rest message)
  (if-let (actor (find-actor sym))
      (apply #'ask actor message)
    (if (fboundp sym)
        (apply sym message)
      (call-next-method))))

(defmethod ask ((str string) &rest message)
  (if-let (actor (find-actor str))
      (apply #'ask actor message)
    (call-next-method)))

;; ------------------------------------------
;; A mailbox repository...
;; ... some things just can't be turned into an Actor service...

(defun make-queryer ()
  (let* ((mb     (mp:make-mailbox))
         (packet (make-rpc-query
                  :replyTo mb)))
    (lambda (dest query)
      (setf (rpc-query-query packet) query)
      (send dest packet)
      (unwind-protect
          ;; response comes via SEND, and mailbox message from SEND is
          ;; always a list
          (apply #'um.dispq:recover-ans-or-exn (mp:mailbox-read mb))
        (setf (rpc-query-query packet) nil)) ;; help out GC
      )))


(let ((queue (list nil)))
  
  (defun ask-with-borrowed-queryer (actor query)
    (let ((queryer (or (sys:atomic-pop (car queue))
                       (make-queryer))))
      (unwind-protect
          (funcall queryer actor query)
        (sys:atomic-push queryer (car queue))
        ))))

;; ----------------------------------------

(defmethod ask ((actor actor) &rest message)
  (ask-with-borrowed-queryer actor message))

;; ------------------------------------------------------------
;; Internal routines for constructing an Actor and enabling it

(defmethod actor-alive-p ((actor actor))
  (and (actor-behavior actor)
       actor))

;; --------------------------------------------------------
;; Directory Introspection

(def-alias get-actors get-recorded-actors)

(defmethod find-actor ((actor actor))
  (actor-alive-p actor))

(defun find-live-actor-in-directory (name)
  (find-actor (find-actor-in-directory name)))

(defmethod find-actor ((name string))
  (find-live-actor-in-directory name))

(defmethod find-actor ((name symbol))
  (find-live-actor-in-directory name))

(defmethod find-actor ((actor (eql nil)))
  nil)

