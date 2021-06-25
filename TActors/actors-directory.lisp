;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
(in-package :actors/directory)

(um:eval-always
  (import '(actors/base:retry-send)))

;; ------------------------------------------------------------

(defun acceptable-key (obj)
  (string-upcase (princ-to-string obj)))

        ;;; =========== ;;;
;; Directories utilize a purely functional (immutable) mapping
;; A directory is itself an Actor.

(defun make-directory-beh (&optional (dir (maps:empty)))
  (alambda
   
   ((cust :register name actor)
    (let ((key (acceptable-key name)))
      (become (make-directory-beh (maps:add dir key actor)))
      (send cust)
      ))
   
   ((cust :unregister name/actor)
    (if (actor-trait-p name/actor)
        (let ((new-dir (maps:fold dir
                                  (lambda (k v acc)
                                    (if (eq v name/actor)
                                        acc
                                      (maps:add acc k v)))
                                  (maps:empty))))
          (become (make-directory-beh new-dir)))
      (let ((key (acceptable-key name/actor)))
        (become (make-directory-beh (maps:remove dir key)))))
    (send cust))
   
   ((cust :clear)
    (become (make-directory-beh (maps:empty)))
    (send cust))
   
   ((cust :get-actors)
    (send cust (um:accum acc
                 (maps:iter dir
                            (lambda (k v)
                              (acc (cons k v)))))
          ))
   ((cust :get-actor-names)
    (send cust (um:accum acc
                 (maps:iter dir
                            (lambda (k v)
                              (declare (ignore v))
                              (acc k))))
          ))
   ((cust :find-actor name)
    (let ((key (acceptable-key name)))
      (send cust (maps:find dir key))))
   
   ((cust :find-names-for-actor actor)
    (send cust (um:accum acc
                 (maps:iter dir
                            (lambda (k v)
                              (when (eq actor v)
                                (acc k)))))
          ))
   ))

(defvar *actors-directory*
  (make-actor (make-directory-beh)))

(defmethod find-actor ((cust actor-trait) name)
  (send *actors-directory* cust :find-actor name))

(defun register-actor (name actor)
  (send *actors-directory* sink :register name actor))

(defun unregister-actor (name)
  (send *actors-directory* sink :unregister name))

(defun get-actor-names (cust)
  (send *actors-directory* cust :get-actor-names))

;; ------------------------------------------
;; Sends directed to mailboxes, functions, etc.

(define-condition invalid-send-target (simple-error)
  ((target :initarg :target :initform nil :accessor target))
  (:documentation "An error indicating a target of SEND that cannot be resolved into something valid.")
  (:report (lambda (condition stream)
	     (format stream "~%Invalid SEND target: ~&  ~S" (target condition)))))

(defmethod retry-send ((obj actor-trait) &rest msg)
  (send* obj msg))

(defmethod retry-send ((obj sponsor) &rest msg)
  (send* obj msg))

(defmethod retry-send (obj &rest msg)
  (declare (ignore msg))
  ;; This is not a good idea from a security standpoint...
  ;; Better to just remain silent.
  ;; (error 'invalid-send-target :target obj)
  )

(defmethod send (rcvr &rest msg)
  (beta (a)
      (find-actor beta rcvr)
    (apply #'retry-send a msg)))

