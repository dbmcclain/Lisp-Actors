;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
;; This really ought to be an Actor-based manager! The directory is a
;; non-essential service during Actor base startup, so we will make it
;; an Actor-based service after all the base code is in place.

(in-package :actors.directory)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            actors.base:actor

            um:when-let
            )))

;; ------------------------------------------------------------

(defmethod acceptable-key (name)
  nil)

(defmethod acceptable-key ((name (eql nil)))
  nil)

(defmethod acceptable-key ((name symbol))
  (and (symbol-package name) ;; why would we care about this?
       (acceptable-key (string name))))

(defmethod acceptable-key ((name string))
  (string-upcase name))

(defmethod acceptable-key ((index integer))
  index)

        ;;; =========== ;;;

(defvar actors-directory (maps:empty))

(defun clear-directory ()
  (um:rmw 'actors-directory (constantly (maps:empty))))

(defun %remove-key (key)
  (um:rmw 'actors-directory (um:rcurry 'maps:remove key)))

(defun register-actor (name actor)
  ;; this simply overwrites any existing entry with actor
  (when-let (key (acceptable-key name))
    (um:rmw 'actors-directory (um:rcurry 'maps:add key actor))
    actor))

(defmethod unregister-actor ((actor actor))
  (maps:iter actors-directory
             (lambda (k v)
               (when (eq v actor)
                 (%remove-key k)))
             ))

(defmethod unregister-actor (name)
  (when-let (key (acceptable-key name))
    (%remove-key key)))

(defun get-actors ()
  (um:accum acc
    (maps:iter actors-directory
               (lambda (k v)
                 (acc (cons k v))))
    ))

(defun get-server-actors ()
  (um:accum acc
    (maps:iter actors-directory
               (lambda (k v)
                 (declare (ignore v))
                 (acc k)))
    ))

(defmethod find-actor ((actor actor))
  actor)

(defmethod find-actor (name)
  (when-let (key (acceptable-key name))
    (maps:find actors-directory key)))

(defmethod find-names-for-actor ((actor actor))
  (let (keys)
    (maps:iter actors-directory
               (lambda (k v)
                 (when (eq v actor)
                   (push k keys))))
    (nreverse keys)))

;; ------------------------------------------

