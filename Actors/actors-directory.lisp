;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
;; This really ought to be an Actor-based manager! The directory is a
;; non-essential service during Actor base startup, so we will make it
;; an Actor-based service after all the base code is in place.

(in-package :actors.directory)

(um:eval-always
  (import '(
            um:when-let
            )))

;; ------------------------------------------------------------

(defgeneric acceptable-key (name)
  (:method (obj)
   nil)
  (:method ((obj (eql nil)))
   nil)
  (:method ((sym symbol))
   (acceptable-key (string sym)))
  (:method ((str string))
   (string-upcase str))
  (:method ((index integer))
   index)
  (:method ((usti uuid:uuid))
   usti))

        ;;; =========== ;;;

(defvar *actors-directory* (maps:empty))

(defun clear-directory ()
  (um:wr '*actors-directory* (maps:empty)))

(defun current-directory ()
  (um:rd '*actors-directory*))

(defun directory-foreach (fn)
  (maps:iter (current-directory) fn))

(defun update-directory (mut-fn)
  (um:rmw '*actors-directory* mut-fn))

(defun register-actor (name actor)
  ;; this simply overwrites any existing entry with actor
  (when-let (key (acceptable-key name))
    (update-directory (um:rcurry 'maps:add key actor))
    actor))

(defun %remove-key (key)
  (update-directory (um:rcurry 'maps:remove key)))

(defgeneric unregister-actor (actor)
  (:method (name)
   (when-let (key (acceptable-key name))
     (%remove-key key)))
  (:method ((actor actor))
   (directory-foreach
    (lambda (k v)
      (when (eq v actor)
        (%remove-key k)))
    )))

(defun get-actors ()
  (um:accum acc
    (directory-foreach
     (lambda (k v)
       (acc (cons k v))))
    ))

(defun get-actor-names ()
  (mapcar #'car (get-actors)))

(defgeneric find-actor (actor)
  (:method ((actor actor))
   actor)
  (:method (name)
   (when-let (key (acceptable-key name))
     (maps:find (current-directory) key))))

(defun find-names-for-actor (actor)
  (um:accum acc
    (directory-foreach
     (lambda (k v)
       (when (eq v actor)
         (acc k)))
     )))

;; ------------------------------------------

