;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
;; This really ought to be an Actor-based manager! The directory is a
;; non-essential service during Actor base startup, so we will make it
;; an Actor-based service after all the base code is in place.

(in-package :actors/directory)

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

(defun need-acceptable-key (name)
  (let ((key (acceptable-key name)))
    (unless key
      (error "Unacceptable name for Actor: ~S" name))
    key))

        ;;; =========== ;;;
;; The Directory utilizes a functional mapping (shareable, immutable).
;; But the pointer to the top of the map is not immutable. We use
;; lock-free accessors/mutators on that top node pointer.
;; (Too bad we don't have Actors in place just yet...)

(defglobal-var *actors-directory* (maps:empty))

(defun clear-directory ()
  (um:wr *actors-directory* (maps:empty)))

(defun current-directory ()
  (um:rd *actors-directory*))

(defun directory-foreach (fn)
  (maps:iter (current-directory) fn))

(defun update-directory (mut-fn)
  (um:rmw *actors-directory* mut-fn))

(defun register-actor (name actor)
  ;; this simply overwrites any existing entry with actor
  (let ((key (need-acceptable-key name)))
    (update-directory (um:rcurry 'maps:add key actor))
    actor))

(defun %remove-key (key)
  (update-directory (um:rcurry 'maps:remove key)))

(defgeneric unregister-actor (actor)
  (:method (name)
   (um:when-let (key (acceptable-key name))
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
   (um:when-let (key (acceptable-key name))
     (maps:find (current-directory) key))))

(defun find-names-for-actor (actor)
  (um:accum acc
    (directory-foreach
     (lambda (k v)
       (when (eq v actor)
         (acc k)))
     )))

;; -------------------------------------------------------
;; in anticipation of networking and distributed SEND

(defmethod ensured-identifier ((actor actor))
  (if (eq actor (current-actor))
      (ensure-identifiable actor)
    (=wait ((id)
            :errorp t
            :timeout *timeout*)
        (inject-into-actor actor
          (=values (ensure-identifiable actor)))
      id)))

(defmethod ensure-identifiable ((actor actor) &optional id)
  (when id
    (setf id (need-acceptable-key id)))
  (inject-into-actor actor
    (let* ((id  (or (get-property actor :usti)
                    (car (find-names-for-actor actor))
                    id
                    (uuid:make-v1-uuid)))
           (found  (find-actor id)))
      (when found
        (unless (eq actor found)
          (error "Conflicting ID for Actor: ~S" id)))
      (unless (eql id (get-property actor :usti))
        (setf (get-property actor :usti) id))
      (unless found
        (register-actor id actor))
      id)))

(defun self-identifier ()
  (ensured-identifier (current-actor)))

;; ------------------------------------------

