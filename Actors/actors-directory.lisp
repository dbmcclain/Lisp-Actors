;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
;; This really ought to be an Actor-based manager! The directory is a
;; non-essential service during Actor base startup, so we will make it
;; an Actor-based service after all the base code is in place.

(in-package :actors/directory)

;; ------------------------------------------------------------

(defun acceptable-key (obj)
  (string-upcase (princ-to-string obj)))

        ;;; =========== ;;;
;; The Directory utilizes a functional mapping (shareable, immutable).
;; But the pointer to the top of the map is not immutable. We use
;; lock-free accessors/mutators on that top node pointer.
;; (Too bad we don't have Actors in place just yet...)

(defglobal-var *actors-directory* (maps:make-shared-map))

(defun clear-directory ()
  (maps:erase *actors-directory*))

(defun register-actor (name actor)
  ;; this simply overwrites any existing entry with actor
  (let ((key (acceptable-key name)))
    (setf (get-property actor 'directory-keys)
          (adjoin key (get-property actor 'directory-keys)
                  :test #'string-equal))
    (maps:add *actors-directory* key actor)
    actor))

(defgeneric unregister-actor (actor)
  (:method (name)
   (um:when-let (key (acceptable-key name))
     (maps:remove *actors-directory* key)))
  (:method ((actor actor))
   (dolist (key (get-property actor 'directory-keys))
     (maps:remove *actors-directory* key))))

(defun get-actors ()
  (um:accum acc
    (maps:iter *actors-directory*
               (lambda (k v)
                 (acc (cons k v))))
    ))

(defun get-actor-names ()
  (mapcar #'car (get-actors)))

(defgeneric find-actor (actor)
  (:method ((actor actor))
   actor)
  (:method (name)
   (let ((key (acceptable-key name)))
     (maps:find *actors-directory* key))))

(defun find-names-for-actor (actor)
  (get-property actor 'directory-keys))

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
    (setf id (acceptable-key id)))
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

