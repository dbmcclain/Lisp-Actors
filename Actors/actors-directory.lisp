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

(defun make-actor-directory ()
  (maps:make-shared-map))

(defun clear-directory (&key (directory *actors-directory*))
  (maps:erase directory))

(defun register-actor (name actor &key directory)
  ;; this simply overwrites any existing entry with actor
  (let ((key       (acceptable-key name))
        (directory (or directory *actors-directory*)))
    (setf (get-property actor 'directory-keys)
          (adjoin (cons key directory) (get-property actor 'directory-keys)
                  :key  #'first
                  :test #'string-equal))
    (maps:add directory key actor)
    actor))

(defun eligible? (dir directory directory-provided?)
  ;; conditions under which dir is an acceptable directory
  (or (and directory-provided?
           (or (null directory)
               (eq dir directory)))
      (not directory-provided?)))

(defgeneric unregister-actor (actor &key directory)
  (:method (name &key directory)
   (um:when-let (key (acceptable-key name))
     (maps:remove (or directory *actors-directory*) key)))
  
  (:method ((actor actor) &key (directory nil directory-provided?))
   (dolist (key-pair (get-property actor 'directory-keys))
     (destructuring-bind (key . dir) key-pair
       (when (eligible? dir directory directory-provided?)
         (maps:remove dir key))
       ))))

(defun get-actors (&key directory)
  (let ((directory (or directory *actors-directory*)))
    (um:accum acc
      (maps:iter directory
                 (lambda (k v)
                   (acc (cons k v))))
      )))

(defun get-actor-names (&key directory)
  (mapcar #'car (get-actors :directory directory)))

(defgeneric find-actor (actor &key directory)
  (:method ((actor actor) &key directory)
   (declare (ignore directory))
   actor)

  (:method (name &key directory)
   (let ((key       (acceptable-key name))
         (directory (or directory *actors-directory*)))
     (maps:find directory key))))

(defun find-names-for-actor (actor &key (directory nil directory-provided?))
  (mapcan (lambda (pair)
            (destructuring-bind (name . dir) pair
              (when (eligible? dir directory directory-provided?)
                (list name))))
          (get-property actor 'directory-keys)))

;; -------------------------------------------------------
;; in anticipation of networking and distributed SEND

(defmethod ensured-identifier ((actor actor) &key directory id)
  (if (current-actor)
      (ensure-identifiable actor :directory directory :id id)
    (=wait ((id)
            :errorp t
            :timeout *timeout*)
        (inject-into-actor actor
          (=values (ensure-identifiable actor :directory directory :id id)))
      id)))

(defmethod ensure-identifiable ((actor actor) &key directory id)
  (let ((id  (when id (acceptable-key id))))
    (inject-into-actor actor
      (let* ((id  (or (get-property actor :usti)
                      (car (find-names-for-actor actor :directory directory))
                      id
                      (uuid:make-v1-uuid)))
             (found  (find-actor id :directory directory)))
        (when found
          (unless (eq actor found)
            (error "Conflicting ID for Actor: ~S" id)))
        (unless (eql id (get-property actor :usti))
          (setf (get-property actor :usti) id))
        (unless found
          (register-actor id actor :directory directory))
        id))))

(defun self-identifier ()
  (ensured-identifier (current-actor)))

;; ------------------------------------------

