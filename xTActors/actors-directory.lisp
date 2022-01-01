;; ----------------------------------------------------------
;; Actors directory -- only for Actors with symbol names or string
;; names.
;;
(in-package :com.ral.actors.directory)

;; ------------------------------------------------------------

(defun acceptable-key (obj)
  (string-upcase (princ-to-string obj)))

        ;;; =========== ;;;
;; Directories utilize a purely functional (immutable) mapping
;; A directory is itself an Actor.

(defun directory-beh (&optional (dir (maps:empty)))
  (alambda
   
   ((cust :register name actor)
    (let ((key (acceptable-key name)))
      (become (directory-beh (maps:add dir key actor)))
      (send cust)
      ))
   
   ((cust :unregister name/actor)
    (if (actor-p name/actor)
        (let ((new-dir (maps:fold dir
                                  (lambda (k v acc)
                                    (if (eq v name/actor)
                                        acc
                                      (maps:add acc k v)))
                                  (maps:empty))))
          (become (directory-beh new-dir)))
      (let ((key (acceptable-key name/actor)))
        (become (directory-beh (maps:remove dir key)))))
    (send cust))
   
   ((cust :clear)
    (become (directory-beh (maps:empty)))
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
  (make-actor (directory-beh)))

(defun find-actor (cust name)
  (send *actors-directory* cust :find-actor name))

(defun register-actor (name actor)
  (send *actors-directory* sink :register name actor))

(defun unregister-actor (name)
  (send *actors-directory* sink :unregister name))

(defun get-actor-names (cust)
  (send *actors-directory* cust :get-actor-names))

