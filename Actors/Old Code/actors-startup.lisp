
(in-package :actors-base)

;; ----------------------------------------------------------------------------

(defun install-actor-directory ()
  (setf *actor-directory-manager*
        (make-actor ()
            ((directory (make-hash-table
                         :test 'equal
                         :single-thread t))
             (rev-directory (make-hash-table
                             :test 'eq
                             :single-thread t)))
          
          (labels ((clean-up ()
                     (setf *actor-directory-manager* #'lw:do-nothing)))
            
            (um:dlambda
              (:clear ()
               (clrhash directory))
              
              (:register (actor name)
               ;; this simply overwrites any existing entry with actor
               (um:when-let (key (acceptable-key name))
                 (setf (gethash key directory) actor
                       (gethash actor rev-directory) key)))
              
              (:unregister (name-or-actor)
               (cond ((typep name-or-actor 'Actor)
                      (um:when-let (key (gethash name-or-actor rev-directory))
                        (remhash key directory)
                        (remhash name-or-actor rev-directory)))
                     (t
                      (um:when-let (key (acceptable-key name-or-actor))
                        (um:when-let (actor (gethash key directory))
                          (remhash key directory)
                          (remhash actor rev-directory))))
                     ))
              
              (:get-all ()
               (let (actors)
                 (maphash (lambda (k v)
                            (setf actors (acons k v actors)))
                          directory)
                 (sort actors #'string-lessp :key #'car)))
              
              (:find (name)
               (um:when-let (key (acceptable-key name))
                 (gethash key directory)))
              
              (:reverse-lookup (actor)
               (gethash actor rev-directory))
              
              (:quit ()
               (clean-up))
              ))))
  (register-actor *actor-directory-manager* :ACTOR-DIRECTORY)
  (pr "Actor Directory created..."))

(defun install-actor-printer ()
  (setf *shared-printer-actor*
        (make-actor () ()
          (um:dlambda
            (:print (&rest things-to-print)
             (dolist (item things-to-print)
               (print item)))
            
            (:quit ()
             (setf *shared-printer-actor* #'blind-print))
            )))
  (register-actor *shared-printer-actor* :SHARED-PRINTER))

(defun install-actor-system (&rest ignored)
  (declare (ignore ignored))
  (install-actor-directory)
  (install-actor-printer))

#||#
#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))
  
  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actors"
                    'install-actor-system
                    :after "Run the environment start up functions"
                    :once))
#||#
