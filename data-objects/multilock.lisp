
(in-package :multilock)

;; --------------------------------------

(defclass multilock (<orderable-mixin>)
  ((lock  :reader multilock-lock
          :initform (mpcompat:make-lock))
   ))

(defclass sharing-multilock (<orderable-mixin>)
  ((lock  :reader multilock-lock
          :initform (mpcompat:make-lock
                     :sharing t))
   ))

(defun make-multilock ()
  (make-instance 'multilock))

(defun make-sharing-multilock ()
  (make-instance 'sharing-multilock))

;; --------------------------------------

(defun do-mlock (locks locking-fn unlocking-fn)
  ;; NOTE: do not mix multilocks with sharing-multilocks
  (let ((ord-locks  (sort locks #'<
                          :key #'order-id))
        (held-locks nil))
    (tagbody
     (dolist (lock ord-locks)
       (if (funcall locking-fn (multilock-lock lock) nil *timeout*)
           ;; can fail due to timeout
           (push lock held-locks)
         (go release)))
     (return-from do-mlock held-locks)
     
     release
     (do-munlock held-locks unlocking-fn)
     )))

(defun do-munlock (revord-locks unlocking-fn)
  (dolist (lock revord-locks)
    (funcall unlocking-fn (multilock-lock lock))))

(defun do-with-multilocks (fn locks locking-fn unlocking-fn)
  (um:when-let (revord-locks (do-mlock locks
                                       locking-fn
                                       unlocking-fn))
    (unwind-protect
        (values (funcall fn) t)
      (do-munlock revord-locks unlocking-fn))
    ))

;; --------------------------------------

(defmacro with-multilocks ((&rest locks) &body body)
  `(do-with-multilocks (lambda ()
                         ,@body)
                       (list ,@locks)
                       #'mpcompat:process-lock
                       #'mpcompat:process-unlock))
        
(defmacro with-sharing-multilocks ((&rest locks) &body body)
  `(do-with-multilocks (lambda ()
                         ,@body)
                       (list ,@locks)
                       #'mpcompat:process-sharing-lock
                       #'mpcompat:process-sharing-unlock))

(defmacro with-exclusive-multilocks ((&rest locks) &body body)
  `(do-with-multilocks (lambda ()
                         ,@body)
                       (list ,@locks)
                       #'mpcompat:process-exclusive-lock
                       #'mpcompat:process-exclusive-unlock))

;; ----------------------------------------------------------------------
