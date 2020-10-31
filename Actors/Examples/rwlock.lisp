;; rwlock.lisp - a proper R/W lock done in Actors
;;
;; Any number of concurrent read-locks when no writer.
;; Write lock only when no readers, or when all the readers are the same as the writer.
;; Any number of read locks during write lock, so long as the reader is the writer.
;; Any number of write locks so long as they are all the same writer.
;; Pending lock requests can be cancelled, e.g., as from a timeout
;; Locks are recursive and must be released as many times as taken.
;;
;; DM/RAL 10/20
;; ----------------------------------------------------------------------------------------------

(defpackage :rwlock
  (:use :cl :ac)
  (:export
   #:rwlock
   
   #:lock-for-read
   #:unlock-for-read
   #:lock-for-write
   #:unlock-for-write

   #:with-read-lock
   #:with-write-lock
   ))

;; --------------------------------
(in-package :rwlock)
;; --------------------------------

(defclass rwlock (actor)
  ((rdwait  :initform (maps:empty))
   (wrwait  :initform (maps:empty))
   (readers :initform nil)
   (writers :initform nil)
   ))

;; --------------------------------

(defun read-lock-obtainable-p (lock who-id)
  (with-slots (writers) lock
    ;; true when no writers, or when we are the writer
    (or (null writers)
        (eq who-id (car writers)))
    ))

(defun write-lock-obtainable-p (lock who-id)
  (with-slots (readers writers) lock
    ;; true when we are the writer, or when no writers and all
    ;; readers are me
    (or (eq who-id (car writers))
        (and (null writers)
             (every (um:rcurry #'eq who-id) readers))) ;; true for NIL readers too!
    ))

;; --------------------------------

(defmethod lock-for-read ((lock rwlock) who-id reply-to)
  (with-slots (readers rdwait) lock
    (perform-in-actor lock
      (cond
        ((read-lock-obtainable-p lock who-id)
         (push who-id readers)
         (send reply-to 'ok))
        
        (t
         (maps:addf rdwait (uuid:make-v1-uuid) (cons who-id reply-to)))
        ))
    ))

;; --------------------------------

(defun drop-pending-reader (lock who-id)
  (with-slots (rdwait) lock
    (maps:iter rdwait
               (lambda (k v)
                 (when (eq (car v) who-id)
                   (maps:removef rdwait k))))
    ))

(defun try-enabling-pending-writer (lock)
  (with-slots (wrwait writers) lock
    ;; the tree is kept in chronological order, and MAPS:ITER proceeds
    ;; from oldest to newest pending request. So we will launch the
    ;; oldest possible writer here.
    (maps:iter wrwait
               (lambda (k v)
                 (when (write-lock-obtainable-p lock (car v))
                   (maps:removef wrwait k)
                   (push (car v) writers)
                   (send (cdr v) 'ok))))
    ))

;; --------------------------------

(defmethod unlock-for-read ((lock rwlock) who-id)
  (with-slots (readers) lock
    (perform-in-actor lock
      (let ((rds readers))
        (if (eq rds (um:removef readers who-id :count 1))
            ;; was not among the readers - remove from pending readers
            (drop-pending-reader lock who-id)
          ;; else - try to enable some pending writer
          (try-enabling-pending-writer lock)
          )))))

;; --------------------------------

(defmethod lock-for-write ((lock rwlock) who-id reply-to)
  (with-slots (writers wrwait) lock
    (perform-in-actor lock
      (if (write-lock-obtainable-p lock who-id)
          (progn
            (push who-id writers)
            (send reply-to 'ok))
        ;; else
        (maps:addf wrwait (uuid:make-v1-uuid) (cons who-id reply-to)))
      )))

;; --------------------------------

(defun enable-oldest-pending-writer (lock)
  (with-slots (wrwait writers) lock
    (let ((cell (sets:min-elt wrwait)))
      (setf wrwait (sets:remove-min-elt wrwait))
      (destructuring-bind (who-id . reply-to)
          (maps:map-cell-val cell)
        (push who-id writers)
        (send reply-to 'ok))
      )))

(defun enable-all-pending-readers (lock)
  (with-slots (rdwait readers) lock
    (maps:iter (shiftf rdwait (maps:empty))
               (lambda (k v)
                 (declare (ignore k))
                 (destructuring-bind (who . reply-to) v
                   (push who readers)
                   (send reply-to 'ok))))
    ))

(defun drop-pending-writer (lock who-id)
  (with-slots (wrwait) lock
    (maps:iter wrwait
               (lambda (k v)
                 (when (eq (car v) who-id)
                   (maps:removef wrwait k))))
    ))

;; --------------------------------

(defmethod unlock-for-write ((lock rwlock) who-id)
  (with-slots (rdwait wrwait writers) lock
    (perform-in-actor lock
      (cond ((eq who-id (car writers))
             ;; we were holding the write lock
             (pop writers)
             (unless writers
               ;; no more writing - enable next in line
               (let ((waiting (sets:union rdwait wrwait)))
                 (unless (sets:is-empty waiting)
                   ;; enable the oldest waiting request
                   (let ((oldest (sets:min-elt waiting)))
                     (if (and (not (maps:is-empty rdwait))
                              (eq oldest (sets:min-elt rdwait)))
                         (enable-all-pending-readers lock)
                       ;; else - enable the oldest pending writer
                       (enable-oldest-pending-writer lock))
                     )))))
            
            (t
             (drop-pending-writer lock who-id))
            ))))

;; --------------------------------

(defmacro with-read-lock ((lock &key timeout (errorp t) on-timeout) &body body)
  (let ((g!lock (gensym))
        (g!id   (gensym))
        (g!ok   (gensym)))
    `(let ((,g!lock ,lock)
           (,g!id   (mp:get-current-process)))
       (=wait (,g!ok) (:timeout ,timeout :errorp ,errorp :on-timeout ,on-timeout)
           (lock-for-read ,g!lock ,g!id =wait-cont)
         (when (eq 'ok ,g!ok)
           (unwind-protect
               (progn
                 ,@body)
             (unlock-for-read ,g!lock ,g!id)))
         ))
    ))

;; --------------------------------

(defmacro with-write-lock ((lock &key timeout (errorp t) on-timeout) &body body)
  (let ((g!lock (gensym))
        (g!id   (gensym))
        (g!ok   (gensym)))
    `(let ((,g!lock ,lock)
           (,g!id   (mp:get-current-process)))
       (=wait (,g!ok) (:timeout ,timeout :errorp ,errorp :on-timeout ,on-timeout)
           (lock-for-write ,g!lock ,g!id =wait-cont)
         (when (eq 'ok ,g!ok)
           (unwind-protect
               (progn
                 ,@body)
             (unlock-for-write ,g!lock ,g!id)))
         ))
    ))
  
;; --------------------------------

        
    