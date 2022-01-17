
(in-package :actors/rwgate)
#|
(define-actor-class rwgate ()
  ;; one of these for each separate shared resource
  ((readers         :initform nil)
   (pending-readers :initform nil)
   (writer          :initform nil)
   (pending-writers :initform nil)))

(defclass resource-handle ()
  ((mbox  :reader resource-handle-mbox  :initform (mp:make-mailbox))
   (rsrc  :reader resource-handle-rsrc  :initarg  :rsrc)))

(defun do-with-shared-action (entry-fn exit-fn handle timeout errorp on-timeout-fn fn)
  (let ((mbox (resource-handle-mbox handle))
        (rsrc (resource-handle-rsrc handle)))
    (funcall entry-fn rsrc mbox)
    (unwind-protect
        (multiple-value-bind (ans ok)
            (mp:mailbox-read mbox nil timeout)
          (declare (ignore ans))
          (if ok
              (funcall fn)
            (if on-timeout-fn
                (funcall on-timeout-fn)
              (when errorp
                (error 'timeout)))
            ))
      (funcall exit-fn rsrc mbox))))

(defmacro with-shared-action (enter-action exit-action handle timeout errorp on-timeout body)
  `(do-with-shared-action #',enter-action #',exit-action
                          ,handle ,timeout ,errorp
                          ,(when on-timeout
                             `(lambda ()
                                ,on-timeout))
                          (lambda ()
                            ,@body)))

;; ------------------------------------------

(defun make-resource-handle (rsrc)
  ;; A resource handle represents a logical user. It can be handed off
  ;; to any other Actor or process. Arbitrary nested locking against
  ;; one handle is permissible.
  (make-instance 'resource-handle
                 :rsrc rsrc))

(defmacro with-shared-reading ((handle &key timeout errorp on-timeout) &body body)
  `(with-shared-action enter-read exit-read
                       ,handle ,timeout ,errorp ,on-timeout ,body))

(defmacro with-exclusive-writing ((handle &key timeout errorp on-timeout) &body body)
  `(with-shared-action enter-write exit-write
                       ,handle ,timeout ,errorp ,on-timeout ,body))

;; ------------------------------------------

(defmethod enter-read ((gate rwgate) mbox)
  ;; Grant permission so long as:
  ;;   (A) no writers, or
  ;;   (B) the writer is the same as the requestor
  (perform-in-actor gate
    (with-slots (readers pending-readers writer) gate
      (cond
       ((or (null writer)
            (eql mbox (car writer)))
        (push mbox readers)
        (mp:mailbox-send mbox mbox))

       (t
        (setf pending-readers (nconc pending-readers
                                     (list (list mbox
                                                 (get-universal-time))))
              ))
       ))))

(defmethod exit-read ((gate rwgate) id)
  (perform-in-actor gate
    (with-slots (readers pending-readers) gate
      (cond
       ((find id pending-readers :key #'car)
        (setf pending-readers (delete id pending-readers
                                      :key #'car
                                      :count 1)))
       
       ((find id readers)
        (setf readers (delete id readers :count 1))
        (enable-rw gate))
       ))))

(defmethod enter-write ((gate rwgate) mbox)
  ;; Grant permission so long as:
  ;;  (A1) There are no readers, or
  ;;  (A2) Every reader is the same as the requestor,
  ;;  -- AND --
  ;;  (B1) There are no writers, or
  ;;  (B2) We are already the writer
  (perform-in-actor gate
    (with-slots (readers writer pending-writers) gate
      (cond
       ((and (or (null readers)
                 (every (um:rcurry #'eql mbox) readers))
             (or (null writer)
                 (eql mbox (car writer))))
        (push mbox writer)
        (mp:mailbox-send mbox mbox))

       (t
        (setf pending-writers (nconc pending-writers
                                     (list (list mbox
                                                 (get-universal-time))))
              ))
       ))))

(defmethod exit-write ((gate rwgate) id)
  (perform-in-actor gate
    (with-slots (writer pending-writers) gate
      (cond
       ((find id pending-writers :key #'car)
        (setf pending-writers (delete id pending-writers
                                      :key #'car
                                      :count 1)))
       
       ((eql id (car writer))
        (pop writer)
        (enable-rw gate))
       ))))

(defun enable-rw (gate)
  (with-slots (readers pending-readers writer pending-writers) gate
    (unless writer
      ;; If no remaining writer. If pending writers, then grant
      ;; writership if:
      ;;   (A) there are no readers, or
      ;;   (B) every reader is the same, and there is a pending
      ;;   writer also the same
      (flet
          ((enable-writer (wr)
             (push wr writer)
             (mp:mailbox-send wr wr)
             (setf pending-writers (mapcan (lambda* (&whole pair (wrx tstamp))
                                             (declare (ignore tstamp))
                                             (if (eql wr wrx)
                                                 (progn
                                                   (push wr writers)
                                                   (mp:mailbox-send wr wr)
                                                   nil)
                                               ;; else
                                               (list pair)))
                                           pending-writers))
             (setf pending-readers (mapcan (lambda* (&whole pair (rdx tstamp))
                                             (declare (ignore tstamp))
                                             (if (eql wr rdx)
                                                 (progn
                                                   (push wr readers)
                                                   (mp:mailbox-send wr wr)
                                                   nil)
                                               ;; else
                                               (list pair)))
                                           pending-readers)))
           (enable-all-readers ()
             (dolist (reader pending-readers)
               (mp:mailbox-send reader reader))
             (setf readers         (nconc readers
                                          (mapcar #'car pending-readers))
                   pending-readers nil)))
        
        (cond
         ((and pending-writers
               (or (null pending-readers)
                   (< (cadar pending-writers)
                      (cadar pending-readers)))
               (null readers))
          (enable-writer (car (pop pending-writers))))

         ((and pending-writers
               (null pending-readers)
               readers
               (every (um:rcurry #'eql (car readers)) (cdr readers))
               (find (car readers) pending-writers :key #'car))
          (let ((wr (car readers)))
            (setf pending-writers (delete wr pending-writers
                                          :key #'car
                                          :count 1))
            (enable-writer wr)))

         (t
          (enable-all-readers))
         )))))

#|
(defvar *rwgate* (make-instance 'rwgate))

;; WE CAN DO...
(let ((handle (make-resource-handle *rwgate*)))
  (with-shared-reading (handle)
    (with-exclusive-writing (handle)
      (pr :hello)
      (values))))

;; Compare with...
(defvar *rwlock* (mp:make-lock :sharing t))

;; NO CAN DO...
(mp:with-sharing-lock (*rwlock*)
  (mp:with-exclusive-lock (*rwlock*)
    (pr :hello)
    (values)))
 |#

(defvar *tbl*
  (let ((tbl (make-hash-table)))
    (setf (gethash 'changed tbl) 0)
    tbl))

(defun mut ()
  (spawn-worker
   (lambda ()
     (hcl:with-hash-table-locked *tbl*
       (sys:with-modification-change
        (gethash 'changed *tbl*)
        (setf (gethash 'a *tbl*) (get-universal-time)
              (gethash 'b *tbl*) (not (gethash 'b *tbl*)))
        )))))
(mut)

(let ((a (gethash 'a *tbl*))
      (b (gethash 'b *tbl*)))
  (values a b))

(hcl:with-hash-table-locked *tbl*
  (let ((a (gethash 'a *tbl*))
        (b (gethash 'b *tbl*)))
    (values a b)))

(defun get-ab ()
  (loop
   (sys:with-modification-check-macro cache-unchanged? (gethash 'changed *tbl*)
     (let ((a (gethash 'a *tbl*))
           (b (gethash 'b *tbl*)))
       (when (cache-unchanged?)
         (return (values a b)))))))

(get-ab)
|#

;; ---------------------------------------------------------------
;; Banker's Queue

(um:make-encapsulated-type E E? D)

(defun make-unshared-queue ()
  (E (cons nil nil)))

(defmethod is-empty? ((q E))
  (null (car (D q))))

(defmethod addq ((q E) item)
  (let ((cell (D q)))
    (if (car cell)
        (push item (cdr cell))
      (push item (car cell)))
    ))

(defmethod popq ((q E))
  (let ((cell (D q)))
    (when (car cell)
      (let ((ans (pop (car cell))))
        (unless (car cell)
          (setf (car cell) (nreverse (cdr cell))
                (cdr cell) nil))
        (values ans t)))))

;; ---------------------------------------------------------------

;; export
(defun make-rw-lock ()
  (make-actor (make-empty-lock-beh)))

;; export
(defmacro with-read-lock ((lock &optional identity) &body body)
  `(send ,lock :rd-lock (or ,identity
                            self
                            mp:*current-process*)
         (lambda ()
           ,@body)))

;; export
(defmacro with-write-lock ((lock &optional identity) &body body)
  `(send ,lock :wr-lock (or ,identity
                            self
                            mp:*current-process*)
         (lambda ()
           ,@body)))

;; export
(defun make-locking-identity ()
  ;; make a new unique identity to share among related locking actions
  (gensym (string :locking-identity-)))

(defun %launch-reader (from cont)
  (with-worker ((lock self))
    (unwind-protect
        (funcall cont)
      (send lock :unlock-rd from))))

(defun %launch-writer (cont)
  (with-worker ((lock self))
    (unwind-protect
        (funcall cont)
      (send lock :unlock-wr))))

(defun make-empty-lock-beh ()
  (um:dlambda
    (:rd-lock (from cont)
     (become (make-readers-lock-beh (list from)))
     (%launch-reader from cont))
    
    (:wr-lock (from cont)
     (become (make-writer-lock-beh from 1 0))
     (%launch-writer cont))

    (:show ()
     (ac:pr (list :empty-lock)))
    ))

(defun make-readers-lock-beh (readers &optional (pending-writers (make-unshared-queue)))
  (labels ((become-writer-lock (writer ct)
             (let ((newq  (make-unshared-queue)))
               (um:nlet iter ()
                 (let ((pair (popq pending-writers)))
                   (if pair
                       (destructuring-bind (from . cont) pair
                         (if (eq from writer)
                             (progn
                               (incf ct)
                               (%launch-writer cont))
                           ;; else
                           (addq newq pair))
                         (go-iter))
                     ;; else
                     (if (plusp ct)
                         (become (make-writer-lock-beh writer ct (length readers) newq))
                       (setf pending-writers newq)))
                   )))))
    (um:dlambda
      (:rd-lock (from cont)
       (push from readers)
       (%launch-reader from cont))
      
      (:unlock-rd (from)
       (setf readers (delete from readers :count 1))
       (if readers
           (when (every (um:curry #'eq (car readers)) readers)
             (become-writer-lock (car readers) 0))
         ;; else - no more readers
         (if (is-empty? pending-writers)
             (become (make-empty-lock-beh))
           ;; else
           (destructuring-bind (writer . cont) (popq pending-writers)
             (%launch-writer cont)
             (become-writer-lock writer 1))
           )))
      
      (:wr-lock (writer cont)
       (if (every (um:curry #'eq writer) readers)
           (progn
             (%launch-writer cont)
             (become-writer-lock writer 1))
         ;; else
         (addq pending-writers (cons writer cont))))

      (:show ()
       (ac:pr (list :rd-lock readers pending-writers)))
      )))

(defun make-writer-lock-beh (writer wr-count rd-count &optional
                                    (pending-writers (make-unshared-queue))
                                    pending-readers)
  (flet ((launch-writer (cont)
           (incf wr-count)
           (%launch-writer cont))

         (launch-reader (from cont)
           (incf rd-count)
           (%launch-reader from cont)))

    (um:dlambda
      (:rd-lock (from cont)
       (if (eq from writer)
           (launch-reader from cont)
         (push (cons from cont) pending-readers)))

      (:wr-lock (from cont)
       (if (eq from writer)
           (launch-writer cont)
         (addq pending-writers (cons from cont))))

      (:unlock-rd _
       (decf rd-count))
      
      (:unlock-wr ()
       (decf wr-count)
       (when (zerop wr-count)
         (if (is-empty? pending-writers)
             (let ((readers (make-list rd-count :initial-element writer)))
               (dolist (pair pending-readers)
                 (destructuring-bind (from . cont) pair
                   (push from readers)
                   (launch-reader from cont)))
               (if readers
                   (become (make-readers-lock-beh readers pending-writers))
                 (become (make-empty-lock-beh))))
           ;; else - next writer
           (destructuring-bind (from . cont) (popq pending-writers)
             (setf writer from)
             (launch-writer cont)
             (let ((newq (make-unshared-queue)))
               (um:nlet iter ()
                 (let ((pair (popq pending-writers)))
                   (when pair
                     (destructuring-bind (from . cont) pair
                       (if (eq from writer)
                           (launch-writer cont)
                         (addq newq pair)))
                     (go-iter))))
               (setf pending-writers newq)
               (let ((new-readers nil))
                 (dolist (pair pending-readers)
                   (destructuring-bind (from . cont) pair
                     (if (eq from writer)
                         (launch-reader from cont)
                       (push pair new-readers))))
                 (setf pending-readers new-readers))
               )))
         ))
      (:show ()
       (ac:pr (list :wr-lock writer wr-count rd-count pending-writers pending-readers)))
      )))

#|
(let ((x (make-rw-lock)))
  (with-read-lock (x :id)
    (with-write-lock (x :id)
      (send x :show))
    (sleep 1)
    (send x :show)))
 |#