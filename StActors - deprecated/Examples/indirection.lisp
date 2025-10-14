
(ql:quickload :core-crypto)

(defpackage #:indirection
  (:use #:cl #:ac)
  (:nicknames #:ind)
  (:import-from #:core-crypto
   #:int
   #:hash=
   #:hash/256)
  (:export
   #:ind
   #:ind-ref
   #:deref
   #:oref
   #:oref-id
   #:new-id
   #:when-created
   #:query-orefs
   #:perform-orefs
   #:find-oref
   #:find-obj
   #:get-changes
   #:mutate
   #:updref
   #:unref
   #:self-check
   #:find-all-changes
   ))

(in-package #:ind)

(defclass ind ()
  ((ref  :accessor ind-ref  :initarg :ref)))

(defun ind (x)
  (make-instance 'ind
                 :ref x))

(defmethod deref ((x ind))
  (deref (ind-ref x)))

(defmethod deref (x)
  x)

;; ------------------------------

(defclass oref ()
  ((id   :reader oref-id   :initarg :id)))

(defmethod oref ((o oref))
  o)

(defmethod oref ((id uuid:uuid))
  (make-instance 'oref
                 :id  id))

(defmethod print-object ((o oref) stream)
  (format stream "#.(oref #/uuid/~A)" (oref-id o)))
  
(defconstant +nil-oref+
  (oref #/uuid/{7d52d07e-7aab-11eb-908d-787b8acbe32e}))
(defconstant +t-oref+
  (oref #/uuid/{8ae6f9fe-7aab-11eb-908d-787b8acbe32e}))
(defconstant +0-oref+
  (oref #/uuid/{8207bda0-7ace-11eb-a43e-787b8acbe32e}))
(defconstant +1-oref+
  (oref #/uuid/{7167c4cc-7ac4-11eb-908d-787b8acbe32e}))
(defconstant +2-oref+
  (oref #/uuid/{83260aca-7ac4-11eb-908d-787b8acbe32e}))
(defconstant +-1-oref+
  (oref #/uuid/{91c2959e-7ac4-11eb-908d-787b8acbe32e}))

(defun new-id ()
  (uuid:make-v1-uuid))

(defmethod when-created ((r oref))
  (uuid:when-created (oref-id r)))

;; -------------------------------

(define-actor-class oref-manager ()
  ((reftbl :accessor oref-manager-reftbl
           :initform (make-hash-table
                      ;; ref UUID -> obj
                      :test          #'uuid:uuid=
                      :hash-function #'uuid:uuid-time
                      :single-thread t))
   (objtbl :accessor oref-manager-objtbl
           :initform (make-hash-table
                      ;; Obj -> (refs, sig)
                      :test          #'eql
                      :single-thread t))
   (changes :accessor oref-manager-changes
            ;; a list of tracked objects that have been mutated
            :initform      nil)
   ))

(defun %add-reference (oref obj reftbl objtbl)
  ;; actor internal routine
  (multiple-value-bind (orefs sig)
      (values-list (gethash obj objtbl))
    (setf (gethash (oref-id oref) reftbl)  obj
          (gethash obj objtbl)             (list (cons oref orefs)
                                                 (or sig
                                                     (hash/256 obj)))
          )))

(defmethod initialize-instance :after ((man oref-manager) &key &allow-other-keys)
  (with-slots (reftbl objtbl) man
    (dolist (pair (load-time-value
                   (list (list +nil-oref+ nil)
                         (list +t-oref+   t)
                         (list +0-oref+   0)
                         (list +1-oref+   1)
                         (list +2-oref+   2)
                         (list +-1-oref+  -1))
                   t))
      (multiple-value-call #'%add-reference (values-list pair) reftbl objtbl))
    ))
                       
(defvar *oref-manager* (make-instance 'oref-manager))

(defmacro %in-oref-manager (mfn slots &body body)
  `(,mfn *oref-manager*
         (with-slots ,slots *oref-manager*
           ,@body)))

(defmacro query-orefs (slots &body body)
  `(%in-oref-manager query-actor ,slots ,@body))

(defmacro perform-orefs (slots &body body)
  `(%in-oref-manager perform-in-actor ,slots ,@body))

(defun find-oref (obj)
  ;; obj -> orefs, sig
  (query-orefs (objtbl)
    (values-list (gethash obj objtbl))))

(defmethod find-obj ((o oref))
  ;; oref -> obj
  (query-orefs (reftbl)
    (gethash (oref-id o) reftbl)))

(defun get-changes ()
  (query-orefs (changes)
    ;; destructive readout
    (shiftf changes nil)))

;; ------------------------------------

(defmethod oref (obj)
  (query-orefs (reftbl objtbl)
    (let ((oref (caar (gethash obj objtbl))))
      (or oref
          (let ((onew  (oref (new-id))))
            (%add-reference onew obj reftbl objtbl)
            onew))
      )))

(defun %do-ref-obj-mutate (objs mut-fn)
  (unwind-protect
      (funcall mut-fn)
    (perform-orefs (objtbl changes)
      (dolist (obj objs)
        (let ((pair (gethash obj objtbl)))
          (when pair ;; obj being tracked?
            (let ((sig (hash/256 obj)))
              (unless (hash= sig (cadr pair)) ;; obj changed?
                (pushnew obj changes) ;; note changed obj
                ;; update tracking with new signature
                (setf (cadr pair) sig)))
            ))))
    ))

(defmacro mutate ((&rest objs) &body body)
  `(%do-ref-obj-mutate (list ,@objs) (lambda ()
                                       ,@body)))

(defun %unreference-object (oref obj objtbl)
  ;; actor internal routine
  (let* ((pair  (gethash obj objtbl))
         (orefs (remove oref (car pair))))
    (if orefs
        (setf (car pair) orefs)
      (remhash obj objtbl))))

(defmethod updref ((o oref) new-obj)
  (perform-orefs (objtbl reftbl)
    (let* ((id      (oref-id o))
           (old-obj (gethash id reftbl)))
      (unless (eql old-obj new-obj)
        ;; unreference/remove old obj
        (%unreference-object o old-obj objtbl)
        ;; add/update new obj refs
        (%add-reference o new-obj reftbl objtbl))
      )))

(defmethod unref ((o oref))
  (perform-orefs (objtbl reftbl)
    (let* ((id      (oref-id o))
           (old-obj (gethash id reftbl)))
      ;; remove the ref
      (remhash id reftbl)
      ;; unreference/remove the obj
      (%unreference-object o old-obj objtbl)
      )))

(defmethod deref ((o oref))
  (deref (find-obj o)))

;; --------------------------------------------------

(defun self-check ()
  (perform-orefs (objtbl reftbl)
    (maphash (lambda* (obj (orefs sig))
               (unless (hash= sig (hash/256 obj))
                 (error "Stale Signature"))
               (unless orefs
                 (error "Unreferenced Obj"))
               (dolist (oref orefs)
                 (unless (eq obj (gethash (oref-id oref) reftbl))
                   (error "Object reference mismatch"))))
             objtbl)
    (maphash (lambda (id obj)
               (let ((orefs (car (gethash obj objtbl))))
                 (unless (find id orefs
                               :key  #'oref-id
                               :test #'uuid:uuid=)
                   (error "Reference not recorded"))))
             reftbl)))

(defun find-all-changes ()
  (query-orefs (objtbl changes)
    (maphash (lambda (obj pair)
               (let ((chk (hash/256 obj)))
                 (unless (hash= chk (cadr pair))
                   (setf (cadr pair) chk)
                   (pushnew obj changes))))
             objtbl)
    (shiftf changes nil)))

;; ----------------------------------------

