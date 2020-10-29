;; persistent-metaclass.lisp
;; --------------------------------------------------------------------------------------
;; Persistent object metaclass. For Persistent Object Prevalence -- all objects memory resident,
;; changes to slots are logged. Initialization reads last committed data file
;; and then runs the existing log entries to bring objects back up to most recent versions.
;;
;; Not transactioned. Single user only.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package :common-lisp)

(defpackage :persistent-object
  (:use #:common-lisp)
  (:nicknames #:po)
  (:export
   #:persistent-class
   #:persistent-object
   #:get-persistent-objects
   #:save-persistent-objects
   #:close-persistent-objects
   #:remove-object
   #:$missing
   ))

;; --------------------------------------------------------------------------------------
(in-package :persistent-object)
;; --------------------------------------------------------------------------------------

(defvar *persistent-classes* (make-hash-table))

;; -------------------------------------------------------------------

(defvar *current-persistent-logfile* nil)

;; --------------------------------------------------------------------------------------
;; metaclass of objects that might contain persistent slots

(defclass persistent-class (#+:LISPWORKS clos:standard-class
			    #+:ALLEGRO clos::standard-class
			    #+:CLOZURE clos:standard-class
			    #+:SBCL    standard-class)
  ((object-counter     :accessor class-object-counter     :initform 0)
   (persistent-objects :accessor class-persistent-objects :initform (make-hash-table))
   (persistent-slots   :accessor class-persistent-slots   :initform nil)
   ))

(defmethod initialize-instance :around ((class persistent-class)
                                        &rest args
                                        &key
                                        direct-superclasses
                                        direct-slots
                                        &allow-other-keys)
  ;; make sure the class inherits from persistent-object
  (apply #'call-next-method
         class
         :direct-superclasses (maybe-add-persistent-object-class
                               class
                               direct-superclasses)
         :direct-slots (mapcar 'convert-persistent-slot direct-slots)
         ;; Tell Lispworks it shouldn't bypass slot-value-using-class
         #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
         args))

(defmethod reinitialize-instance :around ((class persistent-class)
                                        &rest args
                                        &key
                                        direct-superclasses
                                        direct-slots
                                        &allow-other-keys)
  ;; make sure the class inherits from persistent-object
  (apply #'call-next-method
         class
         :direct-superclasses (maybe-add-persistent-object-class
                               class
                               direct-superclasses)
         :direct-slots (mapcar 'convert-persistent-slot direct-slots)
         ;; Tell Lispworks it shouldn't bypass slot-value-using-class
         #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
         args))

;; -----------------------------------------------------------

(defun maybe-add-persistent-object-class (class direct-superclasses)
  ;; Add PERSISTENT-OBJECT to the superclass list if necessary.
  (let ((root-class (find-class 'persistent-object nil))
        (persistent-class (find-class 'persistent-class)))
    (if (or (null root-class)
            (eql class root-class)
            (find-if (lambda (direct-superclass)
                       (member persistent-class
                               (clos:compute-class-precedence-list
                                (class-of direct-superclass))))
                     direct-superclasses))
        direct-superclasses
      (cons root-class direct-superclasses))))

;; -----------------------------------------------------------

(defun convert-persistent-slot (slotd-list)
  ;; convert canonical slot definitions from :allocation :persistent
  ;; into :persistence t, and default to instance allocation.
  (labels ((rebuild-list (lst ans)
             (if (endp lst)
                 (nreverse ans)
               (if (eq :allocation (first lst))
                   (rebuild-list (cddr lst) (list* t :persistence ans))
                 (rebuild-list (cddr lst) (list* (second lst) (first lst) ans)))
               )))
    (if (eq :persistent (getf slotd-list :allocation))
        (rebuild-list slotd-list nil)
      slotd-list)))

;; ---------------------------------------

  
;; mixin metaclass for persistent slots and methods to make them
;; appear persistent

(defclass persistent-slot-definition ()
  ((persistence :accessor persistent-slot-persistence
                :initform nil
                :initarg  :persistence)))

;; -----------------------------------------------------------

(defmethod clos:finalize-inheritance :after ((class persistent-class))
  ;; Register all (effective) persistent slots.
  (let* ((effective-slots (clos:class-slots class))
         (pslots (remove-if (complement 'persistent-slot-persistence)
                            effective-slots)))
    (if pslots
        (setf (class-persistent-slots class) pslots
              (gethash (#+:LISPWORKS clos:class-name
                        #+:ALLEGRO   clos::class-name
			#+:CLOZURE   clos:class-name
			#+:SBCL      class-name
                        class) *persistent-classes*) class))
    ))

;; -----------------------------------------------------------

(defmethod next-class-id ((class persistent-class))
  (prog1
      (class-object-counter class)
    (incf (class-object-counter class))))

;; ---------------------------------------

;; class of direct persistent slots and methods to construct them
;; when appropriate

(defclass persistent-direct-slot-definition (persistent-slot-definition
                                             clos:standard-direct-slot-definition)
  ())

;; Called when the class is being made, to choose the metaclass of
;; a given direct slot. It should return the class of slot definition required.

(defmethod clos:direct-slot-definition-class
           ((class persistent-class) &rest initargs)
  (if (getf initargs :persistence)
      (find-class 'persistent-direct-slot-definition)
    (call-next-method)))

;; ---------------------------------------

;; Class of effective persistent slots and methods to construct
;; them when appropriate

(defclass persistent-effective-slot-definition
          (persistent-slot-definition
           clos:standard-effective-slot-definition)
  ())

;; Called when the class is being finalized, to choose the
;; metaclass of a given effective slot. It should return the
;; class of slot definition required.

(defmethod clos:effective-slot-definition-class
           ((class persistent-class) &rest initargs)
  ;; Use persistent-effective-slot-definition if appropriate
  (declare (ignore initargs))
  (find-class 'persistent-effective-slot-definition))

(defmethod clos:compute-effective-slot-definition ((class persistent-class)
                                                   slot-name
                                                   direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((effective-slotdef (call-next-method))
        (persistent-slotdefs
         (remove-if (complement (um:rcurry #'typep 'persistent-slot-definition))
                    direct-slot-definitions)))

    ;; If any direct slot is persistent, then the effective one is too.
    (if persistent-slotdefs
        (setf (slot-value effective-slotdef 'persistence) t))
    effective-slotdef))

;; -------------------------------------------------------------------

;; underlying access methods for invoking
;; persistent-slot-definition-function.

#|
;; not currently needed since we appear to be a normal instance slot
;; might be needed if we involve slot access in transaction protocol

#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-value-using-class
           ((class persistent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (persistent-slot-persistence slotd)
        (dereference-value (call-next-method))
      (call-next-method))
    ))
|#

;; ---------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod (setf clos:slot-value-using-class)
           (value (class persistent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (when (and (persistent-slot-persistence slotd)
               *current-persistent-logfile*)
      (log-update object slot-name value))
    (call-next-method)))

#+(OR :CLOZURE :SBCL)
(defmethod (setf clos:slot-value-using-class)
           (value (class persistent-class) object slot-def)
  (when (and (persistent-slot-persistence slot-def)
	     *current-persistent-logfile*)
    (log-update object (clos:slot-definition-name slot-def) value))
  (call-next-method))

;; ---------------------------------------

#|
;; not needed since we appear to be a normal instance slot
#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-boundp-using-class
           ((class persistent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (typep slotd 'persistent-slot-definition)
        ;; (not (eq $unbound (aref (persistent-object-cached-values object) (effective-slot-index slotd))))
        (call-next-method)
      (call-next-method))
    ))
|#

;; ---------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-makunbound-using-class
           ((class persistent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (when (and (persistent-slot-persistence slotd)
               *current-persistent-logfile*)
      (log-makunbound-slot object slot-name))
    (call-next-method)))

#+(OR :CLOZURE :SBCL)
(defmethod clos:slot-makunbound-using-class
           ((class persistent-class) object slot-def)
    (when (and (persistent-slot-persistence slot-def)
               *current-persistent-logfile*)
      (log-makunbound-slot object (clos:slot-definition-name slot-def)))
    (call-next-method))

;; ---------------------------------------
#|
;; is this method really necessary?
(defmethod clos:slot-exists-p-using-class
           ((class persistent-class) object slot-name)
  (or (call-next-method)
      (and (find slot-name (clos:class-slots class)
                 :key 'clos:slot-definition-name)
           t)))
|#
;; ----------------------------------------------------------------------------
;; persistent-object -- the root class of all classes containing persistent slots

(defmethod clos:validate-superclass ((class persistent-class)
                                     (super standard-class))
  t)

(defclass persistent-object ()
  ((id :initarg    :object-id
       :reader     persistent-object-id))
  (:metaclass persistent-class)
  (:documentation "Classes of metaclass PERSISTENT-CLASS automatically
inherit from this class."))

;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((obj persistent-object) &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((class  (class-of obj)))
    (unless (slot-boundp obj 'id)
      (setf (slot-value obj 'id) (next-class-id class)))
    (let ((id (persistent-object-id obj)))
      (setf (class-object-counter class) (max (class-object-counter class)
                                              (1+ id))
            (gethash id (class-persistent-objects class)) obj)
      (log-create obj)
      )))

;; -------------------------------------------------------------------

(defun serialize-to-logfile (obj)
  (when *current-persistent-logfile*
    (file-position *current-persistent-logfile*
                   (file-length *current-persistent-logfile*))
    (loenc:serialize obj *current-persistent-logfile* :prefix-length t)))

;; -------------------------------------------------------------------

(defclass snapshot-collection ()
  ((classes :accessor snapshot-collection-classes  :initarg :classes)))

(defclass update-spec ()
  ((object    :accessor update-spec-object      :initarg :object)
   (slot-name :accessor update-spec-slot-name   :initarg :slot-name)
   (value     :accessor update-spec-value       :initarg :value)))

(defclass create-spec ()
  ((object :accessor create-spec-object :initarg :object)))

(defclass remove-spec ()
  ((object :accessor remove-spec-object :initarg :object)))

(defclass makunbound-spec ()
  ((object      :accessor makunbound-spec-object      :initarg :object)
   (slot-name   :accessor makunbound-spec-slot-name   :initarg :slot-name)))

;; ------------------------------------------------------------

(defmethod make-create-spec ((object persistent-object))
  (make-instance 'create-spec
                 :object object))

(defmethod make-update-spec ((object persistent-object) slot-name value)
  (make-instance 'update-spec
                 :object    object
                 :slot-name slot-name
                 :value     value))

(defmethod make-makunbound-spec ((object persistent-object) slot-name)
  (make-instance 'makunbound-spec
                 :object    object
                 :slot-name slot-name))

(defmethod make-remove-spec ((object persistent-object))
  (make-instance 'remove-spec
                 :object object))

(defmethod make-snapshot-collection (classes)
  (make-instance 'snapshot-collection
                 :classes classes))

;; ------------------------------------------------------------

(defvar +object-proxy-code+        (loenc:register-code 111 'object-proxy))
(defvar +missing-object-code+      (loenc:register-code 112 'missing-object))
(defvar +create-spec-code+         (loenc:register-code 113 'create-spec))
(defvar +update-spec-code+         (loenc:register-code 114 'update-spec))
(defvar +makunbound-spec-code+     (loenc:register-code 115 'makunbound-spec))
(defvar +remove-spec-code+         (loenc:register-code 116 'remove-spec))
(defvar +snapshot-collection-code+ (loenc:register-code 117 'snapshot-collection))

;; ------------------------------------------------------------
;; missing object serialization
;; [ +missing-object-code+ ]

(defvar $missing #(:missing))

(loenc:defstore (obj (eql $missing) stream)
  (loenc:store-count +missing-object-code+ stream))

(loenc:defrestore (missing-object stream)
  (declare (ignore stream))
  $missing)

;; ------------------------------------------------------------
;; object-proxy serialization -- occurs when serializing a persitent object
;; that points to another persistent object
;; [ +object-proxy-code+ class-name object-id ]

(loenc:defstore (obj persistent-object stream)
  (let ((id (persistent-object-id obj)))
    (if id
        (let* ((class      (class-of obj))
               (class-name (#+:LISPWORKS clos:class-name
                            #+:ALLEGRO   clos::class-name
			    #+:CLOZURE   clos:class-name
			    #+:SBCL      class-name
                            class)))
          (loenc:store-count  +object-proxy-code+ stream)
          (loenc:store-object class-name stream)
          (loenc:store-count  id stream))
      ;; else
      (loenc:store-object $missing stream))
    ))

(loenc:defrestore (object-proxy stream)
  (let* ((class-name (loenc:restore-object stream))
         (obj-id     (loenc:read-count stream))
         (class      (find-class class-name)))
    (gethash obj-id (class-persistent-objects class) $missing)))

;; ------------------------------------------------------------
;; persistent-class serialization occurs when saving all known objects
;; to a .snap snapshot file

(loenc:defstore (coll snapshot-collection stream)
  (let* ((classes (snapshot-collection-classes coll))
         (ncl     (length classes)))
    (loenc:store-count +snapshot-collection-code+ stream)
    (loenc:store-count ncl stream)
    (dolist (class classes)
      (store-class-structure-info class stream))
    (dolist (class classes)
      (store-slot-data-info class stream))
    ))

(defmethod store-class-structure-info ((class persistent-class) stream)
  (let* ((class-name (#+:LISPWORKS clos:class-name
                      #+:ALLEGRO   clos::class-name
		      #+:CLOZURE   clos:class-name
		      #+:SBCL      class-name
                      class))
         (next-id    (class-object-counter class)))
    (loenc:store-object class-name stream)
    (loenc:store-count  next-id    stream)
    (store-id-rles class stream)))

(defmethod store-id-rles ((class persistent-class) stream)
  (let ((tbl (class-persistent-objects class)))
    (if (zerop (hash-table-count tbl))
        (loenc:store-count 0 stream)
      ;; else
      (let ((grps (get-rle-groups tbl)))
          (loenc:store-count (length grps) stream)
          (dolist (grp grps)
            (destructuring-bind (start . end) grp
              (loenc:store-count start stream)
              (loenc:store-count end   stream) ))
          ))
      ))

(defun get-rle-groups (tbl)
  (let ((ids  nil))
    (maphash (lambda (key obj)
               (declare (ignore obj))
               (push key ids))
             tbl)
    (seq->rle-groups (sort ids #'<))
    ))

(defun seq->rle-groups (ids)
  (labels ((iter (ix ids start grps)
             (if (endp ids)
                 (acons start ix grps)
               (if (= (first ids) ix)
                   (iter (1+ ix) (rest ids) start grps)
                 (iter (first ids) ids (first ids) (acons start ix grps)))
               )))
    (iter (first ids) ids (first ids) nil)
    ))

(defmethod store-slot-data-info ((class persistent-class) stream)
  (let* ((tbl    (class-persistent-objects class))
         (nobjs  (hash-table-count tbl)))
    (loenc:store-count nobjs stream)
    (unless (zerop nobjs)
      (let ((pslots (mapcar #'clos:slot-definition-name
                            (class-persistent-slots class))))
        (loenc:store-object (#+:LISPWORKS clos:class-name
                             #+:ALLEGRO   clos::class-name
			     #+:CLOZURE   clos:class-name
			     #+:SBCL      class-name
                             class) stream)
        (loenc:store-count  (length pslots) stream)
        (dolist (pslot pslots)
          (loenc:store-object pslot stream))
        (maphash (lambda (key obj)
                   (loenc:store-count key stream)
                   (dolist (pslot pslots)
                     (loenc:store-object (slot-value obj pslot) stream)))
                 tbl)))
    ))
  
(loenc:defrestore (snapshot-collection stream)
  (let ((ncl (loenc:read-count stream)))
    (dotimes (ix ncl)
      (read-class-structure-info stream))
    (dotimes (ix ncl)
      (read-slot-data-info stream))
    ))

(defun read-class-structure-info (stream)
  (let* ((class-name (loenc:restore-object stream))
         (class      (find-class class-name))
         (tbl        (class-persistent-objects class))
         (next-id    (loenc:read-count stream))
         (ngrps      (loenc:read-count stream)))
    (setf (class-object-counter class) next-id)
    (dotimes (ix ngrps)
      (let ((start (loenc:read-count stream))
            (end   (loenc:read-count stream)))
        (do ((id start (1+ id)))
            ((>= id end))
          (unless (gethash id tbl)
            (make-instance class :object-id id)))
        ))
    ))

(defun read-slot-data-info (stream)
  (let ((nobjs (loenc:read-count stream)))
    (unless (zerop nobjs)
      (let* ((class-name (loenc:restore-object stream))
             (class      (find-class class-name))
             (tbl        (class-persistent-objects class))
             (npslots    (loenc:read-count stream))
             (pslots     (loop for ix from 1 to npslots
                               collect (loenc:restore-object stream))))
        (dotimes (ix nobjs)
          (let* ((id  (loenc:read-count stream))
                 (obj (gethash id tbl)))
            (dolist (pslot pslots)
              (let ((val (loenc:restore-object stream)))
                (setf (slot-value obj pslot) val)
                ))))
        ))))

;; ----------------------------------------------------------------
;; persistent-object serialization -- occurs when a new object is created
;; [ +create-spec-code+ class-name object-id #persistent-slots {persistent-slot-name slot-value} ]

(loenc:defstore (crt create-spec stream)
  (let* ((obj        (create-spec-object crt))
         (class      (class-of obj))
         (class-name (#+:LISPWORKS clos:class-name
                      #+:ALLEGRO   clos::class-name
		      #+:CLOZURE   clos:class-name
		      #+:SBCL      class-name
                      class))
         (pslots     (mapcar #'clos:slot-definition-name
                             (class-persistent-slots class))))
    (loenc:store-count  +create-spec-code+ stream)
    (loenc:store-object class-name stream)
    (loenc:store-count  (persistent-object-id obj) stream)
    (loenc:store-count  (length pslots) stream)
    (dolist (pslot pslots)
      (loenc:store-object pslot stream)
      (loenc:store-object (slot-value obj pslot) stream))
    ))
  
(loenc:defrestore (create-spec stream)
  (let* ((class-name (loenc:restore-object stream))
         (class      (find-class class-name))
         (id         (loenc:read-count stream))
         (nslots     (loenc:read-count stream))
         (obj        (make-instance class :object-id id)))
    (dotimes (ix nslots)
      (let ((pslot (loenc:restore-object stream))
            (val   (loenc:restore-object stream)))
        (setf (slot-value obj pslot) val)))
    ))

(defmethod log-create ((object persistent-object))
  (serialize-to-logfile (make-create-spec object)))
    
;; ------------------------------------------------------------
;; update-spec serialization -- sent to the logfile for every mutation
;; of a perstistent slot
;; [ +update-spec-code+ class-name object-id slot-name slot-value ]

(loenc:defstore (obj update-spec stream)
  (let* ((per-obj    (update-spec-object obj))
         (class      (class-of per-obj))
         (class-name (#+:LISPWORKS clos:class-name
                      #+:ALLEGRO   clos::class-name
		      #+:CLOZURE   clos:class-name
		      #+:SBCL      class-name
                      class))
         (obj-id     (persistent-object-id per-obj))
         (slot-name  (update-spec-slot-name obj))
         (value      (update-spec-value obj)))
    (loenc:store-count  +update-spec-code+ stream)
    (loenc:store-object class-name stream)
    (loenc:store-count  obj-id stream)
    (loenc:store-object slot-name stream)
    (loenc:store-object value stream)
    ))

(loenc:defrestore (update-spec stream)
  (let* ((class-name (loenc:restore-object stream))
         (class      (find-class class-name))
         (obj-id     (loenc:read-count stream))
         (slot-name  (loenc:restore-object stream))
         (value      (loenc:restore-object stream))
         (obj        (gethash obj-id (class-persistent-objects class) $missing)))
    (unless (eq obj $missing)
      (let ((*current-persistent-logfile* nil))
        (if (eq $missing value)
            (slot-makunbound obj slot-name)
          (setf (slot-value obj slot-name) value)))
      )))

(defmethod log-update ((object persistent-object) slot-name value)
  (serialize-to-logfile (make-update-spec object slot-name value)))

;; ------------------------------------------------------------
;; unbound-spec serialization -- sent to the logfile on slot-makunbound
;; [ +makunbound-code+ class-name object-id slot-name ]

(loenc:defstore (obj makunbound-spec stream)
  (let* ((pobj      (makunbound-spec-object obj))
         (id        (persistent-object-id pobj))
         (slot-name (makunbound-spec-slot-name obj))
         (class     (class-of pobj)))
    (loenc:store-count  +makunbound-spec-code+ stream)
    (loenc:store-object (#+:LISPWORKS clos:class-name
                         #+:ALLEGRO   clos::class-name
			 #+:CLOZURE   clos:class-name
			 #+:SBCL      class-name
                         class) stream)
    (loenc:store-count  id stream)
    (loenc:store-object slot-name stream)
    ))

(loenc:defrestore (makunbound-spec stream)
  (let* ((class-name (loenc:restore-object stream))
         (class      (find-class class-name))
         (id         (loenc:read-count stream))
         (slot-name  (loenc:restore-object stream))
         (obj        (gethash id (class-persistent-objects class) $missing)))
    (unless (eq $missing obj)
      (slot-makunbound obj slot-name))
    ))
    
(defmethod log-makunbound-slot ((object persistent-object) slot-name)
  (serialize-to-logfile (make-makunbound-spec object slot-name)))

;; ------------------------------------------------------------
;; remove-spec serialization -- sent to the logfile on remove-object
;; [ +remove-spec-code+ class-name object-id ]

(loenc:defstore (obj remove-spec stream)
  (let* ((class-name (#+:LISPWORKS clos:class-name
                      #+:ALLEGRO   clos::class-name
		      #+:CLOZURE   clos:class-name
		      #+:SBCL      class-name
                      (class-of obj)))
         (id         (persistent-object-id obj)))
    (loenc:store-count +remove-spec-code+ stream)
    (loenc:store-object class-name stream)
    (loenc:store-count  id stream)))

(loenc:defrestore (remove-spec stream)
  (let* ((class-name (loenc:restore-object stream))
         (class      (find-class class-name))
         (id         (loenc:read-count stream))
         (obj        (gethash id (class-persistent-objects class) $missing)))
    (unless (eq obj $missing)
      (remhash id (class-persistent-objects class))
      (setf (slot-value obj 'id) nil))
    ))

(defmethod remove-object ((object persistent-object))
  (serialize-to-logfile (make-remove-spec object))
  (remhash (persistent-object-id object) (class-persistent-objects (class-of object)))
  (setf (slot-value object 'id) nil))

;; ------------------------------------------------------------

(defun convert-to-uint32 (str)
  (do ((val 0)
       (ix  0 (1+ ix)))
      ((>= ix 4) val)
    (setf val (+ (ash val 8) (char-code (char str ix))))))

;; the magic word for .snap snapshot files
(defvar *persistent-objects-magic* (convert-to-uint32 "SDPO"))

;; ------------------------------------------------------------
;; close the currently open logfile and mark nil so that
;; future logging requests are ignored.

(defun close-persistent-objects (&rest ignored)
  (declare (ignore ignored))
  (when *current-persistent-logfile*
    (close *current-persistent-logfile*)
    (setf *current-persistent-logfile* nil)))

;; ------------------------------------------------------------
;; save-persistent-objects -- save the entire known universe of
;; persistent objects to a new .snap snapshot file, rename the existing
;; .snap snapshot file and its associated logfile, and then create a new
;; logfile for recording future mutations.

(defun save-persistent-objects (filename)
  (close-persistent-objects)
  (let ((dir (pathname-directory
	      (merge-pathnames filename
			       #+:LISPWORKS (hcl:get-working-directory)
			       #+:ALLEGRO   (excl:current-directory)
			       #+:CLOZURE   (ccl:current-directory)
			       #+:SBCL      *default-pathname-defaults*
			       )))
        (fname (pathname-name filename)))
    (ensure-directories-exist (make-pathname
                               :directory dir))
    (with-open-file (stream (make-pathname
                             :directory dir
                             :name      fname
                             :type      "snap")
                            :direction :output
                            :if-exists :rename
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (let ((classes nil))
        (maphash (lambda (key class)
                   (declare (ignore key))
                   (push class classes))
                 *persistent-classes*)
        (loenc:serialize (make-snapshot-collection classes) stream
                         :use-magic *persistent-objects-magic*)))
    
    (setf *current-persistent-logfile* (open (make-pathname
                                              :directory dir
                                              :name      fname
                                              :type      "log")
                                             :direction :output
                                             :if-exists :rename
                                             :if-does-not-exist :create
                                             :element-type '(unsigned-byte 8)))
    ))

;; ------------------------------------------------------------
;; get-persistent-objects -- open a new persistent repository
;; deserialize all contained objects by reading in the base .snap snapshot
;; and then applying all logfile entries against that restored snapshot.
;; At the end of this process, the logfile remains open for additional entries.

(defun get-persistent-objects (filename)
  (close-persistent-objects)
  (let ((dir (pathname-directory
	      (merge-pathnames filename
			       #+:LISPWORKS (hcl:get-working-directory)
			       #+:ALLEGRO   (excl:current-directory)
			       #+:CLOZURE   (ccl:current-directory)
			       #+:SBCL      *default-pathname-defaults*
			       )))
        (fname (pathname-name filename)))
    (ensure-directories-exist (make-pathname
                               :directory dir))
    (with-open-file (stream (make-pathname
                             :directory dir
                             :name      fname
                             :type      "snap")
                            :direction :input
                            :if-does-not-exist nil
                            :element-type '(unsigned-byte 8))
      (if stream
          (loenc:deserialize stream
                             :use-magic *persistent-objects-magic*)))

    (with-open-file (stream (make-pathname
                             :directory dir
                             :name      fname
                             :type      "log")
                            :direction :input
                            :if-does-not-exist nil
                            :element-type '(unsigned-byte 8))
      (if stream
          (ignore-errors
            (do ((ans (loenc:deserialize stream
                                         :prefix-length t)
                      (loenc:deserialize stream
                                         :prefix-length t)))
                ((eq ans loenc:$eof)))
            )))
    
    (setf *current-persistent-logfile* (open (make-pathname
                                              :directory dir
                                              :name      fname
                                              :type      "log")
                                             :direction :output
                                             :if-exists :append
                                             :if-does-not-exist :create
                                             :element-type '(unsigned-byte 8)))
    ))

;; ------------------------------------------------------------

#+:LISPWORKS
(lw:define-action "When quitting image"
                  "Close persistent store"
                  'close-persistent-objects)

;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; -------------------------------------------------------------------

#|
;; tests...

(defclass per-class-1 ()
  ((a :accessor a
      :initarg  :a
      :initform 15
      :allocation :persistent)
   (b :accessor b
      :initform 32
      :initarg  :b)
   (c :accessor c
      :initform "HiC"
      :initarg :c
      :allocation :persistent))
  (:metaclass persistent-class))

(setf fname "persistent-store/persistent-objects")
(get-persistent-objects fname)

(setf x (make-instance 'per-class-1
                       :a 19))

(setf x2 (make-instance 'per-class-1
                        :b 23))

(setf (a x) x2)
(save-persistent-objects fname)

|#

