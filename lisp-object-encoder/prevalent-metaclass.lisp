;; prevalent-metaclass.lisp
;; --------------------------------------------------------------------------------------
;; Prevalent object metaclass. For Persistent Object Prevalence --
;; -- all objects memory resident,
;; -- changes to slots are logged.
;; -- Initialization reads last committed data file
;;    and then runs the existing log entries to bring objects back up
;;    to most recent versions.
;;
;; Not transactioned. Single user only.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; DM/RAL 09/03  extensive modification/improvement
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :common-lisp)

;; --------------------------------------------------------------------------------------
(in-package :prevalent-object)
;; --------------------------------------------------------------------------------------

(defvar *default-prevalent-filename* "prevalent-store/prevalent-objects")

(defvar *prevalent-system*  nil)
(defvar *prevalent-lock*    (mp:make-lock :name "Prevalence"))

(defmacro with-locked-system (&body body)
  `(mp:with-lock (*prevalent-lock*)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-locked-system" 0)

;; ------------------------------------------------------------

(defclass system ()
  ((directory    :accessor system-directory
                 :initform nil)

   (logfile      :accessor system-logfile
                 :initform nil)

   (logpos       :accessor system-logpos
                 :initform 0)
   
   (root-objects :accessor system-root-objects
                 :initform (make-hash-table))

   (oid-table    :accessor system-oid-table
                 :initform (make-hash-table
                            :weak-kind :value))

   (obj-table    :accessor system-obj-table
                 :initform (make-hash-table
                            :weak-kind :key))
   ))
   
;; -------------------------------

;; API
(defun open-system (&optional (path *default-prevalent-filename*))
  "Open system restores the database to its last session, and enables
logging for future updates."
  ;; if the system has already been opened then this is idempotent
  (with-locked-system
    (unless *prevalent-system*
      (restore-system path))))

;; ---------------------------------------
(defvar *restoring*  nil)
;; ---------------------------------------

;; API
(defun add-root-object (key obj)
  (with-locked-system
    (open-system)
    ;; the system root objects table has an OID from inception.  so
    ;; addding / replacing an object with a given key can be done with
    ;; a normal update to a mutable object. The log file will record
    ;; the mutation.
    (mutate-object (system-root-objects *prevalent-system*) key obj)
    obj))

;; API
(defun remove-root-object (key)
  (with-locked-system
    (open-system)
    ;; the system root objects table has an OID from inception.  so
    ;; removing an object with a given key can be done with a normal
    ;; removal to a mutable object. The log file will record the
    ;; removal.
    (remove-object (system-root-objects *prevalent-system*) key)))

;; API
(defun get-root-object (key)
  (with-locked-system
    (open-system)
    (gethash key (system-root-objects *prevalent-system*))))


;; API
(defun root-keys ()
  (with-locked-system
    (open-system)
    (sort
     (loop for k being the hash-key of (system-root-objects
                                        *prevalent-system*)
           collect k)
     'ord:compare<)))

;; --------------------------------------------------------------------------------------

(defun current-prevalent-logfile ()
  (system-logfile *prevalent-system*))

;; --------------------------------------------------------------------------------------
;; metaclass of objects that might contain prevalent slots

(defclass prevalent-class (#+:LISPWORKS clos:standard-class
			    #+:ALLEGRO clos::standard-class
			    #+:CLOZURE clos:standard-class
			    #+:SBCL    standard-class)
  ((prevalent-slots   :accessor class-prevalent-slots   :initform nil)))

;; ----------------------------------------------------------------------------
;; allow standard-class objects as superclasses

(defmethod clos:validate-superclass ((class prevalent-class)
                                     (super standard-class))
  t)

;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around ((class prevalent-class)
                                        &rest args
                                        &key
                                        direct-superclasses
                                        direct-slots
                                        &allow-other-keys)
  ;; make sure the class inherits from prevalent-object
  (apply #'call-next-method
         class
         :direct-superclasses (maybe-add-prevalent-object-class
                               class
                               direct-superclasses)
         :direct-slots (mapcar 'convert-prevalent-slot direct-slots)
         ;; Tell Lispworks it shouldn't bypass slot-value-using-class
         #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
         args))
  
;; ----------------------------------------------------------------------------
  
(defmethod reinitialize-instance :around ((class prevalent-class)
                                            &rest args
                                            &key
                                            direct-superclasses
                                            direct-slots
                                            &allow-other-keys)
    ;; make sure the class inherits from prevalent-object
    (apply #'call-next-method
           class
           :direct-superclasses (maybe-add-prevalent-object-class
                                 class
                                 direct-superclasses)
           :direct-slots (mapcar 'convert-prevalent-slot direct-slots)
           ;; Tell Lispworks it shouldn't bypass slot-value-using-class
           #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
           args))
  
;; -----------------------------------------------------------
  
(defun maybe-add-prevalent-object-class (class direct-superclasses)
    ;; Add PREVALENT-OBJECT to the superclass list if necessary.
    (let ((root-class      (find-class 'prevalent-object nil))
          (prevalent-class (find-class 'prevalent-class)))
      (if (or (null root-class)
              (eql class root-class)
              (find-if (lambda (direct-superclass)
                         (member prevalent-class
                                 (clos:compute-class-precedence-list
                                  (class-of direct-superclass))))
                       direct-superclasses))
          direct-superclasses
        (cons root-class direct-superclasses))))
  
;; -----------------------------------------------------------

#|
(defun convert-prevalent-slot (slotd-list)
    ;; convert canonical slot definitions from :allocation :prevalent
    ;; into :prevalent t, and default to instance allocation.
    (labels ((rebuild-list (lst ans)
               (if (endp lst)
                   (nreverse ans)
                 (if (eq :allocation (first lst))
                     (rebuild-list (cddr lst) (list* t :prevalent ans))
                   (rebuild-list (cddr lst) (list* (second lst) (first lst) ans)))
                 )))
      (if (eq :prevalent (getf slotd-list :allocation))
          (rebuild-list slotd-list nil)
        slotd-list)))
|#

(defun convert-prevalent-slot (slotd-list)
    ;; convert canonical slot definitions from :allocation :prevalent
    ;; into :prevalent t, and default to instance allocation.
    (if (eq :prevalent (getf slotd-list :allocation))
        (let ((new-slotd-list (copy-list slotd-list)))
          (setf (getf new-slotd-list :allocation) :instance
                (getf new-slotd-list :prevalent)  t)
          new-slotd-list)
      slotd-list))

;; ---------------------------------------
;; mixin metaclass for prevalent slots and methods to make them
;; appear prevalent

(defclass prevalent-slot-definition (clos:standard-slot-definition)
  ((prevalence :accessor prevalent-slot-prevalence
               :initform nil
               :initarg  :prevalent)))

;; -----------------------------------------------------------

#+:LISPWORKS
(defmethod clos:process-a-slot-option ((class prevalent-class) option value
                                       already-processed-options slot)
  (case option
    (:prevalent  (list* :prevalent value already-processed-options))
    (t           (call-next-method))))

;; -----------------------------------------------------------
;; class of direct prevalent slots and methods to construct them
;; when appropriate

(defclass prevalent-direct-slot-definition (clos:standard-direct-slot-definition
                                            prevalent-slot-definition)
  ())

;; Called when the class is being made, to choose the metaclass of
;; a given direct slot. It should return the class of slot definition required.

(defmethod clos:direct-slot-definition-class
           ((class prevalent-class) &rest initargs)
  (if (getf initargs :prevalent)
      (find-class 'prevalent-direct-slot-definition)
    (call-next-method)))

;; ---------------------------------------

;; Class of effective prevalent slots and methods to construct
;; them when appropriate

(defclass prevalent-effective-slot-definition
          (clos:standard-effective-slot-definition
           prevalent-slot-definition)
  ())

;; Called when the class is being finalized, to choose the
;; metaclass of a given effective slot. It should return the
;; class of slot definition required.

(defmethod clos:effective-slot-definition-class
           ((class prevalent-class) &rest initargs)
  (declare (ignore initargs))
  ;; Use prevalent-effective-slot-definition if appropriate
  (find-class 'prevalent-effective-slot-definition))
  

(defun prevalent-slot-p (slotd)
  (and (typep slotd 'prevalent-slot-definition)
       (slot-value slotd 'prevalence)))

(defmethod clos:compute-effective-slot-definition ((class prevalent-class)
                                                   slot-name
                                                   direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((effective-slotdef (call-next-method)))
    
    ;; If any direct slot is prevalent, then the effective one is too.
    (setf (slot-value effective-slotdef 'prevalence)
          (some 'prevalent-slot-p direct-slot-definitions))
    effective-slotdef))

;; -----------------------------------------------------------

(defmethod clos:finalize-inheritance :after ((class prevalent-class))
  ;; Register all (effective) prevalent slots.
  (let* ((effective-slots   (clos:class-slots class))
         (pslots (remove-if (complement 'prevalent-slot-p)
                            effective-slots)))
    (setf (class-prevalent-slots class) pslots)))

;; -------------------------------------------------------------------

;; underlying access methods for invoking
;; prevalent-slot-definition-function.

#|
;; not currently needed since we appear to be a normal instance slot
;; might be needed if we involve slot access in transaction protocol

#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-value-using-class
           ((class prevalent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (prevalent-slot-prevalence slotd)
        (dereference-value (call-next-method))
      (call-next-method))
    ))
|#

;; ---------------------------------------

(defun should-log-slot? (slotd)
  (and (not *restoring*)
       (prevalent-slot-prevalence slotd)))

(defun log-update-slot (object slot-name value)
  (with-locked-system
    (open-system)
    (log-update object slot-name value)))

(defun log-makunbound-slot (object slot-name)
  (with-locked-system
    (open-system)
    (log-makunbound object slot-name)))

;; ---------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod (setf clos:slot-value-using-class)
           (value (class prevalent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (when (should-log-slot? slotd)
      (log-update-slot object slot-name value))
    (call-next-method)))

#+(OR :CLOZURE :SBCL)
(defmethod (setf clos:slot-value-using-class)
           (value (class prevalent-class) object slot-def)
  (when (should-log-slot? slot-def)
    (log-update-slot object (clos:slot-definition-name slot-def) value))
  (call-next-method))

;; ---------------------------------------

#|
;; not needed since we appear to be a normal instance slot
#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-boundp-using-class
           ((class prevalent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (typep slotd 'prevalent-slot-definition)
        ;; (not (eq $unbound (aref (prevalent-object-cached-values object) (effective-slot-index slotd))))
        (call-next-method)
      (call-next-method))
    ))
|#

;; ---------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-makunbound-using-class
           ((class prevalent-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (when (should-log-slot? slotd)
      (log-makunbound-slot object slot-name))
    (call-next-method)))

#+(OR :CLOZURE :SBCL)
(defmethod clos:slot-makunbound-using-class
           ((class prevalent-class) object slot-def)
    (when (should-log-slot? slot-def)
      (log-makunbound-slot object (clos:slot-definition-name slot-def)))
    (call-next-method))

;; ---------------------------------------
#|
;; is this method really necessary?
(defmethod clos:slot-exists-p-using-class
           ((class prevalent-class) object slot-name)
  (or (call-next-method)
      (and (find slot-name (clos:class-slots class)
                 :key 'clos:slot-definition-name)
           t)))
|#
;; -----------------------------------------------------------

(defmethod sdle-store:get-slot-details ((slotd prevalent-slot-definition))
  (nconc (call-next-method)
         (list :prevalent  (prevalent-slot-prevalence slotd))
         ))

;; -----------------------------------------------------------

(defmethod sdle-store:serializable-slots-using-class (obj (class prevalent-class))
  (declare (ignore obj))
  (class-prevalent-slots class))

;; -----------------------------------------------------------

(defun touch-slot (obj slot)
  ;; to generate an update log entry
  (setf (slot-value obj slot) (slot-value obj slot)))
