;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; Defines a special backend type which specializes various methods
;; in plumbing.lisp to make it nice and easy to 
;; resolve possible circularities in objects.
;; Most of the work is done using the resolving-object
;; macro which knows how to handle an object which 
;; is a referrer to a previously restored value.
;; Backends wanting to make use of this should take
;; a look at default-backend.lisp and xml-backend.lisp
;; paying special attention to the defbackend form and the 
;; defrestore definitions for cons, array, simple-vector
;; array and hash-table.
;;
;; As a note this will ignore integers, symbols or characters
;; as referrer values. It will handle all other EQ number although
;; software depending on eq numbers are not conforming 
;; programs according to the Hyperspec(notes in EQ).

(in-package :sdle-store)

(defvar *check-for-circs* t)

;; Restoration.
(declaim (type (or fixnum null) *restore-counter*))
(defvar *restore-counter*)
(defvar *need-to-fix*)
(defvar *restored-values*)
(defvar *restore-hash-size* 50)

(declaim (type (or fixnum null) *stored-counter*))
(defvar *stored-counter*)
(defvar *stored-values*)

(defvar *store-hash-size* 50)

(defvar *grouped-store-hash*)
(defvar *grouped-restore-hash*)

(defstruct delay 
  value (completed nil))

(defmacro delay (&rest body)
  `(make-delay :value #'(lambda () ,@body)))

(defun force (delay)
  (unless (delay-completed delay)
    (setf (delay-value delay) (funcall (the function (delay-value delay)))
          (delay-completed delay) t))
  (delay-value delay))

;; -----------------------------------------------------------------------
;; The definitions for setting and setting-hash sits in resolving-object.
(defmacro setting (place get)
  "Resolve the possible referring object retrieved by GET and 
  set it into PLACE. Only usable within a resolving-object form."
  (declare (ignore place get))
  #+ecl nil
  #-ecl (error "setting can only be used inside a resolving-object form."))

(defmacro setting-hash (getting-key getting-value)
  "Insert the value retrieved by GETTING-VALUE with the key 
  retrieved by GETTING-KEY, resolving possible circularities.
  Only usable within a resolving-object form."
  (declare (ignore getting-key getting-value))
  #+ecl nil
  #-ecl (error "setting-hash can only be used inside a resolving-object form."))

;; -----------------------------------------------------------------------
#||#
(defun %must-be-in-circs ()
  (unless *check-for-circs*
    (restore-error "Found a circular values with *check-for-circs* = nil")))

(defun %setting (setter value)
  (if (referrer-p value)
      (progn
        (%must-be-in-circs)
        (push (delay
               (funcall setter
                        (referred-value value
                                        *restored-values*)))
              *need-to-fix*))
    ;; else - not referrer-p
    (funcall setter value)))

(defun %setting-hash (key value hashtable)
  ;; When restoring hash tables, it is possible that the key might
  ;; have a circularity that needs to be resolved before we can
  ;; restore the hash table cell with value.  And value might also
  ;; have a circularity.
  (if (referrer-p key)
      (progn
        (%must-be-in-circs)
        (push (delay
               (setf (gethash (referred-value key *restored-values*)
                              hashtable)
                     (if (referrer-p value)
                         (referred-value value *restored-values*)
                       value)))
              *need-to-fix*))
    ;; else - not referrer-p
    (%setting (um:make-setter (gethash key hashtable)) value)))

(defmacro resolving-object ((var create) &body body)
  "Execute body attempting to resolve circularities found in 
   form CREATE."
  `(macrolet ((setting (place getting)
                `(%setting (um:make-setter ,place) ,getting))
              (setting-hash (getting-key getting-place)
                `(%setting-hash ,getting-key ,getting-place ,',var)))
     (let ((,var ,create))
       ,@body
       ,var)))

#| ;; test macro expansions
(resolving-object (var create) body)
(resolving-objectx (tbl create-table) body)
|#
#||#
;; -----------------------------------------------------------------------
#|
(defmacro resolving-object ((var create) &body body)
  "Execute body attempting to resolve circularities found in 
   form CREATE."
  (with-gensyms (value key)
    `(macrolet ((setting (place getting)
                  `(let ((,',value ,getting))
                     (if (referrer-p ,',value)
                         (if *check-for-circs*
                             (push (delay (setf ,place
                                                (referred-value ,',value
                                                                *restored-values*)))
                                   *need-to-fix*)
                             (restore-error "Found a circular values with *check-for-circs* = nil"))
                         (setf ,place ,',value))))
                (setting-hash (getting-key getting-place)
                  `(let ((,',key ,getting-key))
                     (if (referrer-p ,',key)
                         (let ((,',value ,getting-place))
                           (unless *check-for-circs*
                             (restore-error "Found a circular values with *check-for-circs* = nil"))
                           (push (delay (setf (gethash (referred-value ,',key *restored-values*)
                                                       ,',var)
                                              (if (referrer-p ,',value)
                                                  (referred-value ,',value *restored-values*)
                                                  ,',value)))
                                 *need-to-fix*))
                         (setting (gethash ,',key ,',var) ,getting-place)))))
       (let ((,var ,create))
         ,@body
         ,var))))
|#
;; -----------------------------------------------------------------------

(defstruct referrer val)

(defun referred-value (referrer hash)
  "Return the value REFERRER is meant to be by looking in HASH."
  (gethash (referrer-val referrer)
           hash))

(defclass resolving-backend (backend) 
  ()
  (:documentation "A backend which does the setup for resolving circularities."))

(defun create-serialize-hash ()
  (make-hash-table :test #'eql :size *store-hash-size*))

(defun do-with-serialization-unit (store-hash restore-hash body-fn)
  (let ((*grouped-store-hash*   (or store-hash   (create-serialize-hash)))
        (*grouped-restore-hash* (or restore-hash (create-serialize-hash))))
    (funcall body-fn)))

(defmacro with-serialization-unit ((&key store-hash restore-hash)
                                   &body body)
  "Executes body in a single serialization unit allowing various internal data
structures to be reused.
The keys store-hash and restore-hash are expected to be either nil or
hash-tables as produced by the function create-serialize-hash."
  `(do-with-serialization-unit ,store-hash ,restore-hash (delay ,@body)))

#|
(defmacro with-serialization-unit ((&key store-hash restore-hash)
                                   &body body)
  "Executes body in a single serialization unit allowing various internal data
structures to be reused.
The keys store-hash and restore-hash are expected to be either nil or
hash-tables as produced by the function create-serialize-hash."
  `(let ((*grouped-store-hash* (or ,store-hash (create-serialize-hash)))
         (*grouped-restore-hash* (or ,restore-hash (create-serialize-hash))))
     ,@body))
|#

(defun get-store-hash ()
  (when *check-for-circs*
    (if (boundp '*grouped-store-hash*)
        (clrhash *grouped-store-hash*)
        (create-serialize-hash))))

(defun get-restore-hash ()
  (when *check-for-circs*
    (if (boundp '*grouped-restore-hash*)
        (clrhash *grouped-restore-hash*)
        (create-serialize-hash))))

(defmethod backend-store :around ((backend resolving-backend) (place t) (obj t))
  (call-next-method))

(defmethod backend-store ((backend resolving-backend) (place stream) (obj t))
  "Store OBJ into PLACE. Does the setup for counters and seen values."
  (declare (optimize speed (safety 1) (debug 0)))
  (let ((*stored-counter* 0) 
        (*stored-values* (get-store-hash)))
    (store-backend-code backend place)
    (backend-store-object backend obj place)
    obj))

(defun seen (obj)
  "Has this object already been stored?"
  (declare (optimize speed (safety 0) (debug 0)))
  (gethash obj *stored-values*))

(defun update-seen (obj)
  "Register OBJ as having been stored."
  (declare (optimize speed (safety 0) (debug 0)))
  (incf *stored-counter*)
  (setf (gethash obj *stored-values*) *stored-counter*)
  nil)

(deftype not-circ ()
  "Type grouping integers and characters, which we
  don't bother to check if they have been stored before"
  '(or integer character null (eql t) (eql $unbound-marker)))

(defun needs-checkp (obj)
  "Do we need to check if this object has been stored before?"
  (not (typep obj 'not-circ)))

(defgeneric store-referrer (backend obj place)
  (:documentation "Store the number OBJ into PLACE as a referrer for BACKEND.")
  (:method ((backend resolving-backend) (obj t) (place t))
    (store-error  "store-referrer must be specialized for backend ~(~A~)."
                  (name backend))))


(defun get-ref (obj)
  (declare (optimize speed (safety 0) (debug 0)))
  (if (needs-checkp obj)
      (multiple-value-bind (val win) (seen obj)
        (if win
            val
          (update-seen obj)))
    nil))

(defmethod backend-store-object ((backend resolving-backend) (obj t) (place t))
  "Store object if we have not seen this object before, otherwise retrieve
  the referrer object for it and store that using store-referrer."
  (aif (and *check-for-circs*
            (get-ref obj))
      (store-referrer backend it place)
    ;; else
    (internal-store-object backend obj place)))
       
(defmethod backend-restore ((backend resolving-backend) (place stream))
  "Restore an object from PLACE using BACKEND. Does the setup for 
  various variables used by resolving-object."
  (let ((*restore-counter* 0)
        (*need-to-fix* nil)
        (*restored-values* (get-restore-hash)))
    (check-magic-number backend place)
    (prog1
      (backend-restore-object backend place)
      (um:foreach 'force *need-to-fix*)
      )))

(defun update-restored (spot val)
  (declare (optimize speed (safety 0) (debug 0)))
  (setf (gethash spot *restored-values*) val))

(defun handle-normal (backend reader place)
  (declare (optimize speed (safety 1) (debug 0)))
  (let* ((spot (incf *restore-counter*)) ;; let* forces ordering DM/RAL 08/09
         (vals (new-val (internal-restore-object backend reader place))))
    (update-restored spot vals)
    vals))

(defgeneric referrerp (backend reader)
  (:method ((backend t) (reader t))
   (error "referrerp must be specialized for backend ~A." (name backend)))) 

(defun handle-restore (place backend)
  (declare (optimize speed (safety 1) (debug 0)))
  (let ((reader (get-next-reader backend place)))
    (declare (type symbol reader))
    ;; (format t "~&  rdr: ~A" reader)
    (cond ((referrerp backend reader) 
           (new-val (internal-restore-object backend reader place)))
          ((not (int-or-char-p backend reader))
           (handle-normal backend reader place))
          (t (new-val (internal-restore-object backend reader place))))))

(defmethod backend-restore-object ((backend resolving-backend) (place t))
  "Retrieve a object from PLACE, does housekeeping for circularity fixing."
  (declare (optimize speed (safety 1) (debug 0)))
  (if *check-for-circs*
      (handle-restore place backend)
    (call-next-method)))

; This used to be called int-sym-or-char-p
; but was renamed to handle eq symbols (gensym's mainly).
; The basic concept is that we don't bother
; checking for circularities with integers or
; characters since these aren't gauranteed to be eq 
; even if they are the same object. 
; (notes for eq in CLHS).
(defgeneric int-or-char-p (backend fn)
  (:method ((backend backend) (fn symbol))
    "Is function FN registered to restore an integer or character in BACKEND."
    (member fn '(integer character)))) 

(defun new-val (val)
  "Tries to get a referred value to reduce unnecessary cirularity fixing."
  (declare (optimize speed (safety 1) (debug 0)))
  (if (referrer-p val)
      (multiple-value-bind (new-val win) (referred-value val *restored-values*)
        (if win
            new-val
          val))
      val))

;; EOF
