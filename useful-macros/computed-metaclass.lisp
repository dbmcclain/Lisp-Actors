;; computed-metaclass.lisp
;; --------------------------------------------------------------------------------------
;; Computed object metaclass. For Memory Mapped Computed Objects 
;;
;; Not transactioned. Single user only.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  11/08
;; --------------------------------------------------------------------------------------
;; --------------------------------------------------
(in-package :useful-macros)
;; --------------------------------------------------
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

;; -----------------------------------------------------------
;; Computed-slot-definition
;; -----------------------------------------------------------
;; mixin metaclass for computed slots and methods to make them
;; appear normal

(defclass computed-slot-definition (clos:standard-slot-definition)
  ((function :initarg :function
             :accessor computed-slot-definition-function)))

(defmethod slot-definition-allocation ((slotd computed-slot-definition))
  :computed)

(defmethod (setf slot-definition-allocation) (allocation (slotd computed-slot-definition))
  (unless (eq allocation :computed)
    (error "Cannot change the allocation of a ~S"
           'computed-direct-slot-definition))
  allocation)

;; ---------------------------------------
;; class of direct computed slots and methods to construct them
;; when appropriate

(defclass computed-direct-slot-definition (clos:standard-direct-slot-definition
                                           computed-slot-definition)
  ())

;; ---------------------------------------
;; Class of effective computed slots and methods to construct
;; them when appropriate

(defclass computed-effective-slot-definition
          (clos:standard-effective-slot-definition
           computed-slot-definition)
  ())

;; -----------------------------------------------------------
;; Computed-Metalevel-Class
;; --------------------------------------------------------------------------------------
;; metaclass of objects that might contain extended slots

(defclass computed-metalevel-class (#+:LISPWORKS clos:standard-class
                              #+:ALLEGRO clos::standard-class
			      #+:CLOZURE standard-class
			      #+:SBCL    standard-class)
  ())

;; ---------------------------------------
;; Called when the class is being made, to choose the metaclass of
;; a given direct slot. It should return the class of slot definition required.

(defmethod clos:direct-slot-definition-class
           ((class computed-metalevel-class) &rest initargs)
  (case (getf initargs :allocation)
    (:computed
     (find-class 'computed-direct-slot-definition))
    (t
     (call-next-method))))

;; ---------------------------------------
;; Called when the defclass is expanded, to process a slot option.
;; It should return the new list of slot options, based on
;; already-processed-options.

#+:LISPWORKS
(defmethod clos:process-a-slot-option ((class computed-metalevel-class) option value
                                       already-processed-options slot)
  ;; Hande the :function option by adding it to the list of
  ;; processed options.
  (case option
    (:function (list* :function value already-processed-options))
    (t (call-next-method))
    ))

;; --------------------------------------------------------------
;; Called when the class is being finalized, to choose the
;; metaclass of a given effective slot. It should return the
;; class of slot definition required.

(defmethod clos:effective-slot-definition-class
           ((class computed-metalevel-class) &rest initargs)
  ;; Use computed-effective-slot-definition if appropriate
  (let ((slot-allocation (getf initargs :allocation)))
    (case slot-allocation
      (:computed
       (find-class 'computed-effective-slot-definition))
      (t (call-next-method))
      )))

;; ------------------------------------------------------------

(defmethod clos:compute-effective-slot-definition
    ((class computed-metalevel-class)
     slot-name
     direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((effective-slotdef (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (typecase slotd
        (computed-slot-definition
         (setf (computed-slot-definition-function effective-slotdef)
               (computed-slot-definition-function slotd))
         (return))
        ))
    effective-slotdef))

;; -------------------------------------------------------------------

;; underlying access methods for invoking
;; computed-slot-definition-function.

#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-value-using-class
           ((class computed-metalevel-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (typecase slotd
      (computed-slot-definition
       (funcall (computed-slot-definition-function slotd)
                :get object))

      (t (call-next-method))
      )))

#+(OR :CLOZURE :SBCL)
(defmethod clos:slot-value-using-class
           ((class computed-metalevel-class) object slot-def)
  (typecase slot-def
    (computed-slot-definition
     (funcall (computed-slot-definition-function slot-def)
              :get object))
    
    (t (call-next-method))
    ))

;; ---------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod (setf clos:slot-value-using-class)
           (value (class computed-metalevel-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (typecase slotd
      (computed-slot-definition
       (funcall (computed-slot-definition-function slotd)
                :set object value))

      (t (call-next-method))
      )))

#+(OR :CLOZURE :SBCL)
(defmethod (setf clos:slot-value-using-class)
           (value (class computed-metalevel-class) object slot-def)
  (typecase slot-def
    (computed-slot-definition
     (funcall (computed-slot-definition-function slot-def)
              :set object value))
    
    (t (call-next-method))
    ))

;; ---------------------------------------

;; not needed since we appear to be a normal instance slot
#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-boundp-using-class
           ((class computed-metalevel-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (typecase slotd
      (computed-slot-definition
       (funcall (computed-slot-definition-function slotd)
                :is-set object))

      (t (call-next-method))
      )))

#+(OR :CLOZURE :SBCL)
(defmethod clos:slot-boundp-using-class
           ((class computed-metalevel-class) object slot-def)
  (typecase slot-def
    (computed-slot-definition
     (funcall (computed-slot-definition-function slot-def)
              :is-set object))
    
    (t (call-next-method))
    ))

;; ---------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod clos:slot-makunbound-using-class
           ((class computed-metalevel-class) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (typecase slotd
      (computed-slot-definition
       (funcall (computed-slot-definition-function slotd)
                :unset object))

      (t (call-next-method))
      )))

#+(OR :CLOZURE :SBCL)
(defmethod clos:slot-makunbound-using-class
           ((class computed-metalevel-class) object slot-def)
  (typecase slot-def
    (computed-slot-definition
     (funcall (computed-slot-definition-function slot-def)
              :unset object))
    
    (t (call-next-method))
    ))

;; ---------------------------------------
;; is this method really necessary?

#+:LISPWORKS
(defmethod clos:slot-exists-p-using-class
           ((class computed-metalevel-class) object slot-name)
  (or (call-next-method)
      (and (find slot-name (clos:class-slots class)
                 :key 'clos:slot-definition-name)
           t)))

