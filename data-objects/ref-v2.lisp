;; ref.lisp -- SMP indirect refs
;;
;; DM/RAL  02/17
;; -------------------------------------------------------------
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

(in-package #:ref)
   
(declaim (optimize (speed 3) #|(safety 0)|# #+:LISPWORKS (float 0)))
(declaim (inline basic-ref-value basic-set-ref-value basic-cas))

;; ====================================================================
;; REF - a mostly read-only indirect reference cell
;; can only be mutated through CAS

(defclass ref ()
  ((cell :reader ref-cell :initarg :cell))
  (:default-initargs
   :cell (vector nil)
   ))

(defmethod refp (x)
  nil)

(defmethod refp ((ref ref))
  ref)

(defmacro ref-place (ref)
  `(svref (the simple-vector (ref-cell ,ref)) 0))

(defun basic-ref-value (ref)
  ;; basic-xxx should never be augmented in child classes
  (ref-place ref))

(defmethod ref-value ((ref ref))
  (basic-ref-value ref))

(defmethod deref (x)
  x)

(defmethod deref ((r ref))
  (ref-value r))

(defun basic-set-ref-value (ref val)
  ;; basic-xxx should never be augmented in child classes
  (setf (ref-place ref) val))

(defmethod set-ref-value ((ref ref) val)
  (basic-set-ref-value ref val))

(defsetf basic-ref-value basic-set-ref-value)
(defsetf ref-value       set-ref-value)

(defmethod initialize-instance :after ((ref ref) &key (val nil val-present-p) &allow-other-keys)
  (when val-present-p
    (setf (basic-ref-value ref) val)))

(defun make-ref (&optional val)
  (make-instance 'ref
                 :val  val))

(defmethod ref (x)
  ;; type conversion
  (make-ref x))

(defmethod ref ((ref ref))
  ref)

(defmethod copy-ref ((ref ref))
  (ref (ref-value ref)))

(defun basic-cas (ref old new)
  ;; basic-xxx should never be augmented in child classes
  (sys:compare-and-swap (ref-place ref) old new))

(defmethod cas ((ref ref) old new)
  (basic-cas ref old new))

(defmethod atomic-exch ((ref ref) val)
  (sys:atomic-exchange (ref-place ref) val))

(defmethod atomic-incf ((ref ref))
  #+:LISPWORKS
  (system:atomic-fixnum-incf (ref-place ref))
  #+:ALLEGRO
  (excl:incf-atomic (ref-place ref)))

(defmethod atomic-decf ((ref ref))
  #+:LISPWORKS
  (system:atomic-fixnum-decf (ref-place ref))
  #+:ALLEGRO
  (excl:decf-atomic (ref-place ref)))

;; -----------------------------------------

(defmethod um:rmw ((ref ref) val-fn)
  ;; Generic Read-Modify-Write of Refs. It must be the case that the
  ;; val-fn does not destructively modify the value held in the ref
  ;; object.  And it should not generate non-idempotent side effects,
  ;; since the call to val-fn may occur more than once in the event of
  ;; access collisions.
  (declare (function val-fn))
  (um:nlet-tail iter ()
    (let* ((old  (ref-value ref))
           (new  (funcall val-fn old)))
      (if (cas ref old new)
          new
        (iter)))
    ))

;; ---------------------------------------------------------------
;; WREF - Weak Reference

(defclass wref (ref)
  ()
  (:default-initargs
   :cell (make-array 1
                     :weak t)
   ))

(defmethod wrefp (x)
  nil)

(defmethod wrefp ((w wref))
  w)

(defmethod wref (x)
  (make-instance 'wref
                 :cell (make-array 1
                                   :weak t
                                   :initial-element x)))

(defmethod wref ((w wref))
  w)

(defmethod wref ((r ref))
  (wref (ref-value r)))

;; ---------------------------------------------------------------
;; CREF - Immutable (Copying, or Constant) Ref - like permanent COW
;;
;; Can't enforce COW behavior in Lisp, so on first deref, we assume we are
;; about to do damage to the underlying. Also, once written to, we are no
;; longer COW.
;;
;; So don't do
;;
;;    (let ((val (var-val v)) ;; read-only val
;;          (ref (var-ref v)))
;;      (setf (ref-value ref) val)
;;
;; but okay to do:
;;
;;    (let ((ref (var-ref v)))
;;      (setf (ref-value ref) (VAR-VAL v))
;;
;;  -- or --
;;
;;    (let ((val (var-val v)) ;; read-only val
;;          (ref (var-ref v)))
;;      (setf (ref-value ref) (CLONE val))
;;
;; Once opened for writing, all subsequent read refs use COW access,
;; which clones before reevealing the value.

(defclass cref (ref)
  ())

(defmethod crefp (x)
  nil)

(defmethod crefp ((c cref))
  c)

(defmethod ref ((c cref))
  (copy-ref c))

(defmethod ref-value ((c cref))
  (um:nlet-tail iter ((old (basic-ref-value c)))
    (let* ((new  (clone old)) ;; could take an indefinite amount of time...
           (old2 (basic-ref-value c)))
      (if (eq old old2)
          new
        (iter old2)))))

(defmethod cref (x)
  (make-instance 'cref
                 :val x))

(defmethod cref ((c cref))
  c)

(defmethod cref ((r ref))
  (cref (basic-ref-value r)))

(defmethod um:rmw ((ref cref) val-fn)
  ;; Generic Read-Modify-Write of Refs. It must be the case that the
  ;; val-fn does not destructively modify the value held in the ref
  ;; object.  And it should not generate non-idempotent side effects,
  ;; since the call to val-fn may occur more than once in the event of
  ;; access collisions.
  (declare (function val-fn))
  (um:nlet-tail iter ()
    (let* ((old  (basic-ref-value ref))
           (new  (funcall val-fn (clone old))))
      (if (cas ref old new)
          new
        (iter)))
    ))

;; ---------------------------------------------------------------
;; COW - Copy on Write
;;
;; Can't enforce COW behavior in Lisp, so on first deref, we assume we are
;; about to do damage to the underlying. Also, once written to, we are no
;; longer COW.
;;
;; So don't do
;;
;;    (let ((val (var-val v)) ;; read-only val
;;          (ref (var-ref v)))
;;      (setf (ref-value ref) val)
;;
;; but okay to do:
;;
;;    (let ((ref (var-ref v)))
;;      (setf (ref-value ref) (VAR-VAL v))
;;
;;  -- or --
;;
;;    (let ((val (var-val v)) ;; read-only val
;;          (ref (var-ref v)))
;;      (setf (ref-value ref) (CLONE val))
;;
;; Once opened for writing, all subsequent read refs use COW access,
;; which clones before reevealing the value.

(defclass cow (cref)
  ())

(defmethod cowp (x)
  nil)

(defmethod cowp ((c cow))
  c)

(defmethod ref-value ((c cow))
  (um:rmw c #'identity))

(defun become-ref (c)
  (change-class c 'ref))

(defmethod set-ref-value :after ((c cow) val)
  (become-ref c))

(defmethod cow (x)
  (make-instance 'cow
                 :val x))

(defmethod cow ((c cow))
  c)

(defmethod cow ((r ref))
  (cow (basic-ref-value r)))

(defmethod atomic-incf :before ((c cow))
  (ref-value c))

(defmethod atomic-decf :before ((c cow))
  (ref-value c))

(defmethod atomic-exch :after ((c cow) val)
  (become-ref c))

(defmethod cas :around ((c cow) old new)
  (when (call-next-method)
    (become-ref c)
    t))
  
;; ------------------------------
;; All objects subject to FSTM must define a FSTM:CLONE method
;;
;; CLONE is called when opening for write access.
;;
;; NOTE: These are all shallow (skeleton spine) copies Be careful not
;; to mutate any of the contained objects, but mutation of spine cells
;; is fine.  When operating on a clone, the use of single-thread
;; algorithms is fine. No other threads can see it.

(defgeneric clone (obj)
  (:method ((obj sequence))
   (copy-seq obj))
  (:method ((s structure-object))
   (copy-structure s))
  (:method ((r ref))
   (copy-ref r))
  (:method ((n number))
   n)
  (:method ((c character))
   c)
  (:method ((f function))
   f)
  (:method ((s symbol))
   s)
  (:method ((arr array))
   ;; potentially expensive
   ;; If arr is adjustable, the clone will be too.
   ;; If arr is displaced to another arr, clone will be a non-displaced copy
   ;; Only the structure is copied. If any elements are mutable objects
   ;; the cloned array will still point to those same mutable objects.
   ;; Be Careful!!
   (let* ((dims (array-dimensions arr))
          (adj  (adjustable-array-p arr))
          (eltp (array-element-type arr))
          (ans  (make-array dims
                            :adjustable adj
                            :element-type eltp))
          (src  (make-array (array-total-size arr)
                            :element-type eltp
                            :displaced-to arr))
          (dst  (make-array (array-total-size ans)
                            :element-type eltp
                            :displaced-to ans)))
     (replace dst src)
     ans)))
  
