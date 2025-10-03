;; unshared-list.lisp
;;
;; DM/RAL  2023/12/27 11:56:04 UTC
;; ----------------------------------

(in-package #:loenc)

;; ----------------------------------
;; Proper Lists that have no shared spine cells can be serialized
;; efficiently as a vector of elements. Determining whether or not a
;; list is both proper and unshared (except for possibly its head), is
;; a problem of potentially exponential complexity.
;;
;; But sometimes you already know it is both proper and unshared, so
;; you can make your own declaration that it is an UNSHARED-LIST. We
;; translate that internally to a private struct that gets serialized
;; as a vector of elements, and deserialized as a list.
;;
;; Good example is a &REST list of args which has not yet been
;; destructured.
;;
;; A LIST can be an UNSHARED-LIST if no other references exist to a
;; tail of the list. A tail of a list is any (NTHCDR n list), for n > 0,
;; which excludes the list head.
;;
;; LOENC:ENCODE seeks to abbreviate references to objects referenced
;; more than once. The object is serialized just once, and thereafter
;; an abbreviation marks the place of repeated references to it. This
;; is how EQ items can be serialized and then be deserialized again as
;; EQ.
;;
;; UNSHARED-LIST can often be serialized more efficiently than a LIST,
;; since no abbreviations need to be constructed for CDR tails. An
;; UNSHARED-LIST needs only an abbreviation (at most) to just the head
;; of the list. Hence the list and its elements may be treated
;; similarly to a vector and its vector elements.
;;
;; Without an explicit declaration and conversion to UNSHARED-LIST,
;; trying to determine if a LIST could be an UNSHARED-LIST has
;; potentially exponential complexity. Or else it could only be
;; encoded in a second pass, after all other elements have been
;; examined and abbreviated.
;;

(defstruct (internal-unshared-list
            (:constructor make-internal-unshared-list (cells)))
  (cells #() :type vector :read-only t))

(defgeneric unshared-list (obj)
  (:method ((obj list))
   (let ((vec (ignore-errors ;; catch improper lists
                (coerce obj 'vector))))
     (or (and vec
              (make-internal-unshared-list vec))
         obj)))
  (:method ((obj internal-unshared-list))
   obj)
  (:method (obj)
   (error "List required: ~S" obj)))

;; -----------------------------------------------------
;; BEFORE-STORE - allows you to translate an object into a specialized
;; form for serialization. Specialized forms are typically struct
;; wrappers encapsulating some data.

(defgeneric before-store (obj)
  (:method (obj)
   obj))

(defmethod sdle-store:backend-store-object :around (backend obj stream)
  (call-next-method backend (before-store obj) stream))

;; -------------------------------
;; AFTER-RESTORE -- allows the inverse translation from a specialized
;; form to some more useful form. It might not necessarily become the
;; same kind of object as handed to BEFORE-STORE.

(defgeneric after-restore (obj)
  (:method (obj)
   obj)
  (:method ((obj internal-unshared-list))
   (coerce (internal-unshared-list-cells obj) 'list)))

(defmethod sdle-store:backend-restore-object :around (backend place)
  (after-restore (call-next-method)))

;; --------------------------------------------
;; Special Abbreviated format for UNSHARED-LIST items.

(defconstant +UNSHARED-LIST+ (register-code 124 'internal-unshared-list))

(defstore (obj internal-unshared-list stream)
  (output-type-code +UNSHARED-LIST+ stream)
  (let* ((vec  (internal-unshared-list-cells obj))
         (nel  (length vec)))
    (store-count nel stream)
    (loop for item across vec do
            (store-object item stream))))

(defrestore (internal-unshared-list stream)
  (let* ((nel  (read-count stream))
         (vec  (make-array nel)))
    (sdle-store:resolving-object (obj vec)
      (dotimes (ix nel)
        (sdle-store:setting (aref obj ix) (restore-object stream))))
    (make-internal-unshared-list vec)))


