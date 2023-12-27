;; unshared-list.lisp
;;
;; DM/RAL  2023/12/27 11:56:04 UTC
;; ----------------------------------

(in-package #:loenc)

;; ----------------------------------
;; Proper Lists that have no shared spine cells can be serialized
;; efficiently as a vector of elements. Determining whether or not a
;; list is both proper and unshared (except for possibly its head), is
;; a problem of indefinite complexity.
;;
;; But sometimes you already know it is both proper and unshared, so
;; you can make your own declaration that it is an UNSHARED-LIST. We
;; translate that internally to a private struct that gets serialized
;; as a vector of elements, and deserialized as a list.
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
;;
;; AFTER-RESTORE -- allows the inverse translation from a specialized
;; form to some more useful form. It might not necessarily become the
;; same kind of object as handed to BEFORE-STORE.

(defgeneric before-store (obj)
  (:method (obj)
   obj))

(defmethod sdle-store:backend-store-object :around (backend obj stream)
  (call-next-method backend (before-store obj) stream))

;; -------------------------------

(defgeneric after-restore (obj)
  (:method (obj)
   obj)
  (:method ((obj internal-unshared-list))
   (coerce (internal-unshared-list-cells obj) 'list)))

(defmethod sdle-store:backend-restore-object :around (backend place)
  (after-restore (call-next-method)))
