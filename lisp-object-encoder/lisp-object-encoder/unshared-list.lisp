;; unshared-list.lisp
;;
;; DM/RAL  2023/12/27 11:56:04 UTC
;; ----------------------------------

(in-package #:loenc)

;; ----------------------------------

(defstruct (internal-unshared-list
            (:constructor make-internal-unshared-list (cells)))
  (cells #() :type vector :read-only t))

(defgeneric unshared-list (obj)
  (:method ((obj list))
   (make-internal-unshared-list (coerce obj 'vector)))
  (:method ((obj internal-unshared-list))
   obj)
  (:method (obj)
   (error "List required: ~S" obj)))

;; -----------------------------------------------------

(defgeneric before-store (obj)
  (:method (obj)
   obj))

(defmethod sdle-store:backend-store-object :around (backend obj stream)
  (call-next-method backend (before-store obj) stream))


(defgeneric after-restore (obj)
  (:method (obj)
   obj)
  (:method ((obj internal-unshared-list))
   (coerce (internal-unshared-list-cells obj) 'list)))

(defmethod sdle-store:backend-restore-object :around (backend place)
  (after-restore (call-next-method)))
