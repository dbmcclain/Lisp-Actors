;; unshared-list.lisp
;;
;; DM/RAL  2023/12/27 11:56:04 UTC
;; ----------------------------------

(in-package #:loenc)

;; ----------------------------------

(defstruct (unshared-list
            (:constructor make-unshared-list (cells)))
  (cells nil :type list))

(defgeneric unshared-list (obj)
  (:method ((obj list))
   (make-unshared-list obj))
  (:method ((obj unshared-list))
   obj)
  (:method (obj)
   (error "List required: ~S" obj)))

(defstruct private-unshared-vec
  cells)

(defmethod sdle-store:backend-store-object :around (backend (obj unshared-list) stream)
  (let ((vobj (make-private-unshared-vec
               :cells (coerce (unshared-list-cells obj) 'vector))))
    (call-next-method backend vobj stream)))

(defgeneric after-restore (obj)
  (:method (obj)
   obj)
  (:method ((obj private-unshared-vec))
   (make-unshared-list (coerce (private-unshared-vec-cells obj) 'list))))

(defmethod sdle-store:backend-restore-object :around (backend place)
  (after-restore (call-next-method)))
