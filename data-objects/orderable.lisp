
(in-package :orderable)

;; --------------------------------------

(defvar *order-count* 0)

(defclass <orderable-mixin> ()
  ;; multilocks must be acquired in total order
  ;; to prevent deadlocks
  ((id   :reader order-id
         :initform (sys:atomic-fixnum-incf *order-count*))
   ))

(defmethod ord:compare ((a <orderable-mixin>) (b <orderable-mixin>))
  (- (order-id a) (order-id b)))
