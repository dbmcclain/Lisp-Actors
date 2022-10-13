
(in-package :com.ral.orderable)

;; --------------------------------------

(mpcompat:defglobal *order-count* (list 0))

(defclass <orderable-mixin> ()
  ;; multilocks must be acquired in total order
  ;; to prevent deadlocks
  ((id   :reader order-id
         :initform (mpcompat:atomic-fixnum-incf (car *order-count*)))
   ))

(defmethod ord:compare ((a <orderable-mixin>) (b <orderable-mixin>))
  (- (order-id a) (order-id b)))
