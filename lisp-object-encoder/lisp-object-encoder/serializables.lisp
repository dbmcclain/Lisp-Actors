;; serializables.lisp
;;
;; DM/RAL  2026/06/16T21:49:50U
;; ----------------------------------

(in-package #:serializer)

;; ----------------------------------
;; UUID's

(defmethod make-serializable ((obj uuid:uuid))
  (values :UUID
          (uuid:uuid-to-byte-array obj)))

(defmethod deserialize-type ((typ (eql :UUID)) dat)
  (uuid:byte-array-to-uuid dat))

;; --------------------------------------------
;; HASH-TABLES

(defmethod make-serializable ((ht hash-table))
  (values :HASH-TABLE
          (list (hash-table-rehash-size ht)
                (hash-table-rehash-threshold ht)
                (hash-table-size ht)
                (hash-table-test ht)
                (loop for key being the hash-keys of ht
                        using (hash-value value)
                      collect
                      (cons key value))
                )))

(defmethod deserialize-type ((type (eql :HASH-TABLE)) data)
  (destructuring-bind (rehash-size rehash-threshold size test kvs) data
    (let ((ht (make-hash-table :test test
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold
                               :size size)))
      (loop for (key . value) in kvs do
              (setf (gethash key ht) value))
      ht)))

;; --------------------------------------------
#|
(let ((id #/uuid/{72C0839E-6872-11F1-8592-05C994CCFF01}))
  (nr-reflate-tree (nr-linearize-tree id))
  ;; (nr-linearize-tree id)
  ;; (nr-linearize-tree (nr-linearize-tree id))
  )
|#
;; --------------------------------------------
;; General Structs

#+:LISPWORKS
(defmethod make-serializable ((obj structure-object))
  (values :STRUCT
          (let* ((obj-class  (class-of obj))
                 (class-name (class-name obj-class))
                 (slot-names (structure:structure-class-slot-names obj-class)))
            (list class-name
                  slot-names
                  (mapcar (um:curry #'slot-value obj) slot-names))
            )))

#+:LISPWORKS
(defmethod deserialize-type ((type (eql :STRUCT)) data)
  (destructuring-bind (class-name slot-names slot-values) data
    (let ((class  (sdle-store::find-or-create-class class-name
                                                    'standard-object
                                                    slot-names
                                                    'standard-class)))
      (cond ((eq (type-of class) 'structure-class)
             ;; we apparently found the class and it was a struture
             (let* ((new-instance  (structure::allocate-instance class))
                    (allowed-slots (structure:structure-class-slot-names class)))
               (loop for slot-name in slot-names
                     for val       in slot-values
                     do
                       (let ((slot-sym (find slot-name allowed-slots
                                        :test #'string-equal)))
                         (when slot-sym
                           (setf (slot-value new-instance slot-sym) val)
                           )))
               new-instance))

            (t ;; else -- no such struture known to mankind
               ;; dummy up as a new standard-object instead of a struct
               ;; (to avoid non-portability issues regarding internals of struct implementation)
               (let ((new-instance (allocate-instance class)))
                 (loop for slot-name in slot-names
                         for     val in slot-values
                         do
                         (setf (slot-value new-instance slot-name) val))
                 new-instance))
            ))))

#+:SBCL
(defmethod make-serializable ((obj structure-object))
  (values :STRUCT
          (gather-instance-data obj)))

#+:SBCL
(defmethod deserialize-type ((type (eql :STRUCT)) data)
  (restore-type-object 'structure-object 'structure-class data))


;; --------------------------------------------
;; Class Instances

(defun gather-instance-data (obj)
  (let* ((all-slots (remove-if
                     (complement (lambda (slot)
                                   (let ((slot-name (slot-definition-name slot)))
                                     (and (slot-boundp obj slot-name)
                                          (or sdle-store::*store-class-slots*
                                              (not (eql (clos:slot-definition-allocation slot)
                                                        :class)))))))
                     (sdle-store::serializable-slots obj)) )
         (slot-names (mapcar #'slot-definition-name all-slots)))
    (list (class-name (class-of obj))
          slot-names
          (mapcar (um:curry #'slot-value obj) slot-names)
          )))

(defmethod make-serializable ((obj standard-object))
  (values :INSTANCE
          (gather-instance-data obj)))

(defmethod make-serializable ((obj condition))
  (values :CONDITION
          (gather-instance-data obj)))


(defun restore-type-object (obj-type metaclass data)
  (destructuring-bind (class-name slot-names slot-values) data
    (let* ((class  (sdle-store::find-or-create-class class-name
                                                    obj-type
                                                    slot-names
                                                    metaclass))
           (new-instance (allocate-instance class)))
      (loop for slot-name in slot-names
            for val in slot-values
            do
              (setf (slot-value new-instance slot-name) val))
      new-instance)
    ))

(defmethod deserialize-type ((type (eql :INSTANCE)) data)
  (restore-type-object 'standard-object 'standard-class data))

(defmethod deserialize-type ((type (eql :CONDITION)) data)
  (restore-type-object 'condition 'standard-class data))

;; --------------------------------------------
;; RB-Trees

(defmethod make-serializable ((obj maps:empty))
  (values :MAP-EMPTY nil))

(defmethod make-serializable ((obj sets:node))
  (if (maps::map-cell-p (sets::node-v obj))
      (values :RB-MAP
              (mapcar (lambda (cell)
                        (cons (maps:map-cell-key cell)
                              (maps:map-cell-val cell)))
                      (sets:elements obj)))
    (values :RB-SET
            (sets:elements obj))
    ))

(defmethod deserialize-type ((type (eql :MAP-EMPTY)) data)
  (declare (ignore data))
  (maps:empty))

(defmethod deserialize-type ((type (eql :RB-SET)) data)
  (let ((tree (sets:empty)))
    (dolist (item data)
      (sets:addf tree item))
    tree))

(defmethod deserialize-type ((type (eql :RB-MAP)) data)
  (let ((tree (maps:empty)))
    (dolist (pair data)
      (maps:addf tree (car pair) (cdr pair)))
    tree))



