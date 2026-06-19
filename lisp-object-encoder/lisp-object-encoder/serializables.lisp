;; serializables.lisp
;;
;; DM/RAL  2026/06/16T21:49:50U
;; ----------------------------------

(in-package #:serializer)

#|
Question: Which is more space efficient? Parallel K/V lists? or List of Pairs?

Answer:
  Parallel K/V lists:  VEF Vec Len k1 k2 k3 ... | VEF Vec Len v1 v2 v3 ...,     so 2*(3+N*L(x))   = 6 + 2*N*L(x)
  List of Pairs:       VEF Vec Len | VEF* Vec 2 k1 v1 | VEF* Vec 2 k2 v2 | ..., so 3+N*(3+2*L(x)) = 3*(N+1)+2*N*L(x)

  So, in general, for N > 1, Parallel K/V Lists takes less space.

  So our encodings strive to use parallel k/v lists.

  We could shorten the encoding of CONS-cell pairs. Then we would have 3+N overhead. Still grows with N.
|#
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
  (let ((keys nil)
        (vals nil))
    (maphash (lambda (k v)
               (push k keys)
               (push v vals))
             ht)
    (values :HASH-TABLE
            (list (hash-table-rehash-size ht)
                  (hash-table-rehash-threshold ht)
                  (hash-table-size ht)
                  (hash-table-test ht)
                  keys vals
                  ))))

(defmethod deserialize-type ((type (eql :HASH-TABLE)) data)
  (destructuring-bind (rehash-size rehash-threshold size test keys vals) data
    (let ((ht (make-hash-table :test test
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold
                               :size size)))
      (map nil (lambda (k v)
                 (setf (gethash k ht) v))
           keys vals)
      ht)))

;; --------------------------------------------
#|
(let ((id #/uuid/{72C0839E-6872-11F1-8592-05C994CCFF01}))
  (nr-reflate-tree (nr-linearize-tree id))
  ;; (nr-linearize-tree id)
  ;; (nr-linearize-tree (nr-linearize-tree id))
  )

(let ((ht  (make-hash-table)))
  (dolist (pair '((a . 1)
                  (b . 2)
                  (c . 3)))
    (setf (gethash (car pair) ht) (cdr pair)))
  (inspect (decode (encode ht)))
  (let ((enc (encode ht)))
    (list (length enc) enc))))
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

;; --------------------------------------------
;; Class Instances

(defun gather-instance-data (obj)
  (let* ((all-slots (remove-if
                     (complement (lambda (slot)
                                   (let ((slot-name (slot-definition-name slot)))
                                     (and (slot-boundp obj slot-name)
                                          (or sdle-store::*store-class-slots*
                                              (not (eql (slot-definition-allocation slot)
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

#+:SBCL
(defmethod make-serializable ((obj structure-object))
  (values :STRUCT
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

#+:SBCL
(defmethod deserialize-type ((type (eql :STRUCT)) data)
  (restore-type-object 'structure-object 'structure-class data))

;; -------------------------------------------------------
;; Classes

(defmethod make-serializable ((obj standard-class))
  (values :CLASS
          (list (class-name obj)
                (mapcar #'sdle-store::get-slot-details (class-direct-slots obj))
                (mapcar (if sdle-store::*store-class-superclasses*
                            #'identity 
                          #'class-name)
                        (class-direct-superclasses obj))
                (class-name (class-of obj))
                )))

(defmethod deserialize-type ((typ (eql :CLASS)) data)
  (destructuring-bind (class slots supers meta) data
    (let* ((keywords '(:direct-slots :direct-superclasses
                       :metaclass))
           (final (loop for keyword in keywords
                        for slot in (list slots 
                                          (or supers
                                              (list 'standard-object))
                                          meta)
                        nconc (list keyword slot))))
      (cond ((find-class class nil)
             (cond (sdle-store::*nuke-existing-classes*
                    (apply #'ensure-class class final)
                    #+(and clisp (not mop)) (add-methods-for-class class slots))
                   (t (find-class class))))
            (t (apply #'ensure-class class final)
               #+(and clisp (not mop)) (add-methods-for-class class slots))
            ))))

;; built in classes

(defmethod make-serializable ((obj built-in-class))
  (values :BUILT-IN-CLASS
          (class-name obj)))

(defmethod deserialize-type ((type (eql :BUILT-IN-CLASS)) data)
  (find-class data))

;; --------------------------------------------
;; RB-Trees

(defmethod make-serializable ((obj maps:empty))
  :RB-EMPTY)

(defmethod make-serializable ((obj sets:node))
  (let ((elts  (sets:elements obj)))
    (if (maps::map-cell-p (sets::node-v obj))
        (let ((keys nil)
              (vals nil))
          (dolist (cell elts)
            (push (maps:map-cell-key cell) keys)
            (push (maps:map-cell-val cell) vals))
          (values :RB-MAP
                  (list  keys vals)))
      (values :RB-SET elts))
    ))

(defmethod deserialize-type ((type (eql :RB-EMPTY)) data)
  (declare (ignore data))
  (maps:empty))

(defmethod deserialize-type ((type (eql :RB-SET)) data)
  (let ((tree (sets:empty)))
    (dolist (item data)
      (sets:addf tree item))
    tree))

(defmethod deserialize-type ((type (eql :RB-MAP)) data)
  (destructuring-bind (keys vals) data
    (let ((tree (maps:empty)))
      (map nil (lambda (k v)
                 (maps:addf tree k v))
           keys vals)
      tree)))
