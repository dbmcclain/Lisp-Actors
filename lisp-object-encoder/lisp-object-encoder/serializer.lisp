;; serializer.lisp
;;
;; DM/RAL  2026/06/15T23:25:41U
;; ----------------------------------

(in-package #:serializer)

;; --------------------------------------------
;; DM/RAL  2026/06/15T22:48:38U
#|

NR-LINEARIZE-TREE -- intended as a preparatory step on the way to data
  serialization via octet encoding. It detects and encodes shared nodes
  in a tree graph, which breaks cycles, and then we linearize all lists,
  both proper and improper, into vector representation.

  We already understand Lists and Vectors, including self- and
  cross-referential collections. But we need help finding other data
  references.

  All structured Lisp objects which could refer to other data should
  have methods for MAKE-SERIALIZABLE and DESERIALIZE-TYPE. This includes
  DEFSTRUCT and Class Instances, HASH-TABLEs, etc. MAKE-SERIALIZABLE can
  freely return cyclic data with sharing. That will be untangled by
  NR-LINEARIZE-TREE.

  Once encoded into a ENCODED-TREE object, we are ready to commit to an
  octet representation.

The octet encoder only needs to understand how to encode/decode
fundamental atom data types of Lisp (numbers, characters, vectors,
etc.), along with only 6 very simple data structures whose data have
all been linearized, i.e., made sharing- and cycle-free.

Alternatively, one could use a human readable textual encoding of
these linearized trees.

The use of high-level preparation allows for a massive simplification
to the octet encoder.

NR-REFLATE-TREE -- intended as the final step in decoding
  data, after it has been restored from octet streams to an ENCODED-TREE
  representation. It calls upon DESERIALIZE-TYPE for all objects which
  had MAKE-SERIALIZABLE methods defined. NR-REFLATE-TREE restores the proper and
  improper lists, and reconnect the sharing cycles in the tree graph.

Methods for MAKE-SERIALIZABLE should take an object and return two values - a
  type code, and the data, in list or vector tree form, sufficient to
  recreate an object of identical type.  The type code is arbitrary, but
  should be unique. It can be numeric or symbolic.

Methods for DESERIALIZE-TYPE take two arguments - the type code, and
  the data tree containing the data from MAKE-SERIALIZABLE. They should
  reconstruct an identical object and return it. It is anticipated
  that these methods will use EQL specialization on the TYPE code
  argument.

  What is the cost of not providing MAKE-SERIALIZABLE and DESERIALIZE-TYPE?
The SDLE octet encoder is not as adept with general lists, and so to
avoid missing out on item sharing, it must encode every CONS cell of
any lists it encounters. This is wasteful if you could have known that
nobody else has references to any but the head of the list.

[As you see below, it is impossible to perform this trimming in just
  one pass thru the data. We needed 3 passes:
      1st pass detects sharing, and replaces all repeated references with REF nodes.
      2nd pass walks the tree again, looking for first references and converting them into DEF nodes.
      3rd pass to convert all (now, non-cyclic) lists, proper and improper (dotted) into simple-vectors.]

  So by allowing NR-LINEARIZE-TREE to preprocess the data, we get
complete detection and facilitation of general sharing.  All lists can
be encoded as simple vectors in the octet stream.  Sharing is
maximized, and compressed storage requirements are had by not encoding
the individual CONS cells of LIST spines.

  A simple test showing the encoding, both preprocessed, and raw, of
list of 100 integers. The preprocessed octed stream requires 203
bytes, while the raw encoding requires 302 bytes. The CONS cell spine
of the list is fully 50% of the data size. That entire spine is
removed from the preprocessed encoding.

|#
;; --------------------------------------------
;; Encoding structures - REF, DEF, VEF, VEF* and USER-SER
;; will appear in the encoded representations, along with fundamental
;; data types, and vectors. But no LISTs!! (other than NIL).

(defstruct encoded)

(defstruct (ref
            (:include encoded))
  ;; Encodes a reference to shared data.
  ix)

(defstruct (def
            (:include encoded))
  ;; Encodes the definition of shared data.
  ix obj)

(defstruct (vef
            (:include encoded))
  ;; Encodes a proper LIST as a vector.
  vec)

(defstruct (vef*
            (:include encoded))
  ;; Encodes an improper LIST* as a vector.
  vec)

(defstruct (user-ser
            (:include encoded))
  ;; Encodes a user serialization of an object.
  ;; TYPE will typically be a symbol or an numeric code
  ;; DATA should be a data tree (Lists and Vectors of elementary types).
  type data)

;; --------------------------------------------
;; !! IMPORTANT !!
;; --------------------------------------------
;; Handing an ENCODED-TREE structure to someone attests that the
;; tree is free of cycles, and contains no proper nor improper (aka
;; dotted-) lists. This is a strong attestation.
;;
;; There are at least 2 reasons for the need of this attestation:
;;
;; (1) Attempting to decode a non-encoded-tree risks non-termination
;; due to possible cycles in the tree. The decoding process depends
;; critically on the absense of any cycles.
;;
;; (2) Attempting to re-encode an already encoded tree, risks mixing
;; the encoding of two separate epochs, each with their own local
;; reference counting. They can't meaningfully coexist.
;; --------------------------------------------
;; --------------------------------------------

(defstruct (encoded-tree
            (:include encoded))
  ;; contains the encoded tree
  top)

;; --------------------------------------------
;; --------------------------------------------

(defun proper-list-p (lst)
  (alexandria:proper-list-p lst))

(defun array-effective-size (x)
  (if (array-has-fill-pointer-p x)
      (fill-pointer x)
    (array-total-size x)))

(defun gp-array-p (x)
  (and (arrayp x)
       (plusp (array-effective-size x))
       (eql t (array-element-type x))))

(defun ensure-proper-list (lst)
  (um:nlet iter ((lst lst)
                 (ans nil))
    (cond ((consp lst)
           (go-iter (cdr lst) (cons (car lst) ans)))
          ((null lst)
           (nreverse ans))
          (t
           (nreverse (cons lst ans)))
          )))

#| ;; works okay, but not needed
(defun length* (lst)
  (um:nlet iter ((lst lst)
                 (ct  0))
    (if (consp lst)
        (if (listp (cdr lst))
            (go-iter (cdr lst) (1+ ct))
          (1+ ct))
      ct)))
|#

#|
(ensure-proper-list '(a b . c))
|#


(defgeneric shareable-p (x)
  ;; True if we need ref counting on item
  (:method ((x (eql nil)))
   nil)
  (:method ((x (eql t)))
   nil)
  (:method ((x number))
   nil)
  (:method ((x character))
   nil)
  (:method (x)  ;; any others?
   t))

;; --------------------------------------------

(defgeneric make-serializable (obj)
  (:method (obj)
   (values nil obj)))

(defgeneric deserialize-type (typ dat))
   
;; --------------------------------------------
;; Internal opcodes for constant stack-space recursions...
;;
;; None of these will appear in the external representation. They
;; merely guide the serialization process.
;;
;; We operate an input execution queue and a result stack.
;; Basically a ZAMS machine. Take operands from a queue, and push
;; results onto a stack.

;; --------------------------------------------
;; By making them structs we can use OPCODE-P to test their general
;; category as opcodes

(defstruct opcode
  fn)

(defmacro opcode (name &optional fn)
  `(defparameter ,name
     (make-opcode
      :fn  ',fn)))

(defmacro opcodes (&rest names)
  `(progn
     ,@(mapcar (lambda (op)
                 (if (consp op)
                     `(opcode ,@op)
                   `(opcode ,op)))
               names)))

(opcodes +OP-DEF+
         +OP-LIST+
         +OP-LIST*+
         (+OP-CONS+ zams-cons)
         (+OP-ITER+ zams-iter)
         (+OP-SER+  zams-ser))

;; --------------------------------------------
;; Constant stack space routines (NR-) by using a ZAMs

(defvar *patch-table*  nil)
(defvar *fixup-table*  nil)

(defun zams-ret  (ops tl obj def)
  (values
   (cdr ops)
   (cons
    (if def
        (make-def :ix def :obj obj)
      obj)
    tl)
   ))

(defun zams-iter (ops stk)
  (destructuring-bind (elt vec ix limit def . tl) stk
    (setf (row-major-aref vec ix) elt)
    (when (and *patch-table*
               (user-ser-p elt))
      (push (let ((ix ix))
              (lambda (obj)
                (setf (row-major-aref vec ix) obj)))
            (gethash elt *patch-table*)))
    (incf ix)
    (cond ((< ix limit)
           (setf (third stk) ix)
           (values (cons (row-major-aref vec ix)
                         ops)
                   (cdr stk)))
          (t
           (zams-ret ops tl vec def))
          )))

(defun zams-cons (ops stk)
  (destructuring-bind (new-cdr new-car cell def . tl) stk
    (setf (car cell) new-car
          (cdr cell) new-cdr)
    (when *patch-table*
      (when (user-ser-p new-car)
        (push (lambda (obj)
                (setf (car cell) obj))
              (gethash new-car *patch-table*)))
      (when (user-ser-p new-cdr)
        (push (lambda (obj)
                (setf (cdr cell) obj))
              (gethash new-cdr *patch-table*))))
    (zams-ret ops tl cell def)
    ))

(defun zams-ser (ops stk)
  (destructuring-bind (new-data new-type struct def . tl) stk
    (cond
     (*fixup-table*
      (let ((new-struct (loenc:after-restore
                         (deserialize-type new-type new-data))))
      (map nil (um:rcurry #'funcall new-struct) (gethash struct *fixup-table*))
      (setf struct new-struct)))
     
     (t
      (setf (user-ser-type struct) new-type
            (user-ser-data struct) new-data)
      (when (and *patch-table*
                 (user-ser-p new-data))
        (push (lambda (obj)
                (setf (user-ser-data struct) obj))
              (gethash new-data *patch-table*))
        )))
    (zams-ret ops tl struct def)
    ))

(defun zams-dispatch (obj ops stk)
  (funcall (opcode-fn obj) ops stk))

;; --------------------------------------------
;; And now for some construction macros with intentional capture

(defmacro next (&key ops stk)
  `(progn
     (psetq ops ,ops stk ,stk)
     (go next)))

(defmacro handle-opcode (obj)
  `(multiple-value-bind (new-ops new-stk)
       (zams-dispatch ,obj ops stk)
     (next :ops new-ops
           :stk new-stk)))
 
(defmacro with-next-obj ((obj) data-handler &key op-handler)
  `(let ((,obj  (car ops)))
     (if (opcode-p ,obj)
         ,(or op-handler `(handle-opcode ,obj))
       ,data-handler)))
#+:LISWORKS
(editor:setup-indent "with-next-obj" 1)

(defmacro tree-handler (((obj tree) &key ret) handler &key op-handler)
  `(prog  ((ops  (list ,tree))
           (stk nil))
     NEXT
     (if (endp ops)
         (return (values (car stk) ,@ret))
       (with-next-obj (,obj)
       ,handler
       :op-handler ,op-handler))))
#+:LISPWORKS
(editor:setup-indent "tree-handler" 1)

;; --------------------------------------------

(defun copy-tree-and-refer-to-shared-nodes (tree)
  (let ((ref-counts (make-hash-table :test #'eq))
        (ref-labels (make-hash-table :test #'eq))
        (alts-table (make-hash-table :test #'eq))
        (refctr 0))

    ;; Pass #1 - form a proper deep copy of the tree, taking into
    ;; account structure sharing, and convert pointers to shared nodes
    ;; into REFs.
    ;;
    ;; We also turn shareable structs into USER-SER objects, so we can
    ;; share object internals, and reduce the space cost of
    ;; serializing.
    ;;
    ;; This removes both sharing and cycles. The tree becomes a DAG.
    ;; Every node in this DAG has only one direct reference to itself
    ;; from somewhere higher up.
    ;;
    ;; We can only discern, in this pass, a shared reference after
    ;; seeing the second reference to an object already seen. We have
    ;; to rescan, in Pass #2, to find the first reference again, and
    ;; turn it into a DEF node.
    (tree-handler ((obj tree) :ret (ref-labels alts-table))
      (let* ((share?  (shareable-p obj))
             (ct      (if share? 
                          (incf (gethash obj ref-counts 0))
                        1)))
        (cond
         ((> ct 2)
          (next :ops (cdr ops)
                :stk (cons (make-ref :ix (gethash obj ref-labels))
                           stk)
                ))
         ((= ct 2)
          (let ((ref-id (incf refctr)))
            (setf (gethash obj ref-labels) ref-id)
            (next :ops (cdr ops)
                  :stk (cons (make-ref :ix ref-id)
                             stk)
                  )))
         ((consp obj)
          (let ((new  (list nil)))
            (setf (gethash new alts-table) obj)
            (next :ops (list* (car obj)
                              (cdr obj)
                              +OP-CONS+
                              (cdr ops))
                  :stk (list* new
                              nil
                              stk)
                  )))
         ((gp-array-p obj)
          (let ((new  (alexandria:copy-array obj)))
            (setf (gethash new alts-table) obj)
            (next :ops (list* (row-major-aref obj 0)
                              +OP-ITER+
                              (cdr ops))
                  :stk (list* new
                              0 (array-effective-size obj)
                              nil
                              stk)
                  )))
         (t
          (let ((bs-obj (loenc:before-store obj)))
            (multiple-value-bind (typ data)
                (make-serializable bs-obj)
              (cond (typ
                     (let ((new (make-user-ser)))
                       (setf (gethash new alts-table) obj)
                       (next :ops (list* typ
                                         data
                                         +OP-SER+
                                         (cdr ops))
                             :stk (list*  new
                                          nil
                                          stk)
                             )))
                    (t
                     (unless (eq obj bs-obj)
                       (setf (gethash bs-obj alts-table) obj))
                     (next :ops (cdr ops)
                           :stk (cons bs-obj
                                      stk)
                           ))
                    ))))
         )))
    ))

(defun label-tree-with-shared-nodes (tree ref-labels alts-table)
  
  ;; 2nd pass... Looking for DEF's.
  ;;
  ;; REF nodes were created because we saw a repeated reference. We
  ;; could not know that it was repeated unless we had seen it at
  ;; least once before. The first look should be converted to a DEF.
  
  (tree-handler ((obj tree))
    (let* ((share?  (shareable-p obj))
           (def     (when share?  ;; special for NIL
                      (let ((alt (gethash obj alts-table obj)))
                        (gethash alt ref-labels)))))
      (cond
       ((consp obj)
        (next :ops (list* (car obj)
                          (cdr obj)
                          +OP-CONS+
                          (cdr ops))
              :stk (list* obj
                          def
                          stk)
              ))
       ((gp-array-p obj)
        (next :ops (list* (row-major-aref obj 0)
                          +OP-ITER+
                          (cdr ops))
              :stk (list* obj
                          0 (array-effective-size obj)
                          def
                          stk)
              ))
       ((user-ser-p obj)
        (next :ops (list* (user-ser-type obj)
                          (user-ser-data obj)
                          +OP-SER+
                          (cdr ops))
              :stk (list* obj
                          def
                          stk)
              ))
       (t
        (next :ops (cdr ops)
              :stk (cons
                    (if def
                        (make-def :ix def :obj obj)
                      obj)
                    stk)
              ))
       ))))

(defun vector-linearize-tree (tree)
  
  ;; 3rd and Final stage - convert all lists into vector structs.
  ;; Proper lists become VEF objects.
  ;; Improper lists become VEF* objects.

  (tree-handler ((obj tree))
    (cond
     ((consp obj)
      (next :ops (list*
                  (coerce (ensure-proper-list obj) 'vector)
                  +OP-LIST+
                  (cdr ops))
            :stk (cons
                  (proper-list-p obj)
                  stk)
            ))
     ((gp-array-p obj)
      (next :ops (list* (row-major-aref obj 0)
                        +OP-ITER+
                        (cdr ops))
            :stk (list* obj
                        0 (array-effective-size obj)
                        nil
                        stk)
            ))
     ((def-p obj)
      (next :ops (list* (def-obj obj)
                        +OP-DEF+
                        (cdr ops))
            :stk (cons obj stk)
            ))
     ((user-ser-p obj)
      (next :ops (list* (user-ser-type obj)
                        (user-ser-data obj)
                        +OP-SER+
                        (cdr ops))
            :stk (list* obj
                        nil
                        stk)
            ))
     (t
      (next :ops (cdr ops)
            :stk (cons obj stk)
            )))
    :op-handler (cond
                 ((eq obj +OP-LIST+)
                  (destructuring-bind (vec proper . tl) stk
                    (next :ops (cdr ops)
                          :stk (cons (if proper
                                         (make-vef :vec vec)
                                       (make-vef* :vec vec))
                                     tl)
                          )))
                 ((eq obj +OP-DEF+)
                  (destructuring-bind (new-val def . tl) stk
                    (setf (def-obj def) new-val)
                    (next :ops (cdr ops)
                          :stk (cons def tl)
                          )))
                 (t
                  (handle-opcode obj)))
    ))

(defun nr-linearize-tree (tree)
  ;; Non-destructive encoding
  (multiple-value-bind (new-tree ref-labels alts-table)
      (copy-tree-and-refer-to-shared-nodes tree)
    (make-encoded-tree
     :top (vector-linearize-tree
           (label-tree-with-shared-nodes new-tree ref-labels alts-table)))
    ))

;; --------------------------------------------

(defun copy-tree-reflating-vectors (tree)
  (let ((def-table (make-hash-table)))
    
    ;; First pass - reinflate the VEF & VEF* objects to proper and
    ;; improper lists, and look for DEFs, recording them in the
    ;; def-table.
    ;;
    ;; We are supposed to be cycle free. So list reflation should
    ;; terminate.
    (tree-handler ((obj (encoded-tree-top tree)) :ret (def-table))
      (cond
       ((vef-p obj)
        (let* ((vec  (vef-vec obj))
               (nel  (length vec)))
          (assert (plusp nel)) ;; should always be true
          (next :ops (list* vec
                            +OP-LIST+
                            (cdr ops))
                :stk stk)
          ))
       ((vef*-p obj)
        (let* ((vec  (vef*-vec obj))
               (nel  (length vec)))
          (assert (> nel 1)) ;; should always be true
          (next :ops (list* vec
                            +OP-LIST*+
                            (cdr ops))
                :stk (cons (1- nel)
                           stk)
                )))
       ((def-p obj)
        (next :ops (list* (def-obj obj)
                          +OP-DEF+
                          (cdr ops))
              :stk (cons (def-ix obj)
                         stk)
              ))
       ((user-ser-p obj)
        (next :ops (list* (user-ser-type obj)
                          (user-ser-data obj)
                          +OP-SER+
                          (cdr ops))
              :stk (list* (make-user-ser)
                          nil
                          stk)
              ))
       ((gp-array-p obj)
        (next :ops (list* (row-major-aref obj 0)
                          +OP-ITER+
                          (cdr ops))
              :stk (list* (alexandria:copy-array obj)
                          0 (array-effective-size obj)
                          nil
                          stk)
              ))
       (t
        (next :ops (cdr ops)
              :stk (cons obj stk)
              )))
      :op-handler  (cond
                    ((eq obj +OP-LIST+)
                     (destructuring-bind (vec . tl) stk
                       (next :ops (cdr ops)
                             :stk (cons (coerce vec 'list)
                                        tl)
                             )))
                    ((eq obj +OP-LIST*+)
                     (destructuring-bind (vec nlast . tl) stk
                       (let ((lst  (butlast (coerce vec 'list))))
                         (setf (cdr (last lst)) (aref vec nlast))
                         (next :ops (cdr ops)
                               :stk (cons lst tl)
                               ))))
                    ((eq obj +OP-DEF+)
                     (destructuring-bind (val ix . tl) stk
                       (setf (gethash ix def-table) val)
                       (next :ops (cdr ops)
                             :stk (cons val tl)
                             )))
                    (t
                     (handle-opcode obj)))
      )))

(defun resolve-shared-node-references (tree def-table)
  ;; Pass #2 - In the second pass we replace all the REF objects with
  ;; the object referenced from the def-table filled in from Pass #1.
  (let ((ref-table     (make-hash-table :test #'eq))
        (*patch-table* (make-hash-table :test #'eq)))
    (tree-handler ((obj tree) :ret (*patch-table*))
      (let* ((share? (shareable-p obj))
             (ct     (if share?
                         (incf (gethash obj ref-table 0))
                       1)))
        (cond
         ((> ct 1)
          (next :ops (cdr ops)
                :stk (cons obj stk)
                ))
         ((consp obj)
          (next :ops (list* (car obj)
                            (cdr obj)
                            +OP-CONS+
                            (cdr ops))
                :stk (list* obj
                            nil
                            stk)
                ))
         ((gp-array-p obj)
          (next :ops (list* (row-major-aref obj 0)
                            +OP-ITER+
                            (cdr ops))
                :stk (list* obj
                            0 (array-effective-size obj)
                            nil
                            stk)
                ))
         ((ref-p obj)
          (let ((new  (gethash (ref-ix obj) def-table)))
            (next :ops (cons new
                             (cdr ops))
                  :stk stk
                  )))
         ((user-ser-p obj)
          (next :ops (list* (user-ser-type obj)
                            (user-ser-data obj)
                            +OP-SER+
                            (cdr ops))
                :stk (list* obj
                            nil
                            stk)
                ))
         (t
          (let ((ar-obj  (loenc:after-restore obj)))
            (if (eq ar-obj obj)
                (next :ops (cdr ops)
                      :stk (cons obj
                                 stk))
              (next :ops (cons obj
                               (cdr ops))
                    :stk stk)
              )))
         )))
    ))

(defun chk-enc (obj)
  (if (encoded-p obj)
      (error "Should not be an ENCODED value: ~S" obj)
    obj))

(defun resolve-user-structs (tree *fixup-table*)
  (let ((reftbl (make-hash-table :test #'eq)))
    (tree-handler ((obj tree))
      (let* ((share?  (shareable-p obj))
             (ct      (if share?
                          (incf (gethash obj reftbl 0))
                        1)))
        (cond
         ((> ct 1)
          (next :ops (cdr ops)
                :stk (cons obj stk)
                ))
             
         ((consp obj)
          (next :ops (list* (car obj)
                            (cdr obj)
                            +OP-CONS+
                            (cdr ops))
                :stk (list* obj
                            nil
                            stk)
                ))
         ((gp-array-p obj)
          (next :ops (list* (aref obj 0)
                            +OP-ITER+
                            (cdr ops))
                :stk (list* obj
                            0 (array-effective-size obj)
                            nil
                            stk)
                ))
         ((user-ser-p obj)
          (next :ops (list* (user-ser-type obj)
                            (user-ser-data obj)
                            +OP-SER+
                            (cdr ops))
                :stk (list* obj
                            nil
                            stk)
                ))
         (t
          (next :ops (cdr ops)
                :stk (cons obj stk)
                ))
         )))
    ))
  
(defun nr-reflate-tree (tree)
  ;; Non-destructive decoding
  (multiple-value-bind (new-tree def-table)
      (copy-tree-reflating-vectors tree)
    (multiple-value-bind (res-tree places-table)
        (resolve-shared-node-references new-tree def-table)
      (resolve-user-structs res-tree places-table)
      )))

;; --------------------------------------------

#|
(nr-linearize-tree '(a b c d))
(nr-linearize-tree '(a b c . d))
(let ((sub '(a b c)))
  (nr-linearize-tree (list sub sub)))
(let ((lst  (list* 'a 'b 'c 'd)))
  (setf (cdr (last lst)) (cdr lst))
  (nr-linearize-tree lst))

(setf *print-circle* t)
(setf *print-level* 10)

(nr-reflate-tree (nr-linearize-tree '(a b c d)))
(nr-reflate-tree (nr-linearize-tree '(a b c . d)))
(let ((sub '(a b c)))
  (nr-reflate-tree (nr-linearize-tree (list sub sub))))
(let ((lst  (list* 'a 'b 'c 'd)))
  (setf (cdr (last lst)) (cdr lst))
  (nr-reflate-tree (nr-linearize-tree lst)))

(let ((lst '(let ((lst  (list* 'a 'b 'c 'd)))
              (setf (cdr (last lst)) (cdr lst))
              (nr-reflate-tree (nr-linearize-tree lst)))))
  ;; (nr-linearize-tree lst)
  (nr-reflate-tree (nr-linearize-tree lst))
  )

(let ((lst  (list* 'a 'b 'c 'd)))
  (setf (cdr (last lst)) (cdr lst))
  
  ;; Produce a ENCODED-TREE - okay
  ;; (nr-linearize-tree lst)

  ;; Try an unencoded tree - not an Encoded Tree Error
  ;; (nr-reflate-tree lst)

  ;; Try to get past the gatekeeper - can't encode an already-encoded tree error
  ;; (nr-linearize-tree (nr-linearize-tree lst))
  ;; (nr-linearize-tree (encoded-tree-top (nr-linearize-tree lst)))

  ;; The only correct statement
  ;; (nr-reflate-tree (nr-linearize-tree lst))
  
  ;; Try to sneak past the gatekeeper... -- forbidden data cycle detected
  ;; (nr-reflate-tree (make-encoded-tree :top (nr-reflate-tree (nr-linearize-tree LST))))
  )
|#
;; --------------------------------------------

(defun encode (obj &rest loenc-args)
  (apply #'ex-encode (nr-linearize-tree obj) loenc-args))

(defun try-reflate (dec)
  (if (encoded-tree-p dec)
      (nr-reflate-tree dec)
    (let ((err-str  "SER:DECODE was called on old LOENC:ENCODE'd data"))
      (if cl-user::*break-on-reflate-error*
          (break "Break: ~A" err-str)
        (warn err-str))
      dec)
    ))

(defun decode (enc &rest loenc-args)
  (try-reflate (apply #'ex-decode enc loenc-args)))


(defun serialize (obj stream &rest loenc-args)
  (apply #'ex-serialize (nr-linearize-tree obj) stream loenc-args))
  
(defun deserialize (stream &rest loenc-args)
  (let ((dec (apply #'ex-deserialize stream loenc-args)))
    (if (eq dec stream)  ;; loenc:ex-deserialize produces STREAM for EOF.
        dec
      (try-reflate dec))
    ))

;; --------------------------------------------

(defconstant +ENCODED-TREE+ (loenc:register-code 112 'encoded-tree))

(loenc:defstore (obj encoded-tree stream)
  (sdle-store:output-type-code +ENCODED-TREE+ stream)
  (sdle-store:store-object (encoded-tree-top obj) stream))

(loenc:defrestore (encoded-tree stream)
  (make-encoded-tree
   :top (sdle-store:restore-object stream)))


(defconstant +VEF+  (loenc:register-code 113 'vef))

(loenc:defstore (obj vef stream)
  (sdle-store:output-type-code +VEF+ stream)
  (sdle-store::store-simple-vector-body (vef-vec obj) stream))

(loenc:defrestore (vef stream)
  (make-vef
   :vec  (sdle-store::restore-simple-vector-body stream)))


(defconstant +VEF*+  (loenc:register-code 114 'vef*))

(loenc:defstore (obj vef* stream)
  (sdle-store:output-type-code +VEF*+ stream)
  (sdle-store::store-simple-vector-body (vef*-vec obj) stream))

(loenc:defrestore (vef* stream)
  (make-vef*
   :vec  (sdle-store::restore-simple-vector-body stream)))


(defconstant +REF+  (loenc:register-code 115 'ref))

(loenc:defstore (obj ref stream)
  (sdle-store:output-type-code +REF+ stream)
  (sdle-store:store-count (ref-ix obj) stream))

(loenc:defrestore (ref stream)
  (make-ref
   :ix  (sdle-store:read-count stream)))


(defconstant +DEF+  (loenc:register-code 116 'def))

(loenc:defstore (obj def stream)
  (sdle-store:output-type-code +DEF+ stream)
  (sdle-store:store-count (def-ix obj) stream)
  (sdle-store:store-object (def-obj obj) stream))

(loenc:defrestore (def stream)
  (make-def
   :ix  (sdle-store:read-count stream)
   :obj (sdle-store:restore-object stream)))


(defconstant +USER-SER+  (loenc:register-code 118 'user-ser))

(loenc:defstore (obj user-ser stream)
  (sdle-store:output-type-code +USER-SER+ stream)
  (sdle-store:store-object (user-ser-type obj) stream)
  (sdle-store:store-object (user-ser-data obj) stream))

(loenc:defrestore (user-ser stream)
  (make-user-ser
   :type  (sdle-store:restore-object stream)
   :data  (sdle-store:restore-object stream)))

;; --------------------------------------------
#|
;; A good example of the savings afforded by storing list elements in simple vectors...
;; Here we have a list of 100 integer. A comparison of preprocessed vs raw LOENC encoding,
;; shows that preprocessed takes 203 bytes, while raw takes 302 bytes. The same data are encoded,
;; but the preprocessing allows for the elision of the CONS spine. The CONS overhead is 50% !!

(let* ((str (um:range 1 100))
       (enc (encode str)))
  (let ((*Print-level* 10))
    (pprint (nr-linearize-tree (list str))))
  (list (length enc)
        (length (loenc:encode (list str)))
        enc
        (decode enc)))
|#
;; --------------------------------------------
#|
(defstruct thing
  a b c d)

(let* ((ref  (uuid:make-v1-uuid))
       (lst  '(a b c))
       (x (list (make-thing
                 :a #(1 2 3 4)
                 :b ref
                 :c 301
                 :d lst)
                (make-thing
                 :a '(4 5 6)
                 :b ref
                 :c 500
                 :d lst))))
  (decode (encode x)))
|#
