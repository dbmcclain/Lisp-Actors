;; serializer.lisp
;;
;; DM/RAL  2026/06/15T23:25:41U
;; ----------------------------------

(defpackage #:serializer
  (:use #:common-lisp)
  (:export
   #:encode
   #:decode
   #:serialize
   #:deserialize

   #:make-serializable
   #:deserialize-type
   ))

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

(defun gp-vector-p (x)
  (and (vectorp x)
       (plusp (length x))
       (eql t (array-element-type x))))

(defun dotted-to-proper-list (lst)
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
(dotted-to-proper-list '(a b . c))
|#


(defgeneric sharable-p (x)
  ;; True if we 
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
;; Basically a ZAMS machine.

(defstruct opcode)
(defstruct (new-cons
            (:include opcode)))
(defstruct (new-def
            (:include opcode)))
(defstruct (new-list
            (:include opcode)))
(defstruct (new-list*
            (:include opcode)))
(defstruct (new-iter
            (:include opcode)))
(defstruct (new-user-ser
            (:include opcode)))

(defparameter *new-cons*     (make-new-cons))
(defparameter *new-def*      (make-new-def))
(defparameter *new-list*     (make-new-list))
(defparameter *new-list**    (make-new-list*))
(defparameter *new-iter*     (make-new-iter))
(defparameter *new-user-ser* (make-new-user-ser))

;; --------------------------------------------
;; Constant stack space routines (NR-) by using a ZAMs

(defun copy-tree-and-refer-to-shared-nodes (tree)
  (let ((ref-counts (make-hash-table :test #'eq))
        (ref-labels (make-hash-table :test #'eq))
        (alts-table (make-hash-table :test #'eq))
        (refctr 0))

    ;; First stage - form a proper deep copy of the tree, taking into
    ;; account structure sharing, and convert pointers-to-shared-nodes
    ;; into REFs.
    ;;
    ;; This removes both sharing and cycles. The tree becomes a DAG.
    ;; Every node in this DAG has only one direct reference to itself
    ;; from somewhere higher up.
    
    (um:nlet iter ((xq  (list tree))
                   (ans nil))
      (if (endp xq)
          (progn
            ;; (inspect ref-counts)
            (values (car ans) ref-labels alts-table))
        (let ((obj (car xq)))
          (if (opcode-p obj)
              (cond
               ((new-cons-p obj)
                (destructuring-bind (new-cdr new-car old-obj . tl) ans
                  (let ((new (cons new-car new-cdr)))
                    (setf (gethash new alts-table) old-obj)
                    (go-iter (cdr xq) (cons new tl))
                    )))
               ((new-iter-p obj)
                (destructuring-bind (elt vec ix limit old-obj . tl) ans
                  (setf (aref vec ix) elt)
                  (incf ix)
                  (cond ((< ix limit)
                         (setf (third ans) ix)
                         (go-iter (cons (aref vec ix)
                                        xq)
                                  (cdr ans)))
                        (t
                         (setf (gethash vec alts-table) old-obj)
                         (go-iter (cdr xq) (cons vec tl)))
                        )))
               ((new-user-ser-p obj)
                (destructuring-bind (dat typ old-obj . tl) ans
                  (let ((new  (make-user-ser
                               :type typ
                               :data dat)))
                    (setf (gethash new alts-table) old-obj)
                    (go-iter (cdr xq)
                             (cons new tl))
                    ))))
            ;; else data items
            (let* ((share?  (sharable-p obj))
                   (ct  (if share? 
                            (incf (gethash obj ref-counts 0))
                          1))
                   (lbl (when share?
                          (gethash obj ref-labels))))
              (cond
               (lbl
                (go-iter (cdr xq) (cons (make-ref :ix lbl) ans)))
               ((> ct 1)
                (let ((lbl (incf refctr))
                      (alt (gethash obj alts-table obj)))
                  (setf (gethash alt ref-labels) lbl)
                  (go-iter (cdr xq) (cons (make-ref :ix lbl) ans))
                  ))
               ((consp obj)
                (go-iter (list* (car obj)
                                (cdr obj)
                                *new-cons*
                                (cdr xq))
                         (cons obj ans)))
               ((gp-vector-p obj)
                (go-iter (list* (aref obj 0)
                                *new-iter*
                                (cdr xq))
                         (list* (copy-seq obj)
                                0 (length obj)
                                obj
                                ans)))

               ((encoded-p obj)
                ;; oops!
                (error "You cannot encode an already-encoded tree.")
                ;;
                ;; And why not?
                ;;
                ;; Answer:
                
                ;; You would be mixing a collection of REFs and DEFs
                ;; from entirely different encoding epochs, each of
                ;; which used their own local integer numbering of
                ;; REFs and DEFs.
                ;;
                ;; Both are numbered locally without knowledge of each
                ;; other.  And so they will use duplicate numbering
                ;; schemes, referring to entirely different objects.
                ;;
                ;; Without resorting to timestamped REF's and DEF's we
                ;; would be at a complete loss.
                ;;
                ;; The best solution, if you must pass along an already
                ;; encoded tree, is to first convert that encoding to a
                ;; byte sequence and pass along that byte vector.
                ;;
                ;; ... or maybe we should consider using UUIDs to
                ;; number the refs and defs?
                )
               
               (t
                (let ((bs-obj (loenc:before-store obj)))
                  (multiple-value-bind (typ data)
                      (make-serializable bs-obj)
                    (if typ
                        (go-iter (list* typ
                                        data
                                        *new-user-ser*
                                        (cdr xq))
                                 (cons obj ans))
                      (go-iter (cdr xq)
                               (cons bs-obj ans))
                      ))))
               )))
          )))
    ))

(defun label-tree-with-shared-nodes (tree ref-labels alts-table)
  
  ;; 2nd pass... any references were invented
  ;; because we saw a repeated item. Could not
  ;; know it was repeated unless we had seen it
  ;; once before.
  ;;
  ;; When we made the first REF, we also flagged
  ;; that item to become a DEF. So rescan the
  ;; newly copied tree and convert the fist
  ;; appearances into DEFs.
  
  (um:nlet iter ((xq  (list tree))
                 (ans nil))
    (if (endp xq)
        (car ans)
      (let ((obj  (car xq)))
        (if (opcode-p obj)
            (cond
             ((new-cons-p obj)
              (destructuring-bind (new-cdr new-car cell def . tl) ans
                (setf (car cell) new-car
                      (cdr cell) new-cdr)
                (go-iter (cdr xq)
                         (cons (if def
                                   (make-def :ix def :obj cell)
                                 cell)
                               tl))
                ))
             ((new-iter-p obj)
              (destructuring-bind (elt vec ix limit def . tl) ans
                (setf (aref vec ix) elt)
                (incf ix)
                (cond ((< ix limit)
                       (setf (third ans) ix)
                       (go-iter (cons (aref vec ix)
                                      xq)
                                (cdr ans)))
                      (t
                       (go-iter (cdr xq)
                                (cons
                                 (if def
                                     (make-def :ix def :obj vec)
                                   vec)
                                 tl)))
                      )))
             ((new-user-ser-p obj)
              (destructuring-bind (dat typ struct def . tl) ans
                (setf (user-ser-type struct) typ
                      (user-ser-data struct) dat)
                (go-iter (cdr xq)
                         (cons
                          (if def
                              (make-def :ix def :obj struct)
                            struct)
                          tl))
                )))
          ;; else data items
          (let* ((alt  (when obj  ;; special for NIL
                         (gethash obj alts-table obj)))
                 (def  (when obj  ;; special for NIL
                         (gethash alt ref-labels))))
            (cond
             ((consp obj)
              (go-iter (list* (car obj)
                              (cdr obj)
                              *new-cons*
                              (cdr xq))
                       (list* obj
                              def
                              ans)))
             ((gp-vector-p obj)
              (go-iter (list* (aref obj 0)
                              *new-iter*
                              (cdr xq))
                       (list* obj
                              0 (length obj) def
                              ans)))
             ((user-ser-p obj)
              (go-iter (list* (user-ser-type obj)
                              (user-ser-data obj)
                              *new-user-ser*
                              (cdr xq))
                       (list* obj def ans)))
             (def
              (go-iter (cdr xq)
                       (cons (make-def :ix def :obj obj) ans)))
             (t
              (go-iter (cdr xq) (cons obj ans)))
             )))
        ))))

(defun vector-linearize-tree (tree)
  
  ;; Final stage - convert all lists into vector structs.
  ;; Proper lists become VEF objects.
  ;; Improper lists become VEF* objects.

  (um:nlet iter ((xq  (list tree))
                 (ans nil))
    (if (endp xq)
        (make-encoded-tree :top (car ans))
      (let ((obj (car xq)))
        (if (opcode-p obj)
            (cond
             ((new-cons-p obj)
              (destructuring-bind (vec proper . tl) ans
                (go-iter (cdr xq)
                         (cons (if proper
                                   (make-vef :vec vec)
                                 (make-vef* :vec vec))
                               tl))
                ))
             ((new-iter-p obj)
              (destructuring-bind (elt vec ix limit . tl) ans
                (setf (aref vec ix) elt)
                (incf ix)
                (cond ((< ix limit)
                       (setf (third ans) ix)
                       (go-iter (cons (aref vec ix)
                                      xq)
                                (cdr ans)))
                      (t
                       (go-iter (cdr xq) (cons vec tl)))
                      )))
             ((new-def-p obj)
              (destructuring-bind (new-val def . tl) ans
                (setf (def-obj def) new-val)
                (go-iter (cdr xq) (cons def tl))
                ))
             ((new-user-ser-p obj)
              (destructuring-bind (dat typ struct . tl) ans
                (setf (user-ser-type struct) typ
                      (user-ser-data struct) dat)
                (go-iter (cdr xq)
                         (cons struct
                               tl))
                )))
          ;; else - data items
          (cond
           ((consp obj)
            (if (proper-list-p obj)
                (go-iter  (list*
                           (coerce obj 'vector)
                           *new-cons*
                           (cdr xq))
                          (cons t ans))
              (let ((lst  (dotted-to-proper-list obj)))
                (go-iter (list*
                          (coerce lst 'vector)
                          *new-cons*
                          (cdr xq))
                         (cons nil ans))
                )))
           ((gp-vector-p obj)
            (go-iter (list* (aref obj 0)
                            *new-iter*
                            (cdr xq))
                     (list* obj
                            0 (length obj)
                            ans)))
           ((def-p obj)
            (go-iter (list* (def-obj obj)
                            *new-def*
                            (cdr xq))
                     (cons obj ans)))
           ((user-ser-p obj)
            (go-iter (list* (user-ser-type obj)
                            (user-ser-data obj)
                            *new-user-ser*
                            (cdr xq))
                     (cons obj ans)))
           (t
            (go-iter (cdr xq) (cons obj ans)))
           ))
        ))))

(defun nr-linearize-tree (tree)
  ;; Non-destructive encoding
  (multiple-value-bind (new-tree ref-labels alts-table)
      (copy-tree-and-refer-to-shared-nodes tree)
    (vector-linearize-tree
     (label-tree-with-shared-nodes new-tree ref-labels alts-table))
    ))

;; --------------------------------------------

(defun copy-tree-reflating-vectors (tree)
  (let ((reftbl (make-hash-table)))
    ;; First pass - reinflate the VEF & VEF* objects to
    ;; proper and improper lists, then apply the
    ;; process to each of their elements.
    ;;
    ;; Along the way we also record the DEFs into the
    ;; reftbl and replace them with their reinflated
    ;; def object.
    (um:nlet iter ((xq  (list (encoded-tree-top tree)))
                   (ans nil))
      (if (endp xq)
          (values (car ans) reftbl)
        (let ((obj  (car xq)))
          (if (opcode-p obj)
              (cond
               ((new-list-p obj)
                (destructuring-bind (vec . tl) ans
                  (go-iter (cdr xq)
                           (cons (coerce vec 'list) tl))
                  ))                               
               ((new-list*-p obj)
                (destructuring-bind (vec nlast . tl) ans
                  (let ((lst  (butlast (coerce vec 'list))))
                    (setf (cdr (last lst)) (aref vec nlast))
                    (go-iter (cdr xq)
                             (cons lst tl))
                    )))
               ((new-iter-p obj)
                (destructuring-bind (elt vec ix limit . tl) ans
                  (setf (aref vec ix) elt)
                  (incf ix)
                  (cond ((< ix limit)
                         (setf (third ans) ix)
                         (go-iter (cons (aref vec ix)
                                        xq)
                                  (cdr ans)))
                        (t
                         (go-iter (cdr xq)
                                  (cons vec tl))
                         ))))
               ((new-def-p obj)
                (destructuring-bind (val ix . tl) ans
                  (setf (gethash ix reftbl) val)
                  (go-iter (cdr xq) (cons val tl))
                  ))
               ((new-user-ser-p obj)
                (destructuring-bind (new-dat new-typ . tl) ans
                  (let ((new  (make-user-ser :type new-typ :data new-dat)))
                    (go-iter (cdr xq)
                             (cons new tl))
                    ))))
            ;; else - data items
            (cond
             ((vef-p obj)
              (let* ((vec  (vef-vec obj))
                     (nel  (length vec)))
                (assert (plusp nel)) ;; should always be true
                (go-iter (list* vec
                                *new-list*
                                (cdr xq))
                         ans)
                ))
             ((vef*-p obj)
              (let* ((vec  (vef*-vec obj))
                     (nel  (length vec)))
                (assert (> nel 1)) ;; should always be true
                (go-iter (list* vec
                                *new-list**
                                (cdr xq))
                         (cons (1- nel) ans))
                ))
             ((gp-vector-p obj)
              (go-iter (list* (aref obj 0)
                              *new-iter*
                              (cdr xq))
                       (list* (copy-seq obj)
                              0 (length obj)
                              ans)))
             ((def-p obj)
              (go-iter (list* (def-obj obj)
                              *new-def*
                              (cdr xq))
                       (cons (def-ix obj) ans)))
             ((user-ser-p obj)
              (go-iter (list* (user-ser-type obj)
                              (user-ser-data obj)
                              *new-user-ser*
                              (cdr xq))
                       ans))
             (t
              (go-iter (cdr xq) (cons obj ans)))
             ))
          ))
      )))

(defun resolve-shared-node-references (tree reftbl)
  ;; In the second pass we replace all the REF
  ;; objects with the object referenced from the
  ;; reftbl filled in from pass #1.
  (um:nlet iter ((xq  (list tree))
                 (ans nil))
    (if (endp xq)
        (car ans)
      (let ((obj  (car xq)))
        (if (opcode-p obj)
            (cond
             ((new-cons-p obj)
              (destructuring-bind (new-car new-cdr cell . tl) ans
                (setf (car cell) new-car
                      (cdr cell) new-cdr)
                (go-iter (cdr xq) (cons cell tl))
                ))
             ((new-iter-p obj)
              (destructuring-bind (elt vec ix limit . tl) ans
                (setf (aref vec ix) elt)
                (incf ix)
                (cond ((< ix limit)
                       (setf (third ans) ix)
                       (go-iter (cons (aref vec ix)
                                      xq)
                                (cdr ans)))
                      (t
                       (go-iter (cdr xq)
                                (cons vec tl)))
                      )))
             ((new-user-ser-p obj)
              (destructuring-bind (new-dat new-typ . tl) ans
                (go-iter (cdr xq)
                         (cons
                          (loenc:after-restore
                           (deserialize-type new-typ new-dat))
                          tl))
                )))
          ;; else - data items
          (flet ((anti-cycling (obj)
                   (when (find obj ans :test #'eq)
                     (error "Forbidden data cycle detected."))))
            (cond
             ((consp obj)
              (anti-cycling obj)
              (go-iter (list* (cdr obj)
                              (car obj)
                              *new-cons*
                              (cdr xq))
                       (cons obj ans)))
             ((gp-vector-p obj)
              (anti-cycling obj)
              (go-iter (list* (aref obj 0)
                              *new-iter*
                              (cdr xq))
                       (list* obj
                              0 (length obj)
                              ans)))
             ((ref-p obj)
              (go-iter (cdr xq)
                       (cons (gethash (ref-ix obj) reftbl)
                             ans)))
             ((user-ser-p obj)
              (go-iter (list* (user-ser-type obj)
                              (user-ser-data obj)
                              *new-user-ser*
                              (cdr xq))
                       ans))
             (t
              (go-iter (cdr xq)
                       (cons (loenc:after-restore obj)
                             ans)))
             ))
          )))))

(defun nr-reflate-tree (tree)
  ;; Non-destructive decoding
  (multiple-value-bind (new-tree reftbl)
      (copy-tree-reflating-vectors tree)
    (resolve-shared-node-references new-tree reftbl)
    ))

;; --------------------------------------------

#|
(nr-linearize-tree '(a b c d))
(nr-linearize-tree '(a b c . d))
(let ((sub '(a b c)))
  (nr-linearize-tree (list sub sub)))
(let ((lst  (list* 'a 'b 'c 'd)))
  (setf (cdr (last lst)) (cdr lst))
  (nr-linearize-tree lst))
                      
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
;; UUID's

(defmethod make-serializable ((obj uuid:uuid))
  (values :uuid
          (uuid:uuid-to-byte-array obj)))

(defmethod deserialize-type ((typ (eql :uuid)) dat)
  (uuid:byte-array-to-uuid dat))

;; --------------------------------------------
;; HASH-TABLES

(defmethod make-serializable ((ht hash-table))
  (values :hash-table
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

(defun encode (obj &rest loenc-args)
  (apply #'loenc:encode (nr-linearize-tree obj) loenc-args))

(defun decode (enc &rest loenc-args)
  (nr-reflate-tree (apply #'loenc:decode enc loenc-args)))


(defun serialize (obj stream &rest loenc-args)
  (apply #'loenc:serialize (nr-linearize-tree obj) stream loenc-args))
  
(defun deserialize (stream &rest loenc-args)
  (nr-reflate-tree (apply #'loenc:deserialize stream loenc-args)))

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
