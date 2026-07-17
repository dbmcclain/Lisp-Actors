;; rb-trees.lisp -- Immutable Functional Red-Black Trees
;; --------------------------------------------------------------------------------------
;; Binary tree storage for unique ordered items, plus Maps, FIFO Queues, and Stacks
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; DM/RAL 02/17 - carefully recrafted. Tests show that direct use of stack is much faster
;;                than using S(1) eval schemes, such as CPS style recoding or manual stacking
;;                of intermediate results.
;; DM/RAL  2026/07/14T13:07:56U
;;         Consolidate SETS/MAPS. No need for separate MAP-ENTRY data type.
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRAT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

;; ------------------------------------------------------------------------
(in-package #:com.ral.rb-trees.sets)
;; ------------------------------------------------------------------------
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0))
          (inline empty singleton create))

;; ----------------------------------------------------------------
;; Sets are represented by balanced binary trees
;; The heights of children differ by at most 2
;; Tree nodes are quadruples (l v r h) where:
;;   - l = left child
;;   - v = value
;;   - r = right child
;;   - h = node height, empty node has height 0
;; ----------------------------------------------------------------

(defun empty ()
  nil)

;; ------------------------------------------------------
;; helpful macros...

(um:eval-always
  (defun is-wild? (sym)
    (string= "_" (symbol-name sym)))
  (defun gen-ignore_ (syms)
    (um:when-let (sym (find-if #'is-wild? syms))
      `((declare (ignore ,sym)))
      )))

(defmacro with-node-bindings (lkvrh-syms node &body body)
  ;; Intended only to destructure tree nodes.  Every instance is
  ;; expected to have 5 symbols in lkvrh-syms pattern, corresponding to
  ;; (l k v r h) l = left, k = key, v = val, r = right, h = height.
  (let ((bindings (delete-if #'is-wild?
                             (um:zip lkvrh-syms '(node-l node-k node-v node-r node-h))
                             :key #'car)))
    `(with-accessors ,bindings ,node
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-node-bindings" 2)

(defmacro with-list-bindings (syms lst &body body)
  ;; intended for general top-level list destructuring
  ;; all symbols are optionally bound to list elements
  `(multiple-value-bind ,syms (values-list ,lst)
     ,@(gen-ignore_ syms)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-list-bindings" 2)

;; ------------------------------------------------------
;; private constructors... CREATE, BAL

;; create - create a tree node with left son l, key k, value v, and right son r.
;; We must have all elements of l < k < all elements of r.
;; l and r must be balanced and have a height difference <= 2
;; (intended for internal use only)

(defun create (l k v r &optional (hl (height l)) (hr (height r)))
  (declare (fixnum hl hr))
  (%create l k v r (1+ (max hl hr))))

;; bal - same as create, but performs one step of rebalancing if necessary
;; assumes l and r balanced and height difference <= 3
;; (intended for internal use only)

(defun bal (l k v r)
  (let ((hl (height l))
        (hr (height r)))
    (declare (fixnum hl hr))
    (cond ((> hl (the fixnum (+ 2 hr)))
           (with-node-bindings (ll lk lv lr) l
             (cond ((>= (the fixnum (height ll))
                        (the fixnum (height lr)))
                    (create ll lk lv (create lr k v r)))

                   (t  (with-node-bindings (lrl lrk lrv lrr) lr
                         (create (create ll lk lv lrl) lrk lrv (create lrr k v r))))
                   )))

          ((> hr (the fixnum (+ 2 hl)))
           (with-node-bindings (rl rk rv rr) r
             (cond ((>= (the fixnum (height rr))
                        (the fixnum (height rl)))
                    (create (create l k v rl) rk rv rr))

                   (t  (with-node-bindings (rll rlk rlv rlr) rl
                         (create (create l k v rll) rlk rlv (create rlr rk rv rr))))
                   )))

          (t  (create l k v r hl hr))
          )))

;; -----------------------------------------------------------
;; add - insertion of one element
#|
(unless (fboundp 'false)
  (defun false ()
    (constantly nil)))

(unless (fboundp 'true)
  (defun true ()
    (constantly t)))

(defvar *replace-p* #'false)

(defmacro with-replacement (&body body)
  `(let ((*replace-p* #'true))
     ,@body))

(defmacro without-replacement (&body body)
  `(let ((*replace-p* #'false))
     ,@body))

(defun ensure-function (f)
  (cond
   ((functionp f) f)
   ((and (symbolp f)
         (fboundp f)
         (not (macro-function f)))
    (symbol-function f))
   (t  (constantly f))
   ))

(defmacro with-replacement-p (tf &body body)
  `(let ((*replace-p* (ensure-function ,tf)))
     ,@body))
|#
;; --------------------------------------------

(defmethod add (tree key &optional val)
  (funcall tree :clone-with
           (funcall tree :add key val)))

(defun make-add-fn (compare-fn replace-fn)
  (labels
      ((add (tree key val)
         ;; execute with S(Log2(N))
         (if tree
             (with-node-bindings (l k v r h) tree
               (cond ((and (eql key k)
                           (eql val v))
                      tree)
                     (t  (let ((c (funcall compare-fn key k)))
                           (declare (real c))
                           (cond ((zerop c)
                                  ;; in zero case - to support maps (see below)
                                  ;; ensure that new map value is substituted for old
                                  ;; value - use x instead of v for value field of result.
                                  ;; This is still FP pure - it makes a new tree.
                                  (if (funcall replace-fn v val)
                                      (%create l key val r h)
                                    tree))
                                 
                                 ((minusp c)
                                  (multiple-value-bind (new-left needs-rebal) (add l key val)
                                    (cond ((eq l new-left)  tree)
                                          (needs-rebal      (values (bal new-left k v r) t))
                                          (t                (create new-left k v r))
                                          )))
                                 
                                 (t
                                  (multiple-value-bind (new-right needs-rebal) (add r key val)
                                    (cond ((eq r new-right)  tree)
                                          (needs-rebal       (values (bal l k v new-right) t))
                                          (t                 (create l k v new-right))
                                          )))
                                 )))
                     ))
           ;; else
           (values (singleton-node key val) t)
           )))
    #'add))

(define-modify-macro addf (&rest args)
  add)

;; -----------------------------------------------------------
;; join -- same as create and bal, but no assumptions are made on the
;; relative heights of l and r
;;
;; Internal use only. On entry it is known that l,v,r satisfy an oder
;; relation: max(l) < v < min(r).

(defun add-min-elt (s key val)
  (if s
      (with-node-bindings (l k v r) s
        (bal (add-min-elt l key val) k v r))
    ;; else
    (singleton-node key val)
    ))

(defun add-max-elt (s key val)
  (if s
      (with-node-bindings (l k v r) s
        (bal l k v (add-max-elt r key val)))
    ;; else
    (singleton-node key val)
    ))
  
(defun join (l key val r)
  ;; execute with S(Log2(N))
  (cond ((null l)
         (add-min-elt r key val))
        ((null r)
         (add-max-elt l key val))
        (t
         (with-node-bindings (ll lk lv lr lh) l
           (with-node-bindings (rl rk rv rr rh) r
             (cond  ((> lh (the fixnum (+ 2 rh)))
                     (bal ll lk lv (join lr key val r)))
                    
                    ((> rh (the fixnum (+ 2 lh)))
                     (bal (join l key val rl) rk rv rr))
                    
                    (t (create l key val r lh rh))
                    ))))
        ))

;; ------------------------------------------------------------------------

;; min-elt -- return the value of the smallest element of the set
;; i.e., the value from the leftmost node

(defmethod min-elt ((tree tree))
  ;; -> key, val, found-p
  ;; execute with S(1)
  (let ((nodes  (tree-nodes tree)))
    (and nodes
         (um:nlet iter ((nodes nodes))
           (with-node-bindings (l k v) nodes
             (if l
                 (go-iter l)
               (values k v t))
             ))
         )))

;; remove-min-elt - remove the smallest element of the set
;; i.e., remove the leftmost node

(defmethod remove-min-elt (nodes)
  ;; execute with S(1)
  ;; Return min-element, and rebalanced tree excluding that min-element
  (and nodes
       (um:nlet iter ((nodes nodes)
                      (acc   nil))
         (with-node-bindings (l k v r) nodes
           (if l
               (go-iter l (cons nodes acc))
             (um:nlet rebuild ((mink  k)
                               (minv  v)
                               (nodes r)
                               (acc   acc))
               (if acc
                   (with-node-bindings (_ k v r) (car acc)
                     (go-rebuild mink minv (bal nodes k v r) (cdr acc)))
                 (values mink minv nodes)))
             )))
       ))

(defmethod remove-min-elt ((tree tree))
  (multiple-value-bind (mink minv new-nodes)
      (remove-min-elt (tree-nodes tree))
    (values mink
            minv
            (funcall tree :clone-with new-nodes))
    ))

;; concat - merge two trees l and r into one.
;; All elements of l must precede the elements of r.
;; No assumptions on the heights of l and r.

(defun concat (t1 t2)
  (cond ((null t1) t2)
        ((null t2) t1)
        (t (multiple-value-call #'join t1 (remove-min-elt t2)))
        ))

;; ------------------------------------------------------------------------

(defmethod max-elt ((tree tree))
  ;; -> val, found-p
  ;; execute with S(1)
  (let ((nodes  (tree-nodes tree)))
    (and nodes
         (um:nlet iter ((nodes nodes))
           (with-node-bindings (_ k v r) nodes
             (if r
                 (go-iter r)
               (values k v t))
             ))
         )))

;; remove-max-elt -- remove the largest element of the set
;; also useful for priority-queues

(defmethod remove-max-elt ((tree tree))
  ;; execute with S(1)
  ;; Return max-element, and new balanced tree not containing that max-element.
  (let ((nodes (tree-nodes tree)))
    (and nodes
         (um:nlet iter ((nodes nodes)
                        (acc   nil))
           (with-node-bindings (l k v r) nodes
             (if r
                 (go-iter r (cons nodes acc))
               (um:nlet rebuild ((maxk  k)
                                 (maxv  v)
                                 (nodes l)
                                 (acc   acc))
                 (if acc
                     (with-node-bindings (l k v) (car acc)
                       (go-rebuild maxk maxv (bal l k v nodes) (cdr acc)))
                   (values maxk maxv (funcall tree :clone-with nodes) )))
               )))
         )))

;; --------------------------------------------
#|
(defparameter *s* nil)
(let* ((s  (make-tree)))
  (dotimes (ix 15)
    (addf s (random 100)))
  (setf *s* s)
  (inspect s))
(view-set *s*)
(inspect (ser:encode *s*))
(coerce (map 'vector #'code-char (ser:encode *s*)) 'string)
(inspect (ser:decode (ser:encode *s*)))

(min-elt *s*)
(max-elt *s*)
(inspect (multiple-value-list (remove-min-elt *s*)))
(inspect (multiple-value-list (remove-max-elt *s*)))
(mem *s* 32)
(inspect (split *s* 32))
(inspect (remove *s* 32))

(view-set (let ((xt (sets:empty))) (dotimes (ix 10) (setf xt (sets:add xt ix))) xt))
|#
;; ------------------------------------------------------------------------
;; split - (split s x) returns a triple of (values l present r)
;; where
;; - l is the set of elements of s that are < x
;; - r is the set of elements of s that are > x
;; - present is nil if s contains no element equal to x, or else
;;   present is the set whose top node contains an element equal to x

(defmethod split ((tree tree) key)
  (with-list-bindings (l present r)
      (funcall tree :split key)
    (list (funcall tree :clone-with l)
          present
          (funcall tree :clone-with r))
    ))

(defun make-split-fn (compare-fn)
  (labels
      ((split (tree key)
         ;; for internal use
         ;; execute with S(Log2(N))
         (if tree
             (with-node-bindings (l k v r) tree
               (let ((c (funcall compare-fn key k)))
                 (declare (real c))
                 (cond ((zerop c)  (list l tree r))
                       ((minusp c)
                        (with-list-bindings (ll pres rl) (split l key)
                          (list ll pres (join rl k v r))))
                       (t
                        (with-list-bindings (lr pres rr) (split r key)
                          (list (join l k v lr) pres rr)))
                       )))
           ;; else
           (list nil nil nil)
           )))
    #'split))

;; ------------------------------------------------------------------------

(defmethod mem ((tree tree) key)
  (funcall tree :mem key))

(defun make-mem-fn (compare-fn)
  (labels
      ((mem (tree key)
         ;; execute with S(1)
         (and tree
              (with-node-bindings (l k v r) tree
                (let ((c (funcall compare-fn key k)))
                  (declare (real c))
                  (if (zerop c)
                      (values t v)
                    (mem (if (minusp c) l r) key))
                  )))))
    #'mem))

;; ------------------------------------------------------------------------

(defmethod remove (tree key)
  (funcall tree :clone-with (funcall tree :remove key)))

(defun make-remove-fn (compare-fn)
  (labels
      ((merge-trees (t1 t2)
         ;; merge -- merge two trees l and r into one.
         ;; All elements of l must precede the elements of r
         ;; Assume height difference <= 2
         ;; (for internal use)
         (cond ((null t1) t2)
               ((null t2) t1)
               (t  (multiple-value-call #'bal t1 (remove-min-elt t2)))
               ))
       (remove (tree key)
         ;; execute with S(Log2(N))
         (when tree
           (with-node-bindings (l k v r) tree
             (let ((c (funcall compare-fn key k)))
               (declare (real c))
               (cond ((zerop c)  (merge-trees l r))
                     ((minusp c) (bal (remove l key) k v r))
                     (t          (bal l k v (remove r key)))
                     ))))))
    #'remove))


(define-modify-macro removef (key)
  remove)

;; ------------------------------------------------------------------------

(defun check-same-type (s1 s2)
  (assert (eq (tree-type s1)
              (tree-type s2))))

(defmethod union ((s1 tree) (s2 tree))
  (check-same-type s1 s2)
  (let ((split-fn  (funcall s1 :split-fn)))
    (labels
        ((split (s k)
           (funcall split-fn s k))
         (union (s1 s2)
           ;; execute with S(Log2(N))
           (cond ((null s1) s2)
                 ((null s2) s1)
                 (t  (with-node-bindings (l1 k1 v1 r1 h1) s1
                       (with-node-bindings (l2 k2 v2 r2 h2) s2
                         (cond ((>= h1 h2)
                                (cond ((= h2 1)  (add s1 k2 v2))
                                      (t  (with-list-bindings (l2 _ r2) (split s2 k1)
                                            (join (union l1 l2) k1 v1 (union r1 r2))))
                                      ))
                               
                               (t (cond ((= h1 1)  (add s2 k1 v1))
                                        (t  (with-list-bindings (l1 _ r1) (split s1 v2)
                                              (join (union l1 l2) k2 v2 (union r1 r2))))
                                        ))
                               ))))
                 )))
      (funcall s1 :clone-with (union (tree-nodes s1) (tree-nodes s2)))
      )))

;; ------------------------------------------------------------------------

(defmethod intersection ((s1 tree) (s2 tree))
  (check-same-type s1 s2)
  (let ((split-fn (funcall s1 :split-fn)))
    (labels
        ((split (s k)
           (funcall split-fn s k))
         (intersection (s1 s2)
           ;; execute with S(Log2(N))
           (cond ((null s1) (empty))
                 ((null s2) (empty))
                 (t  (with-node-bindings (l1 k1 v1 r1) s1
                       (with-list-bindings (l2 ans r2) (split s2 k1)
                         (let ((new-l (intersection l1 l2))
                               (new-r (intersection r1 r2)))
                           (cond (ans  (join new-l k1 v1 new-r))
                                 (t    (concat new-l new-r))
                                 )))))
                 )))
      (funcall s1 :clone-with (intersection (tree-nodes s1) (tree-nodes s2)))
      )))

;; ------------------------------------------------------------------------

(defmethod diff ((s1 tree) (s2 tree))
  (check-same-type s1 s2)
  (let ((split-fn  (funcall s1 :split-fn)))
    (labels
        ((split (s k)
           (funcall split-fn s k))
         (diff (s1 s2)
           ;; execute with S(Log2(N))
           (cond ((null s1) s1)
                 ((null s2) s1)
                 (t  (with-node-bindings (l1 k1 v1 r1) s1
                       (with-list-bindings (l2 ans r2) (split s2 k1)
                         (let ((new-l  (diff l1 l2))
                               (new-r  (diff r1 r2)))
                           (cond (ans  (concat new-l new-r))
                                 (t    (join new-l k1 v1 new-r))
                                 )))))
                 )))
      (funcall s1 :clone-with (diff (tree-nodes s1) (tree-nodes s2)))
      )))

;; ------------------------------------------------------------------------

(defmethod compare-trees ((s1 tree) (s2 tree))
  (check-same-type s1 s2)
  (funcall s1 :compare s2))

(defun make-compare-fn (compare-fn)
  (labels
      ((cons-enum (s e)
         (if s
             ;; proceeding down the left side from node s
             ;; form a telescoped list of node vals and right nodes
             ;;   -> (v1 r1 (v2 r2 (v3 r3 (... (vtop rtop e))) ...) (why? used internally)
             ;; where, v1   is the min element of set s,
             ;;        r1   is the right subnode of the min element node,
             ;;        v2   is the value from the parent node of the min element node,
             ;;        r2   is the right subnode of the parent node,
             ;;        ...
             ;;        vtop is the value in the top node,
             ;;        rtop is the right subnode of the top node, and
             ;;        e    is the starting accumulator list
             (with-node-bindings (l k _ r) s
               (cons-enum l (list k r e)))
           ;; else
           e))
       (compare-enums (e1 e2)
         (cond ((and (null e1)
                     (null e2))
                0)
               ((null e1) -1)
               ((null e2)  1)
               (t  (with-list-bindings (k1 r1 t1) e1
                     (declare (list r1 t1))
                     (with-list-bindings (k2 r2 t2) e2
                       (declare (list r2 t2))
                       (let ((c (funcall compare-fn k1 k2)))
                         (declare (real c))
                         (cond ((zerop c)  (compare-enums (cons-enum r1 t1)
                                                          (cons-enum r2 t2)))
                               (t  c)
                               )))))
               ))
       (compare-trees (s1 s2)
         ;; execute with S(Log2(N))
         (compare-enums (cons-enum s1 nil)
                        (cons-enum s2 nil))))
    #'compare-trees))

(defmethod ord:compare ((s1 tree) (s2 tree))
  ;; execute with S(Log2(N))
  (if (eq (tree-type s1)
          (tree-type s2))
      (compare-trees s1 s2)
    -1))

;; ------------------------------------------------------------------------

(defmethod subset ((s1 tree) (s2 tree))
  (check-same-type s1 s2)
  (funcall s1 :subset s2))

(defun make-subset-fn (compare-fn)
  (labels
      ((subset (s1 s2)
         ;; return true if s1 is subset of s2
         ;; execute with S(Log2(N))
         (cond ((null s1)  t)
               ((null s2)  nil)
               (t  (with-node-bindings (l1 k1 v1 r1) s1
                     (with-node-bindings (l2 k2 _ r2) s2
                       (let ((c (funcall compare-fn k1 k2)))
                         (declare (real c))
                         (cond ((zerop c)
                                (and (subset l1 l2)
                                     (subset r1 r2)))
                        
                               ((minusp c)
                                (and (subset (%create l1 k1 v1 nil 1)
                                             l2)
                                     (subset r1 s2)))
                        
                               (t (and (subset (%create nil k1 v1 r1 1)
                                               r2)
                                       (subset l1 s2)))
                               )))))
               )))
    #'subset))

;; --------------------------------------------------------

(defmethod iter ((s tree) fn)
  (labels
      ((iter (s fn)
         ;; perform fn on every set element in pre-order
         ;; execute with S(Log2(N))
         ;; speed 1.0
         ;; no meaningful result returned
         (and s
              (with-node-bindings (l k v r) s
                (iter l fn)
                (funcall fn k v)
                (iter r fn)))))
    (iter (tree-nodes s) fn)))

(defmethod fold ((s tree) fn accu)
  (labels
      ((fold (s fn accu)
         ;; accumulate fn applied to every set element in pre-order
         ;; execute with S(Log2(N))
         (if s
             (with-node-bindings (l k v r) s
               (fold r fn
                     (funcall fn k v (fold l fn accu))))
           ;; else
           accu)))
    (fold (tree-nodes s) fn accu)))

(defmethod every ((s tree) pred)
  (labels
      ((every (s pred)
         ;; return true if every set element satisfies pred
         ;; execute with S(Log2(N))
         (or (null s)
             (with-node-bindings (l k v r) s
               (and (funcall pred k v)
                    (every l pred)
                    (every r pred)))
             )))
    (every (tree-nodes s) pred)))


(defmethod some ((s tree) pred)
  (labels
      ((some (s pred)
         ;; return true of some element of s satisfies pred
         ;; Execute with S(Log2(N))
         (when s
           (with-node-bindings (l k v r) s
             (or (funcall pred k v)
                 (some l pred)
                 (some r pred)))
           )))
    (some (tree-nodes s) pred)
    ))
  
(defmethod filter ((s tree) pred)
  (labels
      ((filter-aux (s pred accu)
         ;; return subset consisting of element that satisfy pred.
         ;; Execute with S(Log2(N))
         (if s
             (with-node-bindings (l k v r) s
               (let ((new-accu (filter-aux l pred accu)))
                 (filter-aux r pred (if (funcall pred k v)
                                        (add new-accu k v)
                                      new-accu))))
           ;; else
           accu)))
    (funcall s :clone-with (filter-aux (tree-nodes s) pred (empty)))
    ))

(defmethod partition ((s tree) pred)
  (labels
      ((partition-aux (s pred pair)
         ;; partition set into two subsets (true-element, false-elements)
         ;; according to pred. Execute with S(Log2(N))
         (if s
             (with-node-bindings (l k v r) s
               (with-list-bindings (tp fp) (partition-aux l pred pair)
                 (partition-aux r pred (if (funcall pred k v)
                                           (list (add tp k v) fp)
                                         (list tp (add fp k v))))
                 ))
           ;; else
           pair)))
    (with-list-bindings (tp fp)
        (partition-aux (tree-nodes s) pred (list (empty) (empty)))
      (list (funcall s :clone-with tp)
            (funcall s :clone-with fp))
      )))

(defmethod cardinal ((s tree))
  (labels
      ((cardinal (s)
         ;; count elements in s (pre-order, FWIW), using S(Log2(N))
         (if s
             (with-node-bindings (l _ _ r) s
               (+ (cardinal l) 1 (cardinal r)))
           ;; else
           0)))
    (cardinal (tree-nodes s))
    ))

(defmethod elements ((s tree))
  (labels
      ((elements-aux (s accu)
         ;; list elements of set in pre-order, using S(1)
         (if s
             (with-node-bindings (l k v r) s
               (elements-aux l (acons k v (elements-aux r accu))))
           ;; else
           accu)))
    (elements-aux (tree-nodes s) nil)
    ))

(defmethod choose ((s tree))
  (min-elt s))

;; --------------------------------------------
;; -------------------------------------------------------------

#|
(defun make-tree (&optional (tree (sets:empty)))
  (if (= (sets:height tree) 10)
      tree
    (make-tree (sets:add tree (random 16384)))))

#+:LISPWORKS
(capi:contain
 (make-instance 'capi:graph-pane
                :roots (list xtt)

                :children-function (lambda (tree)
                                     (cond ((and (null (first tree))
                                                 (null (third tree)))
                                            nil)
                                           ((null (first tree))
                                            (list (list nil #\x nil) (third tree)))

                                           ((null (third tree))
                                            (list (first tree) (list nil #\x nil)))

                                           (t (list (first tree)
                                                    (third tree)))
                                           ))

                :print-function (lambda (node)
                                  (format nil "~A" (second node)))
                ))
|#

#+:LISPWORKS
(progn

  (defun set-children (tree layout)
    (when (node-p tree)
      (with-node-bindings (l _ _ r) tree
        (let ((lx (or l
                      (vector)))
              (rx (or r
                      (vector))))
          (cond ((and (null l)
                      (null r))
                 nil)
                (t
                 (case layout
                   ((:left-right :right-left) (list rx lx))
                   (t  (list lx rx))))
                )))))
  
  (defun print-node (tree keyfn)
    (if (node-p tree)
        (with-node-bindings (_ k) tree
          (with-standard-io-syntax
            (prin1-to-string (funcall keyfn k))
            ))
      ;; else
      ""))
  
  (defmethod key-fn (item)
    item)
  
  (defmethod view-set ((s tree) &key (key #'key-fn) (layout :left-right) &allow-other-keys)
    (capi:contain
     (make-instance 'capi:graph-pane
                    :layout-function   layout
                    :roots             (list (tree-nodes s))
                    :children-function #'(lambda (node)
                                           (set-children node layout))
                    :print-function    #'(lambda (node)
                                           (print-node node key))
                    :action-callback   #'(lambda (item intf)
                                           (declare (ignore intf))
                                           (inspect item))
                    ))) )

#|
;; examine effects of constructing a tree in pure ascending or descending order
(inspect (let ((xt (make-tree)))  (dotimes (ix 100) (setf xt (sets:add xt ix))) xt))
(view-set (let ((xt (make-tree))) (dotimes (ix 10) (setf xt (sets:add xt ix))) xt))
(view-set (let ((xt (make-tree))) (dotimes (ix 10) (setf xt (sets:add xt (- 99 ix) ))) xt))
|#
;; -------------------------------------------------------------
#|
(defparameter *tst-coll*
  (let ((arr (make-array 1_000_000
                         :element-type 'single-float)))
    (dotimes (ix (length arr))
      (setf (aref arr ix) (random 1f0)))
    arr))

(defun tst ()
  (let ((x (make-tree)))
    (dotimes (ix (length *tst-coll*))
      (addf x (aref *tst-coll* ix)))
    (cardinal x)))
(time (tst))

(defun tsth ()
  (let ((tbl (make-hash-table)))
    (dotimes (ix (length *tst-coll*))
      (setf (gethash (aref *tst-coll* ix) tbl) t))
    tbl))
(time (tsth))

;; Whoa!! Hashtable is 15x faster than RB-Trees, uses 20x less alloc,
;; and 10x less page faults
;;
;; Timing with (:TYPE VECTOR) NODE Struct:
;;               μs/add       Alloc bytes    Page Faults
;;               ------       -----------    -----------
;;    RB-Trees:  2.8              920           0.048
;; Hash-tables:  0.19              52           0.005
;;
;; Take away the (:TYPE VECTOR) in the NODE Struct:
;;    RB-Trees:  2.2             1075           0.031
;; Hash-tables:  0.13              53           0.004
|#

