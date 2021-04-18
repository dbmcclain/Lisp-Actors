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
(in-package :sets)
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

;; ------------------------------------------------------
;; Types & Public seed constructors - EMPTY, SINGLETON

(defclass tree ()
  ())

(defmethod ref:clone ((tree tree))
  tree) ;; always immutable

;; -------------------------------

(defclass empty (tree)
  ((is-empty :reader is-empty :allocation :class :initform t)
   (height   :reader height   :allocation :class :initform 0)))

(defconstant +empty+
  (make-instance 'empty))

(defun empty ()
  +empty+)

(declaim (inline empty))

;; -------------------------------

(defclass node (tree)
  ((l  :reader node-l  :initarg :l  :type tree)
   (v  :reader node-v  :initarg :v)
   (r  :reader node-r  :initarg :r  :type tree)
   (h  :reader node-h
       :reader height  :initarg :h  :type fixnum)
   (is-empty :reader is-empty :allocation :class :initform nil)
   )
  (:default-initargs
   :l +empty+
   :r +empty+
   :h 1))

(defun singleton (x)
  (make-instance 'node
   :v  x))

;; ------------------------------------------------------
;; helpful macros...

(um:eval-always
  (defun is-wild? (sym)
    (string= "_" (symbol-name sym)))
  (defun gen-ignore_ (syms)
    (um:when-let (sym (find-if #'is-wild? syms))
      `((declare (ignore ,sym)))
      )))

(defmacro with-node-bindings (lvrh-syms node &body body)
  ;; Intended only to destructure tree nodes.  Every instance is
  ;; expected to have 4 symbols in lvrh-syms pattern, corresponding to
  ;; (l v r h) l = left, v = val, r = right, h = height.
  (let ((bindings (delete-if #'is-wild?
                             (um:zip lvrh-syms '(node-l node-v node-r node-h))
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

;; create - create a tree node with left son l, value v, and right son r.
;; We must have all elements of l < v < all elements of r.
;; l and r must be balanced and have a height difference <= 2
;; (intended for internal use only)

(defun create (l v r &optional (hl (height l)) (hr (height r)))
  (declare (fixnum hl hr))
  (make-instance 'node
   :l  l  :v  v  :r  r
   :h  (the fixnum (1+ (max hl hr)))))

;; bal - same as create, but performs one step of rebalancing if necessary
;; assumes l and r balanced and height difference <= 3
;; (intended for internal use only)

(defun bal (l v r)
  (let ((hl (height l))
        (hr (height r)))
    (declare (fixnum hl hr))
    (cond ((> hl (the fixnum (+ 2 hr)))
           (with-node-bindings (ll lv lr) l
             (cond ((>= (the fixnum (height ll))
                        (the fixnum (height lr)))
                    (create ll lv (create lr v r)))

                   (t  (with-node-bindings (lrl lrv lrr) lr
                         (create (create ll lv lrl) lrv (create lrr v r))))
                   )))

          ((> hr (the fixnum (+ 2 hl)))
           (with-node-bindings (rl rv rr) r
             (cond ((>= (the fixnum (height rr))
                        (the fixnum (height rl)))
                    (create (create l v rl) rv rr))

                   (t  (with-node-bindings (rll rlv rlr) rl
                         (create (create l v rll) rlv (create rlr rv rr))))
                   )))

          (t  (create l v r hl hr))
          )))

;; -----------------------------------------------------------
;; add - insertion of one element

(defvar *replace-p* #'lw:false)

(defmacro with-replacement (&body body)
  `(let ((*replace-p* #'lw:true))
     ,@body))

(defmacro without-replacement (&body body)
  `(let ((*replace-p* #'lw:false))
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

(defmethod add ((tree empty) x)
  (values (singleton x) t))

(defmethod add ((tree node) x)
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r h) tree
    (cond ((eq x v)  tree)
          (t  (let ((c (ord:compare x v)))
                (declare (real c))
                (cond ((zerop c)
                      ;; in zero case - to support maps (see below)
                      ;; ensure that new map value is substituted for old
                      ;; value - use x instead of v for value field of result.
                      (if (funcall *replace-p* v x)
                          (make-instance 'node
                                         :l l :v x :r r :h h)
                        tree))

                      ((minusp c)
                       (multiple-value-bind (new-left needs-rebal) (add l x)
                         (cond ((eq l new-left)  tree)
                               (needs-rebal      (values (bal new-left v r) t))
                               (t                (create new-left v r))
                               )))

                      (t
                       (multiple-value-bind (new-right needs-rebal) (add r x)
                         (cond ((eq r new-right)  tree)
                               (needs-rebal       (values (bal l v new-right) t))
                               (t                 (create l v new-right))
                               )))
                      )))
          )))

(define-modify-macro addf (key)
  add)

;; -----------------------------------------------------------
;; join -- same as create and bal, but no assumptions are made on the
;; relative heights of l and r

(defmethod join ((l empty) v r)
  (add-min-elt r v))

(defmethod join (l v (r empty))
  (add-max-elt l v))

(defmethod add-min-elt ((s empty) v)
  (singleton v))

(defmethod add-min-elt ((s node) v)
  (with-node-bindings (l x r) s
    (bal (add-min-elt l v) x r)))

(defmethod add-max-elt ((s empty) v)
  (singleton v))

(defmethod add-max-elt ((s node) v)
  (with-node-bindings (l x r) s
    (bal l x (add-max-elt r v))))

(defmethod join ((l node) v (r node))
  ;; execute with S(Log2(N))
  (with-node-bindings (ll lv lr lh) l
    (with-node-bindings (rl rv rr rh) r
      (cond  ((> lh (the fixnum (+ 2 rh)))
              (bal ll lv (join lr v r)))

             ((> rh (the fixnum (+ 2 lh)))
              (bal (join l v rl) rv rr))

             (t (create l v r lh rh))
             ))))

;; ------------------------------------------------------------------------

;; min-elt -- return the value of the smallest element of the set
;; i.e., the value from the leftmost node

(defmethod min-elt ((tree empty))
  (values))

(defmethod min-elt ((tree node))
  ;; -> val, found-p
  ;; execute with S(1)
  (with-node-bindings (l v) tree
    (cond ((is-empty l)  (values v t))
          (t             (min-elt l))
          )))

;; remove-min-elt - remove the smallest element of the set
;; i.e., remove the leftmost node

(defmethod remove-min-elt ((tree empty))
  tree)

(defmethod remove-min-elt ((tree node))
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r) tree
    (cond ((is-empty l)  r)
          (t             (bal (remove-min-elt l) v r))
          )))

;; concat - merge two trees l and r into one.
;; All elements of l must precede the elements of r.
;; No assumptions on the heights of l and r.

(defmethod concat ((t1 empty) t2)
  t2)

(defmethod concat (t1 (t2 empty))
  t1)

(defmethod concat ((t1 node) (t2 node))
  (join t1 (min-elt t2) (remove-min-elt t2)))

;; ------------------------------------------------------------------------

(defmethod max-elt ((tree empty))
  (values))

(defmethod max-elt ((tree node))
  ;; -> val, found-p
  ;; execute with S(1)
  (with-node-bindings (_ v r) tree
    (cond ((is-empty r)  (values v t))
          (t             (max-elt r))
          )))

;; remove-max-elt -- remove the largest element of the set
;; also useful for priority-queues

(defmethod remove-max-elt ((tree empty))
  tree)

(defmethod remove-max-elt ((tree node))
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r) tree
    (cond ((is-empty r)  l)
          (t             (bal l v (remove-max-elt r)))
          )))

;; ------------------------------------------------------------------------
;; split - (split s x) returns a triple of (values l present r)
;; where
;; - l is the set of elements of s that are < x
;; - r is the set of elements of s that are > x
;; - present is nil if s contains no element equal to x, or else
;;   present is the set whose top node contains an element equal to x

(defmethod split ((tree empty) x)
  (list (empty) nil (empty)))

(defmethod split ((tree node) x)
  ;; for internal use
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r) tree
    (let ((c (ord:compare x v)))
      (declare (real c))
      (cond ((zerop c)  (list l tree r))
            ((minusp c)
             (with-list-bindings (ll pres rl) (split l x)
               (list ll pres (join rl v r))))
            (t
             (with-list-bindings (lr pres rr) (split r x)
               (list (join l v lr) pres rr)))
            ))))

;; ------------------------------------------------------------------------

(defmethod mem ((tree empty) x)
  nil)

(defmethod mem ((tree node) x)
  ;; execute with S(1)
  (with-node-bindings (l v r) tree
    (let ((c (ord:compare x v)))
      (declare (real c))
      (if (zerop c)
          (values t v)
        (mem (if (minusp c) l r) x))
      )))

;; ------------------------------------------------------------------------

(defmethod merge-trees ((t1 empty) t2)
  t2)

(defmethod merge-trees (t1 (t2 empty))
  t1)

(defmethod merge-trees ((t1 node) (t2 node))
  ;; merge -- merge two trees l and r into one.
  ;; All elements of l must precede the elements of r
  ;; Assume height difference <= 2
  ;; (for internal use)
  (bal t1 (min-elt t2) (remove-min-elt t2)))


(defmethod remove ((tree empty) x)
  tree)

(defmethod remove ((tree node) x)
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r) tree
    (let ((c (ord:compare x v)))
      (declare (real c))
      (cond ((zerop c)  (merge-trees l r))
            ((minusp c) (bal (remove l x) v r))
            (t          (bal l v (remove r x)))
            ))))

(define-modify-macro removef (key)
  remove)

;; ------------------------------------------------------------------------

(defmethod union ((s1 empty) s2)
  s2)

(defmethod union (s1 (s2 empty))
  s1)

(defmethod union ((s1 node) (s2 node))
  ;; execute with S(Log2(N))
  (with-node-bindings (l1 v1 r1 h1) s1
    (with-node-bindings (l2 v2 r2 h2) s2
      (cond ((>= h1 h2)
             (cond ((= h2 1)  (add s1 v2))
                   (t  (with-list-bindings (l2 _ r2) (split s2 v1)
                         (join (union l1 l2) v1 (union r1 r2))))
                   ))

            (t (cond ((= h1 1)  (add s2 v1))
                     (t  (with-list-bindings (l1 _ r1) (split s1 v2)
                           (join (union l1 l2) v2 (union r1 r2))))
                     ))
            ))))

;; ------------------------------------------------------------------------

(defmethod intersection ((s1 empty) s2)
  (empty))

(defmethod intersection (s1 (s2 empty))
  (empty))

(defmethod intersection ((s1 node) (s2 node))
  ;; execute with S(Log2(N))
  (with-node-bindings (l1 v1 r1) s1
    (with-list-bindings (l2 ans r2) (split s2 v1)
      (let ((new-l (intersection l1 l2))
            (new-r (intersection r1 r2)))
        (cond (ans  (join new-l v1 new-r))
              (t    (concat new-l new-r))
              )))))

;; ------------------------------------------------------------------------

(defmethod diff ((s1 empty) s2)
  s1)

(defmethod diff (s1 (s2 empty))
  s1)

(defmethod diff ((s1 node) (s2 node))
  ;; execute with S(Log2(N))
  (with-node-bindings (l1 v1 r1) s1
    (with-list-bindings (l2 ans r2) (split s2 v1)
      (let ((new-l  (diff l1 l2))
            (new-r  (diff r1 r2)))
        (cond (ans  (concat new-l new-r))
              (t    (join new-l v1 new-r))
              )))))

;; ------------------------------------------------------------------------

(defmethod cons-enum ((s empty) e)
  e)

(defmethod cons-enum ((s node) e)
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
  (with-node-bindings (l v r) s
    (cons-enum l (list v r e))))

(defmethod compare-enums ((e1 null) (e2 null))
  0)

(defmethod compare-enums ((e1 null) e2)
  -1)

(defmethod compare-enums (e1 (e2 null))
  1)

(defmethod compare-enums ((e1 cons) (e2 cons))
  (with-list-bindings (v1 r1 t1) e1
    (declare (list r1 t1))
    (with-list-bindings (v2 r2 t2) e2
      (declare (list r2 t2))
      (let ((c (ord:compare v1 v2)))
        (declare (real c))
        (cond ((zerop c)  (compare-enums (cons-enum r1 t1)
                                         (cons-enum r2 t2)))
              (t  c)
              )))))

(defmethod ord:compare ((s1 tree) (s2 tree))
  ;; execute with S(Log2(N))
  (compare-enums (cons-enum s1 nil)
                 (cons-enum s2 nil)))

;; ------------------------------------------------------------------------

(defmethod subset ((s1 empty) s2)
  t)

(defmethod subset (s1 (s2 empty))
  nil)

(defmethod subset ((s1 node) (s2 node))
  ;; return true if s1 is subset of s2
  ;; execute with S(Log2(N))
  (with-node-bindings (l1 v1 r1) s1
    (with-node-bindings (l2 v2 r2) s2
      (let ((c (ord:compare v1 v2)))
        (declare (real c))
        (cond ((zerop c)
               (and (subset l1 l2)
                    (subset r1 r2)))

              ((minusp c)
               (and (subset (make-instance 'node
                                 :l l1  :v v1)
                                l2)
                    (subset r1 s2)))

              (t (and (subset (make-instance 'node
                                   :v v1  :r r1)
                                  r2)
                      (subset l1 s2)))
              )))))

;; --------------------------------------------------------

(defmethod iter ((s empty) fn)
  (values))

(defmethod iter ((s node) fn)
  ;; perform fn on every set element in pre-order
  ;; execute with S(Log2(N))
  ;; speed 1.0
  (with-node-bindings (l v r) s
    (iter l fn)
    (funcall fn v)
    (iter r fn)))

(defmethod fold ((s empty) fn accu)
  accu)

(defmethod fold ((s node) fn accu)
  ;; accumulate fn applied to every set element in pre-order
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r) s
    (fold r fn
          (funcall fn v (fold l fn accu)))))

(defmethod every ((s empty) pred)
  t)

(defmethod every ((s node) pred)
  ;; return true if every set element satisfies pred
  ;; execute with S(Log2(N))
  (with-node-bindings (l v r) s
    (and (funcall pred v)
         (every l pred)
         (every r pred))))


(defmethod some ((s empty) pred)
  nil)

(defmethod some ((s node) pred)
  ;; return true of some element of s satisfies pred
  ;; Execute with S(Log2(N))
  (with-node-bindings (l v r) s
    (or (funcall pred v)
        (some l pred)
        (some r pred))))

(defmethod filter ((s tree) pred)
  (filter-aux s pred (empty)))

(defmethod filter-aux ((s empty) pred accu)
  accu)

(defmethod filter-aux ((s node) pred accu)
  ;; return subset consisting of element that satisfy pred.
  ;; Execute with S(Log2(N))
  (with-node-bindings (l v r) s
    (let ((new-accu (filter-aux l pred accu)))
      (filter-aux r pred (if (funcall pred v)
                             (add new-accu v)
                           new-accu)))))

(defmethod partition ((s tree) pred)
  (partition-aux s pred (list (empty) (empty))))

(defmethod partition-aux ((s empty) pred pair)
  pair)

(defmethod parition-aux ((s node) pred pair)
  ;; partition set into two subsets (true-element, false-elements)
  ;; according to pred. Execute with S(Log2(N))
  (with-node-bindings (l v r) s
    (with-list-bindings (tp fp) (partition-aux l pred pair)
      (partition-aux r pred (if (funcall pred v)
                                (list (add tp v) fp)
                              (list tp (add fp v))))
      )))

(defmethod cardinal ((s empty))
  0)

(defmethod cardinal ((s node))
  ;; count elements in s (pre-order, FWIW), using S(Log2(N))
  (with-node-bindings (l _ r) s
    (+ (cardinal l) 1 (cardinal r))))

(defmethod elements ((s tree))
  (elements-aux s nil))

(defmethod elements-aux ((s empty) accu)
  accu)

(defmethod elements-aux ((s node) accu)
  ;; list elements of set in pre-order, using S(1)
  (with-node-bindings (l v r) s
    (elements-aux l (cons v (elements-aux r accu)))))


(defmethod choose ((s tree))
  (min-elt s))

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

(defmethod set-children (tree layout)
  nil)

(defmethod set-children ((tree node) layout)
  (with-node-bindings (l _ r) tree
    (let ((lx (if (is-empty l)
                  (vector)
                l))
          (rx (if (is-empty r)
                  (vector)
                r)))
      (cond ((and (is-empty l)
                  (is-empty r))
             nil)
            (t
             (case layout
               ((:left-right :right-left) (list rx lx))
               (t  (list lx rx))))
            ))))

(defmethod print-node (x keyfn)
  nil)

(defmethod print-node ((tree node) keyfn)
  (with-node-bindings (_ v) tree
    (with-standard-io-syntax
      (prin1-to-string (funcall keyfn v))
      )))

(defmethod key-fn (item)
  item)

#+:LISPWORKS
(defmethod view-set ((s tree) &key (key #'key-fn) (layout :left-right) &allow-other-keys)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function   layout
                  :roots             (list s)
                  :children-function #'(lambda (node)
                                         (set-children node layout))
                  :print-function    #'(lambda (node)
                                         (print-node node key))
                  :action-callback   #'(lambda (item intf)
                                         (declare (ignore intf))
                                         (inspect item))
                  )))

#|
;; examine effects of constructing a tree in pure ascending or descending order
(inspect (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add xt ix))) xt))
(view-set (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add xt ix))) xt))
(view-set (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add xt (- 99 ix) ))) xt))
|#
;; -------------------------------------------------------------
#|
(defun tst (&optional (n 1000000))
  (let ((x (empty)))
    (loop repeat n do
          (setf x (add x (random 1d0))))
    (cardinal x)))
(time (tst 1000000))

(defun tsth (&optional (n 1000000))
  (let ((x (make-hash-table)))
    (loop repeat n do
          (let ((v (random 1d0)))
            (setf (gethash v x) v)))
    ))
(time (tsth 1000000))
|#

;; --------------------------------------------------
;; Unshared variant

(um:make-encapsulated-type UE UE? UD)

(defun make-unshared-set ()
  (UE (empty)))

(defmethod copy ((set UE))
  (UE (UD set)))

(defmethod add ((set UE) x)
  (setf (UD set) (add (UD set) x)))

(defmethod min-elt ((set UE))
  (min-elt (UD set)))

(defmethod max-elt ((set UE))
  (max-elt (UD set)))

(defmethod mem ((set UE) x)
  (mem (UD set) x))

(defmethod remove ((set UE) x)
  (setf (UD set) (remove (UD set) x)))

(defmethod union ((set1 UE) (set2 UE))
  (UE (union (UD set1) (UD set2))))

(defmethod intersection ((set1 UE) (set2 UE))
  (UE (intersection (UD set1) (UD set2))))

(defmethod diff ((set1 UE) (set2 UE))
  (UE (diff (UD set1) (UD set2))))

(defmethod ord:compare ((set1 UE) (set2 UE))
  (ord:compare (UD set1) (UD set2)))

(defmethod subset ((set1 UE) (set2 UE))
  (subset (UD set1) (UD set2)))

(defmethod iter ((set UE) fn)
  (iter (UD set) fn))

(defmethod fold ((set UE) fn accu)
  (fold (UD set) fn accu))

(defmethod every ((set UE) pred)
  (every (UD set) pred))

(defmethod some ((set UE) pred)
  (some (UD set) pred))

(defmethod filter ((set UE) pred)
  (UE (filter (UD set) pred)))

(defmethod partition ((set UE) pred)
  (destructuring-bind (tp fp)
      (partition (UD set) pred)
    (list (UE tp) (UE fp))
    ))

(defmethod cardinal ((set UE))
  (cardinal (UD set)))

(defmethod elements ((set UE))
  (elements (UD set)))

(defmethod choose ((set UE))
  (choose (UD set)))

(defmethod view-set ((set UE) &rest args &key &allow-other-keys)
  (apply #'view-set (UD set) args))

;; --------------------------------------------------
;; Shared variant - lock free

(um:make-encapsulated-type SE SE? SD)

(defun make-shared-set ()
  (SE (empty)))

(defun rd-set (set)
  (um:rd (SD set)))

(defmacro with-set ((s set) &body body)
  `(let ((,s (rd-set ,set)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-set" 1)

(defun %rmw-set (set fn)
  (um:rmw (SD set) fn))

(defmacro rmw-set ((s set) &body body)
  `(%rmw-set ,set (lambda (,s)
                    ,@body)))

#+:LISPWORKS
(editor:setup-indent "rmw-set" 1)

(defmethod copy ((set SE))
  (with-set (s set)
    (SE s)))

(defmethod add ((set SE) x)
  (rmw-set (s set)
    (add s x)))

(defmethod min-elt ((set SE))
  (with-set (s set)
    (min-elt s)))

(defmethod max-elt ((set SE))
  (with-set (s set)
    (max-elt s)))

(defmethod mem ((set SE) x)
  (with-set (s set)
    (mem s x)))

(defmethod remove ((set SE) x)
  (rmw-set (s set)
    (remove s x)))

(defmethod union ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (SE (union s1 s2)))))

(defmethod intersection ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (SE (intersection s1 s2)))))

(defmethod diff ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (SE (diff s1 s2)))))

(defmethod ord:compare ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (ord:compare s1 s2))))

(defmethod subset ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (subset s1 s2))))

(defmethod iter ((set SE) fn)
  (with-set (s set)
    (iter s fn)))

(defmethod fold ((set SE) fn accu)
  (with-set (s set)
    (fold s fn accu)))

(defmethod every ((set SE) pred)
  (with-set (s set)
    (every s pred)))

(defmethod some ((set SE) pred)
  (with-set (s set)
    (some s pred)))

(defmethod filter ((set SE) pred)
  (with-set (s set)
    (SE (filter s pred))))

(defmethod partition ((set SE) pred)
  (with-set (s set)
    (destructuring-bind (tp fp)
        (partition s pred)
      (list (SE tp) (SE fp))
      )))

(defmethod cardinal ((set SE))
  (with-set (s set)
    (cardinal s)))

(defmethod elements ((set SE))
  (with-set (s set)
    (elements s)))

(defmethod choose ((set SE))
  (with-set (s set)
    (choose s)))

(defmethod view-set ((set SE) &rest args &key &allow-other-keys)
  (with-set (s set)
    (apply #'view-set s args)))

(defmethod copy-as-shared ((set SE))
  (copy set))

(defmethod copy-as-shared ((set UE))
  (SE (UD set)))

(defmethod copy-as-unshared ((set SE))
  (with-set (s set)
    (UE s)))

(defmethod copy-as-unshared ((set UE))
  (copy set))

(defmethod erase ((set SE))
  (rmw-set (s set)
    (declare (ignore s))
    (empty)))

(defmethod erase ((set UE))
  (setf (UD set) (empty)))


