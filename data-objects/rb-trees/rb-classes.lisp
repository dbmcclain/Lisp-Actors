;; rb-classes.lisp
;;
;; DM/RAL  2026/05/18 17:39:25 UTC
;; ----------------------------------

(in-package #:com.ral.rb-trees.sets)

;; ----------------------------------

;; ----------------------------------------------------------------
;; Red-Black Trees -
;; The heights of children differ by at most 2
;; Tree nodes are quadruples (l v r h) where:
;;   - l = left child
;;   - k = key
;;   - v = value
;;   - r = right child
;;   - h = node height, empty node has height 0
;; ----------------------------------------------------------------

;; ------------------------------------------------------
;; Type Tree = Empty | Node(l:Tree,k:T,v:T,r:Tree,h:Fixnum)
;; --------------------------------------------

(defclass tree ()
  ()
  (:metaclass
   #+:LISPWORKS clos:funcallable-standard-class
   #+:SBCL      sb-mop:funcallable-standard-class))

(defun tree-nodes (tree)
  (funcall tree :nodes))

(defun tree-type (tree)
  (funcall tree :type))

(defstruct (node (:type vector)
                 (:constructor singleton-node (k &optional v))
                 (:constructor %create (l k v r h)))
  (l nil :read-only t)
  (k nil :read-only t)
  (v nil :read-only t)
  (r nil :read-only t)
  (h 1   :read-only t))

(defun node-p (x)
  (and (vectorp x)
       (= 5 (length x))))

(defgeneric height (tree)
  (:method ((tree tree))
   (height (tree-nodes tree)))
  (:method ((x null))
   0)
  (:method ((x vector))
   (node-h x)))

(defgeneric is-empty (tree)
  (:method ((tree tree))
   (is-empty (tree-nodes tree)))
  (:method ((tree null))
   t)
  (:method ((tree vector))
   nil))
