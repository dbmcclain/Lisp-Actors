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

(defstruct tree-funcs
  (compare-fn    nil :read-only t)
  (replace-if-fn nil :read-only t)
  (add           nil :read-only t)
  (split         nil :read-only t)
  (mem           nil :read-only t)
  (remove        nil :read-only t)
  (compare       nil :read-only t)
  (subset        nil :read-only t))

(defstruct (tree
            (:constructor %make-tree (type)))
  (type  nil :read-only t)
  (nodes nil :read-only t))

(defun invoked (fn-name tree)
  (slot-value (tree-type tree) fn-name))

(defun invoke (fn-name tree &rest args)
  (apply (invoked fn-name tree) (tree-nodes tree) args))
  
(defstruct (node (:constructor singleton-node (k &optional v))
                 (:constructor %create (l k v r h)))
  (l nil :read-only t)
  (k nil :read-only t)
  (v nil :read-only t)
  (r nil :read-only t)
  (h 1   :read-only t))

(defgeneric height (tree)
  (:method ((tree tree))
   (height (tree-nodes tree)))
  (:method ((x null))
   0)
  (:method ((x node))
   (node-h x)))

(defgeneric is-empty (tree)
  (:method ((tree tree))
   (is-empty (tree-nodes tree)))
  (:method ((tree null))
   t)
  (:method ((tree node))
   nil))
