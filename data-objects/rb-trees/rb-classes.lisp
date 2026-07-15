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

(defstruct (node ;; (:type vector)
                 (:constructor singleton-node (k v))
                 (:constructor %create (l k v r h)))
  l k v r (h 1))


(defmethod height (tree)
  (if tree
      (node-h tree)
    0))

(defmethod is-empty (tree)
  (if tree
      nil
    t))

(when (vectorp (singleton-node nil nil))
  (unless (fboundp 'node-p)
    (let ((nel  (length (singleton-node nil nil))))
      (defun node-p (tree)
        (and (vectorp tree)
             (= nel (length tree))))
      )))

           