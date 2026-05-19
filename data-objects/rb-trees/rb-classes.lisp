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
;;   - v = value
;;   - r = right child
;;   - h = node height, empty node has height 0
;; ----------------------------------------------------------------

;; ------------------------------------------------------
;; Types & Public seed constructors - EMPTY, SINGLETON

(defclass tree ()
  ())

;; -------------------------------

(defclass empty (tree)
  ()
  (:metaclass um:singleton-class))

(defmethod is-empty ((tree empty))
  t)

(defmethod height ((tree empty))
  0)

;; -------------------------------

(defclass node (tree)
  ((l  :reader node-l  :initarg :l  :type tree)
   (v  :reader node-v  :initarg :v)
   (r  :reader node-r  :initarg :r  :type tree)
   (h  :reader node-h
       :reader height  :initarg :h  :type fixnum))
  (:default-initargs
   :l +empty+
   :r +empty+
   :h 1))

(defmethod is-empty ((tree node))
  nil)

