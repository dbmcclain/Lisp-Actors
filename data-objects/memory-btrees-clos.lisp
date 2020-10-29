;; memory-btrees.lisp -- In-memory B-Trees
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/RAL  08/08
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
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :memory-btrees)

;; -------------------------------------------
;; -------------------------------------------
;; Node class for in-memory B-Trees
;;
;; Nodes must have a capacity that is an odd integer. And they must be prepared
;; to hold that many elements, plus 2 additional cells to handle the intermediate needs
;; of add/update-item.

(defclass node (btree:node)
  ((height            :reader   btree:node-height
                      :initarg  :height
                      :initform 1)
   (fill-pointer      :accessor btree:node-fill-pointer
                      :initform 0)
   (node-list         :reader   node-list
                      :initform (make-array 103)
                      :initarg  :node-list)
   ))

;; -----------------------------------

(defmethod btree:node-list-cell ((node node) index)
  (svref (node-list node) index))

(defmethod (setf btree:node-list-cell) (val (node node) index)
  (setf (svref (node-list node) index) val))

;; -----------------------------

(defmethod btree:copy-node-list-cells ((to node) to-index
                                       (from node) from-index
                                       ncells)
  (setf (subseq (node-list to) to-index (+ to-index ncells))
        (subseq (node-list from) from-index (+ from-index ncells))))

;; ------------------------------------------------------------

(defmethod btree:node-capacity ((node node))
  (- (length (node-list node)) 2))

;; -------------------------------------------
;; -------------------------------------------
;; BTree class -- an in-memory version of B-Trees.
;; BTree classes are required to have methods:
;;
;;   btree:root-node,   (setf btree:root-node)
;;   btree:items-count, (setf btree:items-count)
;;   btree:compare-fn,
;;   btree:key-fn
;;   btree:make-node,
;;   btree:discard-node
;;   btree:lock

(defclass btree (btree:btree)
  ((root-node   :accessor btree:root-node
                :initform nil)
   
   (items-count :accessor btree:items-count
                :initform 0)
   
   (node-size   :reader   node-size
                :initarg  :node-size
                :initform 101)

   (btree-lock  :reader   btree:btree-lock
                :initform (mpcompat:make-lock :name "BTree"))
                
   ))

(defun make-btree (&key (compare '-) (key 'identity) (node-size 101))
  (let ((node-size (logior 1 node-size))) ;; ensure size is oddp
    (make-instance 'btree
                   :compare    compare
                   :key        key
                   :node-size  node-size)))

(defmethod btree:make-node ((btree btree) height)
  (make-instance 'node
                 :height    height
                 :node-list (make-array (+ 2 (node-size btree)))
                 ))

(defmethod btree:discard-node ((btree btree) (node node))
  ;; nothing to do here...
  t)

(defmethod btree:coerce-to-object ((btree btree) obj)
  obj)

;; ---------------------------------------------------------

(defmethod btree:get-cache ((btree btree) constructor-fn)
  (declare (ignore constructor-fn))
  nil)

;; --------------------------------------

