;; btree-clos.lisp -- Abstract superclass for B-Trees
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08
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

;; -------------------------------------------
(in-package #:btree)
;; -------------------------------------------

(defclass node ()
  ())

(defgeneric node-height (node)
  (:documentation "(node-height node)
Return the height of the node."))

(defgeneric node-fill-pointer (node)
  (:documentation "(node-fill-pointer node)
Return the current fill pointer of the node."))

(defgeneric (setf node-fill-pointer) (index node)
  (:documentation "(setf (node-fill-pointer node) index)
Set the current fill pointer of the node to index."))

(defgeneric node-list-cell (node index)
  (:documentation "(node-list-cell node index)
Return the contents of the node cell at index."))
  
(defgeneric (setf node-list-cell) (node-or-object node index)
  (:documentation "(setf (node-list-cell node index) node-or-object)
Set the contents of the node cell at index."))
  
(defgeneric node-capacity (node)
  (:documentation "(node-capacity node)
Return the maximum number of usable cells in the node.
This should not include the extra 2 cells at the end used for intermediate results."))
  
(defgeneric copy-node-list-cells (to-node to-index from-node from-index count)
  (:documentation "(copy-node-list-cells to-node to-index from-node from-index ncells)
Copy ncells from the from-node starting at from-index,
to the to-node starting at to-index."))

;; ---------------------------------------------------------------------

(defclass btree ()
  ((compare-fn  :reader   compare-fn
                :initarg  :compare
                :initform '-)

   (key-fn      :reader   key-fn
                :initarg  :key
                :initform 'identity)
   ))

(defgeneric root-node (btree)
  (:documentation "(root-node btree)
Return the root node of the tree."))

(defgeneric (setf root-node) (node-or-nil btree)
  (:documentation "(setf (root-node btree) node)
Set the root node of the tree."))

(defgeneric items-count (btree)
  (:documentation "(items-count btree)
Return the number of items in the tree."))

(defgeneric (setf items-count) (count btree)
  (:documentation "(setf (items-count btree) count)
Set the number of items in the tree."))

(defgeneric make-node (btree height)
  (:documentation "(make-node btree height)
Allocate a new tree node with the given height and return the node."))

(defgeneric discard-node (btree node)
  (:documentation "(discard-node btree node)
Discard the node, doing whatever may be necessary to recycle storage."))

(defgeneric get-cache (btree constructor-fn)
  (:documentation "(get-cache btree constructor-fn)
Return the cache to be used for this BTree."))

(defgeneric btree-lock (btree)
  (:documentation "(btree-lock btree)
Return the lock for the BTree"))

(defgeneric coerce-to-object (btree node-or-object)
  (:documentation "(coerce-to-object val)
Coerces the node cell datum to an object for key comparison"))

;; --------------------------------------
;; Here, and below is independent of details of BTree and Node class
;; --------------------------------------

(defmacro with-locked-btree ((btree) &body body)
  `(mpcompat:with-lock ((btree-lock ,btree))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-locked-btree" 1)

;; --------------------------------------
;; BTree Node Cacheing

#|
(defvar *dbg* (debug-stream:make-debug-stream))
(defun pr (msg)
  (debug-stream:pr *dbg* msg))
|#

(defvar *btree-cacheing* t)

(defun do-with-cache (btree fn-make fn-body)
  (um:when-let (cache (and *btree-cacheing*
                           (get-cache btree fn-make)))
    (funcall fn-body cache)))

(defmacro with-cache ((cache-name btree &optional fn-make) &body body)
  `(do-with-cache ,btree ,fn-make
                  (lambda (,cache-name)
                    ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-cache" 1)

(defun clear-cache (btree)
  "(clear-cache btree)
Invalidate the cache, if any."
  (with-cache (cache btree)
    (um:clear-cache cache)))

(defun check-cache (btree key)
  "(check-cache btree key)
Return the val, its node, and its index, if present in the cache. Otherwise
clear the cache and return nil."
  (with-cache (cache btree)
    (values-list (um:check-cache cache key))))

(defun update-cache (btree key val node index)
  "(update-cache btree key val node index)
Allow the implementation to cache these values."
  (with-cache (cache btree (lambda ()
                             (make-instance 'um:2-way-cache
                                            :nlines 1
                                            :test (um:compose 'zerop (compare-fn btree)))))
    (um:update-cache cache key (list val node index))))

;; --------------------------------------

(defun half-capacity (node)
  ;; for capacity >~ 10
  (ash (truncate (+ 2 (node-capacity node))
                 4)
       1))

;; --------------------------------------

(defun cell-width (node)
  ;; leaf nodes (height = 1) width = 1
  ;; non-leaf nodes (height > 1) width = 2
  (min (node-height node) 2))

;; --------------------------------------

(defun leaf-node-p (node)
  (= 1 (node-height node)))

;; --------------------------------------
;; user API

(defun first-item (btree)
  (with-locked-btree (btree)
    (um:when-let (root (root-node btree))
        (um:nlet iter ((node root))
          (let ((first-node (node-list-cell node 0)))
            (if (leaf-node-p node)
                (coerce-to-object btree first-node)
              (go-iter first-node))
            )))))
    
;; --------------------------------------
;; user API

(defun last-item (btree)
  (with-locked-btree (btree)
    (um:when-let (root (root-node btree))
        (um:nlet iter ((node root))
          (let* ((last-ix   (1- (node-fill-pointer node)))
                 (last-node (node-list-cell node last-ix)))
            (if (leaf-node-p node)
                (coerce-to-object btree last-node)
              (go-iter last-node))
            )) )))
    
;; --------------------------------------
;; user API

(defun map-tree (btree fn
                       &key
                       from
                       to
                       (direction :forward)
                       max-records)
  ;; for each item in the tree, whose keys satisfy the inclusive range [from,to],
  ;; call the user function fn with those items. The function should take one argument
  ;; the item itself. Function return values are ignored.
  (with-locked-btree (btree)
    (um:when-let (root (root-node btree))
        (let* ((compare-fn (compare-fn btree))
               (key-fn     (key-fn btree))
               (delta      (* (or (and from to
                                       (if (minusp (funcall compare-fn to from))
                                           -1
                                         1))
                                  1)
                              (if (eq direction :reverse) -1 1)))
               (count      0))
      
          (um:nlet iter ((node root)
                         (from from))
            (when (or (null max-records)
                      (< count max-records))
              (let* ((cell-width (cell-width node))
                     (limit      (node-fill-pointer node)))
                
                (labels ((doit (ix)
                           (let ((obj (coerce-to-object btree (node-list-cell node ix))))
                             (if (or (null to)
                                     (<= (* delta (funcall compare-fn (funcall key-fn obj) to))
                                         0))
                                 (progn
                                   (funcall fn obj)
                                   (incf count))
                               (return-from map-tree count))
                             ))
                         
                         (iter-entries (start-ix from)
                           (let ((loop-limit (if (plusp delta) limit -1)))
                             (do ((ix start-ix (+ ix delta)))
                                 ((= ix loop-limit))
                               (if (and (> cell-width 1)
                                        (evenp ix))
                                   ;; handle subtrees
                                   (iter (node-list-cell node ix) from)
                                 ;; else handle this item
                                 (doit ix)))
                             )))
                  
                  (if from
                      (multiple-value-bind (found-it ix) (search-node from btree node)
                        (if found-it
                            (iter-entries (node-object-index node ix) nil)
                          (if (> cell-width 1)
                              (progn
                                (iter (node-list-cell node (* 2 ix)) from)
                                (iter-entries (+ delta (* 2 ix)) nil))
                            (iter-entries (if (> delta 0) ix (1- ix)) nil))
                          ))
                    ;; else
                    (iter-entries (if (plusp delta) 0 (1- limit)) nil)) ))) )))) )
  
;; -----------------------------------------------------------

(defun node-object-index (node ix)
  (let ((cell-width (cell-width node)))
    (+ (* cell-width ix) (1- cell-width))))

(defun node-object (btree node ix)
  (coerce-to-object btree (node-list-cell node (node-object-index node ix))))

(defun (setf node-object) (obj node ix)
  (setf (node-list-cell node (node-object-index node ix)) obj))

(defun search-node (key btree node)
  (um:bind*
      ((compare-fn (compare-fn btree))
       (key-fn     (key-fn btree)))
    (um:binsearch 0 (truncate (node-fill-pointer node) (cell-width node))
                  #'(lambda (jx)
                      (funcall compare-fn key (funcall key-fn (node-object btree node jx))))
                  )))

;; -----------------------------------------------------------
;; user API
  
(defun find-item (btree item-key)
  ;; node is an off_t pointing to the node record
  ;; key is a physical key to be compared to records containing keys
  (with-locked-btree (btree)
    (multiple-value-bind (val found-it)
        (check-cache btree item-key)
      (if found-it
          (values val t)
        ;; else
        (um:when-let (root (root-node btree))
            (um:nlet srch ((node root))
              (multiple-value-bind (found-it ix) (search-node item-key btree node)
                (if found-it
                    (progn
                      (let ((val (node-object btree node ix)))
                        (update-cache btree item-key val node ix)
                        (values val t)))
                  ;; else -- if we have a subtree, search there
                  (when (> (node-height node) 1)
                    (go-srch (node-list-cell node (* 2 ix))))
                  ))))
        ))))

;; -----------------------------------------------------------------------

(defstruct cursor
  btree
  state
  dir)

(defun drill-down-left (node state)
  (let ((next-state (list* node 0 state)))
    (if (> (node-height node) 1)
        (drill-down-left (node-list-cell node 0) next-state)
      ;; else - in leaf node
      next-state)))


(defun drill-down-right (node state)
  (let ((nfp (node-fill-pointer node)))
    (if (> (node-height node) 1)
        (drill-down-right (node-list-cell node (1- nfp))
                          (list* node (1- (truncate nfp 2)) state))
      ;; else - in leaf node
      (list* node (1- nfp) state))
    ))

;; user API
(defun create-cursor (btree &key
                            key
                            (from-start t)
                            from-end)
  (declare (ignore from-start))
  (with-locked-btree (btree)
    (um:when-let (root (root-node btree))
        (cond (key
               (um:nlet srch ((node  root)
                              (state nil))
                 (multiple-value-bind (found-it ix) (search-node key btree node)
                   (if (and (not found-it)
                            (> (node-height node) 1))
                       (go-srch (node-list-cell node (* 2 ix))
                                (list* node ix state))
                     ;; else
                     (make-cursor
                      :btree btree
                      :state (list* node ix state)) ))
                 ))

              (from-end
               (make-cursor
                :btree btree
                :state (drill-down-right root nil)))

              (t ;; from start
                 (make-cursor
                  :btree btree
                  :state (drill-down-left root nil)))
              ))))

(defun get-cursor-item-and-advance (cursor dir)
  (with-locked-btree ((cursor-btree cursor))
    (cond ((cursor-state cursor)
           (setf (cursor-dir cursor) dir)
           (destructuring-bind (node ix . rest) (cursor-state cursor)
             (if (or (< ix 0)
                     (>= (node-object-index node ix) (node-fill-pointer node))) ;; beyond end?
                 (progn
                   (setf (cursor-state cursor) rest)
                   (get-cursor-item-and-advance cursor dir))
             
               ;;  else - not beyond end
               (progn
                 (if (eq dir :next)
                     (setf (cursor-state cursor)
                           (if (> (node-height node) 1) ;; interior-node?
                               (drill-down-left (node-list-cell node (* 2 (1+ ix)))
                                                (list* node (1+ ix) rest))
                             ;; else -- leaf node
                             (list* node (1+ ix) rest)))
                 
                   ;; else - dir = :prev
                   (setf (cursor-state cursor)
                         (if (> (node-height node) 1)
                             (drill-down-right (node-list-cell node (* 2 ix))
                                               (list* node (1- ix) rest))
                           ;; else -- leaf node
                           (list* node (1- ix) rest))))
               
                 (values (node-object (cursor-btree cursor) node ix) t))
               )))

          ((eq dir (cursor-dir cursor)) nil) ;; off the end
        
          ((eq dir :next)
           (setf (cursor-state cursor) (drill-down-left (root-node (cursor-btree cursor))
                                                        nil))
           (get-cursor-item-and-advance cursor dir))
        
          (t ;; dir = :prev
             (setf (cursor-state cursor) (drill-down-right (root-node (cursor-btree cursor))
                                                           nil))
             (get-cursor-item-and-advance cursor dir))
          )))

;; user API
(defun cursor-next (cursor)
  (when cursor
    (get-cursor-item-and-advance cursor :next)))

;; user API
(defun cursor-previous (cursor)
  (when cursor
    (get-cursor-item-and-advance cursor :prev)))

;; --------------------------------------------------
;; user API

(defun add/update-item (btree item-key &key (add-fn 'identity) (update-fn 'identity))
  ;; node is an off_t pointing to the node record
  ;; key is a physical key to be compared to records containing keys
  ;;
  ;; When an item cannot be located in the tree, it will create a new entry
  ;; after calling add-fn for its value. Add-fn is a function of one argument, the item key,
  ;; and should return the newly created item as its result.
  ;;
  ;; When an item is already in the tree, the update-fn will be called. Update-fn is a
  ;; function of one argument: the item itself. The update-fn should return the object that
  ;; will be stored in the tree.
  ;;
  (with-locked-btree (btree)
    (multiple-value-bind (val node ix)
        (check-cache btree item-key)
      (if node
          (let ((new-val (funcall update-fn val)))
            (setf (node-object node ix) new-val)
            (update-cache btree item-key new-val node ix)
            (return-from add/update-item))
        ;; else
        (labels
            ((srch (node)
               (multiple-value-bind (found-it ix) (search-node item-key btree node)
                 (if found-it
                     (let ((new-val (funcall update-fn (node-object btree node ix))))
                       (setf (node-object node ix) new-val)
                       (update-cache btree item-key new-val node ix)
                       (return-from add/update-item))
                 
                   ;; else - not there
                   (if (> (node-height node) 1)
                       ;; non-leaf node = 2 element cells = (subnode, obj)
                       (let* ((nx       (* 2 ix))
                              (sub-node (node-list-cell node nx)))
                         (when (srch sub-node)
                           (clear-cache btree) ;; no cacheing because of rearrangements
                           (adjust-subtree-from-add btree node nx)))
                     ;; else - in leaf node = just obj cells
                     (let ((new-val (funcall add-fn item-key)))
                       (incf (items-count btree))
                       (clear-cache btree) ;; no cacheing because of insertions
                       (insert-into-node node ix new-val)))
                   )) ))
          (let ((root (root-node btree)))
            (unless root
              (setf root (make-node btree 1)
                    (root-node btree) root))
            (when (srch root)
              (clear-cache btree) ;; no cacheing because we might rearrange nodes
              (split-rootnode btree root))
            )))
      )))

;; --------------------------------------------------
;; user API

(defun insert-item (btree item-key obj)
  (add/update-item btree item-key
                   :add-fn    (constantly obj)
                   :update-fn (constantly obj)))

;; --------------------------------------------------

(defun adjust-subtree-from-add (btree node ix)
  ;; pnode here is always a non-leaf node
  (unless (or (and (> ix 0)
                   (try-rebalancing-nodes node (- ix 2)))
              (let ((limit (node-fill-pointer node)))
                (and (< ix (1- limit))
                     (try-rebalancing-nodes node ix))))
    (split-subnode btree node ix)))

;; --------------------------------------------------

(defun split-subnode (btree node ix)
  (um:bind*
      ((left-node  (node-list-cell node ix))
       (height     (node-height left-node))
       (cell-width (cell-width left-node))
       (right-node (make-node btree height))
       (left-limit (node-fill-pointer left-node))
       (half-capy  (half-capacity node))
       (ncells     (- left-limit half-capy))
       (mid-obj    (node-list-cell left-node (+ half-capy (1- cell-width)))))

    (when (> height 1)
      (assert (oddp ncells))
      (assert (evenp ix))
      (assert (oddp left-limit)))
    
    (copy-node-list-cells right-node 0
                          left-node (+ half-capy cell-width)
                          (- ncells cell-width))
    (setf (node-fill-pointer right-node) (- ncells cell-width)
          (node-fill-pointer left-node)  (+ half-capy (1- cell-width))
          (node-list-cell node ix) right-node)
    (insert-into-node node ix (list mid-obj left-node))))

;; --------------------------------------------------

(defun split-rootnode (btree root)
  (um:bind*
      ((height     (node-height root))
       (new-root   (make-node btree (1+ height)))
       (limit      (node-fill-pointer root))
       (cell-width (cell-width root))
       (left-node  root)
       (right-node (make-node btree height))
       (half-capy  (half-capacity root))
       (nlcells    (+ half-capy (1- cell-width)))
       (nrcells    (- limit (+ half-capy cell-width))))

    (assert (evenp half-capy))
    (when (> height 1)
      (assert (oddp limit)))
    (when (> (node-height left-node) 1)
      (assert (oddp nlcells))
      (assert (oddp nrcells)))

    (copy-node-list-cells right-node 0 left-node (+ half-capy cell-width) nrcells)
    (setf (node-fill-pointer left-node) nlcells
          (node-fill-pointer right-node) nrcells

          (node-fill-pointer new-root) 3
          (node-list-cell new-root 0) left-node
          (node-list-cell new-root 1) (node-list-cell left-node (+ half-capy (1- cell-width)))
          (node-list-cell new-root 2) right-node

          (root-node btree) new-root)))

;; ----------------------------------------------------------------------------
;; --------------------------------------------------

(defun insert-into-node (node ix obj)
  (let* ((cell-width (cell-width node))
         (limit      (node-fill-pointer node))
         (ncells     (- limit ix))
         (new-limit  (+ limit cell-width)))

    (when (> cell-width 1)
      (assert (evenp ix))
      (assert (oddp limit)))
    
    (copy-node-list-cells node (+ ix cell-width) node ix ncells)
    (if (= cell-width 1)
        (setf (node-list-cell node ix) obj)
      ;; else
      (destructuring-bind (obj sub-node) obj
        (setf (node-list-cell node ix) sub-node
              (node-list-cell node (1+ ix)) obj)))
    
    (setf (node-fill-pointer node) new-limit)
    (>= new-limit (node-capacity node)) )) ;; t if need rebalance or split

;; --------------------------------------------------
;; --------------------------------------------------
;; -----------------------------------------------------------------------
;; user API

(defun delete-item (btree item-key &key (delete-fn 'identity))
  ;; node is an off_t pointing to the node record
  ;; key is a physical key to be compared to records containing keys
  ;;
  ;; When an item to be deleted is located, the delete-fn is called. This should
  ;; be a function of one argument: the item being deleted from the tree itself.
  ;; It does not matter what delete-fn returns. This is called to give the user
  ;; a chance to do whatever might be needed before the object is removed from the tree.
  ;;
  (with-locked-btree (btree)
    (clear-cache btree)
    (um:when-let (root (root-node btree))
      
        (um:nlet srch ((node root))
          (multiple-value-bind (found-it ix)
              (search-node item-key btree node)
            
            (cond (found-it
                   (let ((pobj (node-object btree node ix)))
                     (funcall delete-fn pobj)
                     (decf (items-count btree))
                     (delete-from-node btree node ix))) ;; returns T if parent needs adjustments
          
                  ;; else - not there
                  ((leaf-node-p node)
                   ;; in leaf - so just give up
                   (return-from delete-item))

                  (t ;; non-leaf node = 2 element cells = (subnode, obj)
                     (let ((nx (* 2 ix)))
                       (when (srch (node-list-cell node nx))
                         ;; might return T to trigger need for higher adjustments
                         (adjust-subnodes-from-deletion btree node nx))))
                  ))) )))

;; --------------------------------------------------------------------------------

(defun delete-from-node (btree node ix)
  ;; perform the physical deletion
  ;; Return T if the parent node needs adjustment
  (cond ((leaf-node-p node)
         ;; simple deletion
         (let* ((limit     (node-fill-pointer node))
                (new-limit (1- limit)))
           
           (cond ((= 0 new-limit) ;; can only happen in root node
                  (assert (eq node (root-node btree)))
                  (setf (root-node btree) nil))

                 (t ;; else
                    (when (< ix new-limit)
                      ;; scrunch up the items to fill the hole
                      (copy-node-list-cells node ix node (1+ ix) (- new-limit ix)))
                    (setf (node-fill-pointer node) new-limit)
                    (< new-limit (half-capacity node))) ;; return t if needs coalesce
                 )))

        (t ;; non-leaf node -- replace obj with rightmost subtree object
           (let ((nx (* 2 ix)))
             (multiple-value-bind (obj needs-coalesce)
                 (grab-rightmost-item btree (node-list-cell node nx))
               (setf (node-list-cell node (1+ nx)) obj)
               (when needs-coalesce
                 ;; might return t to trigger need for higher adjustment
                 (adjust-subnodes-from-deletion btree node nx)) )))
        ))

(defun grab-rightmost-item (btree sub-node)
  ;; We are called when an item is removed from a parent node. We will remove
  ;; our rightmost item and return it for placement in the parent node.
  ;; If our capacity drops below half-full then also indicate a need for adjustments.
  (let* ((last-cell  (1- (node-fill-pointer sub-node)))
         (hi-obj     (node-list-cell sub-node last-cell)))
    
    (cond ((leaf-node-p sub-node)
           (setf (node-fill-pointer sub-node) last-cell)
           (values hi-obj
                   (< last-cell (half-capacity sub-node)))) ;; t for needs coalesce
          
          ;; else - non-leaf node -- keep looking
          (t (multiple-value-bind (obj needs-coalesce)
                 (grab-rightmost-item btree hi-obj)
               (if needs-coalesce
                   (values obj
                           ;; might return t to trigger need for higher adjustment
                           (adjust-subnodes-from-deletion btree sub-node last-cell))
                 obj)))
          )))
             
;; -------------------------------------------------------------

(defun adjust-subnodes-from-deletion (btree node ix)
  ;; on entry node is the node which had a deletion
  ;; and node is a non-leaf node. Index ix points somewhere (anywhere)
  ;; in the node-list.
  ;;
  ;;   ... p k p k pl kk pr k p ... -- we are concerned about pl kk and pr
  ;;               ^
  ;; on entry index ix points to pl
  ;;
  ;; First try to rebalance the subnode to the left, then to the right.
  ;; If that fails, then coalesce the subnode to the left, unless we are
  ;; at the front of the list, in which case we coalesce to the right.
  ;;
  ;; Return t if parent node needs further adjustment
  (unless (or (and (> ix 0)
                   (try-rebalancing-nodes node (- ix 2)))
              (let ((limit (node-fill-pointer node)))
                (and (< ix (1- limit))
                     (try-rebalancing-nodes node ix))))
    (if (> ix 0)
        (coalesce-subnodes btree node (- ix 2))
      (coalesce-subnodes btree node ix))
    ))

;; --------------------------------------------------

(defun try-rebalancing-nodes (node nx)
  (let* ((left-subnode   (node-list-cell node nx))
         (right-subnode  (node-list-cell node (+ nx 2)))
         (llimit         (node-fill-pointer left-subnode))
         (rlimit         (node-fill-pointer right-subnode))
         (capy           (node-capacity node))
         (tcells         (+ llimit rlimit 1)))

    ;; condition for rebalancing instead of splitting/coalescing
    (when (and (> tcells capy)
               (<= tcells (* 2 capy)))
      
      (let* ((cell-width (cell-width left-subnode))
             (nxmid      (truncate (+ llimit rlimit) 2))
             ;; make ncells even if subnodes have 2-element cells
             (ncells     (um:align-pwr2 (- (max llimit rlimit) nxmid) cell-width)))

        (when (> cell-width 1)
          (assert (evenp nx))
          (assert (evenp ncells)))
        
        (cond
         ((> llimit (+ rlimit cell-width))
            ;; left subnode more full
            ;; right: xxxxx... -> ____xxxxx...
            (copy-node-list-cells right-subnode ncells
                                  right-subnode 0
                                  rlimit)
          
            ;; right: super-node cell -> ___Sxxxx...
            (setf (node-list-cell right-subnode (1- ncells))
                  (node-list-cell node (1+ nx)))
          
            ;; left->right for ncells-1 -> LLLLSxxxx....
            (if (> ncells 1)
                (copy-node-list-cells right-subnode 0
                                      left-subnode (- llimit (1- ncells))
                                      (1- ncells)))
          
            ;; leftmost cell of the ncells -> supernode
            (setf (node-list-cell node (1+ nx))
                  (node-list-cell left-subnode (- llimit ncells)))
          
            ;; adjust fill pointers
            (setf (node-fill-pointer left-subnode)  (- llimit ncells)
                  (node-fill-pointer right-subnode) (+ rlimit ncells))
          
            (assert (<= (- llimit ncells) (node-capacity node)))
            (assert (<= (+ rlimit ncells) (node-capacity node)))
            (assert (>= (+ rlimit ncells) (1- (half-capacity node))))
            (assert (>= (- llimit ncells) (1- (half-capacity node))))
            :ok) ;; return non-nil to indicate successful rebalancing

         
         ;; ---------------------------------
         ;; ---------------------------------
         ((> rlimit (+ llimit cell-width))
          ;; else right-node more full
          ;; left<-supercell:  xxx... => xxxS...
          (setf (node-list-cell left-subnode llimit)
                (node-list-cell node (1+ nx)))
          
          ;; left<-right:  xxxSRRRR... 
          (if (> ncells 1)
              (copy-node-list-cells left-subnode (1+ llimit)
                                    right-subnode 0
                                    (1- ncells)))
          
          ;; rightmost cell of the ncells -> supercell
          (setf (node-list-cell node (1+ nx))
                (node-list-cell right-subnode (1- ncells)))
          
          ;; shove remaining right cells (rlimit-ncells) down to left end
          (copy-node-list-cells right-subnode 0
                                right-subnode ncells
                                (- rlimit ncells))
          
          ;; adjust fill pointers
          (setf (node-fill-pointer left-subnode) (+ llimit ncells)
                (node-fill-pointer right-subnode) (- rlimit ncells))
          
          (assert (<= (+ llimit ncells) (node-capacity node)))
          (assert (<= (- rlimit ncells) (node-capacity node)))
          (assert (>= (+ llimit ncells) (1- (half-capacity node))))
          (assert (>= (- rlimit ncells) (1- (half-capacity node))))
          :ok) ;; return non-nil to indicate successful rebalancing
         )))))

;; -------------------------------------------------------------

(defun coalesce-subnodes (btree node nx)
  (let* ((left-subnode   (node-list-cell node nx))
         (right-subnode  (node-list-cell node (+ nx 2)))
         (llimit         (node-fill-pointer left-subnode))
         (rlimit         (node-fill-pointer right-subnode))
         (new-limit      (- (node-fill-pointer node) 2)))

    ;; together the left subnode, right subnode, and our kk all fit into the left subnode.
    ;; we already know that right-subnode is below half-capacity
    ;; we need to merge right contents into left subnode and then
    ;; remove our last pair -- might cascade coalescing to higher levels
    ;;
    ;;                         pl D pr
    ;; E.g.,  p1 A p2 B p3 C p4       p5 E p6  :max 11
    ;;
    ;;                                pl x xx
    ;; ==> p1 A p2 B p3 C p4 D p5 E p6       xx x xx
    ;;
    ;; Always sacrifice the right subnode
    ;;

    ;; normal coalesce - move all right elements to left node
    ;; and discard the right node
    (setf (node-list-cell left-subnode llimit) (node-list-cell node (1+ nx)))
    (copy-node-list-cells left-subnode (1+ llimit) right-subnode 0 rlimit)
    (setf (node-fill-pointer left-subnode) (+ llimit rlimit 1))
    (discard-node btree right-subnode)
    
    ;; adjust the ancestor node [pnode]
    (let ((ntail (- new-limit (1+ nx))))
      (cond ((> ntail 0)
             ;; usual case
             (copy-node-list-cells node (1+ nx) node (+ nx 3) ntail)
             (setf (node-fill-pointer node) new-limit)
             ;; signal need for coalescing of pnode if less than half full
             (< new-limit (half-capacity node)))
        
            ;; else -- nothing to move
            ((= new-limit 1)
             ;; can only happen when node is root and the entire tree
             ;; has collapsed to the left node.
             ;; Just replace root by the left node, discarding both
             ;; old root node and right subnode.
             (discard-node btree node)
             (setf (root-node btree) left-subnode))

          ;; else - nothing to move
          (t
           (setf (node-fill-pointer node) new-limit)
           ;; signal need for coalescing of pnode if less than half full
           (< new-limit (half-capacity node)))
          ))))

;; --------------------------------------------------

;; ---------------------------------------------------------------------------------

#+:LISPWORKS
(defun view-btree (btree)
  (with-locked-btree (btree)
    (um:when-let (root (root-node btree))
        (capi:contain
         (make-instance
          'capi:graph-pane
          :title (format nil "B-Tree for ~S" btree)
          :roots (list root)
          :children-function #'(lambda (node)
                                 (let* ((height (node-height node))
                                        (limit  (node-fill-pointer node)))
                                   (when (> height 1)
                                     (nreverse
                                      (loop for ix from 0 below limit by 2 collect
                                            (node-list-cell node ix))))))
          :print-function #'(lambda (node)
                              (let* ((height (node-height node))
                                     (limit  (node-fill-pointer node)))
                                (format nil "~X (H:~D, N:~D)" node height
                                        (if (> height 1) (truncate limit 2) limit))))
          )))))

