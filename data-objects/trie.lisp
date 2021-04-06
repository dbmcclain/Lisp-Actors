;; trie.isp - Simple Trie for strings
;;
;; Case insensitive
;;
;; DM/RAL 04/21
;; ------------------------------------------------------------------

(in-package #:trie)

(defconstant +empty+ "empty")

(defstruct node
  ch children
  (val +empty+))

(defvar *trie* (make-node))

(defun trie-find-node (node str &optional (pos 0))
  (if (>= pos (length str))
      node
    (let ((ch (char str pos)))
      (um:when-let (child (find ch (node-children node)
                                :key #'node-ch))
        (trie-find-node child str (1+ pos))))
    ))

(defun trie-find (node str)
  (um:when-let (child (trie-find-node node str))
    (unless (eq (node-val child) +empty+)
      (values (node-val child) t))))

(defun trie-insert (node str val &optional (pos 0))
  (if (>= pos (length str))
      (setf (node-val node) val)
    (let* ((ch    (char str pos))
           (child (find ch (node-children node)
                        :key #'node-ch)))
      (unless child
        (setf child (make-node :ch ch)
              (node-children node) (sort (cons child (node-children node)) #'char-lessp
                                         :key #'node-ch)
              ))
      (trie-insert child str val (1+ pos)))
    ))

(defun trie-delete (node str)
  (um:when-let (child (trie-find-node node str))
    (setf (node-val child) +empty+)))

(defun trie-catalog (node &optional chars coll)
  ;; post-order traversal collection
  (dolist (child (reverse (node-children node)))
    (let ((chars (cons (node-ch child) chars)))
      (setf coll (trie-catalog child chars coll))
      (unless (eq (node-val child) +empty+)
        (let ((str (coerce (reverse chars) 'string)))
          (push (cons str (node-val child)) coll)
          ))))
  coll)

(defun trie-autocomplete (node str)
  (um:when-let (child (trie-find-node node str))
    (mapcar #'car (trie-catalog child (nreverse (coerce str 'list))))))

;; ----------------------------------------------------------
;; Visualization

(defmethod trie-children (trie layout)
  nil)

(defmethod trie-children ((trie node) layout)
  (node-children trie))

(defmethod print-node (x)
  nil)

(defmethod print-node ((trie node))
  (with-standard-io-syntax
    (if (eq (node-val trie) +empty+)
        (prin1-to-string (node-ch trie))
      (format nil "~S ~S" (node-ch trie) (node-val trie))
      )))

#+:LISPWORKS
(defmethod view-trie ((s node) &key (layout :left-right))
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function layout
                  :roots (list s)

                  :children-function #'(lambda (node)
                                         (trie-children node layout))
                  :print-function    #'(lambda (node)
                                         (print-node node))
                  :action-callback (lambda (item intf)
                                     (declare (ignore intf))
                                     (inspect item))
                  )))

#|
(trie-insert *trie* "chara" :my-cat)
(trie-insert *trie* "cookie" :my-mut)
(trie-insert *trie* "pancake" :my-beagle)
(trie-insert *trie* "lisp" :my-language)
(trie-insert *trie* "character" :what-a)

(trie-catalog *trie*)

(view-trie *trie*)
(view-trie *trie* :layout :top-down)
|#
