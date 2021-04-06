;; trie.isp - Simple Trie for strings
;;
;; Case insensitive
;;
;; DM/RAL 04/21
;; ------------------------------------------------------------------

(in-package #:trie)

(defconstant +empty+ "empty")

(defstruct trie
  ch children
  (val +empty+))

(defvar *trie* (make-trie))

(defun trie-find-node (node str &optional (pos 0))
  (if (>= pos (length str))
      node
    (let ((ch (char str pos)))
      (um:when-let (child (find ch (trie-children node)
                                :key #'trie-ch))
        (trie-find-node child str (1+ pos))))
    ))

(defun trie-find (node str)
  (um:when-let (child (trie-find-node node str))
    (unless (eq (trie-val child) +empty+)
      (values (trie-val child) t))))

(defun trie-insert (node str val &optional (pos 0))
  (if (>= pos (length str))
      (setf (trie-val node) val)
    (let* ((ch    (char str pos))
           (child (find ch (trie-children node)
                        :key #'trie-ch)))
      (unless child
        (setf child (make-trie :ch ch)
              (trie-children node) (sort (cons child (trie-children node)) #'char-lessp
                                         :key #'trie-ch)
              ))
      (trie-insert child str val (1+ pos)))
    ))

(defun trie-delete (node str)
  (um:when-let (child (trie-find-node node str))
    (setf (trie-val child) +empty+)))

(defun trie-catalog (node &optional chars coll)
  ;; post-order traversal collection
  (dolist (child (reverse (trie-children node)))
    (let ((chars (cons (trie-ch child) chars)))
      (setf coll (trie-catalog child chars coll))
      (unless (eq (trie-val child) +empty+)
        (let ((str (coerce (reverse chars) 'string)))
          (push (cons str (trie-val child)) coll)
          ))))
  coll)

(defun trie-autocomplete (node str)
  (um:when-let (child (trie-find-node node str))
    (mapcar #'car (trie-catalog child (nreverse (coerce str 'list))))))

;; ----------------------------------------------------------
;; Visualization

(defmethod print-node (x)
  nil)

(defmethod print-node ((trie trie))
  (with-standard-io-syntax
    (if (eq (trie-val trie) +empty+)
        (prin1-to-string (trie-ch trie))
      (format nil "~S ~S" (trie-ch trie) (trie-val trie))
      )))

#+:LISPWORKS
(defmethod view-trie ((s trie) &key (layout :left-right))
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function layout
                  :roots (list s)

                  :children-function #'trie-children
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
