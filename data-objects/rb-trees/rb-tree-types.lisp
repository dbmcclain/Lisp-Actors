;; rb-tree-types.lisp -- Functorized Tree Types
;;
;; DM/RAL  2026/07/17T06:09:25U
;; --------------------------------------------

(in-package #:com.ral.rb-trees.sets)

;; --------------------------------------------

(defun /eql (a b)
  (not (eql a b)))

;; --------------------------------------------

(defclass tree-type ()
  ()
  (:metaclass
   #+:LISPWORKS clos:funcallable-standard-class
   #+:SBCL      sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((type tree-type)
                                       &key
                                       (compare-fn   'ord:compare)
                                       (replace-p-fn '/eql))
  (let ((add     (make-add-fn     compare-fn replace-p-fn))
        (split   (make-split-fn   compare-fn))
        (mem     (make-mem-fn     compare-fn))
        (remove  (make-remove-fn  compare-fn))
        (compare (make-compare-fn compare-fn))
        (subset  (make-subset-fn  compare-fn)))
    (#+:LISPWORKS
     clos:set-funcallable-instance-function
     #+:SBCL
     sb-mop:set-funcallable-instance-function
     type
     (um:dlambda
       (:compare-fn ()  compare-fn)
       (:replace-p-fn () replace-p-fn)
       (:add (nodes key val)
        (funcall add nodes key val))
       (:split (nodes key)
        (funcall split nodes key))
       (:split-fn ()  split)
       (:mem  (nodes key)
        (funcall mem nodes key))
       (:remove (nodes key)
        (funcall remove nodes key))
       (:compare (nodes1 nodes2)
        (funcall compare nodes1 nodes2))
       (:subset (nodes1 nodes2)
        (funcall subset nodes1 nodes2))
       ))))

(defun tree-type-compare-fn (tree-type)
  (funcall tree-type :compare-fn))

(defun tree-type-replace-p-fn (tree-type)
  (funcall tree-type :replace-p-fn))

#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((type tree-type) (mode (eql 'signature)))
  (declare (ignore mode))
  (values (list :compare-fn :replace-p-fn)
          (list (tree-type-compare-fn type)
                (tree-type-replace-p-fn type))
          nil nil 'tree-type))
  
;; --------------------------------------------

(defvar *tree-types* nil)

(defun find-tree-type (type-list compare-fn replace-p-fn)
  (flet
      ((fn-ptr (x)
         (if (symbolp x)
             (symbol-function x)
           x)))
    (let ((my-compare (fn-ptr compare-fn))
          (my-replace (fn-ptr replace-p-fn)))
      (find-if (lambda (type)
                 (and (eq my-compare
                          (fn-ptr (tree-type-compare-fn type)))
                      (eq my-replace
                          (fn-ptr (tree-type-replace-p-fn type)))))
               type-list)
      )))

(defun ensure-type-available (tree-type)
  (multiple-value-bind (_ actual-type)
      (um:rmw *tree-types*
              (lambda (type-list)
                (let ((found (find-tree-type type-list
                                             (tree-type-compare-fn   tree-type)
                                             (tree-type-replace-p-fn tree-type))
                             ))
                  (if found
                      (values type-list found)
                    (values (cons tree-type type-list) tree-type))
                  )))
    (declare (ignore _))
    actual-type))
  
(defun make-tree-type (&key (compare-fn   'ord:compare)
                              (replace-p-fn '/eql))
  (multiple-value-bind (_ actual-type)
      (um:rmw *tree-types*
              (lambda (type-list)
                (let ((found (find-tree-type type-list compare-fn replace-p-fn)))
                  (if found
                      (values type-list found)
                    (let ((new-type (make-instance 'tree-type
                                                   :compare-fn   compare-fn
                                                   :replace-p-fn replace-p-fn) ))
                      (values (cons new-type type-list) new-type))
                    ))))
    (declare (ignore _))
    actual-type))

(defvar +default-tree-type+ (make-tree-type
                             :compare-fn   'ord:compare
                             :replace-p-fn '/eql))

;; --------------------------------------------

(defmethod initialize-instance :after ((tree tree)
                                       &key
                                       (tree-type +default-tree-type+)
                                       (compare-fn   'ord:compare compare-present-p)
                                       (replace-p-fn '/eql        replace-present-p)
                                       cloning
                                       nodes)
  (unless cloning
    (setf tree-type
          (if (or compare-present-p
                  replace-present-p)
              (make-tree-type
               :compare-fn   compare-fn
               :replace-p-fn replace-p-fn)
            ;; else
            (ensure-type-available tree-type))
          ))
  (#+:LISPWORKS
   clos:set-funcallable-instance-function
   #+:SBCL
   sb-mop:set-funcallable-instance-function
   tree
   (flet ((clone-me (new-nodes)
            (if (eq new-nodes nodes)
                tree
              (make-instance 'tree
                             :tree-type tree-type
                             :cloning   t
                             :nodes     new-nodes))))
     (um:dlambda
       (:nodes () nodes)
       (:type ()  tree-type)
       (:add (key val)
        (clone-me (funcall tree-type :add nodes key val)))
       (:remove (key)
        (clone-me (funcall tree-type :remove nodes key)))
       (:split (key)
        (with-list-bindings (l present r)
            (funcall tree-type :split nodes key)
          (list (clone-me l)
                present
                (clone-me r))))
       (:split-fn ()
        (funcall tree-type :split-fn))
       (:mem (key)
        (funcall tree-type :mem nodes key))
       (:compare (s2)
        (funcall tree-type :compare nodes (funcall s2 :nodes)))
       (:subset (s2)
        (funcall tree-type :subset nodes (funcall s2 :nodes)))
       (:clone-with (nodes)
        (clone-me nodes))
       ))))

(defun make-tree (&rest args &key tree-type compare-fn replace-p-fn)
  ;; Args can be :TREE-TYPE, :COMPARE-FN, :REPLACE-P-FN, or none for defaults.
  (declare (ignore tree-type compare-fn replace-p-fn))
  (apply #'make-instance 'tree args))
