
(defpackage #:finger-tree
  (:use :common-lisp)
  (:export
   #:pushq
   #:popq
   #:addq
   #:getq
   #:is-empty?
   #:not-empty?
   #:make-shared-finger-tree
   #:make-unshared-finger-tree
   #:copy
   #:erase
   #:copy-as-shared
   #:copy-as-unshared
   #:cardinal
   #:elements
   ))

(in-package #:finger-tree)

;; --------------------------------------
#|
(defvar *tst* (make-finger-tree))

(inspect
 (progn
   (dolist (item '(a b c d e f g h i j k l m n o p q r s t u))
     (addq *tst* item))
   *tst*))

(inspect
 (um:nlet iter ((tree nil)
                (items '(a b c d e f g h i j k l m n o p q r s t u)))
   (if (endp items)
       tree
     (go-iter (%put tree (car items)) (cdr items)))
   ))

(let* ((data '(a b c d e f g h i j k l m n o p q r s t u))
       (tree (um:nlet iter ((tree  nil)
                            (items data))
               (if (endp items)
                   tree
                 (go-iter (addq tree (car items)) (cdr items)))
               )))
  (inspect tree)
  ;; (break)
  (um:nlet iter ((tree  tree)
                 (items (reverse data)))
    (if (endp items)
        (assert (null tree))
      (multiple-value-bind (x treex) (getq tree)
        (assert (eq (car items) x))
        (go-iter treex (cdr items)))
      )))
|#
;; --------------------------------------------------------
;; Vector-based skeletal elements to cut down on consing.

(declaim (inline \1f \2f \3f \4f st ft \1f? \4f?))

(defun \1f (a)
  (vector '\1f a))

(defun \2f (a b)
  (vector '\2f a b))

(defun \3f (a b c)
  (vector '\3f a b c))

(defun \4f (a b c d)
  (vector '\4f a b c d))

(defun st (x)
  (vector 'st x))

(defun ft (p q r)
  (vector 'ft p q r))

(defun \1f? (x)
  (eq '\1f (svref x 0)))

(defun \4f? (x)
  (eq '\4f (svref x 0)))

;; -----------------------------------------------------

(defmethod pushq ((tree null) x)
  (st x))

(defmethod pushq ((tree vector) x)
  (symbol-macrolet ((sel (svref tree 0))
                    (a   (svref tree 1))
                    (b   (svref tree 2))
                    (c   (svref tree 3))
                    (p   (svref tree 1))
                    (q   (svref tree 2))
                    (r   (svref tree 3)))
    (case sel
      ((\1f) (\2f x a))
      ((\2f) (\3f x a b))
      ((\3f) (\4f x a b c))
      ((st)  (ft (\1f x) nil (\1f a)))
      ((ft)  (if (\4f? p)
                 (symbol-macrolet ((pa (svref p 1))
                                   (pb (svref p 2))
                                   (pc (svref p 3))
                                   (pd (svref p 4)))
                   (let ((qq (pushq q (\3f pb pc pd))))
                     (ft (\2f x pa) qq r)
                     ))
               (ft (pushq p x) q r))
       ))
    ))

(defmethod popq ((tree vector))
  (symbol-macrolet ((sel (svref tree 0))
                    (a   (svref tree 1))
                    (b   (svref tree 2))
                    (c   (svref tree 3))
                    (d   (svref tree 4))
                    (p   (svref tree 1))
                    (q   (svref tree 2))
                    (r   (svref tree 3)))
    (case sel
      ((\2f) (values a (\1f b)))
      ((\3f) (values a (\2f b c)))
      ((\4f) (values a (\3f b c d)))
      ((st)  (values a nil))
      ((ft)  (if (\1f? p)
                 (symbol-macrolet ((pa (svref p 1))
                                   (ra (svref r 1)))
                   (if q
                       (multiple-value-bind (pp qq) (popq q)
                         (values pa (ft pp qq r)))
                     ;; else null q
                     (if (\1f? r)
                         (values pa (st ra))
                       (multiple-value-bind (aa rr) (popq r)
                         (values pa (ft (\1f aa) nil rr))
                         ))
                     ))
               (multiple-value-bind (x pp) (popq p)
                 (values x (ft pp q r)))
               ))
      )))

(defmethod addq ((tree null) x)
  (st x))

(defmethod addq ((tree vector) x)
  (symbol-macrolet ((sel (svref tree 0))
                    (a   (svref tree 1))
                    (b   (svref tree 2))
                    (c   (svref tree 3))
                    (p   (svref tree 1))
                    (q   (svref tree 2))
                    (r   (svref tree 3)))
    (case sel
      ((\1f) (\2f a x))
      ((\2f) (\3f a b x))
      ((\3f) (\4f a b c x))
      ((st)  (ft (\1f a) nil (\1f x)))
      ((ft)  (if (\4f? r)
                 (symbol-macrolet ((ra (svref r 1))
                                   (rb (svref r 2))
                                   (rc (svref r 3))
                                   (rd (svref r 4)))
                   (let ((qq (addq q (\3f ra rb rc))))
                     (ft p qq (\2f rd x))
                     ))
               (ft p q (addq r x))
               ))
      )))

(defmethod getq ((tree vector))
  (symbol-macrolet ((sel (svref tree 0))
                    (a   (svref tree 1))
                    (b   (svref tree 2))
                    (c   (svref tree 3))
                    (d   (svref tree 4))
                    (p   (svref tree 1))
                    (q   (svref tree 2))
                    (r   (svref tree 3)))
    (case sel
      ((\2f) (values b (\1f a)))
      ((\3f) (values c (\2f a b)))
      ((\4f) (values d (\3f a b c)))
      ((st)  (values a nil))
      ((ft)  (if (\1f? r)
                 (symbol-macrolet ((pa (svref p 1))
                                   (ra (svref r 1)))
                   (if q
                       (multiple-value-bind (rr qq) (getq q)
                         (values ra (ft p qq rr)))
                     ;; else null q
                     (if (\1f? p)
                         (values ra (st pa))
                       (multiple-value-bind (aa pp) (getq p)
                         (values ra (ft pp nil (\1f aa)))
                         ))
                     ))
               (multiple-value-bind (x rr) (getq r)
                 (values x (ft p q rr)))
               ))
      )))

;; Unfortunately, while the Finger Tree represents amortized immutable
;; queue/stack behavior, it becomes expensive O(N) for counting
;; occupancy or retrieving whole contents.
;;
;; A Finger Tree can only be safely accessed from its top node, which
;; then gives assurance that all subnodes are proper elements of that
;; Finger Tree skeleton. But jumping into the niddle and analyzing its
;; contents becomes fraught with danger...
;;
;; What about when a Finger Tree queue contains independent Finger
;; trees as elements? Or worse, when a Finger Tree contains innocent
;; vectors of another kind. We cannot safely examine the elements of
;; finger vectors (\1F \2F \3F \4F)
;;
;; Those contents could either be further skeletal finger tree nodes,
;; or user vectors of arbitrary kind. There is no way to discern the
;; difference, except to know that if we entered from the top, then we
;; know when they should represent skeletal elements.

(defmethod cardinal ((tree null))
  0)

(defmethod cardinal ((tree vector))
  (um:nlet iter ((tree tree)
                 (ct   0))
    (if (null tree)
        ct
      (multiple-value-bind (x treex) (getq tree)
        (declare (ignore x))
        (go-iter treex (1+ ct)))
      )))
              
(defmethod elements ((tree null))
  nil)

(defmethod elements ((tree vector))
  (um:nlet iter ((tree tree)
                 (acc  nil))
    (if (null tree)
        acc
      (multiple-value-bind (x treex) (getq tree)
        (go-iter treex (cons x acc)))
      )))
     
;; ------------------------------------------------------
;; Sharable Mutable Tree - lock free
;;
;; The tree proper is never mutated, only the top level reference to
;; it.  Copies of the top level tree node pointer represent an
;; immutable persistent tree in the state that it had when it was
;; read.

(um:make-encapsulated-type SE SE? SD)

(defun make-shared-finger-tree ()
  (SE nil))

(defun rdq (ft)
  (um:rd (SD ft)))

(defmethod copy ((ft SE))
  (SE (rdq ft)))

(defmethod is-empty? ((ft SE))
  (null (rdq ft)))

(defmethod not-empty? ((ft SE))
  (rdq ft))

(defmethod writeq ((ft SE) writer-fn val)
  (um:rmw (SD ft) (lambda (tree)
                    (funcall writer-fn tree val))))

(defmethod pushq ((ft SE) x)
  (writeq ft #'pushq x))

(defmethod addq ((ft SE) x)
  (writeq ft #'addq x))

(defconstant +unique+ "unique")

(defmethod readq ((ft SE) reader-fn)
  (let (ans)
    (um:rmw (SD ft) (lambda (tree)
                     (setf ans +unique+)
                     (when tree
                       (multiple-value-bind (x treex)
                           (funcall reader-fn tree)
                         (setf ans x)
                         treex))))
    (if (eq ans +unique+)
        (values)
      (values ans t))))

(defmethod popq ((ft SE))
  (readq ft #'popq))

(defmethod getq ((ft SE))
  (readq ft #'getq))

(defmethod erase ((ft SE))
  (writeq ft (constantly nil) nil))

(defmethod cardinal ((ft SE))
  (cardinal (rdq ft)))

(defmethod elements ((ft SE))
  (elements (rdq ft)))

;; ---------------------------------------------------------
;; Unshared variant

(um:make-encapsulated-type UE UE? UD)

(defun make-unshared-finger-tree ()
  (UE nil))

(defmethod copy ((ft UE))
  (UE (UD ft)))

(defmethod is-empty? ((ft UE))
  (null (UD ft)))

(defmethod not-empty? ((ft UE))
  (UD ft))

(defmethod writeq ((ft UE) writer-fn val)
  (setf (UD ft) (funcall writer-fn (UD ft) val)))

(defmethod pushq ((ft UE) x)
  (writeq ft #'pushq x))

(defmethod addq ((ft UE) x)
  (writeq ft #'addq x))

(defmethod readq ((ft UE) reader-fn)
  (if (UD ft)
      (multiple-value-bind (x udp) (funcall reader-fn (UD ft))
        (setf (UD ft) udp)
        (values x t))
    (values)))

(defmethod popq ((ft UE))
  (readq ft #'popq))

(defmethod getq ((ft UE))
  (readq ft #'getq))

(defmethod erase ((ft UE))
  (setf (UD ft) nil))

(defmethod cardinal ((ft UE))
  (cardinal (UD ft)))

(defmethod elements ((ft UE))
  (elements (UD ft)))

;; --------------------------------------------
;; Encapsulation coercion

(defmethod copy-as-shared ((ft UE))
  (SE (UD ft)))

(defmethod copy-as-unshared ((ft UE))
  (copy ft))

(defmethod copy-as-shared ((ft SE))
  (copy ft))

(defmethod copy-as-unshared ((ft SE))
  (UE (rdq ft)))

;; ---------------------------------------------------------
