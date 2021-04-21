;;; ...because I could...

(defpackage #:mach-finger-tree
  (:use #:common-lisp)
  (:export
   ))

(in-package #:mach-finger-tree)

;; ----------------------------
;; Finger Types = 1 to 4 elements

(defun 1f (a)
  (um:dlambda
    (:pushq (x) (2f x a))
    (:addq (x)  (2f a x))
    (:1f? ()    t)
    (:4f? ()    nil)
    (:1fa ()    a)
    ))

(defun 2f (a b)
  (um:dlambda
    (:pushq (x) (3f x a b))
    (:addq (x)  (3f a b x))
    (:popq ()   (values a (1f b)))
    (:getq ()   (values b (1f a)))
    (:1f? ()    nil)
    (:4f? ()    nil)
    ))

(defun 3f (a b c)
  (um:dlambda
    (:pushq (x) (4f x a b c))
    (:addq (x)  (4f a b c x))
    (:popq ()   (values a (2f b c)))
    (:getq ()   (values c (2f a b)))
    (:1f? ()    nil)
    (:4f? ()    nil)
    ))

(defun 4f (a b c d)
  (um:dlambda
    (:popq ()    (values a (3f b c d)))
    (:getq ()    (values d (3f a b c)))
    (:split13 () (values (1f a) (3f b c d)))
    (:split31 () (values (3f a b c) (1f d)))
    (:1f? ()     nil)
    (:4f? ()     t)
    ))

(declaim (inline _1f? _4f? _popq _getq _addq _pushq
                 _split13 _split31 _1fa _empty? empty))

(defun _1f? (q)     (funcall q :1f?))
(defun _4f? (q)     (funcall q :4f?))
(defun _popq (q)    (funcall q :popq))
(defun _getq (q)    (funcall q :getq))
(defun _pushq (q x) (funcall q :pushq x))
(defun _addq (q x)  (funcall q :addq x))
(defun _split13 (q) (funcall q :split13))
(defun _split31 (q) (funcall q :split31))
(defun _1fa (q)     (funcall q :1fa))
(defun _empty? (q)  (funcall q :empty?))

;; ---------------------------------------------
;; Finger Tree Types - empty, 1 element, N elements

(defvar *empty*
  (um:dlambda
    (:pushq (x) (st x))
    (:addq (x)  (st x))
    (:empty? () t)
    ))

(defun empty () *empty*)

(defun st (a)
  (um:dlambda
    (:pushq (x) (ft (1f x) (empty) (1f a)))
    (:addq (x)  (ft (1f a) (empty) (1f x)))
    (:popq ()   (values a (empty)))
    (:getq ()   (values a (empty)))
    (:empty? () nil)
    ))

(defun ft (p q r)
  (um:dlambda
    (:pushq (x)
     (if (_4f? p)
         (multiple-value-bind (1f 3f) (_split13 p)
           (ft (_pushq 1f x) (_pushq q 3f) r))
       (ft (_pushq p x) q r)))
    (:addq (x)
     (if (_4f? r)
         (multiple-value-bind (3f 1f) (_split31 r)
           (ft p (_addq q 3f) (_addq 1f x)))
       (ft p q (_addq r x))))
    (:popq ()
     (if (_1f? p)
         (if (_empty? q)
             (if (_1f? r)
                 (values (_1fa p)
                         (st (_1fa r)))
               (multiple-value-bind (aa rr) (_popq r)
                 (values (_1fa p)
                         (ft (1f aa) q rr))
                 ))
           (multiple-value-bind (pp qq) (_popq q)
             (values (_1fa p)
                     (ft pp qq r))))
       (multiple-value-bind (x pp) (_popq p)
         (values x (ft pp q r)))
       ))
    (:getq ()
     (if (_1f? r)
         (if (_empty? q)
             (if (_1f? p)
                 (values (_1fa r)
                         (st (_1fa p)))
               (multiple-value-bind (aa pp) (_getq p)
                 (values (_1fa r)
                         (ft pp q (1f aa)))))
           (multiple-value-bind (rr qq) (_getq q)
             (values (_1fa r)
                     (ft p qq rr))))
       (multiple-value-bind (x rr) (_getq r)
         (values x (ft p q rr)))
       ))
    (:empty? () nil)
    ))

;; ---------------------------------------------------
;; Composite Trees - front & back pairs

(defun join (a b)
  (cond ((_empty? a) b)
        ((_empty? b) a)
        (t           (qpair a b))
        ))

(defun qpair (a b)
  (um:dlambda
    (:pushq (x) (qpair (_pushq a x) b))
    (:addq (x)  (qpair a (_addq b x)))
    (:popq ()
     (multiple-value-bind (x aa) (_popq a)
       (values x (join aa b))))
    (:getq ()
     (multiple-value-bind (x bb) (_getq b)
       (values x (join a bb))))
    (:empty? () nil)
    ))

;; ----------------------------
;; User API for Lock-Free Shared Finger-Tree Queues
;;
;; MAKE-SHARED-QUEUE, COPY, ADDQ, POPQ, PUSHQ, GETQ,
;; IS-EMPTY?, NOT-EMPTY?, APPENDQ, PREPENDQ, ERASE

(um:make-encapsulated-type SE SE? SD)

(defun make-shared-queue ()
  (SE (empty)))

     ;;; --------------------------- ;;; 

(defun rdq (q)
  (um:rd (SD q)))

(defmacro with-q ((tree q) &body body)
  `(let ((,tree (rdq ,q)))
     ,@body))

(defun rmwq (q fn)
  (um:rmw (SD q) fn))

(defmacro with-rmw ((tree q) &body body)
  `(rmwq ,q (lambda (,tree)
              ,@body)))

(defconstant +unique+ "unique")

     ;;; --------------------------- ;;; 

(defmethod pusher ((q SE) x push-kind)
  (with-rmw (tree q)
            (funcall tree push-kind x)))

(defmethod popper ((q SE) pop-kind)
  (let ((ans +unique+))
    (with-rmw (tree q)
              (if (_empty? tree)
                  tree
                (multiple-value-bind (x treex) (funcall tree pop-kind)
                  (setf ans x)
                  treex)))
    (if (eq ans +unique+)
        (values)
      (values ans t))
    ))

     ;;; --------------------------- ;;; 

(defmethod copy ((q SE))
  (SE (rdq q)))

(defmethod erase ((q SE))
  (with-rmw (tree q)
    (declare (ignore tree))
    (empty)))

(defun pushq (q x)  (pusher q x :pushq))
(defun addq (q x)   (pusher q x :addq))
(defun popq (q)     (popper q :popq))
(defun getq (q)     (popper q :getq))

(defmethod is-empty? ((q SE))
  (_empty? (rdq q)))

(defun not-empty? (q)
  (not (is-empty? q)))

;; --------------------------------------------------
;; Unshared Finger-Tree Queues

(um:make-encapsulated-type UE UE? UD)

(defun make-unshared-queue ()
  (UE (empty)))

(defmethod copy ((q UE))
  (UE (UD q)))

(defmethod erase ((q UE))
  (setf (UD q) (empty)))

(defmethod is-empty? ((q UE))
  (_empty? (UD q)))

     ;;; --------------------------- ;;; 

(defmethod pusher ((q UE) x push-kind)
  (setf (UD q) (funcall (UD q) push-kind x)))

(defmethod popper ((q UE) pop-kind)
  (unless (_empty? (UD q))
    (multiple-value-bind (x qx) (funcall (UD q) pop-kind)
      (setf (UD q) qx)
      (values x t))
    ))

     ;;; --------------------------- ;;; 

(defmethod appendq ((qfront SE) (qback UE))
  (with-rmw (qf qfront)
    (join qf (UD qback))))

(defmethod prependq ((qfront UE) (qback SE))
  (with-rmw (qb qback)
    (join (UD qfront) qb)))

;; -------------------------------------------------------           
 #|
 ;; This closure-based version runs about 10x slower than the vector based one in finger-tree.lisp.
 ;; Here, the shared queue runs about 4x slower than the unshared queues.
 ;;
(defun tst (&optional (n 1000000))
  (let ((q (make-shared-queue)))
    (dotimes (ix n)
      (addq q ix))
    (dotimes (ix n)
      (assert (= ix (popq q))))))

(time (tst))

 |#
