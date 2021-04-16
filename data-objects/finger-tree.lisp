
(defpackage #:finger-tree
  (:use :common-lisp)
  (:export
   #:pushq
   #:popq
   #:addq
   #:getq
   ))

(in-package #:finger-tree)

;; --------------------------------------
#|
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


;; ------------------------------------------------------

