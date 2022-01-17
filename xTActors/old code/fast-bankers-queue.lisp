
non-pure functional - the queue is mutated, but the contents are not.

(defun make-queue ()
  (list nil))

(defun qnorm (cell)
  #F
  (if (car cell)
      cell
    (when (cdr cell)
      (list (reverse (cdr (the cons cell)))))
    ))

(defun popq (queue)
  #F
  (declare (cons queue))
  (let ((cell (car queue)))
    (when cell
      (let ((val (caar (the cons cell))))
        (setf (car queue) (qnorm (cons (cdar (the cons cell)) (cdr (the cons cell)))))
        val))))

(defun addq (queue elt)
  #F
  (declare (cons queue))
  (let ((cell (car queue)))
    (setf (car queue) (qnorm (cons (car cell)
                                   (cons elt (cdr cell))
                                   ))
          )))

(defun appendq (qhd qtl)
  #F
  (when (car qtl)
    (setf (car qhd)
          (if (car qhd)
              (cons (caar (the cons qhd))
                    (append (cdar (the cons qtl))
                            (reverse (caar (the cons qtl)))
                            (cdar (the cons qhd))))
            (car (the cons qtl)))
          )))

#|
(let ((x (make-queue)))
  (addq x 15)
  (addq x 32)
  (appendq x (copy-seq x))
  (send println (popq x))
  (send println (popq x))
  (send println (popq x))
  (send println (popq x))
  (send println (popq x)))
 |#

;; -----------------------------------------------------------------
;; Pure FP Banker's Queue

(defconstant q-empty (list nil))

(defun q-norm (q)
  (cond ((null (car q)) (list (reverse (cdr q))))
        (t  q)
        ))

(defun q-put (q x)
  (q-norm (cons (car q) (cons x (cdr q)))))

(defconstant done  #())

(defun q-take (q)
  (cond ((null (car q)) done)
        (t
         (values (caar q)
                 (q-norm (cons (cdar q) (cdr q)))))
        ))

(defun q-iter (q fn)
  (um:nlet iter ((q  q))
    (multiple-value-bind (q-top q-new) (q-take q)
      (unless (eq q-top done)
        (funcall fn q-top)
        (go-iter q-new))
      )))

(let ((q  (q-put (q-put (q-put nil 1) 2) 3)))
  (q-iter q 'print))