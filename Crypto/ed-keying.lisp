
(in-package :edec)

;; -----------------------------------------------
;; Public/Private Key pair genberation, splittingk, reassembly

(defun gen-key-pair (&optional (nshares 3))
  (multiple-value-bind (skey pkey)
      (ed-random-pair)
    (let* ((xs     (loop repeat nshares collect (random-between 1 *ed-r*)))
           (coffs  (cons skey (loop repeat (1- nshares) collect (random-between 0 *ed-r*))))
           (shares (loop for x in xs collect
                         (cons x (with-mod *ed-r*
                                   (reduce (lambda (c sum)
                                             (m+ c (m* x sum)))
                                           coffs
                                           :from-end t
                                           :initial-value 0)))
                         )))
      (values pkey
              shares))))

(defun prep-interpolation-shares (shares)
  (flet ((prep (share_i)
           (destructuring-bind (xi . yi) share_i
             (flet ((denom (prod share_j)
                      (if (eq share_i share_j)
                          prod
                        (m* prod (m- xi (car share_j))))
                      ))
               (cons xi (m/ yi (reduce #'denom shares
                                       :initial-value 1)))
               ))))
    (mapcar #'prep shares)))

(defun make-lagrange-interpolator (shares)
  (let ((preps (prep-interpolation-shares shares)))
    (lambda (x)
      (flet ((term (sum prep_i)
               (flet ((factor (prod prep_j)
                        (if (eq prep_i prep_j)
                            prod
                          (m* prod (m- x (car prep_j))))
                        ))
                 (m+ sum (reduce #'factor preps
                                 :initial-value (cdr prep_i))))))
        (reduce #'term preps
                :initial-value 0)
        ))
    ))

(defun solve-shares (x shares)
  (let ((fn (make-lagrange-interpolator shares)))
    (funcall fn x)))
#|
(defun solve-shares (x shares)
  (flet ((term (sum share_i)
           (flet ((lprod (xk)
                    (flet ((factor (prod share_j)
                             (if (eq share_i share_j)
                                 prod
                               (m* prod (m- xk (car share_j))))
                             ))
                      (reduce #'factor shares
                              :initial-value 1)
                      )))
             (destructuring-bind (xi yi) share_i
               (m+ sum
                   (m* yi (m/ (lprod x)
                              (lprod xi))
                       ))
               ))))
    (reduce #'term shares
            :initial-value 0)
    ))
|#

(defun skey (&rest shares)
  (with-mod *ed-r*
    (solve-shares 0 shares)))

