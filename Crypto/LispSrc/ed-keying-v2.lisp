
(in-package :edec-ff)

;; -----------------------------------------------
;; Public/Private Key pair generation, splitting, reassembly

(defun gen-shares (skey pkey nshares)
  (with-curve-field
    (let* ((xs     (loop repeat nshares collect (random-between 1 (field-base))))
           (coffs  (cons skey (loop repeat (1- nshares) collect (random-between 0 (field-base)))))
           (shares (loop for x in xs collect
                           (cons x (int
                                    (reduce (lambda (c sum)
                                              (ff+ c (ff* x sum)))
                                            coffs
                                            :from-end t
                                            :initial-value 0)))
                         )))
      (values pkey
              shares))))

(defun gen-key-pair (&optional (nshares 3))
  (multiple-value-bind (skey pkey)
      (ed-random-pair)
    (gen-shares skey pkey nshares)))

(defun reduce-factors (x pair_i pairs init)
  ;; -> init * (x - x1) * (x - x2) * ... * (x - x_i-1) * (x - x_i+1) * ... * (x - x_n)
  ;; i.e., Prod((x - x_j) for j = 1..N, j /= i)
  ;; pairs are (x . y) coords
  (with-curve-field
    (flet ((factor (prod pair_j)
             (if (eq pair_i pair_j)
                 prod
               (ff* prod (ff- x (car pair_j))))
             ))
      (int
       (reduce #'factor pairs
               :initial-value init)))))

(defun prep-interpolation-shares (shares)
  ;; Map (x_i . y_i) -> (x_i . y_i / Prod((x_i - x_j), j = 1..N, j /= i))
  (with-curve-field
    (flet ((prep (share_i)
             (destructuring-bind (xi . yi) share_i
               (cons xi (int (ff/ yi (reduce-factors xi share_i shares 1))))
               )))
      (mapcar #'prep shares))))

(defun make-lagrange-interpolator (shares)
  ;; F(x) = Sum((x_i, y_i) -> y_i * Prod((x - x_j), j = 1 .. N, j /= i)
  ;;                              / Prod((x_i - x_j), j = 1..N, j /= i),
  ;;             i = 1..N)
  (with-curve-field
    (let ((preps (prep-interpolation-shares shares)))
      (lambda (x)
        (flet ((term (sum prep_i)
                 (ff+ sum (reduce-factors x prep_i preps (cdr prep_i)))
                 ))
          (int
           (reduce #'term preps
                   :initial-value 0))
          )))))

(defun solve-shares (x shares)
  (let ((fn (make-lagrange-interpolator shares)))
    (funcall fn x)))

#|
(defun solve-shares (x shares)
  (with-curve-field
    (flet ((term (sum share_i)
             (flet ((lprod (xk)
                      (flet ((factor (prod share_j)
                               (if (eq share_i share_j)
                                   prod
                                 (ff* prod (ff- xk (car share_j))))
                               ))
                        (reduce #'factor shares
                                :initial-value 1)
                        )))
               (destructuring-bind (xi yi) share_i
                 (ff+ sum
                     (ff* yi (ff/ (lprod x)
                                  (lprod xi))
                          ))
                 ))))
      (int
       (reduce #'term shares
               :initial-value 0))
      )))
|#

(defun skey (&rest shares)
  (with-curve-field
    (solve-shares 0 shares)))

