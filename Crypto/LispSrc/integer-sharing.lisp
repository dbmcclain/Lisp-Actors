 

(defun prep-interpolation-shares (shares)
  (flet ((prep (share_i)
           (destructuring-bind (xi . yi) share_i
             (flet ((denom (prod share_j)
                      (if (eq share_i share_j)
                          prod
                        (* prod (- xi (car share_j))))
                      ))
               (cons xi (/ yi (reduce #'denom shares
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
                          (* prod (- x (car prep_j))))
                        ))
                 (+ sum (reduce #'factor preps
                                 :initial-value (cdr prep_i))))))
        (reduce #'term preps
                :initial-value 0)
        ))
    ))

#|
(Let* ((k  (uuid:make-uuid-from-string "{bbe37564-f7b1-11ea-82f8-787b8acbe32e}"))
       (y0 (uuid:uuid-to-integer k))
       (share0 `(0 . ,y0))
       (share1 `(1 . ,(random #.(ash 1 128))))
       (share2 `(2 . ,(random #.(ash 1 128))))
       (fn (make-lagrange-interpolator `(,share0 ,share1 ,share2)))
       (share3 `(3 . ,(funcall fn 3)))
       (fn (make-lagrange-interpolator `(,share1 ,share2 ,share3)))
       (kx (uuid:uuid-string (uuid:integer-to-uuid (funcall fn 0)))))       
  (assert (= y0 (funcall fn 0)))       
  (list share1 share2 share3 kx))
|#

(defun share-uuid (uuid-str)
  (let* ((k  (uuid:uuid-to-integer (uuid:make-uuid-from-string uuid-str)))
         (share1 (random #.(ash 1 128)))
         (share2 (random #.(ash 1 128)))
         (fn     (make-lagrange-interpolator `((0 . ,k) (1 . ,share1) (2 . ,share2))))
         (share3 (funcall fn 3)))
    (list share1 share2 share3)))

(defun uuid-str-from-shares (share1 share2 share3)
  (let ((fn (make-lagrange-interpolator `((1 . ,share1) (2 . ,share2) (3 . ,share3)))))
    (uuid:uuid-string (uuid:integer-to-uuid (funcall fn 0)))))

(setf *print-readably* t)
(share-uuid "{cfe31464-f7b1-11ea-82f8-787b8acbe32e}"))
(apply #'uuid-str-from-shares '(308992582801423990496267172175099217430
                                101134521684481366089366216077669023182
                                -599757286114692367233367102794433148330) )      
