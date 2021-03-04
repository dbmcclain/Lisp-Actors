;; gf-128.lisp -- Galois Polynomial Field of order 571
;; DM/Acudora  02/12
;; ----------------------------------------------------

(in-package :ecc-crypto-b128)

;; handy output defn
(defun hex (x)
  (let ((*print-length* nil))
    (write x :base 16)))

;;------------------------------------------------
;; ECC over the Field F(2^128)
;; based on underlying polynomial field GF(2^128)

(defun gf+ (&rest args)
  (apply #'logxor args))

(defun gf- (&rest args)
  (apply #'gf+ args))

(defvar *nbits* 128)

;; (defvar *gf-order* (1- (ash 1 *nbits*)))

(defvar $prim-128
  ;; B128: t^128 + t^7 + t^2 + t^1 + 1
  (logior (ash 1   *nbits*)
          (ash 1   7)
          (ash 1   2)
          (ash 1   1)
          1))

#|
 ;; for testing
(defun make-gf8 (prim)
  (setf *nbits* 8
        $prim   prim
        *gf-order* (1- (ash 1 8))
        *gf-inv-order* (1- *gf-order*)))
|#

(defun step-state (state)
  (let ((new-state (ash state 1)))
    (if (logbitp *nbits* new-state)
        (logxor new-state $prim)
      new-state)))

#|
(defun gf* (a b)
  (do ((ans  0)
       (x    a   (step-state x))
       (limit (integer-length b))
       (mask 0   (1+ mask)))
      ((>= mask limit) ans)
    (when (logbitp mask b)
      (setf ans (logxor ans x))) ))
|#

(defun gf* (a b)
  (if (fboundp 'c-gf-mul)
      (funcall 'c-gf-mul a b)
    (progn
      ;; right-to-left
      (when (< a b)  ;; get smaller arg in b
        (rotatef a b))
      (cond ((zerop b) 0)
            ((= 1 b)   a)
            (t  (do ((ans  a)
                     (mask (- (integer-length b) 2) (1- mask)))
                    ((minusp mask) ans)
                  (setf ans (step-state ans))
                  (when (logbitp mask b)
                    (setf ans (logxor ans a)))) )
            ))))
      
#|
(defun gf^ (x n)
  (labels ((expt (n)
             (do ((ans   1)
                  (limit (integer-length n))
                  (expon 0  (1+ expon))
                  (mul   x  (gf* mul mul)))
                 ((>= expon limit) ans)
               (when (logbitp expon n)
                 (setf ans (gf* ans mul))))))
    (if (minusp n)
        (gfinv (expt (- n)))
      (expt n))))
|#

(defun gf^ (x n)
  ;; right-to-left
  (labels ((expt (n)
             (cond ((zerop n) 1)
                   ((= 1 n)   x)
                   (t  (do ((ans   x)
                            (mask (- (integer-length n) 2) (1- mask)))
                           ((minusp mask) ans)
                         (setf ans (gf* ans ans))
                         (when (logbitp mask n)
                           (setf ans (gf* ans x))) ))
                   )))
    (if (minusp n)
        (gfinv (expt (- n)))
      (expt n))))


(defun gfdeg (x)
  (1- (integer-length x)))

(defun gfinv (x)
  ;; extended Euclidean algorithm
  (if (fboundp 'c-gf-inv)
      (funcall 'c-gf-inv x)
    (let* ((u  x)
           (v  $prim)
           (g1 1)
           (g2 0))
      (loop until (= u 1) do
            (when (zerop u)
              (error "Zero has no inverse"))
            (let ((j (- (integer-length u) (integer-length v))))
              (when (minusp j)
                (rotatef u  v)
                (rotatef g1 g2)
                (setf j (- j)))
              (setf u  (gf+ u  (ash v  j))
                    g1 (gf+ g1 (ash g2 j)))))
      g1)))

(defun gfmod (x)
  ;; produce x mod f(z)
  (gfinv (gfinv x)))

#|
(defun gfinv (x)
  ;; by Fermat's little theorem
  ;; x^-1 = (gf^ x (1- *gf-order*))
  (when (zerop x)
    (error "Zero has no inverse"))
  (if (= 1 x)
      1
    (let ((ans  1))
      (loop repeat (1- *nbits*) do
            (setf x   (gf* x x)
                  ans (gf* ans x)))
      ans)))
|#

(defun gf/ (a b)
  (if (fboundp 'c-gf-div)
      (funcall 'c-gf-div a b)
    (gf* a (gfinv b))))

(defun gf^2 (x)
  (gf* x x))

;; -----------------------------------------------------------------------------
;;

(defun make-gf-lagrange-interpolator (shares)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (gf* prod (gf+ x2 x)))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum share)
                 (destructuring-bind (x y) share
                   (let ((xs (mapcar #'first (remove share shares))))
                     (gf+ sum
                          (gf* y (gf/ (lprod x0 xs)
                                      (lprod x xs)) )) ))) )
        (reduce #'term shares
                :initial-value 0))) ))

(defun solve-gf-lagrange (x0 &rest shares)
  (let ((fn (make-gf-lagrange-interpolator shares)))
    (funcall fn x0)))

