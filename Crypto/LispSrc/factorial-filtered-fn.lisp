
(asdf:oos 'asdf:load-op :filtered-functions :force t)

(in-package :filtered-functions-user)

(defun sign (n)
  (cond ((< n 0) 'neg)
        ((= n 0) 'zero)
        ((> n 0) 'pos)))

(define-filtered-function fac (n)
  (:filters (:sign #'sign)))

(defmethod fac :filter :sign ((n (eql 'pos)))
  (* n (fac (1- n))))

(defmethod fac :filter :sign ((n (eql 'zero)))
  1)

(defmethod fac :filter :sign ((n (eql 'neg)))
  (error "Fac not defined for negative numbers."))

;; -------------------

(defgeneric %fac2 (sign n))

(defun fac2 (n)
  (%fac2 (signum n) n))

(defmethod %fac2 ((sign (eql -1)) n)
  (error "Fac2 not defined for negative numbers."))

(defmethod %fac2 ((sign (eql 0)) n)
  1)

(defmethod %fac2 ((sign (eql 1)) n)
  (* n (fac2 (1- n))))

;; -------------------

(defgeneric %fac3 (sign n ans))

(defun fac3 (n &optional (ans 1))
  (%fac3 (signum n) n ans))

(defmethod %fac3 ((sign (eql -1)) n ans)
  (error "Fac3 not defined for negative numbers."))

(defmethod %fac3 ((sign (eql 0)) n ans)
  ans)

(defmethod %fac3 ((sign (eql 1)) n ans)
  (fac3 (1- n) (* n ans)))

