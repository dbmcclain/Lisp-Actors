;; pprimes.lisp -- pseudoprime searching
;;
;; DM/RAL 02/18
;; ------------------------------------------------------------------

(in-package :primes)

(defun find-pprime (nbits)
  (let* ((limit> (ash 1 nbits))
         (limit< (ash limit> -1)))
    (um:nlet iter ()
      (let ((x (random-between limit< limit>)))
        (if (= 1 (expt-mod 2 (1- x) x))
            x
          (go-iter))))
    ))