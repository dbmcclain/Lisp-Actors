
(in-package :core-crypto)

(defun hash-test (n)
  (let ((ans nil))
  (dotimes (ix n)
    (setf ans (hash:hash/256 (ed-nth-pt ix) ans)))
  ans))

(time (hash-test 100000))
