
(in-package :core-crypto-test)

(defun mod-inv-test-fn ()
  (with-mod *ed-r*
    (let* ((n    (field-random *ed-r*))
           (invn (minv n)))
      (= 1 (m* n invn)))))

(define-test mod-inv-test
  (assert-true (mod-inv-test-fn)))

(defun mod-exp-test-fn ()
  (with-mod *ed-r*
    (let* ((n       (field-random *ed-r*))
           (invn    (minv n))
           (nexpinv (m^ n (- *ed-r* 2))))
      (= invn nexpinv))))

(define-test mod-exp-test
  (assert-true (mod-exp-test-fn)))

(defun mod-sqrt-test-fn ()
  (with-mod *ed-r*
    (let* ((n (um:nlet-tail iter ((n (field-random *ed-r*)))
                (if (quadratic-residue-p n)
                    n
                  (iter (field-random *ed-r*)))))
           (nsqrt (msqrt n)))
      (= n (m* nsqrt nsqrt)))))

(define-test mod-sqrt-test
  (assert-true (mod-sqrt-test-fn)))

#|
(lisp-unit:run-tests :all :crypto/test)
|#
