
(in-package :edec)

(vec (hash/256 *ed-gen*))
(vec (hash/256 (ed-mul *ed-gen* 1)))
(vec (hash/256 (ed-projective *ed-gen*)))
(vec (hash/256 (ed-compress-pt *ed-gen*)))
(vec (hash/256 (int *ed-gen*)))
(vec (hash/256 (hashable *ed-gen*)))

(hashable *ed-gen*)

(vec (int *ed-gen*))
(vec *ed-gen*)
(vec (ed-compress-pt *ed-gen*))

(defvar *k*  103283306528744408277108197247887855587947703691207848492777210052883788214928)

(with-standard-io-syntax
  (princ (ctr-drbg-int 256))
  (values))

(defun kmul (pt)
  (ed-mul pt *k*))

(defun make-pwd (seed)
  (let* ((h       (int (hash/256 seed)))
         (r       (ctr-drbg-int 256))
         (kloak   (ed-mul (ed-mul *ed-gen* h) r))
         (k*kloak (kmul kloak)))
    (int (hash/256 seed (ed-div k*kloak r)))))
         
(com.ral.crypto.core-crypto:convert-int-to-wordlist (make-pwd :hello))
(coerce (loop for w in (core-crypto:convert-int-to-wordlist (make-pwd :test)) collect (char w 0)) 'string)


