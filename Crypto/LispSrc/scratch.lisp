
(in-package :com.ral.crypto.lattice-key-exchange)

(let* ((msg (map 'vector #'char-code (symbol-name :this))))
  (β (pkey)
      (send lattice-pkey β (machine-instance))
    (β (ctxt)
        (send lat2-encoder β pkey msg)
      (β (dec)
          (send lat2-decoder β ctxt)
        (inspect (list ctxt dec))
        (send println dec)))))

(let* ((msg #(1 2 3 4))
       (vmsg (coerce msg '(simple-array (unsigned-byte 8) 1)))
       (vmsg2 (coerce vmsg '(simple-array (unsigned-byte 8) 1))))
  (eq vmsg vmsg2))