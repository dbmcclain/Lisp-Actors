;; c-sha2.lisp - Interface to C-level SHA2 for speedup
;;
;; DM/RAL 10/22
;; ------------------------------------------------------

;; ------------------------------------------------------------------

(defun return-digest (dig)
  (let ((lisp-dig (make-ub-array 32)))
    (fli:replace-foreign-array lisp-dig dig
                               :start2 0 :end2 32)
    lisp-dig))

(defun c-sha2-file (fname)
    (fli:with-dynamic-foreign-objects ()
      (let ((dig (fli:allocate-dynamic-foreign-object
                  :type :uint8 :nelems 32)))
        (sha2_file (namestring fname) dig)
        (return-digest dig)) ))

(defun update-digest-from-buffer (ctx dig buf diglen)
  (let* ((len (length buf)))
    (loop for pos from 0 below len by diglen do
          (let ((nb (min diglen (- len pos))))
            (fli:replace-foreign-array dig buf
                                       :start1 0
                                       :end1   nb
                                       :start2 pos
                                       :end2   (+ pos nb))
            (sha2_update ctx dig nb))) ))

(defun c-sha2-buffers (&rest buffers)
  (fli:with-dynamic-foreign-objects ((ctx sha2_context))
    (let ((dig (fli:allocate-dynamic-foreign-object
                :type :uint8  :nelems 32)))
      (sha2_starts ctx)
      (dolist (buf buffers)
        (update-digest-from-buffer ctx dig buf 32))
      (sha2_finish ctx dig)
      (return-digest dig) )))

(defun c-sha2-buffers-into (ans &rest buffers)
  (fli:with-dynamic-foreign-objects ((ctx sha2_context))
    (let ((dig (fli:allocate-dynamic-foreign-object
                :type :uint8  :nelems 32)))
      (sha2_starts ctx)
      (dolist (buf buffers)
        (update-digest-from-buffer ctx dig buf 32))
      (sha2_finish ctx dig)
      (fli:replace-foreign-array ans dig
                                 :start1 0
                                 :end1   32
                                 :start2 0
                                 :end2   32)
      ans)))

;; Seeing about a 7x speedup going to C
(defun c-kdf (nbits &rest keys)
  (let* ((nbytes (ceiling nbits 8))
         (ans    (make-ub-array nbytes
                                :initial-element 0))
         (ctr    0)
         (keys   (mapcar #'ensure-8bitv (um:flatten keys)))
         (hash   (make-ub-array 32
                                :initial-element 0))
         (rembits (rem nbits 8)))
    (labels ((gen-partial (start end)
               (incf ctr)
               (let ((num (ensure-8bitv
                           (convert-int-to-nbytes ctr 4))))
                 (loop repeat 8192 do
                       (apply #'c-sha2-buffers-into hash num hash keys))
                 (replace ans hash :start1 start :end1 end) )))

      (loop for start from 0 below nbytes by 32 do
            (let ((nb (min 32 (- nbytes start))))
              (gen-partial start (+ start nb))))
      (mask-off ans rembits) )))

