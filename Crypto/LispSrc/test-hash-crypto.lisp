
(in-package :edec)

(defvar *msg*  (hcl:file-string "Crypto/ed-curves.lisp"))

(defun hcrypt (seed msg)
  (let* ((key   (vec (hash/256 seed)))
         (nel   (length msg))
         (ans   (make-ub8-vector nel)))
    (map-into ans #'char-code msg)
    (loop for pos from 0 below nel by 32 do
          (setf key (vec (hash/256 key)))
          (let ((sub  (subseq ans pos (min nel (+ pos 32)))))
            (replace ans (map 'vector #'logxor key sub) :start1 pos)
            ))
    ans))

(defun acrypt (seed msg)
  (let* ((key   (vec (hash/256 seed)))
         (crypt (ironclad:make-cipher :aes
                                      :key  key
                                      :mode :ctr
                                      :initialization-vector
                                      (subseq (vec (hash/256 key)) 0 16)
                                      ))
         (nel   (length msg))
         (ans   (make-ub8-vector nel)))
    (map-into ans #'char-code msg)
    (ironclad:encrypt-in-place crypt ans)
    ans))

(defun tcrypt (seed msg)
  (let* ((key   (vec (hash/256 seed)))
         (crypt (ironclad:make-cipher :twofish
                                      :key  key
                                      :mode :ctr
                                      :initialization-vector
                                      (subseq (vec (hash/256 key)) 0 16)
                                      ))
         (nel   (length msg))
         (ans   (make-ub8-vector nel)))
    (map-into ans #'char-code msg)
    (ironclad:encrypt-in-place crypt ans)
    ans))

(defun ccrypt (seed msg)
  (let* ((key   (vec (hash/256 seed)))
         (crypt (ironclad:make-cipher :xchacha
                                      :key  key
                                      :mode :stream
                                      :initialization-vector
                                      (subseq (vec (hash/256 key)) 0 24)
                                      ))
         (nel   (length msg))
         (ans   (make-ub8-vector nel)))
    (map-into ans #'char-code msg)
    (ironclad:encrypt-in-place crypt ans)
    ans))

#|                 
(time (loop repeat 100 do (hcrypt :test *msg*)))  ;; 8.5s
(time (loop repeat 100 do (acrypt :test *msg*)))  ;; 0.38s, 24x faster
(time (loop repeat 100 do (tcrypt :test *msg*)))  ;; 0.38s - same as AES
(time (loop repeat 100 do (ccrypt :test *msg*)))  ;; 0.73s - 2x slower than AES
(hcrypt :test *msg*)
(acrypt :test *msg*)
|#

