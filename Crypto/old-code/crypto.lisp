
(in-package :ecc-crypto-b571)

(fli:disconnect-module :cryptolib)
(fli:register-module :cryptolib
                     :real-name
                     (translate-logical-pathname
                      #+:LISPWORKS-32BIT "PROJECTS:DYLIB;libLispCrypto-32.dylib"
                      #+:LISPWORKS-64BIT "PROJECTS:DYLIB64;libLispCrypto-64.dylib"))

(fli:define-c-struct sha2_context
  (total  (:foreign-array :uint32  (2)))
  (state  (:foreign-array :uint32  (8)))
  (buffer (:foreign-array :uint8  (64)))
  (ipad   (:foreign-array :uint8  (64)))
  (opad   (:foreign-array :uint8  (64)))
  (is224  :int))

(fli:define-foreign-function (sha2_starts "sha2_starts" :source)
    ((ctx  (:pointer sha2_context))
     (:constant 0 :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2_update "sha2_update" :source)
    ((ctx   (:pointer sha2_context))
     (input (:pointer :uint8))
     (ilen  :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2_finish "sha2_finish" :source)
    ((ctx    (:pointer sha2_context))
     (output (:pointer :uint8))) ;; should point to 32 bytes
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2 "sha2" :source)
    ((ctx    (:pointer sha2_context))
     (ilen   :int)
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2_file "sha2_file" :source)
    ((path   (:reference-pass (:ef-mb-string :limit 256)))
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :int
  :language :ansi-c
  :module   :cryptolib)

(fli:define-foreign-function (shad2_file "shad2_file" :source)
    ((path   (:reference-pass (:ef-mb-string :limit 256)))
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :int
  :language :ansi-c
  :module   :cryptolib)

(fli:define-foreign-function (sha2_self_test "sha2_self_test" :source)
    ((verbose :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

;; ------------------------------------------------------------------

(fli:define-c-struct aes_context
  (nr     :int)
  (rk     (:pointer :uint32))
  (buf    (:foreign-array :uint32 (68))))

(fli:define-foreign-function (aes_setkey_enc "aes_setkey_enc" :source)
    ((ctx  (:pointer aes_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (aes_setkey_dec "aes_setkey_dec" :source)
    ((ctx  (:pointer aes_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aes_crypt_cbc "aes_crypt_cbc" :source)
    ((ctx     (:pointer aes_context))
     (mode    :int)
     (length  :int)
     (iv      (:pointer :uint8)) ;; should point to 16 bytes
     (input   (:pointer :uint8))
     (output  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aes_self_test "aes_self_test" :source)
    ((verbose :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

;; ------------------------------------------------------------------

(fli:define-c-struct aesx_context
  (nr     :int)
  (rk     (:pointer :uint32))
  (buf    (:foreign-array :uint32 (128))))

(fli:define-foreign-function (aesx_setkey_enc "aesx_setkey_enc" :source)
    ((ctx  (:pointer aesx_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (aesx_setkey_dec "aesx_setkey_dec" :source)
    ((ctx  (:pointer aesx_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aesx_crypt_cbc "aesx_crypt_cbc" :source)
    ((ctx     (:pointer aesx_context))
     (mode    :int)
     (length  :int)
     (iv      (:pointer :uint8)) ;; should point to 16 bytes
     (input   (:pointer :uint8))
     (output  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aesx_self_test "aesx_self_test" :source)
    ((verbose :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

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
            ;;  (transfer-lisp-bytes-to-c-array buf dig :start pos :nb nb)
            (sha2_update ctx dig nb))) ))

#|
(defun transfer-lisp-bytes-to-c-array (src cdst &key start nb)
  (loop for ix from 0 below nb
        for jx from start
        do
        (setf (fli:dereference cdst :index ix)
              (aref src jx)) ))
|#

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

#|
;; compare against Ironclad
(let* ((str "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
       #|
       (vec (let ((vec (make-array (length str)
                                   :element-type '(unsigned-byte 8))))
              (loop for ix from 0 below (length str) do
                    (setf (aref vec ix) (char-code (char str ix))))
              vec))
       |#
       (vec  (map '(ub-vector *) #'char-code str))
       (ans1 (c-sha2-buffers vec))
       (ans2 (let ((dig (ironclad:make-digest :sha256)))
               (ironclad:update-digest dig vec)
               (ironclad:produce-digest dig))))
  (terpri) (hex ans1)
  (terpri) (hex ans2)
  (equalp ans1 ans2))

|#

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

#|
(time (dotimes (ix 10)
        (kdf 571 (ecc-pt-x *ecc-acudora-public-key*) (ecc-pt-y *ecc-acudora-public-key*))))
(time (dotimes (ix 10)
        (c-kdf 571 (ecc-pt-x *ecc-acudora-public-key*) (ecc-pt-y *ecc-acudora-public-key*))))
|#

;; ----------------------------------------------------------------

(fli:define-foreign-function (gf128_add "gf128_add" :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf128_mul "gf128_mul" :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf128_div "gf128_div" :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf128_inv "gf128_inv" :source)
    ((op    (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

(defun convert-gf128-int-to-cbuf (val cbuf)
  (setf (fli:dereference cbuf :index 1 :type :uint64) (ldb (byte 64  0) val)
        (fli:dereference cbuf :index 0 :type :uint64) (ldb (byte 64 64) val)) )

(defun convert-cbuf-to-gf128-int (cbuf)
  (let ((val 0))
    (setf (ldb (byte 64 64) val) (fli:dereference cbuf :index 0 :type :uint64)
          (ldb (byte 64  0) val) (fli:dereference cbuf :index 1 :type :uint64))
    val))

(defun gf128-binop (cfn a b)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd-a (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 16))
          (opnd-b (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 16))
          (opnd-dst (fli:allocate-dynamic-foreign-object
                   :type :uint8 :nelems 72)))
      (convert-gf128-int-to-cbuf a opnd-a)
      (convert-gf128-int-to-cbuf b opnd-b)
      (funcall cfn opnd-a opnd-b opnd-dst)
      (convert-cbuf-to-gf128-int opnd-dst))))

(defun c-gf128-add (a b)
  (gf128-binop #'gf128_add a b))

(defun c-gf128-mul (a b)
  (gf128-binop #'gf128_mul a b))

(defun c-gf128-div (a b)
  (gf128-binop #'gf128_div a b))

(defun gf128-unop (cfn x)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 16))
          (opnd-dst (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 16)))
      (convert-gf128-int-to-cbuf x opnd)
      (funcall cfn opnd opnd-dst)
      (convert-cbuf-to-gf128-int opnd-dst))))

(defun c-gf128-inv (x)
  (gf128-unop #'gf128_inv x))

;; ----------------------------------------------------------------

(fli:define-foreign-function (gf571_add "gf571_add" :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf571_mul "gf571_mul" :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf571_div "gf571_div" :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf571_inv "gf571_inv" :source)
    ((op    (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

#|
(defun convert-gf571-int-to-cbuf (val cbuf)
  (loop for ix from 71 downto 0
        for jx from 0 by 8
        do
        (setf (fli:dereference cbuf :index ix)
              (ldb (byte 8 jx) val)) ))

(defun convert-cbuf-to-gf571-int (cbuf)
  (let ((val 0))
    (loop for ix from 0 below 72
          for jx from (* 71 8) by -8
          do
          (setf (ldb (byte 8 jx) val) (fli:dereference cbuf :index ix)) )
    val))
|#
#|
(defun convert-gf571-int-to-cbuf (val cbuf)
  (loop for ix from 71 downto 0
        for jx from 0 by 8
        do
        (setf (fli:dereference cbuf :index (logxor ix 7))
              (ldb (byte 8 jx) val)) ))

(defun convert-cbuf-to-gf571-int (cbuf)
  (let ((val 0))
    (loop for ix from 0 below 72
          for jx from (* 71 8) by -8
          do
          (setf (ldb (byte 8 jx) val) (fli:dereference cbuf :index (logxor ix 7))) )
    val))
|#
;; -------------------------------------------------------------------------------

(defun convert-gf571-int-to-cbuf (val cbuf)
  (loop for ix from 8 downto 0
        for jx from 0 by 64
        do
        (setf (fli:dereference cbuf :index ix :type :uint64)
              (ldb (byte 64 jx) val)) ))

(defun convert-cbuf-to-gf571-int (cbuf)
  (let ((val 0))
    (loop for ix from 0 below 9
          for jx from (* 8 64) by -64
          do
          (setf (ldb (byte 64 jx) val) (fli:dereference cbuf :index ix :type :uint64)))
    val))

#|
(list *ecc-acudora-private-key*
      (fli:with-dynamic-foreign-objects ()
        (let ((cbuf (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
          (convert-gf571-int-to-cbuf *ecc-acudora-private-key* cbuf)
          (convert-cbuf-to-gf571-int cbuf))))
|#

;; -------------------------------------------------------------------------------

(defun gf571-binop (cfn a b)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd-a (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72))
          (opnd-b (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72))
          (opnd-dst (fli:allocate-dynamic-foreign-object
                   :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf a opnd-a)
      (convert-gf571-int-to-cbuf b opnd-b)
      (funcall cfn opnd-a opnd-b opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(defun c-gf571-add (a b)
  (gf571-binop #'gf571_add a b))

(defun c-gf571-mul (a b)
  (gf571-binop #'gf571_mul a b))

(defun c-gf571-div (a b)
  (gf571-binop #'gf571_div a b))

(defun gf571-unop (cfn x)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72))
          (opnd-dst (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (funcall cfn opnd opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(defun c-gf571-inv (x)
  (gf571-unop #'gf571_inv x))

#|
(gfinv *ecc-acudora-private-key*)
(gf571-inv *ecc-acudora-private-key*)
(time (dotimes (ix 1000)
        ;; about 2900 per sec
        (gfinv *ecc-acudora-private-key*)))
(time (dotimes (ix 1000)
        ;; about 9900 per sec
        (gf571-inv *ecc-acudora-private-key*)))
|#

#|
(fli:define-foreign-function (gf571_integer_length "gf571_integer_length" :source)
    ((opnd  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module   :cryptolib)

(defun gf571-intlen (x)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (gf571_integer_length opnd))))
|#
#|
(fli:define-foreign-function (gf571_shiftl "gf571_shiftl" :source)
    ((opnd  (:pointer :uint8))
     (nsh   :int)
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

(defun gf571-shiftl (x n)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72))
          (opnd-dst (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (gf571_shiftl opnd n opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(fli:define-foreign-function (gf571_prim "gf571_prim" :source)
    ((opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

(defun gf571-prim ()
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd-dst (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
      (gf571_prim opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(fli:define-foreign-function (gf571_is_one "gf571_is_one" :source)
    ((opnd  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(defun gf571-is-one (x)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (gf571_is_one opnd))))

(fli:define-foreign-function (gf571_sizeof_opnd "gf571_sizeof_opnd" :source)
    ()
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
|#

;; -----------------------------------------------------------------------

(fli:define-foreign-function (c_ecc571_add "c_ecc571_add" :source)
    ((x1   (:pointer :uint8))
     (y1   (:pointer :uint8))
     (x2   (:pointer :uint8))
     (y2   (:pointer :uint8))
     (xdst (:pointer :uint8))
     (ydst (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (c_ecc571_sub "c_ecc571_sub" :source)
    ((x1   (:pointer :uint8))
     (y1   (:pointer :uint8))
     (x2   (:pointer :uint8))
     (y2   (:pointer :uint8))
     (xdst (:pointer :uint8))
     (ydst (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(defun c-ecc571-binop (fn apt bpt)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd1-x (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72))
          (opnd1-y (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72))
          (opnd2-x (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72))
          (opnd2-y (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf (car apt) opnd1-x)
      (convert-gf571-int-to-cbuf (cadr apt) opnd1-y)
      (convert-gf571-int-to-cbuf (car bpt) opnd2-x)
      (convert-gf571-int-to-cbuf (cadr bpt) opnd2-y)
      (funcall fn opnd1-x opnd1-y opnd2-x opnd2-y opnd1-x opnd1-y)
      (list (convert-cbuf-to-gf571-int opnd1-x)
            (convert-cbuf-to-gf571-int opnd1-y)))))

(defun c-ecc571-add (apt bpt)
  (c-ecc571-binop #'c_ecc571_add apt bpt))

(defun c-ecc571-sub (apt bpt)
  (c-ecc571-binop #'c_ecc571_sub apt bpt))


(fli:define-foreign-function (c_ecc571_mul "c_ecc571_mul" :source)
    ((x1   (:pointer :uint8))
     (y1   (:pointer :uint8))
     (n    (:pointer :uint8))
     (xdst (:pointer :uint8))
     (ydst (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(defun c-ecc571-mul (apt n)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd1-x (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72))
          (opnd1-y (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72))
          (opnd2   (fli:allocate-dynamic-foreign-object
                    :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf (car apt) opnd1-x)
      (convert-gf571-int-to-cbuf (cadr apt) opnd1-y)
      (convert-gf571-int-to-cbuf n opnd2)
      (c_ecc571_mul opnd1-x opnd1-y opnd2 opnd1-x opnd1-y)
      (list (convert-cbuf-to-gf571-int opnd1-x)
            (convert-cbuf-to-gf571-int opnd1-y)))))


#|
(c-ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)
(ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)

(c-ecc-mul *ecc-acudora-public-key* 15)
(ecc-mul *ecc-acudora-public-key* 15)

(c-ecc-add *ecc-acudora-public-key* *ecc-acudora-public-key*)
(ecc-add *ecc-acudora-public-key* *ecc-acudora-public-key*)

;; seeing about 3x speedup going to C
(time (dotimes (ix 1000)
        ;; about 1700 / sec
        (ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)))
(time (dotimes (ix 1000)
        ;; about 7500 / sec
        (c-ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)))

(time (dotimes (ix 10)
        ;; about 1.5/sec
        (ecc-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*)))
(time (dotimes (ix 10)
        ;; about 11/sec
        (c-ecc-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*)))
|#  