;; encr-file.lisp
;;
;; DM/RAL  2023/02/22 13:38:46
;; ----------------------------------

(defpackage #:encr-file
  (:use #:common-lisp #:edec #:vec-repr #:hash))

(in-package #:encr-file)

;; ----------------------------------

(defun actors-smult (pt)
  (let* ((txt (with-output-to-string (s)
                (sys:call-system-showing-output
                 (with-standard-io-syntax
                   (format nil "~~/bin/edwards-tpm \"#x~X\"" (int pt)))
                 :output-stream s)
                ))
         (lines (um:split-string txt :delims '(#\newline))))
    
    ;; (print lines)
    (ed-decompress-pt (read-from-string (subseq (cadr lines) 2)))
    ))

(defun actors-pkey ()
  ;; (edec:smult edec:*ed-gen*)
  (actors-smult edec:*ed-gen*))

(defconstant +encrypted-file+ {0e2b46c4-b2f1-11ed-8488-f643f5d48a65})

(defun encrypt-file ()
  (let ((in-fname (capi:prompt-for-file "Input"
                                        :pathname (um:remembered-filename :encryption-in)
                                        :filter "*.*"
                                        :filters nil
                                        :operation :open)))
    (when in-fname
      (um:remember-filename :encryption-in in-fname)
      (let* ((v         (loenc:encode (vector in-fname
                                              (hcl:file-binary-bytes in-fname))))
             (nel       (length v))
             (out-fname (capi:prompt-for-file "Output"
                                              :pathname (um:remembered-filename :encryption-out)
                                              :filter "*.*"
                                              :filters nil
                                              :operation :save)))
        (when out-fname
          (um:remember-filename :encryption-out out-fname)
          (multiple-value-bind (skey pkey)
              (ed-random-pair)
            (let* ((kindv (vec (bevn +encrypted-file+ 16)))
                   (idv   (vec (bevn (uuid:make-v1-uuid) 16)))
                   (to    (actors-pkey))
                   (tov   (vec (bevn (actors-pkey) 32)))
                   (fromv (vec (bevn pkey 32)))
                   (keyv  (vec (bevn
                                (hash/256 (ed-mul to skey)
                                          kindv idv tov fromv)
                                32)))
                   (ivecv (vec (bevn (hash/256 keyv) 32)))
                   (cipher (ironclad:make-cipher :aes
                                                 :key keyv
                                                 :initialization-vector ivecv
                                                 :mode :ecb))
                   (digest (ironclad:make-digest :sha3))
                   (wrk    (vec-repr:make-ub8-vector 16))
                   (seg    (make-array 16
                                       :element-type '(unsigned-byte 8)
                                       :adjustable t))
                   (lenv   (vec (bevn nel 4))))
              (ironclad:update-digest digest kindv)
              (ironclad:update-digest digest idv)
              (ironclad:update-digest digest tov)
              (ironclad:update-digest digest fromv)
              (ironclad:update-digest digest ivecv)
              (ironclad:update-digest digest lenv)
              (loop for pos from 0 below nel by 16 do
                    (let ((nb  (min 16 (- nel pos))))
                      (adjust-array seg nb
                                    :displaced-to v
                                    :displaced-index-offset pos)
                      (um:nlet iter ((x  (1+ pos))
                                     (ix 15))
                        (cond ((zerop x)
                               (fill wrk 0 :end (1+ ix))
                               (map-into wrk #'logxor idv wrk))
                              (t
                               (setf (aref wrk ix) (logand x 255))
                               (go-iter (ash x -8) (1- ix)))
                              ))
                      (ironclad:encrypt-in-place cipher wrk)
                      (map-into seg #'logxor wrk seg)))
              (ironclad:update-digest digest v)
              (with-open-file (f out-fname
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :rename
                                 :element-type '(unsigned-byte 8))
                (write-sequence kindv f)
                (write-sequence idv f)
                (write-sequence tov f)
                (write-sequence fromv f)
                (write-sequence ivecv f)
                (write-sequence lenv f)
                (write-sequence v f)
                (write-sequence (vec (bevn (ironclad:produce-digest digest) 32)) f)
                ))))
        ))
    ))

(defun decrypt-file ()
  (let ((in-fname (capi:prompt-for-file "Input"
                                        :pathname (um:remembered-filename :decryption-in)
                                        :filter "*.*"
                                        :filters nil
                                        :operation :open)))
    (when in-fname
      (um:remember-filename :decryption-in in-fname)
      (let ((kindv (make-ub8-vector 16))
            (idv   (make-ub8-vector 16))
            (tov   (make-ub8-vector 32))
            (to    nil)
            (fromv (make-ub8-vector 32))
            (from  nil)
            (lenv  (make-ub8-vector 4))
            (ivecv (make-ub8-vector 32))
            (digv  (make-ub8-vector 32))
            (wrk  (make-ub8-vector 16))
            (v    nil)
            (nel  nil))
        (with-open-file (f in-fname
                           :direction :input
                           :element-type '(unsigned-byte 8))
          (read-sequence kindv f)
          (assert (equalp kindv (vec (bevn +encrypted-file+ 16))))
          (read-sequence idv f)
          (let ((uuid (uuid:byte-array-to-uuid idv)))
            (format t "Created: ~S~%" (uuid:when-created uuid)))
          (read-sequence tov f)
          (setf to (ed-decompress-pt tov))
          (assert (ed-pt= to (actors-pkey)))
          (read-sequence fromv f)
          (setf from (ed-decompress-pt fromv))
          (read-sequence ivecv f)
          (read-sequence lenv f)
          (setf nel (int lenv))
          (setf v (make-ub8-vector nel))
          (read-sequence v f)
          (read-sequence digv f))
        (let* ((keyv (vec (bevn (hash/256 (actors-smult from)
                                          kindv idv tov fromv)
                                32)))
               (cipher (ironclad:make-cipher :aes
                                             :key keyv
                                             :initialization-vector ivecv
                                             :mode :ecb))
               (digest (ironclad:make-digest :sha3))
               (seg    (make-array 16
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t)))
          (ironclad:update-digest digest kindv)
          (ironclad:update-digest digest idv)
          (ironclad:update-digest digest tov)
          (ironclad:update-digest digest fromv)
          (ironclad:update-digest digest ivecv)
          (ironclad:update-digest digest lenv)
          (ironclad:update-digest digest v)
          (assert (equalp digv
                          (vec (bevn (ironclad:produce-digest digest) 32))))
          (loop for pos from 0 below nel by 16 do
                  (let ((nb  (min 16 (- nel pos))))
                    (adjust-array seg nb
                                  :displaced-to v
                                  :displaced-index-offset pos)
                    (um:nlet iter ((x  (1+ pos))
                                   (ix 15))
                      (cond ((zerop x)
                             (fill wrk 0 :end (1+ ix))
                                 (map-into wrk #'logxor idv wrk))
                            (t
                             (setf (aref wrk ix) (logand x 255))
                             (go-iter (ash x -8) (1- ix)))
                            ))
                    (ironclad:encrypt-in-place cipher wrk)
                    (map-into seg #'logxor wrk seg)
                    ))
          (setf v (loenc:decode v))
          (format t "Path: ~A~%" (aref v 0))
          (setf v (aref v 1))
          (let ((out-fname (capi:prompt-for-file "Output"
                                                 :pathname (um:remembered-filename :decryption-out)
                                                 :filter "*.*"
                                                 :filters nil
                                                 :operation :save)))
            (when out-fname
              (um:remember-filename :decryption-out out-fname)
              (with-open-file (f out-fname
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :rename
                                 :if-does-not-exist :create)
                (write-sequence v f)))
            ))
        ))
    ))
