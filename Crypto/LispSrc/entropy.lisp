#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :ecc-crypto-b571)

;; -----------------------------------------------------------

(defun test-ctr-drbg (&rest plt-args)
  (let ((arr (make-array 256
                         :initial-element 0)))
    (loop repeat (* 100 4096) do
          (let ((xs (ctr-drbg 256)))
            (loop for ix from 0 below 32 do
                  (let ((jx (aref xs ix)))
                    (incf (aref arr jx))) )))

    ;; for entropy
    (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
    (map-into arr (lambda (p)
                    (if (plusp p)
                        (- (* p (log p 2)))
                      0))
              arr)
    (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
    (/ (reduce #'+ arr) 8)

#|
     ;; for min-entropy
    (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
    (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
    (/ (log (/ (reduce #'max arr)) 2) 8)
|#
    ))
  
(defun test-ec-drbg (&rest plt-args)
  (let ((arr (make-array 256
                         :initial-element 0)))
    (loop repeat 1000 do
          (let ((xs (ec-drbg 256)))
            (loop for ix from 0 below 32 do
                  (let ((jx (aref xs ix)))
                    (incf (aref arr jx))) )))

    ;; for entropy
    (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
    (map-into arr (lambda (p)
                    (if (plusp p)
                        (- (* p (log p 2)))
                      0))
              arr)
    (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
    (/ (reduce #'+ arr) 8)

#|
     ;; for min-entropy
    (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
    (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
    (/ (log (/ (reduce #'max arr)) 2) 8)
|#
    ))
  
(defun min-entropy (&optional fname)
  (um:with-remembered-prompting (fname :min-entropy fname)
      (with-open-file (fp fname
                          :direction :input
                          :element-type 'ubyte)
        (let* ((arr    (make-array 256
                                   :initial-element 0))
               (carr   (make-array 256
                                   :initial-element 0))
               (nb     (file-length fp))
               (buf    (make-ub-array nb))
               (bits   (ctr-drbg 384))
               (cipher (ironclad:make-cipher :aesx
                                             :key  (subseq bits 0 32)
                                             :mode :cbc
                                             :initialization-vector (subseq bits 32))))
          (read-sequence buf fp)
          (loop for ix from 0 below nb do
                (let ((jx (aref buf ix)))
                  (incf (aref arr jx)) ))
          ;; (ironclad:encrypt-in-place cipher buf)
          (safe-encrypt-in-place cipher buf)
          (loop for ix from 0 below nb do
                (let ((jx (aref buf ix)))
                  (incf (aref carr jx))))

          (labels ((compute (arr &rest plt-args)
                     (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
                     (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
                     (/ (log (/ (reduce #'max arr)) 2) 8)))
            (list (compute arr :clear t)
                  (compute carr :color :red)
                  fname)) ))))

(defun entropy (&optional fname)
  (um:with-remembered-prompting (fname :min-entropy fname)
      (with-open-file (fp fname
                          :direction :input
                          :element-type 'ubyte)
        (let* ((arr    (make-array 256
                                   :initial-element 0))
               (carr   (make-array 256
                                   :initial-element 0))
               (nb     (file-length fp))
               (buf    (make-ub-array nb))
               (bits   (ctr-drbg 384))
               (cipher (ironclad:make-cipher :aesx
                                             :key  (subseq bits 0 32)
                                             :mode :cbc
                                             :initialization-vector (subseq bits 32))))
          (read-sequence buf fp)
          (loop for ix from 0 below nb do
                (let ((jx (aref buf ix)))
                  (incf (aref arr jx)) ))
          ;; (ironclad:encrypt-in-place cipher buf)
          (safe-encrypt-in-place cipher buf)
          (loop for ix from 0 below nb do
                (let ((jx (aref buf ix)))
                  (incf (aref carr jx))))

          (labels ((compute (arr &rest plt-args)
                     (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
                     (map-into arr (lambda (p)
                                     (if (plusp p)
                                         (- (* p (log p 2)))
                                       0))
                               arr)
                     (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
                     (/ (reduce #'+ arr) 8)))
            (list (compute arr :clear t)
                  (compute carr :color :red)
                  fname)) ))))

(defun test-mt-random (&optional (nb (ash 1 24)))
  (with-open-file (fp "VTuning/crypto/tools/junk-random"
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create
                      :element-type 'ubyte)
    (loop repeat (ceiling nb 69) do
          (let* ((n (ecc-random-key))
                 (bytes (convert-int-to-nbytes n 69)))
            (write-sequence bytes fp))) ))


(defun vis-file (&optional fname)
  (um:with-remembered-prompting (fname :min-entropy fname)
    (with-open-file (fp fname
                        :direction :input
                        :element-type 'ubyte)
      (let* ((nb   (file-length fp))
             (buf  (make-ub-array nb))
             (dim  (floor (sqrt nb)))
             (xdim (min 1024 dim))
             (ydim (truncate nb xdim))
             (img  (make-ub-array `(,ydim ,xdim)
                                  :displaced-to buf)))
        (read-sequence buf fp)
        (plt:tvscl 'img img :magn 2 :clear t)
        (min-entropy fname)))))

(defun test-ecc (&optional (n 1000))
  (loop repeat n do
        (let* ((r (ecc-random-key))
               (p (ecc-mul *ecc-gen* r)))
          (validate-public-key p))))



;; test little-Frobenius automorphism
(with-lisp-ecc
    (let* ((x (ecc-pt-x *ecc-gen*))
           (y (ecc-pt-y *ecc-gen*))
           (pt (make-ecc-pt
                :x (gf^ x 16)
                :y (gf^ y 16)))
           (*ecc-b* (gf^ *ecc-b* 16))
           )
      (validate-public-key pt)))
