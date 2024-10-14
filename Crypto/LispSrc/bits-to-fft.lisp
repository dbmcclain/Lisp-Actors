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

#|
(let*((x (big32 #x028F408C #x031CA59B #x46B08C6A #x1A9E723D
                #x57FCB9AF #x54C5B5FE #xBD2F908B #x98728791
                #x21FE8D0C #xB262388F #xD4AAAE09 #x0EB8AD81
                #x466576DC #x80181782 #x6BE8DAEB #x99305FAC
                #x9BE7B169 #xE81F25C3 ))
      (arr (make-array 4096
                       :initial-element 0.0)))
  (loop for ix from 0 below 571
        for jx from 16
        do
        (if (logbitp ix x)
            (setf (aref arr jx) 1.0
                  (aref arr (mod (- jx) 4096)) 1.0)))
  (let* ((fx (fft:inv arr))
         (rx (map 'vector #'realpart fx))
         (rx+ (map 'vector (um:rcurry #'+ 0.01) rx)))
    (plt:plot 'plt rx+
              :clear t)
    (let* ((rx-clip (map 'vector (lambda (x)
                                   (cond ((< x 0.0) 0.0)
                                         ((> x 0.02) 0.02)
                                         (t x)))
                         rx+))
           (rx- (map 'vector (um:rcurry #'- 0.01) rx-clip)))
      (fill rx- 0 :start 10 :end 31)
      (fill rx- 0 :start 200 :end 220)
      (fill rx- 0 :start 3100 :end 3200)
      (let* ((fxx (fft:fwd rx-))
             (rfxx (map 'vector #'realpart fxx))
             (rfxx-clip (map 'vector (lambda (x)
                                       (cond ((< x 0.3) 0.0)
                                             ((>= x 0.3) 1.0)))
                             rfxx)))
        (plt:plot 'pltx (map 'vector #'- arr rfxx-clip) :clear t)
        (plt:plot 'pltxx rfxx :clear t)
        ))))
|#

;; How to encode a single-bit FFT of a valued number
;; e.g., in the LSB of a music file
;; so that full lossless recovery of the encoded number is possible?
(let*((x (big32 #x028F408C #x031CA59B #x46B08C6A #x1A9E723D
                #x57FCB9AF #x54C5B5FE #xBD2F908B #x98728791
                #x21FE8D0C #xB262388F #xD4AAAE09 #x0EB8AD81
                #x466576DC #x80181782 #x6BE8DAEB #x99305FAC
                #x9BE7B169 #xE81F25C3 ))
      (nel (* 4 4 4096))
      (start 2048)
      (del   2)
      (end (+ start (* del 571)))
      (arr (make-array nel
                       :initial-element 0.0)))
  (loop for ix from 0 below 571
        for jx from start by del
        do
        (let ((v (if (logbitp ix x)
                     1.0
                   -1.0)))
          (setf (aref arr jx) v
                (aref arr (mod (- jx) nel)) v)))
  ;; (print (aref arr start))
  (let* ((fx  (fft:inv arr))
         (rx  (map 'vector #'realpart fx))
         (rx+ (map 'vector (lambda (x)
                             (cond ((plusp x) 1.0)
                                   ;; ((minusp x) -1.0)
                                   (t 0.0)))
                   rx)))
    (plt:plot 'plt rx
              :clear t)
    #|
    (fill rx+ 0 :start 10 :end 31)
    (fill rx+ 0 :start 200 :end 220)
    (fill rx+ 0 :start 3100 :end 3200)
    |#
    (let* ((rx- (map 'vector (um:rcurry #'- 0.5) rx+))
           (fxx (fft:fwd rx-))
           (rfxx (map 'vector #'realpart fxx))
           (thrsh 0)
           (rfxx-clip (map 'vector (lambda (x)
                                     (cond ((< x (- thrsh)) -1.0)
                                           ((> x thrsh) 1.0)
                                           (t 0.0)))
                           rfxx)))
      (plt:plot 'pltx (map 'vector #'- arr rfxx-clip)
                :xrange (list start end)
                :clear t)
      ;; (print (aref rfxx-clip start))
      (plt:plot 'pltxx rfxx :clear t)
      ;; (plt:plot 'pltx arr  :color :red)
      (vm:total (map 'vector #'-
                     (subseq arr start end)
                     (subseq rfxx-clip start end)))
      )))
