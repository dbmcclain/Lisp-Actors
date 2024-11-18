
(defpackage #:com.ral.burg
  (:use #:cl)
  (:export
   #:memcof
   #:predict
   #:filter
   #:blit
   #:vec
   ))

(in-package #:com.ral.burg)

;; -------------------------------------------------
;; Burg AR Analysis

(defvar *burg-library*
  #+:MAC (translate-logical-pathname "PROJECTS:DYLIB;libLispBurg.dylib")
  #+:WIN32 "Lisp_FPU.dll")

(fli:register-module *burg-library*)

(fli:define-foreign-function (lisp_memcof
			      "lisp_memcof" :source)
   ((src  (:pointer :double))
    (n    :long)
    (dst  (:pointer :double))
    (m    :long))
   :result-type :double
   :language :ansi-c)


(defun memcof (arr m)
  ;; returns polynomial coeffs of order ord -> ord+1 coffs
  (let* ((n   (length arr))
         (ans (make-array m :element-type 'double-float)))
    (fli:with-dynamic-foreign-objects ()
      (let ((cdst (fli:allocate-dynamic-foreign-object
                   :type :double
                   :nelems m))
            (csrc (fli:allocate-dynamic-foreign-object
                   :type :double
                   :nelems n)))
        (dotimes (ix n)
          (setf (fli:dereference csrc :index ix)
                (coerce (aref arr ix) 'double-float)))
        (let ((xms (lisp_memcof csrc n cdst m)))
          (dotimes (ix m)
            (setf (aref ans ix) (fli:dereference cdst :index ix)))
          (values xms ans))
        ))))
        

#|
(let* ((nformants  3)
       (ord  (* 2 nformants))
       (m    ord)
       (err  0.250d0)
       (ndata 100)
       (y (vm:vectorwise ((x     (vm:dramp ndata))
                          (noise (vm:gnoise ndata :sd err)))
                         (+ noise
                            (sin (* 2.0d0 pi x 0.1d0))
                            (* 0.25 (sin (* 2d0 pi x 0.25)))
                            (* -0.06 (sin (* 2d0 pi x 0.35)))))))
  (labels ((frq (z)
             (/ (phase z) 2 pi))
           (bw (z)
             (/ (log (abs z)) -2 2 pi)))

  (multiple-value-bind (xms dcof)
      (memcof y m)

    (plt:plot 'coffs dcof :clear t)

    (let* ((nfft   16384)
           (nfft/2 (truncate nfft 2))
           (arr    (let ((arr (make-array nfft
                                          :element-type 'single-float
                                          :initial-element 0.0)))
                     (setf (aref arr 0) 1.0)
                     (loop for coff across dcof
                           for ix from 1
                           do
                           (setf (aref arr ix) (- (coerce coff 'single-float))))
                     arr))
           (fcoffs (fft:fwd-magnitude-db arr))
           (frqs   (vops:vscale (/ nfft) (vm:framp nfft/2)))
           (fcoffs (map 'vector '- fcoffs)))
      (plt:plot 'dbplt frqs fcoffs :clear t
                ;; :xrange '(0.09 0.11)
                ))

    (plt:plot 'data y :clear t
              :yrange '(-5 5))

    (multiple-value-bind (xms ym)
        ;; (predict y m :nnew 80 :nprev 0)
        (filter y m)
      (plt:plot 'data ym :color :red)
      (plt:plot 'diff (map 'vector #'- ym y) :clear t)

      (let* ((poly-coffs (Let ((arr (make-array (1+ ord)
                                                :element-type 'double-float)))
                           (setf (aref arr 0) 1d0)
                           (loop for coff across dcof
                                 for ix from 1
                                 do
                                 (setf (aref arr ix) (- coff)))
                           (nreverse arr)))
             (roots (remove-if (lambda (z)
                                 (minusp (imagpart z)))
                               (nrglue:zroots poly-coffs))))
        (list :rms-err xms
              :coffs   dcof
              :roots   (map 'vector (lambda (z)
                                      (complex (abs z)
                                               (/ (phase z) 2 pi)))
                            roots)
              :freq  (map 'vector #'frq roots)
              :bw    (map 'vector #'bw roots))
        )))))
|#

;; --------------------------------------------------------
;; Vector Routines

(defun blit (src srcoff dst dstoff nel)
  (loop for ix from 0 below nel do
        (setf (aref dst (+ dstoff ix))
              (aref src (max 0 (+ srcoff ix))))))

(defun vec (arr offs &optional (nel (- (length arr) offs)))
  (make-array nel
              :displaced-to arr
              :displaced-index-offset offs
              :element-type (array-element-type arr)))

;; ------------------------------------------------------
;; Burg AR Prediction

(defun predict (data m
                     &key
                     nnew nprev
                     (mean-remove t))
  (let* ((nel  (length data))
         (mn   (if mean-remove (vm:mean data) 0.0d0))
         (dat  (if mean-remove
                   (vm:vectorwise ((d data))
                                  (- d mn))
                 data)))
    (multiple-value-bind (xms dcof) (memcof dat m)
      (let* ((dx  (nreverse dcof))
             (ans (make-array (+ m nprev nnew))))
        (blit dat (- nel (+ nprev m)) ans 0 (+ nprev m))
        (loop for kx from (+ nprev m) below (+ nprev m nnew) do
              (setf (aref ans kx)
                    (vm:inner-prod dx (vec ans (- kx m) m))
                    ))
        (loop for kx from (+ nprev m -1) downto m do
              (setf (aref ans kx)
                    (vm:inner-prod dx (vec ans (- kx m) m))
                    ))
        (values xms
                (vm:vectorwise ((a (subseq ans m)))
                               (+ a mn)))
        ))))

(defun filter (data m
                     &key
                     (mean-remove t))
  (let* ((nel  (length data))
         (mn   (if mean-remove (vm:mean data) 0.0d0))
         (dat  (if mean-remove
                   (vm:vectorwise ((d data))
                                  (- d mn))
                 data)))
    (multiple-value-bind (xms dcof) (memcof dat m)
      (let* ((dx  (nreverse dcof))
             (ans (make-array nel)))
        (loop for pos from 0 below nel do
              (setf (aref ans pos)
                    (+ mn ;; (aref data pos)
                       (loop for ix from (- pos m) below pos
                             for coff across dx
                             sum
                             (if (minusp ix)
                                 0
                               (* coff (aref data ix)))
                             ))
                    ))
        (values xms ans)
        ))))

