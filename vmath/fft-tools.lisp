
(in-package #:com.ral.fft)

;; -------------------------------------------------------------------------
;; An FFT vector contains elements from positive freqs in first half, followed
;; by elements from negative freqs in second half:
;;
;; N Elements: 0 .. N-1, N even (e.g., N = 2^n)
;;
;;          positive freqs     |  negative freqs
;;  +---+---+---+--//--+-------+-----+--//--+----+----+
;;  | 0 | 1 | 2 |  //  | N/2-1 | N/2 |  //  | -2 | -1 |
;;  +---+---+---+--//--+-------+-----+--//--+----+----+
;;    ^                        |  ^                 ^
;;    |                           |                 |
;;    DC                       Nyquist           Index N-1
;;                          = Index (N-N/2)
;;
;; ---------------------------------------------------------------------------

(defun center-implant (dst src)
  (replace dst src :start1 (- (truncate (length dst) 2)
                              (truncate (length src) 2)))
  dst)

(defun symmetric-replace (dst src &key (start1 0) (start2 0) end1 end2)
  ;; symmetric - in the sense of FFT symmetry
  (let* ((dstlen  (length dst))
         (mid     (truncate dstlen 2))
         (end1    (or end1 mid))
         (srcseq  (subseq src start2 end2))
         (srclen  (length srcseq))
         (efflen  (- end1 start1)))
    (when (> efflen srclen)
      (setf end1   (+ start1 srclen)
            efflen srclen))
    (when (< start1 end1)
      (cond
       ((= end1 1)
        ;; only 1 element to stuff
        (setf (aref dst 0) (aref srcseq 0)))

       ((< end1 mid)
        (when (> srclen efflen)
          (setf srcseq (subseq srcseq 0 efflen)))
        (replace dst srcseq :start1 start1 :end1 end1)
        (let* ((revseq    (reverse srcseq))
               (nel       (- end1 (max start1 1)))
               (sym-start (- dstlen (1- end1)))
               (sym-end   (+ sym-start nel)))
          (replace dst revseq
                   :start1 sym-start
                   :end1   sym-end)))

       ((= end1 mid)
        ;; allow srcseq to be longer to cover Nyquist if it will
        (replace dst srcseq :start1 start1 :end1 (1+ end1)) ;; incl Nyquist
        (let* ((revseq (reverse (subseq srcseq (if (zerop start1)
                                                   1
                                                 0)
                                        efflen)))
               (nel  (- end1 (max start1 1)))
               (sym-start (- dstlen (1- end1)))
               (sym-end   (+ sym-start nel)))
          (replace dst revseq
                   :start1 sym-start
                   :end1   sym-end)))

       (t ;; (> end1 mid)
          ;; disregard symmetry, treat as normal replace
          (replace dst srcseq :start1 start1 :end1 end1))
       ))
    dst))

#|
(let ((x   #(_ _ _ _ _ _ _ _))
      (src #(a b c))
      (start 2))
  (symmetric-replace x src :start1 start :end1 4))

(let ((x   #(_ _ _ _ _ _ _ _))
      (src #(a b c)))
  (center-implant x src)) ;; => #(_ _ _ A B C _ _)

(let ((x   #(_ _ _ _ _ _ _ _))
      (src #(a b c)))
  (vm:shifth (center-implant x src))) ;; => #(B C _ _ _ _ _ A)
|#

(defun symmetric-fill (dst elt &key (start 0) end)
  ;; symmetric - in the sense of FFT symmetry
  ;; NOTE: if end > mid then treat as normal fill, ignoring symmetry
  (let* ((len (length dst))
         (mid (truncate len 2))
         (end (or end mid)))
    (when (< start end)
      (cond ((= end 1)
             ;; only one element to stuff
             (setf (aref dst 0) elt))

            ((< end mid)
             (fill dst elt :start start :end end)
             (fill dst elt :start (- len (1- end)) :end (min len (1+ (- len start)))))
            
            ((= end mid)
             ;; fill central region, including Nyquist element
             (fill dst elt :start start :end (min len (1+ (- len start)))))

            (t ;; (> end mid)
             ;; just treat as normal fill ignoring symmetry
             (fill dst elt :start start :end end))
            ))
    dst))

 