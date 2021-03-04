
(in-package #:scatter-vec)

(defclass scatter-vector ()
  ((frags      :accessor scatter-vector-frags      :initform (make-array 1
                                                                         :adjustable   t
                                                                         :fill-pointer 0))
   (last-ix    :accessor scatter-vector-last-ix    :initform 0)
   (last-base  :accessor scatter-vector-last-base  :initform 0)
   (length     :accessor scatter-vector-length     :initform 0)
   ))

(defgeneric in-bounds-p (vec ix)
  (:method ((vec vector) ix)
   (array-in-bounds-p vec ix))
  (:method ((vec scatter-vector) ix)
   (and (not (minusp ix))
        (< ix (scatter-vector-length vec))
        )))

(defgeneric xlength (vec)
  (:method ((vec vector))
   (length vec))
  (:method ((vec scatter-vector))
   (scatter-vector-length vec)))

(defgeneric xaref (vec ix)
  (:method ((vec vector) ix)
   (aref vec ix))
  (:method ((vec scatter-vector) ix)
   ;; written to allow possibility of scatter-vector elements also
   ;; being scatter-vectors
   (let ((ix-rem (- ix (scatter-vector-last-base vec))))
     (when (minusp ix-rem)
       ;; start over from front
       (setf (scatter-vector-last-ix vec)   0
             (scatter-vector-last-base vec) 0
             ix-rem  ix))
     (um:nlet iter ((cur-ix (scatter-vector-last-ix vec))
                    (ix-rem ix-rem))
       (let* ((cur-frag (aref (scatter-vector-frags vec) cur-ix))
              (nel      (xlength cur-frag)))
         (if (< ix-rem nel)
             (xaref cur-frag ix-rem)
           (progn
             (incf (scatter-vector-last-ix vec))
             (incf (scatter-vector-last-base vec) nel)
             (go-iter (1+ cur-ix) (- ix-rem nel)))
           )))
     )))

(defgeneric xdefrag (vec)
  ;; return vector copy of vec defragmented
  (:method ((vec vector))
   (copy-seq vec))
  (:method ((vec scatter-vector))
   (xsubseq vec 0)))

(defgeneric xsubseq (vec start &optional end)
  ;; return a subseq vector copy of vec
  (:method ((vec vector) start &optional end)
   (subseq vec start end))
  (:method ((vec scatter-vector) start &optional end)
   (let* ((frags  (scatter-vector-frags vec))
          (nfrags (length frags)))
     (case nfrags
       ((0)  (subseq #() start end))
       ((1)  (let ((fragv (xdefrag (aref frags 0))))
               (if (and (zerop start) ;; try to avoid double copying
                        (or (null end)
                            (= end (length fragv))))
                   fragv ;; fragv is already a copy
                 ;; else
                 (subseq fragv start end))))
       (t
        (let* ((end  (or end (xlength vec)))
               (ans  (make-array (- end start)
                                 :element-type '(unsigned-byte 8))))
          (um:nlet outer ((base    0)
                          (frag-ix 0))
            ;; first - search for start frag
            (if (< base end)
                (let* ((frag (aref frags frag-ix))
                       (nb   (xlength frag)))
                  (when (um:within base start (+ base nb))
                    (um:nlet inner ((pos     0)
                                    (base    base)
                                    (frag-ix frag-ix))
                      (if (< base end)
                          (let* ((fragv      (xdefrag (aref frags frag-ix)))
                                 (nfrag      (length fragv))
                                 (frag-start (max 0 (- start base)))
                                 (frag-end   (- (min (+ base nfrag) end)
                                                base))
                                 (nb         (- frag-end frag-start)))
                            (replace ans fragv
                                     :start1 pos
                                     :start2 frag-start
                                     :end2   frag-end)
                            (go-inner (+ pos nb) (+ base nfrag) (1+ frag-ix)))
                        ;; else
                        ans)))
                  (go-outer (+ base nb) (1+ frag-ix)))
              ;; else
              ans))
          ))
       ))))

(defmethod add-fragment ((sv scatter-vector) frag-vec)
  (vector-push-extend frag-vec (scatter-vector-frags sv))
  (incf (scatter-vector-length sv) (xlength frag-vec)))

