
(in-package #:scatter-vec)

(defclass scatter-vector ()
  ((frags      :accessor scatter-vector-frags      :initform (make-array 1
                                                                         :adjustable   t
                                                                         :fill-pointer 0))
   (last-ix    :accessor scatter-vector-last-ix    :initform 0)
   (last-base  :accessor scatter-vector-last-base  :initform 0)
   (length     :accessor scatter-vector-length     :initform 0)
   ))

;; ----------------------------------------------------

(defgeneric in-bounds-p (vec ix)
  (:method ((vec vector) ix)
   (array-in-bounds-p vec ix))
  (:method ((vec scatter-vector) ix)
   (and (not (minusp ix))
        (< ix (scatter-vector-length vec))
        )))

(defun get-current-frag-and-length (vec vix)
  (let* ((cur-frag (aref (scatter-vector-frags vec) vix))
         (nel      (xlength cur-frag)))
    (values cur-frag nel)))

(defun get-starting-ix-and-base (vec pos)
  (unless (minusp pos)
    (let* ((cur-ix (scatter-vector-last-ix vec))
           (base   (scatter-vector-last-base vec))
           (nfrags (length (scatter-vector-frags vec))))
      (when (< pos base)
        (setf cur-ix 0
              base   0))
      (um:nlet iter ((cur-ix cur-ix)
                     (base   base))
        (when (< cur-ix nfrags)
          (multiple-value-bind (cur-frag nel)
              (get-current-frag-and-length vec cur-ix)
            (declare (ignore cur-frag))
            (if (< pos (+ base nel))
                (values cur-ix base (- pos base))
              (go-iter (1+ cur-ix) (+ base nel)))
            ))
        ))))

;; -----------------------------------------------------

(defgeneric xlength (vec)
  (:method ((vec vector))
   (length vec))
  (:method ((vec scatter-vector))
   (scatter-vector-length vec)))

(defgeneric xdovec (vec fn &rest args &key &allow-other-keys)
  (:method ((vec vector) fn &rest args &key &allow-other-keys)
   (apply fn vec args))
  (:method ((vec scatter-vector) fn &rest args &key (start 0) end &allow-other-keys)
   (multiple-value-bind (cur-ix base ix-rem)
       (get-starting-ix-and-base vec start)
     (when cur-ix
       (let* ((frags  (scatter-vector-frags vec))
              (nfrags (length frags))
              (end    (or end (scatter-vector-length vec))))
         (um:nlet iter ((cur-ix cur-ix)
                        (base   base)
                        (ix-rem ix-rem))
           (when (and (< cur-ix nfrags)
                      (< base end))
             (let* ((cur-frag (aref frags cur-ix))
                    (nel      (xlength cur-frag)))
               (apply #'xdovec cur-frag fn
                      :start ix-rem
                      :end   (min nel (- end base))
                      args)
               (go-iter (1+ cur-ix) (+ base nel) 0)
               ))
           )))
     )))

(defun xupdate-digest (state vec &rest args)
  (apply #'xdovec vec
         (lambda (vec &rest args)
           (apply #'ironclad:update-digest state vec args))
         args))
                        
(defgeneric xposition (item vec &rest args &key &allow-other-keys)
  (:method (item (vec vector) &rest args &key &allow-other-keys)
   (apply #'position item vec args))
  (:method (item (vec scatter-vector) &rest args
                 &key (start 0)
                 end
                 from-end
                 &allow-other-keys)
   (cond (from-end
          (let ((end (if end
                         (min end (xlength vec))
                       (xlength vec))))
            (multiple-value-bind (cur-ix base)
                (get-starting-ix-and-base vec (1- end))
              (multiple-value-bind (cur-frag nel)
                  (get-current-frag-and-length vec cur-ix)
                (declare (ignore cur-frag))
                (um:nlet iter ((cur-ix cur-ix)
                               (vend   (+ base nel)))
                  (unless (or (minusp cur-ix)
                              (> start vend))
                    (multiple-value-bind (cur-frag nel)
                        (get-current-frag-and-length vec cur-ix)
                      (let ((base (- vend nel)))
                        (if-let (pos (apply #'xposition item cur-frag
                                            :start (max (- start base) 0)
                                            :end   (min (- end base) nel)
                                            args))
                            (+ pos base) ;; found it
                          ;; else - try next lower-case-p fragment
                          (go-iter (1- cur-ix) base))
                        )))
                  )))
            ))
         (t
          (multiple-value-bind (cur-ix base ix-rem)
              (get-starting-ix-and-base vec start)
            (when cur-ix
              (let* ((frags  (scatter-vector-frags vec))
                     (nfrags (length frags))
                     (end    (or end (xlength vec))))
                (um:nlet iter ((cur-ix cur-ix)
                               (base   base)
                               (ix-rem ix-rem))
                  (when (and (< cur-ix nfrags)
                             (< base end))
                    (multiple-value-bind (cur-frag nel)
                        (get-current-frag-and-length vec cur-ix)
                      (if-let (pos (apply #'xposition item cur-frag
                                          :start ix-rem
                                          :end   (min nel (- end base))
                                          args))
                          (+ pos base) ;; found it
                        ;; else - not found in this segment
                        (go-iter (1+ cur-ix) (+ base nel) 0))
                      )))
                ))))
         )))

(defgeneric xaref (vec ix)
  (:method ((vec vector) ix)
   (aref vec ix))
  (:method ((vec scatter-vector) ix)
   ;; written to allow possibility of scatter-vector elements also
   ;; being scatter-vectors
   (multiple-value-bind (cur-ix base ix-rem)
       (get-starting-ix-and-base vec ix)
     (declare (ignore base))
     (um:nlet iter ((cur-ix cur-ix)
                    (ix-rem ix-rem))
       (multiple-value-bind (cur-frag nel)
           (get-current-frag-and-length vec cur-ix)
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
       ((1)  (subseq (aref frags 0) start end))
       (t
        (multiple-value-bind (cur-ix base ix-rem)
            (get-starting-ix-and-base vec start)
          (unless cur-ix
            (error "Starting index out of bounds: ~S" start))
          (let* ((end  (if end
                           (min end (xlength vec))
                         (xlength vec)))
                 (narr (- end start))
                 (ans  (make-array narr
                                   :element-type '(unsigned-byte 8))))
            (um:nlet iter ((cur-ix cur-ix)
                           (base   base)
                           (pos    0)
                           (ix-rem ix-rem))
              (if (< pos narr)
                  (multiple-value-bind (cur-frag nel)
                      (get-current-frag-and-length vec cur-ix)
                    (let* ((end2  (min nel (- end base)))
                           (nrep  (- end2 ix-rem)))
                      (replace ans cur-frag
                               :start1 pos
                               :start2 ix-rem
                               :end2   end2)
                      (go-iter (1+ cur-ix)
                               (+ base nel)
                               (+ pos nrep)
                               0)))
                ;; else - we are done
                ans))
            )))
       ))))

(defmethod add-fragment ((sv scatter-vector) frag-vec)
  (vector-push-extend frag-vec (scatter-vector-frags sv))
  (incf (scatter-vector-length sv) (xlength frag-vec)))

