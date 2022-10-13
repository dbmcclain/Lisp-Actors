
(in-package #:com.ral.scatter-vec)

(defclass scatter-vector ()
  ((frags      :accessor scatter-vector-frags
               :initform (make-array 1
                                     :adjustable   t
                                     :fill-pointer 0))
   (last-ix    :accessor scatter-vector-last-ix    :initform 0)
   (last-base  :accessor scatter-vector-last-base  :initform 0)
   (length     :accessor scatter-vector-length     :initform 0)
   ))

(defun make-scatter-vector ()
  (make-instance 'scatter-vector))

;; ----------------------------------------------------

(defun get-current-frag-and-length (vec vix)
  (let* ((cur-frag (aref (scatter-vector-frags vec) vix))
         (nel      (xlength cur-frag)))
    (values cur-frag nel)))

(defun get-starting-ix-and-base (vec pos)
  (unless (minusp pos)
    (let* ((cur-ix (scatter-vector-last-ix vec))
           (base   (scatter-vector-last-base vec))
           (frags  (scatter-vector-frags vec))
           (nfrags (length frags)))
      (when (< pos base)
        (setf cur-ix 0
              base   0))
      ;; for sequential access we are most likely already in the
      ;; correct frag
      (let* ((cur-frag (aref frags cur-ix))
             (nel      (xlength cur-frag)))
        (if (< pos (+ base nel))
            (values cur-ix base (- pos base) cur-frag nel)
          ;; else - search upward for start frag
          (um:nlet iter ((cur-ix (1+ cur-ix))
                         (base   (+ base nel)))
            (when (< cur-ix nfrags)
              (let* ((cur-frag (aref frags cur-ix))
                     (nel      (xlength cur-frag)))
                (cond ((< pos (+ base nel))
                       (setf (scatter-vector-last-ix vec)   cur-ix
                             (scatter-vector-last-base vec) base)
                       (values cur-ix base (- pos base) cur-frag nel))
                      (t
                       (go-iter (1+ cur-ix) (+ base nel)))
                      ))))
          )))))

(defgeneric %xdovec (vec gbase fn &rest args &key &allow-other-keys)
  (:method ((vec vector) gbase fn &rest args &key &allow-other-keys)
   (apply fn vec gbase args))
  (:method ((vec scatter-vector) gbase fn &rest args &key (start 0) end &allow-other-keys)
   (multiple-value-bind (cur-ix base ix-rem)
       (get-starting-ix-and-base vec start)
     (when cur-ix
       (let* ((frags  (scatter-vector-frags vec))
              (nfrags (length frags))
              (end    (if end
                          (min end (scatter-vector-length vec))
                        (scatter-vector-length vec))))
         (um:nlet iter ((cur-ix cur-ix)
                        (base   base)
                        (ix-rem ix-rem))
           (when (and (< cur-ix nfrags)
                      (< base end))
             (let* ((cur-frag (aref frags cur-ix))
                    (nel      (xlength cur-frag)))
               (apply #'%xdovec cur-frag (+ base gbase) fn
                      :start ix-rem
                      :end   (min nel (- end base))
                      args)
               (go-iter (1+ cur-ix) (+ base nel) 0)
               ))
           )))
     )))

;; -----------------------------------------------------

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

(defun xdovec (vec fn &rest args)
  (apply #'%xdovec vec 0 fn args))

(defun xwrite-sequence (vec stream &key (start 0) end)
  (xdovec vec
          (lambda (vec base &rest args)
            (declare (ignore base))
            (apply #'write-sequence vec stream args))
          :start start
          :end   end))

(defun xupdate-digest (state vec &rest args)
  (apply #'xdovec vec
         (lambda (vec base &rest args)
           (declare (ignore base))
           (apply #'ironclad:update-digest state vec args))
         args))
                        
(defgeneric #1=xposition (item vec &rest args &key &allow-other-keys)
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
            (multiple-value-bind (cur-ix base ix-rem cur-frag nel)
                (get-starting-ix-and-base vec (1- end))
              (declare (ignore ix-rem cur-frag))
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
                ))))
         (t
          (apply #'xdovec vec
                 (lambda (vec base &rest args)
                   (when-let (pos (apply #'xposition item vec args))
                     (return-from #1# (+ base pos))))
                 args))
         )))

(defgeneric xaref (vec ix)
  (:method ((vec vector) ix)
   (aref vec ix))
  (:method ((vec scatter-vector) ix)
   ;; written to allow possibility of scatter-vector elements also
   ;; being scatter-vectors
   (multiple-value-bind (cur-ix base ix-rem cur-frag nel)
       (get-starting-ix-and-base vec ix)
     (declare (ignore base nel))
     (unless cur-ix
       (error "Out of bounds array access: ~S" ix))
     (xaref cur-frag ix-rem))
   ))

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
        (let* ((end (if end
                        (min end (xlength vec))
                      (xlength vec)))
               (ans (make-array (- end start)
                                :element-type '(unsigned-byte 8)))
               (pos  0))
          (xdovec vec (lambda (vec base start end)
                        (declare (ignore base))
                        (replace ans vec
                                 :start1 pos
                                 :start2 start
                                 :end2   end)
                        (incf pos (- end start)))
                  start end)
          ans))
       ))))

(defmethod add-fragment ((sv scatter-vector) frag-vec)
  (vector-push-extend frag-vec (scatter-vector-frags sv))
  (incf (scatter-vector-length sv) (xlength frag-vec)))

