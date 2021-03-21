
(in-package #:self-sync)

(defconstant +long-count-base+  #xfd)
(defconstant +max-short-count+  (1- +long-count-base+))
(defconstant +max-long-count+   (1- (* +long-count-base+ +long-count-base+)))
(defconstant +start-sequence+   #(#xfd #xfe))

(defun find-start-seq (enc start end)
  (when (< start (1- end))
    (when-let (pos (scatter-vec:xposition #xfd enc
                                          :start start))
      (when (< pos (1- end))
        (if (eql #xfe (aref enc (1+ pos)))
            pos
          (find-start-seq enc (1+ pos) end))
        ))))

(defun write-self-sync (enc fout)
  (write-sequence +start-sequence+ fout)
  (let* ((start     0)
         (end       (length enc))
         (pos       (find-start-seq enc start end))
         (max-ct    +max-short-count+)
         (short-end (min +max-short-count+ (or pos end)))
         (nb        short-end))
    (write-byte nb fout)
    (loop for ix from 0 below short-end do
          (write-byte (aref enc ix) fout))
    (setf start short-end)
    (um:while (< start end)
      (when (< nb max-ct)
        (incf start 2))
      (when (and pos
                 (< pos start))
        (setf pos (find-start-seq enc start end)))
      (setf max-ct +max-long-count+)
      (let ((long-end (min (+ start +max-long-count+) (or pos end))))
        (setf nb (- long-end start))
        (multiple-value-bind (q r)
            (truncate nb +long-count-base+)
          (write-byte q fout)
          (write-byte r fout))
        (loop for ix from start below long-end do
              (write-byte (aref enc ix) fout))
        (setf start long-end)))
    (write-sequence +start-sequence+ fout)))
    
(defun read-self-sync (fin fout)
  (um:arun-fsm
      ;; bindings
      (ct nb max-ct
          (fout-start (file-position fout)))
      ;; feeder
      (read-byte fin)
    ;; states - first one is starting state
    (start (b)
       ;; looking for start pattern #xFD #xFE
       (when (eql b #xFD)
         (state check-start-fe)))
    (restart (b)
       (state start)
       (start b))
    (check-start-fe (b)
       (case b
         ((#xFE)
          (state read-short-count))
         (t
          (restart b))
         ))
    (read-short-count (b)
       (cond
         ((<= b +max-short-count+)
          ;; we got a valid short count byte
          (setf nb b
                ct b
                max-ct +max-short-count+)
          (file-position fout fout-start)
          (if (zerop ct)
              (state read-long-count)
            (state read-frag)))

         (t
          (restart b))
         ))
    (read-frag (b)
       (case b
         ((#xFD)
          (if (> ct 1)
              (state check-frag)
            ;; else
            (progn
              (write-byte b fout)
              (state read-long-count))
            ))
          (t
           (write-byte b fout)
           (when (zerop (decf ct))
             (state read-long-count)))
          ))
    (check-frag (b)
       (case b
         ((#xfe)
          ;; just saw a start pattern in the middle of our frag
          (state read-short-count))
         (t
          (write-byte #xfd fout)
          (decf ct)
          (state read-frag)
          (read-frag b))
         ))
    (read-long-count (b)
       (cond
        ((< b +long-count-base+)
         ;; we hit a long-count byte
         (when (< nb max-ct)
           (write-sequence +start-sequence+ fout))
         (setf nb b)
         (state read-long-count-2))
        
        ((eql b #xfd)
         (state chk-long-count))
        
        (t
         (restart b))
        ))
    (chk-long-count (b)
       (cond
        ((eql b #xfe)
         ;; we just saw the ending start pattern
         (finish t))
        (t
         (restart b))
        ))
    (read-long-count-2 (b)
       (cond
        ((< b +long-count-base+)
         (setf nb (+ b (* +long-count-base+ nb))
               ct nb
               max-ct +max-long-count+)
         (if (zerop ct)
             (state read-long-count)
           (state read-frag)))
        
        (t
         (restart b))
        ))
    ))

#|
(defun tst (&optional (n 1000))
  (loop repeat n do
        (let* ((data (loop repeat 1000 collect
                           (lw:mt-random (ash 1 128))))
               (enc  (loenc:encode data))
               (ser-enc (ubyte-streams:with-output-to-ubyte-stream (s)
                          (write-self-sync enc s)))
               (ser-dec (ubyte-streams:with-output-to-ubyte-stream (sout)
                          (ubyte-streams:with-input-from-ubyte-stream (sin ser-enc)
                            (read-self-sync sin sout)))))
          (princ #\.)
          (assert (and (= (length enc)
                          (length ser-dec))
                       (every #'eql enc ser-dec)))
          )))

(time (tst))
        
|#
