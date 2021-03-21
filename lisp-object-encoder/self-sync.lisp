
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
    
(defun make-self-sync-fsm (fout finish-fn)
  (um:alet (ct nb max-ct
               (fout-start (file-position fout)))
      (um:alet-fsm
          ;; states - first one is starting state
          ;; ------------------------------------
          (start
           (b)
           ;; looking for start pattern #xFD #xFE
           (when (eql b #xFD)
             (state check-start-fe)))
          ;; -----------------------------------
          (restart
           (b)
           (state start)
           (start b))
        ;; -------------------------------------
        (check-start-fe
         (b)
         (case b
           ((#xFE)
            (state read-short-count))
           (t
            (restart b))
           ))
        ;; -------------------------------------
        (read-short-count
         (b)
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
        ;; -------------------------------------
        (read-frag
         (b)
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
        ;; -------------------------------------
        (check-frag
         (b)
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
        ;; -------------------------------------
        (read-long-count
         (b)
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
        ;; -------------------------------------
        (chk-long-count
         (b)
         (cond
          ((eql b #xfe)
           ;; we just saw the ending start pattern
           (state start)
           (funcall finish-fn t))
          
          (t
           (restart b))
          ))
        ;; -------------------------------------        
        (read-long-count-2
         (b)
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
        ;; -------------------------------------        
        )))

(defun #1=read-self-sync (fin fout)
  (flet ((finish (_)
           (declare (ignore _))
           (return-from #1#)))
    (let ((fsm (make-self-sync-fsm fout #'finish)))
      (loop for b = (read-byte fin nil fin)
            until (eql b fin)
            do
            (funcall fsm b))
      )))

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

(defun encoding (vec)
  (ubyte-streams:with-output-to-ubyte-stream (sout)
    (write-self-sync vec sout)))

(defun decoding (vec)
  (ubyte-streams:with-output-to-ubyte-stream (sout)
    (ubyte-streams:with-input-from-ubyte-stream (sin vec)
      (read-self-sync sin sout))))

#|
(let* ((data '("this" is #\a :test))
       (enc  (loenc:encode data))
       (len-enc (loenc:encode (length enc)))
       (hash-enc (core-crypto:vec (core-crypto:hash/256 enc))))
  (list (encoding len-enc)
        (encoding enc)
        (encoding hash-enc)))
 |#
;; ------------------------------------------------------------
#|
(define-condition bad-count (error)
  ()
  (:report "Bad count prefix in network traffic"))

(define-condition bad-data (error)
  ()
  (:report "Bad data block in network traffic"))

(define-condition bad-hmac (error)
  ()
  (:report "Bad HMAC block in network traffic"))

(defmethod initialize-instance :after ((reader message-reader) &key &allow-other-keys)
  (with-as-current-actor reader
    (with-slots (crypto dispatcher) reader
      (let ((fsout (ubyte-streams:make-ubyte-output-stream))
            (block-fsm  (um:alet (len
                                  len-buf
                                  enc-buf
                                  hmac-buf)
                            (um:alet-fsm
                                (start
                                 (bytes)
                                 (unless (= 4 (length bytes))
                                   (error 'bad-count))
                                 (setf len (convert-vector-to-integer bytes))
                                 (when (> len +MAX-FRAGMENT-SIZE+)
                                   (error 'bad-count))
                                 (setf len-buf bytes)
                                 (state get-data-bytes))
                                ;; -------------------------------
                                (get-data-bytes
                                 (bytes)
                                 (unless (= len (length bytes))
                                   (error 'bad-data))
                                 (setf enc-nbuf bytes)
                                 (state get-hmac-bytes))
                              ;; ---------------------------------
                              (get-hmac-bytes
                               (bytes)
                               (unless (= 32 (length bytes))
                                 (error 'bad-hmac))
                               (setf hmac-buf bytes)
                               (state start)
                               (handle-message dispatcher (secure-decoding crypto len len-buf enc-buf hmac-buf)))
                              ))))
        (labels ((fsm-finish (_)
                   (declare (ignore _))
                   (let ((bytes (stream-bytes fsout)))
                     (handler-bind ((error (lambda (c)
                                             (handle-message dispatcher
                                                             '(actors/internal-message/network:discard))
                                             (become-null-monitor :rd-actor))))
                       (funcall block-fsm bytes))
                     )))
          (let ((fsm (self-sync:make-self-sync-fsm fsout #'fsm-finish)))
            (recv ()
              
              (actors/internal-message/network:rd-incoming
               (frag)
               (destructuring-bind (frag-start frag-end . frag-bytes) frag-start
                 (loop for ix from frag-start below frag-end do
                       (funcall fsm (aref frag-bytes ix)))
                 (retry-recv)))
              
              (actors/internal-message/network:rd-error
               ()
               (become-null-monitor :rd-actor))
              ))
          )))))

(defmethod write-message ((writer message-writer) buffers)
  (with-slots (io-state io-running decr-io-count) writer
    (labels
        ((we-are-done ()
           (become-null-monitor :wr-actor))

         (transmit-next-buffer (state)
           (comm:async-io-state-write-buffer state
                                             (self-sync:encoding (pop buffers))
                                             #'write-next-buffer))
           
         (write-next-buffer (state &rest ignored)
           ;; this is a callback routine, executed in the thread of
           ;; the async collection
           (declare (ignore ignored))
           (cond ((comm:async-io-state-write-status state)
                  (send writer 'actors/internal-message/network:wr-fail))
                 (buffers
                  (transmit-next-buffer state))
                 (t
                  (send writer 'actors/internal-message/network:wr-done))
                 )))
      
      (perform-in-actor writer
        (cond
         ((sys:compare-and-swap (car io-running) 1 2) ;; still running recieve?
          (transmit-next-buffer io-state)
          (recv ()
            (actors/internal-message/network:wr-done ()
              (when (zerop (funcall decr-io-count io-state))
                (we-are-done)))
            (actors/internal-message/network:wr-fail ()
              (funcall decr-io-count io-state)
              (we-are-done))
            ))
         (t
          (we-are-done))
         )))))
|#
