
(in-package #:self-sync)

(um:eval-always
  (import '(scatter-vec:make-scatter-vector
            scatter-vec:xlength
            scatter-vec:xaref
            scatter-vec:xposition
            scatter-vec:xdovec
            scatter-vec:xupdate-digest
            scatter-vec:add-fragment
            scatter-vec:xwrite-sequence
            
            ubyte-streams:make-ubyte-output-stream
            ubyte-streams:stream-bytes
            ubyte-streams:with-output-to-ubyte-stream
            ubyte-streams:with-input-from-ubyte-stream
            )))

;; ------------------------------------------------------------------
(defconstant +long-count-base+  #xfd)
(defconstant +max-short-count+  (1- +long-count-base+))
(defconstant +max-long-count+   (1- (* +long-count-base+ +long-count-base+)))
(defconstant +start-sequence+   #(#xfe #xfd))
(defconstant +version+          #x100)
;; ------------------------------------------------------------------
  
(defun find-start-seq (enc start end)
  (when (< start (1- end))
    (when-let (pos (xposition #xfe enc
                              :start start))
      (when (< pos (1- end))
        (if (eql #xfd (xaref enc (1+ pos)))
            pos
          (find-start-seq enc (1+ pos) end))
        ))))

(defun make-ubv4 ()
  (make-array 4
              :element-type '(unsigned-byte 8)))

(defun int-to-vec-le4 (n)
  (let ((ans (make-ubv4)))
    (um:nlet iter ((ix 0)
                   (n  n))
      (if (> ix 3)
          ans
        (multiple-value-bind (q r) (truncate n 256)
          (setf (aref ans ix) r)
          (go-iter (1+ ix) q)))
      )))

(defun vec-le4-to-int (vec)
  (let ((ans 0))
    (loop for ix from 3 downto 0 do
          (setf ans (um:ash-dpb ans 8 (xaref vec ix))))
    ans))

;; ------------------------------------------------------------------

(defun write-record (enc fout)
  (let* ((renc (make-scatter-vector))
         (ver  (int-to-vec-le4 +version+))
         (len  (int-to-vec-le4 (xlength enc)))
         (crc  (let ((dig (ironclad:make-digest :crc32)))
                 (xupdate-digest dig ver)
                 (xupdate-digest dig len)
                 (xupdate-digest dig enc)
                 (ironclad:produce-digest dig))))
    (add-fragment renc crc)
    (add-fragment renc ver)
    (add-fragment renc len)
    (add-fragment renc enc)
    (let* ((start     0)
           (end       (xlength renc))
           (pos       (find-start-seq renc 0 end))
           (max-ct    +max-short-count+)
           (short-end (min +max-short-count+ (or pos end)))
           (nb        short-end))
      (write-sequence +start-sequence+ fout)
      (write-byte nb fout)
      (xwrite-sequence renc fout :start 0 :end short-end)
      (setf start short-end)
      (um:while (< start end)
        (when (< nb max-ct)
          (incf start 2))
        (when (and pos
                   (< pos start))
          (setf pos (find-start-seq renc start end)))
        (setf max-ct +max-long-count+)
        (let ((long-end (min (+ start +max-long-count+) (or pos end))))
          (setf nb (- long-end start))
          (multiple-value-bind (q r)
              (truncate nb +long-count-base+)
            ;; in little-endian form
            (write-byte r fout)
            (write-byte q fout))
          (xwrite-sequence renc fout :start start :end long-end)
          (setf start long-end)))
      )))
    
;; ------------------------------------------------------------------

(defun make-fsm (finish-fn)
  (um:alet ((ct)
            (nb)
            (max-ct)
            (crc  (make-ubv4))
            (len  (make-ubv4))
            (ver  (make-ubv4))
            (cix)
            (vix)
            (lix)
            (fout (make-ubyte-output-stream)))
      (um:alet-fsm
          ;; states - first one is starting state
          ;; ------------------------------------
        ;; -------------------------------------
        (read-short-count
         (b)
         (cond
          ((and (integerp b)
                (<= b +max-short-count+))
           ;; we got a valid short count byte
           (setf nb  b
                 ct  b
                 max-ct +max-short-count+
                 cix 0
                 lix 0
                 vix 0)
           (file-position fout 0)
           (if (zerop ct)
               (state read-long-count)
             (state read-frag)))
          
          (t
           (resync b))
          ))
        ;; -----------------------------------
        (stuff
         (b)
         (cond
          ((< cix 4)
           (setf (aref crc cix) b)
           (incf cix))
          ((< vix 4)
           (setf (aref ver vix) b)
           (incf vix))
          ((< lix 4)
           (setf (aref len lix) b)
           (incf lix))
          (t
           (write-byte b fout))
          ))
        ;; -----------------------------------
        (sync
         (b)
         ;; looking for start pattern #xFD #xFE
         (case b
           ((:EOF)
            ;; re-init for fresh application of FSM
            (state read-short-count))
           ((#xFE)
            ;; first byte of start pattern
            (state check-start-fd))
           ))
        ;; -----------------------------------
        (resync
         (b)
         (state sync)
         (sync b))
        ;; -------------------------------------
        (check-start-fd
         (b)
         (case b
           ((#xFD)
            ;; 2nd byte of start pattern
            (state read-short-count))
           (t
            (resync b))
           ))
        ;; -------------------------------------
        (read-frag
         (b)
         (case b
           ((:EOF)
            ;; unexpected EOF - re-init for fresh start
            (state read-short-count))
           
           ((#xFE)
            (if (> ct 1)
                (state check-frag-fd)
              ;; else
              (progn
                (stuff b)
                (state read-long-count))
              ))
           (t
            (stuff b)
            (when (zerop (decf ct))
              (state read-long-count)))
           ))
        ;; -------------------------------------
        (check-frag-fd
         (b)
         (case b
           ((:EOF #xFD)
            ;; unexpected EOF or an unexpected start pattern
            ;; in the midst of our frag - so setup to start anew
            (state read-short-count))
           (t
            (stuff #xFE)
            (decf ct)
            (state read-frag)
            (read-frag b))
           ))
        ;; -------------------------------------
        (read-long-count
         (b)
         (cond
          ((and (integerp b)
                (< b +long-count-base+))
           ;; we hit a long-count byte
           (when (< nb max-ct)
             (stuff #xFE)
             (stuff #xFD))
           (setf nb b)
           (state read-long-count-2))
          
          (t
           (check-finished)
           (resync b))
          ))
        ;; -------------------------------------        
        (read-long-count-2
         (b)
         (cond
          ((and (integerp b)
                (< b +long-count-base+))
           (incf nb (* +long-count-base+ b))
           (setf ct     nb
                 max-ct +max-long-count+)
           (if (zerop ct)
               (state read-long-count)
             (state read-frag)))
          
          (t
           (resync b))
          ))
        ;; -------------------------------------        
        (check-finished
         ()
         (let* ((ans (stream-bytes fout))
                (chk (let ((dig (ironclad:make-digest :crc32)))
                       (ironclad:update-digest dig ver)
                       (ironclad:update-digest dig len)
                       (ironclad:update-digest dig ans)
                       (ironclad:produce-digest dig))))
           (when (and (equalp crc chk)
                      (= (vec-le4-to-int ver) #x100)
                      (= (vec-le4-to-int len) (length ans)))
             (funcall finish-fn ans))
           ))
        )))

;; ------------------------------------------------------------------

(defun make-reader (fin)
  (let (ans)
    (flet ((finish (vec)
             (setf ans vec)))
      (let ((mach (make-fsm #'finish)))
        (lambda ()
          (setf ans nil)
          (um:nlet iter ()
            (let ((b (read-byte fin nil :EOF)))
              (funcall mach b)
              (cond (ans)
                    ((eql b :EOF) :EOF)
                    (t  (go-iter))
                    ))))
        ))))

;; ------------------------------------------------------------------

(defun encode (vec)
  (with-output-to-ubyte-stream (sout)
    (write-record vec sout)))

(defun decode (vec)
  (with-input-from-ubyte-stream (sin vec)
    (funcall (make-reader sin))))

;; ------------------------------------------------------------------
#|
(defun tst (&optional (n 1000))
  (loop repeat n do
        (let* ((data (loop repeat 1000 collect
                           (lw:mt-random (ash 1 128))))
               (enc  (loenc:encode data))
               (ser-enc (encode enc))
               (ser-dec (decode ser-enc)))
          (princ #\.)
          (assert (and (= (length enc)
                          (length ser-dec))
                       (every #'eql enc ser-dec)))
          )))

(time (tst))

(defun cat-test ()
  (let* ((data (loop repeat 1000 collect
                     (lw:mt-random (ash 1 128))))
         (enc  (loenc:encode data))
         (ser-enc (with-output-to-ubyte-stream (sout)
                    (write-record enc sout)
                    (write-record enc sout)
                    (write-record enc sout))))
    (with-input-from-ubyte-stream (sin ser-enc)
      (let ((fsm (make-reader sin)))
        (dotimes (ix 3)
          (let ((ans (funcall fsm)))
            (assert (and (= (length enc)
                            (length ans))
                         (every #'eql enc ans)))
            ))))))
(cat-test)
|#

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
      (let ((block-fsm  (um:alet (len
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
        (labels ((fsm-finish (bytes)
                   (handler-bind ((error (lambda (c)
                                           (handle-message dispatcher
                                                           '(actors/internal-message/network:discard))
                                           (become-null-monitor :rd-actor))))
                     (funcall block-fsm bytes))
                   ))
          (let ((fsm (self-sync:make-fsm #'fsm-finish)))
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
                                             (self-sync:encode (pop buffers))
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
