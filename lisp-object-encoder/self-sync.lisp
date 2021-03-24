
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

            useful-macros:nlet
            useful-macros:ash-dpbf
            useful-macros:while
            useful-macros:alet
            useful-macros:alet-fsm
            )))

;; ------------------------------------------------------------------
(defconstant +long-count-base+  #xfd)
(defconstant +max-short-count+  (1- +long-count-base+))
(defconstant +max-long-count+   (1- (* +long-count-base+ +long-count-base+)))
(defconstant +start-sequence+   #(#xfe #xfd))
(defconstant +version+          1)
;; ------------------------------------------------------------------
  
(defun find-start-seq (enc start end)
  (when (< start (1- end))
    (when-let (pos (xposition #xfe enc
                              :start start
                              :end   (1- end)))
      (if (eql #xfd (xaref enc (1+ pos)))
          pos
        (find-start-seq enc (1+ pos) end))
      )))

(defun make-ubv4 ()
  (make-array 4
              :element-type '(unsigned-byte 8)))

(defun int-to-vec-le4 (n)
  (let ((ans (make-ubv4)))
    (nlet iter ((ix 0)
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
          (ash-dpbf ans 8 (xaref vec ix)))
    ans))

(defun crc32 (&rest vecs)
  (let ((dig (ironclad:make-digest :crc32)))
    (dolist (vec vecs)
      (xupdate-digest dig vec))
    (ironclad:produce-digest dig)))

;; ------------------------------------------------------------------

(defun write-record (enc fout)
  (let* ((renc (make-scatter-vector))
         (len  (int-to-vec-le4 (xlength enc)))
         (crc  (crc32 len enc)))
    (add-fragment renc crc)
    (add-fragment renc len)
    (add-fragment renc enc)
    (let* ((start     0)
           (end       (xlength renc))
           (pos       (find-start-seq renc 0 end))
           (max-ct    +max-short-count+)
           (short-end (min +max-short-count+ (or pos end)))
           (nb        short-end))
      (write-sequence +start-sequence+ fout)
      (write-byte +version+ fout)
      (write-byte nb fout)
      (xwrite-sequence renc fout :start 0 :end short-end)
      (setf start short-end)
      (while (< start end)
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
;; State Machine for reading self-sync encoded data
;; Input Alphabet: [#x00-#xFF, :EOF]

(defun reader-fsm (finish-fn)
  (alet ((ct)
         (nb)
         (max-ct)
         (crc  (make-ubv4))
         (len  (make-ubv4))
         (cix)
         (lix)
         (fout (make-ubyte-output-stream)))
      (alet-fsm
        ;; ------------------------------------
        ;; states - first one is starting state
        ;; ------------------------------------
        (read-version
         (b)
         (case b
           ((#x01) ;; this state machine is for Version 1
            (state read-short-count))
           
           ;; future versions will splay from here
           (t
            (resync b))
           ))
        ;; -------------------------------------
        (read-short-count
         (b)
         (cond
          ((and (integerp b)
                (<= b +max-short-count+))
           ;; we got a valid short count byte
           (init-buffers b))
          
          (t
           (resync b))
          ))
        ;; -----------------------------------
        (sync
         (b)
         ;; looking for start pattern #xFD #xFE
         (case b
           ((:EOF)
            ;; re-init for fresh application of FSM
            (state read-version))

           ((#xFE)
            ;; first byte of start pattern
            (state check-start-fd))
           ))
        ;; -------------------------------------
        (check-start-fd
         (b)
         (case b
           ((#xFD)
            ;; 2nd byte of start pattern
            (state read-version))
           
           (t
            (resync b))
           ))
        ;; -------------------------------------
        (read-frag
         (b)
         (cond 
           ((eql b :EOF)
            ;; unexpected EOF - re-init for fresh start
            (state read-version))
           
           ((and (eql b #xFE)
                 (> ct 1))
            (state check-frag-fd))

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
            (state read-version))
           
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
           (maybe-stuff-fefd)
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
           (setup-long-frag-count b))
          
          (t
           (resync b))
          ))
        ;; -----------------------------------
        ;; end of states - start of helpers
        ;; -----------------------------------
        (init-buffers
         (b)
         (setf nb  b
               ct  b
               max-ct +max-short-count+
               cix 0
               lix 0)
         (file-position fout 0)
         (start-frag))
        ;; -----------------------------------
        (stuff
         (b)
         (cond
          ((< cix 4)
           (setf (aref crc cix) b)
           (incf cix))
          ((< lix 4)
           (setf (aref len lix) b)
           (incf lix))
          (t
           (write-byte b fout))
          ))
        ;; -----------------------------------
        (maybe-stuff-fefd
         ()
         (when (< nb max-ct)
           (stuff #xFE)
           (stuff #xFD)))
        ;; -----------------------------------
        (setup-long-frag-count
         (b)
         (incf nb (* +long-count-base+ b))
         (setf ct     nb
               max-ct +max-long-count+)
         (start-frag))
        ;; -----------------------------------
        (start-frag
         ()
         (if (zerop ct)
             (state read-long-count)
           (state read-frag)))
        ;; -----------------------------------
        (resync
         (b)
         (state sync)
         (sync b))
        ;; -------------------------------------        
        (check-finished
         ()
         (let* ((ans (stream-bytes fout))
                (chk (crc32 len ans)))
           (when (and (equalp crc chk)
                      (= (vec-le4-to-int len) (length ans)))
             (funcall finish-fn ans))
           ))
        )))

;; ------------------------------------------------------------------

(defun make-reader (fin)
  (let* ((ans)
         (mach  (flet ((finish (vec)
                         (setf ans vec)))
                  (reader-fsm #'finish))))
    (lambda ()
      (setf ans nil)
      (nlet iter ()
        (let ((b (read-byte fin nil :EOF)))
          (funcall mach b)
          (cond (ans)
                ((eql b :EOF) :EOF)
                (t  (go-iter))
                ))))
    ))

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
  (list (encode len-enc)
        (encode enc)
        (encode hash-enc)))
 |#
;; ------------------------------------------------------------
