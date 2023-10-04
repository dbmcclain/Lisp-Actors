
(in-package #:com.ral.self-sync)

(um:eval-always
  (import '(com.ral.scatter-vec:make-scatter-vector
            com.ral.scatter-vec:xlength
            com.ral.scatter-vec:xaref
            com.ral.scatter-vec:xposition
            com.ral.scatter-vec:xdovec
            com.ral.scatter-vec:xupdate-digest
            com.ral.scatter-vec:add-fragment
            com.ral.scatter-vec:xwrite-sequence
            
            com.ral.ubyte-streams:make-ubyte-output-stream
            com.ral.ubyte-streams:stream-bytes
            com.ral.ubyte-streams:with-output-to-ubyte-stream
            com.ral.ubyte-streams:with-input-from-ubyte-stream

            com.ral.useful-macros:nlet
            com.ral.useful-macros:ash-dpbf
            com.ral.useful-macros:while
            com.ral.useful-macros:alet
            com.ral.useful-macros:alet-fsm
            )))

;; ------------------------------------------------------------------
;; Encoding
;;   +-----------+---------+------------+---------------+---------------+--//--+
;;   | #xFE #xFD | VER (1) | NSHORT (1) | Short Segment | Long Segments | ...  |
;;   +-----------+---------+------------+---------------+---------------+--//--+
;;
;;     Short Segment
;;     +-----------+-------------+------------------------+
;;     | CRC32 (4) | Enc Len (4) | First Bytes (NSHORT-8) |
;;     +-----------+-------------+------------------------+
;;
;;     Long Segment
;;     +---------+----------+------------+
;;     | Rem (1) | Quot (1) | More Bytes |
;;     +---------+----------+------------+
;;
;; Segments are msg chunks located between sequences of #xFE #xFD (the start seq).
;; Short Segment has max length of #xFC = 252 bytes.
;; Long Segments have max length of 252*252-1 = 63503 bytes.
;; Enocding has embedded start sequences elided - replaced at segment boundaries upon decoding.
;; No valid internal encodings have start sequences embedded.
;; CRC32 is over the 4 byte Enc Len + original message bytes.
;; Damaged encodings can be resync'd at next start sequence.
;;
;; ------------------------------------------------------------------
(defconstant+ +long-count-base+  #xFD)
(defconstant+ +max-short-count+  (1- +long-count-base+))
(defconstant+ +max-long-count+   (1- (* +long-count-base+ +long-count-base+)))
(defconstant+ +start-sequence+   #(#xFE #xFD))
;; ------------------------------------------------------------------
;; Note: Use of XAREF, XPOSITION, XLENGTH, XWRITE-SEQUENCE, allows for
;; scatter-gather vectors. Hopefully more efficient than copying and
;; concatenating vectors.

(defun find-start-seq (enc start end)
  (when (< start (1- end))
    (when-let (pos (xposition #xFE enc
                              :start start
                              :end   (1- end)))
      (if (eql #xFD (xaref enc (1+ pos)))
          pos
        (find-start-seq enc (1+ pos) end))
      )))

(defun make-ubv (size &rest args)
  (apply #'make-array size
         :element-type '(unsigned-byte 8)
         args))

(defun int-to-vec-le4 (n)
  (let ((ans (make-ubv 4)))
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
;; Self-Sync Record Writing - soft migration to new versions is
;; possible so long as every version has the same starting sequence
;; #(#xFE #xFD <versionByte>). Beyond that can be version dependent.

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
      (write-byte #x01 fout) ;; this code encodes Version 1
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
;;

(um:eval-always
  (defun %fsm-case (arg case)
    (destructuring-bind (pred form) case
      (if (consp pred)
          `(,pred ,form)
        (if (and (symbolp pred)
                 (string= (string pred) "_"))
            `(t ,form)
          `((eql ,arg ,pred) ,form)))))
  
  (defun %fsm-state (clause)
    (destructuring-bind (name arg &rest cases) clause
      `(,name ,arg
              (cond
               ,@(mapcar (um:curry #'%fsm-case (car arg))
                         (um:group cases 2))))
      )))

(um:defmacro! fsm-states (&rest clauses)
  (let ((first-name (caar clauses)))
    `(macrolet ((,a!state (s)
                `(setq ,',a!this #',s)))
       (labels
           ,(mapcar #'%fsm-state clauses)
         #',first-name))
    ))

;; ------------------------------------------------------------------

(defun make-reader-fsm (finish-fn &key max-reclen)
  (alet ((ct)     ;; remaining count of bytes to stuff
         (needs-fefd?)
         (aout (if max-reclen ;; max allowed record size
                   (make-ubv (+ max-reclen 8)
                             :fill-pointer 0)
                 (make-ubv 256 ;; stuffer accumulator
                           :fill-pointer 0
                           :adjustable   t)))
         (pusher (if max-reclen #'vector-push #'vector-push-extend)))
      (labels
          ;; ------------------------------------
          ;; helper functions
          ;; ------------------------------------
          ((init-buf (b)
             ;; called at start of short frag
             (setf ct  b
                   needs-fefd? (< ct +max-short-count+)
                   (fill-pointer aout) 0))
           ;; -----------------------------------
           (stuff (b)
             ;; byte stuffer
             (funcall pusher b aout)
             (decf ct))
           ;; -----------------------------------
           (maybe-stuff-fefd ()
             ;; if frag was shorter than mox possible length
             ;; then it was because a start sequence was found
             ;; in the data - so put it back.
             (when needs-fefd?
               (stuff #xFE)
               (stuff #xFD)))
           ;; -----------------------------------
           (init-frag (b)
             ;; called at start of long frag
             (incf ct (* +long-count-base+ b))
             (setf needs-fefd? (< ct +max-long-count+)))
           ;; -------------------------------------        
           (check-finish ()
             ;; see if we've found the end of a record
             (when (>= (length aout) 8)
               (let* ((crc (subseq aout 0 4))
                      (len (subseq aout 4 8))
                      (ans (subseq aout 8))
                      (chk (crc32 len ans)))
                 (when (and (equalp crc chk)
                            (= (vec-le4-to-int len) (length ans)))
                   (funcall finish-fn ans))
                 ))))
        ;; ------------------------------------------------------
        (fsm-states
         ;; ------------------------------------
         ;; states - first one is starting state
         ;; All clauses end with setting state, or calling something which does.
         ;; ------------------------------------
         (start  ;; check version
          (b)
          #x01          (state read-short-count)
          ;; additional versions splay out from here
          _             (sync b))
         
         (sync  ;; find a start sequence
          (b)   
          :EOF          (state start)
          #xFE          (state check-start-fd)
          _             (state sync))
         
         (check-start-fd
          (b)
          #xFD          (state start)
          _             (sync b))
         
         (read-short-count ;; first frag is always short
          (b)
          :EOF          (state start) ;; frag was truncated
          (<= 0 b #xFC) (progn (init-buf b) (state read-frag))
          _             (sync b))     ;; frag was clobbered
         
         (read-long-count  ;; remaining frags are long
          (b)
          :EOF          (progn (check-finish) (state start))  ;; end of record / end of file
          (<= 0 b #xFC) (progn (maybe-stuff-fefd) (setf ct b) (state read-long-count-2))
          _             (progn (check-finish) (sync b)))      ;; end of record, start of new?
         
         (read-long-count-2
          (b)
          :EOF          (state start) ;; the frag was truncated
          (<= 0 b #xFC) (progn (init-frag b) (state read-frag))
          _             (sync b))     ;; the frag was clobbered
         
         (read-frag
          (b)
          (zerop ct)    (read-long-count b)
          :EOF          (state start) ;; the frag was truncated
          (= ct 1)      (progn (stuff b) (state read-long-count))
          #xFE          (state check-frag-fd)
          _             (progn (stuff b) (state read-frag)))
          
         (check-frag-fd
          (b)
          #xFD          (state start)  ;; the frag was clobbered - just saw another start seq
          _             (progn (stuff #xFE) (read-frag b)))
         ))))

;; ------------------------------------------------------------------

(defun make-reader (fin &rest args)
  (let* ((ans)
         (mach  (flet ((finish (vec)
                         (setf ans vec)))
                  (apply #'make-reader-fsm #'finish args))))
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
#|
(defun make-reader-fsm (finish-fn &key max-reclen)
  ;; try a version, inspired by Prolog implementation, where all
  ;; states have state info held locally.
  (alet ()
      (labels
          ((make-buf (len)
             (make-ubv (or max-reclen
                           (max len 256))
                       :fill-pointer 0
                       :adjustable   (not max-reclen)))
           (stuff (b buf)
             (if max-reclen
                 (vector-push b buf)
               (vector-push-extend b buf)))
           (check-finish (buf)
             ;; see if we've found the end of a record
             (when (>= (length buf) 8)
               (let* ((crc (subseq buf 0 4))
                      (len (subseq buf 4 8))
                      (ans (subseq buf 8))
                      (chk (crc32 len ans)))
                 (when (and (equalp crc chk)
                            (= (vec-le4-to-int len) (length ans)))
                   (funcall finish-fn ans))
                 ))))
        (fsm-states
         ;; ------------------------------------
         ;; states - first one is starting state
         ;; All clauses end with setting state, or calling something which does.
         ;; ------------------------------------
         (start  ;; check version
           (b)
           
           #x01          (state read-short-count)
           ;; additional versions splay out from here
           _             (sync b))   
         ;; --------------------------
         (sync  ;; find a start sequence
           (b)
           
           :EOF          (state start)
           #xFE          (state check-start-fd)
           _             (state sync))
         ;; --------------------------
         (check-start-fd
          (b)
          
          #xFD          (state start)
          _             (sync b))
         ;; --------------------------
         (read-short-count ;; first frag is always short
          (ct)
     
          :EOF           (state start) ;; frag was truncated
          (<= 0 ct #xFC) (let ((needs-fefd (< ct #xFC))
                               (buf        (make-buf ct)))
                           (state (lambda (b)
                                    (read-frag b needs-fefd buf ct))))
          _              (sync ct))     ;; frag was clobbered
         ;; --------------------------
         (read-long-count  ;; remaining frags are long
          (b0 needs-fefd buf)
                           
          :EOF           (progn (check-finish buf) (state start))  ;; end of record / end of file
          (<= 0 b0 #xFC) (progn
                           (when needs-fefd
                             (stuff #xFE buf)
                             (stuff #xFD buf))
                           (state (lambda (b1)
                                    (read-long-count-2 b1 buf b0))))
          _              (progn (check-finish buf) (sync b0)))      ;; end of record, start of new?
         ;; --------------------------
         (read-long-count-2
          (b1 buf b0)
          
          :EOF           (state start) ;; the frag was truncated
          (<= 0 b1 #xFC) (let* ((ct         (+ b0 (* #xFD b1)))
                                (needs-fefd (< ct #.(1- (* #xFD #xFD)))))
                           (state (lambda (b)
                                    (read-frag b needs-fefd buf ct))))
          _              (sync b1))     ;; the frag was clobbered
         ;; --------------------------
         (read-frag
          (b needs-fefd buf ct)
          
          (zerop ct)    (read-long-count b needs-fefd buf)
          :EOF          (state start) ;; the frag was truncated
          (= ct 1)      (progn
                          (stuff b buf)
                          (state (lambda (b)
                                   (read-long-count b needs-fefd buf))))
          #xFE          (state (lambda (b)
                                 (check-frag-fd b needs-fefd buf ct)))
          _             (progn
                          (stuff b buf)
                          (state (lambda (b)
                                   (read-frag b needs-fefd buf (1- ct)))
                                 )))
         ;; --------------------------         
         (check-frag-fd
          (b needs-fefd buf ct)
          
          #xFD          (state start)  ;; the frag was clobbered - just saw another start seq
          _             (progn
                          (stuff #xFE buf)
                          (read-frag b needs-fefd buf (1- ct))))
         ))))
|#
