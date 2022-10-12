
(in-package :com.ral.actors.encoding.self-sync)

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

            com.ral.useful-macros:nlet
            com.ral.useful-macros:ash-dpbf
            com.ral.useful-macros:while
            com.ral.useful-macros:alet
            com.ral.useful-macros:alet-fsm
            com.ral.useful-macros:when-let

            vec-repr:ub8
            vec-repr:ub8-vector
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
(defconstant +long-count-base+  #xFD)
(defconstant +max-short-count+  (1- +long-count-base+))
(defconstant +max-long-count+   (1- (* +long-count-base+ +long-count-base+)))
(defconstant +start-sequence+   #(#xFE #xFD))
;; ---------------------------------------------------------------

(defun make-ubv (size &rest args)
  (apply #'make-array size
         :element-type 'ub8
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
    
(defun encode (vec)
  (with-output-to-ubyte-stream (sout)
    (write-record vec sout)))

;; ---------------------------------------------------------------
;; Self-Sync Stream Decoding... done with mixed Actors and Functions
;;
;; Finalized decodings are sent immediately to the postprocessing
;; pipline when recognized. No wait for an additional byte of
;; termination code. This is suitable for streaming data of indefinite
;; length.
;;
;; Streamed data typically arrives in packets, which must be kept in
;; chronologial order of arrival, and then scanned by the state
;; machine for decoding.
;;
;; The code in pure Actors turns out to be quite tricky, in the face
;; of fully parallel concurrency.  Explicit serialization of
;; operations must be performed, using continuation Actors, to prevent
;; jumbled ordering in the decodings arising from multiple threads all
;; participating at differing rates.
;;
;; Using functions in place of Actors for the state machine
;; automatically provides the correct seequencing due to call/return
;; semantics. And it runs faster too.
;;
;; The violation of FPL pure behavior code is really okay here because
;; it can be proven that only one thread will ever be running the FSM,
;; by virtue of the serialized access through stream-decoder.
;;
;; --------------------------------------------------------
;;
;; A state machine in call/return semantics, providing an Actor shell
;; for external use.

(defun ssfsm-beh (dest aout stuff-fn)
  #F
  (declare (function stuff-fn)
           ((ub8-vector *) aout))
  (let (state          ;; machine state function
        need-fefd      ;; when T we may need to insert #xFEFD
        (remct 0)      ;; segment bytes remaining
        crcv           ;; copy of 4-byte CRC field
        lenv           ;; copy of 4-byte length field
        (nel   -1))    ;; expected message length
    (declare (fixnum remct nel))
    (macrolet ((new-state (fn)
                 `(setf state #',fn)))
      (labels (;; --------------------
               ;; Utility Functions
               (subrange-code? (b)
                 (declare (ub8 b))
                 (<= 0 b #xFC))
               
               (stuffer-init (ct)
                 (declare (fixnum ct))
                 (setf nel       -1
                       remct     ct
                       need-fefd (< ct +max-short-count+)
                       (fill-pointer aout) 0)
                 (new-state read-segm))

               (segm-init (ct)
                 (declare (fixnum ct))
                 (setf remct     ct
                       need-fefd (< ct +max-long-count+))
                 (new-state read-segm)
                 (check-finish))

               (raw-stuff (b)
                 (funcall stuff-fn b aout))
               
               (stuff (b)
                 (raw-stuff b)
                 (decf remct)
                 (check-finish))

               (check-finish ()
                 (when (zerop remct)
                   (when (minusp nel)
                     (setf crcv (subseq aout 0 4)
                           lenv (subseq aout 4 8)
                           nel  (+ 8 (vec-le4-to-int lenv))))
                   (let ((nbuf  (length aout)))
                     (declare (fixnum nbuf))
                     (when (and need-fefd
                                (< nbuf nel))
                       (raw-stuff #xFE)
                       (raw-stuff #xFD)
                       (incf nbuf 2))
                     (cond ((< nbuf nel)
                            (new-state read-long-count))
                           (t
                            (new-state start)
                            (let* ((ans (subseq aout 8))
                                   (chk (crc32 lenv ans)))
                              (when (equalp crcv chk)
                                (send dest ans))
                              ))
                           ))))

               (restart (b)
                 (new-state start)
                 (start b))

               (inhale (b)
                 (funcall state b))
               
               ;; ----------------
               ;; Machine States
               (start (b)
                 (declare (ub8 b))
                 (when (eql b #xFE)
                   (new-state check-start-fd)))
               
               (check-start-fd (b)
                 (declare (ub8 b))
                 (if (eql b #xFD)
                     (new-state check-version)
                   (restart b)))

               (check-version (b)
                 (declare (ub8 b))
                 (if (eql b #x01)
                     (new-state read-short-count)
                   (restart b)))

               (read-short-count (b)
                 (if (subrange-code? b)
                     (stuffer-init b)
                   (restart b)))

               (read-segm (b)
                 (declare (ub8 b))
                 (if (and (eql b #xFE)
                          (> remct 1))
                     (new-state check-segm-fd)
                   (stuff b)))

               (check-segm-fd (b)
                 (declare (ub8 b))
                 (cond ((eql b #xFD) ;; we just saw a start pattern #xFE #xFD
                        (new-state check-version))
                       (t
                        (stuff #xFE)
                        (new-state read-segm)
                        (read-segm b))
                       ))

               (read-long-count (b)
                 (declare (ub8 b))
                 (cond ((subrange-code? b)
                        (setf remct b)
                        (new-state read-long-count-2))
                       (t
                        (restart b))
                       ))

               (read-long-count-2 (b)
                 (declare (ub8 b))
                 (if (subrange-code? b)
                     (segm-init (+ remct (* b +long-count-base+)))
                   (restart b))))
        
        (new-state check-version) ;; initialize state
        
        ;; finally... we get to the Actor behavior function
        (lambda (cust buf)
          (declare ((array ub8 *) buf))
          (map nil #'inhale buf)
          (send cust :next))
        ))))

(defun decoder-fsm (dest &key max-reclen)
  (let (aout
        stuff-fn)
    (if max-reclen
        (setf aout (make-ubv (+ max-reclen 8)
                             :fill-pointer 0)
              stuff-fn #'vector-push)
      ;; else
      (setf aout (make-ubv 256
                           :fill-pointer 0
                           :adjustable   t)
            stuff-fn #'vector-push-extend))
    (create (ssfsm-beh dest aout stuff-fn))
    ))

;; -----------------------------------------------------------------
;; Stream Decoding

(def-beh stream-decoder-beh (fsm wait-ix queue)
  ((:deliver bufix buf)
   (cond ((eql bufix wait-ix)
          (send fsm self buf)
          (become (busy-stream-decoder-beh fsm (1+ bufix) queue)))
          
         (t
          (become (stream-decoder-beh fsm wait-ix (acons bufix buf queue))))
         ))

  ((:go-silent)
   (become (sink-beh))))

(def-beh busy-stream-decoder-beh (fsm wait-ix queue)
  ((:next)
   (let ((pair (assoc wait-ix queue)))
     (cond (pair
            (send fsm self (cdr pair))
            (become (busy-stream-decoder-beh fsm (1+ wait-ix) (remove pair queue))))
           (t
            (become (stream-decoder-beh fsm wait-ix queue)))
           )))

  ((:deliver bufix buf)
   (become (busy-stream-decoder-beh fsm wait-ix (acons bufix buf queue))))

  ((:go-silent)
   (become (sink-beh))))

(defun stream-decoder (dest)
  ;; Construct an Actor that absorbs chunks of self-sync encoded input
  ;; stream and which triggers events to the dest actor as completed
  ;; decodings arrive.
  ;;
  ;; Incoming packets (chunks of encodings) are delivered along with a
  ;; chronological sequence number. Packets can arrive in any order,
  ;; starting from 1.
  ;;
  (let ((fsm (decoder-fsm dest)))
    (create (stream-decoder-beh fsm 1 nil))
    ))

(defun decode (vec)
  ;; decoding (as a function) for self-contained encoded vectors
  (let* ((mbox    (mp:make-mailbox))
         (mba     (mbox-sender mbox))
         (decoder (stream-decoder mba)))
    (send decoder :deliver 1 vec)
    (car (mp:mailbox-read mbox))))

;; ---------------------------------------------------------------------------
#|
(setf s  (hcl:file-binary-bytes "taxes.lisp"))
(setf se (encode s))
(map 'string #'code-char (self-sync:decode se))
(map 'string #'code-char (decode se))

(let ((se (encode (loenc:encode #(0 1 2 3 4 5)))))
  (print se)
  (decode se))

(let* ((s  (make-ubv 9
                     :initial-contents '(0 1 2 3 4 5 #xFE #xFE #xFD)))
       (se (encode s))
       (dec (stream-decoder writeln)))
  (print se)
  (send dec :deliver 1 se))

  (decode se))

(self-sync:decode (self-sync:encode (loenc:encode #(0 1 2 3 4 5))))

(defun encser (ekey &rest objs)
  (let* ((enc        (apply #'loenc:encode (coerce objs 'vector)))
         (cmpr       (compress enc))
         (seq        (make-nonce))
         (emsg       (encrypt ekey seq cmpr))
         (sig        (sign ekey seq emsg))
         (packet     (vector seq emsg sig))
         (enc-packet (loenc:encode packet)))
    (self-sync:encode enc-packet)))

(setf s (hcl:file-string "taxes.lisp"))
(setf se (loenc:encode s))
(setf sess (encode se))

(let ((out (stream-decoder
            (sink-pipe (printer)
                       (marshal-decoder)
                       (tee (α (&rest args)
                                ;; (break)
                                ;; (inspect args)
                                (assert (stringp (car args)))
                                (assert (eql (length (car args)) (length s)))
                                (assert (every #'char= (car args) s)))))
                       println))))
  (labels ((parser (ct)
             (α (x)
               (let ((nel (min 100 (length x))))
                 (when (plusp nel)
                   (send out :deliver ct (subseq x 0 nel))
                   (incf ct)
                   (send self (subseq x nel)))
                 ))))
    (send (sink-pipe (marshal-encoder) (printer)
                     (self-sync-encoder) (printer)
                     (parser 1))
        s)))

(let ((out (stream-decoder
            (sink-pipe (printer)
                       (marshal-decoder)
                       (tee (α (cust &rest args)
                               (assert (stringp (car args)))
                               (assert (eql (length (car args)) (length s)))
                               (assert (every #'char= (car args) s))))
                       println))))
  (labels ((parser ()
             (α (x)
               (let* ((nel (length x))
                      (nb  (truncate nel 10))
                      (frags (loop for ix from 1
                                   for offs from 0 by nb below nel
                                   collect
                                     (let ((end (min (+ offs nb) nel)))
                                       (list ix (subseq x offs end))))))
                 (dolist (frag (nreverse frags))
                   (send* out :deliver frag))
                 ))))
    (send (sink-pipe (marshal-encoder) (printer)
                     (self-sync-encoder) (printer)
                     (parser))
        s)))

(functionp (lambda (x) x))

(send (sink-pipe (marshal-encoder)
                 (marshal-decoder)
                 println)
      s)
 |#
