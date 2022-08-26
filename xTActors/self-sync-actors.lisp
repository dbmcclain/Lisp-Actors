
(defpackage :ssact
  (:use :cl :ac)
  (:export
   #:encode
   #:stream-decoder
   ))

(in-package :ssact)

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
            useful-macros:when-let
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
;; Finalized decodings are sent to the postprocessing pipline,
;; immediately when recognized. No wait for an additional byte of
;; termination code. This is suitable for streaming data of indefinite
;; length.
;;
;; Streamed data typically arrives in packets, which must be kept in
;; chronologial order of arrival, and then scanned by the state
;; machine for decoding.
;;
;; The code is pure Actors turns out to be quite tricky, in the face
;; of fully parallel concurrency.  Explicit serialization of
;; operations must be performed to prevent jumbled ordering in the
;; decodings arising from multiple threads all participating at
;; differing rates.
;;
;; Using functions in place of Actors for the state machine
;; automatically provides the correct seequencing due to call/return
;; semantics.

(defun stuffer (aout stuff-fn dest)
  (let (needs-fefd?
        crcv
        lenv
        nel
        (done t))
    (um:dlambda
      (:init (size)
       (setf needs-fefd? (< size +max-short-count+)
             crcv        nil
             lenv        nil
             nel         nil
             done        nil
             (fill-pointer aout) 0))

      (:init-frag (size)
       (unless done
         (setf needs-fefd? (< size +max-long-count+))))

      (:stuff (b)
       (unless done
         (funcall stuff-fn b aout)))

      (:check-finish ()
       (unless done
         (when needs-fefd?
           (funcall stuff-fn #xFE aout)
           (funcall stuff-fn #xFD aout))
         (let ((nbuf (length aout)))
           (when (>= nbuf 8)
             (let* ((crc (or crcv
                             (setf crcv (subseq aout 0 4))))
                    (len (or lenv
                             (setf lenv (subseq aout 4 8))))
                    (nb  (or nel
                             (setf nel (vec-le4-to-int len)))))
               (when (>= nbuf (+ nb 8))
                 (let* ((ans (subseq aout 8 (+ nb 8)))
                        (chk (crc32 len ans)))
                   (when (equalp crc chk)
                     (setf done t)
                     (send dest ans))
                   ))
               ))
           )))
      )))

(defun make-stuffer (dest max-reclen)
  (if max-reclen
      (stuffer (make-ubv (+ max-reclen 8)
                         :fill-pointer 0)
              #'vector-push
              dest)
    ;; else
    (stuffer (make-ubv 256
                       :fill-pointer 0
                       :adjustable   t)
            #'vector-push-extend
            dest)))

;; --------------------------------------------------------
;; A state machine in call/return semantics, providing an Actor shell
;; for external use.

(defun ssfsm (stuffer)
  (let (state
        ct)
    (macrolet ((new-state (fn)
                 `(setf state #',fn)))
      (labels ((subrange-code-p (b)
                 (<= 0 b #xFC))
               (start (b)
                 (when (eql b #xFE)
                   (new-state check-start-fd)))
               (check-start-fd (b)
                 (cond ((eql b #xFD)
                        (new-state check-version))
                       (t
                        (new-state start)
                        (start b))
                       ))
               (check-version (b)
                 (cond ((eql b #x01)
                        (new-state read-short-count))
                       (t
                        (new-state start)
                        (start b))
                       ))
               (read-short-count (b)
                 (cond ((subrange-code-p b)
                        (funcall stuffer :init b)
                        (cond ((plusp b)
                               (setf ct b)
                               (new-state read-frag))
                              (t
                               (new-state read-long-count))
                              ))
                       (t
                        (new-state start)
                        (start b))
                       ))
               (read-frag (b)
                 (cond ((and (> ct 1)
                             (eql b #xFE))
                        (new-state check-frag-fd))
                       (t
                        (decf ct)
                        (funcall stuffer :stuff b)
                        (when (zerop ct)
                          (new-state read-long-count)
                          (funcall stuffer :check-finish)))
                       ))
               (check-frag-fd (b)
                 (cond ((eql b #xFD) ;; we just saw a start pattern #xFE #xFD
                        (new-state check-version))
                       (t
                        (decf ct)
                        (funcall stuffer :stuff #xFE)
                        (new-state read-frag)
                        (read-frag b))
                       ))
               (read-long-count (b)
                 (cond ((subrange-code-p b)
                        (new-state read-long-count-2)
                        (setf ct b))
                       (t
                        (new-state start)
                        (start b))
                       ))
               (read-long-count-2 (b)
                 (cond ((subrange-code-p b)
                        (incf ct (* b +long-count-base+))
                        (funcall stuffer :init-frag ct)
                        (cond ((zerop ct)
                               (new-state read-long-count)
                               (funcall stuffer :check-finish))
                              (t
                               (new-state read-frag))
                              ))
                       (t
                        (new-state start)
                        (start b))
                       )))
        (new-state start)
        (lambda (cust buf)
          (loop for b across buf do (funcall state b))
          (send cust :next))
        ))))

(defun make-decoder-fsm (dest &key max-reclen)
  (serializer (create (ssfsm (make-stuffer dest max-reclen)))))

;; -----------------------------------------------------------------
;; Stream Decoding

(defun stream-decoder-beh (fsm wait-ix queue)
  (alambda
   ((:deliver bufix buf)
    (cond ((eql bufix wait-ix)
           (send fsm self buf)
           (become (busy-stream-decoder-beh fsm bufix queue)))
          
          (t
           (become (stream-decoder-beh fsm wait-ix (maps:add queue bufix buf))))
          ))

   ((:go-silent)
    (become (sink-beh)))
   ))

(defun busy-stream-decoder-beh (fsm bufix queue)
  (alambda
   ((:next)
    (let* ((next-bufix (1+ bufix))
           (next-buf   (maps:find queue next-bufix)))
      (cond (next-buf
             (become (busy-stream-decoder-beh fsm next-bufix (maps:remove queue next-bufix)))
             (send fsm self next-buf))
            
            (t
             (become (stream-decoder-beh fsm next-bufix queue)))
            )))
            
   ((:deliver next-bufix next-buf)
    (become (busy-stream-decoder-beh fsm bufix (maps:add queue next-bufix next-buf))))

   ((:go-silent)
    (become (sink-beh)))
   ))

(defun stream-decoder (dest)
  ;; Construct an Actor that absorbs chunks of self-sync encoded input
  ;; stream and which triggers events to the dest actor as completed
  ;; decodings arrive.
  ;;
  ;; Incoming packets (chunks of encodings) are delivered along with a
  ;; chronological sequence number. Packets can arrive in any order,
  ;; starting from 1.
  ;;
  (let ((fsm (make-decoder-fsm dest)))
    (create (stream-decoder-beh fsm 1 (maps:empty)))
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
                       (pass (lambda (cust &rest args)
                               (break)
                               (assert (stringp (car args)))
                               (assert (eql (length (car args)) (length s)))
                               (assert (every #'char= (car args) s))))
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
                       (pass (lambda (cust &rest args)
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
