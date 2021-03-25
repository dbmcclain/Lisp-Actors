
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
;;

(um:defmacro! fsm-states (&rest clauses)
  (let ((first-name (caar clauses)))
    `(macrolet ((,a!state (s)
                `(setq ,',a!this #',s)))
       (labels
           ,(mapcar (lambda (clause)
                      (destructuring-bind (name arg &rest cases) clause
                        `(,name ,arg
                                (cond
                                 ,@(mapcar (lambda (pair)
                                             (destructuring-bind (pred form) pair
                                               (if (consp pred)
                                                   `(,pred ,form)
                                                 (if (and (symbolp pred)
                                                          (string= (string pred) "_"))
                                                     `(t ,form)
                                                   `((eql ,(car arg) ,pred) ,form)))))
                                           (um:group cases 2))))
                        ))
                    clauses)
         #',first-name))
    ))

(defun reader-fsm (finish-fn)
  (alet ((ct)     ;; remaining count of bytes to stuff
         (nb)     ;; length of current frag
         (max-ct) ;; max possible frag length
         (crc  (make-ubv 4)) ;; crc buffer
         (len  (make-ubv 4)) ;; record length buffer
         (cix)    ;; crc buffer index
         (lix)    ;; len buffer index
         (aout (make-ubv 256 ;; stuff accumulator
                         :fill-pointer 0
                         :adjustable   t)))
      (labels
          ;; ------------------------------------
          ;; helper functions
          ;; ------------------------------------
          ((init-bufs (b)
             ;; called at start of short frag
             (setf nb  b
                   ct  b
                   max-ct +max-short-count+
                   cix 0
                   lix 0
                   (fill-pointer aout) 0))
           ;; -----------------------------------
           (stuff (b)
             ;; byte stuffer
             (decf ct)
             (cond
              ((< cix 4) ;; crc comes first
               (setf (aref crc cix) b)
               (incf cix))
              ((< lix 4) ;; followed by 4-byte length
               (setf (aref len lix) b)
               (incf lix))
              (t         ;; followed by actual data
               (vector-push-extend b aout))
              ))
           ;; -----------------------------------
           (maybe-stuff-fefd ()
             ;; if frag was shorter than mox possible length
             ;; then it was because a start sequence was found
             ;; in the data - so put it back.
             (when (< nb max-ct)
               (stuff #xFE)
               (stuff #xFD)))
           ;; -----------------------------------
           (init-frag (b)
             ;; called at start of long frag
             (incf nb (* +long-count-base+ b))
             (setf ct     nb
                   max-ct +max-long-count+))
           ;; -------------------------------------        
           (check-finish ()
             ;; see if we've found the end of a record
             (let* ((ans (copy-seq aout))
                    (chk (crc32 len ans)))
               (when (and (equalp crc chk)
                          (= (vec-le4-to-int len) (length ans)))
                 (funcall finish-fn ans))
               )))
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
          (<= 0 b #xFC) (progn (init-bufs b) (state read-frag))
          _             (sync b))     ;; frag was clobbered
         
         (read-long-count  ;; remaining frags are long
          (b)
          :EOF          (progn (check-finish) (state start))  ;; end of record / end of file
          (<= 0 b #xFC) (progn (maybe-stuff-fefd) (setf nb b) (state read-long-count-2))
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
