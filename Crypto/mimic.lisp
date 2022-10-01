;; mimic.lisp -- transform a white noise distribution of byte codes into
;; a list of words that mimics the statistics of Tolstoy's "War and Peace"
;;
;; DM/RAL 06/15
;; ------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :tolstoy-aont)


;; --------------------------------------------------------------------------

(defvar *xptrees* nil)

(defun str-vec (str)
  (let ((arr (make-array (length str)
                         :element-type '(unsigned-byte 8))))
    (map-into arr 'char-code str)))


(defun tree-find (key tree)
  #F
  (when tree
    (destructuring-bind (l (k v) r) tree
      (cond ((string= key k)  v)
            ((string< key k)  (tree-find key l))
            (t                (tree-find key r))
            ))))

;; --------------------------------------------------------------------------

(defclass huffman-tree ()
  ((prev1    :accessor huffman-tree-prev1 :initform nil)
   (prev2    :accessor huffman-tree-prev2 :initform nil)
   (top      :accessor huffman-tree-top   :initform (car *xptrees*))))

(defmethod transition-tree ((ht huffman-tree) (wd string))
  (with-accessors ((top    huffman-tree-top)
                   (prev1  huffman-tree-prev1)
                   (prev2  huffman-tree-prev2)) ht
    (shiftf prev2 prev1 wd)
    (setf top
          (car
           (or (let ((ent (and prev2
                               (tree-find prev2 (cadr *xptrees*)) )))
                 (and ent
                      (tree-find prev1 (cadr ent))))
               
               (tree-find prev1 (cadr *xptrees*))
               *xptrees*))
          )))

(defmethod huffman-tree-code ((hft huffman-tree) (wd string))
  (tree-find wd (cadr (huffman-tree-top hft))))

(defmethod huffman-tree-top-node ((hft huffman-tree))
  (car (huffman-tree-top hft)))
                   
;; --------------------------------------------------------------------------

(defvar *wp-data-file*
  (merge-pathnames "RAL/wp-data.dat"
                   (sys:get-folder-path :appdata :create t)))

(defun read-wp-map-data ()
  (labels ((restore-tables (lst)
             (setf *xptrees* lst)))
    (hcl:load-data-file *wp-data-file*
                        :eval nil
                        :callback #'restore-tables)))

(unless *xptrees*
  (read-wp-map-data))

;; ------------------------------------------------------

(defclass input-bit-vector ()
  ((vbytes  :accessor input-bit-vector-vbytes :initarg :vec)
   (vlen    :accessor input-bit-vector-vlen)
   (vpos    :accessor input-bit-vector-vpos   :initform 0)
   (bits    :accessor input-bit-vector-bits   :initform 0)
   (nbits   :accessor input-bit-vector-nbits  :initform 0)))

(defmethod initialize-instance :after ((ibv input-bit-vector) &key vec &allow-other-keys)
  (with-accessors ((vlen   input-bit-vector-vlen)) ibv
    (setf vlen (length vec))))

(defmacro with-input-from-bit-vector ((v vec) &body body)
  (let ((ibv (gensym)))
    `(let ((,ibv  (make-instance 'input-bit-vector
                                 :vec ,vec)))
       (funcall (lambda (,v)
                  ,@body)
                ,ibv))))

(defmethod read-bits ((ibv input-bit-vector) nbits)
  #F
  (with-accessors ((vbytes  input-bit-vector-vbytes)
                   (vlen    input-bit-vector-vlen)
                   (vpos    input-bit-vector-vpos)
                   (bits    input-bit-vector-bits)
                   (nb      input-bit-vector-nbits)) ibv
    (cond  ((>= nb nbits)
            (let* ((out-bits  (ldb (byte nbits (- nb nbits)) bits)))
              (setf bits (ldb (byte (- nb nbits) 0) bits)
                    nb   (- nb nbits))
              (values out-bits nbits)))

           ((< vpos vlen)
            (setf bits (logior (ash bits 8)
                               (aref vbytes vpos))
                  nb   (+ nb 8)
                  vpos (1+ vpos))
            (read-bits ibv nbits))

           ((minusp nb)
            (values :eof 0))

           (t
            (let* ((out-bits bits)
                   (out-nbits nb))
              (setf nb -1)
              (values out-bits out-nbits)))
           )))


;; ------------------------------------------------------

(defclass compound-input-bit-vector ()
  ((ibv   :accessor cibv-ibv)
   (vs    :accessor cibv-vs  :initarg :vecs)))

(defmethod initialize-instance :after ((cibv compound-input-bit-vector) &key &allow-other-keys)
  (with-accessors ((ibv  cibv-ibv)
                   (vs   cibv-vs)) cibv
    (setf ibv (when vs
                (make-instance 'input-bit-vector
                               :vec (pop vs))))
    ))

(defmacro with-input-from-compound-bit-vector ((v veclst) &body body)
  (let ((cibv (gensym)))
    `(let ((,cibv  (make-instance 'compound-input-bit-vector
                                  :vecs ,veclst)))
       (funcall (lambda (,v)
                  ,@body)
                ,cibv))))

(defmethod read-bits ((cibv compound-input-bit-vector) nbits)
  #F
  (with-accessors ((ibv    cibv-ibv)
                   (vs     cibv-vs)) cibv
    (if ibv
        (multiple-value-bind (bits nb) (read-bits ibv nbits)
          
          (cond
           
           ((eq bits :EOF)
            (setf ibv (when vs
                        (make-instance 'input-bit-vector
                                       :vec (pop vs))))
            (read-bits cibv nbits))

           ((< nb nbits)
            (um:nlet iter ((bits bits)
                           (nb   nb))
              (multiple-value-bind (bits2 nb2) (read-bits cibv (- nbits nb))
                (cond ((eq bits2 :EOF)
                       (values bits nb))
                      
                      ((< (+ nb nb2) nbits)
                       (go-iter (logior (ash bits nb2)
                                     bits2)
                             (+ nb nb2)))
                      
                      (t
                       (values (logior (ash bits nb2)
                                       bits2)
                               nbits))
                      ))))

           (t
            (values bits nb))
           ))
      ;; else
      (values :eof 0))))

;; ------------------------------------------------------

(defun expon-random (k)
  #F
  (max 1
       (round (log (max 1e-3 (lw:mt-random 1.0)))
              (/ -1 k))))

(defun wp-encode-encryption (v)
  #F
  (let* ((vct  (ubstream:with-output-to-ubyte-stream (s)
                 (let ((x  (prng:ctr-drbg-int 32) #|(lw:mt-random (ash 1 32))|#))
                   (write-32u x s)
                   (write-32u (logxor x (length v)) s))))
         (hft  (make-instance 'huffman-tree))
         (llen 0)
         (plen (expon-random 6)))

    (with-output-to-string (s)
      (with-input-from-compound-bit-vector (ibv (list vct v))
        (labels ((next-bits ()
                   (multiple-value-bind (b nb) (read-bits ibv 7)
                     (cond
                      ((eq b :eof)  :eof)
                      (t
                       (logior (ash b (- 8 nb))
                               (lw:mt-random 2)))
                      )))
                 
                 (pushwd (wd)
                   (let ((len (1+ (length wd))))
                     (incf llen len)
                     (when (> llen 79)
                       (terpri s)
                       (decf plen)
                       (unless (plusp plen)
                         (terpri s)
                         (setf plen (expon-random 6)))
                       (setf llen len))
                     (princ wd s)
                     (princ #\space s)))
                 
                 (next-tree (wd)
                   (transition-tree hft wd)
                   (huffman-tree-top-node hft)))
          
          (um:nlet iter ((bpos -1)
                         (bits nil)
                         (top  (huffman-tree-top-node hft)))
            
            (cond
             
             ;; ----------------------------------------
             ((eq :eof bits)
              (unless (eq top (huffman-tree-top-node hft))
                (um:nlet iter-end ((top top))
                  (let ((new-top (if (< (lw:mt-random 10) 5)
                                     (car top)
                                   (cadr top))))
                    (if (stringp new-top)
                        (pushwd new-top)
                      (go-iter-end new-top))))
                ))
             
             ;; ----------------------------------------
             ((minusp bpos)
                (go-iter 7 (next-bits) top))
              
              ;; ----------------------------------------
              (t
               (let* ((new-top (if (logbitp bpos bits)
                                   (cadr top)
                                 (car top))))
                 (if (consp new-top)
                     (go-iter (1- bpos) bits new-top)
                   ;; else
                   (progn
                     (pushwd new-top)
                     (go-iter (1- bpos) bits
                              (next-tree new-top)))
                   )))
              ))))
        )))

;; ------------------------------------------------------

(defun eol (txt start)
  (and start
       (let ((new-start (position #\newline txt :start start)))
         (and new-start
              (1+ new-start))
         )))

(defun bol (txt start)
  (and start
       (position #\newline (subseq txt 0 start) :from-end t)))
       
(defun strip-snips (text)
  ;; legacy recoveory...
  (let* ((snip  "--- SNIP HERE ---")
         (slen  (length snip))
         (start (eol text
                     (search snip text
                             :test #'string-equal)))
         (txlen (length text))
         (end   (and (> txlen slen)
                     (bol text
                          (search snip text
                                  :test     #'string-equal
                                  :from-end t)))))
    ;; some sanity checking... just want surrounding snip lines,
    ;; not accidentally embedded lines.
    (when (> start 36)
      (setf start nil))
    (when (< end (- txlen 36))
      (setf end nil))
    
    (if start
        (subseq text start end)
      (subseq text 0 end))))
  
(defmethod aont-encode-to-wp ((x vector))
  ;; x is string or vector
  (wp-encode-encryption
   (aont-transform
    (ubstream:with-output-to-ubyte-stream (s)
      (write-compression x s)))))

(defmethod aont-encode-to-wp ((s string))
  (aont-encode-to-wp (babel:string-to-octets (strip-snips s))))

;; ------------------------------------------------------

(defclass reblocker ()
  ;; accepts any number of bits at a time
  ;; and reblocks to 8-bit bytes for output through wrfn
  ((wrfn    :accessor reblocker-wrfn  :initarg :wrfn)
   (bits    :accessor reblocker-bits  :initform 0)
   (nbits   :accessor reblocker-nbits :initform 0)))

(defmethod write-bits ((rblk reblocker) bits nbits)
  #F
  (with-accessors ((wrfn  reblocker-wrfn)
                   (bts   reblocker-bits)
                   (nb    reblocker-nbits)) rblk
    (if (>= (+ nb nbits) 8)
        (let* ((wid  (- 8 nb))
               (pos  (- nbits wid)))
          (setf bts (logior (ash bts wid)
                            (ldb (byte wid pos) bits)))
          (funcall wrfn bts)
          (setf bts 0
                nb  0)
          (when (plusp pos)
            (write-bits rblk (ldb (byte pos 0) bits) pos)))
      ;; else
      (setf nb  (+ nb nbits)
            bts (logior (ash bts nbits)
                        bits))
      )))

;; ------------------------------------------------------

(defclass output-bit-vector ()
  ((vbytes  :accessor output-bit-vector-vbytes :initarg :vec)
   (rblk    :accessor output-bit-vector-rblk)))

(defmethod initialize-instance :after ((obv output-bit-vector) &key &allow-other-keys)
  (with-accessors ((rblk   output-bit-vector-rblk)
                   (vbytes output-bit-vector-vbytes)) obv
    (setf rblk (make-instance 'reblocker
                              :wrfn (lambda (bits)
                                      (write-byte bits vbytes))) ))) 

(defmacro with-output-to-bit-vector ((v) &body body)
  (let ((obv  (gensym))
        (s    (gensym)))
    `(ubstream:with-output-to-ubyte-stream (,s)
       (let ((,obv  (make-instance 'output-bit-vector
                                   :vec ,s)))
         (funcall (lambda (,v)
                    ,@body)
                  ,obv)))
    ))

(defmethod write-bits ((obv output-bit-vector) bits nbits)
  (with-accessors ((rblk output-bit-vector-rblk))  obv
    (write-bits rblk bits nbits)))


;; ------------------------------------------------------

(defclass word-stream ()
  ((str   :accessor word-stream-str    :initarg :str)
   (nel   :accessor word-stream-nel)
   (pos   :accessor word-stream-pos    :initform 0)))

(defmethod initialize-instance :after ((wds word-stream) &key &allow-other-keys)
  (setf (word-stream-nel wds) (length (word-stream-str wds))))

(defmethod next-word ((ws word-stream))
  (with-accessors ((str  word-stream-str)
                   (nel  word-stream-nel)
                   (pos  word-stream-pos)) ws
    (um:when-let (start (and (< pos nel)
                             (position-if-not 'lw:whitespace-char-p str :start pos)))
      (um:if-let (end (position-if 'lw:whitespace-char-p str :start (1+ start)))
          (prog1
              (subseq str start end)
            (setf pos (1+ end)))
        ;; else
        (prog1
            (subseq str start)
          (setf pos nel)))) ))

(defun wp-decode-encryption (str)
  #F
  (let* ((wstrm (make-instance 'word-stream
                               :str str))
         (hft   (make-instance 'huffman-tree))
         (v     (with-output-to-bit-vector (s)
                  (let ((rblk  (make-instance 'reblocker
                                              :wrfn (lambda (bits)
                                                      (write-bits s (ash bits -1) 7)))))
                    
                    (um:nlet iter ()
                      (um:when-let (wd (next-word wstrm))
                        (destructuring-bind (nbits code) (huffman-tree-code hft wd)
                          (write-bits rblk code nbits)
                          (transition-tree hft wd)
                          (go-iter))))
                    ))))

    (ubstream:with-input-from-ubyte-stream (s v)
      (let* ((x   (read-32u s))
             (nb  (logxor x (read-32u s))))
        
        (subseq v 8 (+ 8 nb))))
    ))

;; ------------------------------------------------------

(defun aont-decode-from-wp (str)
  (ubstream:with-input-from-ubyte-stream (s (aont-untransform
                                             (wp-decode-encryption
                                              (strip-snips str))))
    (read-compression s)))

(defun aont-decode-from-wp-to-string (str)
  (babel:octets-to-string (aont-decode-from-wp str)))


;; ------------------------------------------------------

(defun make-encoded-filename (fname)
  (concatenate 'string
               (namestring fname)
               ".wp"))

(defun make-decoded-filename (fname)
  (let* ((ns  (namestring fname))
         (pos (position #\. ns :from-end t)))
    (if (and pos
             (string-equal ".wp" ns :start2 pos))
        (subseq ns 0 pos)
      ns)))

(defun find-unused-ofname (ofname)
  #f
  (um:nlet iter ((ctr nil))
    (let ((new-ofname (if ctr
                          (make-pathname
                           :name     (format nil "~A-~D" (pathname-name ofname) ctr)
                           :defaults ofname)
                        ofname)))
      (if (probe-file new-ofname)
          (iter (if ctr (1+ ctr) 1))
        new-ofname))))

(defun make-safe-filename (fname &optional dirname)
  (find-unused-ofname
   (if dirname
       (make-pathname
        :name  (pathname-name fname)
        :type  (pathname-type fname)
        :defaults dirname)
     fname)))


(defun do-with-prompted-i/o (opt-file prep-out body-fn)
  (um:with-remembered-filename (inpfile :com.ral.aont.inp opt-file)
    (let* ((prep-name (funcall prep-out inpfile))
           (try-name  (make-safe-filename prep-name inpfile)))
    (um:with-remembered-prompting (outfile :com.ral.aont.out try-name
                                           "Save as..."
                                           :filter "*.*"
                                           :operation :save
                                           :if-exists :prompt
                                           :if-does-not-exist :ok)
      (funcall body-fn inpfile outfile)))))

;; ------------------------------------------------------

(defun basic-encode-file-to-wp (inpfile outfile)
  (let ((v (aont-encode-to-wp
            (file-vector inpfile))))
    (ensure-directories-exist outfile)
    (with-open-file (ofp outfile
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write-string v ofp)
      )))
                             
(defun basic-decode-file-from-wp (inpfile outfile)
  (let ((v (aont-decode-from-wp
            (hcl:file-string inpfile))))
    (ensure-directories-exist outfile)
    (with-open-file (ofp outfile
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (write-sequence v ofp))))

;; ------------------------------------------------------

(defun aont-encode-file-to-wp (&optional file)
  (do-with-prompted-i/o file
                        'make-encoded-filename
                        'basic-encode-file-to-wp))

(defun aont-decode-file-from-wp (&optional file)
  (do-with-prompted-i/o file
                        'make-decoded-filename
                        'basic-decode-file-from-wp))

;; ------------------------------------------------------

(defun get-files-for-wp (prompt)
  (let* ((files (capi:prompt-for-files prompt
                                       :pathname (um:remembered-filename :com.ral.aont.inp)
                                       :operation :open
                                       :filter "*.*")))
    (when files
      (um:remember-filename :com.ral.aont.inp (car files)))
    files))

(defun get-dir-for-wp ()
  (let ((dir (capi:prompt-for-directory "Select Output Directory"
                                        :pathname (or (um:remembered-filename :com.ral.aont.outp)
                                                      (um:remembered-filename :com.ral.aont.inp)) )))
    (when dir
      (um:remember-filename :com.ral.aont.outp dir))
    dir))
                                        

(defun get-files-and-dir-for-wp (opt-files opt-dir prompt)
  (let* ((flist (or opt-files
                    (get-files-for-wp prompt)))
         (dir   (if opt-files
                    opt-dir
                  (or (and flist
                           (get-dir-for-wp))
                      (setf flist nil)))))
    (values flist dir)))

(defun aont-enc-dec-files-for-wp (files outdir prompt prep-fn body-fn)
  (multiple-value-bind (flist dir)
      (get-files-and-dir-for-wp files outdir prompt)
    (dolist (file flist)
      (let* ((outfile (make-safe-filename
                       (funcall prep-fn file)
                       dir)))
        (format t "~%~a" file)
        (funcall body-fn file outfile)
        (format t " -> ~A" outfile)
        ))))
  
;; ------------------------------------------------------

(defun aont-encode-files-to-wp (files &optional outdir)
  (aont-enc-dec-files-for-wp files outdir "Select Files for Encoding"
                             'make-encoded-filename
                             'basic-encode-file-to-wp))


(defun aont-decode-files-from-wp (files &optional outdir)
  (aont-enc-dec-files-for-wp files outdir "Select Files for Decoding"
                             'make-decoded-filename
                             'basic-decode-file-from-wp))

;; ------------------------------------------------------

#|
 ;; Compute and show entropy signatures of ensembles
 
(defparameter *raw-hist* (make-array 256 :element-type 'fixnum :initial-element 0))
(defparameter *cryp-hist* (make-array 256 :element-type 'fixnum :initial-element 0))
(defparameter *enc-hist* (make-array 256 :element-type 'fixnum :initial-element 0))

(defun tally (v dst)
  (loop for x across v do
        (incf (aref dst x)))
  v)

(defun norm (v norm)
  (map 'vector (um:rcurry '/ norm) v))

(defun logw (v norm)
  (map 'vector
       (lambda (x)
         (log (max x 1e-6) 10))
       (norm v norm)))

(let ((fs (directory "VTuning/crypto/tools/*.lisp")))
  (fill *raw-hist* 0)
  (fill *cryp-hist* 0)
  (fill *enc-hist* 0)
  (dolist (f fs)
    (let* ((fv (tally (file-vector f) *raw-hist*))
           (cv (tally (aont-transform
                       (ubstream:with-output-to-ubyte-stream (s)
                         (write-compression fv s)))
                      *cryp-hist*))
           (ev (tally (str-vec (wp-encode-encryption cv))
                      *enc-hist*)))
      nil)))
  
(let* ((traw  (loop for x across *raw-hist* sum x))
       (tcryp (loop for x across *cryp-hist* sum x))
       (tenc  (loop for x across *enc-hist* sum x)))
  (plt:plot 'plt (logw *raw-hist* traw)
            :ylog nil
            :line-type :stepped
            :legend "Raw Lisp"
            :xrange '(0 255)
            :title "Comparative Frequency Histograms"
            :xtitle "Byte Code [0 .. 255]"
            :ytitle "Log Probability"
            :clear t)
  (plt:plot 'plt (logw *cryp-hist* tcryp)
            :line-type :stepped
            :legend "Encrypted"
            :color :blue)
  (plt:plot 'plt (logw *enc-hist* tenc)
            :line-type :stepped
            :legend "Tolstoy Enc"
            ;; :alpha 0.5
            :color :red)
  (list
   :raw  (/ (loop for x across *raw-hist*
                  when (plusp x)
                  sum  (- (log traw 2) (log x 2)))
            256)
   :cryp (/ (loop for x across *cryp-hist*
                  when (plusp x)
                  sum  (- (log tcryp 2) (log x 2)))
            256)
   :enc  (/ (loop for x across *enc-hist*
                  when (plusp x)
                  sum  (- (log tenc 2) (log x 2)))
            256))
  )
  
 |#
