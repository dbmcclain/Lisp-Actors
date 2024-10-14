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

(in-package :ecc-crypto-b571)


(defun view-tree (tree)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list tree)

                  :children-function (lambda (tree)
                                       (um:match tree
                                         (()         nil)
                                         ((_ _)      nil)
                                         ((_ lf rt)  (list lf rt))
                                         ))
                  
                  :print-function (lambda (node)
                                    (um:match node
                                      (()         "")
                                      ((ct x)     (format nil "~A(~A)" x ct))
                                      ((ct _ _)   (format nil "(~A)" ct))
                                      ))
                  )))

(defun view-ltree (ltree)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list ltree)

                  :children-function (lambda (tree)
                                       (cond ((null tree) nil)
                                             (t (destructuring-bind (l n r) tree
                                                  (cond ((and (consp l) (consp r))
                                                         (list l r))
                                                        ((consp l) (list l))
                                                        ((consp r) (list r))
                                                        (t nil)) ))))
                  
                  :print-function (lambda (node)
                                    (cond ((null node) "")
                                          (t (destructuring-bind (l n r) node
                                               (cond ((and (consp l) (consp r))
                                                      (format nil "(~A)" n))
                                                     ((consp l)
                                                      (format nil "(~A)~A" n r))
                                                     ((consp r)
                                                      (format nil "~A(~A)" l n))
                                                     (t
                                                      (format nil "~A(~A)~A" l n r)))
                                               ))))
                  )))

(defun view-simple-tree (tree)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list tree)

                  :children-function (lambda (tree)
                                       (cond ((null tree) nil)
                                             (t (destructuring-bind (l r) tree
                                                  (cond ((and (consp l) (consp r))
                                                         (list l r))
                                                        ((consp l) (list l))
                                                        ((consp r) (list r))
                                                        (t nil)) ))))
                  
                  :print-function (lambda (node)
                                    (cond ((null node) "")
                                          (t (destructuring-bind (l r) node
                                               (cond ((and (consp l) (consp r)) "")
                                                     ((consp l)
                                                      (format nil "=/~A" r))
                                                     ((consp r)
                                                      (format nil "~A/=" l))
                                                     (t
                                                      (format nil "~A/~A" l r)))
                                               ))))
                  )))

(defun bit-expand (val nb)
  (nreverse
   (loop for ix from 0 below nb collect
         (ldb (byte 1 ix) val))))

(defun assemble-tree (alst)
  (um:nlet iter ((alst (sort alst '< :key 'car)))
    (let* ((hd   (car alst))
           (hlen (length (cadr hd)))
           (tl   (cdr alst)))
      (if (or tl
              (> hlen 1))
          (if (> hlen 1)
              (let* ((dst (assoc (* 2 (car hd)) tl)))
                (if dst
                    (setf tl (remove dst tl))
                  (setf dst (list (* 2 (car hd)) nil)))
                
                (when (oddp hlen)
                  (setf tl (cons (list (car hd) (list (car (cadr hd)))) tl)
                        (cadr hd) (cdr (cadr hd))))
                
                (setf (cadr dst) (append (cadr dst)
                                         (um:group (cadr hd) 2)))
                (iter (sort (cons dst tl) '< :key 'car)))
            (let* ((hd2 (car tl))
                   (hlen2 (length (cadr hd2)))
                   (tl  (cdr tl))
                   (dst (assoc (+ (car hd) (car hd2)) tl)))
              (if dst
                  (setf tl (remove dst tl))
                (setf dst (list (+ (car hd) (car hd2)) nil)))

              (setf (cadr dst) (append (cadr dst)
                                       (list (list (car (cadr hd))
                                                   (car (cadr hd2))))))
              (when (> hlen2 1)
                (setf (cadr hd2) (cdr (cadr hd2))
                      tl         (cons hd2 tl)))
              (iter (sort (cons dst tl) '< :key 'car))) )
        ;; else
        (let ((tree (car (cadr hd))))
          (view-simple-tree tree)
          tree)))))

#|
(defun make-huffman-codebook (nsym pdfn)
  (let* ((m     (ceiling (1- nsym) 2))
         (codes (list
                 (list (funcall pdfn 0) (list 0))))
         (tree  nil))
    (loop for ix from 1 to m do
          (push (list (funcall pdfn ix) (list ix)) codes))
    (loop for ix from 1 to (if (oddp nsym) m (1- m)) do
          (push (list (funcall pdfn ix) (list (+ m ix))) codes))
    (setf tree (assemble-tree codes))
|#
    

  
(defun huffman-encode (v)
  ;; scan and count codes
  (let ((tbl (make-hash-table)))
    (loop for x across v do
          (let* ((ent (or (gethash x tbl)
                          (setf (gethash x tbl)
                                (list 0)))))
            (incf (car ent))))

    ;; build table of code counts
    ;; list of symbols for each count
    (let ((tbl2 (make-hash-table)))
      (maphash (lambda (x ct)
                 (let ((ct (car ct)))
                   (setf (gethash ct tbl2)
                         (cons x (gethash ct tbl2)))))
               tbl)
      
      (let ((cts nil)
            (tree nil)
            (tbl3 (make-hash-table))
            (codes nil)
            (nbmax 0))

        (maphash (lambda (ct lst)
                   (push (list ct lst) cts))
                 tbl2)
        
        ;; (inspect (sort cts '< :key 'car))

        ;; construct the Huffman tree
        (setf tree (assemble-tree cts))
        
        ;; assign codes lengths
        (um:nlet iter ((top tree)
                       (lvl 1))
          (when top
            (destructuring-bind (l r) top
              (if (consp l)
                  (iter l (1+ lvl))
                (setf (gethash lvl tbl3) (cons l (gethash lvl tbl3))))
              (if (consp r)
                  (iter r (1+ lvl))
                (setf (gethash lvl tbl3) (cons r (gethash lvl tbl3))))
              )))

        ;; construct the code list showing
        ;; for each symbol how many bits are assigned to it.
        (maphash (lambda (lvl lst)
                   (setf nbmax (max nbmax lvl))
                   (dolist (x lst)
                     (push (list x lvl) codes)))
                 tbl3)
        ;; for output in lexicographics symbol order
        (setf codes (sort codes '< :key 'car))

        ;; now construct the actual encoding table
        ;; by assigning consecutive bit codes to lexicographically sorted symbols
        (let* ((tblh (make-hash-table))
               (code 0))
          
          (loop for nb from 1 to nbmax do
                (let* ((nbprev (length (gethash (1- nb) tbl3))))
                  (setf code (ash (+ code nbprev) 1))
                  (loop for x in (sort (gethash nb tbl3) '<)
                        for c from code
                        do
                        (setf (gethash x tblh) (list nb c)) )))
          
          ;; encode the data vector
          (let* ((bits  0)
                 (ctr   0)
                 (nbtot 0)
                 (enc  (ubyte-streams:with-output-to-ubyte-stream (s)
                         (loop for x across v do
                               (destructuring-bind (nb c) (gethash x tblh)
                                 (loop for bix from (1- nb) downto 0 do
                                       (setf bits (+ bits bits (if (logbitp bix c) 1 0))
                                             ctr  (1+ ctr))
                                       (when (>= ctr 8)
                                         (write-byte bits s)
                                         (setf bits 0
                                               ctr  0
                                               nbtot (+ nbtot 8))))))
                         (when (plusp ctr)
                           (incf nbtot ctr)
                           (write-byte (ash bits (- 8 ctr)) s)) )))

            ;; return total nbr of bits, the encoding, and the symbol code table
            (list nbtot enc codes)
            ))))))

(defun huffman-decode (nbits tbl v)
  (let* ((code 0)
         (nbmax (reduce 'max (mapcar 'cadr tbl)))
         (codes (make-array (1+ nbmax)
                            :initial-element 0)))
    ;; compute the base codes
    (loop for ix from 0 below nbmax do
          (let ((nc (count-if (lambda (pair)
                                (= ix (cadr pair)))
                              tbl)))
            (setf code (ash (+ nc code) 1)
                  (aref codes (1+ ix)) code)))

    ;; compute the encodings
    (let* ((encs (loop for pair in tbl collect
                       (destructuring-bind (x nb) pair
                         (prog1
                             (list x (bit-expand (aref codes nb) nb))
                           (incf (aref codes nb))) )))
           (tree nil))
      
      ;; create the Huffman tree
      (dolist (pair encs)
        (destructuring-bind (x bits) pair
          (setf tree (um:nlet iter ((top tree)
                                    (bs  bits))
                       (when bs
                         (let* ((b  (car bs)))
                           (list (if (zerop b)
                                     (or (iter (car top) (cdr bs))
                                         x)
                                   (car top))
                               (if (plusp b)
                                   (or (iter (cadr top) (cdr bs))
                                       x)
                                 (cadr top)))
                           )))
                )))

      ;; decode the data bit vector
      (let* ((pos  0)
             (nel  (length v))
             (enc  (make-empty-vector t)))
        (labels ((nextb ()
                   (when (< pos nel)
                     (prog1
                         (aref v pos)
                       (incf pos)))))
          (um:nlet iter ((top   tree)
                         (bs    (nextb))
                         (bpos  7)
                         (nbits nbits))
            (when (plusp nbits)
              (cond ((minusp bpos)
                     (iter top (nextb) 7 nbits))
                    
                    ((logbitp bpos bs)
                     (if (consp (cadr top))
                         (iter (cadr top) bs (1- bpos) (1- nbits))
                       (progn
                         (vector-append1 enc (cadr top))
                         (iter tree bs (1- bpos) (1- nbits)))))

                    (t
                     (if (consp (car top))
                         (iter (car top) bs (1- bpos) (1- nbits))
                       (progn
                         (vector-append1 enc (car top))
                         (iter tree bs (1- bpos) (1- nbits)))))
                    ))))
        enc))))

(defun write-16u (w s)
  (write-byte (ldb (byte 8 8) w) s)
  (write-byte (ldb (byte 8 0) w) s))

(defun write-32u (w s)
  (write-16u (ldb (byte 16 16) w) s)
  (write-16u (ldb (byte 16  0) w) s))

(defun read-16u (s)
  (let* ((b1 (read-byte s))
         (b2 (read-byte s)))
    (dpb b1 (byte 8 8) (dpb b2 (byte 8 0) 0))))

(defun read-32u (s)
  (let* ((w1 (read-16u s))
         (w2 (read-16u s)))
    (dpb w1 (byte 16 16) (dpb w2 (byte 16 0) 0))))

(defun write-huffman-encoding (v s)
  (destructuring-bind (nbits hv codes) (huffman-encode v)
    (write-16u (length codes) s)
    (dolist (pair codes)
      (destructuring-bind (code nb) pair
        (write-16u  code s)
        (write-byte nb   s)))
    (write-16u (length hv) s)
    (write-32u nbits s)
    (write-sequence hv s)))

(defun read-huffman-encoding (s)
  (let* ((ncodes (read-16u s))
         (codes  (loop repeat ncodes nconc
                       (list (list (read-16u s)
                                   (read-byte s)))))
         (nel    (read-16u s))
         (nbtot  (read-32u s))
         (hv     (make-array nel
                             :element-type '(unsigned-byte 8))))
    (read-sequence hv s)
    (huffman-decode nbtot codes hv)))

;; ------------------------------------------------------------------

(defparameter *tbl* nil)
(defparameter *wds* nil)
(defparameter *hftree* nil)
(defparameter *hyphs*  nil)
(defparameter *aposs*  nil)
(defparameter *hfitbl* nil)

(defun doit ()
  (let* ((xs  (hcl:file-string "VTuning/crypto/tools/war-and-peace-edit.txt"))
         (xss (um:split-string xs :delims '(#\linefeed)))
         (tbl (make-hash-table :test 'equalp)))
    
    (dolist (line xss)
      (unless (find-if (lambda (ch)
                         (member ch '(#\< #\> #\( #\) #\& #\*)))
                       line)
        (let* ((wds (um:tokens (substitute-if-not #\space (lambda (ch)
                                                            (or (alpha-char-p ch)
                                                                (digit-char-p ch)
                                                                (char= #\' ch)
                                                                (char= #\- ch)))
                                                  line)))
               (wds (mapcar (lambda (wd)
                              (string-trim "'-" wd))
                            wds))
               #|
               (wds (um:nlet iter ((wds '("r's"
                                          "att-ention"
                                          "co-o-m-pa"
                                          "co-o-om-pa-ny"))
                                   (ans wds))
                      (if wds
                          (iter (cdr wds) (remove (car wds) ans :test 'string-equal))
                        ans)))
               |#
               )
          
          (dolist (wd wds)
            (unless (string= wd "")
              (let ((wd (string-downcase wd)))
                (setf (gethash wd tbl)
                      (+ 1 (gethash wd tbl 0)))))
            ))))
    (let ((wds nil))
      (maphash (lambda (wd ct)
                 (push (list wd ct) wds))
               tbl)
      (setf *wds* (copy-seq (sort wds 'string-lessp :key 'car)))
      (let ((wds (mapcar 'car *wds*)))
        (setf *hyphs* (sort (um:collect-if (um:curry 'find #\-) wds) 'string-lessp))
        (setf *aposs* (sort (um:collect-if (um:curry 'find #\') wds) 'string-lessp))))
    
    #|
      (let ((wds nil))
        (maphash (lambda (wd ct)
                   (push wd wds))
                 tbl)
        (setf wds (sort wds 'string-lessp))
        (with-open-file (f "VTuning/crypto/tools/war-and-peace-wordlist.txt"
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (dolist (wd wds)
            (format f "~%~A" wd))))
      |#
    
    (let ((tbl2 (make-hash-table)))
      (maphash (lambda (wd ct)
                 (setf (gethash ct tbl2)
                       (cons wd (gethash ct tbl2))))
               tbl)
      
      (let ((wdc nil))
        (maphash (lambda (ct wds)
                   (push (list ct (sort wds 'string-lessp)) wdc))
                 tbl2)
        (setf *tbl* tbl
              *hftree* (assemble-tree wdc)
              *hfitbl* (make-hash-table :test 'string-equal))
        (um:nlet iter ((top *hftree*)
                       (bits 0)
                       (nbits 0))
          (when top
            (if (stringp (car top))
                (setf (gethash (car top) *hfitbl*)
                      (list (1+ nbits) (ash bits 1)))
              ;; else
              (iter (car top) (ash bits 1) (1+ nbits)))
            (if (stringp (cadr top))
                (setf (gethash (cadr top) *hfitbl*)
                      (list (1+ nbits) (1+ (ash bits 1))))
              ;; else
              (iter (cadr top) (1+ (ash bits 1)) (1+ nbits))))
          ))
      )))


(defun wp-encode-encryption (v)
  ;; (format t "~%Enc vlen = ~A" (length v))
  (let* ((v  (ubstream:with-output-to-ubyte-stream (s)
               (let ((x  (lw:mt-random (ash 1 32))))
                 (write-32u x s)
                 (write-32u (logxor x (length v)) s)
                 (write-sequence v s))))
         (vlen (length v))
         (vpos 0)
         (wds  nil))
    (labels ((next ()
               (if (< vpos vlen)
                   (prog1
                       (aref v vpos)
                     (incf vpos))
                 :eof)))
      
      (um:nlet iter ((bpos -1)
                     (bits nil)
                     (top  *hftree*))
        
        (cond ((eq :eof bits)
               (unless (eq top *hftree*)
                 (um:nlet iter-end ((top top))
                   (cond ((stringp (car top))
                          (push (car top) wds))
                         ((stringp (cadr top))
                          (push (cadr top) wds))
                         (t (iter-end #|(if (< (lw:mt-random 10) 5)
                                            (cadr top)
                                          (car top))|#
                                  (cadr top)))
                         ))
                 ))
              
              ((minusp bpos)
               (iter 7 (next) top))
              
              (t
               (let* ((new-top (if (logbitp bpos bits)
                                   (cadr top)
                                 (car top))))
                 (if (consp new-top)
                     (iter (1- bpos) bits new-top)
                   ;; else
                   (progn
                     (assert (stringp new-top))
                     (push new-top wds)
                     (iter (1- bpos) bits *hftree*)) )))
              )))
    ;; (inspect (reverse wds))
    (with-output-to-string (s)
      (let* ((term #(#\. #\. #\. #\? #\. #\. #\. #\! #\. #\.))
             (nterm (length term)))
        (um:nlet iter ((wds (nreverse wds)))
          (when wds
            (let* ((nwds (length wds))
                   (n    (min nwds (+ 3 (lw:mt-random 17))))
                   (hd   (um:take n wds))
                   (tl   (um:drop n wds)))
              (princ (apply 'um:paste-strings #\space
                            (string-capitalize (car hd))
                            (cdr hd))
                     s)
              (princ (aref term (lw:mt-random nterm)) s)
              (terpri s)
              (iter tl)))
          )))
    ))

(defun aont-encode-to-wp (x)
  ;; x is string or vector
  #|
  (wp-encode-encryption
   (if (stringp x)
       (map 'vector 'char-code x)
     x))
  |#
  #||#
  (wp-encode-encryption
   (aont-transform
    (ubstream:with-output-to-ubyte-stream (s)
      (write-compression x s))))
  #||#
  )

(defun wp-decode-encryption (str)
  (let* ((wds (um:tokens (um:nlet iter ((delims '(#\. #\? #\! #\,))
                                        (str  str))
                           (if delims
                               (iter (cdr delims)
                                     (remove (car delims) str))
                             ;; else
                             str)) ))
         (brem 0)
         (bits 0)
         (v    (ubstream:with-output-to-ubyte-stream (s)
                 (loop for wd in wds do
                       (destructuring-bind (nbits code) (gethash wd *hfitbl*)
                         (um:nlet iter ((nbits nbits)
                                        (code  code))
                           (if (> (+ nbits brem) 7)
                               (let* ((wid (- 8 brem))
                                      (pos (- nbits wid)))
                                 (setf bits (logior (ash bits wid)
                                                    (ldb (byte wid pos) code)))
                                 (write-byte bits s)
                                 (setf bits 0
                                       brem 0)
                                 (when (> nbits wid)
                                   (iter pos
                                         (ldb (byte pos 0) code))))
                             ;; else
                             (setf brem (+ brem nbits)
                                   bits (logior (ash bits nbits)
                                                code))
                             ))
                         ))
                 )))
    (ubstream:with-input-from-ubyte-stream (s v)
      (let* ((x  (read-32u s))
             (nb (logxor x (read-32u s))))
        ;; (format t "~%Decr vlen = ~A" nb)
        (subseq v 8 (+ 8 nb))))
    ))


(defun aont-decode-from-wp (str)
  #||#
  (ubstream:with-input-from-ubyte-stream (s (aont-untransform
                                             (wp-decode-encryption str)))
    (read-compression s))
  #||#
  ;; (wp-decode-encryption str)
  )


(defun aont-decode-from-wp-to-string (str)
  (map 'string 'code-char (aont-decode-from-wp str)))

#|
(let* ((s (hcl:file-string "VTuning/crypto/tools/aont.lisp"))
       (x (aont-encode-to-wp s)))
  (print x)
  (print (list (length s) (length x)))
  (aont-decode-from-wp-to-string x))

       
 |#
