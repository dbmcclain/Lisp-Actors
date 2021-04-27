;; hashed-sync.lisp -- Synchronizing large data sets across a network
;; with refined hashing probes
;;
;; DM/RAL 11/17
;; ----------------------------------------------------------------------
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


(defpackage #:hashed-sync
  (:use #:common-lisp #:fac)
  (:import-from #:um
   #:nlet-tail
   #:if-let
   #:when-let
   #:group
   #:dlambda
   #:foreach)
  (:export
   #:make-agent-for-change
   #:sync-stores))

;; -----------------------------------------------------------------------

(in-package #:hashed-sync)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------------------------

(defun hash32 (obj)
  (let ((seq (loenc:encode obj))
        (dig (ironclad:make-digest :sha256)))
    (ironclad:update-digest dig seq)
    (ironclad:produce-digest dig)))
#|
(defun ubseq< (seq-a seq-b)
  (declare (type (vector (unsigned-byte 8)) seq-a seq-b))
  (let* ((len-a (length seq-a))
         (len-b (length seq-b))
         (limit (min len-a len-b)))
    (nlet-tail iter ((ix 0))
      (if (< ix limit)
          (let ((v-a (aref seq-a ix))
                (v-b (aref seq-b ix)))
            (cond ((< v-a v-b)
                   ix)
                  ((= v-a v-b)
                   (iter (1+ ix)))
                  (t
                   nil)))
        (< len-a len-b)))))

;; ---------------------------------------------------------
;; Merkel Trees --
;; While interesting in its own right, and makes for fast integrity
;; checking, it is not particularly useful when comparing two datasets
;; to find their difference.
;;
;; Viz. set1 has '(a b c d), set2 has '(a X b c d). The presence of X
;; in the second set causes every node in the Merkel tree to change,
;; compared to the tree for set 1.
;;
(defun hashtree (objs)
  ;; Compute the Merkel tree of the list of objects.
  ;; Objects are sorted by the hash of their pickled representation
  ;; Nodes are lists: (hashval level object). Leaf nodes at level 0.
  (let ((hobjs (sort (mapcar (lambda (obj)
                               (list (hash32 obj) 0 obj))
                             objs)
                     #'ubseq<
                     :key #'car)))
    (nlet-tail iter ((lst   hobjs)
                     (level 1))
      (if (null (cdr lst)) ;; singleton?
          lst
        (iter (mapcar (lambda (pair)
                        (multiple-value-bind (p1 p2) (values-list pair)
                          (list (hash32 (list (car p1) (car p2))) level pair)))
                      (group lst 2))
              (1+ level))))
    ))
;; ---------------------------------------------------------
;;
;; So instead, we use a hash vector of hashes to try to narrow down
;; and minimize the differences seen between two datasets.
;;
;; 1. First, compute the hash of every object in the dataset.
;;
;; 2. Then sort the objects by their hash value.
;;
;; 3. Next, bin them according to their hash value across N bins,
;; forming sorted sublists in each bin.
;;
;; 4. Compute the hash of the ordered, concatenated, hash values for
;; each object in the sublist, for each bin, forming an N-bin vector
;; of hashes. (Consistent ordering is important here, because (Hash a
;; b) is different from (Hash b a)).
;;
;; Now when we have set1 with '(a b c d) and set2 with '(a X b c d),
;; at most one bin will differ between them.
;;
;; If sublists are large, then iterate the process (using different
;; bin-hashing schemes) for the differing sublists to quickly find the
;; differing elements, operating solely on hash values until those
;; different items are identified. Only then do you need to transfer
;; (potentially large) data objects across the network.
;;
;; An overall hash of the hash vector can establish equality over both
;; data sets without ever having to see more than one hash value from
;; each of them.
;;
(defconstant +hashvec-size+  32)

(defun form-hvec (obj-arr)
  (let ((hash-arr (make-array +hashvec-size+)))
    (dotimes (ix +hashvec-size+)
      (let ((bin (sort (aref obj-arr ix) #'ubseq< :key #'car)))
        (when bin
          (setf (aref obj-arr ix)  bin
                (aref hash-arr ix) (hash32 (mapcar #'car bin))))
        ))
    hash-arr))
  
(defun rebin-hashes (level hashes)
  (let ((obj-arr  (make-array +hashvec-size+)))
    (dolist (hash hashes)
      (let* ((hv  (car hash))
             (ix  (mod (aref hv level) +hashvec-size+)))
        (push hash (aref obj-arr ix))))
    (let ((hash-arr (form-hvec obj-arr)))
      (values hash-arr obj-arr))))
                
(defun hashvec (objs)
  (multiple-value-bind (hash-arr obj-arr)
      (rebin-hashes 0 (mapcar #`(,(hash32 a1) . ,a1) objs))
    (values (hash32 hash-arr) hash-arr obj-arr)))
|#
#|
(let ((x (um:lc ((random 256) (ix <.. 1 1024)))))
  (multiple-value-bind (h-all h-vec obj-vec)
      (hashvec x)
      (plt:plot 'histo (um:lc ((length (aref obj-vec ix)) (ix <.. 0 255)))
                :clear t
                :line-type :stepped
                )
      (let ((ixs (um:lc ((aref (car pair) 0)
                         (lst <- obj-vec)
                         (pair <- lst))))
            (iys (um:lc ((aref (car pair) 1)
                         (lst <- obj-vec)
                         (pair <- lst)))))
      (plt:plot 'xy ixs iys :clear t :symbol :circle)
      )))

(defun tst-sort (&key (nitems 100) (niter #N100_000))
  (let ((x (um:lc ((random 1024) (ix <.. 1 nitems)))))
    (time
     (loop repeat niter do
           (sort (copy-list x) #'<)))))
 |#

;; ---------------------------------------------------------
#|
(defun hash32s (&rest objs)
  (let ((seqs (sort (mapcar (um:compose #'copy-seq #'loenc:encode)
                            objs)
                    #'ubseq<))
        (dig  (ironclad:make-digest :sha256)))
    (dolist (seq seqs)
      (ironclad:update-digest dig seq))
    (ironclad:produce-digest dig)))
|#

(defun plist-to-alist (plist)
  (sort (group plist 2) #'string<
        :key #'car))

;; ---------------------------------------------------------
#|
(defun make-agent-for-change (store-list)
  (make-actor
   (let (h-vec o-vec diffs)
     (dlambda
       ;; ------------------------------------
       (:get-total-hash ()
        (multiple-value-bind (h-all h-v o-v)
          (hashvec store-list)
          (setf  h-vec  h-v
                 o-vec  o-v)
          h-all))

       ;; ------------------------------------
       (:get-hash-vector ()
        h-vec)

       ;; ------------------------------------
       (:note-bin-diffs (bins)
        (foreach (lambda (bin)
                   (setf diffs (nconc (aref o-vec bin) diffs)))
                 bins))
       
       ;; ------------------------------------
       (:get-diff-hashes ()
        (mapcar #'car diffs))
       
       ;; ------------------------------------
       (:compute-level-2 ()
        (multiple-value-bind (h-v o-v)
            (rebin-hashes 1 diffs)
          (setf diffs nil
                h-vec h-v
                o-vec o-v)))

       ;; ------------------------------------
       (:get-objects (hashes)
        (setf diffs (delete-if (lambda (elt)
                                 (not (member (car elt) hashes
                                              :test #'equalp)))
                               diffs))
        (mapcar #'cdr diffs))

       ;; ------------------------------------
       (:add-items (objs)
        (setf store-list (nconc objs store-list)))

       ;; ------------------------------------
       (:get-store ()
        store-list)
       ))))

;; ---------------------------------------------------------

(defun hash-list-partition (lst1 lst2)
  (nlet-tail iter ((lst1  (sort lst1 #'ubseq<))
                   (lst2  (sort lst2 #'ubseq<))
                   (new1  nil)
                   (new2  nil))
    (cond ((null lst1)
           (values new1 (nconc lst2 new2)))
          ((null lst2)
           (values (nconc lst1 new1) new2))
          ((equalp (car lst1) (car lst2))
           ;; elide common elements, but preserve duplicates
           (iter (cdr lst1) (cdr lst2) new1 new2))
          ((ubseq< (car lst1) (car lst2))
           (iter (cdr lst1) lst2 (cons (car lst1) new1) new2))
          (t
           (iter lst1 (cdr lst2) new1 (cons (car lst2) new2)))
          )))
          
;; ---------------------------------------------------------

(defun sync-stores (a b)
  (if (equalp (ask a :get-total-hash)
              (ask b :get-total-hash))
      (print "Stores are the same")
    (labels ((note-diffs ()
               (let (diffs)
                 (loop for ha across (ask a :get-hash-vector)
                       for hb across (ask b :get-hash-vector)
                       for ix from 0
                       do
                       (unless (equalp ha hb)
                         (format t "~&Bin ~A is different" ix)
                         (push ix diffs)))
                 (send a :note-bin-diffs diffs)
                 (send b :note-bin-diffs diffs )
                 ))

             (get-diffs ()
               (hash-list-partition (ask a :get-diff-hashes)
                                    (ask b :get-diff-hashes))))
      (print "Pass 1")
      (note-diffs)
      (print "Pass 2")
      (send a :compute-level-2)
      (send b :compute-level-2)
      (note-diffs)
      (multiple-value-bind (diffs-a diffs-b) (get-diffs)
        (let ((objs-a  (ask a :get-objects diffs-a))
              (objs-b  (ask b :get-objects diffs-b)))
          (send a :add-items objs-b)
          (send b :add-items objs-a))
        ))))
|#
;; ---------------------------------------------------------

#|            

(defun store-test (store-a store-b)
  (let ((a  (make-agent-for-change (copy-list store-a)))
        (b  (make-agent-for-change (copy-list store-b))))
    (sync-stores a b)
    (list (ask a :get-store)
          (ask b :get-store))))


|#

;; ------------------------------------------------------------
;; for LZW Compression of plaintext
#|

#|
(declaim (ftype (function (vector vector &optional fixnum fixnum) vector)
                vector-append))
|#
(defun vector-append (old new &optional (start2 0) end2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (prog1 old                     
    (let* ((old-fill (fill-pointer old))
           (new-fill (+ old-fill (length new))))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (replace old new :start1 old-fill :start2 start2 :end2 end2))))

#|
(declaim (ftype (function (vector t) vector) vector-append1))
|#
(defun vector-append1 (old new)
  (prog1 old                     
    (let* ((old-fill (fill-pointer old))
           (new-fill (1+ old-fill)))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (setf (aref old old-fill) new))))

#|
(declaim (ftype (function (&optional t) vector) make-empty-vector))
|#
(defun make-empty-vector (&optional (element-type t))
  (make-array 0 :element-type element-type :fill-pointer 0 :adjustable t))
 

#|
(declaim (ftype (function (t &optional t) vector) make-vector-with-elt))
|#
(defun make-vector-with-elt (elt &optional (element-type t))
  (make-array 1 :element-type element-type
                :fill-pointer 1
                :adjustable t
                :initial-element elt))

#|
(declaim (ftype (function (vector t) vector) vector-append1-new))
|#
(defun vector-append1-new (old new)
  (vector-append1 (vector-append (make-empty-vector 'octet) old)
                  new))

#|
(declaim (ftype (function (vector vector) vector) vector-append-new))
|#
(defun vector-append-new (old new)
  (vector-append (vector-append (make-empty-vector 'octet) old)
                 new))
 
(deftype octet () '(unsigned-byte 8))

#|
(declaim (ftype (function () hash-table) build-dictionary))
|#
(defun build-dictionary ()
  (let ((dictionary (make-hash-table :test #'equalp)))
    (loop for i below 256
          do (let ((vec (make-vector-with-elt i 'octet)))
               (setf (gethash vec dictionary) vec)))
    dictionary))

#|
(declaim (ftype (function ((vector octet)) (vector octet))
                lzw-compress-octets))
|#
(defun lzw-compress-octets (octets)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop with dictionary-size of-type fixnum = 256
        with w = (make-empty-vector 'octet)
        with result = (make-empty-vector 't)
        with dictionary = (build-dictionary)
        for c across octets
        for wc = (vector-append1-new w c)
        if (gethash wc dictionary) do (setq w wc)
        else do
          (vector-append result (gethash w dictionary))
          (setf (gethash wc dictionary)
                (make-vector-with-elt dictionary-size)) 
          (incf dictionary-size)
          (setq w (make-vector-with-elt c 'octet))
        finally (unless (zerop (length (the (vector octet) w)))
                  (vector-append result (gethash w dictionary)))
                (return result)))

#|
(declaim (ftype (function (vector) (vector octet)) lzw-decompress))
|#
(defun #1=lzw-decompress (octets)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (zerop (length octets))
    (return-from #1# (make-empty-vector 'octet)))
  (loop with dictionary-size = 256
        with dictionary = (build-dictionary)
        with result = (make-vector-with-elt (aref octets 0) 'octet)
        with w = (copy-seq result)
        for i from 1 below (length octets)
        for k = (make-vector-with-elt (aref octets i) 't)
        for entry = (or (gethash k dictionary)
                        (if (= (aref k 0) dictionary-size)
                            (vector-append1-new w (aref w 0))
                          (error "bad compresed entry at pos ~S" i)))
        do (vector-append result entry)
           (setf (gethash (make-vector-with-elt dictionary-size) dictionary)
                 (vector-append1-new w (aref entry 0)))
           (incf dictionary-size)
           (setq w entry)
        finally (return result)))

(defgeneric lzw-compress (datum)
  #|
  (:method ((string string))
    (lzw-compress
     (babel:string-to-octets string)))
  |#
  (:method ((octets vector))
    (lzw-compress-octets octets)))

#|
(defun lzw-decompress-to-string (octets)
  (babel:octets-to-string (lzw-decompress octets)))
|#

#|
(defun test (string)
  (assert (equal #2=(lzw-decompress-to-string (lzw-compress string)) string) ()
          "Can't compress ~S properly, got ~S instead" string #2#)
  t)
|#

;; -------------------------------------------------------------------------

(defun cvt-intvec-to-octets (v)
  ;; convert vector of integers to vector of octets using 7-bit
  ;; encodings so that numbers >= 128 become a sequence of 7-bit
  ;; sections with hi-bit set until the final section.  If bit pattern
  ;; of number is: #b1XXX XXXX YYY YYYY ZZZ ZZZZ, then output becomes
  ;; the sequence: #b1XXX XXXX #b1YYY YYYY #b0ZZZ ZZZZ
  (ubstream:with-output-to-ubyte-stream (s)
    (loop for x across v do
          (cond ((> x 127)
                 (write-sequence
                  (um:nlet-tail iter ((x     x)
                                      (hibit 0)
                                      (ans   nil))
                    (let ((acc  (cons (logior hibit (logand x 127))
                                      ans))
                          (xshf (ash x -7)))
                      (if (plusp xshf)
                          (iter xshf #x80 acc)
                        acc)) )
                  s))
                
                (t (write-byte x s))))
    ))

(defun cvt-octets-to-intvec (v)
  ;; convert vector of octets from 7-bit encodings to vector of integers.
  ;; 7-bit values remain as they are. A sequence of octets with hi-bit set
  ;; indicate an integer encoding in 7-bit sections.
  ;; the sequence: #b1XXX XXXX #b1YYY YYYY #b0ZZZ ZZZZ becomes the integer
  ;; with bit pattern: #b1XXX XXXX YYY YYYY ZZZ ZZZZ
  (let ((acc 0)
        (ans (make-empty-vector 't)))
    (loop for x across v do
          (setf acc (logior (ash acc 7) (logand x 127)))
          (unless (> x 127)
            (vector-append1 ans acc)
            (setf acc 0)))
    ans))


|#

;; --------------------------------------------------------------------------
;; Bloom Filter... Is this item a member of a set? No false negatives,
;; but some false positives.
;;
;; Optimal sizing: For N items, we need M bits per item, and K hashing
;; algorithms, where, for false positive rate p we have:
;;
;;  M = -1.44 N Log2 p
;;  K = -Log2 p
;;
;; So, for N = 1000, p < 1%, we have M = 10 N = 10,000 bits, and K = 7
;;
;; We can use successive octets of a SHA256 hash to provide the K hash
;; functions. Since we need 10,000 bits in the table, round that up to
;; 16384 bits = 2^14, so we need 14 bits per hash, make that 2 octets
;; per hash. We need 7 hashes, so that is 14 octets from the SHA256
;; hash value, which offers 32 octets. That's doable....
;;
;; Unfortunately... this mechanism is only applicable to sets as
;; collections without duplicate items. Linda expressly allows
;; duplicate data in its datastore. So this is only a partial solution
;; to keeping tuple-spaces synchronized.
;;
;; This is not a problem for missing items. If an item is found
;; missing from the other data store, then it and all duplicates will
;; be transfered across. The problem arises only for items already in
;; the other data store. We can't keep items in common up to matching
;; duplication levels using Bloom filters.

(defclass bloom-filter ()
  ((bf-nitems  ;; number of items this filter designed for
    :reader  bf-nitems
    :initarg :nitems)
   (bf-pfalse  ;; probability of false positives 
    :reader  bf-pfalse
    :initarg :pfalse)
   (bf-k       ;; number of hashings needed
    :reader  bf-k
    :initarg :bf-k)
   (bf-m       ;; number of bits in table
    :reader  bf-m
    :initarg :bf-m)
   (bf-ix-octets  ;; number of octets needed per hashing key
    :reader  bf-ix-octets
    :initarg :bf-ix-octets)
   (bf-bits       ;; actual bit table
    :accessor bf-bits
    :initarg  :bf-bits)))

(defun pfalse (n k m)
  (expt (- 1d0 (expt (- 1d0 (/ m)) (* k n))) k))

#|
(plt:fplot 'pfalse '(1 20) (lambda (k)
                             (pfalse #N100 k #N2_000))
           :clear t
           :title "Bloom Filter P_false"
           :xtitle "K"
           :ytitle "P_false"
           :ylog t)
|#

(defun make-bloom-filter (&key (pfalse 0.01) (nitems #N1_000))
  (let* ((bf-k         (ceiling (- (log pfalse 2))))
         (bf-m         (um:ceiling-pwr2 (ceiling (* -1.44 nitems (log pfalse 2)))))
         (bf-ix-octets (ceiling (um:ceiling-log2 bf-m) 8)))

    ;; SHA256 is only 32 octets
    (assert (<= (* bf-ix-octets bf-k) 32))
    
    (make-instance 'bloom-filter
                   :nitems  nitems
                   :pfalse  pfalse
                   :bf-k    bf-k
                   :bf-m    bf-m
                   :bf-ix-octets bf-ix-octets
                   :bf-bits (make-array bf-m
                                        :element-type 'bit
                                        :initial-element 0))
    ))

(defun compute-bix (bf hv ix)
  ;; bf points to Bloom Filter
  ;; hv is SHA32 hash octets vector
  ;; ix is starting offset into octents vector
  (mod
   (loop repeat (bf-ix-octets bf)
         for jx from 0
         for nsh from 0 by 8
         sum
         (ash (aref hv (+ ix jx)) nsh))
   (bf-m bf)))
  
(defmethod add-obj-to-bf ((bf bloom-filter) obj)
  (let ((hv  (hash32 obj)))
    (loop repeat (bf-k bf)
          for ix from 0 by (bf-ix-octets bf)
          do
          (let ((bix (compute-bix bf hv ix)))
            (setf (aref (bf-bits bf) bix) 1)))
    hv))

(defmethod test-obj-hash ((bf bloom-filter) hash)
  (loop repeat (bf-k bf)
        for ix from 0 by (bf-ix-octets bf)
        do
        (let ((bix (compute-bix bf hash ix)))
          (when (zerop (aref (bf-bits bf) bix))
            (return nil)))
        finally (return :maybe)))

(defmethod test-membership ((bf bloom-filter) obj)
  (test-obj-hash bf (hash32 obj)))

#|
(let ((x1  '(1 2 :a 43 (a b c)))
      (x2  '(1 :a 43 3 (d e f)))
      (bf  (make-bloom-filter)))
  (um:lc ((:do
              (add-obj-to-bf bf obj))
          (obj <- x1)))
  (inspect bf)
  (um:lc ((test-membership bf obj)
          (obj <- x2))))
  
 |#
;; ---------------------------------------------------------
#|
(linda:when-rdp ((:full-dir-tree ?t))
                (let* ((all (sets:elements ?t))
                       (nel (length all))
                       (bf  (make-bloom-filter :nitems nel))
                       (hashes (um:accum acc
                                 (dolist (file all)
                                   (acc (add-obj-to-bf bf file))))))
                  (linda:out `(:all-files-bloom-filter ,bf))
                  (linda:out `(:all-files ,all))
                  (linda:out `(:all-files-hashes ,hashes))))

(linda:out `(:other-bloom-filter
             ,(car (linda:remote-srdp '(:all-files-bloom-filter ?bf)
                                      "malachite.local"))))

(let* ((other-bf  (car (linda:srdp '(:other-bloom-filter ?bf))))
       (hashes    (car (linda:srdp '(:all-files-hashes ?hashes))))
       (files     (car (linda:srdp '(:all-files ?files))))
       (diffs     (um:accum acc
                    (um:foreach (lambda (hash file)
                                  (unless (test-obj-hash other-bf hash)
                                    (acc file)))
                                hashes files))))
  (with-standard-io-syntax
    (pprint diffs))
  diffs)
|#
;; ---------------------------------------------------------

(defun make-bf-agent-for-change (&key bindings data)
  (make-actor
   (let (hashes)
     (dlambda
       ;; ------------------------------------
       (:get-bloom-data-filter ()
        (let ((bf (make-bloom-filter
                   :nitems (length data))))
          (setf hashes (um:lc (hash
                               (obj <- data)
                               (hash <-f (add-obj-to-bf bf obj)))
                              ))
          bf))
       
       ;; ------------------------------------
       (:get-missing-data-objects (other-bf)
        (um:lc (obj
                ((hash obj) <-// hashes data)
                (not (test-obj-hash other-bf hash)))
               ))
       
       ;; ------------------------------------
       (:add-data-objects (objs)
        (setf data (nconc objs data)))
       
       ;; ------------------------------------
       (:get-store ()
        (list :bindings bindings
              :data     data))
       ))))

;; ---------------------------------------------------------

(defun bf-sync-stores (a b)
  (let* ((bf-a    (ask a :get-bloom-data-filter))
         (bf-b    (ask b :get-bloom-data-filter))
         (objs-a  (ask a :get-missing-data-objects bf-b))
         (objs-b  (ask b :get-missing-data-objects bf-a)))
    (send a :add-data-objects objs-b)
    (send b :add-data-objects objs-a))

  #|
  (let* ((size-a  (ask a :get-bindings-size))
         (size-b  (ask b :get-bindings-size))
         (size    (max size-a size-b))
         (bf-a    (ask a :get-bloom-bindings-filter size))
         (bf-b    (ask b :get-bloom-bindings-filter size))
         (objs-a  (ask a :get-missing-bindings-objects bf-b))
         (objs-b  (ask b :get-missing-bindings-objects bf-a)))
    (send a :add-bindings-objects objs-b)
    (send b :add-bindings-objects objs-a))
  |#
  )

;; ---------------------------------------------------------

#|
(let ((x1  '(1 2 :a 43 (a b c)))
      (x2  '(1 :a 43 3 (d e f)))
      (a   (make-bf-agent-for-change :data x1))
      (b   (make-bf-agent-for-change :data x2)))
  (bf-sync-stores a b)
  (progn ;; inspect
   (list (ask a :get-store)
         (ask b :get-store))))
  |#