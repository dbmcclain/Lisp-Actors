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


(defun tree-ize (lst)
  (um:nlet iter ((lst lst))
    (if (> (length lst) 2)
        (iter (um:group lst 2))
      lst )))

(defparameter *ht*
  (tree-ize '(1 2 3 4 5 6 7 8)))

(defun leaf-fn (n arg)
  (ecc-mul *ecc-gen* (* n arg)))

(defun node-fn (&rest msgs)
  (reduce #'ecc-add
          msgs
          :initial-value '(0 0)))

(defun hash-tree (tree)
  (let ((ctr 0))
    (um:nlet iter ((subtree tree))
      (cond ((consp subtree)
             (if (cdr subtree) ;; if more than one element
                 (apply #'node-fn (mapcar #'iter subtree))
               (iter (car subtree))))
            (t (leaf-fn (incf ctr) subtree))))))

(defun ht-path-to (n tree)
  `(lambda (sig)
     ,(um:nlet iter ((tree tree))
        (cond ((consp tree)
               (um:if-let (subtree (iter (car tree)))
                   `(node-fn ,subtree ',(hash-tree (cadr tree)))
                 (um:if-let (subtree (iter (cadr tree)))
                     `(node-fn ',(hash-tree (car tree)) ,subtree))))
              
              ((eql n tree)
               `(leaf-fn sig))
              )) ))

(defun print-ht (tree &optional (stream t))
  (labels ((ill-formed ()
             (error "Ill-formed tree")))
    (um:nlet iter ((tree tree))
      (cond ((consp tree)
             (case (car tree)
               (lambda  (iter (third tree)))
               (leaf-fn)
               (node-fn
                (cond ((eql 'quote (car (second tree)))
                       (iter (third tree))
                       (let ((*print-base* 16))
                         (format stream "~&L:~A" (cadr (second tree)))))
                      ((eql 'quote (car (third tree)))
                       (iter (second tree))
                       (let ((*print-base* 16))
                         (format stream "~&R:~A" (cadr (third tree)))))
                      (t (ill-formed))))
               (t (ill-formed))))
            (t (ill-formed)))) ))

(defparameter *tstamps*
  (loop repeat 7 collect (leaf-fn 0)))

(defun get-ts (sig)
  (let* ((leaf (leaf-fn sig))
         (tree (tree-ize 
                (reverse (cons leaf
                               (um:take 7 *tstamps*)))))
         (ans  (with-output-to-string (s)
                 (print-ht (ht-path-to leaf tree) s)))
         (hash (hash-tree tree)))
    (let ((*print-base* 16))
      (print hash))
    (push hash *tstamps*)
    ans))

#|
;; (setf ts (get-ts (std-hash 9)))
(setf ts "L:B095EECB6E8FFDC325B1F48A50C632C5D58FB627BF9A173ED54A107B5DF21672
L:9DFA6BBEE0483C1C496738EEAECF19945E42D7FFE7E550901A4138927355E1A7
L:C24539F265A18137429F2EB7C4C1086EA5BB96F613E6BBBBD7CC425DE284E1BB")

|#

(defun parse-ts-line (line)
  (multiple-value-bind (start end gstart gend)
      (#~m/^([LRlr]):([0-9A-Fa-f]+)$/ line)
    (declare (ignore start end))
    (let ((lr   (subseq line (aref gstart 0) (aref gend 0)))
          (hash (subseq line (aref gstart 1) (aref gend 1))))
      (values
       (case (char lr 0)
         ((#\l #\L) :left)
         ((#\r #\R) :right)
         (t (error "Invalid TS")) )
       (let ((*read-base* 16))
         (read-from-string hash)) ))))

(defun eval-ts (sig ts)
  (with-input-from-string (s ts)
    (um:nlet iter ()
      (let ((line (read-line s nil s)))
        (if (eq s line)
            sig
          (multiple-value-bind (dir hash)
              (parse-ts-line line)
            (setf sig
                  (case dir
                    (:left  (std-hash hash sig))
                    (:right (std-hash sig hash)) ))
            (iter))) ))))
  
(defun check-ts (sig ts)
  (find (eval-ts sig ts) *tstamps*))

;; --------------------------------------------

#|
(setf xs
      (let ((pt *ecc-gen*)
            (ans (make-array (ash 1 16)
                             :element-type 'fixnum)))
        (loop for ix from 0 below (ash 1 16) do
              (progn
                (setf (aref ans ix)
                      (logand 1 (first pt))
                      pt (ecc-add pt *ecc-gen*))))
        ans))
(plt:plot 'plt xs :clear t)
|#

(defun ecc-hash (msg &key (key 1))
  (let* ((delta (ecc-mul *ecc-gen* key))
         (pt    delta)
         (msg   (ensure-8bitv msg))
         (len   (length msg))
         (nblks (truncate len 64))
         (tail  (* nblks 64))
         (sum   (ecc-infinity)))

    (labels ((xor (src)
               (let ((n (convert-bytes-to-int src)))
                 (setf sum (ecc-add sum
                                    ;; avoid mult by 0
                                    (ecc-mul pt (+ n n 1)))
                       pt  (ecc-add pt delta)))))
      
      (xor (convert-int-to-bytes len))
      (loop repeat nblks
            for ix from 0 by 64
            do
            (xor (subseq msg ix (+ ix 64))))
      (when (< tail len)
        (xor (subseq msg tail len)))
      (first sum))))
      
(defun ecc-ctr-encrypt (msg key)
  (let* ((delta (ecc-mul *ecc-gen* key))
         (pt    delta)
         (msg   (ensure-8bitv msg))
         (len   (length msg))
         (nblks (truncate len 64))
         (tail  (* nblks 64))
         (out   (make-ub-array len)))

    (labels ((xor (start end)
               (let* ((submsg (subseq msg start end))
                      (mask   (convert-int-to-nbytes (first pt) (- end start))))
                 (map-into submsg #'logxor submsg mask)
                 (replace out submsg
                          :start1 start)
                 (setf pt (ecc-add pt delta)))))
      
      (loop repeat nblks
            for ix-start from  0 by 64
            for ix-end   from 64 by 64
            do
            (xor ix-start ix-end))
      
      (when (< tail len)
        (xor tail len))

      out)))
                    
    