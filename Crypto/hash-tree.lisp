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

(defun std-hash (&rest msgs)
  (convert-bytes-to-int
   (apply #'basic-hash-with-protocol :sha256 msgs)))

(defun hash-tree (tree)
  (cond ((consp tree)
         (if (cdr tree) ;; if more than one element
             (apply #'std-hash (mapcar #'hash-tree tree))
           (hash-tree (car tree))))
        (t tree)))

(defun ht-path-to (n tree)
  `(lambda (sig)
     ,(um:nlet iter ((tree tree))
        (cond ((consp tree)
               (um:if-let (subtree (iter (car tree)))
                   `(std-hash ,subtree ,(hash-tree (cadr tree)))
                 (um:if-let (subtree (iter (cadr tree)))
                     `(std-hash ,(hash-tree (car tree)) ,subtree))))
              
              ((equalp n tree)
               `(std-hash sig))
              )) ))

(defun print-ht (tree &optional (stream t))
  (labels ((ill-formed ()
             (error "Ill-formed tree")))
    (um:nlet iter ((tree tree))
      (cond ((consp tree)
             (cond ((equalp 'lambda (car tree))
                    (iter (third tree)))
                   ((equalp 'std-hash (car tree))
                    (cond ((numberp (second tree))
                           (iter (third tree))
                           (format stream "~&L:~A" (second tree)))
                          ((numberp (third tree))
                           (iter (second tree))
                           (format stream "~&R:~A" (third tree)))
                          (t (ill-formed))))
                   (t (ill-formed))))
            ((equalp 'sig tree))
            (t (ill-formed)) ) )))

(defparameter *tstamps*
  (loop repeat 7 collect (std-hash 0)))

(defun get-ts (sig)
  (let* ((tree (tree-ize 
                (reverse (cons sig (um:take 7 *tstamps*)))))
         (ans  (let ((*print-base* 16))
                 (with-output-to-string (s)
                   (print-ht (ht-path-to sig tree) s))))
         (hash (hash-tree tree)))
    (print hash)
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
