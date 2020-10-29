;; brooks-iyengar.lisp - A Lisp implementation of the Brooks-Iyengar distributed sensor fusion algorithm
;;
;; DM/RAL  12/17
;; -----------------------------------------------------------------------------------------------------
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


#|
;; not needed here...
(defun interval-overlap (int1 int2)
  (destructuring-bind (a1 b1) int1
    (destructuring-bind (a2 b2) int2
      (let ((a (max a1 a2))
            (b (min b1 b2)))
        (and (< a b)
             (list a b))
        ))))
|#

(defun partition (ints)
  ;; From a list of intervals, form a list of disjoint sub-intervals
  ;; along with a weight that measures how many of the original
  ;; intervals fell within the sub-interval
  ;;
  ;; Intervals:     (start end)
  ;; Sub-intervals: (count (start end))
  ;;
  (let* ((starts (sort (mapcar #'car ints) #'<))
         (ends   (sort (mapcar #'cadr ints) #'<)))
    (um:nlet-tail iter ((pt     (car starts))
                        (starts (cdr starts))
                        (ends   ends)
                        (ct     1)
                        (parts  nil))
      (if (endp ends)
          parts
        (if (endp starts)
            (let ((new-pt (car ends)))
              (iter new-pt nil (cdr ends) (1- ct)
                    (cons (list ct (list pt new-pt)) parts)))
          ;; else
          (let ((new-pts (car starts))
                (new-pte (car ends)))
            
            (cond ((< new-pts new-pte)
                   (iter new-pts (cdr starts) ends (1+ ct)
                         (cons (list ct (list pt new-pts)) parts)))
                  
                  ((< new-pte new-pts)
                   (iter new-pte starts (cdr ends) (1- ct)
                         (cons (list ct (list pt new-pte)) parts)))
                  
                  (t
                   (iter new-pts (cdr starts) (cdr ends) ct parts))
                  ))
          )))))

(defun b-i-estimate (ints &key (mistrust 1))
  ;; The Brooks-Iyengar algorithm
  ;;
  ;; mistrust is the number of potentially unreliable intervals that
  ;; may exist in the list
  ;;
  (let* ((nints (length ints))
         (parts (remove-if (um:rcurry #'< (- nints mistrust))
                           (partition ints)
                           :key #'car))
         (num 0)
         (den 0)
         a b)
    ;; select out those sub-intervals whose weight exceeds the total
    ;; number minus the mistrusted, then form the weighted average of
    ;; the sub-interval midpoints
    (loop for (ct (start end)) in parts do
          (incf num (* ct 0.5 (+ start end)))
          (incf den ct)
          (setf a (if a
                      (min a start)
                    start)
                b (if b
                      (max b end)
                    end)))
    ;; return the weighted average and the total measurement interval
    ;; used in the calculation
    (values (/ num den)
            (list a b))
    ))

#|
  ;; correct answer: 2.63 (1.5 3.2)  (point value and interval)
(let ((i1 '(2.7 6.7))
      (i2 '(0   3.2))
      (i3 '(1.5 4.5))
      (i4 '(0.8 2.8))
      (i5 '(1.4 4.6)))
  (b-i-estimate (list i1 i2 i3 i4 i5)
                :mistrust 1))
|#
