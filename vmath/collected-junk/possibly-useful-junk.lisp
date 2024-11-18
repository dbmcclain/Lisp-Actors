;; Partial quicksort as adopted from Knuth
;; (hopefully faster than using sort on the whole list)
(defun select-largest (vals)
  ;; this routine will be destructive if the argument vals is already a vector...
  (let* ((vvals (coerce vals 'vector))
         (limit (1- (length vvals)))
         (k     limit))
    (labels ((iter-lr (l r)
               (let* ((v (abs (aref vvals k))))
                 (labels ((iter-i (ix)
                            (if (< (abs (aref vvals ix)) v)
                                (iter-i (1+ ix))
                              ix))

                          (iter-j (ix)
                            (if (> (abs (aref vvals ix)) v)
                                (iter-j (1- ix))
                              ix))
                          
                          (iter-ij (i j)
                            (let* ((new-i (iter-i i))
                                   (new-j (iter-j j)))
                              (if (<= new-i new-j)
                                  (let* ((next-i (1+ new-i))
                                         (next-j (1- new-j))
                                         (tmp       (aref vvals new-i)))
                                    (setf (aref vvals new-i) (aref vvals new-j)
                                          (aref vvals new-j) tmp)
                                    (if (<= next-i next-j)
                                        (iter-ij next-i next-j)
                                      (list next-i next-j)))
                                (list new-i new-j))
                              )))
                   (destructuring-bind (i j) (iter-ij l r)
                     (let* ((new-l (if (< j k)
                                       i
                                     l))
                            (new-r (if (< k i)
                                       j
                                     r)))
                       (if (< new-l new-r)
                           (iter-lr new-l new-r))
                       ))
                   ))
               ))
      (iter-lr 0 limit)
      (aref vvals k)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vmin-not-zero (v)
  (let* ((xmin (find-if-not #'zerop v)))
    (if xmin
        (progn
          (loop for x across v
                when (not (zerop x))
                do
                (setf xmin (min xmin x)))
          xmin)
      0)
    ))

