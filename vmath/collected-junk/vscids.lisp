;; vscids.lisp -- vectorized ops on SciDS vars
;; DM/MCFA  09/99

(defun ripple-add (a b dims)
  (labels ((add (a b lim &optional (cy 0))
                (if (null lim)
                    nil
                  (let ((sum (+ (or (car a) 0) (or (car b) 0) cy)))
                    (if (>= sum (car lim))
                        (cons 0 (add (cdr a) (cdr b) (cdr lim) 1))
                      (cons sum (add (cdr a) (cdr b) (cdr lim)))))
                  )))
    (nreverse (add (reverse a) (reverse b) (reverse dims)))
    ))

(defun rev-take (n lst &optional rslt)
  (if (plusp n)
      (rev-take (1- n) (cdr lst) (cons (car lst) rslt))
    rslt))

(defun drop (n lst)
  (if (plusp n)
      (drop (1- n) (cdr lst))
    lst))

(defun get-array-of-arrays (fid varname nsubdims)
  (let ((vdef (scids:get-var-defn fid varname)))
    (let* ((dims    (coerce (getf vdef :dimensions) 'list))
           (revdims (reverse dims))
           (nel     (reduce #'* revdims :start nsubdims))
           (adims   (rev-take nsubdims revdims))
           (ans     (make-array (reverse (drop nsubdims revdims))))
           (vans    (make-array nel :displaced-to ans))
           (scids-count (append (make-list (- (length dims) nsubdims)
                                           :initial-element 1)
                                adims))
           (scids-offset (make-list (length dims)
                                    :initial-element 0))
           (scids-incr   (cons 1
                               (make-list nsubdims :initial-element 0))))
      (dotimes (ix nel)
        (setf (aref vans ix)
              (scids:get-variable fid varname
                                  :start scids-offset
                                  :count scids-count))
        (setf scids-offset (ripple-add scids-offset scids-incr dims)))
      ans)))

(defun get-array-of-images (fid varname)
  (get-array-of-arrays fid varname 2))

(defun get-array-of-vectors (fid varname)
  (get-array-of-arrays fid varname 1))


(defun tst (varname)
  (scids:with-file (fid ())
    (get-array-of-images fid varname)))


(defun tst2 (varname)
  (scids:with-file (fid :remove-if-present t
;;                         ))
                         :type :CDF)
    (let ((dims '(2 3 4 5))
          (scids-offset '(0 0 0 0))
          (scids-count '(1 1 4 5)))
      (scids:define-variable fid varname
                             :type :R4
                             :dimensions dims)
      (dotimes (ix 6)
        (let ((arr (make-array '(4 5) :initial-element 0
                               :element-type 'integer)))
          (setf (aref arr 0 0) (1+ ix))
          (scids:set-variable fid varname arr
                              :start scids-offset
                              :count scids-count))
        (setf scids-offset (ripple-add scids-offset '(1 0 0) dims)))
      )))

