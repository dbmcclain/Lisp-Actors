
(defpackage :com.ral.useful-macros.sep
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.sep)

;; Add separators between groups of digits in large FP fractions and
;; large Integers

(defun sepfp (str &key (sep #\_) (count 5))
  (let ((pos nil))
    (cond
     ((setf pos (position #\. str))
      (incf pos)
      (let* ((pref  (subseq str 0 pos))
             (end   (position-if (complement 'digit-char-p) str
                                 :start pos))
             (tail  (if end
                        (subseq str end)
                      ""))
             (mid   (apply 'paste-strings sep
                           (group (subseq str pos end)
                                     count))))
        (concatenate 'string pref mid tail)))
     (t
      str)
     )))
        
    
(defun sepi (str &key (sep #\_) (count 5))
  (labels
      ((scan (pos ct chars)
         (if (minusp (decf pos))
             (coerce chars 'string)
           (let ((ch (char str pos)))
             (cond ((digit-char-p ch *print-base*)
                    (if (>= ct count)
                        (scan pos 1 (list* ch sep chars))
                      (scan pos (1+ ct) (cons ch chars))))
                   (t
                    (concatenate 'string (subseq str 0 (1+ pos))
                                 (coerce chars 'string)))
                   ))
           )))
    (scan (length str) 0 nil)))
