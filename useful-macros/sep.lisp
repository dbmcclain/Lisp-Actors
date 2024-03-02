
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
  (let* ((wstr  (reverse str))
         (nel   (length wstr)))
    (labels
        ((scan (pos ct chars)
           (if (>= pos nel)
               (coerce chars 'string)
             (let ((ch (char wstr pos)))
               (cond ((digit-char-p ch *print-base*)
                      (if (>= ct count)
                          (scan (1+ pos) 1 (list* ch sep chars))
                        (scan (1+ pos) (1+ ct) (cons ch chars))))
                     (t
                      (loop for ix from pos below nel do
                              (push (char wstr ix) chars))
                      (scan nel 0 chars))
                     ))
             )))
      (scan 0 0 nil))
    ))
