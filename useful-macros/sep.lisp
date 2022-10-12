
(in-package :useful-macros)

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
  (apply 'paste-strings sep
         (group str count :from-end t)))
