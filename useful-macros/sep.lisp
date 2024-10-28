
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
        

(defun best-sep ()
  (case *print-base*
    (10.  #\,)
    (t    #\_)))

(defun best-sep-interval ()
  (case *print-base*
    ((10. 8.)  3)
    ((16. 2.)  4)
    (t         5)))

(defun sepi (str &key
                 (sep   (best-sep))
                 (count (best-sep-interval))
                 (base *print-base*))
  ;; Insert separators into an integer string.
  ;; String can have leading sign, and trailing non-digits, e.g., decimal point?
  (labels
      ((scan (pos chars)
         ;; deal with trailing non-digits?
         (if (minusp (decf pos))
             (coerce chars 'string)
           (let ((ch (char str pos)))
             (if (digit-char-p ch base)
                 (scan2 pos 1 (cons ch chars) (subseq str (1+ pos)))
               (scan pos (cons ch chars)))
             )))
       (scan2 (pos ct chars suff)
         ;; deal with digits sequence
         (if (minusp (decf pos))
             (coerce chars 'string)
           (let ((ch (char str pos)))
             (cond ((digit-char-p ch base)
                    (if (>= ct count)
                        (scan2 pos 1 (list* ch sep chars) suff)
                      (scan2 pos (1+ ct) (cons ch chars) suff)))
                   (t
                    ;; leading sign?
                    (concatenate 'string (subseq str 0 (1+ pos))
                                 (coerce chars 'string)
                                 suff))
                   ))
           )))
    (scan (length str) nil)))
