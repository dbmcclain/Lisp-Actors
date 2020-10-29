;; match-macro-ex2.lisp -- Compiled Pattern Matching - 2nd pass
;;
;; DM/HMSC  03/09
;; -----------------------------------------------------------
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

(in-package "USEFUL-MACROS")

;; -----------------------------------------
;; Optimization of generated match clauses
;; -----------------------------------------

(defun occurrences (item tree)
  (count item (flatten tree) :test 'eq))

(defun wrap-progn (body)
  (if (second body)
      (cons 'progn body)
    (first body)))

;; ---------------------------------------------------------------------
#|
(defun merge-clauses (bodies1 body2 &optional (bodies bodies1) accum)
  (if bodies
      (let ((ans (try-merge-clauses (car bodies) body2)))
        (if (cdr ans)
            (merge-clauses bodies1 body2 (cdr bodies) (append accum (list (car bodies))))
          (append accum ans (cdr bodies))))
    (append bodies1 (list body2))))
|#
(defun merge-clauses (bodies1 body2)
  (if bodies1
      (let ((ans (try-merge-clauses (last1 bodies1) body2)))
        (if (cdr ans)
            (append bodies1 (list body2))
          (append (butlast bodies1) ans)))
    (append bodies1 (list body2))))

#|
(defun try-merge-clauses (t1 t2)
  (if (eql-tree t1 t2)
      (list t1)
    ;; else
    (match2 t1 t2
      
      (('let ((v1 e1)) . bodies1) ('let ((v2 e2)) body2)
       (declare (cons bodies1 body2))
       (if (eql-tree e1 e2)
           (let* ((new-body2   (subst v1 v2 body2))
                  (new-bodies  (merge-clauses bodies1 new-body2)))
             `((let ((,v1 ,e1)) ,@new-bodies)))
         (list t1 t2)))
    
      (('when e1 . bodies1) ('when e2 body2)
       (declare (cons bodies1 body2))
       (if (eql-tree e1 e2)
           `((when ,e1 ,@(merge-clauses bodies1 body2)))
         (list t1 t2)))
      
      (('unless e1 . bodies1) ('unless e2 body2)
       (declare (cons bodies1 body2))
       (if (eql-tree e1 e2)
           `((unless ,e1 ,@(merge-clauses bodies1 body2)))
         (list t1 t2)))

      ( _ _
          (list t1 t2))
      )))
|#

(defun try-merge-clauses (t1 t2)
  (LET ((#27=#:ANS20799
         (SYMBOL-MACROLET ((#15=#:LBL20800
                            (LAMBDA (V1 E1 BODY1 V2 E2 BODY2)
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              (DECLARE (CONS BODY1 BODY2))
                              (IF (EQL-TREE E1 E2)
                                  (LET* ((NEW-BODY2 (SUBST V1 V2 BODY2))
                                         (NEW-BODY  (MERGE-CLAUSES BODY1 NEW-BODY2)))
                                    `((LET ((,V1 ,E1)) ,@NEW-BODY)))
                                (LIST T1 T2))))
                           (#20=#:LBL20801
                            (LAMBDA (E1 BODIES1 E2 BODY2)
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              (DECLARE (CONS BODIES1 BODY2))
                              (IF (EQL-TREE E1 E2)
                                  `((WHEN ,E1 ,@(MERGE-CLAUSES BODIES1 BODY2)))
                                (LIST T1 T2))))
                           (#25=#:LBL20802
                            (LAMBDA (E1 BODIES1 E2 BODY2)
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              (DECLARE (CONS BODIES1 BODY2))
                              (IF (EQL-TREE E1 E2)
                                  `((UNLESS ,E1 ,@(MERGE-CLAUSES BODIES1 BODY2)))
                                (LIST T1 T2))))
                           (#26=#:LBL20803
                            (LAMBDA ()
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              (LIST T1 T2))))
                          (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))
                          (BLOCK #14=#:BLOCK20798
                            (LET ((#1=#:ARG20797 (THE CONS (CONS T1 T2))))
                              (DECLARE (DYNAMIC-EXTENT #1#))
                              (WHEN (CONSP #1#)
                                (LET ((#2=#:VAR20858 (CAR (THE CONS #1#))))
                                  (WHEN (CONSP #2#)
                                    (LET ((#3=#:VAR20857 (CAR (THE CONS #2#))))
                                      (WHEN (EQ #3# 'LET)
                                        (LET ((#4=#:TL20807 (CDR (THE CONS #2#))))
                                          (WHEN (CONSP #4#)
                                            (LET ((#5=#:HD20808 (CAR (THE CONS #4#))))
                                              (WHEN (CONSP #5#)
                                                (LET ((#6=#:HD20810 (CAR (THE CONS #5#))))
                                                  (WHEN (CONSP #6#)
                                                    (LET ((#7=#:TL20813 (CDR (THE CONS #6#))))
                                                      (WHEN (CONSP #7#)
                                                        (UNLESS (CDR (THE CONS #7#))
                                                          (UNLESS (CDR (THE CONS #5#))
                                                            (LET ((#8=#:TL20805
                                                                   (CDR (THE CONS #1#))))
                                                              (WHEN (CONSP #8#)
                                                                (WHEN (EQ (CAR (THE CONS #8#))
                                                                          'LET)
                                                                  (LET ((#9=#:TL20817
                                                                         (CDR (THE CONS #8#))))
                                                                    (WHEN (CONSP #9#)
                                                                      (LET ((#10=#:HD20818
                                                                             (CAR (THE CONS
                                                                                       #9#))))
                                                                        (WHEN (CONSP #10#)
                                                                          (LET ((#11=#:HD20820
                                                                                 (CAR (THE CONS
                                                                                           #10#))))
                                                                            (WHEN (CONSP #11#)
                                                                              (LET ((#12=#:TL20823
                                                                                     (CDR (THE CONS
                                                                                               #11#))))
                                                                                (WHEN (CONSP #12#)
                                                                                  (UNLESS (CDR (THE CONS
                                                                                                    #12#))
                                                                                    (UNLESS (CDR (THE CONS
                                                                                                      #10#))
                                                                                      (LET ((#13=#:TL20819
                                                                                             (CDR (THE CONS
                                                                                                       #9#))))
                                                                                        (WHEN (CONSP #13#)
                                                                                          (UNLESS (CDR (THE CONS
                                                                                                            #13#))
                                                                                            (RETURN-FROM #14#
                                                                                              (LIST #15#
                                                                                                    (CAR (THE CONS
                                                                                                              #6#))
                                                                                                    (CAR (THE CONS
                                                                                                              #7#))
                                                                                                    (CDR (THE CONS
                                                                                                              #4#))
                                                                                                    (CAR (THE CONS
                                                                                                              #11#))
                                                                                                    (CAR (THE CONS
                                                                                                              #12#))
                                                                                                    (CAR (THE CONS
                                                                                                              #13#)))))))))))))))))))))))))))))))
                                      (WHEN (EQ #3# 'WHEN)
                                        (LET ((#16=#:TL20831 (CDR (THE CONS #2#))))
                                          (WHEN (CONSP #16#)
                                            (LET ((#17=#:TL20829 (CDR (THE CONS #1#))))
                                              (WHEN (CONSP #17#)
                                                (WHEN (EQ (CAR (THE CONS #17#)) 'WHEN)
                                                  (LET ((#18=#:TL20835 (CDR (THE CONS #17#))))
                                                    (WHEN (CONSP #18#)
                                                      (LET ((#19=#:TL20837
                                                             (CDR (THE CONS #18#))))
                                                        (WHEN (CONSP #19#)
                                                          (UNLESS (CDR (THE CONS #19#))
                                                            (RETURN-FROM #14#
                                                              (LIST #20#
                                                                    (CAR (THE CONS #16#))
                                                                    (CDR (THE CONS #16#))
                                                                    (CAR (THE CONS #18#))
                                                                    (CAR (THE CONS
                                                                              #19#)))))))))))))))
                                      (WHEN (EQ #3# 'UNLESS)
                                        (LET ((#21=#:TL20843 (CDR (THE CONS #2#))))
                                          (WHEN (CONSP #21#)
                                            (LET ((#22=#:TL20841 (CDR (THE CONS #1#))))
                                              (WHEN (CONSP #22#)
                                                (WHEN (EQ (CAR (THE CONS #22#)) 'UNLESS)
                                                  (LET ((#23=#:TL20847 (CDR (THE CONS #22#))))
                                                    (WHEN (CONSP #23#)
                                                      (LET ((#24=#:TL20849
                                                             (CDR (THE CONS #23#))))
                                                        (WHEN (CONSP #24#)
                                                          (UNLESS (CDR (THE CONS #24#))
                                                            (RETURN-FROM #14#
                                                              (LIST #25#
                                                                    (CAR (THE CONS #21#))
                                                                    (CDR (THE CONS #21#))
                                                                    (CAR (THE CONS #23#))
                                                                    (CAR (THE CONS
                                                                              #24#)))))))))))))))))
                                  (RETURN-FROM #14# (LIST #26#))))
                              (MATCH-FAIL))))))
    (DECLARE (CONS #27#))
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))
    (APPLY (CAR #27#) (CDR #27#))) )

;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; Liveness analysis - not really needed because the compiler does this anyway...
;; (but it sure looks good...)
#| |#
#|
(defun liveness-anal (tree)
  (match tree
    (('let ((v e)) . bodies)
     (declare (cons bodies))
     (let ((new-bodies (liveness-anal bodies)))
       (declare (cons new-bodies))
       (if (or (symbolp e)
               (< (occurrences v new-bodies) 2))
           (if (cdr new-bodies)
               (wrap-progn (subst e v new-bodies))
             (subst e v (car new-bodies)))
       `(let ((,v ,e)) ,@new-bodies))))

    ((_ . _)
     (declare (cons tree))
     (match (mapcar 'liveness-anal tree)
       (('when e ('progn . bodies)) `(when ,e ,@bodies))
       ( form form)))
    
    (_ tree)))
|#

(defun liveness-anal (tree)
 (LET ((#17=#:ANS10050
         (SYMBOL-MACROLET ((#13=#:LBL10051
                            (LAMBDA #14=(V E BODIES)
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              (DECLARE (CONS BODIES))
                              (LET ((NEW-BODIES (LIVENESS-ANAL BODIES)))
                                (DECLARE (CONS NEW-BODIES))
                                (IF (OR (SYMBOLP E) (< (OCCURRENCES V NEW-BODIES) 2))
                                    (IF (CDR NEW-BODIES)
                                        (WRAP-PROGN (SUBST E V NEW-BODIES))
                                      (SUBST E V (CAR NEW-BODIES)))
                                  `(LET ((,V ,E)) ,@NEW-BODIES)))))
                           (#15=#:LBL10052
                            (LAMBDA ()
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              (DECLARE (CONS TREE))
                              (MAPCAR 'LIVENESS-ANAL TREE)))
                           (#16=#:LBL10053
                            (LAMBDA ()
                              (DECLARE (OPTIMIZE
                                        (SAFETY 3)
                                        (SPEED 1)
                                        (SPACE 1)
                                        (FLOAT 1)
                                        (COMPILATION-SPEED 1)
                                        (DEBUG 2)
                                        #+:LISPWORKS
                                        (HARLEQUIN-COMMON-LISP:FIXNUM-SAFETY 3)))
                              TREE)))
                          #18=(DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))
                          (BLOCK #12=#:BLOCK10049
                            (LET ((#1=#:ARG10048 TREE))
                              (DECLARE (DYNAMIC-EXTENT #1#))
                              (WHEN (CONSP #1#)
                                (LET ((#2=#:HD10054 (CAR (THE CONS #1#))))
                                  (WHEN (EQ #2# 'LET)
                                    (LET ((#3=#:TL10055 (CDR (THE CONS #1#))))
                                      (WHEN (CONSP #3#)
                                        (LET ((#4=#:HD10056 (CAR (THE CONS #3#))))
                                          (WHEN (CONSP #4#)
                                            (LET ((#5=#:HD10058 (CAR (THE CONS #4#))))
                                              (WHEN (CONSP #5#)
                                                (LET ((#6=#:HD10060 (CAR (THE CONS #5#))))
                                                  (LET ((V #6#))
                                                    (LET ((#7=#:TL10061 (CDR (THE CONS #5#))))
                                                      (WHEN (CONSP #7#)
                                                        (LET ((#8=#:HD10062 (CAR (THE CONS #7#))))
                                                          (LET ((E #8#))
                                                            (LET ((#9=#:TL10063
                                                                   (CDR (THE CONS #7#))))
                                                              (UNLESS #9#
                                                                (LET ((#10=#:TL10059
                                                                       (CDR (THE CONS #4#))))
                                                                  (UNLESS #10#
                                                                    (LET ((#11=#:TL10057
                                                                           (CDR (THE CONS #3#))))
                                                                      (LET ((BODIES #11#))
                                                                        (RETURN-FROM #12#
                                                                          (LIST #13#
                                                                                . #14#)))))))))))))))))))))
                                  (LET ((#:TL10065 (CDR (THE CONS #1#))))
                                    (RETURN-FROM #12# (LIST #15#)))))
                              (RETURN-FROM #12# (LIST #16#))
                              (MATCH-FAIL))))))
    (DECLARE (CONS #17#))
    #18#
    (APPLY (CAR #17#) (CDR #17#))) )
#| |#

;; ---------------------------------------------------------------------

(defun optimize-clauses (clauses)
  (declare (cons clauses))
  ;; (return-from optimize-clauses clauses)
  (liveness-anal
   (um:foldl 'merge-clauses
             (list (car clauses)) (cdr clauses))))
              
;; ---------------------------------------------------------------------
