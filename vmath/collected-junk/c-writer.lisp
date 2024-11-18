;; C-writer.lisp -- mechanized C code generation and loop unrolling
;; DM/MCFA  09/00

;; This code was inspired by a day spent manually unrolling some loops
;; in a high-speed C version of a 3-parameter optimization routine.
;; The experience was awful enough that the thought of doing it again
;; for 6 parameters seems too horrible to contemplate... Hence why not
;; let Lisp do it for us?


#| Test case...

  for(i = 0; i < 3; ++i)
        for(j > i; j < 3; ++j)
              do_something_with(arr[i,j]);

...should unfold to...

   do_something_with(arr[0,1]);
   do_something_with(arr[0,2]);
   do_something_with(arr[1,2]);

|#

(defparameter *emit-pipe* nil)

(defun emit (code)
  (push code *emit-pipe*))

(defun c-for-expansion (start limitfn incr fn)
  (do ((ix start (+ ix incr)))
      ((funcall (complement limitfn) ix))
    (funcall fn ix)))

(defmacro c-for ((i start (test limit) &key (incr 1)) &body body)
  `(c-for-expansion ,start
                    #'(lambda (,i)
                        (,test ,i ,limit))
                    ,incr
                    #'(lambda (,i) ,@body)))

(defmacro c-for++ ((i start (test limit)) &body body)
  `(c-for (,i ,start (,test ,limit) :incr 1) ,@body))

(defmacro c-for-- ((i start (test limit)) &body body)
  `(c-for (,i ,start (,test ,limit) :incr -1) ,@body))

(defmacro collect-code (&body body)
  `(let ((*emit-pipe* nil))
     ,@body
    (nreverse *emit-pipe*)))

;; Now for the prize!

(defun unfold-cholesky (dim)
  (collect-code

    ;; first the Cholesky decomposition
    (c-for++ (i 0 (< dim))

      (emit `(sum = (1 + lambda) * (aref alpha (,i * ,dim) ,i)
                  + regularization))

      (c-for-- (k (1- i) (>= 0))
        (emit `(sum -= (aref alpha (,i * ,dim) ,k))))

      (emit `(if (sum <= 0) goto recover_with_regularization))
      (emit `((aref pval ,i) = sf = 1.0 / sqrt(sum)))

      (c-for++ (j (1+ i) (< dim))
        (emit `(sum = (aref alpha (,i * ,dim) ,j)))

        (c-for-- (k (1- i) (>= 0))
          (emit `(sum -= (aref alpha (,i * ,dim) ,k) *
                      (aref alpha (,j * ,dim) ,k))))

        (emit `((aref a (,j * ,dim) ,i) = sf * sum))))

    ;; Next the Cholesky solution of a system
    (c-for++ (i 0 (< dim))

      (emit `(sf = (aref pval ,i)))
      (emit `(sum = (aref beta ,i)))

      (c-for-- (k (1- i) (>= 0))
        (emit `(sum -= (aref a (,i * ,dim) + ,k) * (aref beta ,k))))

      (emit `((aref dparm ,i) = sf * sum)))

    (c-for-- (i (1- dim) (>= 0))

      (emit `(sf = (aref pval ,i)))
      (emit `(sum = (aref dparm ,i)))

      (c-for++ (k (1+ i) (< dim))
        (emit `(sum -= (aref a (,k * ,dim) ,i) * (aref dparm ,k))))

      (emit `((aref dparm ,i) = sf * sum)))
    ))


(defun print-unfold-cholesky (dim)

  (labels
      ((aref2 (arr x y)
              (format nil "~a[~d*~d+~d]" arr x dim y))
       (aref1 (arr x)
              (format nil "~a[~d]" arr x))
       (cref  (arr x &optional y)
              (if y (aref2 arr x y)
                (aref1 arr x)))
       (emit  (&rest items)
              (format t "~&")
              (dolist (item items)
                (format t item))
              (format t ";"))
       (comment (str)
                (format t "~&/* ~a */" str)))

    (comment
     "This routine is written so that the alpha and beta matrices")
    (comment
     "are considered immutable. A secondary alpha matrix called \"a\"")
    (comment
     "and a secondar beta vector called \"dparm\" are used")
    (comment 
     "to hold modifications.")
    (comment
     "The routine is modified to include the Levenberg-Marquardt")
    (comment
     "weighting of the alpha diagonal terms, as well as soft-recovery")
    (comment
     "from near singular solutions by means of gradual regularization.")
    (comment "")
    (comment
     "Mechanically open coded from Lisp expansion of the C-loops.")
    (comment
     "\"c-writer.lisp\"  DM/MCFA 09/00")
    (comment "")
    
    ;; first the Cholesky decomposition
    (comment "Cholesky decomposition")
    (c-for++ (i 0 (< dim))

      (emit "sum = (1.0f + lambda) * " (cref "alpha" i i)
            " + regularization")
    
    (c-for-- (k (1- i) (>= 0))
      (emit "sum -= " (cref "a" i k)))

    (emit "if(sum <= 0.0f) goto recover_with_regularization")
    (emit (cref "pval" i) " = sf = (float)(1.0 / sqrt(sum))")
    
    (c-for++ (j (1+ i) (< dim))
      (emit "sum = " (cref "alpha" i j))
      
      (c-for-- (k (1- i) (>= 0))
        (emit "sum -= " (cref "a" i k) " * " (cref "a" j k)))

      (emit (cref "a" j i) " = sf * sum")))

    ;; Next the Cholesky solution of a system
    (comment "now use the Cholesky decomposition to solve a system")
    (c-for++ (i 0 (< dim))

      (emit "sum = " (cref "beta" i))
      (c-for-- (k (1- i) (>= 0))
        (emit "sum -= " (cref "a" i k) " * " (cref "dparm" k)))

      (emit (cref "dparm" i) " = " (cref "pval" i) " * sum"))

    (c-for-- (i (1- dim) (>= 0))

      (emit "sum = " (cref "dparm" i))
      (c-for++ (k (1+ i) (< dim))
        (emit "sum -= " (cref "a" k i) " * " (cref "dparm" k)))

      (emit (cref "dparm" i) " = " (cref "pval" i) " * sum"))
    ))



