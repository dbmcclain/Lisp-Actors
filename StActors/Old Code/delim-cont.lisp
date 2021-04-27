
(defstruct (cont (:constructor make-cont (fn)))
  ;; cont-fn : cont:(fn:('a -> 'b) -> 'b)
  (fn #'values :type function))

(defun run-cont (c k)
  ;; (cont:(fn:('a -> 'b) -> 'b), fn:('a -> 'b)) -> 'b
  (funcall (cont-fn c) k))

(defun returnc (x)
  ;; 'a -> cont:(fn: ('a -> 'b) -> 'b)
  (make-cont (lambda (k)
               (funcall k x))))
#|
 Equiv:

 (run-cont (retiurnc x) #'values)
 <=>
 (with-cont
  (=funcall (=lambda ()
                     (=values x))))
 
 (run-cont c %s) <=> (=bind (x)
                         (=funcall c)
                       ...)
 
 (returnc x) <=> (=lambda ()
                          (=values x))

 (=bind (x)
     form
   body)

 (=bind (x)
     (=funcall (=lambda ()
                        form))
   body)

 (=bind (x)
     (run-cont (make-cont
                (lambda (k)
                  form))
               =bind-cont)
   body)
 
 (=bind (x)
     (=funcall (=lambda ()
                        (=values ...)))
   ...)

 (=bind (x)
     (run-cont 
 |#
type cont = ('a -> 'b) -> 'b
let returnc x -> (fn k -> k x)
let bindc c f ->
   fn k ->
           c (fn x ->
                 (f x) k)

(defun bindc (c f)
  ;; (cont:(fn:('a -> 'b) -> 'b), 'a -> cont:(fn:('a -> 'b) -> 'b)) -> cont:(fn:('a -> 'b) -> 'b)
  ;; (cont('a,'b), fn('a -> cont('a,'b))) -> cont('a,'b)
  (make-cont (lambda (k)
               (run-cont c (lambda (x)
                             (run-cont (funcall f x) k))))))

#|
(run-cont (returnc 'foo) #'values)
=> foo

(run-cont (bindc (returnc 21) (lambda (x) (returnc (* x 2)))) #'values)
=> 42
 |#

(defmacro letc* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (name c) (car bindings)
        `(bindc ,c (lambda (,name)
                     (letc* ,(cdr bindings) ,@body))))))

#|
(run-cont (letc* ((x (returnc 21)))
              (returnc (* x 2)))
            #'values)
 |#

(defmacro progc (&body body)
  (if (null (cdr body))
      (car body)
      (let ((garg (gensym)))
        `(bindc ,(car body)
                (lambda (,garg)
                  (declare (ignore ,garg))
                  (progc ,@(cdr body)))))))

(defun reset (k)
  (if (cont-p k)
      (run-cont k #'values)
      k))

(defmacro shift (var expr)
  (let ((gk (gensym))
        (garg (gensym)))
    `(make-cont (lambda (,gk)
                  (declare (function ,gk))
                  (flet ((,var (,garg)
                           (funcall ,gk ,garg)))
                    (declare (ignorable (function ,var)))
                    ,expr)))))

#|
(shift k (k 2))
=>
@S(CONT...)
   
(reset (shift k (k 2)))
=>
2  ;; IOW Identity

(reset (letc* ((x (shift k (k 2))))
           (returnc (+ x 3))))
=> 5

(reset (letc* ((x (returnc 100))
                 (y (shift k 'foo)))
           (returnc (+ x y))))
=> FOO

(reset (letc* ((x (shift k (k (k 2)))))
           (returnc (+ x 3))))
=>
8

(reset (letc* ((x (shift k (* 2 (k (k 2))))))
           (returnc (+ x 3))))
=>
16
 |#

(defun fail ()
  (shift k 'no))

(defun choice (n)
  (shift k (loop for i from 1 to n do (k i) finally 'no)))

(defun triple (n s)
  (letc* ((i (choice n))
          (j (choice (- i 1)))
          (k (choice (- j 1))))
    (if (= s (+ i j k))
        (returnc (list i j k))
        (fail))))
#|
(reset (letc* (ijk (triple 9 15)))
           (returnc (print ijk))))
 |#

(defun donep (x) (eq 'done x))

(defun nextp (x) (not (donep x)))

(defun next (n k) (lambda () (values n k)))

(defun walkerc (tree)
  (cond
    ((null tree) (returnc 'done))
    ((atom tree) (shift k (next tree #'k)))
    (t (progc
         (walkerc (car tree))
         (walkerc (cdr tree))))))

(defun same-fringe (t1 t2)
  (labels ((rec (r1 r2)
             (if (nextp r1)
                 (and (nextp r2)
                      (multiple-value-bind (n1 k1) (funcall r1)
                        (multiple-value-bind (n2 k2) (funcall r2)
                          (and (eql n1 n2)
                               (rec (funcall k1 nil)
                                    (funcall k2 nil))))))
                 (donep r2))))
    (rec (reset (walkerc t1))
         (reset (walkerc t2)))))

#|
(same-fringe '((1) (2 (3 (4) 5))) '((1 (2) (3 4) 5)))

(same-fringe '((1) (2 (3 (4) 5))) '((1 (2) (4) 5)))
 |#

;; -------------------------------------------------------------

(defmacro reset (k)
  (let ((g!k (gensym)))
    `(catch 'reset
       (let ((,g!k ,k))
         (throw 'reset
                (if (cont-p ,g!k)
                    (run-cont ,g!k #'values)
                  ,g!k))))
    ))

(defmacro shift (var expr)
  (let ((gk (gensym))
        (garg (gensym)))
    `(make-cont (lambda (,gk)
                  (declare (function ,gk))
                  (flet ((,var (,garg)
                           (funcall ,gk ,garg)))
                    (declare (ignorable (function ,var)))
                    ,expr)))
    ))

(* 2 (reset (+ 1 (shift k (k 5)))))

(* 2
   (catch 'reset
     (funcall (lambda (k)
                (funcall k 5))
              (lambda (x)
                (throw 'reset (+ 1 x))))))

(* 2
   (funcall (lambda (k)
              (funcall k 5))
            (lambda (x)
              (+ 1 x))))

(* 2
   (cps:=bind (x)
       (cps:=values 5)
     (+ 1 x)))

(cps:=bind (x)
    (cps:=values
     (cps:=bind (x)
         (cps:=values 5)
       (+ 1 x)))
  (* 2 x))

(defun o (fn &rest fns)
  (if fns
      (funcall fn (apply #'o fns))
    fn))

;; reset   : cont -> val
;; returnc : val -> cont
;; o : (cont -> cont)* -> cont
;; <- : (cont -> cont) -> (cont -> cont) -> cont

(reset (returnc 6))

(defun returnc (&rest args)
  (lambda (k)
    (apply k args)))

(defun reset (fn)
  (if (functionp fn)
      (funcall fn #'values)
    fn))

(defun <- (c f)
  (lambda (k)
    (funcall c (lambda (x)
                 (funcall (funcall f x) k)))))

(funcall (<- (returnc 21) (lambda (x) (returnc (* x 2)))) #'values)

(funcall (returnc 1 2 3) #'values)

(multiple-value-call #'max (values 1 2 3))

(funcall (lambda (k)
           (lambda (&rest args)
           (multiple-value-call #'max (funcall k)))
         (returnc 1 2 3))

;; fn  : val -> val -- ordinary functions in Lisp
;; hfn : fn -> fn   -- curry, rcurry, compose
;; xfn : val -> fn  -- (returnc val)
;; fnx : fn -> val  -- (cont #'values)

fn: val -> val
hfn: (val -> val) -> (val -> val)
xfn: val -> (val -> val)

fn: unary operators in Lisp
compose: fn:(val -> val) -> fn:(val -> val) -> fn:(val -> val)
curry: fn:((val, val) -> val) -> val -> fn:(val -> val)
returnc : 'a -> ('a -> 'b)
reset: ('a -> 'b) -> 'b
cont: fn:(unit -> val)

(reset 10)
(reset (run-cont (returnc 5) (lambda (x) (returnc 6))))

cont: ('a -> 'b)
returnc: 'a -> cont:(('a -> 'b) -> 'b)
reset  : cont:(('a -> 'b) -> 'b) -> 'b
cont   : ('a -> 'b)
run-cont: cont -> 'a -> 'b

(run-cont (returnc 5) (lambda (x) x))