;; reg-mpy.lisp -- Multiply with Simulated Registers
;;
;; DM/RAL 03/24
;; ---------------------------------------------------------------
(in-package :edec)
;; ----------------------------------------------------------------
;; Exploring modular multiply...
;; Max Fixnum is 60 bits. Use 56-bit arithmetic.

(defun ld56 (x)
  (ldb (byte 56 0) x))

(defun ldlo (x)
  (ldb (byte 28 0) x))

(defun ldhi (x)
  (ldb (byte 28 28) x))

(defun ldxs (x)
  (ash x -56))

#|
(format t "~x" (ldlo -1))
(format t "~x" (ldhi -1))
(format t "~x" (ldxs -1))
|#

(defun fmt28 (x)
  (um:sepi (um:without-abbrev
             (format nil "~7,'0x" x))
           :base 16
           :count 4))

(defun fmt56 (x)
  (um:sepi (um:without-abbrev
             (format nil "~14,'0x" x))
           :base 16
           :count 4))

(defun do-show (wd argnames args)
  (let* ((wd     (or wd 0))
         (maxlen (max wd
                      (reduce #'max
                              (mapcar (lambda (name)
                                        (length (symbol-name name)))
                                      argnames))))
         (fmt-names (mapcar (lambda (name)
                              (let* ((nel (length (symbol-name name)))
                                     (str (make-string maxlen :initial-element #\space)))
                                (replace str (symbol-name name) :start1 (- maxlen nel))
                                str))
                            argnames))
         (sep       (format nil "~A +--+--------+--------+"
                            (make-string maxlen :initial-element #\space))))
    (format t "~%~A" sep)
    (loop for name in fmt-names
          for val in args
          do
            (let* ((xlo (fmt28 (ldlo val)))
                   (xhi (fmt28 (ldhi val)))
                   (xxs (format nil "~2x" (ldxs val))))
              (format t "~%~A |~A|~A|~A|" name xxs xhi xlo)
              (format t "~%~A" sep)
              ))
    ))

(defun fmt60 (x &optional (stream t))
  (let ((sep (format nil "+--+--------+--------+")))
    (format stream "~%~A" sep)
    (format stream "~%|~2x|~A|~A|"
            (ldxs x)
            (fmt28 (ldhi x))
            (fmt28 (ldlo x)))
    (format stream "~%~A" sep)))

#|
(fmt60 -1)
 |#
(defmacro show (wd &rest args)
  `(do-show ,wd '(,@args) (list ,@args)))

(defun mul56 (a b &optional (cy 0))
  (let* ((a<  (ldlo a))
         (a>  (ldhi a))
         (b<  (ldlo b))
         (b>  (ldhi b))
         (axs (ldxs a))
         (bxs (ldxs b))

         (i0  (* a< b<))
         (i1  (+ (* a< b>)
                 (* a> b<)))
         (i2  (* a> b>))

         (xs<  (+ (* a< bxs)
                  (* b< axs)))
         (xs>  (ash (+ (* a> bxs)
                       (* b> axs))
                    28))
         (xs   (ash (* axs bxs) 56))

         (p1 i1)
         (p0 (+ i0 cy
                (ash (ldlo p1) 28)))
         (p2 (+ i2
                (ldxs p0)
                (ldhi p1)
                (ash (ldxs p1) 28)
                xs<
                xs>
                xs))
         (ans  (ldb (byte 56 0) p0))
         (cy2  p2))
    (show 4 a b cy i0 i1 i2 p0 p1 p2 xs< xs> xs ans cy2)
    (values ans cy2)
    ))
#|

(let ((x (1- (ash 1 60))))
  (um:sepi (let ((*print-base* 16))
             (prin1-to-string (* x x)))
           :count 4
           :base 16))

(let ((x (1- (ash 1 56))))
  (mul56 x x))

(let ((x (1- (ash 1 59))))
  (mul56 x x))

(let ((x (1- (ash 1 60))))
  (mul56 x x))

(let ((x -1))
  (mul56 x x))

(let* ((base  (ash 1 48))
       (a     (random base))
       (b     (random base))
       (cy    (random (1- base))))
  (mul48 a b cy))
         
|#

(defun red521 (x)
  (let* ((lo  (ldb (byte 521 0) x))
         (hi  (ash x -521))
         (kred 1))
    (+ lo (* kred hi))))

(defvar *base521*  (1- (ash 1 521)))

(defun chk521 (x)
  (let ((diff (- x *base521*)))
    (if (minusp diff)
        x
      diff)))

(defun mpy521 (a b)
  (chk521 (red521 (red521 (* a b)))))

(mpy521 -1 1)