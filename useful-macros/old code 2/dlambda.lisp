;; ----------------------------------------------------------------------
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

#|
;;
;; This is Doug Hoyte's implementation of DLAMBDA...
;;
(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (let ((,g!tail (cdr ,g!args)))
       (case (car ,g!args)
         ,@(mapcar
            (lambda (d)
              `(,(if (eq t (car d))
                     t
                   (list (car d))
                   )
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                            g!args
                          g!tail)) ))
            ds)))))

(defmacro dcase (args &rest clauses)
  `(apply (dlambda
            ,@clauses)
          ,args))

#+:LISPWORKS
(editor:setup-indent "dcase" 1)
|#

;; ----------------------------------------------------------------------
;; Here is a faster implementation on LispWorks
;;

(defun bad-selector (&rest args)
  (declare (ignore args))
  (error "Invalid selector"))

(defun make-jv-dispatcher (jv &optional default)
  ;; generalized (named) jump vectors
  ;; jv is plist of alternating selector symbols, and functions or closures
  ;; default should be a function or nil, called when no selectors match
  (if default
      (lambda (&rest args)
        (if-let (fn (getf jv (first args) nil))
            (apply fn (rest args))
          (apply default args)))
    ;; else
    (lambda (sel &rest args)
      (apply (getf jv sel 'bad-selector) args))
    ))

;; ----------------------------------------------------------------------

(defmacro! dlambda (&rest ds)
  (let* ((dsels   (mapcar #'first ds))
         has-default
         (dfnames (mapcar (lambda (sel)
                            (if (eq sel t)
                                (setf has-default g!default)
                              (gensym (string sel))))
                          dsels)))
    `(labels
         ,(mapcar (lambda (dfname clause)
                    `(,dfname ,@(rest clause)))
                  dfnames ds)
       (declare (inline ,@dfnames))
       (make-jv-dispatcher (list ,@(mapcan (lambda (dsel dfname)
                                             (unless (eq dsel t)
                                               `(',dsel #',dfname)))
                                           dsels dfnames))
                           ,@(when has-default
                               `(#',g!default)))
       )))

(defmacro dcase (args &rest clauses)
  `(apply (dlambda
            ,@clauses)
          ,args))

#+:LISPWORKS
(editor:setup-indent "DCASE" 1)

;; ----------------------------------------------------------------------

