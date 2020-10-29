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

(in-package :useful-macros)

;; --------------------------------------------

(defun separate-declares-and-documentation (body-list &optional decls doc)
  "Used to help define macros that define declarations that can have a
documentation string and any number of declares. Those items must be placed
in the correct spot after the opening of the declaring form (e.g., DEFUN, DEFMETHOD).
And so these elements must be stripped off the incoming macro argument representing
the &body of the defining form. See the example of DEFINE-MONITOR which follows."
  (if (null body-list)

      (if decls
          (values doc (nreverse decls) nil)
        
        (if doc
            (values nil nil (list doc))

          (values nil nil nil)))
    
    (if (and (null doc)
             (stringp (first body-list)))
        (separate-declares-and-documentation (rest body-list) decls (first body-list))
      
      (if (and (consp (first body-list))
               (eq 'declare (first (first body-list))))
          (separate-declares-and-documentation (rest body-list)
                                               (push (first body-list) decls)
                                               doc)
        
        (values doc (nreverse decls) body-list))
      )))

;; ----------------------------------------------
;; A MONITOR is a group of functions whose entry is controlled
;; by a lock, and which may affect globally held values that are
;; shared between processes.
;;
;; Guarded functions can only be entered by one process at a time.

(defmacro with-monitor (name bindings clauses &key pre-lock)
  (labels ((gen-body (def-hdr body)
             (um:bind*
                 ((:values (doc decls body-list) (separate-declares-and-documentation body)))
               
               `(,@def-hdr
                 ,@(if doc `(,doc))
                 ,@decls
                 ,@(if pre-lock `(,pre-lock))
                 (#+:LISPWORKS mp:with-lock
                  #+:ALLEGRO   mp:with-process-lock
		  #+:CLOZURE   mp:with-lock-grabbed
		  #+:SBCL      sb-thread:with-recursive-lock
                    (,name)
                    ,@body-list))
                 )))
    
    `(let ,bindings
       ,@(mapcar (lambda (clause)
                   (match clause
                     
                     ((deftype name meth-comb args &rest body) :when (and meth-comb
                                                                          (symbolp meth-comb))
                      (gen-body `(,deftype ,name ,meth-comb ,args) body))
                     
                     ((deftype name args &rest body)
                      (gen-body `(,deftype ,name ,args) body))
                     ))
                 clauses))
    ))

(defmacro define-monitor (name bindings clauses &key pre-lock)
  `(#+:LISPWORKS dspec:def
		 #+:LISPWORKS (define-monitor ,name)
		 #+:ALLEGRO progn
		 #+:CLOZURE progn
		 #+:SBCL    progn
     (progn
       (defvar ,name
         #+:LISPWORKS (mp:make-lock)
         #+:ALLEGRO   (mp:make-process-lock)
	 #+:CLOZURE   (mp:make-lock)
	 #+:SBCL      (sb-thread:make-mutex))
       (with-monitor ,name ,bindings ,clauses :pre-lock ,pre-lock))))


(defmacro let-monitor (bindings clauses &key pre-lock)
  (let ((glock (gensym)))
    `(let ((,glock #+:LISPWORKS (mp:make-lock)
                   #+:ALLEGRO   (mp:make-process-lock)
		   #+:CLOZURE   (mp:make-lock)
		   #+:SBCL      (sb-thread:make-mutex)
		   ))
       (with-monitor ,glock ,bindings ,clauses :pre-lock ,pre-lock))
    ))

;; ----------------------------------------------
#|
(defclass lock-mixin ()
  ((lock-mixin-lock :reader lock-mixin-lock
		    :initform
		    #+:LISWORKS (mp:make-lock)
		    #+:ALLEGRO  (mp:make-process-lock)
		    #+:CLOZURE  (mp:make-lock)
		    #+:SBCL     (sb-thread:make-mutex)
		    )))

(defmacro let-locking (clauses &key pre-lock)
  (labels ((gen-body (def-hdr lockable-arg body)
             (multiple-value-bind (doc decls body-list)
                 (separate-declares-and-documentation body)
               `(,@def-hdr
                 ,@(if doc `(,doc))
                 ,@decls
                 ,@(if pre-lock `(,pre-lock))
                 (#+:LISPWORKS mp:with-lock
                  #+:ALLEGRO   mp:with-process-lock
		  #+:CLOZURE   mp:with-lock-grabbed
		  #+:SBCL      sb-thread:with-recursive-lock
                  ((lock-mixin-lock ,(if (consp lockable-arg) ;; as from a qualified method arg
                                         (first lockable-arg)
                                       lockable-arg)))
                  ,@body-list))
               )))
    
    `(progn
       ,@(mapcar (lambda (clause)
                   (match clause
                     
                     ((deftype name meth-comb args . body) :when (keywordp meth-comb)
                      (gen-body `(,deftype ,name ,meth-comb ,args) (first args) body))
                     
                     ((deftype name args . body)
                      (gen-body `(,deftype ,name ,args) (first args) body))
                     ))
                 clauses))
    ))
|#
;; ----------------------------------------------

#+:LISPWORKS
(progn
  (editor:setup-indent "define-monitor" 2 2 4)
  (editor:setup-indent "let-monitor" 2 2 4))

