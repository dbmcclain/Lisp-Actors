;; protocols.lisp -- ML-type Protocol Signatures in Lisp
;; ------------------------------------------------------------------
;;
;; Copyright (C) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/RAL  08/08
;; ------------------------------------------------------------------
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

;; -------------------------------------------
(in-package #:protocol)
;; -------------------------------------------

(defstruct protocol
  name parametric-types aux-types signatures)

(defun amper-kw-p (sym)
  ;; lambda list keywords for defgeneric
  (member sym lambda-list-keywords))

(um:defun* clip-ampers ((name args . _))
  ;; return a copy of the method signature, sans &-keywords
  (declare (ignore _))
  `(,name ,(um:if-let (pos (position-if 'amper-kw-p args))
               (um:take pos args)
             args)))

(defun map-to-argname (arg)
  ;; map a method signature argument type to a gensym symbol
  (if (consp arg)
      (gensym (um:mkstr :arg))
    (gensym (um:mkstr arg))))

(defun ensure-used (sig types)
  ;; see if any signature methods fail to mention our parametric types
  (destructuring-bind (_ args . __) sig
    (declare (ignore _ __))
    (if (some (um:rcurry 'member types) args)
        sig
      (error "Irrelevant protocol signature: ~A" sig))
    ))

;; -------------------------------------------------------------------

(defmacro define-protocol (name parametric-types &body body)
  (let ((aux-types  (rest (assoc :type body)))
        (signatures (rest (assoc :signature body))))
    `(progn
       (eval-when (:load-toplevel :execute)
         (setf (get ',name 'protocol)
               (make-protocol
                :name             ',name
                :parametric-types ',parametric-types
                :aux-types        ',aux-types
                :signatures       ',(mapcar (um:compose
                                             (um:rcurry 'ensure-used parametric-types)
                                             'clip-ampers)
                                            signatures)
                )))
       ,@(mapcar (um:lambda* ((name args . rest))
                   `(defgeneric ,name
                        ,(mapcar 'map-to-argname args)
                      ,@(um:if-let (doc (second (member :documentation rest)))
                            `((:documentation ,doc)))))
                 signatures))))

;; -------------------------------------------------------------------------

(defmacro implements-protocol (protocol-name parametric-types)
  `(eval-when (:load-toplevel :execute)
     (check-protocol ',protocol-name ',parametric-types)))

;; -------------------------------------------------------------------

(defun check-protocol (protocol-name parametric-types)
  (format t "~&Checking protocol implementation: ~A~A~%"
	  protocol-name parametric-types)
  (let* ((protocol   (get protocol-name 'protocol))
         (signatures (protocol-signatures protocol)))
    (dolist (sig signatures)
      (verify-signature sig protocol parametric-types))
    ))

;; -------------------------------------------------------------------------

(defun verify-signature (sig protocol param-types)
  (let* ((fndef   (fdefinition (first sig)))
         (methods (clos:generic-function-methods fndef)))
    (verify-method-signature sig protocol methods param-types)))

(defun get-actual-arg (arg aux-types)
  ;; look up the arg type in the aux types, and, if present,
  ;; substitute its definition for the argument and recurse on the new type.
  (um:if-let (actual (second (assoc arg aux-types)))
      (get-actual-arg actual aux-types)

    ;; else -- an OR type?
    (if (and (consp arg)
             (eq 'or (first arg)))
        `(or ,@(mapcar (um:rcurry 'get-actual-arg aux-types) (rest arg)))
      
      ;; else -- no further equivalences, just return itself
      arg)))

(defun substitute-param-types (arg abstract actual)
  ;; substitute the concrete parametric types of the implementation
  ;; for the abstract parametric types used by the protocol spec.
  (if (and (consp arg)
           (eql 'or (first arg)))
      `(or ,@(mapcar (um:rcurry 'substitute-param-types abstract actual)
                     (rest arg)))
    (um:if-let (pos (position arg abstract))
        (nth pos actual)
      ;; else
      arg)))

(defun get-actual-args (args aux-types abstract-param-types actual-param-types)
  ;; perform type abbreviation substitution followed by actual parametric types
  ;; for the protocol abstract parametric types.
  (mapcar #'(lambda (arg)
              (let ((actual (get-actual-arg arg aux-types)))
                (substitute-param-types actual abstract-param-types
					actual-param-types)))
          args))

(defun get-class (arg-name)
  (if arg-name
      (find-class arg-name))) ;; because NIL is not the name of a class

(defun get-arg-classes (args)
  ;; turn the arg names into classes for use in comparing against method specializers.
  (mapcar #'(lambda (arg)
              (if (consp arg)
                  (if (eql 'or (first arg))
                      `(or ,@(get-arg-classes (rest arg)))
                    arg)
                ;; else not consp
                (get-class arg)))
          args))

(defun subclass-p (class super)
  (cond ((consp class) ;; an OR class?
         (ecase (first class)
           (eql  (subclass-p (second class) super))
           (or   (some (um:rcurry 'subclass-p super) (rest class)))))

        ((consp super) ;; can only be an EQL spec
         (assert (eql 'eql (first super)))
         (eql class (second super)))
        
        #+:ALLEGRO
        ((eq super (find-class t)) t)
        
        ((and (typep class 'class)
              (typep super 'class))
         (subtypep class super))
        ))

#|
(defun get-class-name (class) ;; because nil cannot be used with class-name
  (and class
       (class-name class)))

(defun show-subclass-p-args (class super)
  (format t "~&subclass-p ~A ~A~%"
          (if (consp class)
              `(,(first class) ,@(mapcar 'get-class-name (rest class)))
            (get-class-name class))
          (if (consp super)
              `(,(first super) ,@(mapcar 'get-class-name (rest super)))
            (get-class-name super))))
  
(lw:defadvice (subclass-p :show-subclass-p-args :around)
  (class super)
  (show-subclass-p-args class super)
  (let ((ans (lw:call-next-advice class super)))
    (format t "~& ==> ~A~%" ans)
    ans))
|#

(defun verify-method-signature (sig protocol methods param-types)
  (let* ((aux-types       (protocol-aux-types protocol))
         (abstract-params (protocol-parametric-types protocol))
         (args            (get-actual-args
                           (second sig) ;; the signature args
                           aux-types    ;; the aux types of the protocol
                           abstract-params ;; the abstract parametric types
                           param-types)) ;; the actual concrete parametric types
         (arg-classes   (get-arg-classes args)))
    (um:perform iter ((methods methods))
      (if methods
          (let ((specializers (clos:method-specializers (first methods))))
            ;; (format t "~&Checking args: ~A~%" args)
            (if (every 'subclass-p arg-classes specializers)
                (return-from verify-method-signature)

              ;; else -- try next method
              (iter (rest methods))))

        ;; else -- no method exists for prototype
        (error "Method does not implement protocol signature: ~A" sig)))
    ))

;; ----------------------------------------------------------------------

#+:LISPWORKS
(editor:setup-indent "define-protocol" 2 2)
#+:LISPWORKS
(editor:setup-indent "implements-protocol" 2 2)

