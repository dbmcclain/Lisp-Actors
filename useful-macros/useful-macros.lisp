;; useful_macros.lisp -- A collection of really handy macros
;;
;; DM/HMSC  11/97
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

;; -------------------------------------------------------

(defmacro letrec (bindings &body body)
  `(let ,(mapcar 'first bindings)
     ,@(mapcar #`(setf ,@a1) bindings)
     ,@body))

;; -------------------------------------------------------

(define-condition cant-happen (error)
  ((loc  :accessor cant-happen-loc
         :initarg :loc))
  (:report  (lambda (cx stream)
              (format stream "Can't Happen: ~A" (cant-happen-loc cx)))))

(defun cant-happen (where)
  (error (make-condition 'cant-happen
                         :loc where)))

;; -----------------------------------------------------------

(defun wholepart (x)
  (truncate x))

(defun fracpart (x)
  (second (multiple-value-list (truncate x))))

;; ------------------------------------

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro foreach (fn &rest seqs)
  `(map nil ,fn ,@seqs))

(defmacro if-let ((var val) t-clause &optional f-clause)
  `(let ((,var ,val))
     (if ,var
         ,t-clause
       ,f-clause)))

(defmacro when-let ((var val) &body body)
  `(let ((,var ,val))
     (when ,var
       ,@body)))

;; ----------------------------------------------------------------------

(declaim (inline last1 single append1 conc1 mklist))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defun single (arg)
  (and (consp arg)
       (null (cdr (the cons arg)))))

(defun last1 (lst)
  (car (the cons (last lst))))

;; ----------------------------------------

(defmacro with (bindings &body body)
  `(let* ,bindings ,@body))

(defmacro letp (bindings &body body)
  `(let ,bindings ,@body))

#+:LISPWORKS
(editor:setup-indent "with" 1)
#+:LISPWORKS
(editor:setup-indent "letp" 1)

#|
(eval-always
  (defun %i-nlet-tail (letter name letargs decls+body)
    (let ((gs  (gensyms letargs))
          (gb  (gensym))
          (gn  (gensym)))
      (multiple-value-bind (body decls)
          (alexandria:parse-body decls+body)
        `(macrolet
             ((,name ,gs
                `(progn
                   (psetq
                    ,@(mapcan 'list
                              ',(mapcar #'car letargs)
                              (list ,@gs)))
                   (go ,',gn))))
           (block ,gb
           (,letter ,letargs
               ,@decls
               (tagbody
                ,gn
                (return-from
                    ,gb (progn ,@body))))))
        ))))

(defmacro nlet-tail (name letargs &body decls+body)
  (%i-nlet-tail 'let name letargs decls+body))

(defmacro nlet-tail* (name letargs &body decls+body)
  (%i-nlet-tail 'let* name letargs decls+body))
|#

#|
(nlet-tail* iter ((a a-init)
                 (b b-init))
  (declare (fixnum a b))
  (clause1)
  (iter xx yy))

 |#

;; ----------------------------------------------------------------------

;; WITH-TAIL-PURE-CODE -- Compiled code using this macro can run recursively
;; all day long without ever blowing the stack.
;; Tail calls are effectively made into direct jumps.
;;
;; N.B. DM/RAL 02/07 -- tests with LWM 5.01 indicate that this may be
;; unnecessary. Code must be compiled for tail optimization. Interpreted code
;; has problems no matter what.
(defmacro with-tail-pure-code (&body body)
  `(locally
     (declare (optimize (debug 1) (safety 1)))
     ,@body))

;; ----------------------------------------------------------------------
;; NLET-CPS -- conversion of iteration to pure tail recrsion, even when
;; non-tail calling positions, by way of CPS

#+:LISPWORKS
(defmacro nlet-cps (name args &body body)
  (lw:with-unique-names (g!name g!kont g!args g!expr g!body)
    `(labels ((,g!name ,(cons g!kont (mapcar 'first args))
                (macrolet ((=values (&rest ,g!args)
                             `(funcall ,',g!kont ,@,g!args))
                           (=bind (,g!args ,g!expr &body ,g!body)
                             `(let ((,',g!kont (lambda ,,g!args ,@,g!body)))
                                ,,g!expr))
                           (,name (&rest ,g!args)
                             `(,',g!name ,',g!kont ,@,g!args)))
                  ,@body)))
       (,g!name 'values ,@(mapcar 'second args)))
    ))

;; ----------------------------------------------------------------------
#|
(nlet-cps iter ((a lst)
                (b nil))
  (if (endp a)
      (=values b)
    (iter (cdr a)
          (iter (caar a nil))
          (iter (cdar a) nil))
    ))

(nlet-cps iter ((a lst)
                (b nil))
  (if (endp a)
      (=values b)
    (=bind (left)
        (iter (caar a) nil)
      (=bind (right)
          (iter (cdar a) nil)
        (iter (cdr a)
              (list* left right b))
        ))))

(labels ((iter (k a b)
           (if (endp a)
               b
             (cl-cont:with-call/cc
               (iter k (cdr a)
                     (list* (cl-cont:let/cc k
                              (iter k (caar a nil)))
                            (cl-cont:let/cc k
                              (iter k (cdar a) nil))
                            b)))
             )))
  (iter #'values lst nil))
  

 |#
;; ----------------------------------------------------------------------
#|
(defun bad-selector (&rest args)
  (declare (ignore args))
  (error "Invalid selector"))

(defun make-jv-dispatcher (jv &optional default)
  ;; generalized (named) jump vectors
  ;; jv is plist of alternating selector symbols, and functions or closures
  ;; default should be a function of nil, called when no selectors match
  (if default
      (lambda (&rest args)
        (if-let (fn (getf jv (first args) nil))
            (apply fn (rest args))
          (apply default args)))
    ;; else
    (lambda (sel &rest args)
      (apply (getf jv sel 'bad-selector) args))
    ))

(defun jv-dispatch (jv default args)
  (if-let (fn (getf jv (first args) nil))
      (apply fn (rest args))
    (apply default args)))

(defun jv-dispatch-no-default (jv sel args)
  (apply (getf jv sel 'bad-selector) args))

(defmacro! jv-dispatcher (jv &optional default)
  `(let ((,g!jv ,jv))
     ,(if default
          `(lambda (&rest ,g!args)
             (jv-dispatch ,g!jv ,default ,g!args))
        `(lambda (,g!sel &rest ,g!args)
           (jv-dispatch-no-default ,g!jv ,g!sel ,g!args)))))
|#

;; ----------------------------------------------------------------------
#|
(defmacro! dlambda (&rest ds)
  (let* ((dsels   (mapcar 'first ds))
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
       (jv-dispatcher (list ,@(mapcan (lambda (dsel dfname)
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
(editor:setup-indent "DCASE" 1 nil nil 'flet)
#+:LISPWORKS
(editor:setup-indent "DLAMBDA" 0 nil nil 'flet)
|#
;; ----------------------------------------------------------------------
#|
(defmacro! make-state-machine ((initial-state inp-var
                                                 &key on-reset)
                                  &rest state-bindings)
  `(let ((,g!state ,initial-state)
         ,g!state-stack)
     (macrolet ((push-state (state)
                  `(push ,state ,',g!state-stack))
                (pop-state ()
                  `(pop ,',g!state-stack)))
       (dlambda
        (:reset ()
         (setf ,g!state       ,initial-state
               ,g!state-stack nil)
         ,on-reset)

        (t (,inp-var)
           (cond ,@(mapcar
                    (lambda (binding)
                      (destructuring-bind (from-state &rest sub-clauses)
                          binding
                        (let (t-seen)
                          `((eq ,g!state ,from-state)
                            (cond ,@(mapcar
                                     (lambda (subclause)
                                       (destructuring-bind (test to-state &rest clause)
                                           subclause
                                         (setf t-seen (or t-seen
                                                          (eq test 't)
                                                          (eq test 'otherwise)))
                                         `(,test
                                           (setf ,g!state ,to-state)
                                           ,@clause)))
                                     sub-clauses)
                                  ,@(unless t-seen
                                      `((t (error "State machine error in state: ~A"
                                                  ,g!state)))) ))
                          )))
                    state-bindings)
                 (t (error "State machine error")) ))) )))

#+:LISPWORKS
(editor:setup-indent "make-state-machine" 2 4)

;; ----------------------------------------------------------------------

(defmacro! run-state-machine ((initial-state (inp-var step-expr)) &rest state-bindings)
  `(let ((,g!machine (make-state-machine (,initial-state ,inp-var) ,@state-bindings)))
     (block ,g!block
       (tagbody
        ,g!top
        (multiple-value-bind (,g!ans ,g!done)
            (funcall ,g!machine ,step-expr)
          (if ,g!done
              (return-from ,g!block ,g!ans)
            (go ,g!top))
          )))))

#+:LISPWORKS
(editor:setup-indent "run-state-machine" 2 4)
|#
;; ----------------------------------------------------------------------

#+:LISPWORKS
(defun copy-with (s &rest bindings)
  (let ((new-s      (copy-structure s))
        (slot-names (structure:structure-class-slot-names (class-of s))))
    (dolist (name slot-names new-s)
      (let ((val (getf bindings (kwsymbol name) new-s)))
        (unless (eq val new-s)
          (setf (slot-value new-s name) val))))
    ))
          

#|
(defstruct thing
          a b c)

(let ((x (make-thing
          :a 1
          :b 2
          :c 3)))
  (inspect (copy-with x :b 15)))
|#
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------

(defmacro dis (args &rest body)
  `(disassemble
     (compile nil
       (lambda ,(mapcar (lambda (a)
                          (if (consp a)
                            (cadr a)
                            a))
                        args)
         (declare
           ,@(mapcar
               #`(type ,(car a1) ,(cadr a1))
               (remove-if-not 'consp args)))
         ,@body))))

;; ----------------------------------------------------------------------

(defmacro! pointer-& (obj)
  `(lambda (&optional (,g!set ',g!temp))
     (if (eq ,g!set ',g!temp)
       ,obj
       (setf ,obj ,g!set))))

(defun pointer-* (addr)
  (funcall addr))

(defsetf pointer-* (addr) (val)
  `(funcall ,addr ,val))

(defsetf pointer-& (addr) (val)
  `(setf (pointer-* ,addr) ,val))

;; ----------------------------------------------------------------------
;; Ref Cells - also defines MAKE-REF, COPY-REF, REF-P, REF-VAL, and (SET REF-VAL)

(defstruct ref
  val)

;; ----------------------------------------------------------------------

(defmacro! nif (o!expr pos zero neg)
  `(cond ((plusp ,g!expr) ,pos)
         ((zerop ,g!expr) ,zero)
         (t ,neg)))

#|
(nif x '+ 0 '-)
(defmacro! square (o!x)
  `(* ,g!x ,g!x))
(square expr)
|#

;; ----------------------------------------------------
;; Special macro versions of functional composition operators
;; All of these expect a parenthesized list of named dummy args
;; immediately following the EXPANDED-xxx macro name, and prior to
;; the actual arguments
;;
(defmacro expanded-combine ((&rest args) op f1 f2)
  `(lambda ,args
     (funcall ,op (funcall ,f1 ,@args) (funcall ,f2 ,@args))))

(defmacro expanded-compose ((&rest args) &rest fns)
  (cond ((null fns)   'identity)
        ((single fns) (car fns))
        ((single (rest fns))
         `(lambda ,args
            (funcall ,(first fns) (funcall ,(second fns) ,@args))
            ))
        (t  (let ((fn1 (last1 fns))
                  (fns (butlast fns)))
              `(lambda ,args
                 (foldr 'funcall (list ,@fns) (funcall ,fn1 ,@args)))
              ))
        ))

;; so far, only PLOTTER makes use of EXPANDED-CURRY
;; none of the other EXPANDED- variants are in use
(defmacro expanded-curry ((&rest suf-args) f &rest pref-args)
  `(lambda ,suf-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defmacro expanded-rcurry ((&rest pref-args) f &rest suf-args)
  `(lambda ,pref-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defmacro curried-lambda ((&rest args) &body body)
  (if (rest args)
      `(lambda (,(first args))
         (curried-lambda ,(rest args)
                         ,@body))
    `(lambda ,args
       ,@body)))

#|
;; try them out...
(expanded-compose (tree) 'first 'second 'fifth)
(expanded-compose (tree) 'first 'second)
(expanded-combine (x) '+ 'second 'third)
(expanded-rcurry (seq) 'subseq start end)
(expanded-curry  (val) '* 3)

(curried-lambda (a b c) (list a b c))
|#
;; -------------------------------------------------------------

(defmacro! allf (o!val &rest places)
  `(setf ,@(mapcan (rcurry 'list #|place|# g!val) places)))

(defmacro nilf (&rest places)
  `(allf nil ,@places))

#|
(nilf this that thother)
|#

(defmacro tf (&rest places)
  `(allf t ,@places))

;; ------------------------------------
(define-modify-macro addf (&rest args)
  +)

(define-modify-macro subf (&rest args)
  -)

(define-modify-macro mulf (&rest args)
  *)

(define-modify-macro divf (&rest args)
  /)

(define-modify-macro ashf (&rest args)
  ash)

(define-modify-macro appendf (&rest args)
  append)

(define-modify-macro nconcf (&rest args)
  nconc)

;; ------------------------------------

(defun do-expansionf (place env exp-fn)
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
       (setf ,(car new) ,(funcall exp-fn (car new)))
       ,setter)
    ))
  
(defmacro deletef (place item &rest args &environment env)
  (do-expansionf place env
                 (lambda (src)
                   `(delete ,item ,src ,@args))))

(defmacro deletef-if (place pred &rest args &environment env)
  (do-expansionf place env
                 (lambda (src)
                   `(delete-if ,pred ,src ,@args))))

(defmacro removef (place item &rest args &environment env)
  (do-expansionf place env
                 (lambda (src)
                   `(remove ,item ,src ,@args))))

(defmacro removef-if (place pred &rest args &environment env)
  (do-expansionf place env
                 (lambda (src)
                   `(remove-if ,pred ,src ,@args))))

(defmacro aconsf (place key val &environment env)
  (do-expansionf place env
                 (lambda (src)
                   `(acons ,key ,val ,src))))

(defmacro conc1f (place obj &environment env)
  (do-expansionf place env
                 (lambda (src)
                   `(nconc ,src (list ,obj)))))

;; ------------------------------------

(defmacro or-setf (place &body body)
  `(or ,place
       (setf ,place
             (progn
               ,@body))))

(defmacro mv-or-setf (place &body body)
  `(values-list (or-setf ,place
                  (multiple-value-list
                   (progn
                     ,@body)))
                ))

(defmacro! ensure-assoc ((key place &rest args) &body body)
  `(let ((,g!key ,key))
     (or (assoc ,g!key ,place ,@args)
         (car (aconsf ,place ,g!key (progn ,@body))) )))

(defmacro! make-setter (place)
  `(lambda (,g!value)
     (setf ,place ,g!value)))

;; ------------------------------------------------------------
;; if-let*
;;
;;  (if-let* ((s1 e1)
;;            (s2 e2)
;;             ...  )
;;      (... true clause ...)
;;    (... false clause ....))
;;
;; => true-clause is only executed if *all* of the expressions in the let clauses are true
;;    false-clause is executed if *any* of the expressions is false
;;    bindings of sequential symbols in the let clauses are like let*, so that later clauses
;;    may refer to symbols representing the tests of eariler clauses.
;;    let clauses are evaluated in the order stated, and short-circuit evaluation is performed
;;    so that a false let clause aborts the evaluation of all later let clauses.

(defmacro when-let* (let-clauses &body body)
  (if (endp let-clauses)
    `(progn
       ,@body)
    ;; else
    `(when-let ,(first let-clauses)
       (when-let* ,(rest let-clauses) ,@body))
    ))

#+:LISPWORKS
(editor:setup-indent "when-let*" 1)

#|
(when-let* ((a ea) (b eb)) tc)
|#

(defmacro! if-let* (let-clauses true-clause &optional false-clause)
  (if false-clause
      (labels ((generate-if-let* (lets)
                 (if lets
                     `(if-let ,(first lets)
                          ,(generate-if-let* (rest lets))
                        (go ,g!false))
                   `(return-from ,g!block ,true-clause)) ))
        `(block ,g!block
           (tagbody
            ,(generate-if-let* let-clauses)
            ,g!false
            (return-from ,g!block ,false-clause))))
    `(when-let* ,let-clauses ,true-clause) ))

#+:LISPWORKS
(editor:setup-indent "if-let*" 1 2 4)

#|
(if-let* ((a ea)
          (b eb))
  tc
  fc)

(if-let* ((a ea)
          (b eb))
  tc)
|#

#|
;; -------------------------------------------------------
;; Define our own collector objects that
;; perform rapid nconc list accumulation
;;
;; DM/RAL 02/07 -- added a Lock for MP safety

(defclass <collector> ()
  ((hd   :accessor collector-hd)
   (tl   :accessor collector-tl)
   (lock :accessor collector-lock)))

(defmacro with-locked-collector ((c &rest lock-args) &body body)
  `(mp:with-lock ((collector-lock ,c) ,@lock-args)
     ,@body))

(defun collector-discard-contents (c)
  (with-locked-collector (c)
    (let ((v (list nil)))
      (setf (collector-hd c) v
            (collector-tl c) v)
      )))

(defmethod initialize-instance ((c <collector>) &key &allow-other-keys)
  (setf (collector-lock c) (mp:make-lock :name "Collector Lock"))
  (collector-discard-contents c))

(defun collector-contents (c &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  ;;
  ;; If user doesn't want to discard contents,
  ;; then hand him a copy of the contents as they exist at this moment.
  (with-locked-collector (c)
    (let ((lst (cdr (the cons (collector-hd c)))))
      (if discard
          (progn
            (collector-discard-contents c)
            lst)
        (copy-seq lst))
      )))
    
(defun collector-ncontents (c)
  (with-locked-collector (c)
    (length (cdr (the cons (collector-hd c))))
    ))

(defun collector-empty-p (c)
  (zerop (collector-ncontents c)))

(defun collector-append-item (c item)
  (with-locked-collector (c)
    (setf (collector-tl c)
          (cdr (the cons (rplacd (the cons (collector-tl c)) (list item))))
          )))

(defun collector-push-item (c item)
  (with-locked-collector (c)
    (setf (collector-hd c)
          (cons nil (the cons (rplaca (the cons (collector-hd c)) item)))
          )))

(defun collector-pop (c)
  (with-locked-collector (c)
    (let* ((lst (collector-contents c))
           (v   (car lst)))
      (unless (endp lst)
        (setf (collector-hd c) lst))
      v)))

(defun make-collector ()
  (make-instance '<collector>))
|#

;; ---------------------------------------------------------------------
#+:lispworks
(defun constituent (c)
  (and (graphic-char-p c)
       (not (lispworks:whitespace-char-p c))))

#-:lispworks
(progn
  (defvar *whitespace-chars*
      (list #\space #\tab #\newline #\return #\backspace #\Page))
  
  (defun whitespace-char-p (c)
    (member c *whitespace-chars*))
  
  (defun constituent (c)
    (and (graphic-char-p c)
         (not (whitespace-char-p c)))))

#|
;; show whitespace chars
(dolist (item
         (loop for ix from 0 below 256
               for c = (code-char ix)
               when (lw:whitespace-char-p c)
               collect (list ix c)))
  (format t "~%0x~2,'0x ~S" (first item) (second item)))

;; show graphics chars
(dolist (item
         (loop for ix from 0 below 256
               for c = (code-char ix)
               when (graphic-char-p c)
               collect (list ix c)))
  (format t "~%0x~2,'0x ~S" (first item) (second item)))
|#

(defun tokens (str &key (test 'constituent) test-not (start 0) end key)
  (let ((test (if test-not
                  (complement test-not)
                test)))
    (loop for p1 = (position-if test str :start start :end end :key key)
          while p1
          do (setf start (position-if-not test str :start p1 :end end :key key))
          collect (subseq str p1 (or start end))
          while start
          )))

(defun tokens-if (test str &rest args)
  (apply 'tokens str :test test args))

(defun tokens-if-not (test-not str &rest args)
  (apply 'tokens str :test-not test-not args))

(defun split-string (str &key delims (start 0) end key)
  (if delims
      (tokens-if-not (rcurry 'find #|c|# delims) str
                     :start start :end end :key key)
    (tokens str :start start :end end :key key)
    ))


(defun paste-strings (delim &rest args)
  (with-output-to-string (s)
    (when args
      (princ (car args) s)
      (dolist (arg (cdr args))
        (princ delim s)
        (princ arg s)
        ))
    ))
      
;; ----------------------------------------------------------------

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

;; ----------------------------------------------------------------
;; Paul Graham's FN

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar (lambda (f)
                        `(,(rbuild f) ,g))
                      fns)))
    ))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                     g)))
          (rec fns)))
    ))

(defun rbuild (expr)
  (cond ((or (atom expr)
             (eq (car expr) 'lambda))
         expr)

        ((eq (car expr) 'compose)
         (build-compose (cdr expr)))

        (t (build-call (car expr) (cdr expr)))
        ))

(defmacro fnc (expr)
  `',(rbuild expr))

#|
(fn (compose list 1+ truncate))  
 |#

;; ----------------------------------------------------------------

(defmacro if* (test tclause &rest fclauses)
  `(if ,test ,tclause
       ,(if (< (length fclauses) 2)
            (first fclauses)
          `(if* ,(first fclauses)
                ,(second fclauses)
                ,@(cddr fclauses))
          )))

;; Graham's alambda
(defmacro! alambda (parms &body body)
  `(labels ((,a!self ,parms ,@body))
     #',a!self))

#|
(alambda ((a b c) (doit a (+ b c))))
 |#

(defmacro! aif (test tclause &optional fclause)
  `(let ((,a!it ,test))
     (if ,a!it ,tclause ,fclause)))

(defmacro! aif* (test tclause &rest fclauses)
  `(let ((,a!it ,test))
     (if ,a!it ,tclause
       ,(if (< (length fclauses) 2)
            (first fclauses)
          `(aif* ,(first fclauses)
                 ,(second fclauses)
                 ,@(cddr fclauses))
          ))))

(defmacro! awhen (test &rest clauses)
  `(let ((,a!it ,test))
     (when ,a!it ,@clauses)))

;; ------------------------------------------------------------------
(defmethod longer ((x list) (y list))
  (do ((lx x (cdr lx))
       (ly y (cdr ly)))
      ((or (null lx)
           (null ly)) lx)))

(defmethod longer (x y)
  (> (length x) (length y)))

(defun filter (fn lst)
  ;; collect all non-null resutls of applying fn
  ;; to elements of lst
  (mapcan (lambda (x)
            (when-let (val (funcall fn x))
              (list val)))
          lst))

(defun partition (pred lst)
  (let (haves have-nots)
    (um:foreach (lambda (item)
                  (if (funcall pred item)
                      (push item haves)
                    (push item have-nots)))
                lst)
    (values (nreverse haves)
            (nreverse have-nots))))

(defmethod group ((lst list) n &key from-end)
  (check-type n (integer 1))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (when lst
      (cond (from-end
             (let* ((nel   (length lst))
                    (start (mod nel n)))
               (cond
                ((< nel n)     lst)
                ((zerop start) (rec lst nil))
                (t 
                 (cons (subseq lst 0 start)
                       (rec (nthcdr start lst) nil)))
                )))

            (t
             (rec lst nil))
            ))))
      
(defmethod group ((seq sequence) n &key from-end)
  (check-type n (integer 1))
  (let ((stop (length seq)))
    (labels ((rec (start end acc)
               (if (>= end stop)
                   (nreverse (cons (subseq seq start stop) acc))
                 (rec end (+ end n) (cons (subseq seq start end) acc))
                 )))
    (when (plusp stop)
      (cond (from-end
             (let ((start (mod stop n)))
               (if (or (zerop start)
                       (< stop n))
                   (rec 0 n nil)
                 (cons (subseq seq 0 start)
                       (rec start (+ start n) nil))
                 )))

            (t 
             (rec 0 n nil))
            )))))

(defun tuples (nel &rest args)
  (group args nel))

(defun pairs (&rest args)
  (group args 2))

(defun triples (&rest args)
  (group args 3))

#|
(defun prune (test tree)
  (nlet rec ((tree tree)
             (acc  nil))
    (declare (dynamic-extent #'rec))
    (cond ((null tree) (nreverse acc))
          ((consp (car tree))
           (rec (cdr tree)
                (cons (rec (car tree) nil) acc)))
          (t  (rec (cdr tree)
                   (if (funcall test (car tree))
                       acc
                     (cons (car tree) acc))))
          )))
|#

(defun prune (test tree)
  ;; tail-pure version
  (nlet prune-aux ((k    #'values)
                   (tree tree)
                   (acc  nil))
    (cond
     ((null tree)
      (funcall k (nreverse acc)))
     
     ((consp (car tree))
      (prune-aux (lambda (hd)
                   (prune-aux k (cdr tree) (cons hd acc)))
                 (car tree) nil))
     
     (t
      (prune-aux k (cdr tree)
                 (if (funcall test (car tree))
                     acc
                   (cons (car tree) acc))))
     )))

(defun find2 (fn lst)
  (and lst
       (let ((val (funcall fn (car lst))))
         (if val
             (values (car lst) val)
           (find2 fn (cdr lst))))))

(defun before (x y lst &key (test 'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))
               ))))

(defun after (x y lst &key (test 'eql))
  (let ((rest (before y x lst :test test)))
    (and rest
         (member x rest :test test))))

(defun duplicate (obj lst &key (test 'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (endp src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max  (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setf wins obj
                  max  score))))
      (values wins max))))

(defun best (fn lst)
  (when lst
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
            (setf wins obj)))
      wins)))

;; -----------------------------------------------
;; functional version of ACCUM macro...
#||#
(defun do-accum (fn)
  #F
  (declare (function fn))
  (let* ((hd  (list nil))
         (tl  hd))
    (declare (cons hd tl))
    (labels ((accum (val)
               (setf tl
                     (setf (cdr tl)
                           (list val)))))
      (funcall fn #'accum)
      (cdr hd))))
  
(defmacro accum (accfn &body body)
  (with-gensyms (gaccum garg)
      `(do-accum (lambda (,gaccum)
                   (declare (function ,gaccum))
                   (labels ((,accfn (,garg)
                              (funcall ,gaccum ,garg)))
                     ,@body)))
    ))
#||#
;; -----------------------------------------------
#|
(defmacro accum (accfn &body body)
  (with-gensyms (ghead gtail garg)
    `(let* ((,ghead (list nil))
            (,gtail ,ghead))
       (declare (cons ,ghead ,gtail))
       (labels ((,accfn (,garg)
                  (setf ,gtail
                        (setf (cdr ,gtail)
                              (list ,garg)))))
         ,@body
         (cdr ,ghead)))))
|#
#|
(defun alist-plist (alist)
  (accum a
    (dolist (pair alist)
      (a (car pair))
      (a (cdr pair)))))

(defun tst (ngrp)
  (time
   (dotimes (g (* 1000 ngrp))
     (accum acc
       (dotimes (ix 1000)
         (acc ix))))))
|#

;; -----------------------------------------------------
#||#
#|
(defun make-list-builder-v ()
  ;; An O(1) list builder object, in FIFO order. Fast append uses a
  ;; circular list with cell pointing at last cdr, front of list
  ;; always in (cdr cell)
  #F
  (let (cell)
    (labels ((set-cdr (x)
               (when-let (xcell (or cell x))
                 (setf cell (setf (cdr xcell) x))
                 ))
             (append-item (x)
               (set-cdr (cons x (cdr cell))))
             (get ()
               (prog1
                   (cdr cell)
                 (set-cdr nil)))
             (reset (x)
               (setf cell nil)
               (set-cdr (list x))))
      (let ((jv (vector #'append-item #'get #'reset)))
        (lambda (ix &rest args)
          (apply (aref jv ix) args))
        ))))

(defun make-list-builder-v2 ()
  ;; An O(1) list builder object, in FIFO order. Fast append uses a
  ;; circular list with cell pointing at last cdr, front of list
  ;; always in (cdr cell)
  #F
  (let (cell)
    (labels ((set-cdr (x)
               (when-let (xcell (or cell x))
                 (setf cell (setf (cdr xcell) x))
                 ))
             (append-item (x)
               (set-cdr (cons x (cdr cell))))
             (get ()
               (prog1
                   (cdr cell)
                 (set-cdr nil)))
             (reset (x)
               (setf cell nil)
               (set-cdr (list x))))
     (vector #'append-item #'get #'reset)
     )))
|#
;; -----------------------------------------------------
#|
  ;; here it is in CLOS... sure looks more cumbersome...
  
(defclass list-builder ()
  ((cell  :accessor lb-cell  :initform nil)))

(defmethod lb-set-cdr ((lb list-builder) x)
  #F
  (with-accessors ((cell lb-cell)) lb
    (when-let (xcell (or cell x))
      (setf cell (setf (cdr xcell) x)))
    ))

(defmethod lb-reset ((lb list-builder) x)
  #F
  (with-accessors ((cell lb-cell)) lb
    (setf cell nil)
    (lb-set-cdr lb (list x))
    ))

(defmethod lb-append ((lb list-builder) x)
  #F
  (with-accessors ((cell lb-cell)) lb
    (lb-set-cdr lb (cons x (cdr cell)))
    ))

(defmethod lb-get ((lb list-builder))
  #F
  (with-accessors ((cell lb-cell)) lb
    (prog1
        (cdr cell)
      (lb-set-cdr lb nil))
    ))
|#
;; ---------------------------------------------------
;; Simple Struct-based O(1) list constructor - fastest impl

(defstruct lbs
  cell)

(defun lbs-set-cdr (lbs x)
  #F
  (with-accessors ((cell lbs-cell)) lbs
    (when-let (xcell (or cell x))
      (setf cell (setf (cdr xcell) x))
      )))

(defun lbs-reset (lbs x)
  #F
  (with-accessors ((cell lbs-cell)) lbs
    (setf cell nil)
    (lbs-set-cdr lbs (list x))
    ))

(defun lbs-append (lbs x)
  #F
  (with-accessors ((cell lbs-cell)) lbs
    (lbs-set-cdr lbs (cons x (cdr cell)))
    ))

(defun lbs-get (lbs)
  #F
  (with-accessors ((cell lbs-cell)) lbs
    (prog1
        (cdr cell)
      (lbs-set-cdr lbs nil))
    ))

#|
 ;; OOFTAH!! CLOS is about 10x faster than DLAMBDA !!
 ;; using JV is almost twice as faster than CLOS !!
 ;; using JV2 (direct jump through vector) is 2x again !!
 ;; using LBS is fastest yet!!
 ;; Timings for 100M appends: (median of 3 meas)
 ;;   CLFREE Labels  1.395
 ;;   LBS            1.807s
 ;;   JV2            2.527
 ;;   JV             4.537
 ;;   CLOS           8.952
 ;;   RAW           16.432
 ;;   DLAM          60.38
 
(defun bake-off-dlambda (ngrp)
  (time (let ((lb (make-list-builder)))
          (dotimes (g (* 1000 ngrp))
            (dotimes (ix 1000)
              (funcall lb :append ix))
            (funcall lb :get)))))

(defun bake-off-clos (ngrp)
  (time (let ((lb (make-instance 'list-builder)))
          (dotimes (g (* 1000 ngrp))
            (dotimes (ix 1000)
              (lb-append lb ix))
            (lb-get lb)))))

(defmacro jv (jv ix &rest args)
  `(funcall ,jv ,ix ,@args))

(defun bake-off-jv (ngrp)
  (time (let ((lb (make-list-builder-v)))
          (dotimes (g (* 1000 ngrp))
            (dotimes (ix 1000)
              (jv lb 0 ix))
            (jv lb 1)))))

(defmacro jv2 (jv ix &rest args)
  `(funcall (aref ,jv ,ix) ,@args))

(defun bake-off-jv2 (ngrp)
  (time (let ((lb (make-list-builder-v2)))
          (dotimes (g (* 1000 ngrp))
            (dotimes (ix 1000)
              (jv2 lb 0 ix))
            (jv2 lb 1)))))

(defun bake-off-lbs (ngrp)
  (declare (fixnum ngrp))
  (time (let ((lbs (make-lbs)))
          (dotimes (g (* 1000 ngrp))
            (declare (fixnum g))
            (dotimes (ix 1000)
              (declare (fixnum ix))
              (lbs-append lbs ix))
            (lbs-get lbs)))
        ))

(defun bake-off-raw (ngrp)
  #F
  (declare (fixnum ngrp))
  (let (cell)
    (labels ((set-cdr (x)
               (when-let (xcell (or cell x))
                 (setf cell (setf (cdr xcell) x))))
             (append-item (x)
               (set-cdr (cons x (cdr cell))))
             (get-list ()
               (prog1
                   (cdr cell)
                 (set-cdr nil))))
      (declare (inline set-cdr append-item get-list))
      (time (dotimes (g (* 1000 ngrp))
              (declare (fixnum g))
              (dotimes (ix 1000)
                (declare (fixnum ix))
                (append-item ix))
              (get-list)))
      )))

(defun bake-off-raw3 (ngrp)
  #F
  (declare (fixnum ngrp))
  (let (cell)
    (labels ((set-cdr (cell x)
               (when-let (xcell (or cell x))
                 (setf cell (setf (cdr xcell) x))))
             (append-item (x)
               (set-cdr cell (cons x (cdr cell))))
             (get-list ()
               (prog1
                   (cdr cell)
                 (set-cdr cell nil))))
      (time (dotimes (g (* 1000 ngrp))
              (declare (fixnum g))
              (dotimes (ix 1000)
                (declare (fixnum ix))
                (append-item ix))
              (get-list)))
      )))

(defun bake-off-raw2 (ngrp)
  #F
  (declare (fixnum ngrp))
  (labels ((set-cdr (cell x)
             (when-let (xcell (or cell x))
               (setf cell (setf (cdr xcell) x))))
           (append-item (cell x)
             (set-cdr cell (cons x (cdr cell))))
           (get-list (cell)
             (prog1
                 (cdr cell)
               (set-cdr cell nil))))
    (let (cell)
      (time (dotimes (g (* 1000 ngrp))
              (declare (fixnum g))
              (dotimes (ix 1000)
                (declare (fixnum ix))
                (append-item cell ix))
              (get-list cell)))
      )))
|#
;; -----------------------------------------------------

(defun mostn (fn lst)
  (when lst
    (let ((lbs  (make-lbs))
          (max  (funcall fn (car lst))))
      (lbs-append lbs (car lst))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setf max  score)
                 (lbs-reset lbs obj))

                  ((= score max)
                   (lbs-append lbs obj))
                  )))
        (values  (lbs-get lbs)
                 max)
        )))

#|
(defun mostn (fn lst)
  (when lst
    (let ((lbs  (make-lbs))
          (max  (funcall fn (car lst))))
      (lbs-append lbs (car lst))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setf max  score)
                 (lbs-reset lbs obj))

                  ((= score max)
                   (lbs-append lbs obj))
                  )))
        (values  (lbs-get lbs)
                 max)
        )))
|#      

(defun drop (n seq)
  (when seq
    (if (consp seq)
        (nthcdr n seq)
      (subseq seq n))))

(defun take (n seq)
  (when seq
    (if (consp seq)
        (subseq seq 0 (and (nthcdr n seq) n))
      (subseq seq 0 (min n (length seq))))))

(defun lastn (n lst)
  (nreverse (take n (reverse lst))))

(defun split (n seq)
  (let ((hd (take n seq))
        (tl (drop n seq)))
    (values hd tl)))

;; -----------------------------------------------------

(defun zip (&rest lsts)
  (apply 'mapcar 'list lsts))

(defun interleave (&rest lsts)
  (apply 'mapcan 'list lsts))

#|
(defun shallow-flatten (x)
  (nlet-tail rec ((x   x)
                  (acc nil))
    (cond ((null x) (nreverse acc))
          ((atom x) (nreverse (cons x acc)))
          ((atom (car x))
           (rec (cdr x) (cons (car x) acc)))
          (t
           (rec (cdr x) (nconc (reverse (car x)) acc)))
          )))

(defun interleave (&rest lsts)
  (shallow-flatten (apply 'zip lsts)))
|#

;; -----------------------------------------------------
;; Mapping

(defun collect-> (pred start until-fn succ-fn
                       &key
                       (key    'identity)
                       (map-fn 'identity))
  #F
  (accum acc
    (do ((x  start  (funcall succ-fn x)))
        ((funcall until-fn x))
      (when (funcall pred (funcall key x))
        (acc (funcall map-fn x)))
      )))

(defun map-> (fn start until-fn succ-fn)
  (collect-> 'true start until-fn succ-fn :map-fn fn))

(defun mapn (n fn &rest lsts)
  ;; map fn across the first n elements of all list args,
  ;; or until one of the lists is empty.
  ;; 
  ;; We could also do (mapcar fn (take n lst)) if only one list,
  ;; but hopefully this is a bit more efficient.
  #F
  (map-> (lambda (lsts)
           (apply fn (mapcar 'car lsts)))
         lsts
         (lambda (lsts)
           (or (not (plusp n))
               (some 'endp lsts)))
         (lambda (lsts)
           (decf n)
           (mapcar 'cdr lsts))))

(defun mapa-b (fn a b &optional (step 1))
  ;; collect fn on integers [a,b)
  #F
  (assert (not (zerop step)))  ;; otherwise, endless loop
  (map-> fn a
         (lambda (n)
           (or (and (plusp step)
                    (>= n b))
               (and (minusp step)
                    (<= n b))))
         (curry '+ step) ))

(defun map0-n (fn n &optional (step 1))
  (mapa-b fn 0 n step))

(defun map1-n (fn n &optional (step 1))
  (mapa-b fn 1 (1+ n) step))

(defun mappend (fn &rest lsts)
  (apply 'append (apply 'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  ;; collect fn applied to every element of each list in succession
  #F
  (map-> (compose fn 'car)
         (pop lsts)
         'endp
         (lambda (lst)
           (if (single lst)
               (pop lsts)
             (cdr lst))) ))

(defun rmapcar (fn &rest args)
  ;; recursive mapcar for trees
  #F
  (if (some 'atom args)
      (apply fn args)
    (apply 'mapcar
           (lambda (&rest args)
             (apply 'rmapcar fn args))
           args)))

;; ------------------------------------------------------------------
#+:lispworks
(defun pickfile (message &rest rest)
  (apply 'capi:prompt-for-file message rest))

(defun get-time-string (&key (time (get-universal-time))
                             short-form
                             utc)
  (multiple-value-bind (secs mins hrs day mon year dow dst-p tz)
      (if utc
          (decode-universal-time time 0)
        (decode-universal-time time))
    (let ((wkday (aref #("Monday" "Tuesday" "Wednesday"
                                  "Thursday" "Friday" "Saturday"
                                  "Sunday")
                       dow))
          (mth (aref #("January" "February" "March"
                                 "April" "May" "June" "July"
                                 "August" "September" "October"
                                 "November" "December")
                     (1- mon)))
          (zone (if (zerop tz)
						"UTC"
						(aref (if dst-p
								#("EDT" "CDT" "MDT" "PDT")
	                        #("EST" "CST" "MST" "PST"))
    	                  (- tz 5)))))
      (format nil "~A ~A ~A ~A  ~2,'0D:~2,'0D:~2,'0D ~A"
              (if short-form
                  (subseq wkday 0 3)
                wkday)
              (if short-form
                  (subseq mth 0 3)
                mth)
              day year hrs mins secs zone))
    ))

;; ----------------------------------------------------
#|
(defun map-from-below (fn from below &optional (by 1))
  (let ((c (make-collector)))
    (do ((ix from (+ ix by)))
        ((>= ix below) (collector-contents c))
      (collector-append-item c (funcall fn ix))
      )))
|#

(defun map-from-below (fn from below &optional (by 1))
  (loop for ix from from below below by by
        collect (funcall fn ix)))

(defun map-from-to (fn from to &optional (by 1))
  (map-from-below fn from (1+ to) by))

(defun map-from-for (fn from len &optional (by 1))
  (map-from-below fn from (+ from len) by))

;; ---------------------------------------------------------------------
#|
(defun collect-where (lst sels &key (test 'eql) (key 'identity))
  (let ((c (make-collector)))
    (do ((l lst  (cdr l))
         (s sels (cdr s)))
        ((or (endp l)
             (endp s)) (collector-contents c))
      (if (funcall test (funcall key (car s)))
          (collector-append-item c (car l)))
      )))
|#

#|
(defun collect-where (lst sels &key (test 'eql) (key 'identity))
  (let ((c (make-coll)))
    (do ((l lst  (cdr l))
         (s sels (cdr s)))
        ((or (endp l)
             (endp s)) (funcall c :get-items))
      (if (funcall test (funcall key (car s)))
          (funcall c :add (car l)))
      )))

(defun collect-where-not (lst sels &key (test 'eql) (key 'identity))
  (collect-where lst sels :test (complement test) :key key))
|#
;; ---------------------------------------------------------------------

(defmethod subselector (pred (lst list)
                             &key (key 'identity)
                             (start 0) end count
                             (index t)
                             &allow-other-keys)
  (let* ((lst (nthcdr start lst))
         (ct  0))
    (collect-> (lambda (n)
                 (declare (ignore n))
                 (and (funcall pred (funcall key (car lst)))
                      (incf ct)))
               start
               (lambda (n)
                 (or (endp lst)
                     (and count (>= ct count))
                     (and end (>= n end))))
               (lambda (n)
                 (pop lst)
                 (1+ n))
               :map-fn (if index
                           'identity
                         (lambda (n)
                           (declare (ignore n))
                           (car lst)))
               )))

(defmethod subselector (pred (arr array)
                             &key (key 'identity)
                             (start 0) (end (array-total-size arr))
                             count
                             (index t)
                             &allow-other-keys)
  (let ((ct 0))
    (collect-> (lambda (n)
                 (and (funcall pred (funcall key (row-major-aref arr n)))
                      (incf ct)))
               start
               (lambda (n)
                 (or (>= n end)
                     (and count (>= ct count))))
               '1+
               :map-fn (if index
                           'identity
                         (curry 'row-major-aref arr))
               )))

(defun where-if (pred seq &rest keys)
  (apply 'subselector pred seq keys))
             
(defun where-if-not (predicate &rest keys)
  (apply 'where-if (complement predicate) keys))

(defun subselect-if (pred seq &rest keys)
  ;; this works on multi-dimensional arrays too, unlike remove
  ;; returning the row-major indexed elements, but always returns a list
  ;; For actual sequences, use remove as possibly more tuned for efficiency
  (apply 'subselector pred seq :index nil keys))

(defun subselect-if-not (predicate seq &rest keys)
  (apply 'subselect-if (complement predicate) seq keys))


(defun where (item seq &rest keys &key (test 'eql))
  (apply 'where-if (curry test item) seq keys))


(defun nthcar (n list) 
  (car (nthcdr n list)))

(defmethod subselect ((lst list) where-lst)
  ;; not very efficient on lists... use subselect-if on lists
  (mapcar (rcurry 'nthcar lst) where-lst))

(defmethod subselect ((arr array) where-lst)
  ;; possibly more efficent than using subselect-if on arrays
  (map 'vector (curry 'row-major-aref arr) where-lst))

(defmethod row-major-vector ((vec vector))
  vec)

(defmethod row-major-vector ((arr array))
  ;; get a row-major vector displaced onto the array
  ;; can now use remove, delete, map, etc on the vector.
  (make-array (array-total-size arr)
              :element-type (array-element-type arr)
              :displaced-to arr))

(defmethod row-major-vector ((lst list))
  (coerce lst 'vector))

#|
(defmethod subselect (proseq (where null))
  (declare (ignore proseq))
  nil)

(defmethod subselect (proseq (where cons))
  (subselect proseq (coerce where 'vector)))

(defmethod subselect ((proseq list) (where vector))
  (subselect (coerce proseq 'vector) where))

(defmethod subselect ((proseq vector) (where vector))
  (let* ((len  (length where))
         (rslt (if (plusp len)
                   (make-array len
                               :element-type (array-element-type proseq))
                 nil)))
    (when rslt
      (dotimes (ix len rslt)
        (setf (aref rslt ix) (aref proseq (aref where ix)))))
    ))

(defmethod subselect ((proseq array) (where vector))
  (subselect (make-array (array-total-size proseq)
                         :displaced-to proseq
                         :element-type (array-element-type proseq))
             where))
|#


(defun indgen (n)
  (declare (fixnum n))
  (let ((rslt (make-array n :element-type 'fixnum)))
    (declare (type (vector fixnum *) rslt))
    (dotimes (ix n rslt)
      (declare (fixnum ix))
      (setf (aref rslt ix) ix))))

(defun collect-if (predicate proseq &rest rest)
  (apply 'remove-if-not predicate proseq rest))
  
(defun collect-if-not (predicate proseq &rest rest)
  (apply 'remove-if predicate proseq rest))


(defun keep-if (predicate proseq &rest rest)
  (apply 'delete-if-not predicate proseq rest))

(defun keep-if-not (predicate proseq &rest rest)
  (apply 'delete-if predicate proseq rest))

;; ---------------------------------------------------------------------
;; Macros to take the pain out of passing strings to DLL's
;;
#+(AND :COM.RAL :lispworks)
(progn
  (defconstant *null-string*
    (fli:make-pointer :address 0))
  
  (defun _with-cstring (str fn)
    (if str
	(fli:with-foreign-string (cstr nel nb) (mkstr str)
				 (declare (ignore nel nb))
				 (funcall fn cstr))
	(fli:with-coerced-pointer (p :type 'ct:uchar) *null-string*
				  (funcall fn p))))
  
  (defmacro with-cstring (binding &body body)
    `(_with-cstring ,(second binding)
		    (lambda (,(first binding))
                      ,@body)))
  
  (defmacro with-cstrings (bindings &body body)
    (if (null bindings)
	`(progn ,@body)
	`(with-cstring ,(first bindings)
	   (with-cstrings ,(cdr bindings) ,@body))))
  
  (defun actual-args (arglist)
    ;; Some args are default :constant args without a name
    ;; These should not be specified when calling the FLI function.
    ;; Just return the list of actual arguments.
    (remove-if (lambda (arg)
                 (and (consp arg)
                      (eql :constant (first arg))))
	       arglist))
  
  (defun arg-name (arg)
    ;; some args are simple identifiers that default to type :int
    ;; and others are lists that follow the name with a C-type.
    ;; Just return the name of the arg.
    (if (consp arg)
	(first arg)
	arg))
  
  (defun arg-names (arglist)
    ;; Return the list of actual argument names.
    (mapcar 'arg-name arglist))
  
  
  (defmacro ez-define-foreign-function-receiving-c-string (name entry-name args
							   &rest rest)
    (with-gensyms (rslt dll-name)
      (let ((dll-argnames (arg-names (actual-args args))))
	`(progn
	   
	   (fli:define-foreign-function (,dll-name ,entry-name)
	       ,args
	     ,@rest)
	   
	   (defun ,name ,dll-argnames
	     (let ((,rslt (,dll-name ,@dll-argnames)))
	       (unless (fli:null-pointer-p ,rslt)
		 (fli:convert-from-foreign-string ,rslt)))))
	)))
  
  (defmacro ez-define-foreign-function-sending-c-strings (name entry-name args
							  &rest rest)
    (labels
	((is-cstring-arg (arg)
	   (and (consp arg)
		(equal (rest arg) '(ct:out-cstring)))))
      
      (let* ((actuals          (actual-args args))
             (proto-list       (mapcar (lambda (arg)
                                         (if (is-cstring-arg arg)
                                             (list (gensym)
                                                   (arg-name arg))
                                           (arg-name arg)))
                                       actuals))
	     (actual-argnames  (arg-names actuals))
	     (cstring-bindings (remove-if-not 'consp proto-list))
	     (call-argnames    (arg-names proto-list))
	     (dll-name         (gensym)))
	
	`(progn
	   
	   (fli:define-foreign-function (,dll-name ,entry-name)
	       ,args ,@rest)
	   
	   (defun ,name ,actual-argnames
	     (with-cstrings ,cstring-bindings
	       ,(if (eql 'ct:in-cstring (getf rest :result-type))
		    (let ((rslt (gensym)))
		      `(let ((,rslt (,dll-name ,@call-argnames)))
			 (unless (fli:null-pointer-p ,rslt)
			   (fli:convert-from-foreign-string ,rslt))))
		    `(,dll-name ,@call-argnames))
	       )))
	)))
  
  (defmacro ez-define-foreign-function ((name entry-name) args &rest rest)
    (cond ((find '(ct:out-cstring) args :key 'cdr :test 'equal)
	   `(ez-define-foreign-function-sending-c-strings ,name ,entry-name
							  ,args
							  ,@rest))
	  ((eql 'ct:in-cstring (getf rest :result-type))
	   `(ez-define-foreign-function-receiving-c-string ,name ,entry-name
							   ,args 
							   ,@rest))
	  (t
	   `(fli:define-foreign-function (,name ,entry-name)
		,args
	      ,@rest))))
  )

;; ------------------------------------------------------------------
;;

(defmacro def-enum (&rest enums)
  "Generate a list of defconstant's beginning with zero.
If one if the elements of the list is a pair
then generate that and all following constants
beginning with the value of the second element.
This is C++ style enumerations."
  (let ((cur-ix -1)
        (base   0))
    `(um:eval-always
       ,@(mapcar
          (lambda (enum)
            (if (consp enum)
                `(defconstant ,(first enum)
                   ,(progn
                      (setf cur-ix 0)
                      (setf base (second enum))))
              `(defconstant ,enum
                 (+ ,base ,(incf cur-ix)))
              ))
          enums))
    ))

;; ------------------------------------------------------------------
;; !!! Extra !!!

(defun foldl (fn init seq)
  ;; fn should be a function of (accum item)
  (reduce fn seq
          :initial-value init))

(defun foldr (fn seq init)
  ;; fn should be a function of (item accum)
  (reduce fn seq
          :from-end t
          :initial-value init))

(defun => (&rest fns)
  ;; RPN Style passing results left to right
  `(compose ,@(reverse fns)))

#| ;; use Alexandria's version
(defun compose (&rest fns)
  (cond ((null fns)   'identity)
        ((single fns) (car fns))
        ((single (rest fns))
         (destructuring-bind (fn1 fn2) fns
           (lambda (&rest args)
             (funcall fn1 (apply fn2 args)))
           ))
        (t (let* ((rfns (nreverse fns))
                  (fn1  (first rfns))
                  (fns  (nreverse (rest rfns))))
             (lambda (&rest args)
               (foldr 'funcall fns (apply fn1 args)))
             ))
        ))
|#

#|
;; in ML these are referred to as sections
;; these actually correspond to the Dylan operators
;; secr ::= rcurry, secl ::= curry
(defun rcurry (f &rest suf-args)
  (lambda (&rest pref-args)
    (apply f (append pref-args suf-args))))

(defun curry (f &rest pref-args)
  (lambda (&rest suf-args)
    (apply f (append pref-args suf-args))))
|#

#|
;; ML versions
(defun secr (f &rest suf-args)
  (lambda (&rest pref-args)
    (apply f (append pref-args suf-args))))

(defun secl (f &rest pref-args)
  (lambda (&rest suf-args)
    (apply f (append pref-args suf-args))))

;; in ML currying and uncurrying happens with functions of 2 args
(defun curry (f)
  (lambda (a)
    (lambda (b)
      (funcall f a b))))

(defun uncurry (f)
  (lambda (a b)
    (funcall (funcall f a) b)))
|#
#|
(defmacro rcurry (f &rest args)
  (let ((x (gensym)))
    `(lambda (,x)
       (funcall ,f ,x ,@args))
    ))

(defmacro curry (f &rest args)
  (let ((x (gensym)))
    `(lambda (,x)
       (funcall ,f ,@args ,x))
    ))
|#

(defun combine (op &rest fns)
  ;; operationally combine function results
  ;; under binary operation op
  (lambda (x)
    (apply op (mapcar (lambda (fn)
                        (funcall fn x))
                      fns))))


;; -------------------------------------------------------
;;
(defun readlist (&rest args)
  (values (read-from-string
           (mkstr "(" (apply 'read-line args) ")"))))

(defun prompt (&rest args)
  (apply 'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply 'prompt args)))
     (if (funcall quit in)
         (return)
       (format *query-io* "~A~%" (funcall fn in))))))

(defun reread (&rest args)
  (values (read-from-string (apply 'mkstr args))))

(defun explode (sym)
  (map 'list (compose 'intern-symbol 'string) (symbol-name sym)))

;; -------------------------------------------------------
;;
#| |#
#| |#     
     
              
;; ---------------------------------------------------
;;
(defun eqlcond-clause (var val &rest body)
  (cond ((consp val)
         (if (eql 'QUOTE (first val))
             `((eql ,var ,val) ,@body)
           `((or ,@(mapcar (lambda (v)
                             `(eql ,var ,v))
                           val))
             ,@body)
           ))
        
        ((eq val :otherwise) `(t ,@body) )
        
        (t  `((eql ,var ,val) ,@body) )))

(defmacro! eqlcond (o!var &rest clauses)
  `(cond ,@(mapcar
            (lambda (clause)
              (apply 'eqlcond-clause g!var clause))
            clauses)))

;; -------------------------------------------------------
;; Safe FLI interfaces with coercion of caller args to
;; types required by FLI interface
;;
#+:LISPWORKS
(defun* coerce-fli-arg ((name type &rest rest))
  (declare (ignore rest))
  (unless (eq name :constant)
    (case type
      ((:short :long :int) 
       `(,name (coerce ,name 'fixnum)))
      ((:float :single-float)
       `(,name (coerce ,name 'single-float)))
      ((:double :double-float)     
       `(,name (coerce ,name 'double-float)))
      (otherwise
       `(,name ,name))
      )))

#+:LISPWORKS
(defmacro def-safe-fli-function ((name &rest args)
                                 user-args &rest other-args)
  (let ((cname       (intern-symbol (format nil "_~A" name)))
        (caller-args (delete :constant 
                             (mapcar 'first user-args)))
        (coercions   (delete nil 
                             (mapcar 'coerce-fli-arg user-args))))
    `(progn
       (fli:define-foreign-function (,cname ,@args)
           ,user-args
         ,@other-args)
       (defun ,name ,caller-args
         (let ,coercions
           (,cname ,@caller-args))))
    ))

#|
;; Example:
(def-safe-fli-function (diddly)
                       ((a :float)
                        (b :int))
                       :result-type :long)

   ==> (by macro-expansion)

(PROGN
  (FLI:DEFINE-FOREIGN-FUNCTION (_DIDDLY) ((A :FLOAT) (B :INT)) :RESULT-TYPE :LONG)
  (DEFUN DIDDLY (A B)
    (LET ((A (COERCE A 'SINGLE-FLOAT)) (B (COERCE B 'FIXNUM))) (_DIDDLY A B))))
|#

;; ------------------

#|
;; --------------------------------------------
;; So...
;; We can define LET through the use of a lambda function:
;;
;;  (LET ((x <expr>)) <body>)
;;
;; same as
;;
;;  (funcall (lambda (x)
;;               <body>)
;;     <expr>)
;;
;; ----------------------------------
;; Idiom:  (if x x y)  => (or x y)  ;; good one to call (either x y)
;;         (when x y)  => (and x y)
;;         (unless x y) => (and (not x) y)

(defmacro either (a b)
  `(or ,a ,b))

(defmacro both (a b)
  `(and ,a ,b))

(defmacro any (&rest args)
  `(or ,@args))

(defmacro all (&rest args)
  `(and ,@args))
|#

#| -- these already in LW
(defun true (&rest args)
  (declare (ignore args))
  t)

(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun do-nothing (&rest ignore)
  ignore)
|#

;; -------------------------------------------------------
;; finite lists of values spanning a range
;;
(defun make-range (from to &key (by (if (>= to from) 1 -1)))
  (labels
      ((val-of (ix)
         (+ from (* ix by)))
       (iota (tst)
         (map-> #'val-of
                0
                (compose tst #'val-of)
                '1+)))
    
    (cond ((and (> to from) (plusp by))
           (iota (rcurry '>= #|ix|# to)))
          ((and (< to from) (minusp by))
           (decf from)
           (iota (rcurry '< #|ix|# to)))
          (t nil))))

(defun range (a b &optional c)
  (if c
      (make-range a c :by (- b a))
    (make-range a b)))

;; ------------------------------------------------------------
#|
;; Possibly useful idiom...
;;
;;  To make a binary function into a function that can be mapped against
;;  multiple sequences, possibly more than 2,
;;  as in (defun a+b (a b) (+ a b)) in (map 'vector 'a+b '(1 2 3) '(4 5 6) '(7 8 9))
;;  which should have the effect of (vector (reduce 'a+b '(1 4 7))
;;                                          (reduce 'a+b '(2 5 8))
;;                                          (reduce 'a+b '(3 6 9))) 
;;
;;   (um:compose (um:curry 'reduce #'<your-binary-function-here>) 'list)
|#

(defun largest-abs-value (a b)
  ;; return the argument having the largest absolute value
  (if (> (abs a) (abs b))
      a
    b))

(defun max-abs (a &rest bs)
  (foldl 'largest-abs-value a bs))

(defun make-list-reducer (fn)
  (compose (curry 'reduce fn) 'list))

#|
  E.g.,
  (apply 'mapcar (make-list-reducer '+) '((1 2 3) (4 5 6) (7 8 9))) => (12 15 18)
|#
;; -----------------------------------------------------
;; Post incr and decr
 
(defmacro! post-incf (place &optional (incr 1))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar 'list vars forms)
            (,g!old-val ,access)
            (,(car var) (+ ,g!old-val ,incr)))
       ,set
       ,g!old-val)
    ))

#|
(post-incf (aref arr ix))
|#

(defmacro! post-decf (place &optional (decr 1))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar 'list vars forms)
            (,g!old-val ,access)
            (,(car var) (- ,g!old-val ,decr)))
       ,set
       ,g!old-val)
    ))


(defmacro! with-slot-values (slots o!instance &body body)
  `(let (mapcar (lambda (pair)
                  `(,(first pair) (,(second pair) ,g!instance)))
                ,slots)
     ,@body))

(defun firsts-of (lst)
  (mapcar 'first lst))

(defun slice (seq start &optional (nel 1))
  (let* ((len           (length seq))
         (actual-start  (mod start len))
         (slen          (min nel (- len actual-start))))
    (if (< slen nel)
	(concatenate (cond
		       ((listp   seq) 'list)
		       ((stringp seq) 'string)
		       ((vectorp seq) 'vector))
		     (subseq seq actual-start (+ actual-start slen))
                     (slice seq 0 (- nel slen)))
      ;; else
      (subseq seq actual-start (+ actual-start nel))
      )))

(defun left-part (seq nel)
  (slice seq 0 nel))

(defun right-part (seq nel)
  (slice seq (- nel) nel))

;; ---------------------------------------------------------------
;; Array mover

(defun move (src src-from dst dst-from nel)
  (replace dst src
           :start1 dst-from
           :start2 src-from
           :end1   (+ dst-from nel)))

;; ----------------------------------------------------------------

(defun pwr2-q (n)
  (and (plusp n)
       (eql n (ash 1 (1- (integer-length n))))))

#|
  ;; these are leftovers from the 32-bit era
#-:LISPWORKS
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (labels ((iter (n nsh)
             (declare (fixnum n nsh))
             (logior n (ash n nsh))))
    (declare (inline iter))
    (1+ (iter 
         (iter
          (iter
           (iter
            (iter (1- n) -1)
            -2)
           -4)
          -8)
         -16))))

#+:LISPWORKS
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (declare (optimize (float 0)))
  (labels ((iter (n nsh)
             (declare (sys:int32 n))
             (declare (fixnum nsh))
             (sys:int32-logior n (sys:int32>> n nsh))))
    (declare (inline iter))
    (the fixnum
         (sys:int32-to-integer
          (sys:int32-1+
           (iter 
            (iter
             (iter
              (iter
               (iter (sys:int32-1- n) 1)
               2)
              4)
             8)
            16)))
         )))
|#

(defun ceiling-log2 (n)
  (let ((nb (integer-length n)))
    (if (= n (ash 1 (1- nb)))
        (1- nb)
      nb)))

(defun ceiling-pwr2 (n)
  (ash 1 (ceiling-log2 n)))

#|
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (the fixnum
       (ash 1
            (the fixnum
                 (integer-length
                  (the fixnum
                       (1- n)))))))

(defun ceiling-log2 (n)
  (declare (fixnum n))
  (logcount (1- (ceiling-pwr2 n))))

(defun floor-pwr2 (n)
  (declare (fixnum n))
  (let ((c (ceiling-pwr2 n)))
    (if (= n c)
        n
      (ash c -1))))

(defun floor-log2 (n)
  (declare (fixnum n))
  (logcount (1- (floor-pwr2 n))))
|#

(defun floor-log2 (n)
  (1- (integer-length n)))

(defun floor-pwr2 (n)
  (ash 1 (floor-log2 n)))

(defun align-pwr2 (val pwr2)
  (declare (type fixnum pwr2)
           (type integer val))
  (let ((pwr2m1 (1- pwr2)))
    (logandc2 (+ val pwr2m1) pwr2m1)))

;; -----------------------------------------------------------------------

(defun format-error (err)
  "Routine to get the readable error message from a condition object."
  (with-output-to-string (s)
    (let ((*print-escape* nil))
      (print-object err s))))

;; ----------------------------------------------
;; convenience macro

(defmacro with-slot-accessors ((varname struct-name) slot-names &body body)
  `(with-accessors ,(mapcar (lambda (slot-name)
                              `(,(if (consp slot-name)
                                     (first slot-name)
                                   slot-name)
                                ,(intern-symbol (mkstr struct-name #\-
                                                       (if (consp slot-name)
                                                           (second slot-name)
                                                         slot-name)))
                                ))
                            slot-names)
       ,varname
     ,@body))


;; --------------------------------------------
;; (BIND* (bindings) . body)
;;
;; Note that (declare ...) forms may be included and interspersed among
;; BINDINGS; i.e., the (declare ...) form does not belong at the beginning of
;; body.

(defmacro bind* (bindings &body body)
  ;; Check for common programming error:
  (when (and (consp (first body))
             (eq (first (first body)) 'declare))
    ;; Place (declare ...) forms among  
    (warn 
     "Check BIND* declarations syntax. First body form ~s is possibly misplaced."
     (first body)))
  (nlet iter ((bindings bindings))
    
    (labels ((invalid-syntax (bindings)
               (error "Invalid BIND* syntax: ~S" bindings))
             
             (more (bindings)
               (let (decls)
                 (while (and (consp (car bindings))
                             (member (caar bindings) '(:DECLARE DECLARE)))
                   (push `(declare ,@(cdr (first bindings))) decls)
                   (pop bindings))
                 `(,@(nreverse decls)
                   ,(iter bindings))
                 )))

      (cond ((endp bindings) `(progn ,@body))
          
            ((symbolp (first bindings))
             `(let (,(first bindings))
                ,@(more (cdr bindings))))
          
            ((consp (first bindings))
             (let ((binding (first bindings)))
               
               (cond ((symbolp (first binding))

                      (aif (bind*-handler (first binding))
                          ;; something we handle
                          (funcall it
                                   binding
                                   (cdr bindings)
                                   #'more)
                        ;; else
                        ;; regular LET binding
                        (destructuring-bind (name val) (first bindings)
                          `(let ((,name ,val))
                             ,@(more (cdr bindings))))
                        ))
                   
                     ((consp (first binding))
                      ;; destructuring binding
                      (destructuring-bind (lst val) (first bindings)
                        `(destructuring-bind ,lst ,val
                            ,@(more (cdr bindings)))))
                   
                     (t
                      (invalid-syntax bindings))
                     )))
          
            (t (invalid-syntax bindings))
            ))))

(defun bind*-handler (symbol)
  (get symbol 'bind*-handler))

(defmacro! define-bind*-handler (&whole whole symbol (binding-name more-bindings-name) &body body)
  (multiple-value-bind (forms decls doc)
      (parse-body body :documentation t :whole whole)
    `(setf (get ,symbol 'bind*-handler)
           (lambda* (,binding-name ,g!bindings ,g!more)
             ,@doc
             ,@decls
             (symbol-macrolet ((,more-bindings-name (funcall ,g!more ,g!bindings)))
               ,@forms)))))

#|
(define-bind*-handler :ACCESSORS (binding more-bindings)
  (destructuring-bind (a-bindings obj) (rest binding)
    `(with-accessors ,(mapcar (lambda (a-binding)
                                (if (consp a-binding)
                                    a-binding
                                  (list a-binding a-binding)))
                              a-bindings)
         ,obj
       ,@more-bindings)))

(define-bind*-handler :STRUCT-ACCESSORS (binding more-bindings)
  (destructuring-bind (struct-type slot-bindings obj) (rest binding)
    `(with-slot-accessors (,obj ,struct-type) ,slot-bindings
        ,@more-bindings)))

(define-bind*-handler :SLOTS (binding more-bindings)
  (destructuring-bind (slot-bindings obj) (rest binding)
    `(with-slots ,slot-bindings ,obj
       ,@more-bindings)))
                      
(define-bind*-handler :SYMBOL-MACRO (binding more-bindings)
  (destructuring-bind (name form) (rest binding)
    `(symbol-macrolet ((,name ,form))
       ,@more-bindings)))

(define-bind*-handler :VALUES (binding more-bindings)
  (destructuring-bind (names form) (rest binding)
    `(multiple-value-bind ,names ,form
        ,@more-bindings)))

(define-bind*-handler :MATCH (binding more-bindings)
  (destructuring-bind (vars pats val) (rest binding)
    `(destructuring-bind ,vars (optima:ematch ,val ((or ,@pats) (list ,@vars)))
       ,@more-bindings)))
|#
(define-bind*-handler :ACCESSORS ((_ a-bindings obj) more-bindings)
  (declare (ignore _))
  `(with-accessors ,(mapcar (lambda (a-binding)
                              (if (consp a-binding)
                                  a-binding
                                (list a-binding a-binding)))
                            a-bindings)
       ,obj
     ,@more-bindings))

(define-bind*-handler :STRUCT-ACCESSORS ((_ struct-type slot-bindings obj) more-bindings)
  (declare (ignore _))
  `(with-slot-accessors (,obj ,struct-type) ,slot-bindings
     ,@more-bindings))

(define-bind*-handler :SLOTS ((_ slot-bindings obj) more-bindings)
  (declare (ignore _))
  `(with-slots ,slot-bindings ,obj
     ,@more-bindings))
                      
(define-bind*-handler :SYMBOL-MACRO ((_ name form) more-bindings)
  (declare (ignore _))
  `(symbol-macrolet ((,name ,form))
     ,@more-bindings))

(define-bind*-handler :VALUES ((_ names form) more-bindings)
  (declare (ignore _))
  `(multiple-value-bind ,names ,form
     ,@more-bindings))

(define-bind*-handler :MATCH ((_ vars pats val) more-bindings)
  (declare (ignore _))
  `(destructuring-bind ,vars (optima:ematch ,val ((or ,@pats) (list ,@vars)))
     ,@more-bindings))

#|
(bind* ((:match (a b c) ((list :one a :two b :three c)
                         (list :two a :three b :four c))
         `(:one 15 :two 18 :three ,pi)))
  (list a b c))

(bind* ((a 1)
        (b 2)
        (:values (x y z) doit)
        ((a c &key (d 5) &rest xs) doit2))
  body)
(bind* ((a 1)
        (b 2)
        (:values (x y z) (values 15 22 34 55))
        ((d e &rest xs &key (g 99) &allow-other-keys) '(101 102 :g 13 :h 88)))
  (list a b x y z d e xs g))
|#

;; --------------------------------------------------------------

(defun binsearch (low-index hi-index compare-fn)
  (declare (type fixnum low-index hi-index))
  ;; General utility binary search routine.
  ;; low-index = starting index of table, high-index is 1 beyond table's last index.
  ;; compare-fn is a user provided comparison routine of one argument, the index,
  ;; and it should return <0, =0, >0 for each index.
  ;; returns: found, ixu
  ;;
  ;; When found is true, ixu is its index location
  ;; When found is false, ixu is where it would have to be inserted for key < key[ixu]
  ;; each index. Routine stops when comparison yields 0, or when the table is exhausted.
  ;; Comparison values of <0 indicate that the index is too high,
  ;; >0 indicates it is too low.
  ;;
  ;; Easiest way, for numeric args, is to use (- wanted-key ixm-key) for the comparison function,
  ;; where ixm-key corresponds to the ixm sent by this routine.
  ;;
  (nlet srch ((ixl (1- low-index))
              (ixu hi-index))
    (declare (type fixnum ixl ixu))
    (cond ((> (- ixu ixl) 1)
           (let* ((ixm (truncate (+ ixu ixl) 2))
                  (c   (funcall compare-fn ixm)))
             (declare (type fixnum ixm c))
             (cond ((zerop c)  (values t ixm)) ;; found it!
                   
                   ((minusp c) (go-srch ixl ixm))
                   
                   (t          (go-srch ixm ixu))
                   )))

          (t  (values nil ixu))
          )))

;; --------------------------------------

(defun hhmmss.ss (val)
  "(hhmmss.ss val) -- convert time from hhmmss.ss to sec"
  (let* ((h    (truncate val #n1_00_00))
         (mmss (- val (* h #n1_00_00)))
         (m    (truncate mmss #n1_00))
         (s    (- mmss (* #n1_00 m))))
    (values (+ s (* 60 (+ m (* 60 h))))
            h m s)))

(defun hms (val)
  "(hms val) -- convert time from hhmmss.ss to sec"
  (hhmmss.ss val))

(defun ddmmyyyy (val)
  "(ddmmyyyy val) -- convert date from ddmmyyyy to universal time"
  (let* ((d    (truncate val #n1_00_0000))
         (mmyy (- val (* d #n1_00_0000)))
         (m    (truncate mmyy #n1_0000))
         (y    (- mmyy (* m #n1_0000))))
    (values (encode-universal-time 0 0 0 d m y)
            d m y)))

(defun dmy (val)
  "(dmy val) -- convert date from ddmmyy to universal time"
  (let* ((d    (truncate val #n1_00_00))
         (mmyy (- val (* d #n1_00_00)))
         (m    (truncate mmyy #n1_00))
         (y    (+ 2000 (- mmyy (* m #n1_00)))))
    (values (encode-universal-time 0 0 0 d m y)
            d m y)))

  
(defun yyyymmdd (val)
  "(yyyymmdd val) -- convert date from yyyymmdd to universal time"
  (let* ((y     (truncate val #n1_00_00))
         (mmdd  (- val (* y #n1_00_00)))
         (m     (truncate mmdd #n1_00))
         (d     (- mmdd (* m #n1_00))))
    (values (encode-universal-time 0 0 0 d m y)
            y m d)))
    
(defun ymd (val)
  "(ymd val) -- convert date from yymmdd to universal time"
  (yyyymmdd (+ val #.(* 2000 #n1_00_00))))
    
;; --------------------------------------

(defun nn-to-hz (nn)
  ;; 440 Hz = A4 = A above middle C
  (* 440 (expt 2 (/ (- nn 69) 12))))

(defun hz-to-nn (hz)
  ;; nn60 = Middle C = C4
  (+ 69 (* 12 (log (/ hz 440) 2))))

(defun q-for-bw (bw)
  ;; bw in octaves
  (/ (* 2 (sinh (/ (* bw (log 2)) 2)))))

(defun bw-for-q (q)
  ;; bw in octaves
  (* (/ 2 (log 2)) (asinh (/ (* 2 q)))))

;; --------------------------------------
#|
(defmethod slot-names ((class structure-class))
  #+:LISPWORKS (structure:structure-class-slot-names class)
  #+:CLOZURE   (mapcar 'ccl:slot-definition-name
                       (ccl:class-slots class))
  #+:ALLEGRO   (mapcar 'clos:slot-definition-name
                       (clos:class-slots class)))

(defmethod slot-names ((class standard-class))
  #+:LISPWORKS (mapcar 'clos:slot-definition-name
                       (clos:class-slots class))
  #+:CLOZURE   (mapcar 'ccl:slot-definition-name
                       (ccl:class-slots class))
  #+:ALLEGRO   (mapcar 'clos:slot-definition-name
                       (clos:class-slots class)))

(defmethod slot-names (class)
  (error "Can't get slot names for class: ~A" class))
    
;; --------------------------------------

(defun make-struct-copy (type struct)
  ;; assumes we have a standard copy function for the struct
  (let ((copier (intern (mkstr :COPY- (symbol-name type))
                        (symbol-package type))))
    (funcall copier struct)))

(defun copy-struct (obj &rest bindings)
  ;; assumes we have a standard copy function for the struct
  (let* ((type        (type-of obj))
         (cpy         (make-struct-copy type obj)))
    (when bindings
      (let* ((class       (find-class type))
             (slots       (slot-names class))
             (pairs       (group bindings 2))
             (setf-pairs  (mapcar (lambda (pair)
                                    (destructuring-bind (name val) pair
                                      (let ((sname (find name slots
                                                         :test 'string-equal)))
                                        (if sname
                                            (list sname val)
                                          (error "no slot named: ~A for type: ~A"
                                                 name type)))))
                                  pairs)))
        (mapcar (lambda (pair)
                  (destructuring-bind (name val) pair
                    (setf (slot-value cpy name) val)))
                setf-pairs)
        ))
    cpy))
|#

;; -----------------------------------------------------------------

(defun separate-decls-and-body (body)
  (do ((decls (and (stringp (car body)) (list (car body))) )
       (lst   (if (stringp (car body)) (cdr body) body)  (cdr lst)))
      ((not (and (consp (car lst))
                 (eql 'declare (caar lst))))
       (values (nreverse decls) lst))
    (push (car lst) decls)))

;; -----------------------------------------------------------------

(defmacro! defwrapper (name args &body body)
  ;; defines a macro that encapsulates its &body arg into a lambda and
  ;; calls a do-it routine with the lambda function. The do-it routine
  ;; contains the bulk of the the wrapper code. And the macro
  ;; generated wrapped code contains only a function call to the doit
  ;; routine.
  (let ((fn-name   (intern (symbol-name g!fn-name)))
        (body-name (intern (symbol-name g!body))))
    `(progn
       (defun ,fn-name (,@args ,g!fn)
         ,@(subst `(funcall ,g!fn ,@args) '&body body))
       (defmacro ,name ((,@args) &body ,body-name)
         `(,',fn-name ,,@args (lambda (,,@args) ,@,body-name))) )))

#|
  ;; example
(defwrapper with-wrapped-thingy (a b c)
  (let ((ans (startup-fn a)))
    (unwind-protect
        &body ;; <-- this is where a funcall will be placed
      (shutdown ans b c))))
==>
(PROGN
  (DEFUN FN-NAME7520 (A B C #1=#:FN7519)
    (LET ((ANS (STARTUP-FN A))) (UNWIND-PROTECT (FUNCALL #1# . #2=(A B C)) (SHUTDOWN ANS B C))))
  (DEFMACRO WITH-WRAPPED-THINGY (#2# &BODY BODY7521)
    `(FN-NAME7520 ,A ,B ,C (LAMBDA (,A ,B ,C) ,@BODY7521))))

(with-wrapped-thingy (x y z)
    (doit x y z))
==>
(#:FN-NAME32008 X Y Z (LAMBDA (X Y Z) (DOIT X Y Z)))
|#

;; macro to modify places and then restore them around the body
;; the places and transient values are listed as with a let binding list
;; where the car of each binding names the place and the cadr is the transient value
;; e.g., (with-transient-mutation ((place1 val1) (place2 val2)) .... body ...)
;;
(defmacro with-transient-mutation (bindings &body body)
  (let* ((savs (mapcar (lambda (binding)
                         (declare (ignore binding))
                         (gensym))
                       bindings)))
    `(let* ,(mapcar (lambda (sav binding)
                      `(,sav  (shiftf ,(car binding) ,(cadr binding))))
                    savs bindings)
       (unwind-protect
           (progn
             ,@body)
         ,@(mapcar (lambda (sav binding)
                     `(setf ,(car binding) ,sav))
                   savs bindings))
       )))


(defun starts-with (list symbol)
  (and (consp list)
       (eql (first list) symbol)))

(defun symbol-name= (sym1 sym2)
  (and (symbolp sym1)
       (string= sym1 sym2)))

(defun symbol-name-equal (sym1 sym2)
  (and (symbolp sym1)
       (string-equal sym1 sym2)))

(defmacro! xcond (&rest clauses)
  ;; XCOND - COND extended with Scheme abilities
  (when clauses
      (destructuring-bind (hd &rest tl) clauses
        (cond ((single hd)
               `(or ,hd (xcond ,@tl)))
              
              ((member (first hd) '(t else otherwise)
                       :test 'symbol-name-equal)
               `(progn ,@(rest (first clauses))))

              ((symbol-name= (second hd) '=>)
               (assert (= (length hd) 3))
               `(if-let (,g!var ,(first hd))
                    (,(third hd) ,g!var)
                  ;; else
                  (xcond ,@tl)))

              (t `(if ,(first hd)
                      (progn ,@(rest hd))
                    ;; else
                    (xcond ,@tl)))
              ))))

;; -------------------------------------------------------

(defmacro row-type (name &optional slots)
  `(um:eval-always
     (defstruct ,name
       ,@slots)))

(defmacro sum-type (name terms)
  (let ((var (gensym)))
    `(um:eval-always
       ,@(mapcar (lambda (term)
                   (if (consp term)
                       `(row-type ,(car term) ,(cadr term))
                     `(row-type ,term)))
                 terms)
       (deftype ,name ()
                '(or ,@(mapcar (lambda (term)
                                 (if (consp term)
                                     (car term)
                                   term))
                               terms)))
       (defun ,(intern (mkstr (string name) "-P")) (,var)
         (typep ,var ',name))
       )))

#|
(um:sum-type tree (empty
                   (node (l v r h))))
(sum-type basic-thing (integer float string character nil))
 |#

;; -----------------------------------------

(defvar *match-case-sensitive-p* t)

(defun string-compare= (s1 s2)
  ;; should only be called when it is known
  ;; that both s1 and s2 are strings
  (declare (type string s1 s2))
  #f
  (if *match-case-sensitive-p*
      (string= s1 s2)
    (string-equal s1 s2)))

(defun char-compare= (c1 c2)
  ;; should only be called when it is known
  ;; that both c1 and c2 are characters
  (declare (type character c1 c2))
  #f
  (if *match-case-sensitive-p*
      (char= c1 c2)
    (char-equal c1 c2)))

;; -----------------------------------------

(defun string-compare< (s1 s2)
  ;; should only be called when it is known
  ;; that both s1 and s2 are strings
  (declare (type string s1 s2))
  #f
  (if *match-case-sensitive-p*
      (string< s1 s2)
    (string-lessp s1 s2)))

(defun char-compare< (c1 c2)
  ;; should only be called when it is known
  ;; that both c1 and c2 are characters
  (declare (type character c1 c2))
  #f
  (if *match-case-sensitive-p*
      (char< c1 c2)
    (char-lessp c1 c2)))

;; -----------------------------------------

(defun string-compare<= (s1 s2)
  ;; should only be called when it is known
  ;; that both s1 and s2 are strings
  (declare (type string s1 s2))
  #f
  (if *match-case-sensitive-p*
      (string<= s1 s2)
    (string-not-greaterp s1 s2)))

(defun char-compare<= (c1 c2)
  ;; should only be called when it is known
  ;; that both c1 and c2 are characters
  (declare (type character c1 c2))
  #f
  (if *match-case-sensitive-p*
      (char<= c1 c2)
    (char-not-greaterp c1 c2)))

;; -----------------------------------------


(defun eql-tree (v1 v2)
  #f
  (cond ((stringp v1)
         (and (stringp v2)
              (funcall 'string-compare=
                       (the string v1) (the string v2)) ))

        ((characterp v1)
         (and (characterp v2)
              (funcall 'char-compare=
                       (the character v1) (the character v2)) ))
        
        ((numberp v1)
         (and (numberp v2)
              (= (the number v1) (the number v2))))

        ((symbolp v1)
         (and (symbolp v2)
              (eq v1 v2)))

        ((consp v1)
         (and (consp v2)
              (eql-tree (first (the cons v1)) (first (the cons v2)))
              (eql-tree (rest  (the cons v1)) (rest  (the cons v2)))))

        ((vectorp v1)
         (and (vectorp v2)
              (= (the fixnum (length (the vector v1)))
                 (the fixnum (length (the vector v2))))
              (every 'eql-tree v1 v2)))

        ((arrayp v1)
         (and (arrayp v2)
              (equal (the cons (array-dimensions (the array v1)))
                     (the cons (array-dimensions (the array v2))))
              (block check-items
                (dotimes (ix (the fixnum (array-total-size (the array v1))))
                  (declare (fixnum ix))
                  (unless (eql-tree (row-major-aref (the array v1) ix)
                                    (row-major-aref (the array v2) ix))
                    (return-from check-items nil)))
                t)))

        (t (eql v1 v2))
        ))

;; ------------------------------------------------------------

(defun magic-word (str)
  ;; used by various file schemes -- a 4-byte integer
  ;; derived from a 4 character string...
  ;;
  ;; e.g., (magic-word "SDLE") => #x53444c45
  (do ((val 0)
       (ix  0 (1+ ix)))
      ((>= ix 4) val)
    (setf val (+ (ash val 8) (char-code (char str ix))))))

#|
(defun rot-bits (val nbits nrot &key (start 0))
  ;; nbits is the width beginning with start from LSB
  ;; nrot is the number of bit-rotates to perform
  ;;   direction (+/- nrot) in same sense as ash: + = rotl, - = rotr
  (let* ((mask    (ash (1- (ash 1 nbits)) start))
         (bits    (logand val mask))
         (topbits (logxor val bits))
         (nlsh    (mod nrot nbits))
         (hibits  (logand mask (ash bits nlsh)))
         (nrsh    (- nlsh nbits))
         (lobits  (logand mask (ash bits nrsh))))
    (logior topbits hibits lobits)
    ))
|#

(defun rot-bits (val bytespec nrot)
  ;; (ROT-BITS val (BYTE width start) nrot)
  ;; ... a BYTEspec is a cons cell (width . start)
  (let* ((start   (cdr bytespec))
         (nbits   (car bytespec))
         (mask    (ash (1- (ash 1 nbits)) start))
         (bits    (logand val mask))
         (topbits (logxor val bits))
         (nlsh    (mod nrot nbits))
         (hibits  (logand mask (ash bits nlsh)))
         (nrsh    (- nlsh nbits))
         (lobits  (logand mask (ash bits nrsh))))
    (logior topbits hibits lobits)
    ))

;; --------------------------------------------

#+:LISPWORKS
(defun int32-cnot (x)
  (declare (optimize (float 0)))
  (declare (type (signed-byte 32) x))
  (sys:int32-to-integer
   (sys:int32-logxor x
                     (sys:int32>> x 31))))

#+:LISPWORKS
(defun int32-rot (x n)
  (declare (optimize (float 0)))
  (declare (type (signed-byte 32) x n))
  (let ((nn (if (minusp n)
                (+ 32 n)
              n)))
    (declare (type (signed-byte 32) nn))
    (sys:int32-to-integer
     (sys:int32-logxor
      (sys:int32>> x (sys:int32- 32 nn))
      (sys:int32<< (sys:int32-logxor x
                                     (sys:int32>> x 31))
                   nn))
     )))

;; -----------------------------------------------------------


;; -----------------------------------------------------------------------------

(defun chktype (val type)
  (unless (typep val type)
    (error "~A expected (got ~A)" type val))
  val)

(defmacro chkarg ((arg type &optional (coerce-fn 'identity)) &body body)
  `(let ((,arg (chktype (funcall ',coerce-fn ,arg) ',type)))
     (declare (type ,type ,arg))
     ,@body))

(defmacro chkargs (arglist &body body)
  (if arglist
      `(chkarg ,(first arglist) (chkargs ,(rest arglist) ,@body))
    `(progn
       ,@body)))

#|
(chkargs ((a integer) (b float)) (doit toit))
 |#

(defun dfloat (x)
  (if (realp x)
      (coerce x 'double-float)
    (error "Real number expected (got ~A)"  x)))

;; ----------------------------------------------------------

#+:LISPWORKS
(defun stack-probe ()
  (let ((cell (vector)))
    (declare (dynamic-extent cell))
    (format t "~%Stack: ~X" (sys:object-address cell))))


;; -----------------------------------------------

(defun featurep (feature-expression)
  "Returns T if the argument matches the state of the *FEATURES*
list and NIL if it does not. FEATURE-EXPRESSION can be any atom
or list acceptable to the reader macros #+ and #-."
  (etypecase feature-expression
    (symbol (not (null (member feature-expression *features*))))
    (cons (check-type (first feature-expression) symbol)
          (ecase (first feature-expression)
            (and  (every 'featurep (rest feature-expression)))
            (or   (some 'featurep (rest feature-expression)))
            (not  (assert (= 2 (length feature-expression)))
                  (not (featurep (second feature-expression))))))))


;; -----------------------------------------------

(defmacro single-eval ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym n))))
    `(with-gensyms (,@gensyms)
      `(let (,,@(loop for g in gensyms
                      for n in names
                      collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names
                       for g in gensyms
                       collect `(,n ,g)))
           ,@body)))))

#|
(defmacro square (x)
  (single-eval (x)
    `(* ,x ,x)))
|#

;; -----------------------------------------------

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

;; --------------------------------------------------------
;; utilities...

(defmacro nest (&rest r)
  (reduce (lambda (o i)
            `(,@o ,i))
          r
          :from-end t))

(defun fst (a b)
  (declare (ignore b))
  a)

(defun snd (a b)
  (declare (ignore a))
  b)

;; --------------------------------------------------------------------------------

(defmacro gathering (&body body)
  "Run `body` to gather some things and return a fresh list of them.

  `body` will be executed with the symbol `gather` bound to a
  function of one argument.  Once `body` has finished, a list of
  everything `gather` was called on will be returned.

  It's handy for pulling results out of code that executes
  procedurally and doesn't return anything, like `maphash` or
  Alexandria's `map-permutations`.

  The `gather` function can be passed to other functions, but should
  not be retained once the `gathering` form has returned (it would
  be useless to do so anyway).

  Examples:

    (gathering
      (dotimes (i 5)
        (gather i))
    =>
    (0 1 2 3 4)

    (gathering
      (mapc #'gather '(1 2 3))
      (mapc #'gather '(a b)))
    =>
    (1 2 3 a b)

  "
  (let ((result (gensym)))
    `(let ((,result nil))
       (flet ((gather (item)
                (push item ,result)
                item))
         ,@body)
       (nreverse ,result))))

;; --------------------------------------------------------------------
;; Arrow -> - useful for chaining together functions

#|
(defmacro -> (form &optional next-form &rest other-forms)
  ;; others should be a list of forms whose function will take the val
  ;; as its first argument. Transforms to constant stack depth eval.
  (cond ((consp next-form)
         (destructuring-bind (fn . args) next-form
           (if (consp form)
               (let ((g!arg (gensym)))
                 `(let ((,g!arg ,form))
                    (-> (,fn ,g!arg ,@args) ,@other-forms)))
             `(-> (,fn ,form ,@args) ,@other-forms)
             )))
        
        ((null next-form)
         form)
        
        (t
         ;; acts like next-form was CONSTANTLY function form
         `(-> ,next-form ,@other-forms))
        ))

(defmacro _> ((sym) form &optional next-form &rest other-forms)
  ;; similar in spirit to ->, but allows for arbitrary placement of substitution
  (cond (next-form
         `(let ((,sym ,form))
            (_> (,sym) ,next-form ,@other-forms)))
        
        ((null next-form)
         form)
        ))
|#
#|
(-> start-val
    (dosomething arg1 arg2)
    (dosomething arg3 arg4))
=>
(LET ((#:G49408 (DOSOMETHING START-VAL ARG1 ARG2)))
  (DOSOMETHING #:G49408 ARG3 ARG4))

(-> (start-fn arg0)
    (dosomething arg1 arg2)
    (dosomething arg3 arg4))
=>
(LET ((#:G49412 (START-FN ARG0)))
  (LET ((#:G49416 (DOSOMETHING #:G49412 ARG1 ARG2)))
    (DOSOMETHING #:G49416 ARG3 ARG4)))

(-> 15)

(-> (maps:empty)
    (maps:add :one 1)
    (maps:add :two 2)
    (maps:add :three 3))
=>
(LET ((#:G49420 (SETS:EMPTY)))
  (LET ((#:G49424 (MAPS:ADD #:G49420 :ONE 1)))
    (LET ((#:G49428 (MAPS:ADD #:G49424 :TWO 2)))
      (MAPS:ADD #:G49428 :THREE 3))))

(_> (_)
    start-val
    (dosomething _ arg1 _ arg2)
    (dosomething _ arg3 arg4))
=>
(LET ((_ START-VAL))
  (LET ((_ (DOSOMETHING _ ARG1 _ ARG2)))
    (DOSOMETHING _ ARG3 ARG4)))
 |#

(defun read-mailbox-with-timeout (mbox &key timeout errorp on-timeout)
  (multiple-value-bind (ans ok)
      (mp:mailbox-read mbox nil timeout)
    (if ok
        (values ans t)
      (if errorp
          (error 'timeout:timeout)
        (when on-timeout
          (funcall on-timeout))
        ))))

;; ----------------------------------------------------

#||#
(defmacro ->> (arg &rest fn-forms)
  (if fn-forms
      `(->> (funcall ,(car fn-forms) ,arg) ,@(cdr fn-forms))
    arg))
#||#

#|
(defmacro ->> (args &rest fn-forms)
  (labels ((helper (arg &optional fn-form &rest fn-forms)
             (if fn-form
                 (if fn-forms
                     (apply #'helper `(multiple-value-call ,fn-form ,arg) fn-forms)
                   `(multiple-value-call ,fn-form ,arg))
               arg)))
    (apply #'helper `(values ,@args) fn-forms)))
|#
#|
(->> (x)
     (um:rcurry #'doit :yes)
     (um:rcurry #'didit :no)
     (um:rcurry #'mapcar #'1+))

(->> (x)
     #'identity)
(->> (x))
|#
;; -------------------------------------------------------------

(defmacro doseq ((var coll &optional ret-form) &body body)
  ;; mimic syntax of DOLIST
  `(progn
     (map nil (lambda (,var)
                ,@body)
          ,coll)
     ,ret-form))

(defmacro vbind (syms vec &body body)
  (let ((gvec (gensym)))
    `(let ((,gvec ,vec))
       ,@(nlet iter ((syms syms)
                     (pos  0))
           (if (endp syms)
               body
             `((let ((,(car syms) (aref ,gvec ,pos)))
                 ,@(iter (cdr syms) (1+ pos))))
             ))
       )))

(defmacro vbind* (syms vec &body body)
  (let ((gvec (gensym)))
    `(let ((,gvec ,vec))
       ,@(nlet iter ((syms syms)
                     (pos  0))
           (if (endp syms)
               body
             `((symbol-macrolet ((,(car syms) (aref ,gvec ,pos)))
                 ,@(iter (cdr syms) (1+ pos))))
             ))
       )))

(defmacro with-velems (bindings vec &body body)
  (let ((gvec (gensym)))
    `(let ((,gvec ,vec))
       (symbol-macrolet ,(mapcar #`(,(car a1) (aref ,gvec ,(cadr a1))) bindings)
         ,@body))
    ))
       
;; -------------------------------------------------

(defun within (low ix hi)
  ;; test for ix in half-open interval [low, hi)
  (and (<= low ix)
       (< ix hi)))


