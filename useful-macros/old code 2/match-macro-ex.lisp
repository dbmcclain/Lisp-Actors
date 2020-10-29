;; match-macro-ex.lisp -- Compiled Pattern Matching
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

;; -----------------------------------------------------------

;; -----------------------------------------------------------------
;; row-type encodes a #T(TypeName :SLOT-NAME Pat :SLOT-NAME2 Pat2 .. ) template
;; This permits matching against selected slots, listed explicitly in the template
;; against structure slots of type TypeName, igoring those slots of the value struct
;; that haven't been mentioned in the template.
;;
;; NOTE: we can also match against #S(Typename ...), but that expects to match
;; against every known slot of the struct type. Those not explicitly mentioned in the
;; #S() pattern receive their default values. So this isn't quite as convenient as the #T()
;; template types. Every slot of a #S() pattern should be listed. Only those slots of interest
;; need to be listed in a #T() template pattern.
;;

#+(OR :LISPWORKS :CLOZURE :ALLEGRO)
(defstruct row-type
  type slots pats)

(defun quoted-symbol-p (arg)
  (and (consp arg)
       (eq 'QUOTE (first arg))))

(defun function-name-for-symbol (sym)
  (cond ((keywordp sym)
         (intern (symbol-name sym)))

        ((symbolp sym) sym)

        ((stringp sym)
         (intern sym))

        ((quoted-symbol-p sym)
         (function-name-for-symbol (second sym)))

        (t (error "Can't find a function with name: ~S" sym))
        ))

(defun key-symbol (key)
  (if (quoted-symbol-p key)
      (second key)
    key))

#+(OR :LISPWORKS :CLOZURE :ALLEGRO)
(defun |reader-for-#T| (s c n)
  ;; read in the template and pre-process to select out struct type,
  ;; slot-names, and slot patterns
  (declare (ignore c n))
  (let* ((lst   (read s t nil t))
         (type  (car lst))
         (plist (cdr lst))
         (pairs (um:group plist 2))
         (keys  (mapcar 'first pairs))
         (pats  (mapcar 'second pairs))
         (class (find-class type nil)))

    (handler-case
        (when (subtypep type t) ;; is it a type?
          (let* ((all-names
                  (cond ((or (null class)
                             (typep class 'built-in-class)) nil)
                        
                        (t (slot-names class))
                        ))
                 (slots (mapcar #'(lambda (key)
                                    (let ((slot (find (key-symbol key) all-names
                                                      :test 'string=)))
                                      (if slot
                                          (list slot :slot)
                                        (let ((name-sym (function-name-for-symbol key)))
                                          (list (and (fboundp name-sym)
                                                     name-sym)
                                                :function))
                                        )))
                                keys)))
            
            (make-row-type
             :type     type
             :slots    slots
             :pats     pats) ))

      (error (err)
        ;; avoid bombing out on non-type unless we are compiling to
        ;; file. It will bomb later if attempted match.
        (if *compile-file-pathname*
            (error err)
          (make-row-type
           :type  type
           :pats  pats)))
      )))

#+(OR :LISPWORKS :CLOZURE :ALLEGRO)
(set-dispatch-macro-character #\# #\T '|reader-for-#T|)

#+(OR :CLOZURE :ALLEGRO)
(defmethod make-load-form ((obj row-type) &optional environment)
  (make-load-form-saving-slots obj :environment environment))
  

;; --------------------------------------------------------

(defun is-match-any-and-ignore-symbol? (sym)
  (and (symbolp sym)
       (string= "_" sym)))

;; -----------------------------------------

(defun in-range (v from &optional to)
  (cond ((or (realp from) (realp to))
         (and (realp v)
              (or (null from)
                  (<= from v))
              (or (null to)
                  (< v to))))

        ((or (stringp from) (stringp to))
         (and (stringp v)
              (or (null from)
                  (funcall 'string-compare<= from v))
              (or (null to)
                  (funcall 'string-compare< v to))))

        ((or (characterp from) (characterp to))
         (and (characterp v)
              (or (null from)
                  (funcall 'char-compare<= from v))
              (or (null to)
                  (funcall 'char-compare< v to))))

        ((or (symbolp from) (symbolp to))
         (and (symbolp v)
              (or (null from)
                  (string<= (string from) (string v)))
              (or (null to)
                  (string< (string v) (string to)))))

        (t nil)))

;; -----------------------------------------

#|
(define-compiler-macro eql-tree (&whole form v1 v2)
  (cond ((stringp v1)
         (if (stringp v2)
             (or (string= v1 v2)
                 (if (string-equal v1 v2)
                     `(not *match-case-sensitive-p*)
                   `(string-compare= (the string ,v1) (the string ,v2)) ))
           `(and (stringp ,v2)
                 (string-compare= (the string ,v1) (the string ,v2))) ))

        ((stringp v2)
         `(and (stringp ,v1)
               (string-compare= (the string ,v1) (the string ,v2))))

        ((characterp v1)
         (if (characterp v2)
             (or (char= v1 v2)
                 (if (char-equal v1 v2)
                     `(not *match-case-sensitive-p*)
                   `(char-compare= (the character ,v1) (the character ,v2))))
           `(and (characterp ,v2)
                 (char-compare= (the character ,v1) (the character ,v2))) ))
        
        ((characterp v2)
         `(and (characterp ,v1)
               (char-compare= (the character ,v1) (the character ,v2))))

        ((numberp v1)
         (if (numberp v2)
             (= v1 v2)
           `(and (numberp ,v2)
                 (= (the number ,v1) (the number ,v2))) ))

        ((numberp v2)
         `(and (numberp ,v1)
               (= (the number ,v1) (the number ,v2))))

        ((symbolp v1)
         (if (symbolp v2)
             (eq v1 v2)
           `(eq ,v1 ,v2)))

        ((symbolp v2)
         `(eq ,v1 ,v2))
        
        (t form)))
|#
        
;; -----------------------------------------

(defun get-clause-args (pattern)
  (let ((bindings nil))
    (nlet iter ((pattern pattern))
      (cond
       
       ;; put row-type pattern matches first, since we seem to be using them
       ;; a lot...
       #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
       ((row-type-p pattern)
        ;; #T() template patterns
        (dolist (pat (row-type-pats pattern))
          (iter pat)))
       
       #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
       ((typep pattern 'structure-object)
        ;; #S() structure patterns
        (dolist (slot #+:LISPWORKS (structure:structure-class-slot-names
                                    (class-of pattern))
                      #+:CLOZURE   (ccl:class-slots (class-of pattern)))
          (iter (slot-value pattern slot))))

       
       ((stringp pattern))
       
       ((vectorp pattern) ;; here to avoid constantp
        (dotimes (ix (length pattern))
          (iter (aref pattern ix))))

       ((arrayp pattern)
        (dotimes (ix (array-total-size pattern))
          (iter (row-major-aref pattern ix))))

       ;; check for _ here in case it has been defined as a global const somewhere else
       ((is-match-any-and-ignore-symbol? pattern))
       
       ;; constant patterns, including quoted items
       ((constantp pattern))

       ;; binding patterns
       ((symbolp pattern)
        (pushnew pattern bindings))
       
       ;; recursive patterns
       ((consp pattern)
        (destructuring-bind (pat-hd . pat-tl) pattern
          
          ;; AS patterns
          (cond
           ((eq :AS pat-hd)
            (iter (first pat-tl))
            (iter (second pat-tl)))

           ((eq :MEMBER pat-hd))
           ((eq :RANGE  pat-hd))
           ((eq :TYPE   pat-hd))
            
           ((eq '&REST pat-hd)
            (iter (first pat-tl)))
           
           (t
            ;; list patterns
            (iter pat-hd)
            (iter pat-tl))
           )))
       
       ;; huh??
       (t (error "Invalid pattern: ~S" pattern))
       
       ))
    (nreverse bindings)))
  
;; -----------------------------------------

(defun rt-match (val pattern)
  ;; a runtime version of match for when patterns are passed as args
  (let ((bindings nil))
    (nlet iter ((val     val)
                (pattern pattern))
      (cond
       
       ;; put row-type pattern matches first, since we seem to be using them
       ;; a lot...
       #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
       ((row-type-p pattern)
        ;; #T() template patterns
        (when (typep val (row-type-type pattern))
          (nlet iter-t ((slots  (row-type-slots pattern))
                        (pats   (row-type-pats pattern)))
            (or (endp slots)
                (bind*
                    ((slot   (car slots))
                     (pat    (car pats)))
                  (let ((new-val (ecase (second slot)
                                   (:function (funcall (first slot) val))
                                   (:slot     (slot-value val (first slot)))) ))
                    (and (iter new-val pat)
                         (iter-t (cdr slots) (cdr pats))) ))))))
                       
       #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
       ((typep pattern 'structure-object)
        ;; #S() structure patterns
        (when (typep val (type-of pattern))
          (nlet iter-s ((slots #+:LISPWORKS (structure:structure-class-slot-names
                                             (class-of pattern))
                               #+:CLOZURE   (ccl:class-slots (class-of pattern))))
            (or (endp slots)
                (um:bind*
                    ((slot (car slots)))
                  (let ((new-val (slot-value val slot)))
                    (and (iter new-val (slot-value pattern slot))
                         (iter-s (cdr slots))) ))))))
    
       ((stringp pattern)
        (and (stringp val)
             (string-compare= (the string val) pattern)))
     
       ((vectorp pattern)
        (and (vectorp val)
             (= (length val) (length pattern))
             (nlet iter-v ((ix 0))
               (or (>= ix (length pattern))
                   (let ((elt (aref val ix)))
                     (and (iter elt (aref pattern ix))
                          (iter-v (1+ ix)))))) ))

       ((arrayp pattern)
        (and (arrayp val)
             (equal (array-dimensions val) (array-dimensions pattern))
             (nlet iter-a ((ix 0))
               (or (>= ix (array-total-size pattern))
                   (let ((elt (row-major-aref val ix)))
                     (and (iter elt (row-major-aref pattern ix))
                          (iter-a (1+ ix))))) )))
     
       ((is-match-any-and-ignore-symbol? pattern) t)
     
       ;; constant patterns (sort of...)
       ((null pattern)
        (null val))
     
       ((eq 't pattern)
        (not (null val)))
     
       ((keywordp pattern)
        (eq val pattern))
     
       ((numberp pattern)
        (and (numberp val)
             (= (the number val) pattern)))
     
       ((characterp pattern)
        (and (characterp val)
             (char-compare= (the character val) pattern)))

       ((constantp pattern) ;; includes quoted patterns and quoted symbols
        (if (and (consp pattern)
                 (eq 'QUOTE (car pattern))
                 (symbolp (cadr pattern)))
            (eq val pattern)
          ;; else
          (eql-tree val pattern)))
     
       ;; binding patterns
       ((symbolp pattern)
        (let ((prev (assoc pattern bindings)))
          (if prev
              (eql-tree val (cdr prev))
            (push (cons pattern val) bindings))))
      
       ;; recursive patterns
       ((consp pattern)
        (um:bind*
            (((pat-hd &rest pat-tl) pattern))

          ;; AS patterns
          (cond
           ((eq :AS pat-hd)
            (um:bind*
                (((pat1 pat2) pat-tl))
              (and (iter val pat1)
                   (iter val pat2))))

           ((eq :TYPE pat-hd)
            (typep val (car pat-tl)))

           ((eq :MEMBER pat-hd)
            (apply 'member val pat-tl))

           ((eq :RANGE pat-hd)
            (apply 'in-range val pat-tl))

           ((eq '&REST pat-hd)
            (iter val (first pat-tl)))

           (t
            ;; list patterns
            (and (consp val)
                 (let ((hd (car (the cons val))))
                   (and (iter hd pat-hd)
                        (let ((tl (cdr (the cons val))))
                          (iter tl pat-tl))) ))) )))

       ;; huh??
       (t (error "Can't happen in rt-match"))
     
       ))))

(defun match-1ex (val pattern k-then)
  (let ((bindings nil))
    (nlet iter ((val     val)
                (pattern pattern)
                (k-then  k-then))
      (cond
       
       ;; put row-type pattern matches first, since we seem to be using them
       ;; a lot...
       #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
       ((row-type-p pattern)
        ;; #T() template patterns
        `(when (typep ,val ',(row-type-type pattern))
           ,(nlet iter-t ((slots  (row-type-slots pattern))
                          (pats   (row-type-pats pattern)))
              (if (endp slots)
                  (funcall k-then)
                (bind*
                    ((slot   (car slots))
                     (pat    (car pats))
                     (gslot  (gensym "SLOT")))
                  `(let ((,gslot ,(ecase (second slot)
                                    (:function `(funcall ',(first slot) ,val))
                                    (:slot     `(slot-value ,val ',(first slot)))) ))
                     ,(iter gslot pat
                            (lambda ()
                              (iter-t (cdr slots) (cdr pats)))))
                  ))) ))
                       
       #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
       ((typep pattern 'structure-object)
        ;; #S() structure patterns
        `(when (typep ,val ',(type-of pattern))
           ,(nlet iter-s ((slots #+:LISPWORKS (structure:structure-class-slot-names
                                               (class-of pattern))
                                 #+:CLOZURE   (ccl:class-slots (class-of pattern))))
              (if (endp slots)
                  (funcall k-then)
              
                (um:bind*
                    ((slot (car slots))
                     (gslot (gensym "SLOT")))
                  `(let ((,gslot (slot-value ,val ',slot)))
                     ,(iter gslot (slot-value pattern slot)
                            (lambda ()
                              (iter-s (cdr slots)))
                            ))))) ))
    
       ((stringp pattern)
        `(when (and (stringp ,val)
                    (string-compare= (the string ,val) ,pattern))
           ,(funcall k-then)) )
     
       ((vectorp pattern)
        `(when (and (vectorp ,val)
                    (= (length ,val) ,(length pattern)))
           (declare ((array (,(length pattern))) ,val))
           ,(nlet iter-v ((ix 0))
              (if (>= ix (length pattern))
                  (funcall k-then)
                (let ((gval (gensym "ELT")))
                  `(let ((,gval (aref ,val ,ix)))
                     ,(iter gval (aref pattern ix)
                            (lambda ()
                              (iter-v (1+ ix))))
                     )) ))) )

       ((arrayp pattern)
        `(when (and (arrayp ,val)
                    (equal (array-dimensions ,val) ',(array-dimensions pattern)))
           (declare ((array (,(array-dimensions pattern))) ,val))
           ,(nlet iter-a ((ix 0))
              (if (>= ix (array-total-size pattern))
                  (funcall k-then)
                (let ((gval (gensym "ELT")))
                  `(let ((,gval (row-major-aref ,val ,ix)))
                     ,(iter gval (row-major-aref pattern ix)
                            (lambda ()
                              (iter-a (1+ ix)))))
                  )))))
     
       ((is-match-any-and-ignore-symbol? pattern)
        (funcall k-then))
     
       ;; constant patterns (sort of...)
       ((null pattern)
        `(unless ,val
           ,(funcall k-then)) )
     
       ((eq 't pattern)
        `(when ,val
           ,(funcall k-then)) )
     
       ((keywordp pattern)
        `(when (eq ,val ,pattern)
           ,(funcall k-then)) )
     
       ((numberp pattern)
        `(when (and (numberp ,val)
                    (= (the number ,val) ,pattern))
           ,(funcall k-then)) )
     
       ((characterp pattern)
        `(when (and (characterp ,val)
                    (char-compare= (the character ,val) ,pattern))
           ,(funcall k-then)) )

       ((constantp pattern) ;; includes quoted patterns and quoted symbols
        (if (and (consp pattern)
                 (eq 'QUOTE (car pattern))
                 (symbolp (cadr pattern)))
            `(when (eq ,val ,pattern)
               ,(funcall k-then))
          ;; else
          `(when (eql-tree ,val ,pattern)
             ,(funcall k-then))))
     
       ;; binding patterns
       ((symbolp pattern)
        (if (member pattern bindings)
            `(when (eql-tree ,val ,pattern)
               ,(funcall k-then))
          (progn
            (push pattern bindings)
            `(let ((,pattern ,val))
               ,(funcall k-then)))
          ))
     
       ;; recursive patterns
       ((consp pattern)
        (um:bind*
            (((pat-hd &rest pat-tl) pattern))

          ;; AS patterns
          (cond
           ((eq :AS pat-hd)
            (um:bind*
                (((pat1 pat2) pat-tl))
              (iter val pat1 (lambda ()
                               (iter val pat2 k-then)))
              ))

           ((eq :TYPE pat-hd)
            `(when (typep ,val ,(cadr pattern))
               ,(funcall k-then)))

           ((eq :MEMBER pat-hd)
            `(when (member ,val ,@pat-tl) ;; allow for key, test kws
               ,(funcall k-then)))

           ((eq :RANGE pat-hd)
            `(when (in-range ,val ,@pat-tl) ;; allow for from, to
               ,(funcall k-then)))
         
           ((eq '&REST pat-hd)
            (iter val (first pat-tl) k-then) )

           (t
            ;; list patterns
            (let ((ghd (gensym "HD"))
                  (gtl (gensym "TL")))
              `(when (consp ,val)
                 (let ((,ghd (car (the cons ,val))))
                   ,(iter ghd pat-hd
                          (lambda ()
                            `(let ((,gtl (cdr (the cons ,val))))
                               ,(iter gtl pat-tl k-then))))
                   ))
              )) )))

       ;; huh??
       (t (error "Can't happen in match-1ex"))
     
       ))))

;; -----------------------------------------

#|
(defun optimize-clauses (clauses)
  clauses)
|#

(defun has-when-guard (clause)
  (eq :when (second clause)))

(defun get-clause-body (clause)
  (if (has-when-guard clause)
      (nthcdr 3 clause)
    (cdr clause)))

(defun encode-match-body (fn clause)
  (if (has-when-guard clause)
      `(,(first clause) :when ,(third clause) ,(funcall fn (nthcdr 3 clause)))
    `(,(first clause) ,(funcall fn (rest clause)))))
                                                         
(defun encode-match-bodies (fn clauses)
  (mapcar #`(,@(encode-match-body fn a1)) clauses))

(defun get-labeled-clause (lbl clause args)
  (encode-match-body #`(list ,lbl ,@args) clause))

(defun get-labeled-clauses (clauses)
  (mapcar #`(,@(let* ((lbl  (gensym "LBL"))
                      (body (get-clause-body a1))
                      (args (intersection
                             (get-clause-args (first a1))
                             (flatten body)))
                      (opts  #+:LISPWORKS (compiler::listify-optimization-level)
                             #+:ALLEGRO   (sys:declaration-information 'optimize)
                             ))
                 `((,lbl (lambda ,args
                           (declare (optimize ,@opts))
                           ,@body))
                  ,(get-labeled-clause lbl a1 args))
                 ))
          clauses))

(defun clause-generator (garg gblock)
  (lambda (clause)
    (match-1ex garg (first clause)
               (if (has-when-guard clause)
                   (lambda ()
                     `(when ,(third clause)
                        (return-from ,gblock
                          ,(fourth clause))))
                 (lambda ()
                   `(return-from ,gblock
                      ,(second clause))))
               )))
  
;; -----------------------------------------

(defmacro! match (arg &rest clauses)
  ;; by separating out the match clause bodies into labels here
  ;; we allow the user declared optimization levels to have effect
  ;; on the clause bodies, while still allowing for high performance
  ;; in the pattern matching.
  (let ((lbl-clauses (get-labeled-clauses clauses)))
    `(let ((,g!ans (symbol-macrolet ,(mapcar 'first lbl-clauses)
                     #f ;; go for raw speed
                     (block ,g!block
                       (let ((,g!arg ,arg))
                         (declare (dynamic-extent ,g!arg))
                         ,@(optimize-clauses
                            (mapcar (clause-generator g!arg g!block)
                                    (mapcar 'second lbl-clauses)))
                         (match-fail)) ))))
       (declare (cons ,g!ans))
       #f ;; go for raw speed
       (apply (car ,g!ans) (cdr ,g!ans)) )
    ))

(defmacro! match2 (arg1 arg2 &rest clauses)
  `(match (the cons (cons ,arg1 ,arg2))
     ,@(mapcar #`((,(first a1) . ,(second a1)) ,@(cddr a1)) clauses)))

;; -----------------------------------------

(define-condition match-failure (error)
  ()
  (:report "Match Failure"))

(defvar $match-failure-exn
  (make-condition 'match-failure))

(defun match-fail ()
  (error $match-failure-exn))

;; ------------------------------------------

#+:LISPWORKS
(progn
  (editor:setup-indent "match"    1 2)
  (editor:setup-indent "match2"   1 2))

;; ------------------------------------------

#|
(match val)
(defstruct diddly x y z)
(match val
  ((:one two (three . rest)) :when (eql three 3)
   (list two three rest))
  (#T(diddly :x 15 :y yval) yval))
|#
