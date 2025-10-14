#|
MIT License Terms:

Copyright (c) 2017, Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

(in-package :actors)

;; ------------------------------------------------------------------
;; CAUTION - Any code that makes use of parallel execution from inside
;; an Actor body code sets up a potential violation of the invariant
;; that Actor local state is mutable only by the thread executing the
;; Actor.
;;
;; You must take care not to allow mutation of Actor internal state
;; from both the Actor body code and the parallel forms which could be
;; executed by another thread concurrently with the Actor body code.
;;
;; In such cases, take care to use mutually exclusive mutation, using
;; e.g., RMW for atomic read-modify-write on such state bindings.
;;
;; The same precautions must be taken for any internal state bindings
;; that are handed out to ASK'ing clients.
;; -------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; The following code is specifically a part of Actors, but might help
;; when describing non-blocking Actor code...
;;
;; Be careful here... the use of parallel execution could lead to
;; shared access to local state, which might violate single-thread
;; semantics inside of Actor bodies.
;;
;; These macros also game the system by intentional capture of free
;; variable %SK (the current continuation). This is a lexical binding,
;; and must not be made a global special symbol.

;; ----------------------------------------------------------------------------
;; CPS Declarators

(defmacro =lambda (parms &body body)
  ;; define an anonymous CPS function
  `#'(lambda (%sk ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  ;; define a named CPS function
  (let* ((fn  (symb '= name))
         (macro-body (parse-and-assemble-args-from-lambda-list fn parms)))
    `(progn
       (defmacro ,name ,parms
         ,macro-body)
       (defun ,fn (%sk ,@parms) ,@body))))

(defmacro =defmethod (name parms &body body)
  ;; define a named CPS method
  (let ((fn  (symb '= name)))
    (cond ((fboundp fn)
           `(defmethod ,fn (%sk ,@parms) ,@body))
          (t
           (let ((macro-body (parse-and-assemble-args-from-lambda-list fn parms)))
             `(progn
                (defmacro ,name ,(method-macro-parms parms)
                  ,macro-body)
                (defmethod ,fn (%sk ,@parms) ,@body))))
          )))

(defmacro =flet (bindings &body body)
  (let* ((names  (mapcar 'first bindings))
         (args   (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (symb '= name))
                         names)))
    `(flet ,(mapcar (lambda (fn args body)
                      `(,fn (%sk ,@args) ,@body))
                    fns args bodies)
       (macrolet ,(mapcar (lambda (name parms fn)
                            `(,name ,args ,(parse-and-assemble-args-from-lambda-list fn parms)))
                          names args fns)
         ,@body))))

(defmacro =labels (bindings &body body)
  (let* ((names  (mapcar 'first bindings))
         (args   (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (symb '= name))
                         names)))
    `(macrolet ,(mapcar (lambda (name parms fn)
                          `(,name ,args ,(parse-and-assemble-args-from-lambda-list fn parms)))
                        names args fns)
       (labels ,(mapcar (lambda (fn args body)
                          `(,fn (%sk ,@args) ,@body))
                        fns args bodies)
         ,@body))))

#|
(defmacro =flet (bindings &body body)
  (let* ((names  (mapcar 'first bindings))
         (args   (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (symb '= name))
                         names)))
    `(flet ,(mapcar (lambda (fn args body)
                      `(,fn (%sk ,@args) ,@body))
                    fns args bodies)
       (macrolet ,(mapcar (lambda (name args fn)
                            (let ((lister (if (find '&rest args)
                                              'list*
                                            'list)))
                              `(,name ,args (,lister ',fn '%sk ,@(remove '&rest args)))
                              ))
                          names args fns)
         ,@body))))

(defmacro =labels (bindings &body body)
  (let* ((names  (mapcar 'first bindings))
         (args   (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (symb '= name))
                         names)))
    `(macrolet ,(mapcar (lambda (name args fn)
                          (let ((lister (if (find '&rest args)
                                            'list*
                                          'list)))
                            `(,name ,args (,lister ',fn '%sk ,@(remove '&rest args)))
                            ))
                        names args fns)
       (labels ,(mapcar (lambda (fn args body)
                          `(,fn (%sk ,@args) ,@body))
                        fns args bodies)
         ,@body))))
|#

#+:LISPWORKS
(editor:setup-indent "=flet" 1 nil nil 'flet)
#+:LISPWORKS
(editor:setup-indent "=labels" 1 nil nil 'flet)


(defmacro =bind (args expr &body body)
  `(do-bind (lambda (%sk)
              ,expr)
            (lambda (&optional ,@args &rest #1=#:ignored)
              (declare (ignore #1#))
              ,@body)))

(defun do-bind (exprfn contfn)
  (funcall exprfn (=cont contfn)))


(defmacro =values (&rest retvals)
  ;; invoke a continuation. This should generally be in tail position
  `(funcall %sk ,@retvals))

(defmacro =funcall (fn &rest args)
  ;; invoke a CPS function
  `(funcall ,fn %sk ,@args))

(defmacro =apply (fn &rest args)
  ;; invoke a CPS function
  `(apply ,fn %sk ,@args))

(defmacro =ask (obj &rest msg)
  `(par-ask ,obj %sk ,@msg))

(defmacro =exec (form)
  ;; must be used inside of =PAR-BIND
  `(do-exec %sk (lambda ()
                  ,form)))

(defun do-exec (cont-fn form-fn)
  (spawn (lambda ()
           (funcall cont-fn (capture-ans-or-exn form-fn)))
         ))

(defmacro =perform (actor &body body)
  `(=ask ,actor :perform (lambda ()
                           ,@body)))


(defmacro with-cont (&body body)
  ;; for REPL toplevel call to function defined with =defun
  `(let ((%sk #'values))
     ,@body))

(define-symbol-macro =values-callback %sk)

;; ---------------------------------------------------

(defun do-with-future (form-fn body-cont)
  (funcall form-fn (=cont (lambda (&optional ans)
                            (apply body-cont (multiple-value-list
                                              (recover-ans-or-exn ans)))))))

(defmacro with-future (args form &body body)
  `(do-with-future
    (lambda (%sk)
      ,form)
    (lambda (&optional ,@args &rest #1=#:ignored)
      (declare (ignore #1#))
      ,@body)))

(defmacro ensure-actor (&body body)
  ;; Some code that uses continuations needs to ensure that the
  ;; continuation is handled quickly, e.g., interrupt handlers.
  ;;
  ;; Continuations belonging to Actors are handled very quickly as a
  ;; send of a continuation message to the Actor so that the
  ;; continuation is guaranteed to execute in the single-threaded
  ;; context of the Actor.
  ;;
  ;; But code which is not contained in a running Actor just has the
  ;; continuation turned into a function that can take arbitrary
  ;; execution time.
  ;;
  ;; So this wrapper macro ensures that the contained code will be
  ;; executed from inside of a running Actor.
  `(ensure-actor-execution (lambda ()
                             ,@body)))

(defun ensure-actor-execution (fn)
  (if (current-actor)
      (funcall fn)
    (spawn fn)))

(defun do-with-timeout-future (timeout timeout-cont form-fn body-cont)
  ;; this handles the timeout timing in a non-blocking manner,
  ;; contrasted with:
  ;;
  ;;    (spawn (lambda ()
  ;;              (sleep timeout)))
  ;;
  ;; which would invoke an Actor on a thread and then block that
  ;; thread for the duration of the sleep.
  ;;
  (let* ((timed-out   nil)
         (timeout-fn  (=cont (or timeout-cont
                                 (lambda ()
                                   (error "Timeout")))))
         (timer       (make-timer (lambda ()
                                    (setf timed-out t)
                                    (mp:funcall-async timeout-fn)))))
         
    (schedule-timer-relative timer timeout)
    (funcall form-fn (=cont (lambda (ans)
                              (unschedule-timer timer)
                              (unless timed-out
                                (apply body-cont (multiple-value-list
                                                  (recover-ans-or-exn ans)))))
                            ))
    ))

(defmacro with-timeout-future (args (timeout &rest on-timeout) form &body body)
  `(do-with-timeout-future ,timeout
                           ,(when on-timeout
                              `(lambda ()
                                 ,@on-timeout))
                           (lambda (%sk)
                             ,form)
                           (lambda (&optional ,@args &rest #1=#:ignored)
                             (declare (ignore #1#))
                             ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-future"  2)

;; ------------------------------------------------------------------------

(defmethod par-ask ((actor actor) kcont &rest msg)
  ;; In making a parallel ask, there are questions about who will
  ;; handle the ask, and who will handle the reply?  The asking could
  ;; generate an error, so who should handle the error?
  ;;
  ;; We are clearly asking the indicated Actor to handle the query.
  ;;
  ;; Surrounding the query is capture-ans-or-exn which packages up the
  ;; reply, or an error, for transmission back to the query
  ;; originator.
  ;;
  ;; And for Actors asking Actors it is clear who should handle the
  ;; reply. But when non-Actor threads make a parallel query, it isn't
  ;; obvious who should handle the reply/error. In this case we spawn
  ;; another Actor to handle the reply.
  (let ((self (current-actor)))
    (labels ((handle-ans (ans)
               (apply kcont (multiple-value-list
                             (recover-ans-or-exn ans)))))
      (cond
       ((eql self actor)
        ;; Actor asking itself
        (handle-ans (apply 'asking-self-call msg)))
       (self
        ;; Actor asking another Actor
        (apply 'send actor
               :ask-{061B3878-CD81-11E7-9B0D-985AEBDA9C2A}
               (=cont #'handle-ans)
               msg))
       (t
        ;; non-Actor asking an Actor
        (apply 'send actor
               :ask-{061b3878-cd81-11e7-9b0d-985aebda9c2a}
               (um:curry #'spawn #'handle-ans)
               msg))
       ))))

(defmethod par-ask ((sym symbol) kcont &rest message)
  (if-let (actor (find-actor sym))
      (apply 'par-ask actor kcont message)
    (call-next-method)))

(defmethod par-ask ((str string) kcont &rest message)
  (if-let (actor (find-actor str))
      (apply 'par-ask actor kcont message)
    (call-next-method)))

(defmethod par-ask (obj kcont &rest msg)
  (apply kcont (multiple-value-list
                (apply 'ask obj msg))))

(defvar *blocker*
  ;; general utility Actor for possibly blocking actions
  ;; Route them to this Actor to keep all the others unblocked.
  (make-actor
   (dlambda
     (:perform (fn)
      (if (asking-p)
        ;; we are being wrapped with capture-ans-or-exn
        (funcall fn)
        ;; else - don't allow us to bomb out
        (ignore-errors
          (funcall fn))))
     
     (:whoami ()
      (list :blocker (current-actor)))
     
     (t (&rest msg)
        (error "undefined message: ~S" msg))
     )))

;; ------------------------------------------------

(defun trn (mat)
  ;; transpose of list-form matrix
  (apply 'mapcar 'list mat))

(=defun pmap (fn &rest lists)
  ;;
  ;; Parallel map - returns a result list.
  ;; Use like PAR for indefinite number of parallel forms,
  ;; each of which is the same function applied to different args.
  ;;
  ;; PMAP is intended for use within a =PAR-BIND which checks each
  ;; result for possible error.  (see example below)
  ;;
  (let* ((grps   (trn lists))
         (len    (length grps))
         (count  (list len))
         (ansv   (make-array len)))
    (labels ((done (ix ans)
               (setf (aref ansv ix) ans)
               (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                 (=values (capture-ans-or-exn
                           (lambda ()
                             (map 'list 'recover-ans-or-exn ansv)))))
               ))
      (if grps
          (loop for grp in grps
                for ix from 0
                do
                (apply 'spawn
                       (lambda (ix &rest args)
                         (done ix (apply 'capture-ans-or-exn fn args)))
                       ix grp))
        ;; else - empty lists, nothing to do
        (=values nil)))
    ))

(defun par-xform (pfn &rest forms)
  ;; Internal transform function. Converts a list of forms to a form
  ;; suitable for application by PMAP or PSOME
  `(,pfn 'funcall
         (list ,@(mapcar (lambda (form)
                           `(lambda ()
                              ,form))
                         forms))))

(defmacro par (&rest forms)
  ;; PAR - perform forms in parallel
  ;; This is intended for use within an =BIND clause
  ;;  (example below in WITH-ALL-FUTURES)
  (apply 'par-xform 'pmap forms))

(defmacro with-all-futures (args forms &body body)
  (let ((g!lst (gensym)))
    `(with-future (,g!lst)
         (par ,@forms)
       (destructuring-bind ,args ,g!lst
         ,@body))
    ))

#+:LISPWORKS
(editor:setup-indent "with-all-futures" 2)

(defmacro parlet (bindings &body body)
  ;; either directly, or eventually
  (let ((args  (mapcar 'first bindings))
        (forms (mapcar 'second bindings)))
    `(with-all-futures ,args
         ,forms
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "parlet" 2)


#|
;; examples...

(with-future (q r)
    (=ask 7 'truncate 4)
  (pr (list q r)))

(with-future (lst)
    (par
      (sin 1)
      (sin 2)
      (sin 3))
  (pr (reduce '+ lst)))
=> 1.8918884

(with-all-futures (&rest lst)
    ((sin 1)
     (sin 2)
     (sin 3))
  (pr lst))

(with-all-futures (a b c)
    ((sin 1)
     (sin 2)
     (sin 3))
  (pr (+ a b c)))
=> 1.8918884

(parlet ((a (sin 1))
         (b (sin 2))
         (c (sin 3)))
    (pr (+ a  b c)))
=> 1.8918884

(with-some-future (x)
    ((sin 1)
     (sin 2)
     (sin 3))
  (pr x))

(with-future (lst)
    (pmap (lambda (x y)
            (list x y))
          '(a b c)
          '(1 2 3))
  (pr lst))
==> '((a 1) (b 2) (c 3))

(with-future (lst)
    (=exec
      (mapcar 'sin '(1 2 3)))
  (pr (reduce '+ lst)))
=> 1.8918884

(with-future (lsts)
    (par
      (mapcar 'sin '(1 2 3)))
  (pr (reduce '+ (car lsts))))
=> 1.8918884

(with-future (lst)
    (pmap (lambda (fn)
            (funcall fn 1))
          '(sin cos tan))
  (pr lst))
=> (0.84147096 0.5403023 1.5574077)

(with-future (lst)
    (pmap (lambda (fn)
            (mapcar fn '(2 3 4)))
          '(sin cos tan))
  (pr lst))
=> ((0.9092974 0.14112 -0.7568025) (-0.41614684 -0.9899925 -0.6536436) (-2.1850398 -0.14254655 1.1578213))

(with-future (lst)
    (par
      (list 1 2)
      (mapcar 'sin '(1 2 3)))
  (pr lst))
=> (((1 2) (0.84147096 0.9092974 0.14112)))

(with-all-futures (ints sins)
    ((list 1 2)
     (mapcar 'sin '(1 2 3)))
  (pr (list :ints ints :sins sins)))

(=bind (x)
    (1+ 15)
  (pr x))
|#

;; ---------------------------------------------------------------

(=defun psome (fn &rest forms)
  ;;
  ;; Parallel OR - returns the first parallel exec that returns a
  ;; non-null value, or error.  Use like PMAPCAR against parallel
  ;; execs, each of which is the same function applied to different
  ;; args. At the very least, return a null after all forms have
  ;; answered.
  ;;
  ;; PFIRST is intended for use within an =PAR-BIND which checks result
  ;; for possible error.  (see example below)
  ;;
  (let* ((grps   (trn forms))
         (count  (list (length grps)))
         (ret    (list nil)))
    (labels ((done (ans)
               ;; continue on first non-null or error result
               ;; or after all forms have responded
               (multiple-value-bind (val err)
                   (ignore-errors
                     (recover-ans-or-exn ans))
                 (let ((ct (mpcompat:atomic-decf (car (the cons count)))))
                   (when (and (or val
                                  err
                                  (zerop ct))
                              (mpcompat:CAS (car ret) nil t))
                     (=values ans))
                   ))))
      (if grps
          (loop for grp in grps
                do
                (apply 'spawn (lambda (&rest args)
                                (done (apply 'capture-ans-or-exn fn args)))
                       grp))
        ;; else
        (=values nil))
      )))

(defmacro par-oneof (&rest forms)
  (apply 'par-xform 'psome forms))

(defmacro with-some-future ((ans) forms &body body)
  ;; actually like a WHEN-LET on OR of parallel conditionals
  `(with-future (,ans)
       (par-oneof ,@forms)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-some-future" 2)

#|
;; example
(with-some-future (which)
    ;; setup a race...
    ((progn
       (sleep 1)
       :one)
     (progn
       (sleep 1)
       :two)
     (progn
       (sleep 1)
       :three))
  (pr which))

(with-future (which)
    ;; same race, different syntax...
    (psome (lambda (id)
             (sleep 1)
             id)
           '(:one :two :three))
  (pr which))
|#

;; -------------------------------------------------------------
;; SMAPCAR, SMAP = sequential mapping where fn might require async
;; Actor participation. No spawning of transient Actors is used here.
;; But fn might.

(=defun smap (fn &rest lsts)
  ;; fn must be defined with =DEFUN or =LAMBDA, and return via =VALUES
  (labels ((mapper (lsts accum)
             (if (some 'endp lsts)
                 (=values (nreverse accum))
               (let ((hds (mapcar 'car lsts))
                     (tls (mapcar 'cdr lsts)))
                 (=bind (ans)
                     (=apply fn hds)
                   (mapper tls (cons ans accum))))
               )))
    (mapper lsts nil)))

#|
(=defun smapc (fn &rest lsts)
  ;; fn must be defined with =DEFUN or =LAMBDA, and return via =VALUES
  (labels ((mapper (lsts)
             (if (some 'endp lsts)
                 (=values)
               (let ((hds (mapcar 'car lsts))
                     (tls (mapcar 'cdr lsts)))
                 (=bind (&rest ans)
                     (=apply fn hds)
                   ans ;; just to remove unused warning...
                   (mapper tls)))
               )))
    (mapper lsts)))
|#

;; ------------------------------------------------
