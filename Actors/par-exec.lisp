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

(in-package :actors.par)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:single
            um:with-captured-ans-or-exn
            um:recover-ans-or-exn
            
            ref:ref
            ref:atomic-decf

            timeout:timeout
            timeout:*timeout*

            cps:=wait
            cps:=values
            cps:with-cont
            cps:with-cps
            
            actors.executives:terminate-actor
            actors.base:spawn-worker
            )))
            
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

(defun pmapcar (fn &rest lists)
  ;; Parallel map of fn over list of params. First group is performed
  ;; by our own Actor in parallel with workers acting on other groups.
  (when lists
    (let ((grps (apply #'um:zip lists)))
      (if (single grps)
          (apply fn (car grps))
        ;; else
        (let* ((len    (length grps))
               (count  (ref:ref len))
               (ansv   (make-array len))
               (actors nil))
          (handler-bind ((timeout (lambda (c)
                                    (declare (ignore c))
                                    (map nil 'terminate-actor actors))))
            (=wait () (:timeout *timeout* :errorp t)
                (flet ((done (ix ans)
                         (setf (aref ansv ix) ans)
                         (when (zerop (ref:atomic-decf count))
                           (=values))))
                  (setf actors (loop for grp in (cdr grps)
                                     for ix from 1
                                     collect
                                     (spawn-worker 
                                      (lambda (ix args)
                                        (done ix (mcapture-ans-or-exn
                                                   (apply fn args))))
                                      ix grp)))
                  ;; perform the first of the branchees ourself
                  (done 0 (with-captured-ans-or-exn
                            (apply fn (car grps)))))
              ;; blocking, in this case, means the wait for worker threads
              ;; to complete after we have performed the first funcall
              (map 'list 'recover-ans-or-exn ansv))
            ))))))

(defmacro par (&rest forms)
  `(pmapcar 'funcall
            (list ,@(mapcar #`(lambda ()
                                ;; WITH-CONT allows form to use =VALUES
                                (with-cont
                                  ,a1))
                            forms))))

(defmacro =non-blocking (expr)
  ;; Must be used within an =BIND form.
  ;;
  ;; In general, if PAR, PMAPCAR, PARLET are used within an Actor and
  ;; any of the parallel clauses use Actor services provided by the
  ;; Actor, then the blocking wait will produce a dealdlock condition.
  ;;
  ;; The Actor needs to remain open to message processing while those
  ;; clauses execute. So PAR, PMAPCAR, PARLET need to be placed within
  ;; an =NON-BLOCKING and =BIND form in the Actor code.
  ;;
  ;; Beware - this invites multi-thread access to internal state.
  ;; Best to use accessor methods (which are messages) for the Actor
  ;; rather than direct access to shared state. Messaging keeps
  ;; single-thread semantics.
  `(spawn-worker (lambda ()
                   (with-cps ,expr))))

;; -------------------------------------------------------------------

#|
(par
  (print :doit1)
  (print :doit2))
(pmapcar 'print '(:doit1 :doit2))

(=bind (ans)
    (=values (pmapcar 'print '(:doit1 :doit2 :doit3)))
  (print (format nil "~&=bind ans = ~A" ans)))

(spawn
 (lambda ()
   (=bind (ans)
       (=values (pmapcar 'print '(:doit1 :doit2 :doit3)))
     (print (format nil "~&=bind ans = ~A" ans)))))

(spawn
 (lambda ()
   (=bind (ans)
       (=non-blocking (progn
                        (=values (pmapcar 'print '(:doit1 :doit2 :doit3)))
                        (print :should-not-reach)))
     (format t "~&=bind ans = ~A" ans))))
|#  

(defmacro parlet (bindings &body body)
  ;; either directly, or eventually
  (let ((args  (mapcar 'first bindings))
        (forms (mapcar 'second bindings)))
    `(destructuring-bind ,args
         (par ,@forms)
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "parlet" 2)

;; ----------------------------------------------------------------------

