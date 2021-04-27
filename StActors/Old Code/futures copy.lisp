;; lazy.lisp -- SMP-safe DEFERRED, LAZY, ONCE-ONLY, ONCE-THEREAFTER, FUTURE, and FORCE
;;
;; DM/RAL  09/16,03/17
;; -----------------------------------------------
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


(in-package #:um.lazy)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:curry
            um:foreach
            um:group
            um:capture-ans-or-exn
            um:recover-ans-or-exn

            ac:spawn
            ac:current-actor
            )))
      
;; equiv to #F
(proclaim '(optimize (speed  3)
                     #|(safety 0)|#
                     #+:LISPWORKS (float  0)))

;; ------------------------------------

(defmethod force (x)
  x)

(defmethod force ((fn function))
  ;; for thunks - have to rely on programmer
  (funcall fn))

;; ------------------
;; DEFERRED - Simple, uncached, laziness
;; A FORCE always re-evals

(defmacro deferred (&body body)
  `(lambda ()
     ,@body))

;; ------------------------------------
;; Control whether to spin-wait (WITH-SPIN ...) or context switch
;; (WITHOUT-SPIN ...) during waiting.  Default is to context switch.
;;

(defvar *spin-wait*  nil)

(defmacro with-spin (&body body)
  `(let ((*spin-wait* t))
     ,@body))

(defmacro without-spin (&body body)
  `(let ((*spin-wait* nil))
     ,@body))

;; ------------------------------------
;; A limited, locally useful, definition of ref cells.
;; Lacking the generality and type specificity of REF cells
;; as defined in tools/data-objects/. We save some memory
;; and a layer of indirection with our local definition.

(defun make-ref (&optional initial-value (cas-flag :uneval))
  ;; MAKE-REF - uniform ref cell creation
  ;;
  ;; The convention is that a ref is a singleton list containing a
  ;; cons cell.  The car of the cons cell is a value, and its cdr is a
  ;; CAS flag.
  ;;
  ;; We utilize the extra layer of indirection so that both the
  ;; contents and CAS-flag can be switched simultaneously.
  ;;
  (list (cons initial-value cas-flag)))

(defmacro cell (ref)
  ;; refer to the cons cell itself
  `(car ,ref))

(defmacro cell-val (ref)
  ;; refer to the value car of the cell
  `(car (cell ,ref)))

(defmacro cell-casflag (ref)
  ;; refer to the CAS-flag of the cell
  ;; We can't use CDAR directly because of CAS semantics
  `(cdr (cell ,ref)))

;; ------------------------------------
 
(defun forcer (ref evalfn valfn)
  ;; FORCER - a generalized forcing function
  ;;
  ;; The ref's CAS flag begins in state :UNEVAL, and on the first call
  ;; it will be switched to :IN-PROCESS while the evalfn is called on
  ;; the initial value car.  The result of that call will be stored
  ;; back into a car, along with a null CAS flag, and the two will be
  ;; simulateneously stored in the singleton list of the ref. The
  ;; function result will also be returned to the caller.
  ;;
  ;; Doing this two-stage update forces other threads to await the
  ;; first-time call results.
  ;;
  ;; Thereafter, this function will return the result of calling valfn
  ;; on the updated value car of the ref.
  ;;
  ;; If anything should happen before the null CAS flag and value pair
  ;; are stored in the ref's singleton list, the original CAS flag
  ;; will be restored for another try. If you don't want to permit
  ;; retries, then be sure to thwart it in the evalfn. (e.g.,
  ;; ENSURE-ONCE-ONLY below)
  ;;
  (labels ((ok-to-proceed ()
             (null (cell-casflag ref))))
    (loop
     (cond ((ok-to-proceed)
            (return (funcall valfn (cell-val ref))))
           
           ((mpcompat:CAS #-OPENMCL (cell-casflag ref)
                          #+OPENMCL (cdr (car ref))
                          :uneval :in-process)
            (unwind-protect
                (let ((ans  (funcall evalfn (cell-val ref))))
                  ;; change both caar and cdar at same time
                  (when (eq :in-process (cell-casflag ref))
                    (setf (cell ref) (cons ans nil)))
                  (return ans))
              
              ;; In case of early exit, put the CAS flag back for another try later.
              ;; If we finished normally, then this step will silently fail.
              (mpcompat:CAS #-OPENMCL (cell-casflag ref)
                            #+OPENMCL (cdr (car ref)) :in-process :uneval)))
           
           ((not *spin-wait*)
            ;; :CLOZURE ?? -- will default to spinning
	    #+:ALLEGRO
	    (mp:process-wait "" #'ok-to-proceed)
	    #+:LISPWORKS
            (mp:wait-processing-events nil
                                       :wait-function #'ok-to-proceed))
           ))))
                                  
;; ------------------------------------
;; A LAZY evaluation performs on the first FORCE. It's value is saved
;; and returned on all future FORCEs. If the function produces an
;; exception, that exception will be re-raised on future FORCEs too.

(defstruct (lazy
             (:constructor %make-lazy))
  valfn)

(defun make-lazy (fn)
  (%make-lazy
   :valfn (make-ref fn)))

(defmacro lazy (&body body)
  `(make-lazy (lambda ()
                ,@body)))

(defmethod force ((x lazy))
  (recover-ans-or-exn
   (forcer (lazy-valfn x)
           #'capture-ans-or-exn
           #'identity)))

;; ------------------------------------
;; A ONCE-ONLY is performed only once, on the first FORCE.
;; Thereafter, FORCEs will return a NIL. This is true even if the
;; perform bombs out.  Any threads simultaneously FORCEing will have
;; to wait until the first FORCE has finished.

(defstruct (once-only
            (:constructor %make-once-only))
  fn)

(defun make-once-only (fn)
  (%make-once-only
   :fn (make-ref fn)))

(defmacro once-only (&body body)
  `(make-once-only (lambda ()
                     ,@body)))

(defun ensure-once-only (ref fn)
  ;; prevent re-exec even if we bomb out in fn
  (unwind-protect
      (funcall fn)
    (setf (cell ref) (list nil))))

(defmethod force ((x once-only))
  (let ((ref (once-only-fn x)))
    (forcer ref
            (curry #'ensure-once-only ref)
            (constantly nil))))

;; ------------------------------------
;; A ONCE-THEREAFTER performs the first clause on the first FORCE.
;; All additional FORCEs will perform the second clause. This is true,
;; even if the clause bombs out.  Any threads simultaneously FORCEing
;; will have to wait until the first FORCE has finished, in case of
;; side effects.

(defstruct (once-thereafter
            (:constructor %make-once-thereafter))
  fn-first
  fn-rest)

(defun make-once-thereafter (fn-first fn-rest)
  (%make-once-thereafter
   :fn-first (make-ref fn-first)
   :fn-rest  fn-rest))

(defmacro once-thereafter (clause-first clause-rest)
  `(make-once-thereafter (lambda ()
                           ,clause-first)
                         (lambda ()
                           ,clause-rest)))

(defmethod force ((x once-thereafter))
  (let ((ref (once-thereafter-fn-first x)))
    (forcer ref
            (curry #'ensure-once-only ref)
            (lambda (arg)
              (declare (ignore arg))
              (funcall (once-thereafter-fn-rest x))))))

;; -------------------------------------------------------
;; Futures...

;; NOTE: the code in the body of the future may not have the same
;; dynamic environment as the parent thread. Lexical bindings are
;; transferrable to the child thread, but not dynamic bindings.
;;
;; A FUTURE performs a body in parallel, while FORCE awaits and
;; returns its result.  If an exception is raised in the body, it will
;; be reflected back to the caller.  All additional FORCEs will return
;; the same result or raise the same exception.
;;
;; FUTUREs turn a parallel computation into a first class object which
;; can be collected and mapped.

(defstruct (future
             (:constructor %make-future))
  ans)

(defmethod force ((x future))
  ;; the evalfn should never be called because a FUTURE cell starts
  ;; with a CAS flag of :IN-PROCESS, not :UNEVAL
  ;;
  ;; Actors are certainly welcome to FORCE futures - may block
  ;; waiting... but that's usually okay.
  ;;
  (forcer (future-ans x)
          'cant-happen
          #'recover-ans-or-exn))

(let ((abort-exn (load-time-value
                  (capture-ans-or-exn
                   #'error "Aborted future")
                  t))) ;; read-only
      
  (defun make-future (fn)
    #| ;; leave the risk to the user...
    (when (current-actor)
      ;; Actors are generally incompatible with parallel concurrent
      ;; access to their internal Actor state.
      (error "Futures should not be created inside of Actors"))
    |#
    (let ((ref (make-ref abort-exn :in-process)))
      (labels ((gf ()
                 (setf (cell ref)
                       (cons (capture-ans-or-exn fn)
                             nil))))
        (spawn #'gf)
        (%make-future
         :ans  ref)
        ))))
    
(defmacro future (&body body)
  `(make-future (lambda () ,@body)))

;; -----------------------------------------------------
;; PMAP and NPMAP -- parallel list mapping. PMAP is functional,
;; while NPMAP is destructive.

(defun pmap (fn lst)
  ;; non-destructive form
  ;; on the assumption that fn is a time consuming operation
  ;; and that re-scanning the list is inconsequential in comparison...
  ;; the fn should still refrain from nested futures
  (labels ((%pmap (lst)
             (when (consp lst)
               (let* ((tail (future (%pmap (cdr lst))))
                      (val  (funcall fn (car lst))))
                 (cons val tail))))
           (%force (lst &optional accum)
             ;; at this point the resulting lst is not a true list,
             ;; but rather a dotted pair
             (if lst
                 (%force (force (cdr lst)) (cons (car lst) accum))
               (nreverse accum))))
    (%force (%pmap lst))))
      
(defun npmap (fn lst)
  ;; destructive form
  (labels ((%npmap (lst)
             (when (consp lst)
               (let ((tail (future (%npmap (cdr lst)))))
                 (setf (car lst) (funcall fn (car lst)))
                 tail)
               ))
           (%force (ans)
             (when ans
               (%force (force ans)))))
    (%force (%npmap lst))
    lst))

;; ----------------------------------------------------
;; PVMAP and NPVMAP -- parallel vector mapping. PVMAP is functional,
;; while NPVMAP is destructive

(defun npvmap (fn vec &key (dest vec) (grp 1))
  ;; potentially destructive form
  ;; but vector division takes advantage of data parallelism much
  ;; better than mapping a list which is necessarily serialized
  (labels ((map-range (lo hi)
             (if (>= (+ lo grp) hi)
                 (progn
                   (loop for ix from lo below hi do
                         (setf (aref dest ix) (funcall fn (aref vec ix))))
                   nil)
               ;; else
               (let* ((mid  (truncate (+ lo hi) 2))
                      (sync (future (map-range mid hi)))
                      (lam  (map-range lo mid)))
                 (cons lam sync))
               ))
           (%force (pair)
             (when pair
               (%force (car pair))
               (%force (force (cdr pair))))) )
    (%force (map-range 0 (length vec)))
    dest))

(defun pvmap (fn vec &key (grp 1))
  ;; non-destructive form
  (npvmap fn vec
          :dest (make-array (length vec)
                            :element-type (array-element-type vec))
          :grp grp))

#|
;; -------------------------------------------
;; PAR Construct

(defun %par (&rest fns)
  ;; Accept a list of functions, execute them all in parallel.
  ;;
  ;; Since they are all slated for parallel execution, evaluation
  ;; order must be irrelevant.
  ;;
  ;; We group them to try to balance the load among available threads,
  ;; including our own. 
  ;;
  (let* ((sem    (mp:make-semaphore :count 0))
         (nthr   *nbr-execs*) ;; nbr CPU cores
         (nfpg   (ceiling (length fns) nthr))
         (fngrps (group fns nfpg))
         (nwrk   (1- (length fngrps))))
    (foreach (lambda (grp)
                  (spawn (lambda ()
                           (unwind-protect
                               (foreach #'funcall grp)
                             (mp:semaphore-release sem)))
                         ))
                (cdr fngrps))
    (foreach #'funcall (car fngrps))
    (when (plusp nwrk)
      (mp:semaphore-acquire sem :count nwrk))
    ))

(defmacro par (&rest clauses)
  ;; Accept a list of clauses and execute them in parallel,
  ;; synchronizing at the closing paren.
  ;;
  ;; Note: Don't confuse PAR with FUTURE. The clauses in a PAR form do
  ;; not return anything of useful value.
  ;;
  (if (rest clauses)
      `(%par ,@(mapcar #`(lambda ()
                           ,a1)
                       clauses))
    ;; else
    (first clauses)))
|#
#|
(time
 (par
   (sleep 1) ;; 1
   (sleep 1) ;; 2
   (sleep 1) ;; 3
   (sleep 1) ;; 4
   (sleep 1) ;; 5
   
   (sleep 1) ;; 1
   (sleep 1) ;; 2
   (sleep 1) ;; 3
   (sleep 1) ;; 4
   (sleep 1) ;; 5
   
   (sleep 1) ;; 1
   (sleep 1) ;; 2
   (sleep 1) ;; 3
   (sleep 1) ;; 4
   (sleep 1) ;; 5
   
   (sleep 1) ;; 1
   (sleep 1) ;; 2
   (sleep 1) ;; 3
   #|
   |#
   ))
|#

#|
;; transpose
(apply 'mapcar 'list (um:group '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) 4))
|#

(defstruct lazy
  cell)

(defmacro lazy (expr)
  `(make-lazy
    :cell (cons
           (lambda ()
             ,expr)
           :uneval)))

(defun force (v)
  (cond
   ((lazyp v)
    (recover-ans-or-exn
     (um:nlet-tail iter ()
       (let ((cell (lazy-cell v)))
         (cond
          ((eq (cdr cell) :eval)
           (car cell))
          ((sys:compare-and-swap (cdr cell) :uneval :in-proc)
           (let ((val (um:mcapture-ans-or-exn
                        (funcall (car cell)))))
             (setf (lazy-cell v) (cons val :eval))
             val))
          (t
           (mp:process-allow-scheduling)
           (iter))
          )))))
   (t
    v)))
  
