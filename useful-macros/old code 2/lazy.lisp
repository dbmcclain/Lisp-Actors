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

#|
(defpackage #:um.lazy
  (:use #:common-lisp)
  (:export
   #:force
   #:deferred
   #:with-spin
   #:without-spin
   #:lazy
   #:once-only
   #:once-thereafter
   #:future
   #:unsafe-future
   #:pmap
   #:pvmap
   #:npmap
   #:npvmap
   ))
|#

(in-package #:um.lazy)

;; equiv to #F
(proclaim '(optimize (speed  3)
                     ;; (safety 0)
                     (float  0)))

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
  (list (cons initial-value cas-flag)))

(defmacro cell (ref)
  ;; refer to the cons cell itself
  `(car ,ref))

(defmacro cell-val (ref)
  ;; refer to the value car of the cell
  `(caar ,ref))

(defmacro cell-casflag (ref)
  ;; refer to the CAS-flag of the cell
  ;; We can't use CDAR directly because of CAS semantics
  `(cdr (car ,ref)))

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
           
           ((sys:compare-and-swap (cell-casflag ref) :uneval :in-process)
            (hcl:unwind-protect-blocking-interrupts-in-cleanups
                (let ((ans  (funcall evalfn (cell-val ref))))
                  ;; change both caar and cdar at same time
                  (setf (cell ref) (cons ans nil))
                  (return ans))
              
              ;; In case of early exit, put the CAS flag back for another try later.
              ;; If we finished normally, then this step will silently fail.
              (sys:compare-and-swap (cell-casflag ref) :in-process :uneval)))
           
           ((not *spin-wait*)
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
  (um:recover-ans-or-exn
   (forcer (lazy-valfn x)
           #'um:capture-ans-or-exn
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
  (hcl:unwind-protect-blocking-interrupts-in-cleanups
      (funcall fn)
    (setf (cell ref) (list nil))))

(defmethod force ((x once-only))
  (let ((ref (once-only-fn x)))
    (forcer ref
            (um:curry #'ensure-once-only ref)
            #'lw:false)))

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
            (um:curry #'ensure-once-only ref)
            (lambda (arg)
              (declare (ignore arg))
              (funcall (once-thereafter-fn-rest x))))))

;; -------------------------------------------------------
;; Futures...

;; NOTE: the code in the body of the future may not have the same
;; dynamic environment as the parent thread. Lexical bindings are
;; transferrable to the child thread, but not dynamic bindings.

(defun background-worker-thread-p (proc)
  (= 19 (mismatch "Background execute "
                  (string (mp:process-name proc)))))

(defun background-worker-thread-idle-p (proc)
  (and (background-worker-thread-p proc)
       (string= "Waiting for job to execute"
                (mp:process-whostate proc))
       ))
       
(defun available-thread-p ()
  (find-if #'background-worker-thread-idle-p
           (mp:list-all-processes)))

;; -------------------------------------------------------------
;; A FUTURE performs a body in parallel, while FORCE awaits and returns its result.
;; If an exception is raised in the body, it will be deferred to the caller.
;; All additional FORCEs will return the same result or raise the same exception.

(defstruct (future
             (:constructor %make-future))
  ans)

(defmethod force ((x future))
  ;; the evalfn should never be called because a FUTURE cell starts
  ;; with a CAS flag of :IN-PROCESS, not :UNEVAL
  (forcer (future-ans x)
          'cant-happen
          #'um:recover-ans-or-exn))

(let ((future-count (list 0))
      (abort-exn    (um:capture-ans-or-exn #'error "Aborted future")))

  (defun make-future (unsafe fn)
    ;; MAKE-FUTURE - what's unsafe or safe?  Unsafe ignores the
    ;; possibility that the assigned BG worker thread running the
    ;; function could, itself, rely on a FUTURE. And with limited
    ;; thread resources, would tie up the thread during FORCE, leading
    ;; to thread exhaustion and deadlock. When in doubt, use safe
    ;; mode.
    ;;
    (let ((ref (make-ref abort-exn :in-process)))
      (labels ((gf ()
                 (hcl:unwind-protect-blocking-interrupts-in-cleanups
                     (setf (cell-val ref) (um:capture-ans-or-exn fn))
                   (progn
                     (setf (cell-casflag ref) nil)
                     (sys:atomic-fixnum-decf (car future-count))))))
        
        (sys:atomic-fixnum-incf (car future-count))
        (if (or unsafe
                (<= (car future-count) (mp:set-funcall-async-limit nil))
                (available-thread-p))
            ;; if caller asked for unsafe mode, or the number of
            ;; outstanding FUTUREs is within the limited number of BG
            ;; worker threads, or there seems to be an available BG
            ;; worker thread, then use BG worker threads.
            (mp:funcall-async #'gf)
          ;; else - create one for ourselves
          (mp:process-run-function (gensym "future-") '() #'gf))
        (mp:process-allow-scheduling) ;; help to get ball rolling...
        (%make-future
         :ans  ref)
        ))))
    
(defmacro future (&body body)
  `(make-future nil (lambda () ,@body)))

(defmacro unsafe-future (&body body)
  `(make-future t (lambda () ,@body)))

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
               (let* ((tail (unsafe-future (%pmap (cdr lst))))
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
               (let ((tail (unsafe-future (%npmap (cdr lst)))))
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
                      (sync (unsafe-future (map-range mid hi)))
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

