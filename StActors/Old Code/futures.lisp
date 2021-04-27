;; lazy.lisp -- SMP-safe DEFERRED, LAZY, ONCE-ONLY, ONCE-THEREAFTER, FUTURE, and FORCE
;;
;; DM/RAL  09/16,03/17
;; -----------------------------------------------

#||#
(defpackage #:actors.lazy
  (:use #:common-lisp
   #:actors)
  (:nicknames #:aclz)
  (:export
   #:force
   #:deferred
   #:with-spin
   #:without-spin
   #:lazy
   #:once-only
   #:once-thereafter
   #:future
   #:pmap
   #:pvmap
   #:npmap
   #:npvmap
   ))
#||#

(in-package #:actors.lazy)

;; equiv to #F
(proclaim '(optimize (speed  3)
                     (safety 0)
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
;;
;; A FUTURE performs a body in parallel, while FORCE awaits and
;; returns its result.  If an exception is raised in the body, it will
;; be deferred to the caller.  All additional FORCEs will return the
;; same result or raise the same exception.
;;
;; FUTUREs turn a parallel computation into a first class object which
;; can be collected and mapped.

(defstruct (future
             (:constructor %make-future))
  ans)

(defmethod force ((x future))
  ;; the evalfn should never be called because a FUTURE cell starts
  ;; with a CAS flag of :IN-PROCESS, not :UNEVAL
  (forcer (future-ans x)
          'cant-happen
          #'um:recover-ans-or-exn))

(let ((abort-exn #.(um:capture-ans-or-exn
                    #'error "Aborted future")))

  (defun make-future (fn)
    (let ((ref (make-ref abort-exn :in-process)))
      (labels ((gf ()
                 (hcl:unwind-protect-blocking-interrupts-in-cleanups
                     (setf (cell-val ref) (um:capture-ans-or-exn fn))
                   (setf (cell-casflag ref) nil))))
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
         (nthr   4) ;; nbr CPU cores
         (nfpg   (ceiling (length fns) nthr))
         (fngrps (um:group fns nfpg))
         (nwrk   (1- (length fngrps))))
    (map nil (lambda (grp)
               (spawn (lambda ()
                        (unwind-protect
                            (map nil #'funcall grp)
                          (mp:semaphore-release sem)))
                      ))
         (cdr fngrps))
    (map nil #'funcall (car fngrps))
    (when (plusp nwrk)
      (mp:semaphore-acquire sem :count nwrk))
    ))

(defmacro par (&rest clauses)
  ;; Accept a list of clauses and execute them in parallel,
  ;; synchronizing at the closing paren.
  (if (rest clauses)
      `(%par ,@(mapcar #`(lambda ()
                           ,a1)
                       clauses))
    ;; else
    (first clauses)))

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
(progn
  (with-serial-dispatch ()
    (sleep 3)
    (print :hello))
  (with-serial-dispatch ()
    (print :yep))
  (print :ok))

(progn
  (with-parallel-dispatch ()
    (sleep 3)
    (print :hello))
  (with-parallel-dispatch ()
    (print :yep))
  (print :ok))
|#

