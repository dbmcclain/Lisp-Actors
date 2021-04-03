;; actors/promises - this is almost too easy...
;;
;; Promises represent async action deferral. Wrap a body of code with
;; PROMISE. The code will be performed asynchronously with the running
;; thread, and the PROMISE form will return a promise object to the
;; current thread. WHen you are ready for the results, call REALIZE on
;; the returned promise.
;;
;; Action code can produce errors, and those will be reflected back to
;; the caller of the PROMISE form, and raised upon REALIZE. You can
;; surround your REALIZE call with HANDLER-CASE or HANDLER-BIND in
;; order to deal with them.
;;
;; If a new promise is based on first realizing a previous promise,
;; just remember that the body of code doing the realizing may need
;; its own handlers, since it will be running in a separate thread
;; from the main running thread.
;;
;; If you try to REALIZE a promise before it has been delivered, you
;; will block waiting for its delivery.
;;
;; A promise can be realized any number of times, and each time will
;; return the same answer or raise the same error. You can hand off a
;; promise to any other thread for realization. Promises can yield
;; multiple values.
;;
;; Added FULFILL to grant more flexibility in fulfilling promises. Use
;; with WITH-PROMISE around a body of code. That body is responsible
;; for calling FULFILL on the named promise arg of the WITH-PROMISE.
;; That code can run anywhere you deem most appropriate. The
;; WITH-PROMISE returns a new promise object to the caller of the
;; form.
;;
;; Interesting aside... Apart from the SPAWN-WORKER in PROMISE, this
;; code has general utility and is otherwise independent of Actors.
;; Also does not depend on CPS.
;;
;; DM/RAL 04/21
;; -------------------------------------------------

(in-package :actors/promises)

(defstruct promise
  (ans    nil)
  (cxlock (mp:make-lock)               :read-only t)
  (cxvar  (mp:make-condition-variable) :read-only t))


(defun do-fulfill (promise fn)
  ;; while a promise yet to be fulfilled can be handed off to any
  ;; number of threads, only the first one completing a fulfillment
  ;; will affect the resulting realization of a promise.
  (with-accessors ((ans    promise-ans)
                   (cxlock promise-cxlock)
                   (cxvar  promise-cxvar)) promise
    (when (sys:compare-and-swap ans nil
                                (um:call-capturing-ans-or-exn fn))
      (mp:with-lock (cxlock)
        (mp:condition-variable-broadcast cxvar))
      )))
  
(defmacro fulfill (promise &body body)
  ;; fulfill a promise, waking up any waiting threads
  `(do-fulfill ,promise (lambda () ,@body)))

(defmacro with-promise (promise &body body)
  ;; promise should be a symbol, body must cause promise to be
  ;; fulfilled, return the new promise object
  `(let ((,promise (make-promise)))
     ,@body
     ,promise))

#+:LISPWORKS
(progn
  (editor:setup-indent "with-promise" 1)
  (editor:setup-indent "fulfill" 1))

(defmacro promise (&body body)
  ;; perform body in another thread, return the promise object
  (lw:with-unique-names (promise)
    `(with-promise ,promise
       (spawn-worker (lambda ()
                       (fulfill ,promise
                         ,@body))))
    ))

(defun realize (promise &optional (timeout *timeout*))
  ;; wait for a pomise to be fulfilled
  (with-accessors ((ans    promise-ans)
                   (cxlock promise-cxlock)
                   (cxvar  promise-cxvar)) promise
    (um:recover-ans-or-exn
     (um:check/lock/check ans cxlock
       (if (mp:condition-variable-wait cxvar cxlock
                                       :timeout     timeout
                                       :wait-reason "Waiting for promise")
           ans
         (error 'timeout))
       ))))

#|
;; demonstrate chained promises
(let* ((prom1 (promise
                (+ 1 2)))
       (prom2 (promise
                (* (realize prom1) 5)))
       (prom3 (promise
                (1- (realize prom2)))))
  ;; we wait for the last one...
  (realize prom3))
|#