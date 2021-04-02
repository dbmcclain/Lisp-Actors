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
;; DM/RAL 04/21
;; -------------------------------------------------

(in-package :actors/promises)

(defstruct promise
  (mbox (mp:make-mailbox)))

(defmacro promise (&body body)
  (lw:with-unique-names (promise)
    `(let ((,promise (make-promise)))
       (ac:spawn-worker (lambda ()
                          (mp:mailbox-send (promise-mbox ,promise)
                                           (um:capture-ans-or-exn
                                             ,@body))))
       ,promise)))

(defun realize (promise &optional (timeout *timeout*))
  (let ((ans (mp:mailbox-read (promise-mbox promise)
                              "Waiting to realize a promise" timeout)))
    (cond (ans
           ;; a real answer will never be NIL
           (mp:mailbox-send (promise-mbox promise) ans) ;; in case of repeated REALIZE
           (um:recover-ans-or-exn ans))
          (t
           ;; we had a timeout - can't just return NIL since that
           ;; might be mistaken for an actual reply. Can't return a
           ;; flag because there may be multiple values already
           ;; expected. Better to have caller wrap with TIMEOUT
           ;; handlers.
           (error 'timeout))
          )))

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