
;; Macros vs Functions & Closures

;; CPS Continuations - sources, filters, sinks

;; Actors vs Functions - sources, filters, sinks

;; Actors & CPS vs Functions

(defun single-task-beh (serv)
  (lambda (cust &rest msg)
    (let ((tag (tag self)))
      (become (in-use-st-beh serv tag cust nil))
      (send* serv tag msg))))

(defun in-use-st-beh (serv tag cur-cust q)
  (alambda
   ((cust &rest ans) when (eq cust tag)
    (send* cur-cust ans)
    (if q
        (multiple-value-bind (next-req new-q) (popq q)
          (become (in-use-st-beh serv tag (car next-req) new-q))
          (send* serv tag (cdr next-req)))
      (become (single-task-beh serv))))

   ((cust &rest msg)
    (become (in-use-st-beh serv tag cur-cust (addq q (cons cust msg)))))
   ))

(defun single-thread-beh (serv &key (sponsor *sponsor*))
  (lambda (&rest msg)
    (send* sponsor serv msg)))



;; ------------------------------------

(defvar *tstvar* nil)
(defun tst (&key (dest *tstvar*))
  (lambda ()
    (print dest)))
(defvar *tst* (tst))
