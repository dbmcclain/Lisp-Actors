;; seriallizers.lisp -- Illustrating the need for serializers to avoid data race conditions
;;
;; DM/RAL 01/22
;; -------------------------------------------------
;;
;; This example shows that, even if you write FPL-pure code, you can
;; still have "mutability" of shared state as far as outside observers
;; see it. And you need to avoid concurrency data races.  Serializers
;; offer one way to fix the concurrency data race problem.

(in-package :ac)

(defun cell-beh (value)
  ;; A shared "mutable" value cell
  ;;
  ;; Notice that even though we never physically mutate the value
  ;; parameter, to outside observers the cell state does appear
  ;; mutated via BECOME.
  (alambda
   ((cust :read)
    (send cust value))

   ((cust :write new-val)
    (become (cell-beh new-val))
    (send cust self))

   ((cust :cas old new)
    (if (eql old value)
        (become (cell-beh new))
      (send cell value)))
   ))

(defun counter-svc-beh (cell)
  ;; a service to increment the value in the shared "mutable" cell.
  ;; 
  ;; The split-read/write for updating cell value works only when we
  ;; have just a single thread of execution performing this code.
  ;;
  ;; If any degree of concurrency exists, then immediately after our
  ;; :READ request, another concurrent thread could begin its own
  ;; execution of the same code, hence using the wrong initial value.
 ;;
 (lambda (cust amount)
    (β  (count)
        (send cell β :read)
      (let ((new-count (+ count amount)))
        (β  _
            (send cell β :write new-count)
          (send cust new-count)))
      )))

;; ----------------------------------------------------------
;; Single-Thread No-Concurrency

(with-single-thread
  ;; Single-Thread No Concurrency - Works okay, correct answer is 111.
  (let* ((cell        (make-actor (cell-beh 0)))
         (counter-svc (make-actor (counter-svc-beh cell))))
    (send counter-svc println 1)
    (send counter-svc println 10)
    (send counter-svc println 100)))

;; ----------------------------------------------------------
;; Single-Thread with Concurrency

(with-single-thread
  ;; Single-Thread Concurrency - Incorrect Solution
  (let* ((cell        (make-actor (cell-beh 0)))
         (counter-svc (make-actor (counter-svc-beh cell))))
    (send (actor ()
            (send counter-svc println 1)
            (send counter-svc println 10)
            (send counter-svc println 100)))))

(with-single-thread
  ;; Single-Thread Concurrency - Correct Serialized Solution
  (let* ((cell        (make-actor (cell-beh 0)))
         (counter-svc (SERIALIZER (make-actor (counter-svc-beh cell)))))
    (send (actor ()
            (send counter-svc println 1)
            (send counter-svc println 10)
            (send counter-svc println 100)))))

;; ----------------------------------------------------------
;; Parallel MultiCore SMP Concurrency

(progn
  ;; Parallel SMP Concurrency - Incorrect Solution
  (let* ((cell        (make-actor (cell-beh 0)))
         (counter-svc (make-actor (counter-svc-beh cell))))
    (send (actor ()
            (send counter-svc println 1)
            (send counter-svc println 10)
            (send counter-svc println 100)))))

(progn
  ;; Parallel SMP Concurrency - Correct Serialized Solution
  (let* ((cell        (make-actor (cell-beh 0)))
         (counter-svc (SERIALIZER (make-actor (counter-svc-beh cell)))))
    (send (actor ()
            (send counter-svc println 1)
            (send counter-svc println 10)
            (send counter-svc println 100)))))

