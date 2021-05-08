
(in-package :ac)

(ac:make-remote-actor "echo@rincon.local"
                      :register :RECHO)

(ac:make-remote-actor "eval@rincon.local"
                      :register :REVAL)

(ac:make-remote-actor "eval@arroyo.local"
                      :register :REVAL)

(loop repeat 5 do
      (send :reval (ac:usti (println)) `(machine-instance)))

#|
(spawn (lambda ()
         (let ((s1 (make-actor
                    (um:dlambda
                      (:get-val ()
                       (=bind (x)
                           (=async (=values 15))
                         (+ x 2)))))))
           (assert (eql 17 (ask s1 :get-val))))
         ))
|#

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(get-actor-names (println))
(find-actor (println) :reval)

(defun make-tree-beh ()
  (lambda (cust n)
    (cond ((zerop n)
           (send cust))
          (t
           (send (α (make-tree-beh)) self (1- n))
           (send (α (make-tree-beh)) self (1- n))
           (become (lambda* _
                     (become (lambda* _
                               (send cust))))))
          )))

(let ((top (α (make-tree-beh)))
      (me  (actor _
             (send (println) :DONE))))
  (sendx t top me 24))


(defun burn-time ()
  (loop repeat 10000 do
        (user::erfc (random 1d0))))

(defun make-erfc-tree-beh ()
  (lambda (cust n)
    (cond ((zerop n)
           (burn-time)
           (send cust))
          (t
           (send (α (make-erfc-tree-beh)) self (1- n))
           (send (α (make-erfc-tree-beh)) self (1- n))
           (become (lambda* _
                     (become (lambda* _
                               (send cust))))))
          )))


(let ((top (α (make-erfc-tree-beh)))
      (me  (actor _
             (send (println) :DONE))))
  (sendx t top me 10))

(progn
  (defun make-empty-sort-beh (x)
    (lambda (y)
    (if (< y x)
        (become (make-lt-sort-beh x (α (make-empty-sort-beh y))))
      (become (make-ge-sort-beh x (α (make-empty-sort-beh y)))))))
  
  (defun make-lt-sort-beh (x lt)
    (lambda (y)
      (if (< y x)
          (send lt y)
        (become (make-sort-beh x lt (α (make-empty-sort-beh y)))))))
  
  (defun make-ge-sort-beh (x ge)
    (lambda (y)
      (if (< y x)
          (become (make-sort-beh x (α (make-empty-sort-beh y)) ge))
        (send ge y))))

  (defun make-sort-beh (x lt ge)
    (lambda (y)
      (if (< y x)
          (send lt y)
        (send ge y)))))

(defun tst (n)
  (um:nlet iter ((ix  0)
                 (top nil))
    (when (< ix n)
      (let ((x  (random #.(ash 1 60))))
        (cond (top
               (sendx nil top x)
               (go-iter (1+ ix) top))
              (t
               (go-iter (1+ ix) (α (make-empty-sort-beh x))))
              )))))

(time (tst 1000000))
