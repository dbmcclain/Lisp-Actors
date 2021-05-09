
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

;; -----------------------------------------------------
;; Do-Nothing Fork-Bomb

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

(let* ((top   (α (make-tree-beh)))
       (timer (α (make-timer-beh)))
       (me    (actor _
                (let ((k-show (actor (dt)
                                (send (println)
                                      (format nil "dt = ~,2F"
                                              (* 1e-6 dt))))))
                  (sendx nil timer :stop k-show)))
              ))
  ;; Test 33.6 Million Actors
  (sendx nil timer :start)
  (sendx nil top me 24))

;; -----------------------------------------------------
;; Direct Function Call Empty Fork-Bomb

(defun run-direct-funcall-tree-bomb (n)
  (cond ((zerop n))
        (t
         (run-direct-funcall-tree-bomb (1- n))
         (run-direct-funcall-tree-bomb (1- n)))
        ))

(let* ((timer (α (make-timer-beh)))
       (me    (actor _
                (let ((k-show (actor (dt)
                                (send (println)
                                      (format nil "dt = ~,2F"
                                              (* 1e-6 dt))))))
                  (sendx nil timer :stop k-show)))
              ))
  ;; Test 400 * 33.6 Million Funcalls => 33 sec
  (sendx nil timer :start)
  (dotimes (ix 400)  ;; Funcalls are 400x faster
    (run-direct-funcall-tree-bomb 24))
  (send me))

;; -----------------------------------------------------
;; Erfc Fork-Bomb

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

(let* ((top   (α (make-erfc-tree-beh)))
       (timer (α (make-timer-beh)))
       (me    (actor _
                (let ((k-show (actor (dt)
                                (send (println)
                                      (format nil "dt = ~,2F"
                                              (* 1e-6 dt))))))
                  (sendx nil timer :stop k-show)))
              ))
  (sendx nil timer :start)
  (sendx t top me 10))

;; ---------------------------------------------------

