
(in-package :cps)

(defun tst-nlet (n)
  (nlet iter ((n  n))
    (when (plusp n)
      (iter (1- n)))))

(defun tst-nlet-tail (n)
  (nlet-tail iter ((n  n))
    (when (plusp n)
      (iter (1- n)))))

(defun tst-tlet (n)
  (=tlet iter ((n n))
    (when (plusp n)
      (iter (1- n)))))

;; Interpreted code comparison
;; =TLET is 33% faster than NLET-TAIL !!
(time (tst-nlet      1000000)) ;; => blows stack
(time (tst-nlet-tail 1000000)) ;; 4.67s
(time (tst-tlet      1000000)) ;; 3.48s

;; Compiled code comparison
;; NLET-TAIL is 128x faster than -TLET
;; NLET-TAIL is 2x faster than NLET
(time (tst-nlet      1000000000))  ;; 2.64s
(time (tst-nlet-tail 1000000000))  ;; 1.22s
(time (tst-tlet        10000000))  ;; 1.57s
