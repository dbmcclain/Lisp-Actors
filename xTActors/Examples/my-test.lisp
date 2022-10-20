;; my-test.lisp
;;
;; DM/RAL  2022/10/20 10:09:36
;; ----------------------------------

(defpackage #:my-test
  (:use #:common-lisp #:ac) ;; AC refers to Actors Package
  (:export
   #:echo
   #:receiver
   #:run))

(in-package #:my-test)

;; ----------------------------------

(def-actor receiver    ;; uses DEFLEX, and not DEFPARAMETER
  (lambda (msg)        ;; no customer, so we must be a Sink
     (send fmt-println "~a: ~a~%" :receiver msg)))

(def-actor echo
  (lambda (msg)         ;; again, no customer so we must be a data Sink
    (send fmt-println "~a: ~a~%" :echo msg)
    (send receiver msg)))

(defun run ()
  (send echo :hello))

#|
(run)
 |#
