(defpackage test-harness
  (:use :cl)
  (:export :test-system))

(in-package :test-harness)

(defun test-system (system)
  "Run the test suite for SYSTEM, then quit the Lisp process with status of those tests

The Lisp process returns 0 on successful tests, -1 otherwise as per
the usual process return semantics."
  (handler-case
      (if (asdf-test-harness:run-suite system)
          (progn
            (format *standard-output* "~&Test-system succeeded for system: `~a`~&" system)
            (uiop:quit 0))
          (progn
            (format *standard-output* "~&Test-system failed for system: `~a`~&" system)
            (uiop:quit -1)))
    (error (e)
      (format *standard-output*
              "~&Testing system `~a` signalled error:~&~a~&" system e)
      (uiop:quit -1))))
