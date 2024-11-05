;; fix-walker.lisp -- As per Tim Bradshaw
;; ;; DM/RAL  2024/11/05 16:45:33 UTC
;;
;; A fix to errant walker behavior which fails to agree with compiler
;; regarding symbol-macrolet symbols in the context of implied
;; tagbodies for DO, and DOTIMES
;; --------------------------------------------

(in-package :walker)

(defun walk-tagbody-1 (form context env)
 (and form
      (recons form
              (if (or (symbolp (car form)) (integerp (car form)))
                  (walk-template (car form) 'quote context env)
                (let ((result (walk-form-internal (car form) context env)))
                  (if (or (symbolp result) (integerp result))
                      ;; Stop spurious tags leaking into the
                      ;; expansion
                      (relist (car form) 'progn result)
                    result)))
              (walk-tagbody-1 (cdr form) context env))))
