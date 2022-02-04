;; generic-operations.lisp - Predicative Dispatch of Generic Arithmetic Ops
;;
;; DM/RAL  02/22
;; ---------------------------------------------------------------------------

(in-package :propagators)

;; ----------------------------------------------
;; Generic Operators using Predicative Dispatch

(defmacro make-generic-operator (name arity default-oper)
  (lw:with-unique-names (args)
  `(defun ,name (&rest ,args)
     (apply 'apply-generic-operator ',name ,arity ,default-oper ,args))
  ))

(defun apply-generic-operator (name arity default-oper &rest args)
  (unless (eql (length args) arity)
    (error "Generic Operator ~A requires ~D args" name arity))
  (let ((oper (or (find-if (lambda (gop)
                             (destructuring-bind (tests fn) gop
                               (and (every 'funcall tests args)
                                    fn)))
                           (get name 'generic-opers))
                  default-oper)))
    (apply oper args)))

(defun assign-operation (name fn &rest preds)
  (setf (get name 'generic-opers)
        (cons (list preds fn) (get name 'generic-opers))))

(deflex the-contradiction (list 'contradiction))

(defun contradictory? (x) (eq x the-contradiction))

(make-generic-operator 'merge-info 2 (lambda (content increment)
                                       (if (default-equal? content increment)
                                           content
                                         the-contradiction)))

(defun any? (x)
  (declare (ignore x))
  t)

(assign-operation 'merge-info
                  (lambda (content increment)
                    (declare (ignore increment))
                    content)
                  'any? 'nothing?)

(assign-operation 'merge-info
                  (lambda (content increment)
                    (declare (ignore content))
                    increment)
                  'nothing? 'any?)

(assign-operation 'merge-info
                  (lambda (content increment)
                    (let ((new-range (intersect-intervals content increment)))
                      (cond ((interval-equal? new-range content) content)
                            ((interval-equal? new-range increment) increment)
                            ((empty-interval? new-range) the-contradiction)
                            (t new-range)
                            )))
                  'interval? 'interval?)

(defun ensure-inside (interval number)
  (if (<= (interval-lo interval) number (interval-hi interval))
      number
    the-contradiction))

(defun number? (x)
  (realp x))

(assign-operation 'merge-info
                  (lambda (content increment)
                    (ensure-inside increment content))
                  'number? 'interval?)

(assign-operation 'merge-info
                  (lambda (content increment)
                    (ensure-inside content increment))
                  'interval? 'number?)

;; --------------------------------

(make-generic-operator generic-+    2 '+)
(make-generic-operator generic--    2 '-)
(make-generic-operator generic-*    2 '*)
(make-generic-operator generic-/    2 '/)
(make-generic-operator generic-sq   1 'sq)
(make-generic-operator generic-sqrt 1 'sqrt)

(defprop adder      generic-+)
(defprop subtractor generic--)
(defprop multiplier generic=*)
(defprop divider    generic-/)
(defprop squarer    generic-sq)
(defprop sqrter     generic-sqrt)

(assign-operation 'generic-+    'add-interval 'interval? 'interval?)
(assign-operation 'generic--    'sub-interval 'interval? 'interval?)
(assign-operation 'generic-*    'mul-interval 'interval? 'interval?)
(assign-operation 'generic-/    'div-interval 'interval? 'interval?)
(assign-operation 'generic-sq   'sq-interval 'interval? 'interval?)
(assign-operation 'generic-sqrt 'sqrt-interval 'interval? 'interval?)


