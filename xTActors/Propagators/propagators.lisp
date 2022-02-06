;; propagators.lisp -- An implementation of Radul and Sussman Propagators using Actors
;; from "The Art of the Propagator", Alexey Radul and Gerald Jay Sussman, MIT-CSAIL-TR-2009-002, Jan 26, 2009.
;;
;; DM/RAL 02/22
;;
;; Actors represent an ideal platform for Propagators, since they are
;; inherently asynchronous. However, a network of propagators with
;; feedback faces the possibility of never-ending computation. So
;; asking for the results of a network computation may depend on when
;; you ask.
;;
;; The public face of a Propagator Network is a collection of CELLS
;; (Actors), which contain values that can be queried and updated with
;; new information.
;;
;; Hidden PROPAGATORS (Actors) connect CELLS together with
;; transforming functions to form a Propagator Network. A PROPAGATOR
;; can have any number of input CELLS and just one output CELL.
;;
;; By conventon, PROPAGATORS are set up via oridinary functions
;; relating the CELL argumens. The last CELL argument is the output
;; CELL, and all the others are input CELLS.
;;
;; Whenever a CELL value is updated, it triggers the PROPAGATORS
;; connecting the CELL to other CELLS. The PROPAGATORS query each
;; input CELL for its value, and if the collection of input CELLS
;; altogether have useful values, then the PROPAGATOR computes a
;; function of these values and updates its output CELL. That may, in
;; turn, trigger other PROPAGATORS.
;;
;; --------------------------------------------------------------------------------

(in-package :cl-user)

(defpackage :propagators
  (:use :cl :ac :def*)
  (:export
   ))

(in-package :propagators)

;; ---------------------------------------------
;; The unique value representing no contents

(defconstant nothing (list 'the-nothing))

(defun nothing? (thing)
  (eql thing nothing))

;; ---------------------------------------------

(defun basic-cell-beh (propagators content)
  ;; cell's internal content is an interval. And this accommodates
  ;; both intervals and numbers.
  (alambda
   ((:new-propagator new-propagator)
    (unless (member new-propagator propagators)
      (become (basic-cell-beh (cons new-propagator propagators) content))))

   ((:add-content increment)
    (cond ((nothing? increment))
          ((nothing? content)
           (become (basic-cell-beh propagators increment))
           (send-to-all propagators))
          ((not (default-equal? content increment))
           (error "Ack! Inconsistency!"))
          ))

   ((cust :content)
    (send cust content))
   ))

(defun cell (&optional (value nothing))
  (make-actor (basic-cell-beh nil value)))

(defmacro defcell (name &optional (value 'nothing))
  `(deflex ,name (cell ,value)))

;; ----------------------------------------

(defgeneric default-equal? (a b)
  (:method (a b)
   (eql a b))
  (:method ((a rational) b)
   (default-equal-rational? b a))
  (:method ((a number) b)
   (default-equal-number? b a)))

(defvar *tolerance* 1e-8)

(defgeneric default-equal-number? (b a)
  (:method (b a)
   nil)
  (:method ((b number) a)
   (< (abs (- a b)) *tolerance*)))

(defgeneric default-equal-rational? (b a)
  (:method (b a)
   nil)
  (:method ((b rational) a)
   (eql a b))
  (:method ((b number) a)
   (< (abs (- a b)) *tolerance*)))

;; -----------------------------------------

(defun konst-beh (propagators content)
  (alambda
   ((:new-propagator new-propagator)
    (unless (member new-propagator propagators)
      (become (konst-beh (cons new-propagator propagators) content))
      ))

   ((cust :content)
    (send cust content))
   ))

(defun konst (val)
  (make-actor (konst-beh nil val)))

(defmacro defkonst (name val)
  `(deflex ,name (konst ,val)))

;; -----------------------------------------
;; User (REPL) Interface to CELLS. Actors use message sends.

(defun add-content (cell val)
  (send cell :add-content val))

(defun content (cell)
  (ask cell :content))

;; -----------------------------------------

(defun attach-propagator (prop &rest cells)
  (send-to-all cells :new-propagator prop)
  (send prop))

(defun attach-propagator-fn (fn &rest cells)
  (let* ((output (car (last cells)))
         (inputs (butlast cells))
         (prop   (make-actor
                  (lambda ()
                    (β  args
                        (send par β inputs :content)
                      (unless (some 'nothing? args)
                        (send output :add-content (apply fn args))))
                    ))))
    (apply 'attach-propagator prop inputs)))

(defmacro defprop (name fn)
  (lw:with-unique-names (cells)
    `(defun ,name (&rest ,cells)
       (apply 'attach-propagator-fn ',fn ,cells))
    ))

;; ---------------------------------------------------------

(defun conditional (p if-true if-false output)
  (let ((prop  (make-actor
                (lambda ()
                  (β  (pred)
                      (send p β :content)
                    (unless (nothing? pred)
                      (β  (val)
                          (send (if pred
                                    if-true
                                  if-false)
                                β :content)
                        (unless (nothing? val)
                          (send output :add-content val)))
                      )))
                )))
    (attach-propagator prop p if-true if-false)))

(defun switch (predicate if-true output)
  (conditional predicate if-true (cell) output))

(defun do-compound-propagator (to-build &rest cells)
  (let ((prop (make-actor
               (lambda ()
                 (β args
                     (send par β cells :content)
                   (unless (every 'nothing? args) ;; prevent infinite recursion
                     (funcall to-build)
                     (become (sink-beh)))
                   ))
               )))
    (apply 'attach-propagator prop cells)))

(defmacro compound-propagator ((&rest cells) &body body)
  `(do-compound-propagator (lambda ()
                             ,@body)
                           ,@cells))

;; ------------------------------------------------
;; Arithmetic Propagators

(defun sq (x)
  (* x x))

(defprop adder      +)
(defprop subtractor -)
(defprop multiplier *)
(defprop divider    /)
(defprop squarer    sq)
(defprop sqrter     sqrt)
(defprop absolute-value abs)
(defprop inverter   not)
(defprop <?         <)

;; -------------------------------------------------------
;; Proceed to manual execution of section-2.lisp

