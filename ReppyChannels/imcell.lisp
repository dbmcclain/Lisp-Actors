;; imcell.lisp -- I and M cells using Reppy's Channels
;; Server handles both I and M cells (any number of them).
;; Provides Make, Set, and Get along with Get event for use in combinators.
;;
;; M-cells are destructive read, setting a full cell is an error,
;; getting an empty cell blocks.
;; (M for Mutable) -- these guarantee serialization of threads reading the cell.
;;
;; I-cells are non-destructive read, setting a full cell is an error,
;; getting an empty cell blocks.
;; (I for Immutable)
;;
;; DM/MCFA  01/00
;; ----------------------------------------------------

(defpackage "IMCELL"
  (:use "USEFUL-MACROS" "COMMON-LISP" "REPPY-CHANNELS")
  (:export "MAKE-MCELL"
           "MAKE-ICELL"
           "SET-CELL"
           "GET-CELL"
           "IM-CELL-ERROR"))

(in-package "IMCELL")

;; ---------------------------------------------------------

(define-condition IM-cell-error (error)
  ((cell :accessor im-cell-error-cell
         :initarg  :cell)
   ))

(defmethod print-object ((err IM-cell-error) stream)
  (format stream "Can't set IM-cell"))

;; ---------------------------------------------------------
(defvar +no-value+ (load-time-value #() t))  ;; unique under eq

(defclass <IM-cell-mixin> ()
  ((val   :accessor IM-cell-val
          :initform (list +no-value+))
   (chan  :accessor IM-cell-chan
          :initform (make-channel))
   ))

(defmethod set-cell ((m <IM-cell-mixin>) val)
  (if (sys:compare-and-swap (car (IM-cell-val m)) +no-value+ val)
      (poke (IM-cell-chan m) val)
    ;; else
    (error (make-condition 'IM-cell-error
                           :cell m))
    ))

;; ---------------------------------------------------------

(defclass M-cell (<IM-cell-mixin>)
  ())

(defun make-mcell ()
  (make-instance 'M-cell))

(defmethod get-cell ((m M-cell) cbfn)
  (sync (wrap (recvEvt (IM-cell-chan m))
              (lambda (ans)
                (setf (car (IM-cell-val m)) +no-value+)
                (funcall cbfn ans)))
        ))

;; -------------------------------------------------------

(defclass I-cell (<IM-cell-mixin>)
  ())

(defun make-icell ()
  (make-instance 'I-cell))

(defmethod get-cell ((i I-cell) cbfn)
  (sync (wrap (recvEvt (IM-cell-chan i))
              (lambda (val)
                (poke (IM-cell-chan i) val)
                (funcall cbfn val)))
        ))


