;;
;; scheme coin - a common lisp blockchain
;;
;; Burton Samograd
;; 2017
;; hacked on by DM/RAL 12/17
;; ---------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defpackage #:scheme-blockchain
  (:use #:common-lisp)
  (:export
   ))

(in-package #:scheme-blockchain)

#|
(load "~/quicklisp/setup.lisp")
|#

(defconstant *coin-name* "Scheme Coin")

#|
(eval-when (compile load)
  (ql:quickload "ironclad"))
|#

(defun rest2 (l)
  (cddr l))

(defun interp (x &optional env)
  "Interpret (evaluate) the expression x in the environment env."
  (cond
   ((symbolp x) (get-var x env))
   ((atom x) x)
   ((scheme-macro (first x))
    (interp (scheme-macro-expand x) env))
   ((case (first x)
      (QUOTE (second x))
      (BEGIN (last1 (mapcar #'(lambda (y) (interp y env))
                            (rest x))))
      (SET! (set-var! (second x) (interp (third x) env) env))
      (if (if (interp (second x) env)
              (interp (third x) env)
            (interp (fourth x) env)))
      (LAMBDA (let ((parms (second x))
                    (code (maybe-add 'begin (rest2 x))))
                #'(lambda (&rest args)
                    (interp code (extend-env parms args env)))))
      (t ;; a procedure application
         (apply (interp (first x) env)
                (mapcar #'(lambda (v) (interp v env))
                        (rest x))))))))

(defun scheme-macro (symbol)
  (and (symbolp symbol) (get symbol 'scheme-macro)))

(defmacro def-scheme-macro (name parmlist &body body)
  `(setf (get ',name 'scheme-macro)
         #'(lambda ,parmlist .,body)))

(defun scheme-macro-expand (x)
  (if (and (listp x) (scheme-macro (first x)))
      (scheme-macro-expand
       (apply (scheme-macro (first x)) (rest x)))
    x))

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
    (set-global-var! var val))
  val)

(defun get-var (var env)
  (if (assoc var env)
      (second (assoc var env))
    (get-global-var var)))

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound scheme variable: ~A" var)
      val)))

(defun extend-env (vars vals env)
  "Add some variables and values to and environment."
  (nconc (mapcar #'list vars vals) env))

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member 
      (null? null) (eq? eq) (equal? equal) (eqv? eql)
      (write prin1) (display princ) (newline terpri)))

(defun init-scheme-interp ()
  (mapc #'init-scheme-proc *scheme-procs*)
  (set-global-var! t t)
  (set-global-var! nil nil))

(defun init-scheme-proc (f)
  (if (listp f)
      (set-global-var! (first f) (symbol-function (second f)))
    (set-global-var! f (symbol-function f))))

(defun maybe-add (op exps &optional if-nil)
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defun length=1 (x) 
  (and (consp x) (null (cdr x))))

(defun last1 (list)
  (first (last list)))

(defun scheme ()
  (init-scheme-interp)
  (loop (format t "~&==> ")
        (print (interp (read) nil))))

(def-scheme-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin . ,body)
    `(let (,(first bindings))
       (let* ,(rest bindings) . ,body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(init-scheme-interp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; and there we have a scheme interpreter with macros. ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct sblock
  (index 0) (timestamp 0) data (previous-hash "") hash)

(defstruct transaction 
  from to (value 0) (accuracy 1)
  (duration 0)
  data hash previous-hash)

(defun to-byte-array (x)
  (loenc:encode x)
  #|
  (let ((retval (make-array 0 :adjustable t 
                            :fill-pointer t 
                            :element-type '(unsigned-byte 8))))
    (map 'nil (lambda (c) (vector-push-extend (char-code c) retval))
         (let ((*print-base* 16))
           (format nil "~A" x)) )
    (coerce retval 'ironclad::simple-octet-vector))
  |#
  )

(defun make-address (x)
  (let ((digester (ironclad:make-digest :sha3)))
    (ironclad:update-digest digester
                            (to-byte-array x))
    (ironclad:produce-digest digester)))

(defun hash-block (block)
  (let ((digester (ironclad:make-digest :sha3)))
    (dolist (accessor-fn '(sblock-index
                           sblock-timestamp
                           sblock-data
                           sblock-previous-hash))
      (ironclad:update-digest digester
                              (to-byte-array (funcall accessor-fn block))))
    (ironclad:produce-digest digester)))

(defun hash-transaction (block)
  (let ((digester (ironclad:make-digest :sha3)))
    (dolist (accessor-fn '(transaction-from
                           transaction-to
                           transaction-value
                           transaction-accuracy
                           transaction-duration
                           transaction-data))
      (ironclad:update-digest digester
                              (to-byte-array (funcall accessor-fn block))))
    (ironclad:produce-digest digester)))

(defun make-genesis-block (data time)
  (let* ((block (make-sblock 
                 :index 0
                 :timestamp time
                 :data data
                 :hash 0))
         (hash (hash-block block)))
    (setf (sblock-hash block) hash)
    block))

(defmacro create-genesis-block (data)
  `(make-genesis-block ,data (get-universal-time)))

(defun next-block (last-block data)
  (let ((block (make-sblock :index         (1+ (sblock-index last-block))
                            :timestamp     (get-universal-time)
                            :data          data
                            :previous-hash (hash-block last-block))))
    (setf (sblock-hash block) (hash-block block))
    (push  block *blockchain*)
    block))
                            
;; (setf *print-base* 16)

(defconstant *base-code* '(set! x 0))

(defparameter *network-address*  (make-address *coin-name*))
(defparameter *quester-address*  (make-address "quester"))
(defparameter *miner-address*    (make-address "miner"))
(defparameter *contract-address* (make-address "contract"))

(defparameter *block-transactions*
  (let ((transaction (make-transaction :from  *network-address*
                                       :to    *quester-address*
                                       :value (* 10000 10000 10000)
                                       :data  *base-code*)))
    (setf (transaction-hash transaction)
          (hash-transaction transaction))
    (list transaction)))

(defparameter *blockchain* 
  (list (create-genesis-block *block-transactions*)))

(defparameter *previous-block* (car *blockchain*))

(defparameter *solved-transactions* (make-hash-table :test #'equalp
                                                     :weak-kind t))
(eval-when (compile load)
  (defun new-transaction (&key from to (value 0) accuracy data 
                               previous-hash duration)
    (let ((transaction (make-transaction :from from :to to :value value
                                         :accuracy accuracy :data data 
                                         :previous-hash previous-hash
                                         :duration duration)))
      (setf (transaction-hash transaction)
            (hash-transaction transaction))
      (when previous-hash
        (setf (gethash
               (transaction-hash transaction)
               *solved-transactions*)
              t))
      transaction)))

(defmacro submit-answer (from transaction data)
  `(push (new-transaction :from ,from :to *contract-address*
                          :previous-hash  (transaction-hash transaction)
                          :data ,data)
         *block-transactions*))

(defun has-transaction-not-been-solved (transaction)
  (let ((hash (transaction-hash transaction)))
    (if (gethash hash *solved-transactions*)
        (not (setf (gethash hash *solved-transactions*)
                   transaction))
      t)))

(defun viable-transaction (transaction)
  (and (has-transaction-not-been-solved transaction)
       (<= (sblock-index (car *blockchain*))
           (or (transaction-duration transaction) 
               (get-universal-time))))) ;; can still submit

(defun verify-transaction (transaction)
  (handler-case
      (interp (transaction-data transaction))
    (error (e) e)))
  
(defun execute-transactions (miner-address)
  (dolist (transaction *block-transactions*)
    (when (viable-transaction transaction)
      (print :submitting-answer)
      (submit-answer miner-address transaction     
                     (verify-transaction transaction))
      )))

(defmacro transfer (from to value)
  `(push (new-transaction :from ,from :to ,to
                          :value ,value)
         *block-transactions*))

(defun mine ()
  (when *block-transactions*
    (execute-transactions *miner-address*)
    (transfer *network-address* *miner-address* 1)
    (setf *previous-block* 
          (next-block *previous-block* *block-transactions*))
    (setf *block-transactions* nil)))
  
(defmacro execute (from value code &key (accuracy value) 
                        (duration (+ 2 (sblock-index (car *blockchain*)))))
  `(push (new-transaction :from ,from :to *contract-address*
                          :value ,value
                          :accuracy ,accuracy :data ',code 
                          :duration ,duration)
         *block-transactions*))

(defun process-transfer-request (request stream)
  (destructuring-bind (from to value)
      request
    (transfer from to value)))

(defun process-execute-request (request stream)
    (destructuring-bind (from value data &key (accuracy value) 
                              (duration (+ 2 (sblock-index (car *blockchain*))))) 
        request
      (execute from value data :accuracy accuracy :duration duration)))

(defun process-blocks-request (request stream)
  (let ((*print-base* 16))
    (print *blockchain* stream)))

(defun process-coin-server-request (stream)
  (let ((request (read stream)))
    (case request
      (transfer (process-transfer-request (cdr request) stream))
      (execute (process-execute-request (cdr request) stream))
      (blocks (process-blocks-request (cdr request) stream)))))

(Defun coin-server (handle)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle
                               :direction :io
                               :element-type
                                  'base-char)))
    (process-coin-server-request stream)))

(defvar *server* (comm:start-up-server :function #'coin-server
                                       :service 9999
                                       :process-name 
                                       (format nil "~A server" *coin-name*)))

(loop
 (mine)
 (sleep 1))
