;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package #:com.ral.useful-macros)
;; -------------------------------------------------------------------

(defun map-array (fn arr)
  (let ((new  (make-array (array-dimensions arr)
                          :element-type (array-element-type arr)
                          :adjustable   (adjustable-array-p arr)
                          :fill-pointer (and (adjustable-array-p arr)
                                             (fill-pointer arr)))
              ))
    (if (cdr (array-dimensions arr))
        (let ((rma-src (make-array (array-total-size arr)
                                   :element-type (array-element-type arr)
                                   :displaced-to arr
                                   :displaced-index-offset 0))
              (rma-dst (make-array (array-total-size arr)
                                   :element-type (array-element-type arr)
                                   :displaced-to new
                                   :displaced-index-offset 0)))
          (map-into rma-dst fn rma-src))
      ;; else
      (map-into new fn arr))
    new))

(defun map-tree (fn tree)
  (cond ((consp tree)
         (cons (map-tree fn (car tree))
               (map-tree fn (cdr tree))))
        (t
         (funcall fn tree))
        ))

(defun skip-underscore (atom)
  (cond ((null atom)
         nil)
        ((arrayp atom)
         (map-array (um:curry #'map-tree #'skip-underscore) atom))
        ((symbolp atom)
         (or (um:read-extended-number-syntax (symbol-name atom))
             atom))
        (t
         atom)
        ))

(defmacro with-extended-number-syntax (&body body)
  `(progn
     ,@(map-tree #'skip-underscore body)))


;; --------------------------------------------

(defun patched-numreader (stream subch pref prev-fn)
  (let ((tok  (make-array 16
                          :element-type 'character
                          :adjustable   t
                          :fill-pointer 0)))
    (um:nlet iter ()
      (let ((ch  (read-char stream nil stream)))
        (cond ((eq ch stream))
              ((char= #\_ ch)
               (go-iter))
              ((or (whitespace-char-p ch)
                   (get-macro-character ch))
               (unread-char ch stream))
              (t
               (vector-push-extend ch tok)
               (go-iter))
              )))
    (with-input-from-string (s tok)
      (funcall prev-fn s subch pref))
    ))

(defun patch-numreader (subch)
  (set-dispatch-macro-character #\# subch
                                (um:rcurry #'patched-numreader
                                           (get-dispatch-macro-character #\# subch))))

(unless (fboundp 'do-nothing)
  (defun do-nothing (&rest ignored)
    (declare (ignore ignored))))


(progn
  (patch-numreader #\x)
  (patch-numreader #\o)
  (patch-numreader #\b)
  (patch-numreader #\r)
  (setf (symbol-function 'patch-numreader) #'do-nothing))

;; --------------------------------------------
#|
#|
(print #x1_00)
(multiple-value-list
 (ignore-errors
   (eval (read-from-string "(list 'x 11:23)"))))

(handler-bind
    ((conditions:package-not-found-reader
      (lambda (c)
        (inspect c))))
  (read-from-string "(list 'x 11:23 'y)"))

(multiple-value-list
 (ignore-errors
   (error "What")
   ))
|#

(defun sexi-read (stream hrs)
  (let ((tok  (make-array 16
                          :element-type 'character
                          :adjustable   t
                          :fill-pointer 0)))
    (loop for ch across hrs do (vector-push-extend ch tok))
    (vector-push-extend #\: tok)
    (um:nlet iter ()
      (let ((ch  (read-char stream nil stream)))
        (cond ((eq ch stream))
              ((or (whitespace-char-p ch)
                   (find ch #(#\( #\) )))
               (unread-char ch stream))
              (t
               (vector-push-extend ch tok)
               (go-iter))
              )))
    (or (read-extended-number-syntax tok)
        (intern tok))
    ))

;; --------------------------------------------

#+nil ;; #+:LISPWORKS
(progn
  (lw:defadvice (read ext-num :around)
      (stream &rest args)
    (handler-case
        (let ((ans (apply #'lw:call-next-advice stream args)))
          (or (and (symbolp ans)
                   (read-extended-number-syntax (symbol-name ans)))
              ans))
      (conditions:package-not-found-reader (c)
        (sexi-read stream (slot-value c 'package)))
      ))

  (lw:defadvice (read-preserving-whitespace ext-num :around)
      (stream &rest args)
    (handler-case
        (let ((ans (apply #'lw:call-next-advice stream args)))
          (or (and (symbolp ans)
                   (read-extended-number-syntax (symbol-name ans)))
              ans))
      (conditions:package-not-found-reader (c)
        (sexi-read stream (slot-value c 'package)))
      ))

  #+nil
  (lw:defadvice (eval ext-num :around)
      (exp)
    (handler-case
        (lw:call-next-advice exp)
      (unbound-variable (c)
        ;; (inspect c)
        (or (read-extended-number-syntax (symbol-name (cell-error-name c)))
            (error c)))
      )))

;; --------------------------------------------

#+:SBCL
(defun special-reader (next-fn stream &rest args)
  (handler-case
      (let ((ans (apply next-fn stream args)))
        (or (and (symbolp ans)
                 (read-extended-number-syntax (symbol-name ans)))
            ans))
    (SB-INT:SIMPLE-READER-PACKAGE-ERROR (c)
      (sexi-read stream (slot-value c 'package)))
    ))

#+:SBCL
(defun special-eval (next-fn expr)
  (handler-case
      (funcall next-fn expr)
    (unbound-variable (c)
      (or (read-extended-number-syntax (symbol-name (cell-error-name c)))
          (error c)))
    ))
    
#+nil ;; #+:SBCL
(sb-ext:with-unlocked-packages (:cl)
     (cl-advice:make-advisable 'cl:read
                               :arguments '(stream &rest args)
                               :force-use-arguments t)
     (cl-advice:make-advisable 'cl:read-preserving-whitespace
                               :arguments '(stream &rest args)
                               :force-use-arguments t)
     (cl-advice:make-advisable 'cl:eval
                               :arguments '(expr)
                               :force-use-arguments t)
     (cl-advice:add-advice :around 'cl:read  #'special-reader)
     (cl-advice:add-advice :around 'cl:read-preserving-whitespace #'special-reader)
     (cl-advice:add-advice :around 'cl:eval #'special-eval))

;; --------------------------------------------
#|
(read-from-string "11:23")
(read-from-string "2026/05/23")
(list 2026/05/23)
|#                         
                          
|#
