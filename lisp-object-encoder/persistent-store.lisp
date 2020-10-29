;; persistent-store.lisp
;; --------------------------------------------------------------------------------------
;; Persistent global store using portable encoding / decoding for Lisp objects
;; Simple-minded for things like *PRINT-CIRCLE*, *PRINT-LENGTH*, etc. Not OODBMS.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
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

;; ------------------------------------------
(in-package :persistent-store)
;; ------------------------------------------

(defclass persistent-store ()
  ((directory           :accessor persistent-store-directory
                        :initarg :directory)
   (items               :accessor persistent-store-items
                        :initform (make-hash-table))
   (dirty               :accessor persistent-store-dirty
                        :initform nil)
   ))

;; --------------------------

(defun make-persistent-store (&optional
                              (directory (merge-pathnames
                                          "persistent-store/"
                                          #+:LISPWORKS
                                          (hcl:get-working-directory)
                                          #+:ALLEGRO
                                          (excl:current-directory))))
  (make-instance 'persistent-store
                 :directory directory))

;; --------------------------

(defvar $default-persistent-store (make-persistent-store))
(defvar $persistent-filename      "persistent-store.sdps")

;; --------------------------

(defvar $unbound #(:unbound))

;; --------------------------

(defmethod persist-in-store ((store persistent-store) (sym symbol))
  (if (constantp sym)
      (error "Symbol is constant: ~A. Can't persist" sym)
    (with-accessors ((store-items  persistent-store-items)
                     (store-dirty  persistent-store-dirty)) store
      (rps:add-dependent 'rps-mark-dirty sym) ;; so we can track dirty using (setf (value 'sym) xx)
      (unless (gethash sym store-items)
        (setf (get sym 'persistent-store) store
              (gethash sym store-items) (if (boundp sym)
                                            (symbol-value sym)
                                          $unbound)
              store-dirty t) ))))

(defmethod persist-in-store ((store persistent-store) (items list))
  (let ((ngsyms nil))
    (dolist (sym items)
      (if (and (symbolp sym)
               (not (constantp sym)))
          (persist-in-store store sym)
        ;; else
        (push sym ngsyms)))
    (when ngsyms
      (error "Can't persist ~A" (nreverse ngsyms)))
    ))

;; --------------------------

(defun persist (item &optional (store $default-persistent-store))
  (persist-in-store store item))

;; --------------------------

(defmethod mark-dirty ((sym symbol))
  (um:when-let (store (get sym 'persistent-store))
    (setf (persistent-store-dirty store) t)))

(defun rps-mark-dirty (sym at old-val new-val)
  (declare (ignore at old-val new-val))
  (mark-dirty sym))

;; --------------------------

(defmethod unpersist ((sym symbol))
  (um:when-let (store (get sym 'persistent-store))
    (with-accessors ((store-items  persistent-store-items)
                     (store-dirty  persistent-store-dirty)) store
      (rps:remove-dependent 'rps-mark-dirty sym)
      (remhash sym store-items)
      (setf store-dirty t)
      (remprop sym 'persistent-store))
    ))

;; --------------------------

(defun convert-to-uint32 (str)
  (do ((val 0)
       (ix  0 (1+ ix)))
      ((>= ix 4) val)
    (setf val (+ (ash val 8) (char-code (char str ix))))))

;; --------------------------

(defvar $persistent-magic (convert-to-uint32 "SDPS"))

;; --------------------------

(defmethod commit-store ((store persistent-store))
  (with-accessors ((store-items     persistent-store-items)
                   (store-dirty     persistent-store-dirty)
                   (store-directory persistent-store-directory)) store
    (when store-dirty
      (let* ((store-filename (merge-pathnames $persistent-filename
                                              store-directory))
             (bak-filename (merge-pathnames (make-pathname
                                             :name (pathname-name store-filename)
                                             :type "bak")
                                            store-filename))
             (tmp-filename (merge-pathnames
                            (um:mkstr (mpcompat:generate-uuid))
                            store-filename))
             (new-items    (make-hash-table)))
        
        (ensure-directories-exist tmp-filename)
        (with-open-file (file tmp-filename
                              :direction :output
                              :if-exists :rename
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
          (let ((items         nil)
                (unbound-items nil))
            (maphash (lambda (sym val)
                       (declare (ignore val))
                       (if (boundp sym)
                           (let* ((val (symbol-value sym)))
                             (setf items (acons sym val items)
                                 (gethash sym new-items) val))
                         (setf unbound-items (cons sym unbound-items)
                               (gethash sym new-items) $unbound)))
                     store-items)
            (loenc:serialize (list items unbound-items) file
                             :use-magic $persistent-magic)
            ))
        (if (probe-file bak-filename)
            (delete-file bak-filename))
        (if (probe-file store-filename)
            (rename-file store-filename bak-filename))
        (rename-file tmp-filename store-filename)
        (setf store-items new-items
              store-dirty nil)
        ))))

(defun commit (&optional (store $default-persistent-store))
  (commit-store store))

;; --------------------------

(defmethod retrieve-store ((store persistent-store))
  (with-accessors ((store-items     persistent-store-items)
                   (store-dirty     persistent-store-dirty)
                   (store-directory persistent-store-directory)) store
    
    (let ((store-filename (merge-pathnames $persistent-filename
                                           store-directory)))
      (when (probe-file store-filename)
        (with-open-file (file store-filename
                              :direction :input
                              :element-type '(unsigned-byte 8))
          (destructuring-bind (items unbound-items) 
              (loenc:deserialize file
                                 :use-magic $persistent-magic)
            (let ((tbl (make-hash-table)))
              
              (dolist (pair items)
                (destructuring-bind (sym &rest val) pair
                  (rps:add-dependent 'rps-mark-dirty sym)
                  (setf (gethash sym tbl) val
                        (symbol-value sym) val
                        (get sym 'persistent-store) store) ))
              
              (dolist (sym unbound-items)
                (rps:add-dependent 'rps-mark-dirty sym)
                (setf (gethash sym tbl) $unbound
                      (get sym 'persistent-store) store)
                (makunbound sym))
              
              (setf store-items tbl
                    store-dirty nil)
              )))) )))


(defun retrieve (&optional (store $default-persistent-store))
  (retrieve-store store))

;; --------------------------

(defmethod revert ((sym symbol))
  (revert (list sym)))

(defmethod revert ((items list))
  (let ((ngsyms nil))
    (dolist (sym items)
      (let ((store nil))
        (if (and (symbolp sym)
                 (not (constantp sym))
                 (setf store (get sym 'persistent-store)))
            (let ((val (gethash sym (persistent-store-items store))))
              (if (eq val $unbound)
                  (makunbound sym)
                (setf (symbol-value sym) val)) )
          ;; else
          ;; collect symbols not persisted for later error message.
          ;; continue with rest of list making those reversions that you can.
          (push sym ngsyms)
          )))
      
    (when ngsyms
      (error "Symbols ~A not persisted. Can't revert their values." (nreverse ngsyms)))
    ))
  
;; --------------------------

;; --------------------------

;; --------------------------

#|
;; test it out

(setf x 15 a 32 b :this c "THAT" my-pi pi)
(list x a b c my-pi)

(persist '(x a b c my-pi))

(commit)

(setf x 0 a 0 b 0 c 0 my-pi 0)
(list x a b c my-pi)

(unpersist 'my-pi)

(retrieve)
(list x a b c my-pi)

(setf my-pi (* 2 pi))
(mark-dirty 'my-pi)
(list x a b c my-pi)

(retrieve)
(list x a b c my-pi)


(setf my-pi pi)
(list x a b c my-pi)
(revert 'my-pi)

(revert '(two-pi my-pi a))

|#