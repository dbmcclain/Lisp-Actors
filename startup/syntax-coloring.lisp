;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:syntax-coloring:syntax-coloring.lisp,v 1.4.3.2 2021/08/10 16:07:05 martin Exp $" -*-

;; Copyright (c) 1987--2021 LispWorks Ltd. All rights reserved.

;;;; --------------------------------------------------
;;;;
;;;; Note: LispWorks has built-in syntax coloring for Lisp mode
;;;; buffers.  Before using the example, you need to turn this off by
;;;; M-x Global Font Lock Mode.  If you want to customize the colors
;;;; used by the built-in syntax coloring in the LispWorks IDE, then
;;;; open the Preferences dialog, select Environment and click on the
;;;; Styles tab.
;;;;
;;;; This example demonstrates an implementation of
;;;; syntax coloring using the editor interface.
;;;; To use, compile and load this file. This
;;;; defines two commands:
;;;;     "Color Buffer"  - colors a complete buffer
;;;;     "Color Expression" - colors the enclosing top level expression. 
;;;; Invoke these commands to see the effect. 
;;;; The various *sc-*-face* variables below can
;;;; be modified to get different colors. 
;;;; -----------------------------------------------

(defpackage "SYNTAX-COLORING"
  (:use "EDITOR")
  (:add-use-defaults))



(in-package "SYNTAX-COLORING")

(defvar *sc-string-face*
  (make-face 'sc-string-face
             :foreground :pink4
             :if-exists :overwrite))

(defvar *sc-comment-face*
  (make-face 'sc-comment-face
             :foreground :blueviolet
             :if-exists :overwrite))


(defvar *sc-keyword-face*
  (make-face 'sc-keyword-face
             :foreground :purple
             :if-exists :overwrite))

(defvar *sc-function-name-face*
  (make-face 'sc-function-name-face
             :foreground :red2
             :if-exists :overwrite))

(defvar *sc-variable-name-face*
  (make-face 'sc-variable-name-face
             :foreground :red3
             :underline-p t
             :if-exists :overwrite))

(defvar *sc-type-face*
  (make-face 'sc-type-face
             :foreground :pink2
             :if-exists :overwrite))

(defvar *sc-builtin-face*
  (make-face 'sc-builtin-face
             :foreground :pink2
             :if-exists :overwrite))




(defun point-forward (point)
  (character-offset point 1))
(defun point-backwards (point)
  (character-offset point -1))

(defun point-character (point)
  (character-at point 0))

(defun is-symbol-character (x)
  (not (or (get-macro-character x)
           (whitespace-char-p x))))


(defun sc-fontify-region (start end &optional verbose)
  (let* ((buffer (point-buffer start)))
    (when verbose
      (message "Fontifying ~s..." (buffer-name buffer)))
    (remove-text-properties-no-edit start end '(face nil))
    (lisp-sc-fontify-syntactically-region start end)
    (lisp-sc-fontify-key-words-region start end)
    (when verbose
      (message "Fontifying ~s...done" (buffer-name buffer)))))

(defun sc-unfontify-region (start end)
  (remove-text-properties-no-edit start end '(face nil)))

(defun sc-fontify-buffer (buffer)
  (sc-fontify-region (buffers-start buffer) (buffers-end buffer) t))

(defun sc-unfontify-buffer (buffer)
  (sc-unfontify-region (buffers-start buffer) (buffers-end buffer)))

(defun sc-apply-highlight (start end face)
  (with-point ((check start :temporary))
    (flet ((boring-face-p (face1 face2)
                          (declare (ignore face1))
                          (or (eq face2 nil)
                              (eq face2 face))))
      (declare (dynamic-extent #'boring-face-p))
      (unless (text-property-not-all check 'face :dont-care
                                     :limit end
                                     :test #'boring-face-p)
        (put-text-property-no-edit start end 'face face)))))



(defvar *sc-maximum-size* (* 10 1024 1024))





(defcommand "Color Buffer" (p &optional (buffer (current-buffer)))
  "Fontify the current buffer the way `font-lock-mode' would."
  (declare (ignorable p))
  (sc-fontify-buffer buffer))

(defcommand "Color Expression" (p )
  "Fontify the current buffer the way `font-lock-mode' would."
  (let ((point  (current-point)))
  (unless (and  (start-line-p point)
               (eq (print (point-character point)) #\())
    (beginning-of-defun-command p))

  (with-point ((start point)(end point))
    (form-offset end 1)
    (sc-fontify-region start end nil))))


(defun sc-apply-highlight-to-form (form-start form-end start end face)
  (let ((highlight-start (if (point< form-start start) start form-start))
        (highlight-end (if (point> form-end end) end form-end)))
    (and (point< highlight-start highlight-end)
         (sc-apply-highlight
          highlight-start highlight-end face))))

(defun lisp-sc-fontify-syntactically-region (start end)
  (let ((point (buffer-point (point-buffer start))))
    (move-point point start)
    (with-point ((form-end start :temporary))
      (let ((pattern (load-time-value (precompile-regexp "[;\"|#\\]"))))
        (loop (unless (regular-expression-search point pattern :limit end :to-end nil)
                (return))
              (ecase (character-at point 0)
                (#\; (move-point form-end point)
                     (line-end form-end)
                     (sc-apply-highlight point form-end *sc-comment-face*)
                     (unless (line-offset point 1 0)
                       (return)))
                (#\" (move-point form-end point)
                     (unless (form-offset form-end 1)
                       (return))
                     (sc-apply-highlight-to-form point form-end start end
                                                          *sc-string-face*)
                     (move-point point form-end))
                (#\|
                 (unless  (form-offset point 1) (return)))
                (#\#
                 (let ((prev-char (character-at point -1)))
                   (if (and prev-char
                            (and (not (whitespace-char-p prev-char))
                                 (not (member prev-char
                                              '(#\" #\) #\( #\' #\` #\, #\:)))))
                       (character-offset point 1)
                     (case (character-at point 1)
                       (#\|
                        (move-point form-end point)
                        (unless (form-offset form-end 1)
                          (return))
                        (sc-apply-highlight-to-form point form-end start end
                                                           *sc-comment-face*)
                        (move-point point form-end))
                       (#\\
                        (unless (form-offset point 1)
                          (return)))
                       ((#\+ #\-)
                        (move-point form-end point)
                        (unless (form-offset form-end 1)
                          (return))
                        (sc-apply-highlight-to-form point form-end start end
                                                           *sc-comment-face*)
                        (move-point point form-end))
                       (otherwise
                        (point-forward point))))))
                (#\\ (unless (character-offset point 2)
                       (return)))))))))

(defparameter *lisp-mode-control-structures-fsa*
  (precompile-regexp
   (string-append
    "("
    (regexp-opt '("when" "unless" "cond" "if"
                         "case" "ecase" "typecase" "etypecase"
                         "loop" "loop-finish" "do" "do*" "dotimes" "dolist"
                         "do-symbols" "do-external-symbols" "do-all-symbols"
                         "proclaim" "declaim" "declare"
                         "flet" "labels" "macrolet"
                         "block" "return" "return-from"
                         "catch" "throw"
                         "handler-case" "handler-bind"
                         "restart-case" "restart-bind" "with-simple-restart"
                         "let" "let*" "prog" "destructuring-bind"
                         "multiple-value-bind" "multiple-value-prog1" "values"
                         "progn" "progv" "prog1" "prog2"
		         "inline" "unwind-protect"
		         "eval-when"
                         "with-slots" "with-accessors"
                         "with-input-from-string" "with-output-to-string"
                         "with-open-file" "with-open-stream"
                         "with-hash-table-iterator" "with-package-iterator"
                         "with-compilation-unit" "with-standard-io-syntax"
                         "pprint-logical-block" "print-unreadable-object")
                t)
    "\\>")))


;;; Key words include specialforms, some important marcos
;;;  names inside defining forms like DEFUN, and keywords.


(defun lisp-sc-fontify-key-words-region (start end)
  (let ((point (buffer-point (point-buffer start))))
    (move-point point start )
    ;;;; First part - highlight defining macros and the names
    (with-point ((form-start start :temporary)
                 (form-end start :temporary))
      (let ((pattern (load-time-value (precompile-regexp "^(def[^ 	]*[ 	]+"))))
        ;;;; Find a line starting with (def
        (loop (let ((found-len (regular-expression-search point pattern :limit end :to-end nil)))
                (unless found-len
                  (return))
                (point-forward point)   ;;skip opening bracket
                (move-point form-end point)
                (unless
                    (and
                     (form-offset form-end 1) ;;; move to the end of the DEF* word
                     (progn
                       (sc-apply-highlight point form-end *sc-keyword-face*) ;; highlight the DEF*
                       (let ((name-face 
                              (cond ((looking-at "defun\\|defmacro\\|defmethod\\|defgeneric"
                                                 point)
                                     *sc-function-name-face*)
                                    ((looking-at "defvar\\|defparameter"
                                                 point)
                                     *sc-variable-name-face*)
                                    (t
                                     *sc-type-face*))))
                         (character-offset point (1- found-len))
                         (move-point form-start point)  ;;; start of name
                         (let ((setf-len (looking-at "(setf[ 	]+" point)))
                           (cond (setf-len    ;;; defining (setf xxxx) skip the (setf part
                                  (character-offset point setf-len)) 
                                 ((eq (point-character point) #\( ) ;;; unknown defining form with 
                                  (point-forward form-start)        ;;; name starying wth #(
                                  (point-forward point)))
                           ;;; skip the name
                           (loop (let ((next (point-character point)))
                                   (unless next
                                     (return))
                                   (unless (is-symbol-character next)
                                     (return))
                                   (point-forward point)))
                           (when setf-len   ;;; (setf xxx) form, check for the closing paren
                             (let ((next (point-character point)))
                               (if (and next
                                        (eq next #\) ))
                                   (point-forward point)
                                 (move-point form-start point))))
                           (when (point> point form-start)
                             (sc-apply-highlight form-start point name-face))))
                       (point-backwards point)
                       (form-offset point 1)))
                  (unless (line-offset point 1 0)
                    (return)))))))
  (move-point point start)

  ;;; Second part - highlight keywrods and lambda list special symbols (&optional etc.)

  (with-point ((form-start start :temporary))
    (let ((pre-pattern-buf-start (load-time-value (precompile-regexp ".[^\")('`,]")))
          (pre-pattern (load-time-value (precompile-regexp "[^][&$%^!?><.{}~+=_@*a-z0-9:#-].[^\")('`,]")))
          (pattern (load-time-value (precompile-regexp "[:&]"))))
      (loop (cond ((not (regular-expression-search point pattern :limit end :to-end nil))
                   (return))
                  ((if (point-backwards point)
                       (and (looking-at pre-pattern point)
                            (progn
                              (point-forward point)
                              t))
                     (looking-at pre-pattern-buf-start point))
                   (move-point form-start point)
                   (let ((face (if (eql (point-character form-start) #\:)
                                   *sc-builtin-face*
                                 *sc-type-face*)))
                     (flet ((boring-face-p (face1 face2)
                              (declare (ignore face1))
                              (or (eq face2 nil)
                                  (eq face2 face))))
                       (declare (dynamic-extent #'boring-face-p))
                       ;; MJS 30Mar01: checking the face here avoids problems with
                       ;; form-offset on malformed things inside comments etc (already
                       ;; given a face).
                       (if (boring-face-p :dont-care (get-text-property form-start 'face))
                           (progn

                             (unless  (form-offset point 1)
                               (return))
                             (sc-apply-highlight form-start point face))
                         (unless (text-property-any point 'face :dont-care
                                                    :test #'boring-face-p)
                           (return))))))
                  (t (unless (character-offset point 2)(return)))))))
  (move-point point start)

  ;;; Third part - highlight all "known" words (listed in 
  ;;; *lisp-mode-control-structures-fsa*), which include special
  ;;; and CL macros

  (with-point ((form-start start :temporary))
    (let ((pattern *lisp-mode-control-structures-fsa*))
      (loop (let ((length (regular-expression-search point pattern :limit end :to-end nil)))
              (unless length
                (return))
              (move-point form-start point)
              (point-forward form-start)
              (character-offset point length)
              (sc-apply-highlight form-start point *sc-keyword-face*)))))))


