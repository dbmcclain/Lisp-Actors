;; lambda-list-parser.lisp -- These functions are used in Actors macros to parse lambda lists
;; These functions need to be compiled ahead of use, so they are in this separate source file.
;;
;; DM/RAL  11/19
;; -------------------------------------------------------------------------

(in-package :actors)

;; ----------------------------------------------------------------------
;; Lambda list parsers

(defun binding-name (arg)
  (if (consp arg)
      (car arg)
    arg))

(defun lambda-keyword-p (arg)
  (member arg '(&optional &key &rest &allow-other-keys)))

(defun parse-lambda-list (parms)
  ;; this function is quick and dirty - it should work properly on
  ;; well formed lambda lists, but all bets are off on abused notation
  ;; and incorrect lambda lists
  (if-let (pos (position-if 'lambda-keyword-p parms))
      (multiple-value-bind (required post) (um:split pos parms)
        (let (optional keys rest)
          (um:nlet-tail iter ((post post))
            (case (pop post)
              ((&optional)
               (if-let (pos (position-if 'lambda-keyword-p post))
                   (multiple-value-bind (pre post) (um:split pos post)
                     (setf optional pre)
                     (iter post))
                 ;; else
                 (setf optional post)
                 ))
            ((&key)
             (if-let (pos (position-if 'lambda-keyword-p post))
                 (multiple-value-bind (pre post) (um:split pos post)
                   (setf keys pre)
                   (iter post))
               ;; else
               (setf keys post)
               ))
            ((&rest)
             (setf rest (car post)))
            ))
          (values required optional keys rest)))
    ;; else
    parms))

#|
(defun parse-and-assemble-args-from-lambda-list (fn parms)
  (multiple-value-bind (required optional restp rest keyp keys)
      (parenscript:parse-lambda-list parms)
    (let* ((lister   (if restp
                         'list*
                       'list))
           (args     (append (mapcar 'binding-name required)
                             (mapcar 'binding-name optional)
                             (mapcan (lambda (arg)
                                       (labels ((pairup (arg)
                                                  `(,(um:kwsymb arg) ,arg)))
                                         (pairup (binding-name arg))))
                                     keys)
                             (when restp
                               (list rest)))))
      `(,lister ',fn '%sk ,@args)
      )))
|#

(defun parse-and-assemble-args-from-lambda-list (fn parms)
  (multiple-value-bind (required optional keys rest)
      (parse-lambda-list parms)
    (let* ((lister   (if rest
                         'list*
                       'list))
           (args     (append (mapcar 'binding-name required)
                             (mapcar 'binding-name optional)
                             (mapcan (lambda (arg)
                                       (labels ((pairup (arg)
                                                  `(,(um:kwsymb arg) ,arg)))
                                         (pairup (binding-name arg))))
                                     keys)
                             (when rest
                               (list rest)))))
      `(,lister ',fn '%sk ,@args)
      )))

(defun method-macro-parms (parms)
  (if-let (pos (position-if 'lambda-keyword-p parms))
      (multiple-value-bind (pre post) (um:split pos parms)
        (append (mapcar 'binding-name pre) post))
    ;; else
    parms))

