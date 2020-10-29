;; lambda-list-parser.lisp -- These functions are used in Actors macros to parse lambda lists
;; These functions need to be compiled ahead of use, so they are in this separate source file.
;;
;; DM/RAL  11/19
;; -------------------------------------------------------------------------

(in-package :cps)

;; ----------------------------------------------------------------------
;; Lambda list parsers

(defun binding-name (arg)
  (if (consp arg)
      (car arg)
    arg))

(defun lambda-keyword-p (arg)
  (member arg '(&optional &key &rest &allow-other-keys &aux)))

(defun parse-lambda-list (parms)
  ;; this function is quick and dirty - it should work properly on
  ;; well formed lambda lists, but all bets are off on abused notation
  ;; and incorrect lambda lists
  (um:if-let (pos (position-if 'lambda-keyword-p parms))
      (multiple-value-bind (required post) (um:split pos parms)
        (let ((optional (list nil))
              (keys     (list nil))
              (aux      (list nil))
              rest)
          (um:nlet-tail iter ((post post))
            
            (flet ((extract (var)
                     (um:if-let (pos (position-if 'lambda-keyword-p post))
                         (multiple-value-bind (pre new-post) (um:split pos post)
                           (setf (car var) pre)
                           (iter new-post))
                       ;; else
                       (setf (car var) post))))
                                 
              (case (pop post)
                ((&optional)
                 (extract optional))
                
                ((&key)
                 (extract keys))
                
                ((&rest)
                 (setf rest (car post))
                 (iter (cdr post)))

                ((&aux)
                 (extract aux))

                ((&allow-other-keys)
                 (iter post))
                )))
          (values required (car optional) (car keys) rest (car aux))))
    ;; else
    parms))

(defun parse-and-assemble-args-from-lambda-list (fn parms)
  ;; form a parameter calling form from a lambda list
  (multiple-value-bind (required optional keys rest)
      (parse-lambda-list parms)
    (let* ((lister   (if rest
                         'list*
                       'list))
           (args     (nconc (mapcar 'binding-name required)
                            (mapcar 'binding-name optional)
                            (mapcan (lambda (arg)
                                      (flet ((pairup (arg)
                                               `(,(um:kwsymb arg) ,arg)))
                                        (pairup (binding-name arg))))
                                    keys)
                            (when rest
                              (list rest)))))
      `(,lister ',fn '%sk ,@args)
      )))

(defun method-macro-parms (parms)
  (um:if-let (pos (position-if 'lambda-keyword-p parms))
      (multiple-value-bind (pre post) (um:split pos parms)
        (append (mapcar 'binding-name pre) post))
    ;; else
    parms))

