
(in-package :cached-var)

;; -----------------------------------------------------------------------------
;;

(defmacro def-cached-var (name creator &optional cache-name)
  (let ((cname (or cache-name
                   (intern (format nil "*~A*" (string name)))))
        (new   (gensym)))
    `(progn
       (defvar ,cname nil)
       (defun ,name ()
         (or ,cname
             (let ((,new ,creator))
               (if (mpcompat:CAS ,cname nil ,new)
                   ,new
                 ,cname))))
       )))

#+:LISPWORKS
(editor:setup-indent "def-cached-var" 1)

(defun get-cached-symbol-data (sym key1 key2 fn-compute)
  ;;
  ;; defines a generalized 2-level cache lookup, defined in the
  ;; symbol-plist of sym
  ;;
  ;; key1 = category
  ;; key2 = instance
  ;;
  (let* ((alst  (get sym key1))
         (item  (cdr (assoc key2 alst))))
    (or item
        (let ((item (funcall fn-compute)))
          (setf (get sym key1)
                (acons key2 item alst))
          item))))

