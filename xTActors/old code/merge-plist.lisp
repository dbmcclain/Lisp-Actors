

(defun merge-plist (new-plist old-plist)
  ;; return a plist that has all the new content plus all the old
  ;; content that wasn't mentioned in the new.
  (labels ((trim (plist ans)
             (if plist
                 (destructuring-bind (key val &rest tl) plist
                   (if (eq #'trim (getf ans key #'trim))
                       (trim tl (list* key val ans))
                     (trim tl ans)))
               ans)))
    (trim old-plist (trim new-plist nil))))

#|
(merge-plist '(:a 1 :b 2 :c 3) '(:a 10 :a 20 :c 13 :d 15))
|#

(defun merge-alist (new-alist old-alist)
  (labels ((trim (alist ans)
             (if alist
                 (destructuring-bind (hd . tl) alist
                   (if (assoc (car hd) ans)
                       (trim tl ans)
                     (trim tl (cons hd ans))))
               ans)))
    (trim old-alist (trim new-alist nil))))

#|
(merge-alist '((:a . 1) (:b . 2) (:c . 3)) '((:a . 10) (:a . 20) (:c . 13) (:d . 15)))
|#

;; -----------------------------------------------------

(defclass collection ()
  ())

(defclass ordered-collection (collection)
  ())

(defclass rb-tree (ordered-collection)
  ((map  :accessor rb-tree-map  :initarg :tree  :initform (maps:empty))))

(defclass alist (collection)
  ((lst  :accessor alist-list   :initarg :list  :initform nil)))

(defclass plist (collection)
  ((lst  :accessor plist-list   :initarg :list  :initform nil)))

(defclass set (rb-tree)
  ())

(defclass map (rb-tree)
  ())

;; Any chosen heirarchy is arbitrary. There are many others that could work too.
        