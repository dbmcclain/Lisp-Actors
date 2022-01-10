(in-package "EDITOR")


;; 17/12/92 AA - rewrite in portable lisp (use do-external-symbols etc.)
;; 17 May 1993, AA - add symbols keyword (a list of symbols from which to
;; choose)
;; DF02August95: return the package as fourth value so COMPLETE-SYMBOL-1 can deal 
;; specially with keywords. (Task-3093 - Problem was SYMBOL-NAME excluding the colon)

;;; Y 6Oct95 symbol-name$symbol -> sys::symbol-name$symbol 
;;; Y 27Jan97 use string= and don't call editor-string

#+:LISPWORKS7
(setf (symbol-function 'complete-symbol)
      (lambda (symbol &key
                      predicate 
                      symbols
                      (default-package *package*)
                      abbreviated
                      return-common-string)
        "Returns 3 values: a list of symbols, the length of the input
symbol name and a common prefix string (if :return-common-string is
set)"
        (block my-top
          (multiple-value-bind (package string external prefix-end)
              (pathetic-parse-symbol symbol default-package)
            ;; Y 18 Nov 2016 on empty string return nothing, because it is unlikely 
            ;; to be whatt he user intended, and it will matchg all visible symbols
            ;; which may take significant amount of time to display. 
            (unless (or package (> (length string) 0))
              (return-from my-top
                (values nil 0 nil default-package)))
            (unless package (setq package default-package))
            
            (let* ((len (length string))
                   (the-predicate (if (eq predicate t)
                                      #'(lambda (x) (or (fboundp x) (boundp x)))
                                    predicate))
                   symbols-to-search)
              (if abbreviated
                  (setq symbols-to-search
                        (let ((*package* package))
                          (let ((list (common-utilities::abbreviated-find-symbols string :packages package :external-only external)))
                            (if the-predicate
                                (remove-if-not the-predicate list)
                              list))))
                (flet ((push-symbol-if-required (s)
                         (let ((sname (symbol-name s)))
                           (when (and (>= (length sname) len)
                                      (string= string  sname  :end2 len)
                                      
                                      (or (not the-predicate) (funcall the-predicate s)))
                             (push s symbols-to-search)))))
                  (cond
                   (symbols
                    (dolist (symbol symbols)
                      (push-symbol-if-required symbol)))
                   (external
                    (do-external-symbols (symbol package)
                      (push-symbol-if-required symbol)))
                   (t
                    (do-symbols (symbol package)
                      (push-symbol-if-required symbol))))))
              
              (let ((third-value 
                     (if (and return-common-string (cdr symbols-to-search))
                         (loop with symbol-names = (mapcar 
                                                    #-:Lucid 'sys::symbol-name$symbol
                                                    #+:Lucid #'symbol-name
                                                    symbols-to-search)
                               with min-common-len = (loop for symbol-name in symbol-names
                                                           minimize (length symbol-name))
                               for non-common-len from len to (1- min-common-len)
                               for first = (char (first symbol-names) non-common-len)
                               do
                               (loop for symbol-name in (cdr symbol-names)
                                     do
                                     (unless (eq (char symbol-name non-common-len)
                                                 first)
                                       (return  (subseq
                                                 (car symbol-names)
                                                 len
                                                 non-common-len))))
                               finally
                               (return
                                (subseq (car symbol-names)
                                        len
                                        non-common-len)))
                       (
                        #-:Lucid sys::symbol-name$symbol
                                 #+:Lucid symbol-name
                                 (car symbols-to-search)))))
                (when prefix-end
                  (incf len prefix-end)
                  (setq third-value (string-append
                                     (subseq symbol 0 prefix-end)
                                     third-value)))
                (values symbols-to-search len third-value package)))))))