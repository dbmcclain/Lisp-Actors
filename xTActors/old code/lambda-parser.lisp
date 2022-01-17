

(in-package :actors/user)

(defstruct lambda-args
  reqd
  opts
  rest
  keys
  allow
  auxs)

#||#
(defun pat-arg-parser-beh (&key reqd)
  (lambda (cust pat)
    (send println pat)
    (match pat
      (()
       (become (pat-arg-parser-beh))
       (send cust (reverse reqd)))

      (sym when (and (symbolp sym)
                     (not (keywordp sym)))
           (become (pat-arg-parser-beh))
           (send cust (reverse (cons sym reqd))))

      (('&optional . more)
       (become (pat-arg-optional-beh :reqd reqd))
       (send self cust more))
      
      (('&rest sym . more) when (symbolp sym)
       (become (pat-arg-rest-beh :reqd (cons sym reqd)))
       (send self cust more))
      
      (('&key . more)
       (become (pat-arg-key-beh :reqd reqd))
       (send self cust more))
      
      ((sym . rest) when (and (symbolp sym)
                              (string= sym "_"))
       (send self cust rest))

      ((kw . rest) when (keywordp kw)
       (send self cust rest))

      ((sym . rest) when (symbolp sym)
       (become (pat-arg-parser-beh :reqd (cons sym reqd)))
       (send self cust rest))

      ((sym . rest) when (atom sym)
       (send self cust rest))

      ((('quote _) . rest)
       (send self cust rest))

      ((('function _) . rest)
       (send self cust rest))

      ((lst . rest)
       (beta (args)
           (send self beta lst)
         (send (make-actor (pat-arg-parser-beh :reqd (reverse args))) cust rest)
         ))

      (_
       (err "invalid arglist: ~S" pat))
      )))

(defun pat-arg-rest-beh (&key reqd)
  (lambda (cust pat)
    (match pat
      (()
       (become (pat-arg-parser-beh))
       (send cust (reverse reqd)))
      (('&key . more)
       (become (pat-arg-key-beh :reqd reqd))
       (send self cust more))
      (_
       (err "invalid arglist: ~S" pat))
      )))

#|
(send (make-actor (pat-arg-parser-beh)) println '(a b (c x . y) . z))
|#
#||#
;; ----------------------------------------------------------------------------------
;; Stanard Ordinary Lambda Lists

(defun parse-aux (args &key reqd opts rest keys allow auxs)
  (match args
    (()
     (make-lambda-args
      :reqd  reqd
      :opts  opts
      :rest  rest
      :keys  keys
      :allow allow
      :auxs  (nreverse auxs)))
    ;; -----------------
    (((sym _) . more) when (symbolp sym)
     (parse-aux more :reqd reqd :opts opts :rest rest :keys keys :allow allow
                :auxs (cons (car args) auxs)))
      ;; -----------------
      ((sym . more) when (symbolp sym)
       (parse-aux more :reqd reqd :opts opts :rest rest :keys keys :allow allow
                  :auxs (cons (car args) auxs)))
      ;; -----------------
      (_
       (error "Invalid arglist: ~S" args))
      ))

(defun parse-allow (args &key reqd opts rest keys)
  (match args
      (()
       (make-lambda-args
        :reqd  reqd
        :opts  opts
        :rest  rest
        :keys  keys
        :allow t))
      ;; -----------------
      (('&aux . more)
       (parse-aux more :reqd reqd :opts opts :rest rest :keys keys :allow t))
      ;; -----------------
      (_
       (error "Invalid arglist: ~S" args))
      ))

(defun parse-keys (args &key reqd opts rest keys)
  (match args
    (()
     (make-lambda-args
      :reqd reqd
      :opts opts
      :rest rest
      :keys (reverse keys)))
    
    ;; -----------------
    (('&allow-other-keys . more)
     (parse-allow more :reqd reqd :opts opts :rest rest :keys (nreverse keys)))
    
    (('&aux . more)
     (parse-aux more :reqd reqd :opts opts :rest rest :keys (nreverse keys)))
    
    ;; -----------------
    (( ((sym aka) _ symp) . more) when (and (symbolp sym)
                                             (symbolp aka)
                                             (symbolp symp))
     (parse-keys more :reqd reqd :opts opts :rest rest
                 :keys (cons (car args) keys)))
    
    ;; -----------------
    (( ((sym aka) _) . more) when (and (symbolp sym)
                                        (symbolp aka))
     (parse-keys more :reqd reqd :opts opts :rest rest
                 :keys (cons (car args) keys)))
    
    ;; -----------------
    (( ((sym aka)) . more) when (and (symbolp sym)
                                    (symbolp aka))
     (parse-keys more :reqd reqd :opts opts :rest rest
                 :keys (cons (car args) keys)))
    
    ;; -----------------
    (( (sym _ symp) . more) when (and (symbolp sym)
                                       (symbolp symp))
     (parse-keys more :reqd reqd :opts opts :rest rest
                 :keys (cons (car args) keys)))
    
    ;; -----------------
    (( (sym _) . more) when (symbolp sym)
     (parse-keys more :reqd reqd :opts opts :rest rest
                 :keys (cons (car args) keys)))
    
    ;; -----------------
    ((sym . more) when (symbolp sym)
     (parse-keys more :reqd reqd :opts opts :rest rest
                 :keys (cons (car args) keys)))
    
    ;; -----------------
    (_
     (error "Invalid arglist: ~S" args))
    ))

(defun parse-rest (args &key reqd opts rest)
  (match args
    (()
     (make-lambda-args
      :reqd reqd
      :opts reqd
      :rest rest))
      ;; -------------
      (('&key . more)
       (parse-keys more :reqd reqd :opts opts :rest rest))

      (('&aux . more)
       (parse-aux more :reqd reqd :opts opts :rest rest))
      ;; -------------
      (_
       (error "Invalid arglist: ~S" args))
      ))
    
(defun parse-optional (args &key reqd opts)
  (match args
    (()
     (make-lambda-args
      :reqd reqd
      :opts (reverse opts)))
    ;; ----------------
    (('&rest sym . more) when (symbolp sym)
     (parse-rest more :reqd reqd :opts (nreverse opts) :rest sym))
    
    (('&key . more)
     (parse-keys more :reqd reqd :opts (nreverse opts)))
    
    (('&aux . more)
     (parse-aux more :reqd reqd :opts (nreverse opts)))
    ;; ----------------
    (( (sym _ symp) . more) when (and (symbolp sym)
                                       (symbolp symp))
     (parse-optional more :reqd reqd :opts (cons (car args) opts)))
    ;; ----------------
    (( (sym _) . more) when (symbolp sym)
     (parse-optional more :reqd reqd :opts (cons (car args) opts)))
    ;; ----------------
    (( sym . more) when (symbolp sym)
     (parse-optional more :reqd reqd :opts (cons sym opts)))
    ;; ----------------
    (_
     (error "Invalid arglist: ~S" args))
    ))

(defun parse-args (args &optional reqd)
  (match args
    (()
     (make-lambda-args
      :reqd (nreverse reqd)))
    
    (('&optional . more)
     (parse-optional more :reqd (nreverse reqd)))
    
    (('&rest sym . more) when (symbolp sym)
     (parse-rest more :reqd (nreverse reqd) :rest sym))
    
    (('&aux . more)
     (parse-aux more :reqd (nreverse reqd)))
    
    (('&key . more)
     (parse-keys more :reqd (nreverse reqd)))
    
    ((sym . more) when (symbolp sym)
     (parse-args more (cons sym reqd)))

    (_
     (error "invalid arglist: ~S" args))
    ))
     
#|
(parse-args '(a b c &rest d &key (e 15) f &allow-other-keys))
|#



       
      

    
    
