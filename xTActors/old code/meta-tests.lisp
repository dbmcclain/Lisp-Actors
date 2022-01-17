
(in-package :meta)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun xdigit-char-p (d)
    (digit-char-p d *read-base*))
  
  (deftype xdigit ()
    `(satisfies xdigit-char-p))
)

(eval-when (:compile-toplevel :execute)
  (enable-meta-syntax))

#+:LISPWORKS
(editor:setup-indent "with-string-meta" 1)

(defun parse-int (string &aux (s +1) d (n 0) (base #10r10))
  (with-string-meta (buffer string)
    (and
     (let ((*read-base* #10r10))
       (match
           [{[#\# {[{#\x #\X} !(setq base 16)]
                   [{#\o #\O} !(setq base  8)]
                   [{#\b #\B} !(setq base  2)]
                   [[@(digit d) !(setq base (digit-char-p d 10))]
                    $[@(digit d) !(setq base (+ (* 10 base) (digit-char-p d 10)))]
                    {#\r #\R}]
                  }]
             []}]))
     (not (assert (< 1 base #10r37)))
     (let ((*read-base* base))
       (match
         [ {#\+ [#\- !(setq s -1)] []}
         @(xdigit d) !(setq n (digit-char-p d base))
         ${#\_ ;; allow digit spacer
           [@(xdigit d) !(setq n (+ (* n base) (digit-char-p d base)))]
          }]))
     (* s n))))

(eval-when (:compile-toplevel :execute)
  (disable-meta-syntax))

#|
(parse-int "1_200_354")
(parse-int "#x1_200_354")
(parse-int "#10r100")
(parse-int "#36rdbm")
|#
