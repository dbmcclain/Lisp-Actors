
(in-package :edec)

(multiple-value-bind (s p)
    (ed-random-pair)
  (format t "~%Fr(~A)" (hex-str s))
  (format t "~%Pt(~A)" (hex-str (ed-compress-pt p))))