
(in-package :edec)

(loop repeat 10000 do
      (assert (< (ed-random-pair) *ed-r*)))