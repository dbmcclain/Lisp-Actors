
;; packages.lisp
;; DM/RAL  02/09
;; ------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :cl-user)

(defpackage #:ref ;; for V3
  (:use #:common-lisp)
  (:import-from :um
   #:rmw
   #:rd
   #:wr
   #:rd-object
   #:wr-object
   #:rmw-object
   #:cas-object
   #:atomic-exch-object)
  (:export
   #:ref
   #:ref-val
   #:val
   #:wval
   #:ref-p
   #:rd
   #:wr
   #:rmw
   #:rd-object
   #:wr-object
   #:rmw-object
   #:cas-object
   #:atomic-exch-object
   #:cow
   #:cow-p
   #:clone
   #:ielt
   #:islot-value
   #:iaref
   #:alist
   #:alist-lst
   #:tree
   #:tree-lst
   ))

(defpackage #:priq
  (:use #:common-lisp #:def*)
  #-OPENMCL (:import-from :mpcompat
   :CAS)
  (:export
   #:unsafe-lifo
   #:lifo
   #:unsafe-fifo
   #:fifo
   #:unsafe-priq
   #:priq
   #:mailbox
   #:prio-mailbox

   #:make-unsafe-lifo
   #:make-lifo
   #:make-unsafe-fifo
   #:make-fifo
   #:make-unsafe-priq
   #:make-priq
   #:addq
   #:popq
   #:peekq
   #:emptyq-p
   #:contents
   #:findq
   #:lastq
   #:countq

   #:make-mailbox
   #:make-prio-mailbox
   #:mailbox-send
   #:mailbox-read
   #:mailbox-peek
   #:mailbox-empty-p
   #:mailbox-not-empty-p
   #:mailbox-discard
   
   #:locked-exec
   ))

