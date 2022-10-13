;; managed-buffers.lisp
;; --------------------------------------------------------------------------------------
;; A system for managing cached buffers
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
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

;; ------------------------------------------------------------
(in-package #:com.ral.managed-buffers)
;; -------------------------------------------------------------

(defconstant $MIN-MANAGED-BUFFER-SIZE 4)
(defconstant $MAX-MANAGED-BUFFER-SIZE 4095)

(defun make-buffer (nb)
  "Internal routine to make a buffer of size nb bytes. The size will
already have been rounded up to the next power of 2 equal to or larger
than the original requested size."
  (make-array nb
              :element-type '(unsigned-byte 8)
              :fill-pointer 0
              :adjustable   t))

;; ------------------------------------

(defvar *buffer-queues* (make-priq))

(defun init-buffer-queues ()
  (setf *buffer-queues* (make-priq)))

(defun get-buffer (nel)
  "User visible routine to return a buffer of nel bytes in length. All
recyclable buffers are categorized as corresponding to powers of 2 in
size. Buffers larger than a power of 2 are stored in the queue for the
next lower power of 2.

The first available buffer equal to or larger than the requested size
is handed back. If none are available in the power of 2 size nearest
the requested size, then queues for larger sizes are searched. The
caller will receive a buffer at least as large as, or larger than,
requested.

The fill pointer of the buffer will have been set by this routine to
the actual requested size before handing back to the caller. But the
buffers are all stretchy vectors, so if necesary, they can be grown in
use.

If no buffers are available then a new one is manufactured for the
caller."
  (let* ((nb (max (um:ceiling-pwr2 nel)
                  $MIN-MANAGED-BUFFER-SIZE))
         (arr (or (find-first-fit nb)
                  (make-buffer nb))))
    (setf (fill-pointer arr) nel)
    arr))

(defun find-first-fit (nb)
  (um:nlet iter ((ix nb))
    (when (<= ix $max-managed-buffer-size)
      (or (popq *buffer-queues* :prio ix)
          (go-iter (ash ix 1))))
    ))

(defun recycle-buffer (buf)
  "Recycle the buffer back into our queue of available buffers if the
size of the buffer is equal to or smaller than 4096 bytes. Larger
buffers are simply discarded as garbage. All buffers have an actual
physical size which will correspond to queues representing the nearest
lower power of 2.

So for example, if the buffer was originally obtained with a size of
128 bytes, but grown during use to 280 bytes, then we will recycle
that buffer into the queue representing size 256.

No buffer should be recycled unless it was first obtained by calling
get-buffer. None of our managed buffers is manufactured as smaller
than 128 bytes. If a buffer is recycled, having grown larger than 4096
bytes, we simply discard it."
  (let* ((nb (array-total-size buf))
         (ix (um:floor-pwr2 nb)))
    (when (<= $min-managed-buffer-size ix $max-managed-buffer-size)
      (addq *buffer-queues* buf :prio ix))
    ))

;; -----------------------------------------------------------------

(defun do-with-temporary-buffer (fn len)
  (let ((tmp (and (integerp len)
                  (get-buffer len))))
    (unwind-protect
        (funcall fn tmp)
      (when tmp
        (recycle-buffer tmp)))))

(defmacro with-temporary-buffer ((buf-name &optional len) &body body)
  `(do-with-temporary-buffer (lambda (,buf-name) ,@body) ,len))

