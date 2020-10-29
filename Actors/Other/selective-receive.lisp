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


(in-package :fac)

(defun do-recv (select-fn timeout)
  (let ((self (current-actor)))
    (unless (get-property self 'has-recv)
      (let (old-fn
            selector-fn
            (msgq (priq:make-unsafe-fifo))
            timer)
        (setf old-fn
              (become
               (dlambda
                 (:recv-{204E1756-D84E-11E7-9D93-985AEBDA9C2A} (sel-fn timeout)
                   (setf selector-fn sel-fn)
                   (when timeout
                     (unless timer
                       (setf timer (mp:make-timer #'send self
                                                  :timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A})))
                     (mp:schedule-timer-relative timer timeout)))
                 (:timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A} ()
                  (error "RECV Timeout"))
                 (t (&rest msg)
                    (if selector-fn
                        (if-let (ans-fn (funcall selector-fn msg))
                            (progn
                              (when timer
                                (mp:unschedule-timer timer))
                              (setf selector-fn nil)
                              (funcall ans-fn)
                              (foreach (lambda (msg)
                                         (apply self msg))
                                       (priq:contents msgq)))
                          ;; else - not one of the messages we are looking for
                          (priq:addq msgq msg))
                      ;; else -- not currently in a RECV
                      (apply old-fn msg)))
                 ))
              (get-property self 'has-recv) t)))
    (funcall self :recv-{204E1756-D84E-11E7-9D93-985AEBDA9C2A} select-fn timeout)))
