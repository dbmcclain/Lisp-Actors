;; progress-bar.lisp -- a more general purpose progress bar with user cancel button
;;
;; DM/RAL 06/07
;; ----------------------------------------------------------------------------
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

(in-package #:progress-bar)

(capi:define-interface progress-bar-without-cancel ()
  ((modulo  :accessor progress-bar-modulo :initarg  :modulo   :initform   t)
   (bar-min :accessor progress-bar-min    :initarg  :bar-min  :initform   0)
   (bar-max :accessor progress-bar-max    :initarg  :bar-max  :initform 100))
  
  (:panes
   (progress-bar-pane
    capi:progress-bar
    :start bar-min
    :end   bar-max
    :slug-start 0
    :visible-min-width 300
    :visible-max-width 300
    :accessor progress-bar-pane))
  
  (:layouts
   (main-layout
    capi:column-layout
    '(progress-bar-pane)
    :adjust :right))
  
  (:default-initargs
   :layout  'main-layout
   :window-styles '(#|:borderless|#
                    :never-iconic
                    :textured-background ;; only works if :borderless is false
                    :moveable-by-window-background
                    :shadowed
                    :always-on-top)
   ;;:transparency 0.5
   ;;:title   "FSync Scanning..."
   :title "Please Wait..."
   :auto-menus nil
   :confirm-destroy-function (constantly nil)
   ))

(defmethod initialize-instance :after ((obj progress-bar-without-cancel) &key &allow-other-keys)
  obj)

(defmethod progress-bar-cancel ((intf progress-bar-without-cancel))
  nil)

;; -------------------------------------------------------------------------

(capi:define-interface progress-bar-with-cancel ()
  ((cancel  :accessor progress-bar-cancel :initform nil)
   (modulo  :accessor progress-bar-modulo :initarg  :modulo   :initform   t)
   (bar-min :accessor progress-bar-min    :initarg  :bar-min  :initform   0)
   (bar-max :accessor progress-bar-max    :initarg  :bar-max  :initform 100))
  
  (:panes
   (progress-bar-pane
    capi:progress-bar
    :start bar-min
    :end   bar-max
    :slug-start 0
    :visible-min-width 300
    :visible-max-width 300
    :accessor progress-bar-pane)
   (cancel-button
    capi:push-button
    :accessor      progress-bar-cancel-button
    :data          "Cancel"
    :callback      'kill-scan
    :callback-type :interface))
  
  (:layouts
   (main-layout
    capi:column-layout
    '(progress-bar-pane
      cancel-button)
    :adjust :right))
  
  (:default-initargs
   :layout  'main-layout
   :window-styles '(#|:borderless|#
                    :never-iconic
                    :textured-background ;; only works if :borderless is false
                    :moveable-by-window-background
                    :shadowed
                    :always-on-top)
   ;;:transparency 0.5
   ;;:title   "FSync Scanning..."
   :title "Please Wait..."
   :auto-menus nil
   :confirm-destroy-function (constantly nil)
   ))

(defmethod initialize-instance :after ((obj progress-bar-with-cancel) &key &allow-other-keys)
  obj)

(define-condition user-cancel (condition) ())

(defun kill-scan (intf)
  (setf (progress-bar-cancel intf) t))

(defun incr-value (intf &optional (dbar 1))
  ;; called only from within the capi pane process
  (if (progress-bar-cancel intf)
      (error 'user-cancel)
    (capi:execute-with-interface 
     intf
     (lambda ()
       (let* ((bar    (progress-bar-pane intf))
              (new-ct (+ dbar (capi:range-slug-start bar)))
              (limit  (capi:range-end bar)))
         (when (>= new-ct limit)
           (setf new-ct 
                 (if (progress-bar-modulo intf)
                     (mod new-ct limit)
                   limit)))
         (setf (capi:range-slug-start bar) new-ct))
       ))
    ))

(defun set-value (intf val)
  ;; called only from within the capi pane process
  (if (progress-bar-cancel intf)
      (error 'user-cancel)
    (let ((bar (progress-bar-pane intf)))
      (capi:execute-with-interface 
       intf
       (lambda ()
         (setf (capi:range-slug-start bar) val))
       ))))

(defun do-with-progress-bar (fn initargs)
  (let ((bar (capi:display
              (apply 'make-instance
                     (if (getf initargs :allow-cancel)
                         'progress-bar-with-cancel
                       'progress-bar-without-cancel)
                     initargs))))
    (unwind-protect
        (funcall fn bar)
      (capi:execute-with-interface bar
                                   'capi:destroy bar)
      )))

(defmacro with-progress-bar ((bar-name &rest initargs) &body body)
  `(do-with-progress-bar (lambda (,bar-name)
                           ,@body)
                         (list ,@initargs)))

#+:LISPWORKS
(editor:setup-indent "with-progress-bar" 1)
