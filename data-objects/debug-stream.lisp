;; debug-stream.lisp -- A useful item for showing debugging printouts
;;
;; DM/RAL  06/07
;; ----------------------------------------------------------------------
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

(in-package debug-stream)

;; -----------------------------------------------------------

(defclass <dbg-stream> (capi:interface)
  ((text-pane :accessor text-pane  :initarg :text-pane)
   ))

(defmethod output-stream-of ((intf <dbg-stream>))
  (capi:collector-pane-stream (text-pane intf)))

(defun make-debug-stream (&key (name (gensym))
                               display
                               (title (format nil "Debug Output - ~A" name))
                               always-on-top)
  (let* ((pane (make-instance 'capi:collector-pane
                              :visible-min-width  '(:character 80)
                              :visible-min-height '(:character 25)
                              :echo-area          t
                              :graphics-options nil
                              :name name
                              ;;:background :black
                              ;;:foreground :yellow
                              #+:WIN32
                              :font
                              #+:WIN32
                              (gp:make-font-description
                               :family "Courier New" ;; "Lucida Console"
                               :size   8 ;; 9
                               :slant  :roman
                               :weight :normal)
                              ))
         (intf (make-instance '<dbg-stream>
                              :layout (make-instance 'capi:column-layout
                                                     :description (list pane))
                              :text-pane pane
                              :title title
                              :name  name
                              :window-styles
                              (append '(:textured-background
                                        :moveable-by-window-background)
                                      (if always-on-top
                                          (list :always-on-top))
                                      ))))
    (when display
      (capi:display intf))
    intf))

(defmethod capi:pane-popup-menu-items :around ((pane capi:collector-pane) (intf <dbg-stream>))
  (append (call-next-method)
          `(,(make-instance 'capi:menu-item
                            :selection-callback 'clear
                            :text "Clear"))
          ))
                                        
                                                           
(defmethod debug-print ((intf <dbg-stream>) obj)
  (capi:execute-with-interface intf
                               (lambda ()
                                 (let ((*print-length* nil)
                                       (stream (output-stream-of intf)))
                                   (capi:display intf)
                                   (princ obj stream)
                                   (terpri stream)
                                   (force-output stream)))))

;; -------------------------------------

(defun find-named-debug-window (name)
  (find name (capi:collect-interfaces '<dbg-stream>)
        :test 'equalp
        :key  'capi:capi-object-name))


(defmethod debug-print (name obj)
  (let ((intf (or (find-named-debug-window name)
                  (make-debug-stream :name name
                                     :display t))
              ))
    (debug-print intf obj)))
    
(defun pr (intf obj)
  (debug-print intf obj))

;; ------------------------------------

(defmethod clear (name)
  (let ((intf (find-named-debug-window name)))
    (if intf
        (clear intf))))

(defmethod clear ((intf <dbg-stream>))
  (capi:execute-with-interface intf
                               (lambda ()
                                 (clear (text-pane intf)))
                               ))

(defmethod clear ((pane capi:collector-pane))
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (setf (capi:editor-pane-text pane) ""))
                              ))

(defun cls (dbg)
  (clear dbg))
