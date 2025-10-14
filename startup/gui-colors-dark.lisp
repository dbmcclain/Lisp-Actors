#|
The MIT License

Copyright (c) 2018 Emotiq AG

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

(defun set-pane-background-colors (x)
  (typecase x
    (capi:echo-area-pane
     (setf (capi:simple-pane-background x) :gray80 ;; (color:make-rgb 0.0 0.33 0.33)
           (capi:simple-pane-foreground x) :black))

    (capi:collector-pane
     (setf (capi:simple-pane-background x) :black
           (capi:simple-pane-foreground x) (color:make-rgb .2 1.0 .2)))

    (capi:listener-pane
     (setf (capi:simple-pane-background x) :black
           (capi:simple-pane-foreground x) (color:make-rgb 0.8 0.8 .2)))
    
    (capi:editor-pane
     (setf (capi:simple-pane-background x) :black
           (capi:simple-pane-foreground x) (color:make-rgb 0.8 0.8 0.2)))
    
    (capi:tab-layout
     (mapcar 'set-pane-background-colors (capi:tab-layout-panes x)))
;    (capi:output-pane
;     (setf (capi:simple-pane-background x) :black
;        (capi::simple-pane-foreground x) :white))
    ))

(let ((*HANDLE-WARN-ON-REDEFINITION* :warn)
      (*redefinition-action* :warn))
  (defmethod capi:interface-display :before ((self lw-tools:listener))
    (capi:map-pane-descendant-children
     self 'set-pane-background-colors))
  (defmethod capi:interface-display :before ((self lw-tools::help-interface))
    (capi:map-pane-descendant-children
     self 'set-pane-background-colors))
  (defmethod capi:interface-display :before ((self lw-tools:editor))
    (capi:map-pane-descendant-children
     self 'set-pane-background-colors))
  (defmethod capi:interface-display :before ((self lw-tools:output-browser))
    (capi:map-pane-descendant-children
     self 'set-pane-background-colors))
  (defmethod capi:interface-display :before ((self lw-tools:shell))
    (capi:map-pane-descendant-children
     self 'set-pane-background-colors))
  (defmethod capi:interface-display :before ((self lw-tools:inspector))
    (capi:map-pane-descendant-children
     self 'set-pane-background-colors))
  )

#|
(progn
  ;; (editor::set-parenthesis-colours '(:forestgreen))
  ;; (setf editor::*parenthesis-font-faces* t)
  (setf editor::*font-lock-function-name-face*
        (editor::make-face 'fond-lock-function-name-face
                           :foreground (color:make-rgb 0.4 0.4 1.0)
                           :if-exists :overwrite))
  )
|#

#||#
(progn
  (setf editor::*font-lock-variable-name-face*
        (editor::make-face 'fond-lock-variable-name-face
                           :foreground (editor::create-dark-background-switchable-color :forestgreen :forestgreen)
                           :if-exists :overwrite))
  )
#||#

