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

(in-package :ecc-crypto-b571)

(defvar *snippy-start* "---- START OF LICENSE BLOCK ----")
(defvar *snippy-stop*  "---- END OF LICENSE BLOCK ----")

(defun acudora-shares-folder ()
  (merge-pathnames "Acudora/shares/"
                   (sys:get-folder-path :appdata)))

(defun write-share-file (share-pt share-filename)
  (with-open-file (share-file share-filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (let ((snip "~%--- SNIP HERE ---~%"))
      (format share-file ";; machine generated -- do not modify")
      (format share-file snip)
      (write-sequence
       (encode-bytes-to-base64 share-pt)
       share-file)
      (format share-file snip))))
  
(defvar *clas-user-share-file*
  "com.Acudora.audiounit.CLAS")

(defvar *vTuning-user-share-file*
  "com.Acudora.audiounit.vTuning")

(defvar *vDBM-user-share-file*
  "com.Acudora.audiounit.vDBM")

(defun save-user-share-file (bundle-id pt)
  (let ((filename (merge-pathnames
                   (concatenate 'string "." bundle-id)
                   (acudora-shares-folder))))
    (ensure-directories-exist filename)
    (write-share-file pt filename)))

(defun decode-and-save-user-shares (shares-enc)
  (let* ((from (+ (length *snippy-start*) (search *snippy-start* shares-enc)))
         (to   (search *snippy-stop* shares-enc :start2 from)))
    (destructuring-bind (pt-clas pt-vTun pt-vDBM)
        (loenc:decode
         (decode-bytes-from-base64
          (subseq shares-enc from to)))
      (save-user-share-file *clas-user-share-file*    pt-clas)
      (save-user-share-file *vTuning-user-share-file* pt-vTun)
      (save-user-share-file *vDBM-user-share-file*    pt-vDBM) )))

;; --------------------------------------------------------------------

(defclass msg-editor-pane (capi:editor-pane)
  ())

(capi:define-interface messaging-intf ()
  ()
  (:panes
   (msg-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "EMail Message"
                     :title "EMail Message"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 10)
                     :visible-max-height t
                     :accessor msg-text-pane)

   (msg-save-license-button capi:push-button
                            :text "Save License"
                            :callback 'save-license)

   (cancel-button capi:push-button
                  :text "Cancel"
                  :callback (lambda (x intf)
                              (declare (ignore x))
                              (capi:destroy intf)))
   )
  
  (:layouts

   (row-layout1
    capi:row-layout
    '(msg-save-license-button
      nil
      cancel-button))

   (column-layout1
    capi:column-layout
    '(msg-text-pane
      row-layout1))
   )
  (:menu-bar edit-menu)
  (:menus
   (edit-menu
    "Edit"
    (("Cut"        :data :cut  )
     ("Copy"       :data :copy )
     ("Paste"      :data :paste)
     ("Select All" :data :select-all
                   :accelerator "accelerator-a"))
    :callback 'do-menu-item
    #|:callback-type :item|#))

  (:default-initargs
   :layout 'column-layout1
   :title "Acudora Licensing"
   :best-width 400
   :window-styles '( #+:COCOA :textured-background
                              :movable-by-window-background)
   ))

(defun do-menu-item (item intf)
  (let ((edpane (msg-text-pane intf)))
    (case item
      (:select-all
       (capi:call-editor edpane "Mark Whole Buffer"))
      (:copy
       (capi:call-editor edpane "Copy To Cut Buffer"))
      (:cut
       (capi:call-editor edpane "Copy To Cut Buffer")
       (capi:call-editor edpane "Delete Region"))
      (:paste
       (capi:call-editor edpane "Delete Region")
       (capi:call-editor edpane "Insert Cut Buffer"))
      )))

(defmethod capi:pane-interface-select-all ((pane msg-editor-pane) intf)
  (when (eq pane (msg-text-pane intf))
    (capi:call-editor pane "Mark Whole Buffer")))
     
(defmethod capi:interface-keys-style ((intf msg-editor-pane))
  #+:MACOSX :mac
  #+:WIN32  :pc)

(defun make-saver-intf ()
  (setf sys:*stack-overflow-behaviour* nil)
  (let ((intf (capi:display (make-instance 'messaging-intf))))
    intf))

(defun save-license (x intf)
  (declare (ignore x))
  (handler-case
      (let ((txt (capi:editor-pane-text (msg-text-pane intf))))
        (when txt
          (when (zerop (length txt))
            (error "PLaintext needed"))
          (decode-and-save-user-shares txt)
          (capi:display-message "User License Saved")
          (capi:destroy intf)))
    
    (error (err)
      (capi:display-message "Error: ~A" err)) ))
  
