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

(defclass msg-editor-pane (capi:editor-pane)
  ())

(defmethod capi:pane-interface-select-all ((pane msg-editor-pane) intf)
  (capi:call-editor pane "Mark Whole Buffer"))

(defmethod capi:pane-interface-copy-object ((pane msg-editor-pane) intf)
  (capi:call-editor pane "Copy To Cut Buffer"))

(defmethod capi:pane-interface-cut-object ((pane msg-editor-pane) intf)
  (capi:call-editor pane "Copy To Cut Buffer")
  (capi:call-editor pane "Delete Region"))

(defmethod capi:pane-interface-paste-object ((pane msg-editor-pane) intf)
  (capi:call-editor pane "Delete Region")
  (capi:call-editor pane "Insert Cut Buffer"))

(defmethod capi:interface-keys-style ((intf msg-editor-pane))
  #+:MACOSX :mac
  #+:WIN32  :pc)

;; ---------------------------------------------------------------------------

(defparameter *Remote-Connection-Indicator*
  (make-instance 'gpi:<Labeled-LED-Indicator>
                 :width    70
                 :height   20
                 :on-color (color:make-rgb 0 1 0)
                 :label    "Remote"
                 :label-color (color:make-gray 0.8)))

;; ---------------------------------------------------------------------------

(capi:define-interface escrow-intf ()
  ((master-key1 :accessor master-key1 :initform nil)
   (master-key2 :accessor master-key2 :initform nil)
   (master-key3 :accessor master-key3 :initform nil))
  (:panes
   (led-panel-1
    gpi:<display-panel>
    :min-height 20
    :max-height 20
    :min-width  70
    :max-width  70
    :accessor   led1
    :items (list
            (make-instance 'gpi:<Labeled-LED-Indicator>
                 :width    70
                 :height   20
                 :on-color (color:make-rgb 0 1 0)
                 :label    "Member 1"
                 :label-color (color:make-gray 0.8))))

   (led-panel-2
    gpi:<display-panel>
    :min-height 20
    :max-height 20
    :min-width  70
    :max-width  70
    :accessor   led2
    :items (list
            (make-instance 'gpi:<Labeled-LED-Indicator>
                 :width    70
                 :height   20
                 :on-color (color:make-rgb 0 1 0)
                 :label    "Member 2"
                 :label-color (color:make-gray 0.8))))

   (led-panel-3
    gpi:<display-panel>
    :min-height 20
    :max-height 20
    :min-width  70
    :max-width  70
    :accessor   led3
    :items (list
            (make-instance 'gpi:<Labeled-LED-Indicator>
                 :width    70
                 :height   20
                 :on-color (color:make-rgb 0 1 0)
                 :label    "Member 3"
                 :label-color (color:make-gray 0.8))))
                 
   (key1-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "key1-text"
                     :title "Member Key1"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 6)
                     :visible-max-height t
                     :accessor key1-text-pane)

   (key2-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "key2-text"
                     :title "Member Key2"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 6)
                     :visible-max-height t
                     :accessor key2-text-pane)

   (master-key-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "master-key-text"
                     :title "MasterKey"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 20)
                     :visible-max-height t
                     :accessor master-key-text-pane)

   (get-master-key-button capi:push-button
                       :text "Get Member Key"
                       :callback 'get-member-master-key)

   (clear-keys-button capi:push-button
                      :text "Clear Fields"
                      :callback 'clear-member-keys)
   
   (cancel-button capi:push-button
                  :text "Cancel"
                  :callback (lambda (x intf)
                              (declare (ignore x))
                              (capi:destroy intf)))
   )
  
  (:layouts

   (row-layout1
    capi:row-layout
    '(get-master-key-button
      clear-keys-button
      nil
      cancel-button))

   (row-layout-leds
    capi:row-layout
    '(led-panel-1
      led-panel-2
      led-panel-3))
   
   (column-layout2
    capi:column-layout
    '(key1-text-pane
      key2-text-pane
      row-layout-leds))

   (row-layout2
    capi:row-layout
    '(column-layout2
      master-key-text-pane))
   
   (column-layout1
    capi:column-layout
    '(row-layout2
      row-layout1))
   )
  #|
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
    #|:callback-type :item|#
    ))
  |#

  (:default-initargs
   :layout 'column-layout1
   :title "Acudora Escrow Keying"
   :best-width 400
   :window-styles '( #+:COCOA :textured-background
                              :movable-by-window-background)
   ))

#|
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
|#

(defun make-escrow-intf ()
  ;; (setf sys:*stack-overflow-behaviour* nil)
  ;; (com.sd.butterfly.int:lw-start-butterfly)
  (let ((intf (capi:display (make-instance 'escrow-intf))))
    ;; (init-crypto)
    ;; (assert *passwds*)
    ;; (assert *public-keys*)
    intf))

(defun get-member-messaging-fields (intf)
  (labels ((get-text (accessor)
             (string-trim '(#\space #\tab #\newline #\return)
                          (capi:text-input-pane-text (funcall accessor intf))))
           (get-editor-text (accessor)
             (capi:editor-pane-text (funcall accessor intf))))
    (let ((ans (list :key1   (get-editor-text 'key1-text-pane)
                     :key2   (get-editor-text 'key2-text-pane))))
      (labels ((empty-p (name)
                 (zerop (length (getf ans name)))))
        (when (empty-p :key1)
          (capi:display-message "Key1 required")
          (return-from get-member-messaging-fields))
        (when (empty-p :key2)
          (capi:display-message "Key2 required")
          (return-from get-member-messaging-fields))
        ans))))

(defun set-led (led onoff)
  (setf (gpi:panel-item-value (first (gpi:display-panel-items led))) onoff))

(defun clear-member-keys (x intf)
  (declare (ignore x))
  (setf (capi:editor-pane-text (key1-text-pane intf)) ""
        (capi:editor-pane-text (key2-text-pane intf)) ""
        (capi:editor-pane-text (master-key-text-pane intf)) ""
        (master-key1 intf) nil
        (master-key2 intf) nil
        (master-key3 intf) nil)
  (set-led (led1 intf) nil)
  (set-led (led2 intf) nil)
  (set-led (led3 intf) nil))

(defun get-member-master-key (x intf)
  (handler-case
      (let ((ans (get-member-messaging-fields intf)))
        (when ans
          (let ((master-key (unlock-one (getf ans :key1)
                                        (getf ans :key2))))
            (if master-key
                (destructuring-bind (board-member key) master-key
                  (declare (ignore board-member))
                  (setf (capi:editor-pane-text (key1-text-pane intf)) ""
                        (capi:editor-pane-text (key2-text-pane intf)) "")
                  (cond ((null (master-key1 intf))
                         (setf (master-key1 intf) key)
                         (set-led (led1 intf) t))

                        ((null (master-key2 intf))
                         (setf (master-key2 intf) key)
                         (set-led (led2 intf) t))

                        ((null (master-key3 intf))
                         (setf (master-key3 intf) key)
                         (set-led (led3 intf) t)) )

                  (when (and (master-key1 intf)
                             (master-key2 intf)
                             (master-key3 intf))
                    (let ((msg (get-escrow-key
                           (master-key1 intf)
                           (master-key2 intf)
                           (master-key3 intf))))
                      (if msg
                          (setf (capi:editor-pane-text (master-key-text-pane intf)) msg)
                        (progn
                          (clear-member-keys x intf)
                          (error "Invalid key set"))) )))
              ;; else
              (progn
                (clear-member-keys x intf)
                (error "Invalid key pair")))
            )))
    (error (err)
      (capi:display-message "Error: ~A" err)) ))


;; ----------------------------------------------------------------------------
#|
(capi:define-interface escrow-master-intf ()
  ()
  (:panes
   (key1-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "escrow-key1-text"
                     :title "Member1 Key"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 8)
                     :visible-max-height t
                     :accessor key1-text-pane)

   (key2-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "escrow-key2-text"
                     :title "Member2 Key"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 8)
                     :visible-max-height t
                     :accessor key2-text-pane)

   (key3-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "escrow-key3-text"
                     :title "Member3 Key"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 8)
                     :visible-max-height t
                     :accessor key3-text-pane)

   (master-key-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "escrow-master-key-text"
                     :title "MasterKey"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 20)
                     :visible-max-height t
                     :accessor master-key-text-pane)

   (get-master-key-button capi:push-button
                       :text "Get Master Key"
                       :callback 'get-master-key)
   
   (clear-keys-button capi:push-button
                      :text "Clear Fields"
                      :callback 'clear-master-keys)
   
   (cancel-button capi:push-button
                  :text "Cancel"
                  :callback (lambda (x intf)
                              (declare (ignore x))
                              (capi:destroy intf)))
   )
  
  (:layouts

   (row-layout1
    capi:row-layout
    '(get-master-key-button
      clear-keys-button
      nil
      cancel-button))

   (column-layout1
    capi:column-layout
    '(key1-text-pane
      key2-text-pane
      key3-text-pane
      master-key-text-pane
      row-layout1))
   )
  #|
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
    #|:callback-type :item|#
    ))
  |#

  (:default-initargs
   :layout 'column-layout1
   :title "Acudora Escrow Master Keying"
   :best-width 400
   :window-styles '( #+:COCOA :textured-background
                              :movable-by-window-background)
   ))

(defun make-escrow-master-intf ()
  (setf sys:*stack-overflow-behaviour* nil)
  (com.sd.butterfly.int:lw-start-butterfly)
  (let ((intf (capi:display (make-instance 'escrow-master-intf))))
    ;; (init-crypto)
    ;; (assert *passwds*)
    ;; (assert *public-keys*)
    intf))

(defun get-master-messaging-fields (intf)
  (labels ((get-text (accessor)
             (string-trim '(#\space #\tab #\newline #\return)
                          (capi:text-input-pane-text (funcall accessor intf))))
           (get-editor-text (accessor)
             (capi:editor-pane-text (funcall accessor intf))))
    (let ((ans (list :key1   (get-editor-text 'key1-text-pane)
                     :key2   (get-editor-text 'key2-text-pane)
                     :key3   (get-editor-text 'key3-text-pane))))
      (labels ((empty-p (name)
                 (zerop (length (getf ans name)))))
        (when (empty-p :key1)
          (capi:display-message "Key1 required")
          (return-from get-master-messaging-fields))
        (when (empty-p :key2)
          (capi:display-message "Key2 required")
          (return-from get-master-messaging-fields))
        (when (empty-p :key3)
          (capi:display-message "Key3 required")
          (return-from get-master-messaging-fields))
        ans))))

(defun clear-master-keys (x intf)
  (declare (ignore x))
  (setf (capi:editor-pane-text (key1-text-pane intf)) ""
        (capi:editor-pane-text (key2-text-pane intf)) ""
        (capi:editor-pane-text (key3-text-pane intf)) ""
        (capi:editor-pane-text (master-key-text-pane intf)) ""))

(defun get-master-key (x intf)
  (declare (ignore x))
  (handler-case
      (labels ((prep-key (key)
                 (let* ((snippy "--- SNIP HERE ---")
                        (pos    (search snippy key))
                        (from   (if pos
                                    (+ (length snippy) pos)
                                  0))
                        (to     (or (search snippy key :start2 from)
                                    (length key))))
                   (decode-object-from-base64 (subseq key from to)))))
        (let ((ans (get-master-messaging-fields intf)))
          (when ans
            (let ((master-key (get-escrow-key
                               (prep-key (getf ans :key1))
                               (prep-key (getf ans :key2))
                               (prep-key (getf ans :key3)))))
              (if master-key
                  (setf (capi:editor-pane-text (master-key-text-pane intf)) master-key)
                ;; else
                (error "Invalid key set"))
              ))))
    (error (err)
      (capi:display-message "Error: ~A" err)) ))

|#
