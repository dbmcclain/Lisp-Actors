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

(capi:define-interface messaging-intf ()
  ()
  (:panes
   (msg-to-id-pane capi:text-input-pane
                  :text ""
                  :title "To E-Mail(s)"
                  :title-args '(:visible-min-width (:character 13)
                                :visible-max-width t)
                  :visible-min-width '(:character 75)
                  :visible-max-width t
                  :accessor msg-to-id-pane)
   (msg-my-id-pane capi:text-input-pane
                 :text ""
                 :title "My E-Mail"
                 :title-args '(:visible-min-width (:character 13)
                               :visible-max-width t)
                 :visible-min-width '(:character 30)
                 :visible-max-width t
                 :accessor msg-my-id-pane)
   (msg-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "PlainCryptoText"
                     :title "Plain/Crypto Text"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 80)
                     :visible-max-width t
                     :visible-min-height '(:character 10)
                     :visible-max-height t
                     :accessor msg-text-pane)

   (msg-encrypt-button capi:push-button
                       :text "Encrypt"
                       :callback 'encrypt-message)

   (msg-decrypt-button capi:push-button
                       :text "Decrypt"
                       :callback 'decrypt-message)
   
   (cancel-button capi:push-button
                  :text "Cancel"
                  :callback (lambda (x intf)
                              (declare (ignore x))
                              (capi:destroy intf)))
   )
  
  (:layouts

   (row-layout1
    capi:row-layout
    '(msg-encrypt-button
      msg-decrypt-button
      nil
      cancel-button))

   (row-layout2
    capi:row-layout
    '(column-layout2
      nil))

   (column-layout2
    capi:column-layout
    '(msg-to-id-pane
      msg-my-id-pane))

   (column-layout1
    capi:column-layout
    '(row-layout2
      msg-text-pane
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
   :title "Acudora PKI Messaging"
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

(defun make-messaging-intf ()
  (setf sys:*stack-overflow-behaviour* nil)
  (com.sd.butterfly.int:lw-start-butterfly)
  (let ((intf (capi:display (make-instance 'messaging-intf))))
    ;; (init-crypto)
    ;; (assert *passwds*)
    ;; (assert *public-keys*)
    intf))

(defun get-messaging-fields (intf)
  (labels ((get-text (accessor)
             (string-trim '(#\space #\tab #\newline #\return)
                          (capi:text-input-pane-text (funcall accessor intf))))
           (get-editor-text (accessor)
             (capi:editor-pane-text (funcall accessor intf))))
    (let ((ans (list :to-id  (get-text 'msg-to-id-pane)
                     :my-id  (get-text 'msg-my-id-pane)
                     :text   (get-editor-text 'msg-text-pane))))
      (labels ((empty-p (name)
                 (zerop (length (getf ans name)))))
        (when (empty-p :my-id)
          (capi:display-message "My Email required")
          (return-from get-messaging-fields))
        ans))))

(defun encrypt-message (x intf)
  (declare (ignore x))
  (handler-case
      (let ((ans (get-messaging-fields intf)))
        (when ans
          (labels ((empty-p (name)
                     (zerop (length (getf ans name)))))
            (cond ((empty-p :to-id)
                   (error "To E-Mail required"))
                  ((empty-p :text)
                   (error "Plaintext needed"))
                  (t 
                   (let ((ctext (encrypt-ibe
                                 (getf ans :text)
                                 (getf ans :my-id)
                                 (getf ans :to-id))
                                #|
                     (encrypt-for-multiple-recipients
                      (getf ans :text)
                      (getf ans :my-id)
                      (mapcar (um:curry #'string-trim
                                        '(#\Space #\Return #\Page #\VT #\Newline #\Tab))
                              (um:split-string (getf ans :to-id)
                                               :delims (list #\, #\;))) )
                     |#
                     )
                         (snip "---------- SNIP HERE --------------"))
                     (setf (capi:editor-pane-text (msg-text-pane intf))
                           (format nil "~A~%~A~%~A~%"
                                   snip
                                   (coerce ctext 'simple-base-string)
                                   snip) )))
                  ))))
    (error (err)
      (capi:display-message "Error: ~A" err)) ))

(defun decrypt-message (x intf)
  (declare (ignore x))
  (handler-case
      (let ((ans (get-messaging-fields intf)))
        (when ans
          (labels ((empty-p (name)
                     (zerop (length (getf ans name))))
                   (select-cryptotext ()
                     (let* ((text  (getf ans :text))
                            (snip  "--- SNIP HERE ---")
                            (start (search snip text
                                           :test #'string-equal)))
                       (if start
                           (let* ((text (subseq text (+ start 17)))
                                  (end  (search snip text
                                                :test #'string-equal)))
                             (if end
                                 (subseq text 0 end)
                               text))
                         text))))
            (cond ((empty-p :text)
                   (error "Cryptotext needed"))
                  (t
                   (multiple-value-bind (ptext uuid-when kpub)
                       (;; decrypt-for-multiple-recipients
                        decrypt-ibe
                        (select-cryptotext)
                        (getf ans :my-id))
                     (setf (capi:editor-pane-text (msg-text-pane intf))
                           (format nil "Signed: ~A~%~A~%~%~A"
                                   uuid-when kpub
                                   (coerce ptext 'simple-base-string))) ))
                  ))))
    (error (err)
      (capi:display-message "Error: ~A" err))))

#|
To: william@acudora.com
From: david@acudora.com
This is a test...

---------- SNIP HERE --------------
JwQnAgSCtdCrkJWnjK3xt9vg0Y6Jgsz38oX3zvXG/ajG66KDlO77sqSnuKm3kpSdlIO75/jDgams
wuzW5PjLjI/21N3I+ZmTxIyB/M/HhpqW9bPD/cNFBIHZsvXN/LXwhaWimej5qN/njfq52sHGyJGG
0u2P4OHkuqDR9ajwvZydluXmrvig452JvMzAxsjm0af5xOz+rq7wlPiB8vWy2evxhajFpuOtzysn
AicCBIfMmKeH4KW6rKu34Juhw5fowfz/yJnMkL+e6+m5pdKo7v3os+qezprirorwwKKh9a7d7umM
leSo9Yynj+jx3qPLtKL3s4Hup6menePHmtGV5x4EhKa/0b7p4vCZ3b2Vy8fd9dGdjaTkuLLl3ZmM
8rmYp+6/uYyk+5irtP3Hy+z//+Lagbz3ndPm+PHo/7u1jZ2e/qyoqMvx+vuti7S65J+rhau2AicC
BInKveWGz5WXrMPt1+CY0KG3n5+t5Ibe9/Ge4biRxaqGufyP/5C/25/ir7Xlosi8+tfryqi/8IX3
+9q9r4GR/NqsiqOV8YSW67KO7oOZ1ePfmwgEh/CPucPCkr6q07b+3r62pdGE0d2ptdyCrovswoLK
572PpfeMsIz9gNON1e+33a7Z5ebExYqBxKOq0PrI7+qr05Grg460huWutLPupr3exoW3XBeCcO2f
iplZmw1Bk8CNacZ5XlzlxbO7yhCWh3KyErq1om6FYb+RuvZq2DMMzvcn3OOTdHHnXPuvo2OakRWJ
CW03bpea2H91pD2bdesJbOam77gqLb2nn1TJLtadxDg7VbYRVCbQQMgO1pMnISvtOFl5qVtt5h6h
DpEsG9gPvBMGvarO+U2U1BtJ4GdM26dfPsnYsswktMAboO3Ly6TbqeNY6EHqgHI+vFtPIT2wpJ3P
rrLiRF0elOXkQdPuqhrwMMu9tg0UoDBpho4/nSGfDQslftdXh6Q6wWf4SkvFeUzRiuud8a3fqF4Q
FXQeu7WYsyvGL8eYVvCwDoR9da1yvrNvr94gt3eJ8oXECIuFJ+s5ay4QmkWmxFquwFJF1sN/ZcT3
YBXJbDo0ryatXY6BInfjVDmx7faq8P4Kah2ggRkhBfDeLfQ9nubm4SYUUemyKaUaZuGwiNzmGoim
4FB1kGbm+HSlhDbHLVuSRwtHLx/Ue1jqFyCnh+sy+h5jTBJ9gd3xsT2ak0Dwtz6uNHyE4Jz6J95F
vw==
---------- SNIP HERE --------------
|#
