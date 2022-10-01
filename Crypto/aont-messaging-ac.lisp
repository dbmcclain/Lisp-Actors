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

(in-package :tolstoy-aont)


(defclass msg-editor-pane (capi:editor-pane)
  ())

(capi:define-interface messaging-intf ()
  ()
  (:panes
   (msg-text-pane msg-editor-pane
                     :text ""
                     :buffer-name "PlainCryptoText"
                     :title "Plain/Encoded Text"
                     :title-position :top
                     :title-args '(:visible-min-width (:character 20)
                                   :visible-max-width t)
                     :visible-min-width '(:character 85)
                     :visible-max-width nil
                     :visible-min-height '(:character 20)
                     :visible-max-height nil
                     :drop-callback 'enc/dec-drop-callback
                     :accessor msg-text-pane)

   (msg-encode-button capi:push-button
                       :text "Encode"
                       #+:MSWINDOWS :drop-callback #+:MSWINDOWS 'enc-drop-callback
                       :callback 'encode-message)

   (msg-decode-button capi:push-button
                       :text "Decode"
                       #+:MSWINDOWS :drop-callback #+:MSWINDOWS 'dec-drop-callback
                       :callback 'decode-message)
   
   (cancel-button capi:push-button
                  :text "Cancel"
                  :callback (lambda (x intf)
                              (declare (ignore x))
                              (capi:destroy intf)))
   )
  
  (:layouts

   (row-layout1
    capi:row-layout
    '(msg-encode-button
      msg-decode-button
      nil
      cancel-button))

   (column-layout1
    capi:column-layout
    '(msg-text-pane
      row-layout1))
   )
  (:menu-bar file-menu edit-menu)
  (:menus
   (file-menu
    "File"
    (("Encode..."     :data :encode-file)
     ("Decode..."     :data :decode-file))
    :callback 'do-enc/dec-file)
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
   :title "RAL AONT Messaging"
   :best-width 400
   :window-styles '( #+:COCOA :textured-background
                              :movable-by-window-background)
   ;; :drop-callback 'top-level-drop-callback
   ))

(defun show-error (err)
  (capi:display-message "Error: ~A" err))

(defun do-with-handled-error (fn)
  (handler-case
      (funcall fn)
    (error (err)
      (show-error err))))

(defmacro with-handled-error (&body body)
  `(do-with-handled-error (lambda ()
                            ,@body)))

(deflex tolstoy-files-encoder
  (α (files)
    (with-handled-error
     (cond ((or (null files)
                (stringp files))
            (aont-encode-files-to-wp files))

           ((consp files)
            (dolist (file files)
              (aont-encode-files-to-wp file)))
           ))))

(deflex tolstoy-files-decoder
  (α (files)
    (with-handled-error
     (cond ((or (null files)
                (stringp files))
            (aont-decode-files-from-wp files))

           ((consp files)
            (dolist (file files)
              (aont-decode-files-from-wp file)))
           ))))
#|
(defun drop-callback (pane drop-object stage)
  (inspect (list stage drop-object))
  (case stage
    (:formats (capi:set-drop-object-supported-formats drop-object '(:string :filename-list)))
    ((:enter :drag)   (if (capi:drop-object-provides-format drop-object :filename-list)
                          (setf (capi:drop-object-drop-effect drop-object) :copy)))
    (:drop
     (inspect (capi:drop-object-get-object drop-object pane :filename-list))
     (setf (capi:drop-object-drop-effect drop-object) :copy))
    ))
|#

(defun enc/dec-drop-callback (pane drop-object stage)
  (flet ((set-effect-for-operation ()
           (dolist (effect '(:move :copy :link :generic))
             (when (capi:drop-object-allows-drop-effect-p drop-object effect)
               (setf (capi:drop-object-drop-effect drop-object) effect)
               (return t)))))
    
    (case stage
      (:formats         (capi:set-drop-object-supported-formats drop-object '(:string :filename-list)))
      ((:enter :drag)   (if (or (capi:drop-object-provides-format drop-object :filename-list)
                                (capi:drop-object-provides-format drop-object :string))
                            (set-effect-for-operation)))
      (:drop
       (let* ((x (capi:drop-object-pane-x drop-object))
              (ac (if (< x 100)
                      tolstoy-files-encoder
                    tolstoy-files-decoder)))
         (cond ((and (capi:drop-object-provides-format drop-object :string)
                     (set-effect-for-operation))
                (send ac (capi:drop-object-get-object drop-object pane :string)))
               
               ((and (capi:drop-object-provides-format drop-object :filename-list)
                     (set-effect-for-operation))
                (send ac (capi:drop-object-get-object drop-object pane :filename-list)))
               )))
      )))

#+:MSWINDOWS
(defun enc-drop-callback (pane drop-object stage)
  (case stage
    (:formats (capi:set-drop-object-supported-formats drop-object '(:string :filename-list)))
    ((:enter :drag)   (if (capi:drop-object-provides-format drop-object :filename-list)
                          (setf (capi:drop-object-drop-effect drop-object) :copy)))
    (:drop
     (setf (capi:drop-object-drop-effect drop-object) :copy)
     (send tolstoy-files-encoder (capi:drop-object-get-object drop-object pane :filename-list)))
    ))

#+:MSWINDOWS
(defun dec-drop-callback (pane drop-object stage)
  (case stage
    (:formats (capi:set-drop-object-supported-formats drop-object '(:string :filename-list)))
    ((:enter :drag)   (if (capi:drop-object-provides-format drop-object :filename-list)
                          (setf (capi:drop-object-drop-effect drop-object) :copy)))
    (:drop
     (setf (capi:drop-object-drop-effect drop-object) :copy)
     (send tolstoy-files-decoder (capi:drop-object-get-object drop-object pane :filename-list)))
    ))

(defun do-enc/dec-file (item intf)
  (declare (ignore intf))
  (case item
    (:encode-file
     (send tolstoy-files-encoder nil))
    (:decode-file
     (send tolstoy-files-decoder nil))
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
       ;; (capi:call-editor edpane "Delete Region")
       (capi:call-editor edpane "Insert Cut Buffer"))
      )))

(defmethod capi:pane-interface-select-all ((pane msg-editor-pane) intf)
  (when (eq pane (msg-text-pane intf))
    (capi:call-editor pane "Mark Whole Buffer")))
     
(defmethod capi:interface-keys-style ((intf msg-editor-pane))
  #+:MACOSX :mac
  #+:WIN32  :pc)

(defun make-aont-messaging-intf ()
  (setf sys:*stack-overflow-behaviour* nil)
  (let ((intf (capi:display (make-instance 'messaging-intf))))
      ;; (init-crypto)
    ;; (assert *passwds*)
    ;; (assert *public-keys*)
    intf))

(deflex tolstoy-encoder
  (α (cust intf txt)
    (with-handled-error
     (send cust intf (aont-encode-to-wp txt)))))

(deflex tolstoy-decoder
  (α (cust intf txt)
    (handler-case
        (send cust intf (aont-decode-from-wp-to-string txt))
      (error ()
        (show-error "Huh?"))
      )))

(deflex text-displayer
  (α (intf txt)
    (let* ((snip  "---------- SNIP HERE --------------")
           (ftxt  (format nil "~A~%~A~%~A~%"
                          snip txt snip)))
      (capi:execute-with-interface
       intf
       (lambda ()
         (setf (capi:editor-pane-text (msg-text-pane intf)) ftxt)
         (do-menu-item :select-all intf)
         ;; (do-menu-item :copy intf)
         ))
      )))

(defun eol (txt start)
  (and start
       (let ((new-start (position #\newline txt :start start)))
         (and new-start
              (1+ new-start))
         )))

(defun bol (txt start)
  (and start
       (position #\newline (subseq txt 0 start) :from-end t)))
       
(defun select-text-between-snips (text)
  (let* ((snip  "--- SNIP HERE ---")
         (slen  (length snip))
         (start (eol text
                     (search snip text
                             :test #'string-equal)))
         (txlen (length text))
         (end   (and (> txlen slen)
                     (bol text
                          (search snip text
                                  :test     #'string-equal
                                  :from-end t)))))
    (if start
        (subseq text start end)
      (subseq text 0 end))))

(defun encode-message (x intf)
  (declare (ignore x))
  (with-handled-error
    (let* ((edpane (msg-text-pane intf))
           (txt    (capi:editor-pane-text edpane)))
      (when txt
        (cond ((zerop (length txt))
               (error "Plaintext needed"))
              (t
               (let ((txt (select-text-between-snips txt)))
                 (send tolstoy-encoder text-displayer intf txt)))
              ))
      )))

(defun decode-message (x intf)
  (declare (ignore x))
  (with-handled-error
      (let* ((edpane (msg-text-pane intf))
             (text   (capi:editor-pane-text edpane)))
        (when text
          (cond ((zerop (length text))
                 (error "Encoded text needed"))
                (t
                 (let ((txt (select-text-between-snips text)))
                   (send tolstoy-decoder text-displayer intf txt)))
                )))
      ))

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
