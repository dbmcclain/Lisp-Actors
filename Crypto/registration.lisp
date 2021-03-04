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

(defun get-receptor-path (widget data)
  (declare (ignore widget))
  (let ((ans (capi:prompt-for-directory
              "Select Receptor Mount Point"
              :pathname data
              :if-does-not-exist :prompt)))
    (format nil "~A" (or ans data ""))))

(capi:define-interface registration-intf ()
  ()
  (:panes
   (reg-name-pane capi:text-input-pane
                  :text ""
                  :title "Name"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor reg-name-pane)
   (reg-company-pane capi:text-input-pane
                     :text ""
                     :title "Company"
                     :title-args '(:visible-min-width (:character 12)
                                   :visible-max-width t)
                     :visible-min-width '(:character 30)
                     :visible-max-width t
                     :accessor reg-company-pane)
   (reg-street-pane capi:text-input-pane
                  :text ""
                  :title "Street"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor reg-street-pane)
   (reg-city-pane capi:text-input-pane
                  :text ""
                  :title "City"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor reg-city-pane)
   (reg-state-pane capi:text-input-pane
                  :text ""
                  :title "State"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 10)
                  :visible-max-width t
                  :accessor reg-state-pane)
   (reg-zip-pane capi:text-input-pane
                  :text ""
                  :title "ZIP"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 10)
                  :visible-max-width t
                  :accessor reg-zip-pane)
   (reg-email-pane capi:text-input-pane
                  :text ""
                  :title "EMail"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor reg-email-pane)
   (reg-phone-pane capi:text-input-pane
                  :text ""
                  :title "Telephone"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 15)
                  :visible-max-width t
                  :accessor reg-phone-pane)
   (reg-serno-pane capi:text-input-pane
                  :text ""
                  :title "Receptor S/N"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 14)
                  :visible-max-width t
                  :accessor reg-serno-pane)

   (reg-installer-pane capi:text-input-pane
                  :text "david@acudora.com"
                  :title "Installed By"
                  :title-args '(:visible-min-width (:character 12)
                                :visible-max-width t)
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :accessor reg-installer-pane)

   (generate-license-button capi:push-button
                       :text "Generate"
                       :callback 'generate-license)

   #||#
   (receptor-dir-pane capi:text-input-pane
                      :text "/Volumes/Receptor"
                      :title "Receptor"
                      :title-args '(:visible-min-width (:character 12)
                                    :visible-max-width t)
                      :visible-min-width '(:character 30)
                      :visible-max-width t
                      :buttons '(:ok nil
                                 :completion t)
                      :completion-function 'get-receptor-path
                      :accessor reg-receptor-dir-pane)
   #||#

   (line-pinboard-object-1 capi:line-pinboard-object
                           :visible-max-width 1))
  
  (:layouts

   (column-layout1
    capi:column-layout
    '(reg-name-pane
      reg-company-pane
      reg-street-pane
      reg-city-pane
      reg-state-pane
      reg-zip-pane
      reg-phone-pane
      reg-email-pane))

   (column-layout2
    capi:column-layout
    '(reg-serno-pane
      receptor-dir-pane
      reg-installer-pane
      generate-license-button))
   
   (vline capi:simple-pinboard-layout
          '(line-pinboard-object-1))
   
   (row-layout-main
    capi:row-layout
    '(column-layout1
      vline
      column-layout2)))
   

  (:default-initargs
   :layout 'row-layout-main
   :title "Receptor VTuning License Creator"
   :best-width 400
   :window-styles '(:textured-background :movable-by-window-background)))

(defun make-registration-form ()
  (setf sys:*stack-overflow-behaviour* nil)
  (let ((intf (capi:contain (make-instance 'registration-intf))))
    ;; (init-crypto)
    intf))

;; (make-registration-form)

;; --------------------------------------------------------------

#|
    (let ((ans (list
                :name          (get-text 'reg-name-pane)
                :company       (get-text 'reg-company-pane)
                :street        (get-text 'reg-street-pane)
                :city          (get-text 'reg-city-pane)
                :state         (get-text 'reg-state-pane)
                :zip           (get-text 'reg-zip-pane)
                :phone         (get-text 'reg-phone-pane)
                :email         (get-text 'reg-email-pane)
                :serno         (get-text 'reg-serno-pane)
                :installer     (get-text 'reg-installer-pane))))
|#

(defun get-info-fields (intf)
  (labels ((get-text (accessor)
             (string-trim '(#\space #\tab #\newline #\return)
                          (capi:text-input-pane-text (funcall accessor intf)))))
    (let ((ans (loop for pair in '((:name reg-name-pane)
                                   (:company   reg-company-pane)
                                   (:street    reg-street-pane)
                                   (:city      reg-city-pane)
                                   (:state     reg-state-pane)
                                   (:zip       reg-zip-pane)
                                   (:phone     reg-phone-pane)
                                   (:email     reg-email-pane)
                                   (:serno     reg-serno-pane)
                                   (:installer reg-installer-pane)
                                   (:receptor-path reg-receptor-dir-pane))
                     collect (car pair)
                     collect (get-text (cadr pair)) )))
      (labels ((empty-p (name)
                 (zerop (length (getf ans name)))))
        (dolist (pair '((:name      "Name")
                        (:street    "Street Address")
                        (:city      "City")
                        (:state     "State")
                        (:zip       "ZIP")
                        (:phone     "Telephone")
                        (:email     "EMail")
                        (:serno     "Serial Number")
                        (:installer "Installer")))
          (destructuring-bind (keyw txt) pair
            (when (empty-p keyw)
              (progn
                (capi:display-message "~A required" txt)
                (return-from get-info-fields))) ))
        ans))))
                    
(defparameter *license-filename* nil)
(defparameter *license-text*     nil)
(defun license-repository-path ()
  (shared-acudora-file "licenses/"))

#|
      (let* ((dirname (merge-pathnames
                       "Program Files/VST Plugins/Unsupported Plugins/Acudora/"
                       (getf ans :receptor-path)))
             (fname   (format nil "VTuningLicense-~A.txt"
                               (getf ans :serno)))
             (fpath   (merge-pathnames
                       fname
                       dirname))
        (ensure-directories-exist dirname)
        (with-open-file (f fpath
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
|#

(defun generate-license (x intf)
  (declare (ignore x))
  (handler-case
      (let ((ans (get-info-fields intf)))
        (when ans
          (let* ((mons #("???"
                         "January"
                         "February"
                         "March"
                         "April"
                         "May"
                         "June"
                         "July"
                         "August"
                         "September"
                         "October"
                         "November"
                         "December"))
                 (tzs  #("UTC"
                         "UTC-1"
                         "UTC-2"
                         "UTC-3"
                         "UTC-4"
                         "EST"
                         "CST"
                         "MST"
                         "PST"))
                 (receptor-path (getf ans :receptor-path)))
            (setf *license-filename* (format nil "VTuningLicense-~A.txt"
                                             (getf ans :serno))
                  *license-text*
                  (format nil
                          #>.end
#########################################################################
### WARNING!! ###
###
### DO NOT ALTER THIS FILE, NOR ANY OTHER COMPONENT OF THE VTUNING SYSTEM.
###
### VTuning is Protected by:
###
### 	    NIST B-571 Elliptic Curve Cryptography,
### 	    AES-256/CBC Encryption, and 
###	    SHA-256 Signatures
###
### The contents of this file are necessary components of the keying 
### system. Any alteration to this file, or to any other component of 
### VTuning, will render the system completely inoperative.
###
### WARNING!! ###
##########################################################################

This copy of VTuning[3.0] has been licensed to:

Name:           ~A
Company:        ~A
Street:         ~A
City/State/Zip: ~A, ~A  ~A
EMail:          ~A
Telephone:      ~A

Receptor SN:    ~A
Date:           ~A
Duration:       Forever
Installed by:   ~A

##########################################################################
.end
          (getf ans :name)
          (getf ans :company)
          (getf ans :street)
          (getf ans :city) (getf ans :state) (getf ans :zip)
          (getf ans :email)
          (getf ans :phone)
          (getf ans :serno)
          (multiple-value-bind (ss mm hh dd mon yr dow dst tz)
              (decode-universal-time (get-universal-time))
            (declare (ignore dow dst))
            (format nil "~2,'0d ~A ~d at ~2,'0d:~2,'0d:~2,'0d ~A"
                    dd (aref mons mon) yr
                    hh mm ss (aref tzs tz)))
          (getf ans :installer)))
        (capi:with-busy-interface (intf)
          (with-transient-ramdisk ()
            (copy-vtuning-payload-archive-to-ramdisk (getf ans :installer))
            (with-open-file (fp (staging-path *license-filename*)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
              (write-sequence *license-text* fp))
            (with-open-file (fp (merge-pathnames *license-filename*
                                                 (license-repository-path))
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
              (write-sequence *license-text* fp))
            (rebuild-vtuning-payload *license-filename*)
            (copy-vtuning-payload-to-receptor receptor-path) ))
        (capi:display-message "Finished"))))
    (error (err)
      (capi:display-message "Error: ~A" err)) ))

  
