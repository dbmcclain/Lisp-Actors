;; machine-id.lisp -- Get Mac Machine Identification String
;; DM/Acudora  08/11
;; --------------------------------------------------------------------------------
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

(defpackage :machid
  (:use :common-lisp)
  (:export
   #:get-machine-id
   #:get-machine-id-digest
   ))

(in-package #:machid)

(fli:define-foreign-type mach-port-t         () `(:unsigned :int))
(fli:define-foreign-type io-registry-entry-t () `(:unsigned :int))
(fli:define-foreign-type CFTypeRef           () `(:pointer :void))
(fli:define-foreign-type CFStringRef         () `(:pointer :void))
(fli:define-foreign-type CFAllocatorRef      () `(:pointer :void))
(fli:define-foreign-type CFStringEncoding    () :uint32)

(defconstant *kCFAllocatorDefault*  fli:*null-pointer*)

;; ----------------------------------------------

(defconstant *kIORegistryIterateRecursively* #x00000001)

(fli:define-foreign-function (IORegistryGetRootEntry "IORegistryGetRootEntry" :source)
    ((masterPort mach-port-t))
  :result-type io-registry-entry-t
  :language :ansi-c)

(fli:define-foreign-function (IORegistryEntrySearchCFProperty "IORegistryEntrySearchCFProperty" :source)
    ((root  io-registry-entry-t)
     (plane (:reference-pass (:ef-mb-string
                              :external-format :ascii)))
     (key   CFStringRef)
     (:constant *kCFAllocatorDefault* CFAllocatorRef)
     (action :uint32))
  :result-type CFTypeRef
  :language :ansi-c)

(fli:define-foreign-function (IOObjectRelease "IOObjectRelease" :source)
    ((handle io-registry-entry-t))
  :result-type :int
  :language :ansi-c)

;; ----------------------------------------------

(defconstant *kCFStringEncodingUTF8*  #x08000100)

(fli:define-foreign-function (CFStringCreateWithCString "CFStringCreateWithCString" :source)
    ((:constant *kCFAllocatorDefault* CFAllocatorRef)
     (str (:reference-pass (:ef-mb-string
                            :external-format :ascii)))
     (:constant *kCFStringEncodingUTF8* CFStringEncoding))
  :result-type CFStringRef
  :language :ansi-c)

(fli:define-foreign-function (CFStringGetCString "CFStringGetCString" :source)
    ((str-ref CFStringRef)
     (buf     (:pointer (:unsigned :char)))
     (buflen  :uint32)
     (:constant *kCFStringEncodingUTF8* CFStringEncoding))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (CFRelease "CFRelease" :source)
    ((ptr CFTypeRef))
  :result-type :int
  :language :ansi-c)
  
;; ----------------------------------------------

(defun get-ioregistry-text (key)
  #F
  (fli:with-dynamic-foreign-objects ()
    (let* ((root   (IORegistryGetRootEntry 0))
           (cfkey  (CFStringCreateWithCString key)))
      (unwind-protect
          (let* ((buflen 128)
                 (buf    (fli:allocate-dynamic-foreign-object
                          :type '(:unsigned :char) :nelems buflen))
                 (val    (IORegistryEntrySearchCFProperty root "IOService" cfkey
                                                          *kIORegistryIterateRecursively*)))
            (when (fli:null-pointer-p val)
              (error "Invalid key"))
            (CFStringGetCString val buf buflen)
            (cfrelease val)
            (fli:convert-from-foreign-string buf
                                             :external-format :ascii))
        (CFRelease cfkey)
        (IOObjectRelease root)
        )) ))

#|
(defun get-machine-id ()
  (concatenate 'string 
               (get-ioregistry-text "IOPlatformUUID")
               "/"
               (get-ioregistry-text "IOPlatformSerialNumber")))
|#

(defun get-machine-id ()
  #F
  (get-ioregistry-text "IOPlatformUUID"))

(defun get-machine-id-digest (&key (niter 8192))
  (let ((machid (get-machine-id))
        (digest (make-array 32 :element-type '(unsigned-byte 8))))
    (let ((digester (ironclad:make-digest :sha256)))
      (ironclad:update-digest digester machid)
      (ironclad:produce-digest digester :digest digest))
    (loop repeat niter
          for digester = (ironclad:make-digest :sha256)
          then (reinitialize-instance digester)
          do
          (progn
            (ironclad:update-digest digester machid)
            (ironclad:update-digest digester digest)
            (ironclad:produce-digest digester :digest digest)))
    (ironclad:byte-array-to-hex-string digest)))
