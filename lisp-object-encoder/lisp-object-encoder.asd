;; lisp-object-encoder.asd
;; --------------------------------------------------------------------------------------
;; Portable Lisp Object Encoding / Decoding for Network Transport
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
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

(asdf:defsystem "lisp-object-encoder"
  :description "Lisp-Object-Encoder: Network portable encoding / decoding of Lisp objects"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "managed-buffers")
                (:file "scatter-vec")
		(:file "ubyte-streams")
                (:file "self-sync")
                (:file "xzlib") ;; ZLIB adapted to scatter vectors
                (:file "lzw")
                (:file "lisp-object-encoder")
                (:file "safe-marshaling")
                #+(AND :COM.RAL :LISPWORKS) (:file "persistent-store")
		#+(AND :COM.RAL :LISPWORKS) (:file "prevalent-metaclass")
                #+(AND :COM.RAL :LISPWORKS) (:file "prevalent-objects")
                )

  :SERIAL T
  :depends-on   ("data-objects"
                 "ironclad"
                 "sdle-store"
                 "babel"
                 "snappy"
                 ;; "zlib"
                 ))

