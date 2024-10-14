;; ecc-keys.lisp
;; DM/Acudora  11/11
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

(in-package :ecc-crypto-b571)

;; -------------------------------------------------

(defvar *ecc-acudora-private-key*
  (big32 #x02E21053 #x4C1B3BC2 #x91B567D9 #x451766B9
         #xE702113D #xBF895EE2 #x38F634CB #x400F7835
         #x44F50D3F #x3125BEEF #x93A90F31 #xD478567A
         #x5E96E5B9 #xD1602124 #x14CC5F8D #x2D2761FC
         #x52A17B9E #x159E83C8 ))

(defvar *ecc-acudora-public-key*
  (ecc-mul *ecc-gen* *ecc-acudora-private-key*))

;; ---------------------------------------------------------------------------------

(defvar *ecc-vtuning-product-private-key* ;; private
  (big32 #x02AA311A #xA2A18F9B #x330C2E40 #xF2CC8EA3
         #x149DF58D #xDCB8389C #x3223702A #x7AE60F68
         #xAD14940D #x5E0346EF #x134248FF #x1F801A0E
         #x8FD4D0EB #x7498D05D #x7753AC50 #xEC8F0E9A
         #xBB548BCE #x542F6B1D ))

(defvar *ecc-vtuning-product-public-key*
  (ecc-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*))

;; -------------------------------------------------
;; String Encryption

(defvar *ecc-strings-private-key*
  (big32 #x00781008 #xA267866C #x1B9B4653 #x379E408A
         #x582F3BB7 #x5FD33D21 #xF6182102 #x536036FC
         #x27D88E9F #xA89DF3B6 #xA7831762 #xA23FA0A5
         #x2743CC7D #x10E87379 #xB2B3B61A #xB0438A04
         #xF1972370 #xC8CB62DB ))

(defvar *ecc-strings-public-key*
  (ecc-mul *ecc-gen* *ecc-strings-private-key*))

;; ---------------------------------------------------------------------------------

   