;; ecc-package.lisp
;; -----------------------------------
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

(in-package #:cl-user)

(um:eval-always
  (project:defproject
   (#:cached-var           #:com.ral.cached-var)
   (#:crypto-utils         #:com.ral.crypto.crypto-utils)
   (#:cryptolib            #:com.ral.crypto.cryptolib)
   (#:gflib                #:com.ral.crypto.gflib)
   (#:ecc                  #:ecc-crypto-b571)
   (#:ecc-crypto-b571      #:com.ral.crypto.ecc-crypto-b571)
   (#:pbc-interface        #:com.ral.crypto.pbc-interface)
   (#:crypto-lib-loader    #:com.ral.crypto.crypto-lib-loader)
   (#:edwards-ecc          #:com.ral.crypto.edwards-ecc)
   (#:lattice              #:com.ral.crypto.lattice-crypto)
   (#:lattice-ke           #:com.ral.crypto.lattice-key-exchange)
   (#:finite-field         #:com.ral.crypto.finite-field)
   (#:kdf                  #:com.ral.crypto.kdf)
   (#:ciphers              #:com.ral.crypto.ciphers)
   ))

(defpackage #:com.ral.cached-var
  (:use #:common-lisp)
  (:export
   #:def-cached-var
   #:get-cached-symbol-data))

(defpackage #:com.ral.crypto.crypto-utils
  (:use #:common-lisp #:cached-var)
  (:export
   #:my-random-state
   #:my-random

   #:ubyte
   #:ub-vector
   #:make-ub-array

   #:safe-char-code
   #:convert-text-to-int8-array
   #:ensure-8bitv
   #:hexit
   #:big32
   #:convert-string-to-bytes
   #:convert-bytes-to-string
   #:convert-int-to-bytes
   #:convert-int-to-nbytes
   #:convert-int-to-nbytesv
   #:convert-bytes-to-int

   #:fragmentize
   #:format-fragments

   #:strengthen-key
   #:make-key-from-plaintext
   #:print-c-array
   
   #:encode-bytes-to-base64
   #:decode-bytes-from-base64
   #:encode-object-to-base64
   #:decode-object-from-base64

   #:encode-bytes-to-base85
   #:decode-bytes-from-base85
   #:encode-object-to-base85
   #:decode-object-from-base85

   #:convert-int-to-lev
   #:convert-lev-to-int
   
   #:encode-bytes-to-base58
   #:decode-bytes-from-base58
   #:encode-object-to-base58
   #:decode-object-from-base58

   #:format-bytes
   #:read-blob

   #:write-int
   #:write-vector
   #:write-cp-string
   #:write-sequences

   #:make-cp-string-vector
   #:read-nvector
   #:read-int
   #:read-vector
   #:read-cp-string
   #:read-sequences

   #:wipe
   #:wipe-object

   #:need-ubyte-vector
   #:safe-update-digest
   #:safe-update-hmac

   #:convert-hashint-to-32bytes
   #:sha2-file
   #:shad2-file
   #:sha2d-file
   #:sha2-key
   #:sha2-buffers
   #:sha_d-256
   #:sha3/256-file
   #:sha3-file
   #:sha3-buffers
   #:sha3/256-buffers
   #:basic-hash-with-protocol
   #:hash-with-protocol

   #:convert-int571-to-80bytes
   
   #:with-sensitive-objects
   #:check-paths-not-equal

   #:make-nonce
   #:mask-off
   ))

(defpackage #:com.ral.crypto.cryptolib
  (:use #:common-lisp #:crypto-utils) 
  (:export
   #:sha2_context
   #:sha2_starts
   #:sha2_update
   #:sha2_finish
   #:sha2
   #:sha2_file
   #:shad2_file
   #:sha2_self_test
   #:sha2_hmac

   #:aes_context
   #:aes_setkey_enc
   #:aes_setkey_dec
   #:aes_crypt_cbc
   #:aes_self_test

   #:aesx_context
   #:aesx_setkey_enc
   #:aesx_setkey_dec
   #:aesx_crypt_cbc
   #:aesx_self_test

   #:c-sha2-file
   #:c-sha2-buffers
   #:c-sha2-buffers-into
   #:c-kdf
   ))

(defpackage #:com.ral.crypto.gflib
  (:use #:common-lisp #:cryptolib)
  (:export
   #:gf128_add
   #:gf128_mul
   #:gf128_div
   #:gf128_inv

   #:deref64
   #:ldb64

   #:convert-gf128-int-to-cbuf
   #:convert-cbuf-to-gf128-int
   #:foreign-gf-buffer
   #:foreign-gf128-buffer
   #:with-gf128-buffers
   #:gf128-binop
   #:c-gf128-add
   #:c-gf128-mul
   #:c-gf128-div
   #:gf128-unop
   #:c-gf128-inv

   #:gf571_add
   #:gf571_mul
   #:gf571_div
   #:gf571_inv

   #:convert-gf571-int-to-cbuf
   #:convert-cbuf-to-gf571-int
   #:foreign-gf571-buffer
   #:with-gf571-buffers
   #:gf571-binop
   #:c-gf571-add
   #:c-gf571-mul
   #:c-gf571-div
   #:gf571-unop
   #:c-gf571-inv

   #:c-ecc571-setCurve
   #:with-ecc-lib
   #:c_ecc571_add
   #:c_ecc571_sub
   #:chk-c-inf
   #:c-ecc571-binop
   #:c-ecc571-add
   #:c-ecc571-sub
   #:c-ecc571-mul
   #:gf-random-k*
   ))

(defpackage #:com.ral.crypto.modular-arith
  (:use #:common-lisp
   #:cached-var)
  (:export
   #:with-mod
   #:mod-base   
   #:reset-blinders
   #:m=
   #:m^
   #:msqrt
   #:msqr
   #:m+
   #:m-
   #:m*
   #:m/
   #:minv
   #:mmod
   #:mchi
   #:quadratic-residue-p
   #:m!
   #:mabs

   #:msqrt*
   #:mmax
   #:mmin
   #:bezout
   #:msigned

   #:with-qf
   #:qf-re
   #:qf-im
   #:qfmod
   #:qf=
   #:qf-conj
   #:qf+
   #:qf-
   #:qf*
   #:qf/
   #:qf^
   #:qfsqr
   #:qf-abssqr
   #:qfsqrt
   ))

#|
(defpackage #:crypto-mod-math
  (:use #:common-lisp)
  (:export
   #:reset-blinders
   #:expt-mod
   #:sqrt-mod
   #:mult-mod
   #:add-mod
   #:sub-mod
   #:inv-mod
   #:div-mod
   #:quadratic-residue-p
   ))
|#

(defpackage #:com.ral.vec-repr
  (:use #:common-lisp)
  (:export
   #:ub8        ;; type
   #:ub8-vector ;; type
   #:make-ub8-vector
   #:ub8v
   #:ub8v-obj
   #:ub8v-as-str
   #:ub8v-val
   #:ub8v-str
   #:val
   #:lev
   #:lev-vec
   #:bev
   #:bev-vec
   #:base58
   #:base58-str
   #:base58-chk
   #:base64
   #:base64-str
   #:base85
   #:base85-str
   #:hex
   #:hex-str
   #:levn
   #:bevn
   #:convert-int-to-vec
   #:convert-vec-to-int
   #:int
   #:int=
   #:int/=
   #:int<
   #:int<=
   #:int>
   #:int>=
   #:vec
   #:vec-cmp
   #:vec=
   #:vec/=
   #:vec<
   #:vec<=
   #:vec>
   #:vec>=
   #:sbs
   #:str
   #:short-str
   #:validate-base58-string
   #:check-repr
   ))

(defpackage #:com.ral.crypto.hash
  (:use #:common-lisp
        #:vec-repr
        #:cached-var)
  (:export
   #:hash
   #:hash-val
   #:hash-fn
   #:hash-bytes
   #:hash-length
   #:hash/ripemd/160
   #:hash/sha2/256
   #:hash/256
   #:hash/384
   #:hash/512
   #:hash/var
   #:get-raw-hash-nbytes
   #:get-raw-hash-nbits
   #:get-hash-nbytes
   #:get-hash-nbits
   #:hash-to-range
   #:hashable
   #:hash-check
   #:hash=
   #:hash-function-of-hash
   #:in-place-otp
   ))

(defpackage #:com.ral.crypto.prng
  (:use #:common-lisp #:cached-var)
  (:import-from #:crypto-utils
   #:convert-bytes-to-int
   #:mask-off
   #:convert-int-to-nbytesv
   #:make-ub-array
   #:make-nonce)
  (:export
   #:ctr-hash-prng
   #:get-entropy
   #:basic-random
   #:basic-random-between
   
   #:random-between
   
   #:ctr-drbg
   #:ctr-drbg-int
   #:field-random
   #:safe-field-random
   ))

(defpackage #:com.ral.crypto.kdf
  (:use
   #:common-lisp
   #:crypto-utils
   #:cryptolib)
  (:export
   #:kdf
   #:apply-kdf))

(defpackage #:com.ral.crypto.ciphers
  (:use #:common-lisp
   #:crypto-utils
   #:kdf
   #:prng)
  (:export
   #:*ctr-hmac-cipher-file-encryption*
   #:ctr-hmac-signature

   #:generic-ctr-cipher
   #:ctr-cipher-key
   #:ctr-cipher-salt
   #:ctr-cipher-nonce
   #:ctr-cipher-ctr
   #:ctr-cipher-cvec
   #:ctr-cipher-sig
   #:ctr-cipher-ecb
   #:ctr-cipher-mac
   
   #:wipe-obj
   #:generic-ctr-hmac-cipher
   #:update-mac
   #:get-mac
   #:mac-length
   #:cipher-overhead

   #:ctr-hmac-cipher
   #:make-ecb-cipher
   #:make-cipher-block
   #:make-displaced-cipher-block
   #:make-ctr-hmac-cipher

   #:encrypt-decrypt-ctr
   #:safe-encrypt-in-place
   #:safe-decrypt-in-place

   #:ctr-encrypt-stream-with-cipher
   #:ctr-hmac-encrypt-stream
   #:ctr-hmac-encrypt-sequence
   #:ctr-hmac-encrypt-file

   #:ctr-decrypt-stream-with-cipher
   #:ctr-hmac-decrypt-stream
   #:ctr-hmac-decrypt-sequence
   #:ctr-hmac-decrypt-file

   #:delivered-ctr-hmac-encrypt
   ))

(defpackage #:com.ral.crypto.ecc-crypto-b571
  (:use
   #:common-lisp
   #:modmath
   #:cached-var
   #:cryptolib
   #:gflib
   #:prng
   #:ciphers
   #:crypto-utils
   #:kdf)
  ;; (:nicknames #:ecc)
  (:import-from #:um ;; #:com.ral.useful-macros
   #:defstub
   #:with-fast-impl)
  (:export
   ))

#|
(defpackage #:com.ral.primes
  (:use #:common-lisp)
  (:export
   #:divides?
   #:expt-mod
   #:random-between
   #:make-prime
   #:is-prime?
   #:extended-gcd
   #:compute-modulo-inverse
   #:provably-prime?
   #:factors-of
   #:generate-strong-prime
   #:generate-rsa-base
   #:add-mod
   #:sub-mod
   #:mult-mod
   #:inv-mod
   #:div-mod
   #:expt-mod
   #:decompose
   ))

(defpackage #:lagrange-4-square
  (:use #:common-lisp)
  (:import-from #:primes
   #:is-prime?
   #:expt-mod)
  (:import-from #:com.ral.useful-macros
   #:curry
   #:nlet)
  (:export
   #:decompose-integer
   ))
|#

(defpackage #:com.ral.crypto.pbc-interface
  (:use #:common-lisp
        #:vec-repr
        #:hash
        #:prng
        #:modmath)
  ;; (:nicknames #:pbc)
  (:export
   ;; classes and their slot readers
   #:crypto-val
   #:crypto-val-vec
   #:g1-cmpr
   #:g1-cmpr-pt
   #:g2-cmpr
   #:g2-cmpr-pt
   #:zr
   #:zr-val
   #:gt
   #:gt-val
   #:public-key
   #:public-key-val
   #:secret-key
   #:secret-key-val
   #:signature
   #:signature-val
   #:pairing
   #:pairing-val
   #:crypto-text
   #:crypto-text-vec
   #:public-subkey
   #:secret-subkey
   
   #:init-pairing
   #:set-generator  ;; 1 each for G1, and G2 groups
   
   #:get-g1
   #:get-g2
   #:get-order
   
   #:make-key-pair
   #:check-public-key

   #:make-public-subkey
   #:make-secret-subkey
   #:ibe-encrypt
   #:ibe-decrypt
   
   #:sign-message       ;; BLS Sigs
   #:check-message
   #:combine-signatures ;; for BLS MultiSigs

   #:compute-pairing

   #:pbc=
   
   #:add-zrs
   #:sub-zrs
   #:mul-zrs
   #:div-zrs
   #:exp-zrs
   #:neg-zr
   #:inv-zr

   #:add-pts  ;; non-bent nomenclature for ECC
   #:sub-pts
   #:mul-pts  ;; bent nomenclature for ECC
   #:div-pts
   #:neg-pt
   #:inv-pt
   
   #:mul-pt-zr
   #:expt-pt-zr  ;; bent nom

   #:mul-gts
   #:div-gts
   #:expt-gt-zr
   #:inv-gt
   
   #:keying-triple
   #:keying-triple-pkey
   #:keying-triple-sig
   #:keying-triple-skey
   
   #:signed-message
   #:signed-message-msg
   #:signed-message-sig
   #:signed-message-pkey

   #:pbc-hash
   #:hash-to-pbc-range
   #:sign-hash
   #:check-hash

   #:crypto-packet
   #:crypto-packet-pkey
   #:crypto-packet-id
   #:crypto-packet-tstamp
   #:crypto-packet-rval
   #:crypto-packet-cmsg

   #:g1-from-hash
   #:g2-from-hash
   #:zr-from-hash

   #:compute-vrf
   #:validate-vrf
   #:validate-vrf-mapping
   #:vrf
   #:vrf-seed
   #:vrf-x
   #:vrf-y
   #:vrf-proof

   #:make-pedersen-proof
   #:validate-pedersen-proof
   #:make-cloaked-proof
   #:validate-cloaked-proof

   #:confidential-purchase
   #:confidential-purchase-pbuy
   #:confidential-purchase-psell
   #:confidential-purchase-tbuy
   #:confidential-purchase-rsell
   #:check-confidential-purchase

   #:*pairing*
   #:*pairing-name*
   #:with-pairing
   #:set-pairing
   #:list-all-pairings

   #:make-keying-triple
   #:make-keying-pairs

   ;; for safe-reader
   #:address
   #:addr
   #:addr-str
   #:make-pkey
   #:make-skey
   #:make-sig
   #:make-addr
   #:read-safely
   ))

(defpackage #:com.ral.crypto.edwards-ecc
  (:nicknames #:edec)
  (:use
   #:common-lisp
   #:crypto-utils
   #:cached-var
   #:vec-repr
   #:hash
   #:prng)
  (:import-from #:um
   #:defstub
   #:stub-function-p
   #:with-fast-impl)
  (:export
   #:ed-curve
   #:with-ed-curve
   #:set-ed-curve
   #:ed-curves
   #:*edcurve*
   #:*ed-gen*
   #:*ed-r*
   #:*ed-h*
   #:*ed-q*
   #:*ed-c*
   #:*ed-d*
   #:*ed-name*
   #:*ed-nb*
   #:*ed-nbits*
   #:ecc-pt
   #:ecc-proj-pt
   #:ed-affine
   #:ed-pt=
   #:ed-neutral-point
   #:ed-neutral-point-p
   #:ed-satisfies-curve
   #:ed-add
   #:ed-negate
   #:ed-sub
   #:ed-mul
   #:ed-div
   #:ed-nth-pt
   #:ed-nth-proj-pt
   #:ed-compress-pt
   #:ed-decompress-pt
   #:ed-validate-point
   #:ed-valid-point-p
   #:ed-random-pair
   #:ed-random-generator
   #:hash-to-pt-range
   #:hash-to-grp-range
   #:ed-pt-from-hash
   #:ed-pt-from-seed
   
   #:elligator-random-pt
   #:elligator-tau-vector
   #:elligator-encode
   #:elligator-decode
   #:elligator-limit
   #:elligator-nbits
   #:to-elligator-range
   #:elli2-encode
   #:elli2-decode
   #:elli2-random-pt
   #:elligator-body
   #:refresh-elligator
   
   #:ed-schnorr-sig
   #:ed-schnorr-sig-verify
   
   #:ed-convert-int-to-lev
   #:ed-convert-lev-to-int
   #:make-deterministic-keys
   #:compute-deterministic-skey
   #:compute-schnorr-deterministic-random
   #:ed-dsa
   #:ed-dsa-validate
   
   #:compute-deterministic-elligator-skey
   #:compute-elligator-summed-pkey
   #:compute-elligator-schnorr-deterministic-random
   #:elligator-ed-dsa
   #:elligator-ed-dsa-validate

   #:ecc-pt
   #:ecc-pt-x
   #:ecc-pt-y
   #:make-ecc-pt

   #:ecc-proj-pt
   #:ecc-proj-pt-x
   #:ecc-proj-pt-y
   #:ecc-proj-pt-z
   #:make-ecc-proj-pt

   #:ecc-cmpr-pt
   #:ecc-cmpr-pt-cx
   #:make-ecc-cmpr-pt
   
   #:ed-vrf
   #:ed-prove-vrf
   #:ed-check-vrf

   #:modr
   #:modq

   #:with-embedding-field
   #:with-curve-field
   
   #:gen-key-pair
   #:gen-shares
   #:solve-shares
   #:skey

   #:smult

   #:*my-pkey*
   #:*my-skey*
   #:*curve1174*
   #:*curve-E382*
   #:*curve41417*
   #:*curve-Ed448*
   #:*curve-e521*
   #:*curve-Ed3363*

   #:ssig-nonce
   #:ssig-sign
   #:ssig-verify
   ))

(defpackage #:com.ral.crypto.finite-field
  (:use #:common-lisp #:def*)
  (:export
   #:ffbase
   #:ffbase-base
   #:ffbase-nbits
   #:ffbase-bits
   #:ffbase-wrap
   #:ffbase-sqrt-fn
   #:ffbase-montgy-ninv
   #:ffbase-montgy-rsq
   #:ffbase-inst-class
   
   #:basic-normalize

   #:with-field
   #:field-base
   
   #:ffld
   #:ffld-val
   #:copy-ffld
   
   #:ffield-mismatch
   #:need-same-ffield
   #:ffld-base
   #:ffld-class
   #:ffld-montgomery-class

   #:ff-normalize

   #:define-ffield
   #:define-ffield1

   #:ffadd
   #:ffadd=
   #:ffsub
   #:ffsub=
   #:ffneg
   #:ffmul
   #:ffmul=
   #:ffsqr
   #:ffsqr=
   #:ffinv
   #:ffdiv

   #:ff+
   #:ff-
   #:ff*
   #:ff/
   #:ff^

   #:ff=
   #:ff/=
   #:ff<
   #:ff<=
   #:ff>=
   #:ff>
   #:ff0=
   
   #:ffabs

   #:non-square-residue
   #:ffsqrt

   #:ffmod
   #:ff-signed
   #:ff-bezout
   #:ff-chi
   #:ff-quadratic-residue-p
   ))

(defpackage #:com.ral.crypto.core-crypto
  (:use #:common-lisp
   #:modmath
   #:finite-field
   #:edwards-ecc
   #:cached-var
   #:crypto-utils
   #:vec-repr
   #:prng
   #:hash)
  (:import-from #:pbc
   #:read-safely
   #:address
   #:addr
   #:addr-str)
  (:export
   #:defstub
   #:stub-function-p
   #:with-fast-impl
   
   ;; from crypto/modular-arith
   #:with-mod
   #:reset-blinders
   #:m=
   #:m^
   #:msqrt
   #:msqr
   #:m+
   #:m-
   #:m*
   #:m/
   #:minv
   #:bezout
   #:mmod
   #:mchi
   #:quadratic-residue-p
   #:m!

   ;; from crypto/finite-field
   #:with-field
   #:define-field
   #:ff=
   #:ff^
   #:ffsqrt
   #:ffsqr
   #:ff+
   #:ff-
   #:ff*
   #:ff/
   #:ffinv
   #:ffmod
   
   ;; from vec-repr
   #:bev
   #:lev
   #:base58
   #:base58-chk
   #:base64
   #:base85
   #:hex
   #:int
   #:int=
   #:int/=
   #:int<
   #:int<=
   #:int>
   #:int>=
   #:vec
   #:vec-cmp
   #:vec=
   #:vec/=
   #:vec<
   #:vec<=
   #:vec>
   #:vec>=
   #:str
   #:bev-vec
   #:lev-vec
   #:hex-str
   #:base58-str
   #:base64-str
   #:base85-str
   #:bevn
   #:levn
   ;; from hash
   #:hash
   #:hash/256
   #:hash/384
   #:hash/512
   #:hash-bytes
   #:hash-length
   #:hashable
   #:get-hash-nbytes
   #:hash=
   #:hash-check
   #:hash/ripemd/160
   #:hash/sha2/256
   ;; from edwards-ecc
   #:with-ed-curve
   #:set-ed-curve
   #:*edcurve*
   #:*ed-r*
   #:*ed-q*
   #:*ed-gen*
   #:ed-curve-name
   #:ed-neutral-point
   #:ed-neutral-point-p
   #:ed-mul
   #:ed-add
   #:ed-sub
   #:ed-div
   #:ed-negate
   #:ed-pt=
   #:ed-affine
   #:ed-compress-pt
   #:ed-decompress-pt
   #:ed-nth-proj-pt
   #:ed-nth-pt
   #:ed-random-pair
   #:ed-from-hash
   #:ed-random-generator
   #:ed-validate-point
   #:ed-valid-point-p
   #:ed-nbytes
   #:ed-nbits
   #:get-hash-nbits
   
   #:elli2-encode
   #:elli2-decode
   #:elli2-random-pt
   
   #:ed-convert-int-to-lev
   #:ed-convert-lev-to-int
   #:compute-deterministic-skey
   #:compute-schnorr-deterministic-random
   #:ed-dsa
   #:ed-dsa-validate
   
   #:compute-deterministic-elligator-skey
   #:compute-elligator-summed-pkey
   #:compute-elligator-schnorr-deterministic-random
   #:elligator-ed-dsa
   #:elligator-ed-dsa-validate

   #:convert-int-to-nbytes
   #:convert-int-to-nbytesv
   #:convert-bytes-to-int
   #:sha3-buffers
   #:sha3/256-buffers
   #:ctr-drbg
   #:ctr-drbg-int
   #:random-between
   #:field-random
   #:safe-field-random

   #:add-to-startups
   #:add-to-shutdowns
   #:ensure-dlls-loaded
   #:startup
   #:shutdown
   #:read-safely
   #:address
   #:addr
   #:addr-str

   #:convert-int-to-wordlist
   #:convert-wordlist-to-int
   ))

(defpackage #:com.ral.crypto.crypto-lib-loader
  (:use #:cl)
  (:export
   #:load-dlls
   #:unload-dlls
   ))

(defpackage #:com.ral.crypto.edwards-ecc-modmath
  (:nicknames #:edec-mm)
  (:use #:common-lisp
   #:edec
   #:modmath
   #:crypto-utils
   #:cached-var
   #:vec-repr
   #:hash
   #:prng)
  (:import-from #:um ;; #:com.ral.useful-macros
   #:defstub
   #:with-fast-impl)
  (:export
   ))

(defpackage #:com.ral.crypto.edwards-ecc-finite-field
  (:nicknames #:edec-ff)
  (:use #:common-lisp
   #:edec
   #:finite-field
   #:crypto-utils
   #:cached-var
   #:vec-repr
   #:hash
   #:prng)
  (:import-from #:um ;; #:com.ral.useful-macros
   #:defstub
   #:with-fast-impl)
  (:export
   ))


