(in-package :pbc)

(defun make-keying-triple (public-key secret-key &optional signature)
  "Make a PBC:KEYING-TRIPLE from its integer representation of PUBLIC-KEY and SECRET-KEY"
  (make-instance 'pbc:keying-triple
                 :pkey public-key
                 :skey secret-key
                 :sig (or signature (pbc:sign-hash (hash:hash/256 public-key) secret-key))))

(defun make-keying-pairs ()
  "Makes a public/private keypair seeded via UUID:MAKE-V1-UUID

Returns a list of of the generated public and private keys as integers"
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     (pbc:keying-triple-pkey keypair)    ;; public is first
     (pbc:keying-triple-skey keypair)))) ;; private is second



