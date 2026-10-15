;;;; sha256.lisp: the hand-rolled SHA-256 against reference digests.
;;;; The FIPS 180-4 vectors cover one and two blocks; the runs of "a" sit on
;;;; either side of the padding boundaries (55/56 bytes leave or lose room
;;;; for the length, 64 fills a block exactly). Expected values come from
;;;; OpenSSL, not from this implementation.

(in-package #:splice-tests)

(def-suite sha256 :in splice-tests)
(in-suite sha256)

(defun a-run (n) (make-string n :initial-element #\a))

(test fips-180-4-vectors
  (is (string= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
               (sha256-hex "")))
  (is (string= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
               (sha256-hex "abc")))
  (is (string= "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
               (sha256-hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

(test padding-boundaries
  (is (string= "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
               (sha256-hex (a-run 1))))
  (is (string= "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"
               (sha256-hex (a-run 55))))
  (is (string= "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"
               (sha256-hex (a-run 56))))
  (is (string= "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34"
               (sha256-hex (a-run 63))))
  (is (string= "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
               (sha256-hex (a-run 64))))
  (is (string= "31eba51c313a5c08226adf18d4a359cfdfd8d2e816b13f4af952f7ea6584dcfb"
               (sha256-hex (a-run 119)))))

(test hashes-utf-8-not-characters
  (is (string= "c3a3e84b30327c8e3f04a788d8b68da3fd94c22353c5a30e3fe6ca7ebde3ab3f"
               (sha256-hex (format nil "h~cllo ~c"
                                   (code-char #xe9) (code-char #x2603))))))

(test hex-is-64-lowercase-digits
  (let ((hex (sha256-hex "splice")))
    (is (= 64 (length hex)))
    (is (every (lambda (c) (find c "0123456789abcdef")) hex))))

(test self-test-passes
  (is (eq t (sha256-self-test))))
