;;;; sha256.lisp: pure-CL SHA-256 (FIPS 180-4), used for audit-log hash
;;;; chaining and source-form fingerprints. Self-contained so splice has no
;;;; dependencies outside SBCL itself.

(in-package #:splice)

(defconstant +sha256-k+
  (if (boundp '+sha256-k+) (symbol-value '+sha256-k+)
      (make-array 64 :element-type '(unsigned-byte 32)
                  :initial-contents
                  '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
                    #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                    #xd807aa98 #x12835b01 #x243185be #x550c7dc3
                    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
                    #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
                    #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
                    #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
                    #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
                    #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                    #x748f82ee #x78a5636f #x84c87814 #x8cc70208
                    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))))

(declaim (inline rot32 shr32 add32))
(defun rot32 (x n)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) n))
  (logior (ldb (byte 32 0) (ash x (- 32 n))) (ash x (- n))))
(defun shr32 (x n)
  (declare (type (unsigned-byte 32) x))
  (ash x (- n)))
(defun add32 (&rest xs)
  (ldb (byte 32 0) (apply #'+ xs)))

(defun sha256-pad (bytes)
  (let* ((len (length bytes))
         (bitlen (* 8 len))
         (padded-len (* 64 (ceiling (+ len 9) 64)))
         (out (make-array padded-len :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
    (replace out bytes)
    (setf (aref out len) #x80)
    (loop for i from 0 below 8
          do (setf (aref out (- padded-len 1 i))
                   (ldb (byte 8 (* 8 i)) bitlen)))
    out))

(defun sha256-digest (bytes)
  "SHA-256 of a (vector (unsigned-byte 8)); returns a 32-byte vector."
  (let ((h (make-array 8 :element-type '(unsigned-byte 32)
                         :initial-contents
                         '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                           #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
        (w (make-array 64 :element-type '(unsigned-byte 32)))
        (msg (sha256-pad bytes)))
    (loop for block-start from 0 below (length msg) by 64 do
      (loop for tt from 0 below 16
            for off = (+ block-start (* 4 tt))
            do (setf (aref w tt)
                     (logior (ash (aref msg off) 24)
                             (ash (aref msg (+ off 1)) 16)
                             (ash (aref msg (+ off 2)) 8)
                             (aref msg (+ off 3)))))
      (loop for tt from 16 below 64
            for s0 = (logxor (rot32 (aref w (- tt 15)) 7)
                             (rot32 (aref w (- tt 15)) 18)
                             (shr32 (aref w (- tt 15)) 3))
            for s1 = (logxor (rot32 (aref w (- tt 2)) 17)
                             (rot32 (aref w (- tt 2)) 19)
                             (shr32 (aref w (- tt 2)) 10))
            do (setf (aref w tt)
                     (add32 (aref w (- tt 16)) s0 (aref w (- tt 7)) s1)))
      (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
            (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
        (loop for tt from 0 below 64
              for bigs1 = (logxor (rot32 e 6) (rot32 e 11) (rot32 e 25))
              for ch = (logxor (logand e f) (logandc1 e g))
              for t1 = (add32 hh bigs1 ch (aref +sha256-k+ tt) (aref w tt))
              for bigs0 = (logxor (rot32 a 2) (rot32 a 13) (rot32 a 22))
              for maj = (logxor (logand a b) (logand a c) (logand b c))
              for t2 = (add32 bigs0 maj)
              do (setf hh g g f f e
                       e (add32 d t1)
                       d c c b b a
                       a (add32 t1 t2)))
        (setf (aref h 0) (add32 (aref h 0) a) (aref h 1) (add32 (aref h 1) b)
              (aref h 2) (add32 (aref h 2) c) (aref h 3) (add32 (aref h 3) d)
              (aref h 4) (add32 (aref h 4) e) (aref h 5) (add32 (aref h 5) f)
              (aref h 6) (add32 (aref h 6) g) (aref h 7) (add32 (aref h 7) hh))))
    (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 8
            do (loop for j from 0 below 4
                     do (setf (aref out (+ (* 4 i) j))
                              (ldb (byte 8 (* 8 (- 3 j))) (aref h i)))))
      out)))

(defun string-to-utf8 (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun sha256-hex (string)
  "Hex SHA-256 of STRING (UTF-8)."
  (string-downcase
   (with-output-to-string (s)
     (loop for b across (sha256-digest (string-to-utf8 string))
           do (format s "~2,'0x" b)))))

(defun sha256-self-test ()
  "Validate against FIPS 180-4 test vectors; error if wrong."
  (assert (string= (sha256-hex "")
                   "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  (assert (string= (sha256-hex "abc")
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
  (assert (string= (sha256-hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                   "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"))
  t)
