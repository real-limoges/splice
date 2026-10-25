;;;; cluster-tcp.lisp

(in-package #'splice/cluster)

(defvar *wire-lock* (sb-thread:make-mutex))


(defun send-form (stream form)
  (sb-thread:with-mutex (*wire-lock*)
    (let ((*package* (find-package :splice/cluster))
          (*print-pretty* nil))
      (prin1 form stream)
      (terpri stream)
      (force-output stream))))

(defun read-form (stream)
  "Next message on STREAM (or :EOF). *READ-EVAL* is off so a peer's
text never runs here"
  (let ((*package (find-package :splice/cluster))
        (*read-eval* nil))
    (read stream nil :eof)))


;;; Demo's patch set spec

(defun harness-source (form)
  "FORM as source text read back in splice/harness"
  (with-standard-io-syntax
    (let ((*package* (find-package :splice/harness))
          (*print-pretty* nil))
      (prin1-to-string form))))

(defun currency-patch-spec ()
  "Flagship patch set as shippable source code. CONDUCTOR-SHIP ads
the hash")
