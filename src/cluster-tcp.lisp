;;;; cluster-tcp.lisp

(in-package #:splice/cluster)

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
  (let ((*package* (find-package :splice/cluster))
        (*read-eval* nil))
    (read stream nil :eof)))
