;;;; cluster-tcp.lisp: cluster protocol over loopback TCP
;;;;
;;;; One printed list per line. Member serves one condutor connection
;;;; and drives its host's workload once commited. Connductor
;;;; connects to every member, starts a reader thread per member
;;;; that hands messages to CONDUCTOR-RECEIVE, ships the patch
;;;; and waits for the cluster to finish.

(in-package #:splice/cluster)


(defvar *wire-lock* (sb-thread:make-mutex)
  "Serializes writes to every cluster stream in the image.
Replies, health reports, and the watch thread's message never interleave")


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
