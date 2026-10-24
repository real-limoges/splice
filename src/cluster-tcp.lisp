;;;; cluster-tcp.lisp

(in-package #'splice/cluster)

(defvar *wire-lock* (sb-thread:make-mutex))
