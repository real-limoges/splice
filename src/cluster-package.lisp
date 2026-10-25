;;;; cluster-package.lisp: the cluster's package.

(defpackage #:splice/cluster
  (:use #:common-lisp)
  (:export #:run-member #:run-conductor))
