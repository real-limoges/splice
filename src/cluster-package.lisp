;;;; cluster-package.lisp: the cluster's package.

(defpackage #:splice/cluster
  (:use #:common-lisp)
  (:local-nicknames (#:h #:splice/harness))
  (:export #:run-member #:run-conductor #:currency-patch-spec))
