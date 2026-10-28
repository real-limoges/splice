;;;; package.lisp

(defpackage #:splice-orders/demo-tests
  (:use #:common-lisp #:fiveam)
  (:local-nicknames (#:orders #:splice-orders) (#:d #:splice-orders/demo))
  (:export #:demo))

(in-package #:splice-orders/demo-tests)
