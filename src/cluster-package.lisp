;;;; cluster-package.lisp: the cluster's package.

(defpackage #:splice/cluster
  (:use #:common-lisp)
  (:export
   ;; the transport-free protocol (system splice/cluster)
   #:make-conductor #:conductor-add-member #:conductor-ship
   #:conductor-receive #:conductor-phase #:conductor-finals
   #:make-member #:member-receive #:member-send-health
   #:send-to-member #:send-to-conductor
   #:member-committed #:final-report-fields
   #:spec-hash #:spec-with-hash #:build-patch-set

   ;; the TCP transport (system splice/cluster-tcp)
   #:run-member #:run-conductor))
