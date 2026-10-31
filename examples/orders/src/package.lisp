;;;; package.lisp: the orders example's package (system splice-orders).

(defpackage #:splice-orders
  (:use #:common-lisp)
  (:export
   #:run-workload #:run-canary-session #:make-rng #:rng-next #:rng-uniform
   #:rng-range #:*thread-prefixes*

   ;; the flagship as shippable source, for the cluster
   #:currency-patch-spec #:run-orders-member

   ;; the application under patch
   #:order #:order-id #:order-cents #:order-discount-code #:order-float-total
   #:order-item-count #:order-unit-price #:order-status
   #:price-order #:apply-discount #:total-invoice #:print-total
   #:process-order #:make-test-order
   #:register-currency-patch-set #:reset-workload #:make-pre-existing-orders
   #:snapshot-order

   ;; fault injection
   #:*fault* #:make-fault #:fault-description #:all-fault-kinds

   ;; session results
   #:session-result #:session-result-final-state #:session-result-transitions
   #:session-result-tasks-run
   #:session-result-detection-observations #:session-result-veto-p
   #:session-result-reversal-report #:session-result-pre-existing-snapshot
   #:session-result-post-rollback-comparison #:session-result-wallclock-ms

   ;; observer hooks
   #:live-session #:*live-session* #:live-session-patch-set
   #:live-session-orders #:live-session-seed #:live-session-fault
   #:live-session-threads #:live-session-started
   #:live-session-audit-start-seq #:live-session-paused-at
   #:old-pinned-task-id #:start-held-task))
