i;;;; harness-package.lisp: the synthetic workload's package (system splice).

(defpackage #:splice/harness
  (:use #:common-lisp)
  (:export
   #:run-workload #:run-canary-session #:make-rng #:rng-next #:rng-uniform
   #:rng-range

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
   #:coherence-violations #:reset-coherence-tracking))
