;;;; package.lisp: package definition for splice

(defpackage #:splice
  (:use #:common-lisp)
  (:export
   ;; patch-set declaration and registry
   #:defpatch-set #:register-patch-set #:find-patch-set #:unregister-patch-set
   #:patch-set #:patch-set-name #:patch-set-state #:patch-set-description
   #:new                                ; contract-time accessor for new fns

   ;; lifecycle
   #:install #:promote #:roll-back #:freeze #:unfreeze #:seal-patch
   #:wait-for-state
   #:patch-set-lifecycle-history

   ;; task scope / canary router
   #:with-task #:*task-id* #:current-route #:task-route-for

   ;; SPRT
   #:make-sprt #:sprt-observe #:sprt-verdict #:sprt-llr #:sprt-n
   #:sprt-upper #:sprt-lower

   ;; outcome reporting (harness → evaluator)
   #:report-outcome

   ;; class migration
   #:migration-journal-count #:reverse-migration-report

   ;; audit
   #:open-audit-log #:close-audit-log #:audit #:*audit-log*
   #:verify-audit-chain #:reconstruct-state #:verify-reconstruction

   ;; conditions & restarts
   #:splice-condition #:patch-vetoed #:canary-anomaly #:canary-verdict
   #:migration-failure #:rollback-initiated #:rollback-failure
   #:fallback-to-old #:force-promote #:abort-patch

   ;; condition readers
   #:patch-vetoed-contract #:patch-vetoed-detail
   #:canary-anomaly-cause #:canary-anomaly-task
   #:migration-failure-instance #:migration-failure-cause

   ;; utilities
   #:sha256-hex #:sha256-self-test))

(defpackage #:splice/harness
  (:use #:common-lisp #:splice)
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
   #:session-result-mixed-executions #:session-result-tasks-run
   #:session-result-detection-observations #:session-result-veto-p
   #:session-result-reversal-report #:session-result-pre-existing-snapshot
   #:session-result-post-rollback-comparison #:session-result-wallclock-ms
   #:coherence-violations #:reset-coherence-tracking))

(defpackage #:splice/cluster
  (:use #:common-lisp)
  (:export #:run-member #:run-conductor #:currency-patch-spec))
