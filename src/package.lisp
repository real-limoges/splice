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

   ;; outcome reporting (harness -> evaluator)
   #:report-outcome

   ;; class migration
   #:migration-journal-count #:reverse-migration-report

   ;; audit
   #:open-audit-log #:close-audit-log #:audit #:*audit-log*
   #:verify-audit-chain #:reconstruct-state #:verify-reconstruction

   ;; conditions & restarts
   #:splice-condition #:patch-vetoed #:canary-anomaly #:canary-verdict
   #:migration-failure #:rollback-initiated #:rollback-failure
   #:illegal-transition
   #:fallback-to-old #:force-promote #:abort-patch

   ;; condition readers
   #:splice-condition-patch-set
   #:patch-vetoed-contract #:patch-vetoed-detail
   #:canary-anomaly-cause #:canary-anomaly-task
   #:canary-verdict-stream #:canary-verdict-direction
   #:canary-verdict-n #:canary-verdict-llr
   #:migration-failure-instance #:migration-failure-direction
   #:migration-failure-cause
   #:rollback-initiated-trigger #:rollback-failure-cause

   ;; utilities
   #:sha256-hex #:sha256-self-test))

