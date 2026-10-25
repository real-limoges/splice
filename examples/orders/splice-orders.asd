;;;; splice-orders.asd: the orders example (SBCL only). Needs splice.asd
;;;; loaded or on the source registry.

(defsystem "splice-orders"
  :description "The orders example: a synthetic order-processing service with
injected faults, the flagship patch set that moves it from float dollars to
rational minor units, and the session driver that runs it under canary."
  :author "splice project"
  :license "MIT"
  :depends-on ("splice")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "app")
                             (:file "flagship")
                             (:file "session")
                             (:file "cluster-spec")))))

(defsystem "splice-orders/criteria"
  :description "Executes every success criterion and writes
artifacts/criteria-report.md with the measured numbers."
  :depends-on ("splice-orders")
  :components ((:module "test" :components ((:file "criteria"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (unless (uiop:symbol-call :splice-orders/criteria :run-criteria)
                      (error "splice criteria failed; see artifacts/criteria-report.md"))))

(defsystem "splice-orders/demo"
  :description "The live-image demo: snapshots, the sampler, renderers, the
presenter gate, recording and the act driver. Everything testable without a browser."
  :depends-on ("splice-orders")
  :serial t
  :components ((:module "src/demo"
                :components ((:file "package")
                             (:file "record")))))
