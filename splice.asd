;;;; splice.asd: definition file (SBCL only)

(defsystem "splice/core"
  :description "The splice library. The coherent patch sets, the SPRT-gated canary rollout,
the reversible class migration, and the hash-chained log. Very specifically loads no SBCL contribs."
  :author "splice project"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "sha256")
                             (:file "types")
                             (:file "conditions")
                             (:file "audit")
                             (:file "sprt")
                             (:file "router")
                             (:file "migration")
                             (:file "patch")
                             (:file "lifecycle")))))

(defsystem "splice"
  :description "Safety net for hot-patching a running SBCL image: coherent
patch sets, SPRT-gated canary rollout, reversible class migration, and a
hash-chained log. The umbrella is splice/core, the synthetic workload, and
the cluster prototype."
  :author "splice project"
  :license "MIT"
  :depends-on ("splice/core" "sb-bsd-sockets")
  :serial t
  :components ((:module "src"
                :components ((:file "harness-package")
                             (:file "harness")
                             (:file "cluster-package")
                             (:file "cluster"))))
  :in-order-to ((test-op (test-op "splice/tests"))))

(defsystem "splice/tests"
  :description "Unit tests. Fast enough to run on every change; the criteria
run is (asdf:test-system \"splice/criteria\")."
  :depends-on ("splice" "fiveam" "sb-introspect")
  :serial t
  :components ((:module "test/unit"
                :components ((:file "package")
                             (:file "sha256")
                             (:file "sprt")
                             (:file "router")
                             (:file "audit")
                             (:file "patch")
                             (:file "migration")
                             (:file "lifecycle"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (unless (uiop:symbol-call :fiveam :run!
                                              (uiop:find-symbol* :splice-tests
                                                                 :splice-tests))
                      (error "splice unit tests failed"))))

(defsystem "splice/criteria"
  :description "Executes every success criterion and writes
artifacts/criteria-report.md with the measured numbers."
  :depends-on ("splice")
  :components ((:module "test" :components ((:file "criteria"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (unless (uiop:symbol-call :splice-criteria :run-criteria)
                      (error "splice criteria failed; see artifacts/criteria-report.md"))))

(defsystem "splice/demo"
  :description "The live-image demo: snapshots, the sampler, renderers, the
presenter gate, recording and the act driver. Everything testable without a browser."
  :depends-on ("splice")
  :serial t
  :components ((:module "src/demo"
                :components ((:file "package")
                             (:file "record")))))
