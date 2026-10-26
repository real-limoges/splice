;;;; splice.asd: definition file (SBCL only)

(defsystem "splice"
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
                             (:file "lifecycle"))))
  :in-order-to ((test-op (test-op "splice/tests"))))

(defsystem "splice/cluster"
  :description "The cluster protocol as values: patch specs, the conductor and
the member. No transport, so like splice it loads no SBCL contribs."
  :author "splice project"
  :license "MIT"
  :depends-on ("splice")
  :serial t
  :components ((:module "src"
                :components ((:file "cluster-package")
                             (:file "cluster-protocol"))))
  :in-order-to ((test-op (test-op "splice/cluster-tests"))))

(defsystem "splice/cluster-tcp"
  :description "The cluster's TCP transport: sexps over sockets."
  :author "splice project"
  :license "MIT"
  :depends-on ("splice/cluster" "sb-bsd-sockets")
  :serial t
  :components ((:module "src"
                :components ((:file "cluster-tcp")))))

(defsystem "splice/tests"
  :description "Unit tests. Fast enough to run on every change."
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

(defsystem "splice/cluster-tests"
  :description "Unit tests for cluster protocol. Shares the unit suite's
package and fixtures and runs *no* socket."
  :depends-on ("splice/tests" "splice/cluster" "fiveam")
  :serial t
  :components ((:module "test/unit"
                :components ((:file "protocol"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (unless (every (lambda (suite)
                                     (uiop:symbol-call :fiveam :run!
                                                       (uiop:find-symbol* suite :splice-tests)))
                                   '(:cluster-protocol))
                      (error "splice cluster tests failed"))))
