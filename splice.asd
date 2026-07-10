;;;; splice.asd — system definition. SBCL-only (uses sb-mop, sb-thread,
;;;; sb-introspect); no dependencies outside SBCL contribs.

(defsystem "splice"
  :description "Safety net for hot-patching a running SBCL image: coherent
patch sets, SPRT-gated canary rollout, reversible class migration, and a
hash-chained provenance log."
  :author "splice project"
  :license "MIT"
  :depends-on ("sb-introspect" "sb-bsd-sockets")
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
                             (:file "lifecycle")
                             (:file "harness")
                             (:file "cluster")))))
