# splice

A safety net for hot-patching a running SBCL image: code redefinition as a
reversible, monitored, auditable operation. Coherent multi-function patch
sets ride a 5% → 25% → 100% canary ramp gated by Wald sequential probability
ratio tests; class redefinitions migrate live instances forward with a
journal that makes rollback exact; every decision lands in a hash-chained
provenance log that can reconstruct the image's mutation history.

Design: `artifacts/DESIGN.md`. Measured results: `artifacts/criteria-report.md`.
Log format: `artifacts/audit-log-schema.md`. A real log: `artifacts/demo-audit.log`.

## Requirements

SBCL only (uses sb-mop, sb-thread, sb-introspect, sb-bsd-sockets). No
external libraries — even SHA-256 is in-tree. Developed against SBCL 2.2.9
(stock Debian/Ubuntu package).

## Run it

```sh
# the narrated demo: clean patch promotes; faulty patch is caught mid-ramp
# and fully reverted (reverse instance migration included); audit chain
# verified, state reconstructed from the log, tampering detected
sbcl --non-interactive --load demo.lisp

# every success criterion from DESIGN.md §10, with measured numbers
# (~3-5 min; writes artifacts/criteria-report.md)
sbcl --non-interactive --load criteria.lisp
SPLICE_RUNS=10 sbcl --non-interactive --load criteria.lisp   # quicker pass

# the distributed stretch (§9/§10.8): 2 member SBCL images + a conductor;
# fault injected on one member; pooled-SPRT/local-reject aggregation rolls
# back the whole cluster
sbcl --non-interactive --load demo-cluster.lisp
```

## Using the library

```lisp
(asdf:load-system "splice")
(splice:open-audit-log #p"audit.log")

(splice:defpatch-set fix-currency
    (:description "floats -> minor units" :ramp '(5 25 100))
  (:function price-order (lambda (order) ...))
  (:function total-invoice (lambda (order) ...))
  (:method (print-total order stream) (lambda (order stream) ...))
  (:class order
    :add-slots ((amount-minor-units :initform 0))
    :drop-slots (float-total)
    :migrate-forward (lambda (inst old-values) ...)
    :migrate-backward (lambda (inst new-values) ...))
  (:contract invoice-exact
    :check (lambda () (= 1099 (funcall (splice:new 'total-invoice) o)))))

(splice:install 'fix-currency)   ; contracts gate -> shims -> CANARY-5

;; mark task boundaries; each task pins all-old or all-new per patch set
(splice:with-task () (process-order order))
```

Task outcomes feed the per-stage SPRTs automatically; the set advances,
rolls back (restoring the captured originals and reverse-migrating
instances), promotes, or freezes for a human (`splice:unfreeze`). Operator
entry points: `roll-back`, `promote`, `freeze`, `unfreeze`, `seal-patch`.

## Layout

| path | what |
|---|---|
| `src/` | the system: types, SHA-256, conditions, audit log, SPRT, per-task router, class migration, registrar/shims, lifecycle, workload harness, cluster prototype |
| `demo.lisp`, `demo-cluster.lisp`, `criteria.lisp` | runnable entry points |
| `artifacts/` | design doc, audit-log schema, criteria report, demo audit log |
| `PROCENANCE.md`, `NOTES.md` | sources, design decisions, build notes, dead ends |
