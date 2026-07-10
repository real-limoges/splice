# Artifact schema

## Phase 1 output

`artifacts/DESIGN.md` — the design document. Expected sections (see
PROJECT_BRIEF.md kickoff prompt for full detail):

1. Prior art and limits
2. Failure taxonomy (auto-rollback vs. human-in-the-loop), incl. SPRT-based
   canary health decision
3. Multi-function coherent patch-set abstraction + contract checks
4. Class redefinition + reverse migration on rollback
5. Gradual canary rollout mechanism
6. Provenance/audit log design
7. Cluster propagation sketch
8. Concrete success criteria

Stop condition: Fable halts after this file is written. Human review gates
Phase 2.

## Phase 2 outputs

- `src/` — patch-set core, canary dispatch (CLOS `eql`-specialized on
  version tag), SPRT evaluator, class-redefinition migration handling
- `src/harness.lisp` (or similar) — synthetic workload + realistic fault
  injection, extending smolder's failure-injection machinery where it
  generalizes
- `artifacts/audit-log-schema.md` — the actual shape of a provenance/audit
  log entry (patch applied, canary transition, rollback decision), defined
  once Fable builds the real log format
- `demo.lisp` or equivalent runnable script exercising a full
  patch → canary → detect → rollback cycle, including at least one
  multi-function patch set and one class redefinition with reverse
  migration
- (stretch) distributed propagation prototype across 2+ SBCL images, if
  attempted
