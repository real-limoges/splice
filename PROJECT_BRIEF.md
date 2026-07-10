# splice

Live-patching safety net for SBCL — hot-patching a running image treated as a
reversible, monitored, auditable operation rather than a one-way mutation.

## Structure note

Unlike prior projects in this family (wildfire, traffic, parliament, cascade,
taiga, slate, apron, smolder), splice is **not** a two-phase Fable/Opus
pipeline. It's a single Fable session with an internal checkpoint: Fable
produces a design document, stops for human review, then continues to build
the prototype against the reviewed design. There is no separate Opus phase.

## Phase 1 — design document

Fable produces `artifacts/DESIGN.md` covering prior art and its limits, a
failure taxonomy for auto-rollback vs. human-in-the-loop, the multi-function
coherent patch-set abstraction with contract checks, class redefinition via
`update-instance-for-redefined-class` (including reverse migration on
rollback), gradual canary rollout gated on an SPRT decision, the
provenance/audit log design, and a sketch of cluster-wide patch propagation.
Concrete, harder success criteria close out the document. **Fable stops here
for review — do not proceed to Phase 2 without explicit go-ahead.**

## Phase 2 — prototype (after review)

Fable builds the full system against the reviewed design: coherent
multi-function patch-set commits with contract checks, SPRT-based canary
health evaluation with gradual ramp, `update-instance-for-redefined-class`
migration with reverse-migration-on-rollback, a full provenance/audit log,
and a synthetic workload harness with realistic (not just uniform-random)
injected fault patterns — reusing or extending smolder's failure-injection
machinery where it generalizes. Distributed propagation across 2+ SBCL
images is attempted if time allows, otherwise left as a design artifact only.

## Kickoff prompt (paste to Fable)

```
Splice is a safety net for hot-patching a running SBCL image in production,
treating code redefinition as a reversible, monitored, auditable operation
rather than a one-way mutation or a toy demo. CL has raw material no other
ecosystem has — near-instant fdefinition swaps, update-instance-for-redefined-class
for migrating live instances, a condition system that can carry rollback
logic natively — and the goal here is to actually prove out that advantage
under real pressure, not just sketch it.

Phase 1 — design document. Before writing implementation code, produce a
design document covering:

- Prior art and its limits: Erlang's two-version cap, Java HotSwap/JRebel's
  method-body-only constraint, Smalltalk image-mutation culture, and — reach
  further — any academic work on live patching in OS kernels (ksplice,
  kpatch) for how they handle multi-symbol coherent patches, since that's
  the harder problem here too.
- A failure taxonomy (auto-rollback triggers vs. human-in-the-loop cases),
  extended with a statistical dimension: sequential hypothesis testing
  (e.g. a simple SPRT) to decide "this canary is failing" with a
  false-positive rate bound, not just a raw counter.
- Multi-function coherent patch sets as core, not deferred. Design an atomic
  patch-set commit: multiple fdefinition/method swaps that succeed or
  revert together, with a contract-check step (pre/post-condition
  assertions between functions in the set) that can veto a patch-set
  before it goes live.
- Class redefinition as core. update-instance-for-redefined-class migration
  of live instances mid-canary is a first-class deliverable — including
  what happens when a canary is rolled back after instances have already
  been migrated forward (reverse migration).
- Canary dispatch with gradual rollout (5% → 25% → 100% ramps gated on the
  SPRT decision) rather than a single fixed split.
- Provenance/audit log as a real artifact: every patch, canary state
  transition, and rollback decision recorded with enough detail to
  reconstruct the image's full mutation history.
- A distributed dimension: design how a patch-set would propagate across a
  cluster of SBCL images and how canary health signals would aggregate
  cluster-wide, even if not fully built.
- Concrete, harder success criteria: a patch-set touching 3+ interdependent
  functions plus a class redefinition, injected with realistic failure
  patterns, should be caught and fully reverted — including reversed
  instance migration — with a complete audit trail.

Write this to artifacts/DESIGN.md. Stop after this document for review.

Phase 2 — prototype (after review). Build the full system against the
design: coherent multi-function patch-set commits with contract checks,
SPRT-based canary health evaluation with gradual ramp,
update-instance-for-redefined-class migration with
reverse-migration-on-rollback, full provenance/audit log, and a synthetic
workload harness with realistic injected fault patterns (reuse or extend
smolder's failure-injection machinery). If time allows, prototype the
distributed propagation design against 2+ SBCL images rather than leaving
it purely on paper. Scope is otherwise a single SBCL image — no
distributed/multi-node coordination required for a complete result.
```
