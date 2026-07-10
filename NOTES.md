# Notes

Running notes, decisions, and dead ends as splice progresses.

## 2026-07-10 — Phase 1

- `artifacts/DESIGN.md` written; sources verified by live search and pinned in
  PROCENANCE.md. Stopped for review per the brief — Phase 2 gated on go-ahead.
- Dead end considered and rejected: canarying class *shape* per-task via
  parallel class hierarchies + identity splitting (`become:`-style). Cost and
  identity hazards disproportionate; union-shape + write-through chosen
  instead (DESIGN.md §5.1, §11.2).
- Dead end considered and rejected: stop-the-world for multi-symbol atomic
  swap. SBCL exposes no safe userland stop-the-world; shim indirection + one
  CAS on the epoch cell gets the same observable atomicity without it.
- Flagged for reviewer attention: the three open questions in DESIGN.md §11.5
  (paired-fallback default, journal retention policy, method-patch scope).

## 2026-07-10 — Phase 2 (build)

Reviewer approved the design including the §11.5 leans (paired fallback
opt-in, time-based journal retention + seal-patch, primary-method-only
patching in v1); built to those.

Environment: SBCL 2.2.9 (Debian package), installed fresh in this container.
smolder's failure-injection machinery could not be pulled in (repo access
requires an interactive approval unavailable in this autonomous session), so
the fault patterns were implemented natively against the same taxonomy the
design specifies (latent / load-correlated / slow-burn / delayed-onset /
migration-time).

Bugs the harness caught in splice itself while building (all fixed, all
covered by criteria now):

- **Swallowed-outcome bug**: an inner `handler-case` (error-swallowing task
  mode) unwound before the outer `handler-bind` recorded the failure — every
  canary failure reported as success, so a latent fault sailed to promotion.
  The fix records the outcome in whichever handler actually receives it.
- **Promotion coherence hole**: fresh tasks during `:promoting` pinned :old
  and would have straddled the destructive fdefinition swap mid-task.
  `:promoting` now pins :new.
- **Old-code-created canary instances**: rollback synthesis would have
  overwritten their correct old slots with values derived from default new
  slots. Creator route is now recorded at `initialize-instance` and
  old-born instances are left untouched at reversal (`:old-born-unchanged`).
- **Weak-table corruption on SBCL 2.2.9**: reproducible
  "Corrupt NEXT-chain" crashes in the migration journal under GC pressure +
  concurrency, even with all access mutex-guarded. Deviation from design
  §5.3: strong keys + explicit lifecycle purging instead of weak keys
  (entries released at rollback / promotion / seal).
- **Mutex starvation**: a zero-think-time task loop starved the rollback
  completion thread; the harness stops driving load once the decision state
  (`:rolling-back`/`:promoting`) is reached.
- **Heap exhaustion at scale**: chain verification originally slurped the
  whole log; 700k-entry criteria logs blew the 1GB heap. Verification and
  reconstruction are now streaming, and the logger's in-memory tail is
  bounded.

Dead ends:

- Instantaneous-concurrency thresholds for the load-correlated fault: OS
  scheduler noise (sleep granularity, mutex convoys) made "N tasks in the
  slow path simultaneously" trip even at a 5% ramp. Replaced with a
  windowed traffic-share mechanism (canary calls > 12% of a 10ms window's
  traffic), which is machine-independent and cleanly separates the 5% stage
  from the 25% stage — 100/100 runs caught at 25%+, 0 at 5%.
- `&rest`/`apply` shims measured exactly 2.0x a plain call on a 24ns
  function — over the criterion. Fixed-arity shim factories (compiled once
  per arity) plus folding the coherence trace into the route-lookup assoc
  brought it to 1.33x.

Results: all DESIGN.md §10 criteria pass — see artifacts/criteria-report.md.
The distributed stretch (§10.8) was built and passes: two member SBCL images,
source-shipped patch set (hash-verified), fault on one member only, cluster
rollback on both, chains and reconstructions intact (`demo-cluster.lisp`).
