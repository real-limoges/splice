# Splice — Design Document

**Status:** Phase 1 deliverable, awaiting human review. No implementation code exists yet.
**Scope:** A safety net for hot-patching a running SBCL image, treating code
redefinition as a reversible, monitored, auditable operation.

---

## Table of contents

1. [Goals and non-goals](#1-goals-and-non-goals)
2. [Prior art and its limits](#2-prior-art-and-its-limits)
3. [Architecture overview](#3-architecture-overview)
4. [Coherent multi-function patch sets](#4-coherent-multi-function-patch-sets)
5. [Class redefinition and reverse migration](#5-class-redefinition-and-reverse-migration)
6. [Canary dispatch and gradual rollout](#6-canary-dispatch-and-gradual-rollout)
7. [Failure taxonomy and the SPRT health decision](#7-failure-taxonomy-and-the-sprt-health-decision)
8. [Provenance and audit log](#8-provenance-and-audit-log)
9. [Distributed propagation sketch](#9-distributed-propagation-sketch)
10. [Success criteria](#10-success-criteria)
11. [Known limitations and open questions](#11-known-limitations-and-open-questions)

---

## 1. Goals and non-goals

### Goals

- **Reversibility as an invariant.** Every patch-set application must be
  revertible to a state observably identical to the pre-patch state — including
  CLOS instances that were migrated forward during the canary.
- **Coherence for multi-function patches.** A patch touching N interdependent
  functions must never expose a single logical task (request) to a mixture of
  old and new implementations. This is the hard problem kernel live-patchers
  solve with stop-the-world or per-task consistency; we solve it with
  per-task version pinning (§4.2).
- **Statistically grounded rollback.** "The canary is failing" is a sequential
  hypothesis test with bounded false-positive rate (Wald SPRT, §7.3), not a
  raw error counter.
- **Class redefinition as a first-class citizen**, using
  `update-instance-for-redefined-class`, with journaled, reversible instance
  migration (§5).
- **Complete provenance.** The audit log alone must suffice to reconstruct the
  image's full mutation history (§8).

### Non-goals

- Patching the SBCL runtime, compiler, or foreign (FFI) code.
- Surviving image-level corruption (heap damage, runtime bugs). Those are
  fail-stop: splice detects, freezes, and recommends restart — it does not
  pretend rollback fixes a damaged heap.
- Full distributed coordination. The cluster design (§9) is a sketch; Phase 2
  prototypes it against 2+ images only if time allows.
- Transparent patching of *inlined* or *block-compiled* call sites. These
  bypass fdefinition indirection; splice detects and vetoes/warns rather than
  silently half-patching (§11.1).

---

## 2. Prior art and its limits

### 2.1 Erlang/OTP hot code loading

Erlang keeps at most **two versions** of a module live: *current* and *old*.
Fully-qualified (`Mod:fun`) calls always reach current code; local calls stay
in the version the process is executing. Loading a third version **purges** the
old one, and any process still lingering in it is killed
([Erlang code loading docs](https://www.erlang.org/doc/system/code_loading.html),
[`code` module docs](https://www.erlang.org/doc/apps/kernel/code.html)).

**Lessons and limits.** The current/old split is a coarse two-version epoch
system with a brutal escape hatch (kill lingering processes on purge). There is
no canary, no health evaluation, no rollback story — `release_handler` can
downgrade, but the decision to do so is entirely human. The external-call
indirection (calls to the module name reach whatever is current) is the
ancestor of our shim indirection. Splice generalizes the two-version cap into
explicit *patch epochs*, but deliberately keeps Erlang's discipline: **at most
one in-flight canary per function** — overlapping patch-sets touching the same
function are queued or vetoed, never stacked.

### 2.2 Java HotSwap and JRebel

Stock JVM HotSwap (`RedefineClasses`) is restricted to **method-body-only**
changes: no adding/removing methods or fields, no hierarchy changes, because
instances are fixed-size memory structures that cannot be resized in place
([JRebel's HotSwap guide](https://www.jrebel.com/blog/java-hotswap-guide),
[HotswapAgent](https://github.com/HotswapProjects/HotswapAgent)). JRebel works
around this at the classloader/bytecode layer, translating classes on the fly
through an extra indirection layer
([JRebel: reloading classes at runtime](https://www.jrebel.com/blog/how-to-reload-java-classes-at-runtime)).

**Lessons and limits.** HotSwap's constraint is exactly what CLOS was designed
to avoid: `update-instance-for-redefined-class` makes instance-schema change a
*specified language feature*, not a VM hack. JRebel's indirection layer
validates our approach — put a routing layer between call site and
implementation and version changes become cheap — but JRebel is a
developer-productivity tool: no production canary, no health signal, no
rollback semantics, no audit trail. Splice is what JRebel would need to become
to be trusted in production.

### 2.3 Smalltalk image-mutation culture

Smalltalk (Squeak/Pharo lineage) pioneered live class redefinition: the
ClassBuilder rebuilds the class, then migrates every live instance to the new
shape (historically via bulk `become:`, forwarding all references from old
instances to newly-built ones). The whole development experience *is* image
mutation ([Pharo class redefinition overview](https://copyprogramming.com/howto/programmatically-create-and-delete-classes-in-pharo-smalltalk)).

**Lessons and limits.** Smalltalk proves that a live image with full-fidelity
instance migration is a workable, even pleasant, way to build software — but
the culture is *developer-time*, not production-time. Migration is eager,
global, and irreversible; there is no notion of running old and new shapes
side by side, no health gate, and the "audit log" is at best a changeset
browser. Splice takes the ambition (the image is mutable, instances migrate)
and adds what production demands: journaling, reversal, and gating.

### 2.4 Kernel live patching: Ksplice, kpatch, kGraft, livepatch

This is the closest prior art to our hard problem — **multi-symbol coherent
patches against a system that cannot stop**.

- **Ksplice** (Arnold & Kaashoek, EuroSys 2009) constructs patches at the
  object-code layer and applies them atomically: it briefly captures all CPUs
  (`stop_machine`), checks that **no thread's stack contains a frame in any
  to-be-patched function** (the *activeness safety check*), and only then
  redirects all patched functions at once. If the check fails it backs off and
  retries. For data-structure changes it uses *shadow data structures* hung
  off existing objects ([paper PDF](https://pdos.csail.mit.edu/papers/ksplice:eurosys.pdf),
  [ACM DL](https://dl.acm.org/doi/10.1145/1519065.1519085)).
- **kpatch** (Red Hat) likewise used a stop-and-check model; **kGraft** (SUSE)
  instead used lazy **per-task migration**: each task is switched from old to
  new world at a safe point (syscall boundary), and until every task has
  crossed, both versions run simultaneously with an ftrace-based selector
  ([LWN on kpatch](https://lwn.net/Articles/597123/),
  [SUSE kGraft guide](https://documentation.suse.com/sles/12-SP5/html/SLES-all/cha-kgraft.html)).
- The upstream **livepatch** consistency model is a hybrid: kGraft's per-task
  consistency plus kpatch's stack-trace checking. Patching enters a
  *transition state*; each task converges to the patched state when its stack
  is verifiably clear of patched functions, and interrupt handlers inherit the
  patched state of the task they interrupt
  ([kernel livepatch docs](https://docs.kernel.org/livepatch/livepatch.html)).

**Lessons adopted directly:**

1. **Per-task consistency is the right coherence model.** A logical task picks
   one world (all-old or all-new) at a well-defined boundary and keeps it for
   its lifetime. This is the core of our canary router (§4.2), and it is
   *cheaper* for us than for the kernel: we control the request entry point,
   so we don't need stack inspection to find safe switching points.
2. **Atomic multi-symbol flip.** Ksplice redirects all symbols in a patch
   under one `stop_machine`. Our analogue: all shims consult a single
   patch-set epoch cell, so the "flip" is one atomic write (§4.3).
3. **Activeness checking still matters at promotion.** Indirection lets old
   and new coexist safely during canary, but *promotion* (destructive
   fdefinition swap) and *class un-redefinition* (rollback) benefit from a
   quiescence check — the moral equivalent of Ksplice's stack check (§4.4).
4. **Shadow data ≈ migration journal.** Ksplice's shadow structures inspired
   our per-instance migration journal for reversible class redefinition
   (§5.3).

### 2.5 What none of them have

None of these systems combine: (a) multi-symbol coherence, (b) *statistical*
canary gating, (c) reversible instance-schema migration, and (d) a
reconstructable audit trail. Kernel livepatching has (a) only; Erlang has a
weak (a); HotSwap/JRebel have none; Smalltalk has (c) without the "reversible."
The union is the thesis of splice — and CL is the only mainstream-adjacent
runtime where all four are implementable in userland, because fdefinition
cells, CLOS redefinition hooks, and the condition system are all specified,
programmable surface area.

---

## 3. Architecture overview

Seven components, one lifecycle.

```
             ┌──────────────────────────────────────────────────────┐
             │                      splice                          │
             │                                                      │
  defpatch-  │  ┌───────────┐   ┌──────────┐   ┌────────────────┐  │
  set forms ─┼─▶│ Registrar │──▶│ Contract │──▶│ Shim installer │  │
             │  │ (compile, │   │ checker  │   │ (fdefinition & │  │
             │  │  analyze) │   │ (veto)   │   │  method shims) │  │
             │  └───────────┘   └──────────┘   └───────┬────────┘  │
             │                                         │           │
             │  ┌───────────┐   ┌──────────┐   ┌───────▼────────┐  │
             │  │ Migration │◀──│  SPRT    │◀──│ Canary router  │  │
             │  │ engine    │   │ evaluator│   │ (per-task      │  │
             │  │ (u-i-f-r-c│   │ (ramp    │   │  epoch pinning)│  │
             │  │  + journal)│  │  gate)   │   └────────────────┘  │
             │  └───────────┘   └──────────┘                       │
             │        │              │                             │
             │  ┌─────▼──────────────▼─────────────────────────┐   │
             │  │ Audit logger (append-only, hash-chained)     │   │
             │  └──────────────────────────────────────────────┘   │
             │  ┌──────────────────────────────────────────────┐   │
             │  │ Cluster conductor (Phase-2 stretch, §9)      │   │
             │  └──────────────────────────────────────────────┘   │
             └──────────────────────────────────────────────────────┘
```

### Patch-set lifecycle state machine

```
 REGISTERED ──contract veto──▶ VETOED (terminal)
     │
  contracts pass
     ▼
  STAGED ──install──▶ CANARY-5 ──SPRT accept──▶ CANARY-25 ──accept──▶ CANARY-100
     │                   │                          │                     │
     │              SPRT reject / hard trigger ─────┴─────────┬───────────┤
     │                   │                                    │      SPRT accept
     │                   ▼                                    │           ▼
     │              ROLLING-BACK ──▶ ROLLED-BACK (terminal)   │      PROMOTED (terminal)
     │                   │                                    │
     │              rollback failure                     SPRT indecision
     │                   ▼                                at stage timeout
     └──abort──▶     FROZEN (human-in-the-loop) ◀─────────────┘
```

Every transition emits an audit-log entry. `FROZEN` means: canary ramp halted
at its current fraction, both versions still live, human decision required
(resume, rollback, or promote with override — all logged with the operator's
identity).

---

## 4. Coherent multi-function patch sets

### 4.1 The patch-set object

A patch set is declared as one form and is the *only* unit of application:

```lisp
(splice:defpatch-set fix-currency-representation
    (:description "Move order math from floats to rational minor units"
     :ticket "SPLICE-42")

  (:function price-order
    (lambda (order) ...new implementation...))

  (:function apply-discount
    (lambda (order code) ...new implementation...))

  (:function total-invoice
    (lambda (order) ...new implementation...))

  (:method (print-total (order stream))          ; method patch: gf + specializers
    (lambda (order stream) ...))

  (:class order
    :add-slots ((amount-minor-units :initform 0))
    :drop-slots (float-total)
    :migrate-forward  (lambda (instance discarded-plist)
                        (setf (slot-value instance 'amount-minor-units)
                              (round (* 100 (getf discarded-plist 'float-total)))))
    :migrate-backward (lambda (instance discarded-plist)
                        (setf (slot-value instance 'float-total)
                              (/ (getf discarded-plist 'amount-minor-units) 100.0))))

  (:contract round-trip-conservation
    :check (lambda ()
             (let ((o (make-test-order :cents 1099)))
               (= (funcall (splice:new #'total-invoice) o)
                  (splice:expected-total o)))))

  (:contract cross-function-consistency
    :check (lambda ()
             ;; new price-order output must be consumable by new apply-discount
             (typep (funcall (splice:new #'price-order) (make-test-order))
                    'exact-money))))
```

The registrar compiles every function in the set (compile failure ⇒ `VETOED`),
captures the **old world**: for each plain function the current
`fdefinition` (and `(setf name)` definition where applicable); for each method
the current method object found via `find-method`; for each class the current
`defclass` form-equivalent (slot definitions, superclasses) reconstructed via
the MOP. This captured old world is what rollback restores, and it is written
into the audit log *before* anything is installed.

### 4.2 Coherence: per-task epoch pinning

The central design decision. During canary, we do **not** swap fdefinitions to
new code. Instead, for every function in the set, we install a **shim**: a
funcallable object that routes each call to old or new based on the calling
task's pinned epoch.

- A **task** is a logical unit of work — one request in a server, one job in a
  worker pool. The application marks task boundaries with
  `(splice:with-task () ...)` (the Phase-2 harness does this at its request
  entry point).
- At task entry, the router makes **one** routing decision per active patch
  set: draw from the current ramp fraction (§6), then bind a task-local
  binding (a dynamic variable, thread-local by SBCL's binding semantics)
  `*task-epochs*` mapping patch-set → `:old` or `:new`.
- Every shim in that patch set consults `*task-epochs*`. Therefore a single
  task sees **all-old or all-new, never a mixture** — kGraft/livepatch
  per-task consistency ([livepatch docs](https://docs.kernel.org/livepatch/livepatch.html)),
  obtained for free because we own the task boundary and both worlds are
  simultaneously valid (no code is destroyed during canary).
- Code running *outside* any `with-task` scope (background threads that
  predate the canary, timers) sees the shim's default route: `:old` until
  promotion. This mirrors Erlang's "lingering process" semantics but without
  the kill-on-purge cliff — old code is never invalidated under a running
  task's feet.

Shim mechanics: the shim closes over `old-fn`, `new-fn`, and the patch-set
control block. Overhead is one special-variable lookup and one branch per
call — measured and reported by the Phase-2 harness (§10, target: canary-mode
overhead < 2× a plain funcall; **zero** overhead after promotion, because
promotion removes the shim entirely).

Method patches shim at method-function granularity: the shim method (same
specializers, same qualifiers) is installed via `add-method`, replacing the
old method, and its body routes between old and new method functions. The old
method object is retained for exact restoration.

### 4.3 Atomic commit and atomic revert

Multi-symbol atomicity reduces to **one word**:

- Installing shims is *not* the commit. Shims install one at a time, but each
  shim's initial routing table sends 100 % of traffic to `:old` — installation
  is behaviorally invisible.
- The **commit** is a single atomic write (`sb-ext:cas` /
  `sb-ext:atomic-update` on the patch-set control block's state slot) flipping
  the set from `STAGED` to `CANARY-5`. Tasks that sampled the control block
  before the flip run all-old; tasks after it may draw a canary route. There
  is no observable intermediate state — Ksplice needs `stop_machine` to get
  this property; the indirection buys it for one CAS.
- **Revert** is the same flip in reverse (`→ ROLLING-BACK`): the routing
  default returns to 100 % `:old` instantly (in-flight `:new`-pinned tasks
  finish on new code — coherence is preserved even during rollback; the
  router refuses *new* pins). Then shims are removed, restoring the captured
  old fdefinitions/method objects, then class rollback runs (§5.4), then the
  set reaches `ROLLED-BACK`.

### 4.4 Promotion and the quiescence check

Promotion (after `CANARY-100` passes its SPRT gate) destructively installs the
new world: `(setf (fdefinition name) new-fn)` for each function, permanent
`add-method` for each method, shims discarded. Before promoting, splice runs a
**quiescence check**, the userland analogue of Ksplice's activeness check: it
waits until no live task remains pinned `:old` (tracked by a per-set counter
incremented/decremented at task entry/exit). Unlike Ksplice we never *need*
to inspect foreign stacks — the counter is exact for task-scoped work — but
threads outside task scope are noted in the audit log as "unswitched at
promotion" with their `sb-thread:thread-name`, since they will only pick up
new code on their next call through the (now-swapped) fdefinition.

The class-migration journal (§5.3) is retained for a configurable window
after promotion, then dropped — after which rollback of that set is no longer
offered (audit-logged as `journal-expired`).

### 4.5 Contract checks (the veto gate)

Contracts run in `REGISTERED → STAGED`, *before* any shim exists, against the
new functions invoked directly (`splice:new` accessor) on synthetic inputs:

1. **Intra-function post-conditions** — new `total-invoice` returns an
   `exact-money`, never a float.
2. **Cross-function invariants** — the reason contracts live at the *set*
   level: new `price-order`'s output type feeds new `apply-discount`'s input
   contract; an encoder/decoder pair round-trips.
3. **Old/new differential checks** (optional) — run old and new on the same
   synthetic input and assert an expected relationship (equal results, or a
   documented intended difference).
4. **Static checks run automatically by the registrar:**
   - arity/lambda-list compatibility between old and new
     (`sb-introspect:function-lambda-list`) — incompatible arity with callers
     *outside* the set ⇒ veto;
   - inlining hazard: any function in the set proclaimed `inline`, or with
     callers compiled against an inline expansion ⇒ veto with explanation
     (§11.1);
   - macro-dependency closure: if the set redefines a macro, every function
     whose definition macroexpanded it must be in the set too
     (computed via `sb-introspect` who-calls/who-macroexpands data when
     available) — otherwise veto. This is the "ripple set" rule.

A failing contract signals `splice:patch-vetoed` (a condition carrying the
contract name, the failure detail, and the full patch-set identity) and the
set never touches a live code path. Contracts are also *sampled at runtime*
during canary (configurable rate) as post-hoc checks feeding the failure
taxonomy (§7.1).

---

## 5. Class redefinition and reverse migration

### 5.1 The asymmetry: classes cannot canary per-task

Functions can run two worlds side by side; **instance layout cannot**. A CLOS
class has one current definition; when redefined, every instance migrates
(lazily, at first touch) to the new layout via
`update-instance-for-redefined-class`
([CLHS](https://www.lispworks.com/documentation/HyperSpec/Body/f_upda_1.htm),
[CLHS §4.3.6](http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_4-3-6.html)).
So the design splits the worlds:

- **Code** in the patch set canaries per-task as in §4.
- **Class shape** goes live for *all* instances at `STAGED → CANARY-5`.
- Therefore **old code must tolerate the new shape** during canary. The
  registrar enforces this mechanically: slots that old code reads may not be
  dropped outright — a dropped slot is (behind the scenes) *retained* during
  canary and only physically removed at promotion. Concretely, the canary-time
  class definition is the **union shape**: old slots ∪ new slots. New code
  uses new slots; old code keeps seeing old slots, kept coherent by
  write-through synchronization in the migration functions where the patch
  author declares a correspondence (as `float-total ↔ amount-minor-units`
  above). Promotion applies the *final* (new-only) shape; rollback applies
  the old-only shape. This is the CLOS translation of Ksplice's shadow-data
  technique — extra state rides along until one world wins.

### 5.2 Forward migration mid-canary

At `STAGED → CANARY-5`, splice redefines the class to the union shape (a
programmatic `defclass` via the MOP or re-evaluated form). CLOS marks
instances obsolete; each instance migrates at first access, in whatever thread
touches it, via our `update-instance-for-redefined-class` method
specialized on the patched class, which:

1. journals the instance's pre-migration state (§5.3),
2. runs the author's `:migrate-forward` with the discarded-slot plist
   (per the CLHS protocol: added slots, discarded slots, property list of
   discarded values),
3. emits an audit entry (class, direction `:forward`, instance identity hash,
   journal sequence number),
4. on *any* error: signals `splice:migration-failure` — a **hard rollback
   trigger** (§7.1), because a half-migrated population is the one state we
   refuse to operate in.

Eager migration is available (`:eager t` walks a registered instance
population and touches each) for classes where lazy first-touch latency or
long-tail unmigrated instances are unacceptable; the Phase-2 harness exercises
both modes.

### 5.3 The migration journal

Reverse migration cannot rely on `:migrate-backward` alone: forward migration
may be lossy (rounding, dropped slots, collapsed states). The journal makes
rollback *exact* rather than best-effort:

- A weak-key hash table (`make-hash-table :weakness :key`) maps instance →
  journal record: `(patch-set-id, seq, plist of all pre-migration local-slot
  values captured cheaply before step 2 above)`. Weak keys mean journal
  entries die with their instances — no leak, and a dead instance needs no
  reverse migration by definition.
- Journal records are *also* streamed to the audit log (values rendered via
  bounded, readably-printable serialization; unprintable values are recorded
  as `#<opaque>` hashes and force the class's rollback mode to
  `:migrate-backward`-only, flagged as **lossy-reversal**, which downgrades
  auto-rollback to human-in-the-loop for that set — §7.2).

### 5.4 Reverse migration on rollback

On rollback of a set containing a class patch:

1. Router stops pinning `:new` (no new code will read new slots).
2. Splice waits for `:new`-pinned in-flight tasks to drain (bounded wait;
   overrun ⇒ `FROZEN`, because yanking the class out from under running new
   code is worse than pausing).
3. The class is redefined *back* to the captured old definition — this is
   itself a CLOS redefinition, triggering `update-instance-for-redefined-class`
   a second time, in the reverse direction. Our method now:
   - if a journal record exists: restore every old slot exactly from the
     journal (byte-identical rollback — success criterion §10.4);
   - else (instance was *created* during the canary, under the union shape,
     so it has no pre-migration state): run the author's `:migrate-backward`
     to synthesize old-shape values — these instances are flagged in the
     audit log as `synthesized-reversal`;
   - on error: `FROZEN` + `splice:rollback-failure` condition. Rollback
     failures are never silently swallowed.
4. Journal entries for the set are marked consumed; final audit entry records
   counts: exactly-restored, synthesized, dead-skipped.

This answers the brief's hard question — *what happens when a canary rolls
back after instances migrated forward* — with a three-way answer:
journal-exact restore for migrated survivors, author-defined synthesis for
canary-born instances, and an explicit frozen/human path when neither is safe.

---

## 6. Canary dispatch and gradual rollout

### 6.1 Ramp schedule

Default ramp: **5 % → 25 % → 100 %**, each stage gated by its own SPRT
decision (§7.3). Stages are configurable per patch set
(`:ramp '(1 5 25 100)` for riskier sets). At each stage:

- The router pins new tasks to `:new` with probability = stage fraction,
  using a per-task deterministic draw (hash of task id and patch-set id, so
  a task's routing is reproducible from the audit log — no
  `random`-state-dependent unreproducibility; this also makes the harness's
  "no mixed execution" property checkable offline).
- The SPRT for the stage starts fresh (statistics do not carry across stages;
  each stage must independently earn its promotion — carrying evidence
  forward would let a lucky 5 % stage subsidize a bad 25 % stage where
  load-correlated faults first appear).
- Stage advance (`SPRT accept`), stage hold (`continue` at timeout ⇒
  `FROZEN`), or rollback (`SPRT reject` / hard trigger) are the only exits.

### 6.2 Control/experiment bookkeeping

Every task records `(patch-set, route, outcome, latency)` into a bounded
in-memory ring, whether routed `:old` or `:new`. The `:old` stream is the
**control**: the SPRT compares canary failure behavior against
*contemporaneous* baseline, not a stale constant, so an ambient outage (a
dependency failing for everyone) does not read as a canary failure — that
situation (both arms degraded) is classified `confounded` and goes to the
human path (§7.2) rather than triggering a misleading auto-rollback.

### 6.3 Why not a fixed split

A single fixed split (say 50 %) maximizes blast radius exactly when knowledge
is minimal. The ramp bounds worst-case exposure: a patch that fails
deterministically dies in the 5 % stage having touched ~5 % of traffic for
only as long as the SPRT needs (for a gross failure, tens of observations —
see §7.3 expected-sample-size note).

---

## 7. Failure taxonomy and the SPRT health decision

### 7.1 Auto-rollback triggers (no human in the loop)

| # | Trigger | Detection point | Latency |
|---|---------|-----------------|---------|
| A1 | Error escaping new code | Shim boundary handler: any `error` propagating out of a `:new`-routed call is caught, logged, counted as a canary failure, and (policy) either re-signaled or **retried once on `:old`** ("paired fallback") | immediate count; rollback via A3 or A2 |
| A2 | Hard-failure classes | Specific conditions that indicate structural breakage regardless of rate: `splice:migration-failure`, storage-condition from new code, contract sampled false at runtime | immediate rollback, bypasses SPRT |
| A3 | SPRT reject on failure rate | Per-stage SPRT (§7.3) on task outcomes | bounded by SPRT expected sample size |
| A4 | SPRT reject on latency regression | Second SPRT on the Bernoulli variable "task latency > contemporaneous control p95" (canary arm p exceeds control's 0.05 by margin δ) | same machinery, different stream |
| A5 | Watchdog: task overrun | `:new`-pinned task exceeds hard deadline (hang/livelock pattern) — counts as failure outcome; K consecutive overruns ⇒ immediate rollback | deadline-bounded |

A1's paired fallback deserves emphasis: because both worlds are live, the shim
can catch a new-code error *and satisfy the request with old code in the same
task* — the canary failure costs a retry, not a user-visible error. The
condition system carries this natively: the shim's handler invokes a
`splice:fallback-to-old` restart established at task entry. That is a
capability none of the §2 systems has, and the Phase-2 demo shows it.

### 7.2 Human-in-the-loop cases (⇒ `FROZEN`, operator paged)

| # | Case | Why not automatic |
|---|------|-------------------|
| H1 | SPRT indecision at stage timeout | Evidence genuinely insufficient; auto-deciding either way violates the error bounds we promised |
| H2 | Confounded signal: control arm also degraded | Rollback would be superstition; the problem is ambient |
| H3 | Rollback failure (reverse migration error, restore error) | The safety net itself failed; automation must stop escalating |
| H4 | Lossy-reversal class patch (§5.3) hits a rollback trigger | Exact restoration impossible; a human accepts data synthesis or not |
| H5 | Unswitched non-task threads at promotion/rollback boundaries | Only a human knows if that background thread matters |
| H6 | Second rollback of the same patch set (resubmitted after fix) | Pattern suggests the failure model is wrong, not the code |

### 7.3 The SPRT decision

Per stage, per signal stream (errors; latency), splice runs a Wald sequential
probability ratio test
([Wald 1945](https://link.springer.com/chapter/10.1007/978-1-4612-0919-5_17);
[overview](https://en.wikipedia.org/wiki/Sequential_probability_ratio_test)):

- Each canary task outcome is Bernoulli: failure (1) or success (0).
- **H₀ (healthy):** failure probability `p = p₀`, where `p₀` is estimated
  from the contemporaneous control arm over a trailing window (floored at a
  configurable minimum, e.g. 0.001, so a zero-error control doesn't make the
  test infinitely trigger-happy).
- **H₁ (failing):** `p = p₁ = max(c·p₀, p₀ + Δ)` — a multiplicative *and*
  additive degradation margin (defaults: `c = 3`, `Δ = 0.02`), so the test is
  sensitive both when baseline is tiny and when it is moderate.
- After each canary observation `xᵢ`, accumulate the log-likelihood ratio:

  ```
  Λ ← Λ + xᵢ·ln(p₁/p₀) + (1-xᵢ)·ln((1-p₁)/(1-p₀))
  ```

- Decision thresholds with the classical Wald bounds for false-positive rate
  α (rolling back a healthy patch) and false-negative rate β (promoting a
  failing one):

  ```
  upper A = ln((1-β)/α)   →  Λ ≥ A ⇒ reject H₀ ⇒ canary failing ⇒ rollback
  lower B = ln(β/(1-α))   →  Λ ≤ B ⇒ accept H₀ ⇒ stage healthy ⇒ advance ramp
  otherwise continue sampling
  ```

  Defaults `α = 0.01`, `β = 0.05`: at most ~1 % of *healthy* patch stages get
  spuriously rolled back, and a truly degraded canary slips through a stage
  with probability ≤ ~5 % (and still faces the next stage's independent test).
- **Minimum sample floor** per stage (e.g. 50 canary tasks) before either
  boundary may fire, guarding against pathological early crossings when `p₀`
  is being re-estimated.
- **Stage timeout**: if `B < Λ < A` when the stage clock expires ⇒ H1
  (`FROZEN`), never a silent promote.

SPRT is the right tool here because it is *optimal in expected sample size*
for given (α, β) (Wald–Wolfowitz): a gross failure (p ≫ p₁) crosses A in a
handful of observations — fast rollback when it matters most — while a clean
canary accepts quickly and ramps. A fixed-sample test would either dawdle on
disasters or firehose false positives. Statistics per step (`n`, `Λ`, current
`p₀`, thresholds) are audit-logged, so every rollback decision is auditable
*and reproducible* from the log alone.

### 7.4 Condition-system integration

All splice decisions travel as conditions in a small hierarchy:

```
splice-condition
├── patch-vetoed            (contract veto; carries contract + detail)
├── canary-anomaly          (single failure observation; carries task, route, cause)
├── canary-verdict          (SPRT boundary crossing; carries n, Λ, direction)
├── migration-failure       (hard trigger)
├── rollback-initiated      (carries trigger, full decision context)
└── rollback-failure        (⇒ FROZEN)
```

with restarts `splice:fallback-to-old` (per-task, §7.1), `splice:freeze`,
`splice:force-promote`, and `splice:abort-patch` available wherever the
corresponding transition is legal. Operators at a REPL attached to the image
interact with a live incident through standard restart machinery — the CL-only
capability the brief calls out, made load-bearing.

---

## 8. Provenance and audit log

### 8.1 Requirements

The log alone must reconstruct: what code ran when, why every transition
happened, and what every rollback restored. "Alone" means: given the log and
the original image, an offline tool re-derives the image's current code state
and verifies it against the live image.

### 8.2 Format

Append-only file of readable Lisp forms (one plist per line — greppable,
`read`-able, no parser dependency), fsync'd on state transitions:

```lisp
(:seq 41 :ts "2026-07-10T14:02:11.302Z" :prev-hash "9f2c…"
 :image (:boot-id "b1e2…" :lisp "SBCL 2.4.x" :host "node-a")
 :event :canary-transition
 :patch-set (:name fix-currency-representation :hash "77aa…")
 :from :canary-5 :to :canary-25
 :sprt (:stream :errors :n 812 :llr -3.12 :a 4.55 :b -2.94
        :p0 0.004 :p1 0.024 :alpha 0.01 :beta 0.05 :verdict :accept))
```

Entry kinds: `patch-registered` (full source forms + SHA-256 of each compiled
function's source text + captured old-world identity hashes),
`contract-result`, `install`, `canary-transition`, `task-sample` (sampled, not
every task, at a logged sampling rate), `instance-migration` (§5.2/§5.4),
`rollback-decision` (the complete SPRT state or hard-trigger condition report
that caused it), `rollback-complete` (restoration verification results),
`promotion`, `freeze`/`unfreeze` (with operator identity), `journal-expired`.

### 8.3 Integrity and reconstruction

- **Hash chain:** every entry carries SHA-256 of the previous entry
  (`:prev-hash`); truncation or tampering breaks the chain. (Tamper-*evidence*,
  not tamper-*proofing* — an attacker with image access owns everything; the
  threat model is operational honesty, not adversaries.)
- **Reconstruction tool** (Phase-2 deliverable): folds over the log to compute
  the expected current fdefinition/class state (as source hashes), then
  compares against the live image. Success criterion §10.5 tests exactly this.
- The exact field-by-field schema will be pinned in
  `artifacts/audit-log-schema.md` during Phase 2, per `artifacts/SCHEMA.md`.

---

## 9. Distributed propagation sketch

Design-only in Phase 1; prototyped against 2+ images in Phase 2 only if time
allows. Single-image completeness does not depend on any of this.

### 9.1 Topology and roles

A minimal **conductor** process (itself an SBCL image running splice) and N
member images. No consensus protocol — the conductor is the single decision
point, and the failure story is designed around that honestly rather than
pretending to Paxos.

### 9.2 Propagation: two-phase, reusing the local lifecycle

1. **Prepare:** conductor ships the serialized patch set (source forms +
   metadata + expected hashes) to each member. Each member independently
   compiles, runs the full contract-check gate, and reports
   `STAGED`/`VETOED`. Any veto ⇒ cluster-wide abort (a set that fails
   contracts on one image is not "mostly fine").
2. **Commit:** conductor issues epoch-numbered stage commands
   (`(:advance :canary-5 :epoch 7)`). Members flip their local atomic epoch
   cell (§4.3) — cluster rollout is a ramp of ramps: first one member runs
   the full local ramp to `CANARY-100`, then the fleet advances in cohorts.

### 9.3 Cluster-wide SPRT aggregation

The log-likelihood ratio is a sum over observations, so **per-node LLRs add**:
each member periodically reports `(n, Λ, p₀-window)` for the current stage;
the conductor's cluster verdict uses `Λ_cluster = Σ Λ_node` against the same
(A, B) thresholds — a pooled SPRT with the same error bounds, under the
assumption of homogeneous outcome distributions across nodes. Heterogeneity
(one sick node, healthy fleet) is caught by *also* running each node's local
SPRT: **either** a local reject on any node **or** a cluster reject triggers
cluster-wide rollback. Node-local hard triggers (A2) roll back locally first
and notify — safety never waits on the network.

### 9.4 Partition behavior

A member that loses the conductor: freezes its ramp at the current stage,
keeps its local SPRT running with the conservative thresholds, auto-rolls-back
on local reject, and never advances on its own. A conductor that loses a
member: excludes it from aggregation, and will not advance the cluster past
the current stage until the member is reachable or an operator excludes it
(logged). Rejoin: member reports its full audit-log tail since divergence;
conductor reconciles (the hash-chained log makes divergence detection exact)
and either fast-forwards the member or rolls it back to match the cluster
verdict.

### 9.5 Cluster audit

Each node keeps its own hash-chained log; the conductor's log records every
command issued and every report received, with per-node sequence numbers, so
the cluster history is the union of node logs ordered by
(node, seq) plus the conductor's Lamport-style command ordering. The
reconstruction tool from §8.3 extends naturally: reconstruct each node, then
verify every node converged to the cluster verdict.

---

## 10. Success criteria

The Phase-2 prototype passes when all of the following hold, demonstrated by
the synthetic workload harness (which reuses/extends smolder's
failure-injection machinery for the fault patterns) and recorded in committed
artifacts:

1. **The flagship scenario.** A patch set touching **3 interdependent
   functions** (`price-order` → `apply-discount` → `total-invoice`, sharing a
   changed internal money representation) **plus a class redefinition**
   (`order` gains `amount-minor-units`, drops `float-total`) is:
   - installed, contract-checked, and ramped 5 → 25 under live synthetic load;
   - injected with a realistic fault (see 2) mid-ramp;
   - **caught by the SPRT and fully reverted**, including reverse migration of
     every already-migrated `order` instance, **with a complete audit trail**.

2. **Realistic fault patterns, not uniform coin flips.** The harness injects,
   in separate runs, at least: (a) *latent input-dependent* failure (fails
   only for a p-fraction input subclass); (b) *load-correlated* failure
   (emerges only above a concurrency threshold — must be caught at the 25 %
   stage, not the 5 % stage, demonstrating why staged ramps exist);
   (c) *slow-burn latency regression* (no errors; caught by the latency
   SPRT); (d) *delayed-onset* failure (healthy for N calls, then degrades —
   resource-leak shape); (e) *migration-time* failure (the forward migration
   function itself errors on rare instance states ⇒ hard trigger A2).

3. **Coherence, verified not asserted.** Across every run, an offline check
   over the harness's execution records proves **zero tasks executed a
   mixture** of old and new implementations from one patch set. The
   deterministic routing draw (§6.1) makes this reproducible from the audit
   log.

4. **Rollback exactness.** After the flagship rollback: every patched
   function's `fdefinition` is `eq` to the captured original; every surviving
   migrated instance's slots match its journal record exactly
   (canary-born instances flagged `synthesized-reversal` are enumerated);
   re-running the workload post-rollback produces results identical to a
   never-patched control image.

5. **Audit reconstruction.** The reconstruction tool, given only the log,
   derives the image's expected code state at 3+ checkpoints (pre-patch,
   mid-canary, post-rollback) and matches the live image at each; breaking
   the hash chain at any entry is detected.

6. **Statistical honesty.** Over ≥ 100 harness runs with a *clean* patch,
   the spurious-rollback rate is consistent with the configured α (0.01) —
   binomial 95 % CI containing α, not just "it seemed fine." Over ≥ 100 runs
   with each injected fault, detection rate ≥ 1 − β and median
   detection-latency (observations to boundary crossing) is reported per
   fault class.

7. **Overhead bounds.** Measured shim overhead during canary < 2× plain
   funcall on the hot path; post-promotion overhead exactly zero (shims
   gone); numbers in the final report.

8. **(Stretch)** Two SBCL images + conductor execute the flagship scenario
   with the fault injected on one member only: local SPRT rejects, cluster
   rolls back both members, both audit logs reconcile.

---

## 11. Known limitations and open questions

### 11.1 Call paths that bypass fdefinition indirection

SBCL compiles some calls so they do not go through the fdefinition lookup at
call time: `declaim inline` + inlined callers, local functions
(`flet`/`labels`), and block-compiled units. Patched functions with such
callers would leave stale fast paths. **Mitigation:** the registrar's static
checks (§4.5) detect proclaimed-inline functions and known inlined call sites
and veto, with the suggested remedy of adding the affected callers to the set
(recompiled against the new definitions). Residual risk: callers compiled in
files whose inlining decisions aren't introspectable — documented as an
operational rule ("don't inline what you may need to patch"), the same rule
kernel livepatching imposes on hot-patchable symbols.

### 11.2 Class canary is shape-global

The union-shape approach (§5.1) means slot *storage* changes are visible to
all tasks even at 5 % ramp; only *behavior* canaries. A truly per-task class
shape would require parallel class hierarchies and identity-splitting
(Smalltalk `become:` territory) — rejected as disproportionate; the union
shape plus write-through correspondence keeps old code correct, which is what
actually matters.

### 11.3 Non-task threads

Work outside `with-task` scope never canaries and only adopts patches at
promotion (§4.2). For a system whose main loops are long-lived threads rather
than request-shaped work, splice's guarantees weaken to Erlang-style "next
external call" semantics — flagged at promotion (H5). Phase 2's harness is
request-shaped by construction; the limitation is documented, not solved.

### 11.4 SPRT modeling assumptions

Outcomes are treated as iid Bernoulli within a stage; real traffic has
autocorrelation (bursts). The control-arm baseline (§6.2) absorbs shared
bursts, and the minimum-sample floor blunts early noise, but a formally tight
error bound under dependence is out of scope — the α/β guarantees should be
read as engineering targets validated empirically (criterion §10.6), which is
also how industry canary systems treat them.

### 11.5 Open for review

- Should paired fallback (A1 retry-on-old) be default-on or opt-in per patch
  set? Default-on masks failures from end users (good) but changes side-effect
  semantics for non-idempotent tasks (dangerous). Current lean: **opt-in**,
  with the harness demoing both.
- Journal retention window after promotion (§4.4): time-based, GC-pressure
  based, or explicit operator ack? Current lean: time-based default with
  explicit `splice:seal-patch` to end it early.
- Whether method-combination-affecting changes (`:around` additions etc.)
  belong in v1 method patching or are vetoed initially. Current lean: veto in
  v1, plain primary-method replacement only.

---

## Sources

- Arnold & Kaashoek, *Ksplice: Automatic Rebootless Kernel Updates*, EuroSys 2009 — [PDF](https://pdos.csail.mit.edu/papers/ksplice:eurosys.pdf), [ACM DL](https://dl.acm.org/doi/10.1145/1519065.1519085)
- Linux kernel livepatch consistency model — [docs.kernel.org/livepatch](https://docs.kernel.org/livepatch/livepatch.html)
- kpatch — [LWN: dynamic kernel patching](https://lwn.net/Articles/597123/); kGraft — [SUSE SLES kGraft guide](https://documentation.suse.com/sles/12-SP5/html/SLES-all/cha-kgraft.html)
- Erlang/OTP code replacement — [Compilation and Code Loading](https://www.erlang.org/doc/system/code_loading.html), [`code` module](https://www.erlang.org/doc/apps/kernel/code.html)
- Java HotSwap limits & JRebel — [JRebel HotSwap guide](https://www.jrebel.com/blog/java-hotswap-guide), [JRebel class reloading](https://www.jrebel.com/blog/how-to-reload-java-classes-at-runtime), [HotswapAgent](https://github.com/HotswapProjects/HotswapAgent)
- Smalltalk/Pharo class redefinition & instance migration — [Pharo class redefinition overview](https://copyprogramming.com/howto/programmatically-create-and-delete-classes-in-pharo-smalltalk)
- Wald, *Sequential Tests of Statistical Hypotheses* (1945) — [Springer introduction](https://link.springer.com/chapter/10.1007/978-1-4612-0919-5_17); [SPRT overview](https://en.wikipedia.org/wiki/Sequential_probability_ratio_test)
- CLHS, `update-instance-for-redefined-class` — [LispWorks HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Body/f_upda_1.htm), [§4.3.6 Redefining Classes](http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_4-3-6.html)
