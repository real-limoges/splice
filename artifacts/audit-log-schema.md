# Audit log schema

The provenance log is an append-only file of readably-printed Lisp property
lists, one per line, written by `splice:audit` (src/audit.lisp). It is
`read`-able from any Lisp with zero dependencies, greppable from a shell, and
hash-chained for tamper evidence. `artifacts/demo-audit.log` is a real log
produced by `demo.lisp`.

## Envelope (every entry)

| field | meaning |
|---|---|
| `:seq` | monotonically increasing entry number, starts at 1 |
| `:ts` | wall-clock timestamp `YYYY-MM-DDTHH:MM:SS.mmm` (local) |
| `:prev-hash` | SHA-256 (hex) of the previous entry's full line text; `"genesis"` for the first entry. Truncation, edits, or reordering break the chain — see `verify-audit-chain`. |
| `:boot-id` | first 12 hex digits of this image incarnation's identity (hash of boot time + machine), so logs from different images/restarts can't be silently interleaved |
| `:event` | entry kind (below) |

Values that cannot be printed readably (instances, functions, conditions)
are sanitized: conditions become `(:condition <type> <printed>)`, everything
else becomes `(:opaque <hash16>)` (`sanitize-for-log`).

## Entry kinds

### `:patch-registered`
Full identity of the set at registration, before anything is installed.
Fields: `:patch-set` (name), `:description`, `:ticket`, `:ramp`,
`:paired-fallback`, `:functions` (per function: `:name`,
`:new-source-hash` — SHA-256 of the printed new source, `:old-identity-hash`
— hash of the captured original's printed identity, `:new-source` — the
source form itself), `:methods` (`:gf`, `:specializers`,
`:new-source-hash`), `:classes` (names), `:class-details` (per class:
`:add-slots`, `:drop-slots`, `:lossy`).

### `:contract-result`
One per contract per gate. Fields: `:patch-set`, `:contract` (name; static
checks use `:static-arity`, `:static-inlining`, `:static-missing`,
`:static-overlap`), `:phase` (`:pre-flight` or `:runtime`), `:verdict`
(`:pass` / `:fail` / `:veto`), `:detail` (failure text, if any).

### `:class-shape`
A class changed shape. Fields: `:class`, `:shape` (`:union` at canary start,
`:new` at promotion, `:original` at rollback), `:patch-set`, plus
`:added`/`:retained-drops` (union), `:dropped` (promotion), or
`:reversal-counts` (rollback — see `:rollback-complete`).

### `:install`
Shims installed dormant; the atomic flip to the first stage follows as a
`:canary-transition`. Fields: `:patch-set`, `:shimmed-functions`,
`:shimmed-methods`, `:union-classes`.

### `:canary-transition`
Every state-machine edge. Fields: `:patch-set`, `:from`, `:to`, and for
stage entries `:fraction` plus `:sprt` — the *complete* SPRT state
(`:stream :n :llr :a :b :p0 :p1 :alpha :beta`), so every gating decision is
reproducible from the log alone.

### `:task-sample`
Every Nth task's routing pins (default N=50). Fields: `:task` (id),
`:pins` (per active set: `:patch-set`, `:route`, `:fraction`, `:state`).
Because the routing draw is deterministic —
`sha256(task-id "|" set-name) mod 100 < fraction` — an offline checker can
recompute every sampled decision (criteria C3 does).

### `:canary-anomaly`
An error escaped :new-routed code (recorded once per task per set).
Fields: `:patch-set`, `:task`, `:cause` (printed), `:cause-type`.

### `:instance-migration`
One per instance per direction. Fields: `:class`, `:direction`
(`:forward` / `:backward` / `:finalize`), and:
- forward: `:journal-seq`, `:pre-state` (plist of pre-migration slot
  values — this is the streamed copy of the journal record), or
  `:outcome :failed` + `:cause`
- backward: `:mode` — `:journal-exact` (restored from the journal),
  `:synthesized-reversal` (canary-born via new code; author's backward
  migration synthesized old-shape values), or `:old-born-unchanged`
  (canary-born via old code; old slots were already canonical)

### `:rollback-decision`
Why the rollback fired, with full decision context. Fields: `:patch-set`,
`:trigger` — one of `(:sprt-reject <sprt-snapshot>)`,
`(:hard-trigger <kind> <detail>)`, `(:runtime-contract <name>)`,
`(:operator <who>)` — plus `:pinned-new`/`:pinned-old` in-flight counts at
decision time.

### `:rollback-complete`
Fields: `:patch-set`, `:restoration-verified` (every fdefinition checked EQ
against the captured original before declaring success), `:reversal` (per
class: `:journal-exact` / `:synthesized` / `:old-born-unchanged` / `:failed`
counts).

### `:rollback-failed`
The safety net itself failed; the set is FROZEN (H3). Fields: `:patch-set`,
`:cause`.

### `:promotion` / `:promotion-quiescence` / `:force-promote`
Promotion completed (fields: `:functions` with installed source hashes,
`:journal-retained`); quiescence-timeout note listing still-pinned old task
count (H5); operator-forced promotion with `:operator`.

### `:freeze` / `:unfreeze`
Human-in-the-loop boundary. Freeze carries `:reason` (e.g.
`(:stage-timeout ...)` with both SPRT snapshots, `(:confounded
:control-rate r)`, `(:lossy-reversal-needs-human ...)`,
`(:rollback-failure ...)`); unfreeze carries `:operator`, `:decision`
(`:resume`/`:rollback`/`:promote`), and `:was-frozen-for`.

### `:sealed` / `:journal-expired`
Journal released (rollback no longer offered): `:operator`, `:released`
count.

### `:coherence-violation`
Should never appear. A shim observed a route different from the task's pin.
Fields: `:patch-set`, `:task`, `:pinned`, `:observed`.

## Reconstruction

`reconstruct-state` folds a log into the image's expected state — per
function `:original` / `:shimmed` / `(:new <set>)`, per class `:original` /
`:union` / `:new`, per set its lifecycle state — and refuses to run if the
hash chain is broken. `verify-reconstruction` compares that against the live
image (`EQ` on function objects, slot-name sets on classes). Criterion §10.5
exercises both plus tamper detection.

## Deviations from DESIGN.md §8.2

- Entries carry `:boot-id` but not the full image block (`:lisp`, `:host`)
  on every line; image identity is constant per log file and recoverable
  from the boot id. (Space; trivially added.)
- `:task-sample` was added beyond the design's entry list so that the
  zero-mixed-executions property is checkable offline (criterion §10.3).
- fsync is `finish-output` per entry; OS-level fsync is left to deployment.
