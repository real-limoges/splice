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
