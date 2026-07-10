# Provenance

Filled in as Phase 1 proceeded (2026-07-10). All sources below were verified
via live web search during the Phase 1 session, not recalled from memory.

## Prior art sources cited in DESIGN.md

- Erlang/OTP hot code loading — https://www.erlang.org/doc/system/code_loading.html
  (current/old two-version model, purge kills lingering processes) and
  https://www.erlang.org/doc/apps/kernel/code.html (soft vs. hard purge).
- Java HotSwap / JRebel — https://www.jrebel.com/blog/java-hotswap-guide
  (method-body-only constraint, debug-mode restriction),
  https://www.jrebel.com/blog/how-to-reload-java-classes-at-runtime
  (classloader/bytecode indirection layer),
  https://github.com/HotswapProjects/HotswapAgent.
- Smalltalk image-mutation practice —
  https://copyprogramming.com/howto/programmatically-create-and-delete-classes-in-pharo-smalltalk
  (Pharo class redefinition incl. instance-variable and superclass changes).
  Weakest citation of the set; the ClassBuilder/`become:` characterization in
  DESIGN.md §2.3 additionally rests on general Squeak/Pharo lineage knowledge
  and should be upgraded to a primary Pharo-books citation during Phase 2 if
  the section is leaned on.
- ksplice / kpatch (kernel live patching) —
  Arnold & Kaashoek, EuroSys 2009: https://pdos.csail.mit.edu/papers/ksplice:eurosys.pdf
  and https://dl.acm.org/doi/10.1145/1519065.1519085 (object-code layer,
  stop_machine + activeness stack check, shadow data structures, 88%-of-CVEs
  result); kpatch: https://lwn.net/Articles/597123/; kGraft per-task lazy
  migration: https://documentation.suse.com/sles/12-SP5/html/SLES-all/cha-kgraft.html;
  upstream hybrid consistency model (kGraft per-task + kpatch stack check):
  https://docs.kernel.org/livepatch/livepatch.html.
- Wald SPRT — Wald 1945, intro at
  https://link.springer.com/chapter/10.1007/978-1-4612-0919-5_17; threshold
  form A = ln((1−β)/α), B = ln(β/(1−α)) and Wald–Wolfowitz optimality:
  https://en.wikipedia.org/wiki/Sequential_probability_ratio_test.
- CLHS `update-instance-for-redefined-class` / `make-instances-obsolete` —
  https://www.lispworks.com/documentation/HyperSpec/Body/f_upda_1.htm and
  §4.3.6 http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_4-3-6.html
  (two-step update protocol, discarded-slot property list).

## Design decisions and their justification

- **Shim indirection instead of fdefinition swap during canary** — both
  worlds stay valid simultaneously, so no stop-the-world and no stack
  inspection is needed; SBCL exposes no safe userland stop-the-world anyway.
- **Per-task epoch pinning for coherence** — directly adopted from the
  livepatch/kGraft per-task consistency model; we own the task boundary
  (`with-task`), so the switch point is free rather than inferred from stacks.
- **Atomic commit as a single CAS on the patch-set epoch cell** — reduces
  multi-symbol atomicity (ksplice's stop_machine problem) to one word, which
  the shim indirection makes sufficient.
- **One in-flight canary per function; overlapping sets queued/vetoed** —
  generalizes Erlang's two-version cap; stacking canaries makes the SPRT's
  attribution meaningless.
- **Union-shape class redefinition during canary** (old slots ∪ new slots,
  write-through correspondence) — instance layout can't canary per-task, so
  old code must stay correct under the new shape; CLOS analogue of ksplice's
  shadow data structures.
- **Weak-key migration journal + streamed audit copy** — makes rollback of
  forward-migrated instances exact rather than best-effort; weak keys tie
  journal lifetime to instance lifetime (dead instances need no reversal).
- **Canary-born instances get `:migrate-backward` synthesis, flagged
  `synthesized-reversal`** — they have no pre-migration state to restore, so
  exactness is impossible by construction; flagging keeps the audit honest.
- **SPRT per stage, statistics reset between stages** — a lucky 5% stage must
  not subsidize a bad 25% stage; load-correlated faults first appear at higher
  ramp fractions (harness fault pattern (b) exists to prove this matters).
- **Contemporaneous control arm for p₀, not a fixed constant** — ambient
  outages hit both arms and classify as `confounded` (human path) instead of
  triggering a superstitious rollback.
- **Hard triggers (migration failure, storage-condition, runtime contract
  failure) bypass the SPRT** — structural breakage is not a rate question.
- **Deterministic per-task routing draw (hash of task-id × set-id)** — makes
  routing reproducible from the audit log and the zero-mixed-execution
  property checkable offline.
- **Readable-plist hash-chained log lines, not JSON** — `read`-able with zero
  dependencies from any Lisp; hash chain gives tamper-evidence (threat model
  is operational honesty, not adversaries with image access).
- **Conductor-based cluster design, no consensus protocol** — honest about
  the single decision point; per-node LLRs sum, so pooled SPRT is exact under
  homogeneity, and local per-node SPRTs catch the one-sick-node case.
- **Paired fallback (retry failed :new call on :old) leans opt-in** —
  masking failures is good for users but changes side-effect semantics for
  non-idempotent work; flagged as open question §11.5 for review.
- **v1 method patching = primary-method replacement only** — method-
  combination-affecting changes vetoed initially; open question §11.5.

## Phase 2 build notes

Deviations from the reviewed DESIGN.md, and why (details in NOTES.md):

- **Contract ordering (§4.5 vs §5)**: contracts exercise the new functions,
  and new code reads the added class slots — so the union class shape is
  applied *before* the contract gate, not after. A contract veto reverts the
  shape through the same reverse-migration machinery. (Discovered
  empirically: the original ordering vetoed every class-touching patch.)
- **Journal keys (§5.3)**: strong keys + explicit lifecycle purging
  (rollback / promotion / seal) instead of weak keys. SBCL 2.2.9's weak
  tables corrupt ("Corrupt NEXT-chain") under GC pressure with concurrent
  access even when every access is mutex-guarded. The design's weak-key
  intent (entries die with instances) is approximated by purging at the
  same lifecycle points the design already defines.
- **`:task-sample` audit entries (§8.2)**: added beyond the design's entry
  list so criterion §10.3 (zero mixed executions) is verifiable offline
  from the log, not just asserted in-process.
- **Fixed-arity shim factories (§4.2)**: the generic `&rest` shim measured
  exactly 2.0x a plain call on a trivial function, at the §10.7 boundary;
  per-arity compiled shims (one assoc doing both route lookup and coherence
  marking) measure 1.33x. Variadic functions still get the `&rest` shim.
- **Cluster prototype (§9)**: prepare runs compile + static checks; the
  full contract gate runs at commit (inside INSTALL). Members ramp
  autonomously — the cohort "ramp of ramps" and partition handling are
  designed but not built. Pooled SPRT (LLR summation), two-phase
  prepare/commit with hash-verified source shipping, local-reject
  escalation, and cluster-wide rollback are built and demonstrated.
- **`update-instance-for-redefined-class` author signature (§4.1)**:
  migration functions receive `(instance values-plist)` — forward gets the
  journal snapshot of old-shape slots (under the union shape nothing is
  discarded yet, so the CLHS discarded-plist would be empty), backward gets
  the discarded new-slot values per CLHS.
- **Fault injection**: implemented natively (same pattern taxonomy as the
  design); smolder's machinery was unreachable from this session.

Measured outcomes backing the §10 criteria are in
artifacts/criteria-report.md (generated, committed per run).
