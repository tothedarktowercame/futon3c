**Status:** INSTANTIATE complete (2026-04-29) for three sibling artifact-classes; deferred siblings recorded in scope-out.
**Home-repo:** futon3c
**Family:** `archaeology-control` (structural-law-inventory.sexp:200)
**Sibling:** [M-invariant-queue-extend](M-invariant-queue-extend.md) — same apparatus (probe / boundary / canary)
**Origin:** Joe + claude-11 conversation 2026-04-29 unpacking the rank-1 candidate `obsolescence-recognition`. See claude-repl session log for derivation.

# M-archaeology-control: Obsolescence-recognition as a namespace of sibling invariants

## 1. IDENTIFY

### Motivation

The inventory's rank-1 candidate is `obsolescence-recognition` —
"Superseded autostashes do not remain in the live queue once equivalent
commits have landed." Family `archaeology-control` ("Latent work should
not accumulate as invisible operational debt"). Status: candidate;
`:best-current-exemplar none-yet`; harvest hits 50; priority/score 550.

The narrow stash form is one slice of a broader shape. Each kind of
latent-work artifact has its own subsumption test against a stronger
record (commit, merged work, passing test, real check-fn, etc.).
**Obsolescence-recognition is a shape; each artifact-class is a sibling
invariant under the same family, distinguished by namespace prefix.**

The current state across the futon stack: zero stashes anywhere right
now. So the narrow form has nothing to fire on today. But the *shape*
recurs in artifacts the apparatus we just built creates and tracks
(deferred-stubs, pipeline-tracers), so the family has live signal even
when stashes don't.

### Theoretical anchoring

- **Subsumption shape.** Artifact A is obsolete relative to record P
  when A's information content is subsumed by P, and A occupies a
  parallel/transient position while P is in the canonical/durable
  position. The check is per-artifact-class; the shape is universal.
- **Namespace as discrimination.** Sibling invariants under one family
  share the family concept but differ by artifact-class via namespace
  (`obsolescence-recognition/autostash`, `.../deferred-stub`,
  `.../pipeline-tracer`, etc.). No new structural element ("subfamily")
  required — the inventory's `family → invariants` shape already supports
  this.
- **Multi-mode binding (M-invariant-queue-unstuck precedent).** Probe +
  boot check + pre-commit hook is the multi-mode pattern that gives
  computational guarantee. Strongest mode (pre-commit refusal) for the
  artifact classes where blocking is safe; probe-only for the rest.
- **Detection vs cleanup separation.** Probes are read-only (detection).
  Cleanup is destructive and stays operator-driven (e.g. `futon-sync.clj
  park --yes`). Conflating them creates "auto-cleanup race conditions"
  Joe explicitly wants to avoid.

### Scope in

- **Three sibling invariants** under `archaeology-control`, all sharing
  the check-fn shape `(fn [_] {:outcome :ok | :violation :detail
  {:obsolete-artifacts [...]}})`:
  1. **`obsolescence-recognition/autostash`** — refinement of the
     existing narrow form. Subsumption test: `git stash show -p N |
     git apply --reverse --check` clean against HEAD; or tree-hash of
     stash matches a tree reachable from HEAD.
  2. **`obsolescence-recognition/deferred-stub`** — fires when a
     `register-deferred-taps!` registration persists alongside a real
     `register-family-check!` for the same family-id. Subsumption
     test: `(get @futon3c.logic.probe/family-check-fns family-id)`
     returns a non-deferred fn. *Dogfood: M-invariant-queue-extend
     Track 1+2 lifts will trigger this.*
  3. **`obsolescence-recognition/pipeline-tracer`** — fires when an
     open `:pipeline-tracer-item` evidence entry should have been
     closed. Subsumption test: per-track-id heuristic (e.g. Track 4.3
     closes when arxana view columns are merged). For now, manual
     close via `tracer/emit-tracer-closed!`.
- **Probe taps for all three** in `futon3c.logic.probe-taps`, registered
  via a new `register-archaeology-control-taps!` function.
- **Pre-commit hook for the autostash slice** — strongest mode; refuses
  commit if obsolete autostashes detected. Mirrors the
  `check-coverage-ratchet.sh` pattern.
- **Inventory updates**: rename existing `:invariant id obsolescence-
  recognition` to `:obsolescence-recognition/autostash` (with a
  ratchet-recorded migration if the rename counts as a status change);
  add stub entries for the two new siblings as `:status :candidate`.
- **Tests** for each of the three check-fns.

### Scope out

- **Auto-running futon-sync.clj**. Stays manual. The invariant says
  *when* cleanup is needed; Joe (or a separate cleanup mission)
  decides when to run.
- **`obsolescence-recognition/branch-merged`** — covered by `gh pr
  list --merged` + `git for-each-ref`, but each repo needs its own
  config. Defer to a separate sub-mission.
- **`obsolescence-recognition/test-skip`** — requires a test runner
  hook to detect now-passing skips. Defer.
- **`obsolescence-recognition/conditional-todo`** — requires parsing
  natural-language `remove once X` clauses. Defer.
- **`obsolescence-recognition/dead-shim`** — usage-grep over the
  codebase. Defer.

### Completion criteria

1. Three sibling invariants registered in
   `docs/structural-law-inventory.sexp` under family
   `archaeology-control` with namespace IDs.
2. Three real check-fns registered as probe taps; the
   `register-deferred-taps!` mechanism updated to skip families that
   already have real check-fns (so Track 4.1's family-canary live
   binding doesn't false-positive against this).
3. Probe-tap tests covering: zero artifacts → `:ok`; one obsolete
   artifact → `:violation` with detail; multiple → `:violation` with
   list.
4. Pre-commit hook for the autostash slice; documented activation
   command.
5. The HUD widget renders the three new family-fired entries (no extra
   work — same `:family-fired` shape).

### Relationship to other missions

- **Sibling:** M-invariant-queue-extend (apparatus); this mission is
  a concrete write-class generalization that fits Track 3's
  template alongside bell receipts / gate traversals.
- **Self-test:** Track 4.1 (coverage-ratchet load-time wiring) just
  landed; the deferred-stub for it (if any was registered) would now
  be obsolete. The
  `obsolescence-recognition/deferred-stub` invariant is built to
  detect exactly this.
- **Tools-not-replaced:** `futon0/scripts/futon-sync.clj` keeps its
  role as the operator-driven cleanup. The probe + pre-commit hook
  give the *guarantee*; futon-sync gives the *cleanup*.

### Source material

- `futon3c/docs/structural-law-inventory.sexp:200-209` — current
  family + narrow invariant.
- `futon0/scripts/futon-sync.clj` — manual cleanup for stashes/dirty
  trees; reference for what "cleanup" looks like.
- `futon3c/src/futon3c/logic/probe.clj`,
  `futon3c/src/futon3c/logic/probe_taps.clj` — apparatus to extend.
- `futon3c/src/futon3c/logic/ratchet.clj` and
  `futon3c/scripts/check-coverage-ratchet.sh` — pre-commit pattern
  to mirror.
- `futon5a/data/stack-stereolithography-priority-queue.json` —
  priority source.

### Owner and dependencies

- **Owner:** Joe (architectural authority on what counts as obsolete) +
  claude (probe-tap implementation, test scaffolding, mission-doc
  maintenance).
- **Primary repo:** futon3c (apparatus extension).
- **Touches:** futon0 (futon-sync reference), all repos (live stash
  state).

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

(Pending Joe ratification of the IDENTIFY phase.)

## Checkpoints

### 2026-04-29 — INSTANTIATE complete: three sibling check-fns landed

**Concrete deliverables**

- `src/futon3c/logic/archaeology.clj` (~280 lines) — three check-fn
  factories under namespace `obsolescence-recognition/<artifact-class>`:
  - `check-autostash-obsolescence repo-paths` — `git stash show -p N |
    git apply --reverse --check` per stash. Real subsumption.
  - `check-deferred-stub-obsolescence inventory-path` — walks
    `family-check-fns`; cross-references inventory `:status
    :operational`. Inventory-as-canonical-record proxy.
  - `check-pipeline-tracer-obsolescence` — open vs closed
    `:pipeline-tracer-item` evidence by tag query; flags closed-or-
    past-target tracers.
  - Plus `register-archaeology-control-taps!` convenience wiring
    mirroring `register-default-taps!`.
- `test/futon3c/logic/archaeology_test.clj` — 12 deftests / 21
  assertions / 0 failures.
- `docs/structural-law-inventory.sexp` lines 200-209: renamed existing
  `:obsolescence-recognition` → `:obsolescence-recognition/autostash`;
  added `:obsolescence-recognition/deferred-stub` +
  `:obsolescence-recognition/pipeline-tracer` siblings. All three
  `:status :operational-when-enabled` with implementation /
  enforcement / evidence triples.
- `scripts/check-autostash-obsolescence.sh` — pre-commit hook for the
  autostash slice (strong-mode binding); operator-installed via
  symlink. Mirrors `check-coverage-ratchet.sh`.
- PSR at `holes/labs/M-archaeology-control/psr/2026-04-29__derive__
  subsumption-witness-siblings.md` + PUR at the corresponding
  `pur/...` path.

**Live verification**

Loaded `futon3c.logic.archaeology` into running JVM via Drawbridge,
registered all three taps against the live evidence store. Results:

```
{:registered #{:obsolescence-recognition/autostash
               :obsolescence-recognition/deferred-stub
               :obsolescence-recognition/pipeline-tracer}
 :autostash {:outcome :ok :detail {:scanned-repos 1 :obsolete-count 0}}
 :deferred  {:outcome :ok :detail {:registered-count 3 :obsolete-count 0}}
 :tracer    {:outcome :ok :detail {:open-count 6 :closed-count 0 :obsolete-count 0}}}
```

The pipeline-tracer correctly sees the 6 open tracer items emitted
earlier this session. Zero obsolescent artifacts detected — clean
state.

**Test sweep**

- Mission suites (boundary + invariant + store + ratchet + probe +
  probe-taps + tracer + archaeology): **87 tests / 238 assertions / 0
  failures**.

**Net mission state — INSTANTIATE complete for first three siblings**

- ✅ `obsolescence-recognition/autostash` — probe-tap + pre-commit
  hook (operator-installed). Strong-mode binding available.
- ✅ `obsolescence-recognition/deferred-stub` — probe-tap; surfaces
  inventory ↔ live-registry mismatch. Dogfood-ready.
- ✅ `obsolescence-recognition/pipeline-tracer` — probe-tap; surfaces
  closed-or-past-due tracers. Operates against the tracer surface
  M-invariant-queue-extend just shipped.

**Activation steps for the operator** (all gated; nothing happens
automatically until you run them):

```clojure
;; In the running JVM, via Drawbridge or the dev REPL:
(require '[futon3c.logic.archaeology :as arch])
(arch/register-archaeology-control-taps!)
```

```bash
# Install the autostash pre-commit hook (per repo, optional):
cd ~/code/futon3c
ln -sf ../../scripts/check-autostash-obsolescence.sh \
       .git/hooks/pre-commit
```

**Status:** MAP / DERIVE / INSTANTIATE / DOCUMENT for the three
chosen artifact-classes complete. ARGUE / VERIFY skipped per the
M-invariant-queue-unstuck inversion precedent (well-trodden patterns;
PSR/PUR carries the argument; structural checks ran inline). Deferred
artifact-classes (branch-merged, test-skip, conditional-todo,
dead-shim) recorded in the IDENTIFY scope-out — handed off to
follow-on missions when their pressure surfaces.

### 2026-04-29 — mission opened

- IDENTIFY complete with three sibling invariants scoped under namespace
  `obsolescence-recognition/*`.
- Joe confirmed "namespace rather than subfamily" approach.
- Pending: Joe priority direction for which of the three lands first
  (autostash is most concrete; deferred-stub is most dogfood; pipeline-
  tracer is most apparatus-coherent).
- Status: IDENTIFY complete; ready for MAP / DERIVE on a chosen first
  invariant.
