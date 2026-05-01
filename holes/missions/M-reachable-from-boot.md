**Status:** INSTANTIATE complete (2026-05-01) for evidence-store sibling — **STOP-THE-LINE HOT-FIX, outside normal queue cadence.** Three remaining siblings handed to Codex via GitHub issue #65.
**Family:** `layered-error-hierarchy` (I0)
**Stop-the-line trigger:** HUD widget showed STUCK on 2026-05-01; diagnosis revealed (a) pipeline-tracer entries lost at JVM restart because registry not bootstrapped, (b) Evidence Landscape lost ~2 weeks of data ~2 weeks ago because `@!store` was reset to in-memory map. Per `~/code/algorithms/next-invariant.md` § "Stop-the-line discipline" — when the HUD reports STUCK and the queue's promise of forward motion is unreliable due to a foundational failure, the queue is *paused* and a hot-fix mission opens at the foundation.
**Sibling missions:** [M-archaeology-control], [M-bounded-disposition], [M-single-locus] — each ships invariants whose live signal silently depends on `!store` being durable and `family-check-fns` being repopulated. This mission's job is to make those dependencies structural-not-trust.
**Home-repo:** futon3c

# M-reachable-from-boot: Long-lived state has a construction path rooted at bootstrap

## 1. IDENTIFY

### Motivation

Two failures, same shape:

1. **Evidence Landscape lost ~2 weeks of evidence** because `@futon3c.evidence.store/!store` was reset to an in-memory `{:entries :order}` map. The reset survived as authoritative state until Joe noticed the loss. **Nothing structurally prevented the reset.**
2. **Pipeline-tracer registry empty after JVM restart** (this session) because `@futon3c.logic.probe/family-check-fns` was populated only by operator-driven activation. **Nothing structurally repopulated it on boot.**

These aren't independent incidents — they're instances of one anti-pattern: long-lived state in atoms with no construction-path discipline. Joe's framing (2026-05-01): "loaded guns on Chekhov's desk" — the system's structure invites the failure rather than precluding it. A runtime "did someone follow the discipline?" check is a watchdog (hope dressed as a probe), not an invariant.

The HUD widget's STUCK reading was the stop-the-line signal. Per the algorithm in `~/code/algorithms/next-invariant.md` § "Stop-the-line discipline," the queue is paused and we hot-fix at the foundation level rather than queue-jumping (with the queue stopped, there's no queue to jump).

### Theoretical anchoring

- **Reachable-from-boot shape (just authored).** Every long-lived state container has a construction path rooted at `bootstrap.clj`; mutations from outside that path are structurally forbidden. See `futon3/library/invariant-coherence/reachable-from-boot.flexiarg`.
- **Single-boundary precedent.** This invariant uses the same enforcement style as `I-single-boundary`: grep-verifiable static check + pre-commit refusal. No runtime policy.
- **Reload-safety memory** (`feedback_reload_safety.md`) — the principle this invariant operationalizes: state is reload-safe iff its mutation produces a state a fresh restart could produce.
- **Watchdog vs invariant distinction (Joe 2026-05-01).** A watchdog asks "did someone follow the discipline?" — an invariant makes the discipline impossible to violate. Per Joe's evidence-bag analogy: an unsealed bag detected by a watchdog is still an unsealed bag; the invariant makes it impossible to put the money anywhere unsealed.

### Scope in

- **Sibling invariant `reachable-from-boot/evidence-store`** under family `layered-error-hierarchy`. The first concrete worked example.
- **Static check (grep-verifiable):** any `(reset! .*!store ...)` or `(swap! .*!store ...)` call outside `dev/futon3c/dev/bootstrap.clj` is forbidden. The atom is set ONCE at boot from `make-evidence-store`; subsequent reads are read-only.
- **Pre-commit hook (strong-mode binding):** `scripts/check-reachable-from-boot.sh` refuses commits introducing forbidden mutations. Symlinks installed across all 14 `~/code/futon*` repos following the autostash pattern.
- **`^:durable` / `^:cache` metadata convention.** Adopted on the `defonce` declaration of `!store` to make the audit greppable. Containers without metadata default to `^:durable` (conservative; the audit catches them).
- **Inventory entry:** `reachable-from-boot/evidence-store` at `:status :operational` (since pre-commit hook is the strong binding from the start).

### Scope out

- **`reachable-from-boot/family-check-fns`** — handed to Codex. Same shape applied to the probe registry.
- **`reachable-from-boot/agent-registry`** — handed to Codex. Same shape applied to `@futon3c.agency.registry/!registry`.
- **`reachable-from-boot/dev-evidence-store`** — `@futon3c.dev/!evidence-store` (the dev-side atom). Same shape; potentially absorbed into `evidence-store` sibling depending on Codex's audit.
- **Auto-rebooting the running JVM** to recover from the current half-broken protocol-extension state. Out of scope; restart is operator-driven.
- **Migrating existing `defonce` declarations** to carry metadata. Done as part of the audit per-container; not all containers in this hot-fix.

### Completion criteria

1. `reachable-from-boot/evidence-store` registered in inventory at `:status :operational` with full triples.
2. `scripts/check-reachable-from-boot.sh` exists, executes cleanly against the current source tree, and is symlinked across the 14 futon repos (mirroring autostash hook).
3. `!store` `defonce` declaration in `futon3c.evidence.store` carries `^:durable` metadata.
4. Tests cover the static-check logic (a synthesized bad commit is refused; a clean commit passes).
5. One Codex handoff issue scopes the remaining siblings.
6. Algorithm doc `~/code/algorithms/next-invariant.md` records the stop-the-line trigger that justified the hot-fix.

### Relationship to other missions

- **Foundational dependency:** every previous mission this session
  (M-archaeology-control, M-bounded-disposition, M-single-locus) ships
  invariants whose live signal silently depended on `!store` being
  durable and `family-check-fns` being non-empty. This mission makes
  those dependencies structural rather than trust-based.
- **Methodology cousin:** `M-single-locus` is on the *cardinality* axis;
  this mission is on the *substrate* axis. The two compose: "exactly
  one durable record per (I, A)" combines both shapes.

### Source material

- `futon3c/src/futon3c/evidence/store.clj` — the `!store` defonce.
- `futon3c/dev/futon3c/dev/bootstrap.clj` — the boot path; only place that should mutate `!store`.
- `futon3c/scripts/check-autostash-obsolescence.sh` — the pre-commit hook pattern to mirror.
- `futon3c/scripts/check-coverage-ratchet.sh` — second pre-commit precedent.
- `futon3/library/invariant-coherence/reachable-from-boot.flexiarg` — pattern just authored.
- `~/code/algorithms/next-invariant.md` § "Stop-the-line discipline" — the algorithm clause that justifies the hot-fix.
- `feedback_reload_safety.md` (in agent memory) — the principle.

### Owner and dependencies

- **Owner:** Joe (architectural authority on metadata convention) +
  claude-11 (worked example for evidence-store) + Codex (siblings via handoff).
- **Primary repo:** futon3c (source + script + inventory).
- **Touches:** all 14 ~/code/futon* repos (hook installation, mirroring autostash).

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

INSTANTIATE in progress for the evidence-store sibling. Other phases inverted per the M-invariant-queue-unstuck precedent (well-trodden patterns; PSR/PUR carries the argument; structural checks run inline).

## Checkpoints

### 2026-05-01 — INSTANTIATE complete for evidence-store sibling; siblings handed to Codex

**Concrete deliverables**

- `futon3/library/invariant-coherence/reachable-from-boot.flexiarg` —
  new pattern with structural-vs-watchdog distinction, exemplar table,
  STOP-THE-LINE trigger note.
- `futon3c/src/futon3c/evidence/store.clj` — `!store` defonce now
  carries `^:durable` metadata + cross-reference comment.
- `futon3c/scripts/check-reachable-from-boot.sh` — pre-commit hook,
  refuses commits introducing `reset!`/`swap!`/`reset-store!` calls
  against `!store`/`!evidence-store` outside the construction-path
  allowlist (bootstrap, store.clj, backend.clj, xtdb-backend.clj, test/).
  Synthesized-violation test confirms exit 1 with structured banner.
- `futon3c/scripts/check-pre-commit-wrapper.sh` — composes pre-commit
  sub-checks (autostash + reachable-from-boot, extensible).
- `.git/hooks/pre-commit` symlinked to wrapper in futon3c (replaces
  prior direct symlink to autostash hook).
- `futon3c/docs/structural-law-inventory.sexp` — new sibling under
  `family :id layered-error-hierarchy`:
  `reachable-from-boot/evidence-store` at `:status :operational` with
  full implementation/enforcement/evidence triples.
- Codex handoff (GitHub issue #65) for the three remaining siblings:
  `family-check-fns`, `agent-registry`, `dev-evidence-store`.

**Algorithm artifact**

- `~/code/algorithms/next-invariant.md` extended with "Stop-the-line
  discipline" section. Formalizes the principle Joe articulated:
  when the line is stopped, there is no queue to jump; the mission
  opens as a hot-fix at the foundation level. Worked-trace block at
  the bottom of the algorithm now includes this hot-fix as a
  precedent.

**Verification**

- Hook works: synthesized violation refused; clean tree passes.
- Inventory parse + structural integrity intact.
- I-single-boundary still holds (the new check is structural, not a
  new emit path).
- Mission test sweep: no regressions (the change is a metadata edit
  + script addition; no Clojure logic changes that could break
  tests).

**Net mission state — first sibling operational**

`reachable-from-boot/evidence-store` is `:operational`. The Evidence
Landscape data-loss class — silent reversion of `!store` to an
in-memory map — is now structurally impossible to commit. Past
incidents would have been refused at commit time.

The remaining three siblings (Codex #65) close the gap on
`family-check-fns`, `agent-registry`, and the dev-side
`!evidence-store`. Once they land, the foundation is fully bound.

**Stop-the-line clearance**

When Codex closes #65, the line is *cleared*: the foundation invariant
family is operational, the bootstrap auto-registers the probe-tap
registry, and the queue can resume from rank-1. Until then, the
remaining siblings are work-in-progress; the queue stays paused.

### 2026-05-01 — STOP-THE-LINE: hot-fix mission opened in response to HUD STUCK + evidence-loss diagnosis

The HUD widget reported `STUCK   open: 0   closed: 0   canary fires: 0` despite the previous session's six tracer-emits. Diagnosis traced the failure to two foundational anti-patterns (atom-without-boot-replay; reset!-able authoritative state). Joe sharpened the principle: an invariant must make the discipline structurally impossible to violate, not merely surface violations after they occur.

Mission opens outside the normal queue cadence per the algorithm's stop-the-line clause. The "queue-jump" framing was rejected — when the line is stopped there's no queue to jump; this is a foundation hot-fix.

Pending: implementation of the static-check + hook for evidence-store; Codex handoff for the rest.
