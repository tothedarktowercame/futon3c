Status: open

# M-bounded-in-flight-state: transactional discipline over the file-system substrate

**Phase:** IDENTIFY (2026-05-03)
**Working name:** subject to revision in MAP. The research artefact carries the name `M-commit-as-you-go`; the mission-level name should reflect the *invariant* shape rather than the operator-action shape.
**Parent:** M-the-futon-stack (Q4: is the stack set up so the invariants can work?)
**Prerequisite for:** M-live-geometric-stack (the live counterpart cannot compute reliable Δ or drift while the substrate carries half-finished transactions).
**Sibling-class candidates:** M-bounded-disposition (commitment-tracking), M-reachable-from-boot (self-recovery), M-state-snapshot-witness (autobiographical memory). Probably a new sibling-shape; see Q-shape below.
**Lab dir:** `holes/labs/M-bounded-in-flight-state/`
**Research artefact:** `holes/labs/M-commit-as-you-go/cleanup-2026-05-03-futon6-data-leak.md` (40 commits across 13 repos, 18 findings, 9 phases — the founding empirical input).
**Candidate queue entry:** `futon5a/data/stack-stereolithography-priority-queue.json` `run-068` (research-phase name; will be revised once MAP confirms the invariant name).

## 1. IDENTIFY

### The gap (operator's framing, 2026-05-03)

> If the operator doesn't say "now commit and push" all the time, we accumulate loads of messy files.

Stated minimally. Empirically true: yesterday's audit surfaced eight repos simultaneously dirty, several with 50-500 uncommitted entries; today's sweep cleared them in 40 commits across 13 repos. The same dynamic applies to PRs (a parallel form of "in-flight scope"); it's mentioned here for completeness but the mission's locus is the working tree.

The mess is not a tooling preference. **The mess is a substrate-state defect** — every dirty file is an in-flight transaction the system has no way to reason about. Some of the cascading effects:

- **Obvious effects:** drift between origin and working tree; conflict surface on next pull; lost work when a process clobbers an uncommitted file (cf. the futon7 `*.clj` zero-byte glob misfire); operator confusion about which version is real.
- **Effects only visible via M-live-geometric-stack:** Δ and drift signals computed over the substrate manifold are corrupted when the substrate carries half-finished transactions; the piano roll's frozen-tape view shows the same; a "live" reading of the geometric stack reading mid-edit working trees would yield gibberish (gradients pointing nowhere, drift indistinguishable from authoring).

The first set is the operator-felt cost. The second set is the structural cost — the one that makes this an *invariant* concern rather than a discipline preference.

### Reframe: transactional memory applied to file-system-backed storage

The natural framing — "commit as you go" — *sounds* like a VCS automation feature. It is not. The right framing was named (operator, 2026-05-03) as **Clojure-style transactional memory applied to file-system-backed storage**:

| Clojure STM | TM-on-FS (futon stack) |
|---|---|
| Atom/ref holds well-defined value | File system holds well-defined state — *iff* every transaction has committed |
| `swap!` / `dosync` is a transaction | An edit-burst, ending in `git commit`, is a transaction |
| Between transactions, state is observable & coherent | Between commits, durable substrate (git history + futon1a + storage/) is coherent |
| Transactions are bounded — they complete or roll back | Edit-bursts must complete (commit) or roll back (`git restore`/`git stash`) within bounded time/scope |
| Open transactions don't contaminate observable state | Uncommitted state is **legitimately invisible** to other observers — but only because it's *temporary* |

Restated as the invariant claim:

> **At every observation point, the durable state of the file-system substrate must be reachable from a sequence of completed transactions, with bounded in-flight scope.**

This is structural: a property *the substrate must hold* if anything reading it (other operators, agents, M-live-geometric-stack's gradient computation, the mission machinery itself) is to get coherent answers.

### What this is *not*

- **Not jj.** jj automates the act of committing — every working-copy state is a change-id. We do not auto-commit. *What counts as a transaction* is a semantic question only the operator can answer (the 504-file futon6 deletion, the 28-file path-rename, the entangled WS-bridge fix all have different transaction shapes — no automation can correctly bucket them).
- **Not pijul.** pijul rebuilds conflict theory from below with patch-as-typed-object. We do not touch conflict semantics. Git is the substrate; we add an invariant *layer* on top.
- **Not "commit more often as a discipline."** That would be a preference. This is an invariant — a property the system must hold, with a check-fn that surfaces violations as evidence.

### What it is

A **structural invariant** (in the M-the-futon-stack sense) that:

1. **Pressures the operator** when in-flight scope grows past bounds — graduated drive, not binary alarm. Sleep-pressure analogue: tolerable for a while, increasingly hard to ignore.
2. **Surfaces at boot-time** as evidence (`:family-fired`) like every other invariant in the skeleton.
3. **Composes with sibling invariants** — works alongside `bounded-disposition/branch`, `bounded-disposition/mission-doc`, `single-locus/*`, `reachable-from-boot/*` rather than replacing any.
4. **Operates *above* git** — git is the durable substrate; we don't modify its commit/merge/conflict semantics.

### Empirical input (already gathered)

The M-commit-as-you-go research artefact is the founding evidence base. Highlights:

- **40 commits across 13 repos** in one operator session.
- **18 findings** clustering into 5 axes: reconciliation, locate-and-verify, scope-and-boundary, stage-and-commit, operational hygiene.
- **20-question checklist** for "what does a worst-case cleanup ask?" — the basis of the check-fn's pre-flight logic.
- **Concrete failure modes** observed: WIP-entanglement, gitignored-but-volatile generated artefacts (priority queue, vitality scans), operational refs to moved paths (101 files for futon6, plus futon7's daily.clj), zero-byte zombie files, build-artefact pollution (futon0/web/), heterogeneous commit-vs-coherent-commit tradeoff, lab-dir lifecycle proliferation.

The mission's job is **not** to gather more evidence; it's to **specify the invariant precisely enough to instantiate**.

### Provisional cognitive-faculty placement

The current futonic skeleton has 9 shapes (5 new + 4 predecessor; see M-the-futon-stack §1). All 9 are *static binding* (single-boundary, single-locus, evidence-per-turn, etc.) — they fire as `:ok` / `:violation`.

This invariant looks different. It's a *graduated drive* that rises continuously and modulates priorities before reaching stop-the-line. The biological analogue is sleep pressure / homeostatic drive. **Provisional 10th shape**: `accumulating-drive` (faculty: *homeostatic-pressure* — knowing how much unfinished business has built up since the last reset, and *feeling* proportionally).

This is provisional because:

- It might prove to fit `bounded-disposition` extended — that shape's `commitment-tracking` faculty is adjacent.
- It might prove to be its own shape with its own siblings (drive over a *different* substrate state, e.g. branch ahead-count, mission-doc count, etc.).

MAP will determine. Calling it "10th shape" should not preclude either resolution.

## 2. MAP

This section folds in the MAP work from the 2026-05-03 IDENTIFY-extension dialogue (operator + agent), which surfaced **two structural concepts** the working-tree-specific invariant rests on: the **block** (an operator-attestable transactional unit) and the **mana economy** (the metabolic-balance frame). Both generalise beyond this mission, but per "proliferation costs attention" they are folded here rather than spawned as parent missions.

### MAP-1 — Blocks (operator-attestable transactional units)

A *block* is a coherent unit of operator-attestable change. Equivalent shapes:

| Framing | Block manifests as |
|---|---|
| Bitcoin | Miner-selected transaction set with header attesting their inclusion |
| Clojure STM | A `dosync` body — coordinated mutations bound by a transaction |
| Futonic methodology | A PSR → apply → PUR arc (intention stated, work applied, outcomes recorded) |
| Working-tree | The file changes the operator includes in `git commit -m "..."`. Commit message IS the block header |

Defining feature is **coherence + attestation**, not size or duration. A 1-line gitignore tweak and a 257-file lab-dir commit are both legitimate blocks; the operator can write one coherent message about each.

#### MAP-1a — Multi-channel block cadences (Process C is plural)

Different artefact-classes have different natural block-rhythms:

| Cadence | Block boundary | Period | Example |
|---|---|---|---|
| **PSR-block** | PSR/PUR cycle completion | minutes-hours | Apply pattern X to tension Y, document outcome → commit |
| **Session-block** | dev-laptop-env start ↔ stop | hours | A session's edits cluster into a session-end commit batch |
| **Day-block** | natural workday boundary | ~24h | End-of-day cleanup commit |
| **Mission-phase-block** | IDENTIFY → MAP, MAP → DERIVE, etc. | days-weeks | Phase transition is itself a commit point |
| **Excursion-block** | E-* opens → E-* closes | days | Excursion completion = commit |

Pressure dissipates at **any** block boundary. The futonic methodology already carries all of these; this invariant makes them *load-bearing for substrate transactional discipline*, not just authoring conventions.

#### MAP-1b — Blocks are hinge-encounters (`structure/cook-ting`, `structure/hinge-point`)

A block-closure at a hinge is the **blade-of-no-thickness slipping through a cavity** — the cleavage was already there; the operator perceives it and slips in. A block-closure forced at a non-hinge is **cutting through gristle** — possible, but it dulls the blade. The pressure function does not *create* hinges; it **reveals that they're nearby or overdue**, and the operator's skilled action is perception.

Diagnostic from cook-ting: *"If the operator's hand is hovering, or is making many small niggling strokes, the blade is in the wrong place. Step back. Look for the cleavage. Do not force."* — applies directly here. If the pressure invariant is making the operator hover over `git commit -m` in the wrong place, the threshold or the cadence is mis-tuned.

This means the invariant must be **gentle by default and sharp only at clear hinges**. Stop-the-line tier should fire only when an obvious hinge has been *missed* — not to manufacture one.

### MAP-2 — Mana economy + allostatic load

The deeper concept: **the invariant is not about working-tree state per se. It's about the operator's metabolic state in the stack.** Working-tree dirtiness is one *symptom*; many other symptoms share the same shape:

- **Mission proliferation** beyond operator capacity to attend.
- **Session sprawl** (sessions left running too long without natural close).
- **Untriaged signals** (evidence accumulating without operator review).
- **Branches/PRs ahead-of-origin without disposition**.
- **Open excursions without arrival**.
- And the locus of this mission: **uncommitted edits past natural hinge-cadence**.

These all *drain mana*. A block-closure at a hinge *awards mana*. The allostatic-load model: when net drain exceeds award over time, the system degrades — the operator tires, perception of hinges sharpens less, the blade nicks more often, more proliferation, more drain. Without the award channel, the system has no way to recover capacity.

Restating the invariant in this frame:

> **Net mana balance over time stays non-negative. Block-closures award; allostatic-load symptoms drain; the operator's task is to perceive hinges and close blocks faster than load accumulates.**

The working-tree case is then one *measurable symptom-channel* among several — the most concrete one to instantiate first because (a) git already provides per-file discrete state, (b) the cleanup-as-block pattern was just demonstrated empirically (40 commits / 18 findings), and (c) it's the gateway to M-live-geometric-stack's substrate semantics.

#### MAP-2a — Connection to existing futonic discipline

The mana economy isn't new infrastructure; it's a **lens on what already exists**:

- `scripts/give-mana` (now in futon5; previously in futon3) is the explicit award mechanism.
- PSR/PUR cycles are block-shaped — already discrete units of operator-attestable change.
- Evidence-per-turn for chats is the *automatic* form of block-discipline (each turn → one evidence entry). Block-discipline for code is the operator-attestable parallel.
- `structure/hinge-point` and `structure/cook-ting` already articulate the perception-not-imposition stance.
- Mission/excursion lifecycle phases already carry natural block boundaries.

What this invariant adds is a **measurement layer**: surfacing the symptoms (drain) and the awards (commit at hinge), so the imbalance is legible before it becomes catastrophic.

#### MAP-2b — Why this folds into one mission rather than spawning a family

Per operator (2026-05-03): "proliferation of missions costs attention." A `M-mana-allostasis` parent mission would replicate (in slow motion) the very anti-pattern it's trying to track. **Better to instantiate the working-tree symptom-channel here, and let the broader frame be carried in the mission's framing rather than as separate missions.** Future siblings (mission-proliferation-bound, session-sprawl-bound, etc.) can be added as the same invariant's siblings *if and when* a clear hinge for each opens.

The mission's name (`M-bounded-in-flight-state`) becomes adequate-but-narrow under the new frame. ARGUE may revisit; for now, keep the name and let the broader framing be carried in IDENTIFY/MAP text.

### MAP-3 — Updated answers to the seven (eight) questions

Each question's MAP-level answer, refined by the block + mana frame:

#### Q-shape

**Lean: 10th cognitive-faculty shape — `metabolic-balance`** (faculty: *allostatic homeostasis* — knowing the system's current mana balance and its trajectory). Earlier draft called it `accumulating-drive` / `homeostatic-pressure`; the block + mana frame generalises that to a balance-tracking faculty that can both *award* (at hinges) and *drain* (with allostatic load).

`bounded-disposition` siblings (branch, mission-doc, stash) can be reread as **single-tier drain channels** within the same faculty: their default disposition is the drain, their non-default disposition is partial-recovery, their full retirement (merged-not-yet-deleted → deleted; archived; closed) is the award. This unifies their structure with the new invariant without forcing structural change to the existing operational siblings.

ARGUE will ratify the new-shape vs extend-bounded-disposition decision against the unified read.

#### Q-substrate-boundary

Unchanged: full substrate = git working trees + futon1a + ~/code/storage/. **First INSTANTIATE = git working trees only.** Widening per sibling rollout.

#### Q-transaction-boundary

Refined: **the transaction is the *block*; the block manifests as one or more git commits.** A commit is the persistence layer; the block is the attestation layer. A block-id may live in `git notes`, a commit-message footer, or a sidecar; the choice is a Q-disposition-surface adjacency, deferred to ARGUE.

#### Q-pressure-function

Now reframed as a **mana-flow function**:

```
balance(operator, t) = ∫ awards(τ) dτ  −  ∫ drains(τ) dτ        for τ ∈ [t₀, t]

where:
  awards = block-closures at recognised hinges (positive impulse per close)
  drains = symptom accumulation:
    S(working-tree)         — count × age × size, eligibility-filtered
    S(missions)             — open-count past attention-bound
    S(sessions)             — sessions-overdue-for-close
    S(branches/PRs)         — ahead-of-origin past hinge-cadence
    ...

  Process C (per-channel cadence) modulates each S — i.e. a drain
  starts low while within-cadence, rises sharply when a hinge passes
  unrecognised.
```

First INSTANTIATE implements the working-tree drain channel only. Other channels added as siblings.

Tier semantics under cook-ting reread:

- *P low* — no force; nothing to perceive yet.
- *Advisory* — a hinge is approaching; HUD pulse or single boot-line surfaces the cleavage.
- *High* — a hinge appears overdue; operator-visible nudge surfaces *which* hinge.
- *Stop-the-line* — a hinge has clearly been missed; the substrate's transactional discipline is now broken; operator must close before continuing.

Stop-the-line is rare by construction — a missed hinge, not a manufactured one.

#### Q-disposition-surface

Refined working answer: **`.futon-disposition.edn` per repo for working-tree opt-out, paired with `git notes` for block-id annotations** (so a block spanning multiple commits can carry a stable identifier). The `.edn` schema:

```edn
{:in-progress {"path/or/glob" {:reasoning "..."
                                :since "2026-05-03"
                                :review-by "2026-05-17"}}
 :ignored-here-but-tracked {...}
 :decided-at "2026-05-03T..."}
```

Future evolution: futon1a evidence as the canonical home if file-based proves cumbersome.

#### Q-relationship-to-siblings

Still: **edit-time counterpart of `reachable-from-boot/*`'s boot-time check.** Now further: **first symptom-channel of `metabolic-balance` shape**; bounded-disposition siblings reread as single-tier drain channels.

#### Q-relationship-to-PRs

Still deferred to a future sibling; same shape, branch-scope variant.

#### Q-naming

Provisionally: keep `M-bounded-in-flight-state` (working-tree focus). The broader frame (mana / allostasis / metabolic balance) is captured in IDENTIFY / MAP text; renaming would itself proliferate.

If ARGUE forces a rename, candidate is `M-stack-metabolism-working-tree` — explicit about both the shape (metabolism) and the locus (working-tree).

### MAP — outstanding work

| Question | DERIVE/ARGUE deliverable |
|---|---|
| Q-shape | ARGUE: ratify `metabolic-balance` shape vs `bounded-disposition` extended; reread bounded-disposition siblings under the unified read |
| Q-substrate-boundary | DERIVE: per-sibling rollout plan |
| Q-transaction-boundary | DERIVE: block-id mechanism (commit footer vs `git notes` vs sidecar) |
| Q-pressure-function | DERIVE: specific functional form; calibrate thresholds against today's empirical run |
| Q-disposition-surface | ARGUE: `.futon-disposition.edn` vs `git notes` vs futon1a evidence |
| Q-relationship-to-siblings | DOCUMENT: write the unified read as a flexiarg under `library/invariant-coherence/` |
| Q-relationship-to-PRs | (out of scope; future sibling) |
| Q-naming | ARGUE: revisit only if reread forces it |

Five Qs decided at MAP. Three (Q-shape ratification, Q-pressure-function form, Q-disposition-surface alternatives) carry working answers held provisionally for ARGUE.

---

## Open questions for DERIVE / ARGUE / VERIFY / INSTANTIATE (originally enumerated; preserved here as the working set MAP refined)

Each question carries its own derivation thread. Specific questions, not "go figure it out":

### Q-shape

Is this a new 10th cognitive-faculty shape (`accumulating-drive`), or an extension of `bounded-disposition` (commitment-tracking faculty)? Test: do the existing bounded-disposition siblings (branch, mission-doc) admit a graduated-drive variant cleanly, or do they break when forced to? If they break, new shape; if they bend, extension.

### Q-substrate-boundary

What counts as the "durable substrate" the invariant guards? Candidates:

- Git working trees only.
- Git + futon1a evidence store.
- Git + futon1a + `~/code/storage/`.
- Git + futon1a + storage + gitignored runbook source-of-truth files (e.g. `stack-stereolithography-runbook.edn`).

The futon7/data/probes case (moved to storage; daily.clj still references the old path) suggests the substrate is *all reachable durable state*, not just git. But that's an empirical observation, not yet a derivation.

### Q-transaction-boundary

What's a "transaction"? Per-commit? Per-session? Per-mission? Per-theme-cluster? Finding 18 (theme-driven not path-driven clustering) hints at *thematic*, not size-based. But operationally we need a boundary the check-fn can recognize.

### Q-pressure-function

Specific function over (count, age, size, churn, repo-priority, lab-dir-vs-source-vs-data, ...). The 18 findings give us the input variables; the function shape is open. Candidates:

- Linear: `P = max(count/N, age/D)`
- Multiplicative: `P = (count * age) / (N * D)`
- Content-aware: weights vary by file class (lab-dir, source, generated, mission-doc, etc.)
- Two-process model: homeostatic process S + circadian process C (natural cadences — end of session, day boundary, mission boundary).

### Q-disposition-surface

How does the operator declare *legitimate* in-flight state — work that's deliberately uncommitted, e.g. a long refactor in progress? Candidates:

- `.futon-disposition.edn` per repo (sketched yesterday).
- `git notes` annotations.
- Session-scope marker (a sidecar `.session-id`).
- Live evidence in futon1a tagging working-tree paths as `:in-progress`.

Each candidate has a different lifecycle and ergonomic shape.

### Q-relationship-to-siblings

Where does this sit among the existing siblings? Working hypothesis: it's the *edit-time* counterpart of `reachable-from-boot/*`'s *boot-time* check — the same property (substrate is reconstructible from durable state) viewed at two timescales.

### Q-relationship-to-PRs

PRs are a parallel form of in-flight scope (changes that haven't merged into the trunk). Is this invariant cleanly extensible to PRs? Or does the local working-tree case need to ship first and PRs become a separate sibling? Operator's intuition (2026-05-03): "the same in principle, but a passing comment for now."

### Q-naming

`M-bounded-in-flight-state` is a working name. Real name comes from MAP, once the invariant shape is settled. Candidates: `M-bounded-in-flight-state`, `M-transactional-working-tree`, `M-substrate-transaction-discipline`. The right name probably emerges from how Q-shape resolves.

## Scope in

- The check-fn that surfaces working-tree pressure as boot-time evidence.
- The pressure function (variables, tiers, opt-out surface).
- The disposition vocabulary and its declaration mechanism.
- Integration with the existing structural-law-inventory and probe-tap registry.
- A worked example of the operator-attestable commit flow under the new invariant.
- Pairing with M-live-geometric-stack's substrate semantics (consistency proof: "given this invariant, M-live-geometric-stack's Δ/drift signals are well-defined").

## Scope out

- Reimplementing git, jj, or pijul. Git stays the substrate. We do not touch its semantics.
- Auto-committing. The act of committing remains operator-controlled.
- Modifying conflict/merge theory.
- PR/branch in-flight scope (mentioned in Q-relationship-to-PRs as a future sibling, not in scope here).
- Multi-machine federation of in-flight state. Single-machine first.

## Time box

MAP through INSTANTIATE in roughly the rhythm of M-bounded-disposition / M-reachable-from-boot / M-single-locus (siblings). Calendar bound: weeks-not-quarters. Not race-against-clock; the empirical input is already gathered.

## Completion criteria

1. Q-shape, Q-substrate-boundary, Q-transaction-boundary, Q-pressure-function, Q-disposition-surface answered to the depth needed for a check-fn to be writable and testable.
2. The invariant is *operational* (firing on every JVM start, not `:operational-when-enabled` only).
3. Worked example: take a deliberately-messy working tree, run the check-fn, see graduated pressure surface, resolve via commit/stash/ignore/in-progress; the same flow repeats verifiably on a second example.
4. M-live-geometric-stack's substrate-semantics MAP can cite this invariant as held — i.e. its Δ/drift derivation does not need to defend "what if working trees are dirty?" because *they aren't allowed to be unboundedly dirty*.
5. M-the-futon-stack §"What invariantly exists today" updated to reflect the new shape (whether 10th cognitive faculty or extension of #2 commitment-tracking).
6. The structural-law-inventory has a new family entry; the probe-tap registry has a new family-id; the queue entry (`run-068`) is reconciled with the actual mission name.

## Risk register

- **Drift toward jj**: if the check-fn's "advisory" tier becomes too aggressive, it'll feel like an auto-commit nag. Mitigation: keep the lower tier silent; only the high tier is operator-visible; stop-the-line is the rare case.
- **Over-counting WIP**: legitimately-paused work (refactors, exploratory edits) shouldn't accumulate pressure as if it were drift. The disposition surface (Q-disposition-surface) is the relief valve; if it's too cumbersome, the invariant becomes a chore.
- **Substrate boundary ambiguity**: if Q-substrate-boundary admits multiple correct answers depending on context, the check-fn becomes hard to specify. Mitigation: pick the smallest defensible substrate (git working tree only) for the first INSTANTIATE; widen later if M-live-geometric-stack's MAP needs more.
- **Premature shape commitment**: declaring this a 10th shape too early might foreclose the (cleaner) "extend bounded-disposition" path. Mitigation: hold both candidates open through MAP; let the check-fn implementation force the resolution.
- **Operator-burnout** (general futonic risk): another invariant to maintain is another cognitive load. Mitigation: this invariant *reduces* operator load over time — it surfaces "you have N hours of pent-up commit pressure" earlier than the operator would notice unaided.

## Owner and dependencies

- **Owner:** operator (Joe), with agent assistance for derivation, implementation, and worked examples.
- **Empirical input:** complete (40 commits, 18 findings, 20-question checklist in the M-commit-as-you-go research artefact).
- **Dependencies-in:** `bounded-disposition` siblings as design reference; `single-locus/artifact-live-copy` for the storage-vs-tracked-vs-untracked taxonomy precedent; `reachable-from-boot/*` as the dual.
- **Dependencies-out:** **M-live-geometric-stack** awaits this invariant's INSTANTIATE before its substrate-semantics MAP can stabilise.

## Lifecycle phases (placeholders for now)

### 2. MAP

To be written. The seven open questions above are the workload.

### 3. DERIVE

DERIVE proceeds via three new patterns drafted 2026-05-03 (operator confirmed the
"block as instantiation of futon" claim against `futon-theory/futonic-logic`'s formal
definition; the futonic loop's 咅 → 鹽 step *is* the git commit). Patterns:

1. **`futon3/library/structure/block-as-futonic-revolution.flexiarg`** — central
   structural claim. A block is one revolution of the futonic loop:
   象 → 部 → 咅 → 鹽 → 香 + 🔮 → 未知 → 味. Names the **five-phase block
   lifecycle** (identification → specification → elaboration → **passion** →
   closure) — the explicit *passion* phase distinguishes earned from
   counterfeit closure. Block-bar-Bitcoin-STM convergence noted; jj-style
   auto-commit named as the principal anti-pattern.

2. **`futon3/library/structure/mana-allostasis.flexiarg`** — dynamic claim.
   Net mana balance is the integral of (block-closures-at-hinges − allostatic-
   load-symptoms). **Dissonance is the precursor to resolution** — load is not
   a defect to minimise; it is the structural tension that makes block-closure
   meaningful. Pluri-channel framing (working-tree, missions, sessions,
   branches, evidence) made explicit.

3. **`futon3/library/invariant-coherence/drain-channel-shape.flexiarg`** —
   invariant-family claim. Existing `bounded-disposition` siblings (branch,
   mission-doc, stash) reread as **single-tier drain channels**;
   bounded-in-flight-state is a graduated-tier channel; all share the
   *metabolic-balance* faculty. Harmolodic voicing: drains are co-equal voices
   in one harmonic structure, not subordinated to a dominant line.

These three together discharge the structural, dynamic, and family-shape derivation
threads. Music-theoretic anchoring (bar / dissonance / harmolodics) runs through
all three; explicit cross-references to `structure/cook-ting` and
`structure/hinge-point` keep them anchored in the existing pattern theory.

Outstanding for ARGUE: ratify the `metabolic-balance` faculty against the
"extend bounded-disposition" alternative; calibrate pressure-function thresholds
against the 2026-05-03 empirical run (40 commits / 18 findings).

### 4. ARGUE

Five threads from MAP/DERIVE require explicit argument. Each follows
IF/HOWEVER/THEN/BECAUSE; alternatives are named and rejected; the
chosen decision is the one defended.

#### ARGUE-1: Faculty shape — `metabolic-balance` (new 10th) vs `bounded-disposition` (extended)

**IF**: a new sibling invariant requires a graduated drive (continuous
pressure with multiple tiers), and the existing nine-shape skeleton
has only static-binding faculties.

**HOWEVER**: two paths exist:
- *Extend* `bounded-disposition` to admit graduated tiers. Existing
  operational siblings (branch, mission-doc, stash) currently use a
  single binary bound; extending the family forces one of two awkward
  outcomes — (a) only the new sibling uses graduated tiers, leaving
  the family inconsistent, or (b) all siblings get graduated tiers,
  changing already-operational invariants in production for no
  separate reason.
- *Add* a new 10th shape `metabolic-balance`. Cleaner separation, but
  introduces a new entry to the cognitive-faculty skeleton (M-the-
  futon-stack §"What invariantly exists today" needs updating).

**THEN**: introduce `metabolic-balance` as the 10th shape (faculty:
*allostatic homeostasis*) and **reread** the existing
`bounded-disposition` siblings as *single-tier instances* of the same
faculty under the harmolodic frame
(`invariant-coherence/drain-channel-shape`). No structural change to
operational siblings; no inconsistency. The 9-shape skeleton becomes
10-shape.

**BECAUSE**: the harmolodic reread is a *strict generalisation* — it
describes existing siblings under a richer frame without breaking
them. The faculty is unchanged in their case (bounded means bounded);
what's added is the cross-channel diagnosis ("what is the system's
metabolic balance?") which the operator already does informally and
which the apparatus can now name. The cost (one new shape) is paid
only once and unifies subsequent siblings (PRs, sessions,
untriaged-evidence, etc.) under the same faculty. The alternative
(extend `bounded-disposition`) accumulates change-cost on every
existing sibling.

*Rejected alternatives*:
- *Hybrid (split graduated drive into a separate sub-shape under
  bounded-disposition)*: doesn't simplify the structure; introduces
  an opaque hierarchy.
- *Defer the decision to INSTANTIATE*: would require the check-fn to
  be written without knowing which family-id namespace it lives in.
  The decision must precede implementation.

#### ARGUE-2: Disposition surface — `.futon-disposition.edn` per repo

**IF**: the operator must declare legitimate in-flight state (work
deliberately paused; a refactor in progress; experimental edits)
without losing access to the relief valve.

**HOWEVER**: four candidate surfaces:
- `.futon-disposition.edn` per repo (file-based, version-controlled)
- `git notes` annotations (ref-attached, no tree pollution)
- Sidecar marker (e.g., session-id-tagged)
- futon1a evidence (queryable, integrated with existing machinery)

Each has different lifecycle properties. File-based is most
discoverable but adds a file. `git notes` is least intrusive but
tooling is uneven. Sidecar markers are ephemeral. futon1a evidence
is the most integrated but introduces a runtime dependency for a
property that needs to read at boot.

**THEN**: choose `.futon-disposition.edn` for the first INSTANTIATE.
Schema (per pattern in MAP):

```edn
{:in-progress {"path/or/glob" {:reasoning "..."
                                :since "2026-05-03"
                                :review-by "2026-05-17"}}
 :ignored-here-but-tracked {...}
 :decided-at "2026-05-03T..."}
```

The file is itself tracked. Each entry must carry a `:reasoning` and
either a `:review-by` date or an explicit `:no-review-needed`
declaration. Entries without `:review-by` and older than 30 days
become themselves a drain symptom (an in-progress declaration that
has overstayed its welcome).

**BECAUSE**: the disposition surface is a place the operator **goes
to attest** something. File-based is consistent with how the operator
attests every other futonic decision (PSRs in lab dirs, mission docs
in holes, structural-law-inventory in docs). The `:review-by`
self-decay prevents the disposition surface from becoming a drain-
hiding mechanism — exactly the failure mode the invariant is built
to prevent.

*Rejected alternatives*:
- `git notes`: tooling is uneven across operator workflows; futon
  agents inspect working trees more naturally than ref-attached
  metadata. Re-evaluate if a clear use-case emerges.
- Sidecar/session marker: wrong lifecycle. In-progress declarations
  outlive sessions.
- futon1a evidence: introduces a coupling between the boot-time check
  and the runtime evidence store. Premature optimisation; revisit if
  file-based proves cumbersome.

#### ARGUE-3: Pressure function form — `max(...)` over normalised channels

**IF**: the pressure function must surface drain-channel state as a
felt-sense signal that scales smoothly across channels of different
units (count, age, bytes).

**HOWEVER**: candidate forms:
- *Linear sum*: `P = α·count + β·age + γ·bytes`. Tunable but no
  tier-membership signal.
- *Multiplicative*: `P = count × age × bytes / (N·D·B)`. Sharp
  growth; overshoots quickly.
- *Weighted geometric mean*: `P = (count^a · age^b · bytes^c)^(1/(a+b+c))`.
  Smooth but requires three exponents to tune.
- *Max over normalised channels*: `P = max(count/N, age/D, bytes/B)`.
  Each channel saturates the signal independently; tier crossings are
  legible.

**THEN**: choose **`max` over normalised channels**:

```
S(channel-state) = max(count_eligible / N_NOMINAL,
                       age_eligible_max / D_NOMINAL,
                       bytes_eligible / B_NOMINAL)
```

with channel-specific eligibility filters (working-tree:
exclude-noise, exclude-disposition-opted-out, exclude-volatile;
sessions: exclude-on-cadence; missions: exclude-:closed-and-
:archived). The tier interpretation:

- `S < 1.0`: silent (no perceived drain in any single dimension)
- `1.0 ≤ S < 2.0`: advisory (one or more dimensions at ~1×nominal)
- `2.0 ≤ S < 4.0`: high (one or more at ~2×nominal — overdue)
- `S ≥ 4.0`: stop-the-line (a hinge has clearly been missed)

Process C (per-channel cadence modulation) lives inside the
eligibility filter: a working-tree edit *within* its channel cadence
contributes to `count_eligible` weakly; once the cadence passes, it
contributes fully. This makes the function "switched on" at cadence-
crossing rather than at numeric-threshold-crossing.

**BECAUSE**: `max` preserves channel-independence — no single
channel's high reading is offset by another channel's low reading.
This matches harmolodic listening: each voice can be heard for what
it is. The alternative forms either flatten dissonance (linear sum
hides one-loud-voice cases) or amplify it disproportionately
(multiplicative pins on outliers). `max` also makes calibration
tractable: one threshold per channel-dimension, three nominal values
per channel.

The empirical run (2026-05-03; 40 commits, 18 findings) provides
the calibration anchor. With nominal `N_count=20`, `D_age=7d`,
`B_bytes=10MB` for working-tree, the futon6 pre-cleanup state
(507 dirty files, ~98MB, ~85d oldest) computes to:
`max(507/20, 85d/7d, 98MB/10MB) = max(25.35, 12.14, 9.8) = 25.35` —
deeply stop-the-line. After cleanup: `max(3/20, ..., ...) ≈ 0.15` —
silent. The function discriminates correctly at the empirical
extremes.

*Rejected alternatives*: see HOWEVER.

#### ARGUE-4: Block-id mechanism — commit message footer for first INSTANTIATE

**IF**: a single block may manifest as 1+N git commits (e.g., the
2026-05-03 sweep produced 40 commits inside what was conceptually one
"clear all WIP" block), and the apparatus needs to bind them.

**HOWEVER**: candidate mechanisms:
- *Commit message footer*: `Block: <id>` line in the commit message.
  Visible in `git log`; queryable via `--grep`.
- *git notes*: ref-attached metadata, doesn't pollute message body.
- *Sidecar `.session-id`-style file*: tracked, queryable as a tree.

**THEN**: choose **commit message footer**, optional, with format
`Block: <kind>-<YYYY-MM-DD>-<slug>` (e.g.
`Block: psr-2026-05-03-mc1`, `Block: sweep-2026-05-03-clear-wip`).
Block-id is *advisory*, not mandatory: most commits don't need one;
when a coherent multi-commit block crosses repos or sessions, the
operator (or agent acting per PSR) writes the footer.

**BECAUSE**: lowest tooling friction; uses git's existing log
infrastructure; visible to humans reading `git log` without special
tools; readable from outside the futon ecosystem. The cost is a few
extra lines on multi-commit blocks; the benefit is that
post-hoc reconstruction of "what blocks happened" is a single
`git log --grep='^Block:'` away.

*Rejected alternatives*:
- `git notes`: requires `git notes show` to inspect; less discoverable
  for casual log reading.
- Sidecar file: introduces a tracked artefact whose state has to be
  cleaned up at block-close. Adds a transaction-management problem
  to solve a transaction-binding problem.

#### ARGUE-5: Substrate widening order — open, reactive, harmolodic

**IF**: the metabolic-balance faculty admits multiple drain channels
(working-tree, mission-proliferation, session-sprawl, branch/PR
ahead-of-origin, untriaged-evidence, attention-on-too-many-tabs, etc.),
and only the working-tree sibling is in scope for first INSTANTIATE.

**HOWEVER**: there's a temptation to schedule a roadmap ("next we
do sessions, then PRs, then evidence...") so the family's growth is
predictable. But a scheduled rollout violates `cook-ting`: it
manufactures hinges where none have been recognised.

**THEN**: declare the substrate-widening order **open and reactive**.
Each new sibling is opened only when:
(a) the channel's drain has been observed empirically — a hinge is
visible — and
(b) the operator can articulate which existing block-cadence the
channel maps to (per `structure/block-as-futonic-revolution`).

The first INSTANTIATE lands working-tree only. Subsequent siblings
become candidates as their hinges open; M-the-futon-stack tracks
them as they are added.

**BECAUSE**: harmolodic structure (per `drain-channel-shape`) is
violated by hierarchical scheduling. Each voice enters when its
moment arrives. The alternative (pre-scheduled rollout) would also
contradict the mana-allostasis principle that *dissonance is a
precursor to resolution*: building siblings before the empirical
drain has surfaced is closure without prior dissonance — hollow
consonance at the architectural level.

*Rejected alternatives*: pre-scheduled rollout (above), and "all
channels at once" (overwhelming surface; INSTANTIATE complexity
explodes).

#### Plain-language argument (per `mission-lifecycle.md` §ARGUE)

In a long-running stack, unfinished work piles up across many kinds of
channel — edits not yet committed, missions left open, sessions never
properly closed — and the operator gradually tires without quite
knowing why. This mission watches each kind of unfinished work and
surfaces, quietly, when it is approaching its natural completion
point; it does not pester the operator to commit more often, because
forcing closure at the wrong moment is itself costly. Closing work
at a natural completion point feels good and earns a small lift;
holding it past that point starts to feel heavy, and the heaviness
becomes visible before anything actually breaks. The result: the
felt cost of letting things drift becomes legible early, so finding
the right moment to finish a chunk of work is easier than forcing
one.

### ARGUE outstanding

None blocking VERIFY. The five threads above ratify the choices
required for INSTANTIATE; the plain-language argument completes the
ARGUE deliverable per `mission-lifecycle.md`. ARGUE may revisit if
VERIFY surfaces empirical contradictions.

### 5. VERIFY

The 2026-05-03 sweep is the live calibration corpus: 40 commits, 18
findings, 9 phases of cleanup log, before/after futon-sync state for
14 repos. VERIFY exercises the ARGUE decisions against this corpus
and surfaces gaps honestly. Five threads.

#### V-1 — Pressure function calibration

Using ARGUE-3's `max(count/N, age/D, bytes/B)` with nominal `N_count=20`,
`D_age=7 days`, `B_bytes=10 MB`, computed for each repo at three
moments:

| Repo | Pre-sweep state | P_pre | Post-sweep state | P_post |
|---|---|---|---|---|
| futon6 | 507 D, ~98 MB, ~85d oldest | **25.35** (STL) | 3 / 2 ?? | 0.15 (silent) |
| futon3 | 69 / 43 / ↑1, large | **3.45** (high) | clean | 0.0 |
| futon3c | 89 / 32 / ↑7 | **4.45** (STL) | 6 dev/* deferred | 0.30 (silent, deferred-with-reason) |
| futon0 | 14 / 28 / ↑9 | **2.10** (high) | web/ deferred | 0.05 (silent) |
| futon5a | 1 / 82 | **4.10** (STL) | clean | 0.0 |
| futon4 | 12 / 17 / ↑1 | 1.45 (advisory) | clean | 0.0 |

**Findings:**
- Tier discrimination works at the empirical extremes. Pre-sweep,
  three repos cross stop-the-line (P ≥ 4.0); two cross high (2 ≤ P < 4);
  one stays advisory. Post-sweep, all are silent or near-silent.
- The "deferred-with-reason" channels (futon3c entangled dev/*,
  futon0/web/ untracked) compute as low pressure post-sweep
  *because* eligibility filters exclude them when their
  disposition is documented (here: in the mission text, even though
  `.futon-disposition.edn` doesn't yet exist).
- **Calibration sanity check**: the operator's felt-sense pre-sweep
  ("this is a mess, time to clean") matched the function's stop-the-
  line readings on three repos simultaneously — exactly when the
  cross-channel diagnosis (`drain-channel-shape`) says systemic
  strain is present.

#### V-2 — Five-phase block lifecycle audit

Three representative blocks from today, walked phase-by-phase:

**Block A — futon6 data-leak cleanup (one commit, 504 deletions)**
- *Identification*: operator sees 507 dirty in `futon-sync` (P=25.35);
  hinge clearly present.
- *Specification*: "data/* and se-data/math-processed/* outputs belong
  in storage, not the source repo."
- *Elaboration*: 24-step pre-flight + storage-mirror-existence check +
  count cross-reference (the worked checklist that became the
  cleanup-log artefact).
- *Passion*: actual file-system mv, gitignore update, staging — 6
  bash phases.
- *Closure*: commit `338a2fa`; futon-sync drops 507 → 3.
- All five phases present. ✓

**Block B — futon3c WS-bridge fix attempt (BLOCKED at pre-commit hook)**
- *Identification*: WS-bridge parse-failed errors; hinge clear.
- *Specification*: "buffer chunked frames; parse on `last?` true."
- *Elaboration*: code change written; test would have to run live.
- *Passion*: code patch made; verified via JVM hot-reload behaviour.
- *Closure*: **rejected** — pre-commit hook fired on dev.clj's
  pre-existing `swap! reg/!registry` call sites (operator WIP
  entanglement). Block did not close on disk.
- The substrate's invariant fired correctly. The five-phase model
  predicted it: closure (5) failed because the file's *content*
  contains material from outside this block's specification (2). The
  block as-specified was correct; its substrate-projection was
  contaminated by an earlier block's open WIP.
- ✓ The lifecycle correctly identifies "block half-closed" as the
  state to persist (the in-memory JVM has the change running; the
  on-disk record awaits the surrounding WIP block to close).

**Block C — futon3 path rename `holes/<repo>.devmap → holes/features/<repo>.devmap` (28-file commit)**
- *Identification*: pattern-detection on M files showed identical
  "+1/-1 line replacement" diff signature across 28 files. A
  systematic-rename hinge.
- *Specification*: "all path references update to new location."
- *Elaboration*: file list enumerated; commit message specified.
- *Passion*: stage 28 files, write commit message.
- *Closure*: commit `3177195`; 28 files, +55/-54.
- All five phases present, with *Identification* doing the
  load-bearing work — the operator (agent) detected the rename
  signature; without that perception, the commit would have been
  bundled with unrelated changes and the block would not have been
  one block.

#### V-3 — Harmolodic reread sanity check

Did re-reading the existing `bounded-disposition` siblings as
single-tier instances under `metabolic-balance` change their
operational behaviour? Test: did today's 52 mission-doc dispositions
+ 5 branch dispositions + 0 stash dispositions continue to fire and
clear correctly under the existing check-fns?

- `bounded-disposition/branch`: 5 dispositions set (`merged-not-yet-
  deleted` for one, `abandoned` for four). Existing check-fn behaviour
  unchanged; futon-sync confirms the violation cleared.
- `bounded-disposition/mission-doc`: 52 dispositions set (22+15
  archived, 15 parked). Existing check-fn behaviour unchanged; bound
  satisfied for both repos.
- `bounded-disposition/stash`: not exercised today; baseline retained.

✓ The reread is a strict generalisation. No behavioural change to
existing operational siblings.

#### V-4 — Disposition surface schema test

Synthetic `.futon-disposition.edn` for the residual cases the
2026-05-03 sweep deliberately left in flight. Demonstrates whether
the schema (ARGUE-2) would have correctly captured the operator's
intent:

```edn
;; futon3c/.futon-disposition.edn  (proposed)
{:in-progress
 {"dev/futon3c/dev.clj"            {:reasoning "WS-bridge chunk-buffering fix interleaved with operator WIP on bridge-state tracking; commit when surrounding WIP lands."
                                     :since "2026-05-02"
                                     :review-by "2026-05-17"}
  "dev/futon3c/dev/bootstrap.clj"  {:reasoning "Multi-watcher shutdown-order patch interleaved with operator WIP on snapshot error handling."
                                     :since "2026-05-02"
                                     :review-by "2026-05-17"}}
 :decided-at "2026-05-03T19:13:00Z"}

;; futon0/.futon-disposition.edn  (proposed)
{:in-progress
 {"web/"  {:reasoning "war-machine UI source tree (125 MB, 2825 files) deferred to M-single-entry-point's INSTANTIATE; needs source-only subset selection before adding."
            :since "2026-05-02"
            :review-by "M-single-entry-point INSTANTIATE complete"}}
 :decided-at "2026-05-03T19:00:00Z"}
```

Schema test results:
- ✓ The `:reasoning` field captures the *real* operator rationale,
  not just the existence of WIP.
- ✓ The `:review-by` field admits both date-form and event-form
  ("M-single-entry-point INSTANTIATE complete"); event-form is more
  honest for missions whose date is genuinely unknown.
- ✓ The schema is small enough to write by hand without tooling
  (relevant to the "ARGUE-2 BECAUSE" claim about file-based
  attestation matching every other futonic decision).
- ⚠ **Outstanding**: how does the apparatus *enforce* `:review-by`
  self-decay? An entry whose `:review-by` is past-due should
  re-enter the eligible-for-pressure set, but mechanically the
  check-fn must read the disposition file and compare against `now`.
  This is straightforward but specifies a runtime dependency the
  current INSTANTIATE plan does not yet name. Note for V-5.

#### V-5 — Spec gaps and edge cases the live data surfaced

Honest accounting. The empirical run found three places where the
spec is not yet adequate:

**Gap 1: Cross-repo block coherence.**
Today's futon0/futon2 war-machine port spanned two repos: `futon0`
retired the report scripts, `futon2` received them. Conceptually
*one block* with one operator-attestation, but mechanically two
commits on two histories. The `Block:` footer mechanism (ARGUE-4)
*can* bind them by id, but does not bind them *atomically*. If one
side commits and the other fails, the block is half-open across
the substrate.

*Mitigation for INSTANTIATE*: name "cross-repo blocks" as a
recognised pattern and require both sides to share the same
`Block:` id. Detection of orphaned half-blocks (one repo's footer
references a block-id with no corresponding commit elsewhere)
becomes a sibling diagnostic. Not in scope for first INSTANTIATE.

**Gap 2: Counterfeit closure detection.**
ARGUE-1 + DERIVE Pattern 2 say the *passion* phase distinguishes
earned from counterfeit closure. But the spec does not provide
a check-fn-level heuristic for "this commit closed a block without
passion." Today's 40 commits all involved real work; no test case
for the failure mode.

*Candidate heuristics* (for VERIFY-INSTANTIATE bridge): empty-diff
commits; formatter-only diffs (unchanged AST); merge commits with
zero conflict-resolution payload; commits where the diff matches a
known automation signature. Each has false-positives. None is
definitive.

*Mitigation*: declare counterfeit-closure detection out of scope
for first INSTANTIATE; the disposition surface relies on operator
attestation throughout. If empirical evidence accumulates that
counterfeit closures are happening, open as a follow-up sibling.

**Gap 3: Disposition-surface enforcement loop.**
Per V-4 ⚠: `:review-by` self-decay needs the check-fn to compare
disposition entries against `now` and re-eligible past-due paths.
The implementation is straightforward; the *spec* did not previously
require the check-fn to read the disposition file. INSTANTIATE plan
must add this read step explicitly.

#### VERIFY exit

The exit criterion from `mission-lifecycle.md` §VERIFY is "the
design holds against actual data, and any gaps surfaced are named."
Met:
- Pressure function discriminates correctly at empirical extremes (V-1).
- Five-phase lifecycle held on representative blocks; the "rejected"
  Block B confirmed the failure mode the spec predicts (V-2).
- Harmolodic reread is a strict generalisation; existing operational
  siblings unaffected (V-3).
- Disposition surface schema captures real operator intent (V-4).
- Three spec gaps named honestly (V-5); all have mitigations or
  out-of-scope-for-first-INSTANTIATE declarations.

**Outstanding for INSTANTIATE**: the disposition-file read in the
check-fn (V-4 ⚠ + V-5 Gap 3); the cross-repo block-id propagation
(V-5 Gap 1, deferred); counterfeit-closure detection (V-5 Gap 2,
out of scope for first INSTANTIATE).

#### V-6 — Machine-side contract (operator obligations vs system obligations)

The MAP-2 invariant reads "the operator's task is to perceive hinges
and close blocks faster than load accumulates." Operator framing,
correct so far as it goes — but the operator's task is feasible only
if the machine carries reciprocal obligations under a contract:

| Operator-side | System-side (machine contract) |
|---|---|
| Perceive approaching hinges | Make hinge-proximity *legible* |
| Close blocks at hinges | Surface what's open, what's overdue, what's earned |
| Articulate `:reasoning` for in-progress work | Read the disposition file; honour `:review-by` self-decay |
| Distinguish passion from rubber-stamp | Provide enough block-context that closure can be inspected |

VERIFY against the four contract claims Joe named:

**Claim 1 — Mana economy visible in War Machine / AIF surface.**
Verified as coherent. War Machine is the existing dynamic strategic
visualiser (`project_war_machine`); the AIF surface already carries
portfolio-inference, observation vector, strategic state. Mana
balance + per-channel drain trajectory is a natural addition under
the same surface.
*Spec gap*: `mana-allostasis.flexiarg` says "visualise the balance
trajectory" but does not bind to a specific surface. Amend MAP-2 to
name War Machine + HUD as the canonical home (War Machine for
inspection-and-thought; HUD for momentary felt-sense).

**Claim 2 — Old futon5a mana calculations become sensible as work proceeds.**
Verified with stronger evidence than expected. Two precursors exist
in production code:
- `futon5/scripts/nonstarter_mana.clj` carries `donate`, `sospeso`,
  `pool` — the `sospeso` command has a `--session` parameter and
  computes `(1-p)*C` to pool (pure *dana*, Buddhist generosity).
  Sessions are already the mana-bearing unit; the M-bounded-in-
  flight-state framing is the formal generalisation, not a new
  parallel system.
- `futon5a/.../run-064-budgeted-action-selection--mana-gated-work.md`
  captures the candidate invariant `mana-gated-work`: "Action
  selection should be constrained by available budget or license."
  Same shape as metabolic-balance; "weakly implemented" status
  documents the gap this mission closes.
*Spec implication*: this mission's INSTANTIATE should *unify* with
`mana-gated-work` (run-064) rather than parallel it. The candidate
queue already ranks `mana-gated-work` as a sibling concern; reading
both under the harmolodic frame in `drain-channel-shape.flexiarg`
makes them voices in one composition. The futon5a runbook entry for
`run-064` should be cross-referenced.

**Claim 3 — Mana attaches to sessions, not agents.**
Verified. `nonstarter_mana.clj`'s `--session` flag is the operational
evidence: production code already takes sessions as the mana-bearing
unit. Theoretically sound: sessions are operator-coherent temporal
units; agents are instances within a session; the operator (who is
the one whose felt-sense the mana economy serves) inhabits the
session, not any individual agent. The "p4ng agent" precursor was
wrong about the unit; it was right about needing to track *something*.
*Spec gap*: ARGUE-3's pressure function specifies per-repo drain but
leaves the *aggregation unit* implicit. Amend ARGUE-3 to bind:
**drain channels measure substrate state per-repo; the operator's
mana balance aggregates per-session.** This separation matters —
substrate state is timeless (the working tree is dirty regardless of
who's looking); session balance is temporally bounded (this session
is the one accumulating fatigue).

**Claim 4 — Blocks should be visible.**
Verified as a contract requirement. `block-as-futonic-revolution.flexiarg`
names the block-id mechanism (commit footer) but does not require
runtime surfacing. Without runtime surfacing, the operator cannot
perceive "this block has been open 4h past its session-cadence" —
which is exactly the perception the invariant says they need.
*Spec gap*: amend MAP-2 (or add MAP-3) to require:
- A *current-blocks* view (what's open, per session, with cadence
  context).
- A *closure feed* (recent block-closures, for "did I just earn a
  lift?").
- A *trajectory readout* (balance over the last N hours; gradient).

#### V-6 conclusion

VERIFY agrees with all four contract claims. The existing
infrastructure (`nonstarter_mana.clj`, `mana-gated-work` candidate)
is *closer* to the spec than the spec acknowledged — sessions are
already the mana unit in code; the candidate queue already ranks
the budgeted-action-selection family as a sibling. The spec needs
three amendments to make machine-side obligations first-class:

1. **MAP-2 amendment**: name War Machine + HUD as canonical
   surfaces; specify the three required views (open-blocks, closure-
   feed, trajectory).
2. **ARGUE-3 amendment**: bind drain-channel measurement to per-repo,
   mana-balance aggregation to per-session.
3. **DOCUMENT cross-reference**: explicitly unify with futon5a
   `run-064-budgeted-action-selection--mana-gated-work` and
   `nonstarter_mana.clj` as the same family at different
   instantiation depths.

These amendments don't reopen DERIVE or ARGUE — they make explicit
what was implicit, and they bind the spec to existing infrastructure
rather than parallel it. Recommend folding into MAP/ARGUE before
INSTANTIATE.

**Outstanding for INSTANTIATE (consolidated)**:
- Disposition-file read in the check-fn (V-4 ⚠ / V-5 Gap 3).
- Cross-repo block-id propagation (V-5 Gap 1, deferred).
- Counterfeit-closure detection (V-5 Gap 2, out of scope).
- Three V-6 amendments (in scope, machine-contract).
- Unification with `nonstarter_mana.clj` + `mana-gated-work`
  candidate (in scope, infrastructure-binding).

### 6. INSTANTIATE

In free-jazz form: the head stated up-front; the details elaborate
it; the head re-stated at the end as the alignment check.

#### Head — three load-bearing amendments

These three claims are the structural anchor for everything that
follows. Implementation details are improvisations *on* the head;
acceptance is the head returning intact at the end.

**(I) Machine-side contract surface.**
War Machine + HUD are the canonical homes for the mana economy.
War Machine is the *inspectable-and-thought-about* surface (per
`project_war_machine`'s "dynamic strategic visualiser" framing);
HUD is the *momentary felt-sense* surface. Three views are
required:
- *open-blocks* — what is currently open, per session, with cadence
  context (how long open / how close to overdue).
- *closure-feed* — recent block-closures with their `Block:` ids and
  per-block awards.
- *trajectory* — net mana balance over the last N hours, with
  per-channel drain decomposition visible on demand.

**(II) Drain measures per-repo; balance aggregates per-session.**
The pressure function from ARGUE-3 operates per-repo per-channel —
substrate state is timeless (the tree is dirty regardless of who is
looking). Mana balance, however, is *operator-felt* and aggregates
per-session: the session is the temporal frame in which the
operator integrates drain and award. Two units; cleanly separated.
The implementation must surface both: per-repo channel state for
substrate diagnosis, and per-session running balance for operator
felt-sense.

**(III) Unify with existing infrastructure, do not parallel it.**
Sessions are already the mana-bearing unit in production code via
`futon5/scripts/nonstarter_mana.clj` (the `sospeso` command's
`--session` parameter; `(1-p)*C` to pool as pure *dana*). The
candidate invariant `mana-gated-work` (queue `run-064`,
`futon5a/.../run-064-budgeted-action-selection--mana-gated-work.md`)
documents the same shape as metabolic-balance, "weakly implemented"
by its own admission. INSTANTIATE binds to both: the new check-fn
reads from and writes to the same session-attributed mana store
that `nonstarter_mana.clj` already exposes, and the new family
entry in the structural-law-inventory cross-references `run-064`
as the family-level claim that this mission's check-fn is the
working-tree-channel sibling of.

#### Formal block specification (operational schema)

The head names the load-bearing structural claims. This subsection
fixes the *operational* schema for a block — the shape the check-fn
will recognise, the apparatus will record, and the operator will
inspect. It is the contract surface between the spec and the
implementation.

```edn
;; A block record (EDN; canonical home: futon1a evidence + commit footers).
{:block/id           "<kind>-<YYYY-MM-DD>-<slug>"
                       ; e.g., "psr-2026-05-03-mc1",
                       ;       "sweep-2026-05-03-clear-wip",
                       ;       "instantiate-head-2026-05-03-mbi"
 :block/session      <session-id>
                       ; the operator's coherent engagement window;
                       ; future: tied to nonstarter_mana.clj's --session.
 :block/kind         #{:psr :session :day :mission-phase
                       :excursion :sweep :ad-hoc}
                       ; one of the cadences enumerated in MAP-1a.
 :block/span         [{:repo   "..."
                       :paths  ["..."]
                       :commits ["<sha>" ...] ; nil while open
                       :role   "..."} ...]
                       ; one entry per repo touched.
 :block/phase        {:identification {:at <iso> :note "..."}
                      :specification  {:at <iso> :note "..."}
                      :elaboration    {:at <iso> :note "..."}
                      :passion        {:at <iso> :note "..."}
                      :closure        {:at <iso-or-nil> :note "..."}}
                       ; per block-as-futonic-revolution; closure-:at
                       ; is nil while open.
 :block/opened-at    <iso>
 :block/closed-at    <iso-or-nil>
 :block/articulation "..."
                       ; the operator-attestable narrative —
                       ; commit-message body, mission-doc section,
                       ; or both. The 咅 of the futonic loop.
 :block/disposition  #{:open :closed :half-closed :rolled-back
                       :counterfeit-suspected}
 :block/witness      [<evidence-id> ...]
                       ; pointers to substrate records that hold
                       ; the block's effects (commits, files at
                       ; storage, futon1a hyperedges, etc.).
}
```

**Operational invariants on the schema:**

1. A block is `:open` from the moment its `:identification` phase
   records to the moment its `:closure` phase records. Half-closed
   means closure recorded but `:closed-at` lags `:opened-at` past
   cadence (Process C engaged); rolled-back means closure failed and
   the block dissolves (`捨` in futonic terms); counterfeit-suspected
   means closure recorded without the `:passion` phase having
   `:at` set.

2. `:span` may include 1+N repos. Cross-repo blocks are bound by
   shared `:block/id` in commit footers (ARGUE-4); atomic cross-repo
   commit is *not* required (V-5 Gap 1).

3. The `:phase` map is append-only — phases record once, and
   re-opening a phase requires a new block-id, not a mutation of
   an existing record.

4. `:articulation` is the load-bearing field for accountability.
   An empty or vague `:articulation` is a counterfeit-suspected
   signal (V-5 Gap 2 vector).

#### Improvisation — implementation work

The head fixes the structural anchor. The details below elaborate
it; each item is a discrete unit of implementation work, scopable
into one or more blocks.

**Code locations:**
- `futon3c.logic.archaeology` extended with the new sibling, OR a
  new namespace `futon3c.logic.metabolic-balance` if the shape
  warrants. Decision in the first INSTANTIATE block, not here.
- `.futon-disposition.edn` parser as a small util namespace
  (`futon3c.logic.disposition-edn` or similar).
- Probe-tap registration follows existing pattern (cf.
  `obsolescence-recognition/*` siblings).
- `futon5/scripts/nonstarter_mana.clj` extended (or paired with) a
  read-side API so the new check-fn can ask "session S's current
  balance" and "session S's open blocks."

**Schema and disposition-file work:**
- `.futon-disposition.edn` schema implementation (per ARGUE-2),
  with `:review-by` self-decay logic.
- The check-fn reads the disposition file at boot AND reads it on
  each probe-tap fire; entries past `:review-by` re-enter the
  eligible-for-pressure set.
- Event-form `:review-by` (e.g., "M-single-entry-point INSTANTIATE
  complete") is parsed against current mission state at check time.

**Pressure function implementation:**
- `max(count/N, age/D, bytes/B)` per channel as ARGUE-3 specifies.
- Calibration anchors from V-1's empirical run: `N_count=20`,
  `D_age=7d`, `B_bytes=10MB` for working-tree.
- Channel-eligibility filter: noise-gitignored, volatile-tracked,
  disposition-opted-out.
- Process-C cadence modulation per channel (PSR / session / day /
  mission-phase / excursion).

**Block-id mechanism (ARGUE-4):**
- Helper for emitting `Block:` footer in commit messages (advisory).
- Helper for `git log --grep='^Block:'` parsing for diagnostics.
- Cross-repo block-id binding deferred (V-5 Gap 1).

**Inventory + registry updates:**
- Add `metabolic-balance/working-tree` family to
  `futon3c/docs/structural-law-inventory.sexp`. Status:
  `operational-when-enabled` initially, then `operational` once
  binding to `nonstarter_mana.clj` is in place.
- Cross-reference to `mana-gated-work` (run-064) as the parent
  family-level claim.
- Reconcile queue entry `run-068-commit-as-you-go--working-tree-
  commit-pressure` to reflect operational state once the check-fn
  is live (the run-id remains stable as research-phase ID).

**War Machine + HUD wiring:**
- War Machine's AIF surface gains the three views (open-blocks,
  closure-feed, trajectory). Per project_war_machine's existing
  view-mode mechanism.
- HUD widget for momentary mana balance (single-cell or
  small-strip).
- Endpoint in futon3c (`/api/alpha/aif-stack/live` or a sibling)
  emits per-session balance + per-repo channel state on demand.

**M-the-futon-stack update:**
- §"What invariantly exists today" updated to declare the 10th
  shape `metabolic-balance` and reread bounded-disposition siblings
  as single-tier instances (per ARGUE-1).
- Mermaid View 2 (invariant taxonomy) extended with the new shape
  + its working-tree sibling.

**Tests:**
- Unit tests for the pressure function against V-1's calibration
  anchors. Pre-cleanup futon6 → P=25.35; post → 0.15.
- Integration test for the disposition-file read with `:review-by`
  self-decay (synthetic file from V-4 as starting point).
- Live-JVM verification: hot-reload the new check-fn via Drawbridge,
  confirm the boot-time-equivalent invocation fires evidence
  correctly.
- Cross-channel test: a session with high working-tree drain but
  closed PSR-blocks should show net balance recovering, even though
  the per-repo drain reading is high. Confirms ARGUE-3 +
  amendment-(II) work as paired.

**Documentation (folds into DOCUMENT phase):**
- Pattern cross-references in `library/structure/` and
  `library/invariant-coherence/` updated to point at the new
  inventory entry once it lands.
- A flexiarg under `library/p4ng/` documenting the
  agent-vs-session-as-mana-unit precursor as a *flawed-but-
  instructive* historical record (per Joe's framing). New file:
  `library/p4ng/agent-as-mana-unit-was-the-wrong-unit.flexiarg`
  or similar.
- A flexiarg under `library/budgeted-action-selection/` (new
  namespace if needed) documenting the unification with
  `mana-gated-work`.

#### Calibration anchors (drawn from V-1)

These are the empirical pinning points the implementation must
reproduce:

| Repo + state | Expected P | Tier |
|---|---|---|
| futon6 pre-cleanup (507/98MB/85d) | 25.35 | Stop-the-line |
| futon3 pre-sweep (69/large/?) | 3.45 | High |
| futon3c pre-sweep (89/?/↑7) | 4.45 | Stop-the-line |
| futon4 pre-sweep (12/?/↑1) | 1.45 | Advisory |
| post-sweep (any clean repo) | 0.0 | Silent |
| futon3c post-sweep with deferred dev/* | 0.30 | Silent (deferred-with-reason) |

If the implementation's pressure readings on these states do not
discriminate by tier in the same way, the calibration is wrong; fix
before declaring `:operational`.

#### Head re-stated — alignment check

The three head amendments are also the acceptance criteria:

1. ✅ Mana economy visible in War Machine + HUD with three views.
2. ✅ Drain per-repo; balance per-session; both surfaced separately.
3. ✅ Unification with `nonstarter_mana.clj` and `mana-gated-work`
   (run-064); cross-reference recorded in inventory.

If any of the three is missing at INSTANTIATE-complete, the head
hasn't returned. Re-open INSTANTIATE; do not declare COMPLETE.

#### INSTANTIATE outstanding

The implementation work above is bounded but not trivial. First-pass
INSTANTIATE blocks (each its own block-shape, naturally):

- *Block 1*: namespace decision + check-fn skeleton.
- *Block 2*: pressure function + calibration tests against V-1 anchors.
- *Block 3*: `.futon-disposition.edn` schema, parser, `:review-by`
  self-decay.
- *Block 4*: probe-tap registration, structural-law-inventory entry,
  family-id assignment.
- *Block 5*: `nonstarter_mana.clj` read-side binding + session
  attribution.
- *Block 6*: War Machine views (open-blocks, closure-feed,
  trajectory).
- *Block 7*: HUD widget.
- *Block 8*: M-the-futon-stack §What-invariantly-exists update;
  Mermaid View 2 extension.
- *Block 9*: cross-mission flexiargs (p4ng precursor;
  budgeted-action-selection unification).

Nine blocks, distributed across futon3c (code + inventory +
mission update) and futon5 + futon5a (mana CLI binding + queue
reconciliation) and futon0 + futon3 (HUD + library docs). Each
block is one operator-attestable revolution; the whole sequence
is the INSTANTIATE phase of this mission.

#### Coup de grâce — self-bootstrapping closure

The mission specifies blocks. The current uncommitted work — three
flexiargs in futon3, the mission body in futon3c, the lab dir
README — *is* one block by the spec the mission has just written.
Closing it tests the spec on itself.

```edn
{:block/id           "instantiate-head-2026-05-03-mbi"
 :block/session      "2026-05-03-mission-arc-mbi"
 :block/kind         :mission-phase
 :block/span
   [{:repo   "futon3"
     :paths  ["library/structure/block-as-futonic-revolution.flexiarg"
              "library/structure/mana-allostasis.flexiarg"
              "library/invariant-coherence/drain-channel-shape.flexiarg"]
     :commits []  ; pending closure
     :role   "DERIVE deliverables — three new patterns"}
    {:repo   "futon3c"
     :paths  ["holes/missions/M-bounded-in-flight-state.md"
              "holes/labs/M-bounded-in-flight-state/README.md"]
     :commits []  ; pending closure
     :role   "Mission body IDENTIFY through INSTANTIATE-head + lab dir"}]
 :block/phase
   {:identification
      {:at "2026-05-03T~midday"
       :note "Mission opened from M-commit-as-you-go research artefact;
              hinge clear after Joe's TM-on-FS reframe."}
    :specification
      {:at "2026-05-03T~midday"
       :note "IDENTIFY drafted in proper-mission form; subsequent
              turns each produced one phase: MAP (block + mana),
              DERIVE (three patterns), ARGUE (5 IF/HOWEVER/THEN/
              BECAUSE + plain-language), VERIFY (V-1..V-6),
              INSTANTIATE-head."}
    :elaboration
      {:at "2026-05-03T~afternoon"
       :note "Three flexiargs drafted in full with music-theoretic
              threading (bar / dissonance / harmolodics) and the
              five-phase block lifecycle. Mission text built up
              through six substantive turns. Cross-checks against
              futon-theory/futonic-logic, structure/cook-ting,
              futon5/scripts/nonstarter_mana.clj,
              futon5a/.../run-064-budgeted-action-selection."}
    :passion
      {:at "2026-05-03T~late-afternoon"
       :note "Substantive drafting work — not rubber-stamp. Cross-
              referenced existing patterns; computed pressure values
              against empirical run; named gaps honestly. The five-
              phase walks in V-2 are themselves attestation that
              passion was present."}
    :closure
      {:at nil  ; pending the commit-pair below
       :note "On closure: paired commits across futon3 + futon3c
              with `Block: instantiate-head-2026-05-03-mbi` footer."}}
 :block/opened-at    "2026-05-03T~midday"
 :block/closed-at    nil
 :block/articulation "M-bounded-in-flight-state mission lands
                      IDENTIFY through INSTANTIATE-head, with three
                      DERIVE-deliverable flexiargs. The mission's own
                      INSTANTIATE-head amendments specify a block-
                      schema; this commit-pair closes this same
                      block under that schema, demonstrating
                      operability at the moment of specification."
 :block/disposition  :open
 :block/witness      []}  ; will fill on closure
```

**Five-phase audit on this block:**

| Phase | Status | Notes |
|---|---|---|
| Identification | ✓ | Mission-arc opened from research artefact; hinge clearly present |
| Specification | ✓ | Six turns of mission-form, each producing a phase |
| Elaboration | ✓ | Three flexiargs (full IF/HOWEVER/THEN/BECAUSE) + mission body |
| Passion | ✓ | Substantive cross-checking and pattern-anchoring; not rubber-stamp |
| Closure | pending | Paired commits across futon3 + futon3c with shared `Block:` footer |

If the closure step succeeds — that is, if a commit-pair lands with
`Block: instantiate-head-2026-05-03-mbi` in both messages, with no
pre-commit hook rejecting either side, with the diff of each
matching the `:span` declared above, with no entanglement requiring
deferral — then the mission's spec is self-consistent at the
operational level. The proof is the act, not the description.

If the closure step *fails* — pre-commit hook rejects, or some part
of the span turns out to be entangled with other operator WIP — the
spec has surfaced a real discontinuity that V-5 didn't anticipate;
INSTANTIATE re-opens, not COMPLETE.

**This is the coup de grâce**: the mission proves itself by closing
on its own terms.

#### INSTANTIATE acceptance

INSTANTIATE is COMPLETE when:
1. All three head amendments hold (alignment check above passes).
2. Nine first-pass blocks are operational (or have explicit
   deferred-with-reason status in `.futon-disposition.edn`).
3. The coup-de-grâce block above is closed, witnessing that the
   spec is operable.

Until #3 lands, INSTANTIATE remains in flight.

### 7. DOCUMENT

DOCUMENT closes the mission by:
1. Producing the docbook-style entry that explains *what was built,
   how to use it, how it connects to prior work* — readable without
   the mission doc.
2. Updating M-the-futon-stack §"What invariantly exists today" with
   the 10th cognitive-faculty shape (`metabolic-balance`) and the
   harmolodic-reread of bounded-disposition siblings.
3. Running a **QA pass** over the items that surfaced during
   IDENTIFY → INSTANTIATE but are not in scope for INSTANTIATE-
   head.

#### QA-items bookkeeping

These are real refinements / open questions / cross-checks surfaced
during the mission arc that should be addressed at DOCUMENT time
rather than during INSTANTIATE-head. Each carries a phase-of-origin
and a brief note. The DOCUMENT-time QA pass walks the list and
either resolves, defers, or dismisses each.

**From the mission text itself:**

| # | Item | Origin | Notes |
|---|---|---|---|
| Q-01 | Block-id parser disambiguation | INSTANTIATE coup-de-grâce | `git log --grep='^Block:'` matches prose-mentions too. Parser must match `^Block: <kind>-<YYYY-MM-DD>-<slug>$` specifically, ideally checking for *last* such match in a message rather than any. |
| Q-02 | Cross-repo block atomicity | V-5 Gap 1 | Block-id binds across repos but doesn't atomically ensure all sides land. Future sibling: orphaned-half-block detector. |
| Q-03 | Counterfeit-closure detection | V-5 Gap 2 | No clean heuristic. Defer; revisit if empirical evidence of counterfeit closure accumulates. Candidate signals: empty-diff commits, formatter-only diffs, automated-merge-only commits. |
| Q-04 | `:review-by` self-decay enforcement loop | V-4 ⚠ / V-5 Gap 3 | Check-fn must read disposition file each fire and compare against `now`. Implementation detail for INSTANTIATE Block 4. |
| Q-05 | Disposition opt-out granularity | V-4 schema | Path globs covered. Line-range opt-out probably out of scope; document the boundary. |
| Q-06 | Mission name reconciliation | ARGUE-1 / Q-naming | Working name `M-bounded-in-flight-state`. ARGUE held it; revisit at DOCUMENT — does the name still fit after the broader mana-allostasis frame landed? |
| Q-07 | Queue entry reconciliation | INSTANTIATE handoff | `run-068-commit-as-you-go--working-tree-commit-pressure` carries research-phase name. Once mission is operational, update `futon5a/data/stack-stereolithography-runbook.edn` (the source-of-truth) so the queue regenerates with the operational name + cross-reference to `run-064-mana-gated-work`. |
| Q-08 | Inventory entry shape | INSTANTIATE Block 4 | New family in `futon3c/docs/structural-law-inventory.sexp` should reference the harmolodic reread (declare bounded-disposition siblings as single-tier instances under metabolic-balance). The reread is operationally inert; making it explicit is a docbook-level statement. |
| Q-09 | Plain-language argument re-test | ARGUE plain-language | Have a non-stack-internal reader (or "outside-reader" simulation) actually test the elevator pitch. The exit criterion in `mission-lifecycle.md` is "someone outside the project can understand from the plain-language argument alone" — needs an actual outsider check. |
| Q-10 | Music-theoretic terminology grounding | DERIVE patterns | "Harmolodics" used without an in-library anchor. Either add a brief flexiarg (e.g., `library/music-theory/harmolodics.flexiarg`) or pre-define the term within the patterns that use it. |
| Q-11 | Free-jazz framing as methodology | INSTANTIATE form | Head-improvisation-head structure is ad-hoc here. Could be generalised as a mission-section template, possibly under `library/futon-theory/` or `library/structure/`. Defer until used a second time. |
| Q-12 | Session-id mechanics | V-6 amendment (II) / INSTANTIATE Block 5 | Session as temporal frame is named; session-id formation/persistence is not. `nonstarter_mana.clj`'s `--session` is the existing mechanism; needs a documented binding. |
| Q-13 | 5-phase lifecycle ↔ 8-phase futonic-loop mapping | DERIVE Pattern 1 | The five-phase block lifecycle maps onto the futonic loop's eight elements but isn't 1:1. Pattern names the mapping in prose; a side-by-side table would help future readers. |
| Q-14 | Empirical re-calibration | V-1 / INSTANTIATE Block 2 | Today's nominal `N=20`, `D=7d`, `B=10MB` chosen from a single empirical run. After first INSTANTIATE produces more data points, revisit. |
| Q-15 | Cross-mission back-references | mission text throughout | Mission references `M-the-futon-stack`, `M-live-geometric-stack`, `M-single-entry-point`, `M-commit-as-you-go`. Each target should accept a back-reference (a "see M-bounded-in-flight-state" line in their relevant section). |

**From the surrounding work that prompted this mission:**

| # | Item | Origin | Notes |
|---|---|---|---|
| Q-16 | futon7 daily.clj refers to moved data path | 2026-05-03 cleanup | `src/f7/daily.clj` reads from `data/probes/*.edn` after we moved the dir to storage. Captured for the M-commit-as-you-go log; needs a separate scoped fix when next exercised. |
| Q-17 | The 6 entangled futon3c dev/* M's | 2026-05-03 cleanup | `dev.clj`, `dev/agents.clj`, `dev/bootstrap.clj`, `dev/config.clj`, `dev/invoke.clj`, `dev/peripheral_agents.clj` — operator WIP. The disposition surface candidate (V-4 synthetic .futon-disposition.edn) addresses this; once the disposition mechanism is operational, these gain a real `:review-by`. |
| Q-18 | futon0/web/ war-machine source tree | 2026-05-03 cleanup / M-single-entry-point | 125 MB / 2,825 files untracked. Deferred to M-single-entry-point. The disposition surface should carry the `:review-by` "M-single-entry-point INSTANTIATE complete" entry. |

**DOCUMENT-time QA pass produces:** for each item, one of `:resolved
{...}`, `:deferred {to "..." reason "..."}`, `:dismissed {reason "..."}`.
The QA-pass record itself becomes a docbook entry capturing the
mission's known-knowns and known-unknowns at close.

#### DOCUMENT exit criterion

Mission COMPLETE when:
1. Docbook entry exists, self-contained, readable without the mission doc.
2. M-the-futon-stack §"What invariantly exists today" reflects 10th shape.
3. QA-items list above has been walked and recorded as resolved /
   deferred / dismissed.
4. Cross-mission back-references accepted.

## Checkpoints

### 2026-05-03 — IDENTIFY opened

**What's now true that wasn't before:**

- The "commit as you go" intuition has been precisely reframed as transactional discipline over file-system-backed storage (TM-on-FS). The framing makes the invariant's *systemic* role legible: this is what M-live-geometric-stack's substrate-semantics presupposes, not a tooling preference layered on top.
- The empirical input is gathered (40 commits, 18 findings) and the founding artefact is committed (`holes/labs/M-commit-as-you-go/cleanup-2026-05-03-futon6-data-leak.md`).
- Seven open questions are named precisely enough to be each derivable in turn.
- The differentiation from jj and pijul is clear and on the record (this is not VCS automation, not change theory; it is invariant discipline above git).

**What's not yet true:**

- The invariant's *shape* (Q-shape) is not yet decided.
- The check-fn is not written.
- M-the-futon-stack's nine-shape skeleton has not yet been updated to include or extend for this invariant.
- The candidate-queue entry (`run-068`) still carries the research-phase name.

**Risk register state:** all five entries above are *live*. The premature-shape-commitment risk is the most immediate; held in check by keeping both candidates open through MAP.

**Next-move:** open MAP. First derivation: Q-shape (10th cognitive faculty vs extension of `bounded-disposition`). The choice cascades into pressure-function form, sibling placement, and inventory entry shape.

### 2026-05-03 — MAP-1 + MAP-2 folded in (block + mana economy)

**What's now true that wasn't before:**

- The structural concept of the **block** (operator-attestable transactional unit) is named and folded into MAP. Five cadences enumerated (PSR, session, day, mission-phase, excursion); pressure dissipates at any of them.
- **Block-as-hinge-encounter** anchored in `library/structure/cook-ting` and `library/structure/hinge-point`. The invariant *reveals* hinges; it does not manufacture them. Stop-the-line tier fires on a *missed* hinge, not on a force-the-cut threshold.
- The **mana economy** named: block-closures award, allostatic-load symptoms drain, balance trajectory is the load-bearing signal. Working-tree dirtiness is one symptom-channel; mission-proliferation, session-sprawl, untriaged-evidence, ahead-of-origin are sibling drain channels (deferred).
- The mission scope holds steady: still working-tree first, but the broader frame is now in IDENTIFY/MAP text rather than spawning a parent mission (per "proliferation costs attention").
- Q-shape provisionally moves from `accumulating-drive` to `metabolic-balance` (faculty: *allostatic homeostasis*) — both award and drain, not just accumulation. Existing `bounded-disposition` siblings reread as single-tier drain channels under the unified read.
- Q-pressure-function reframed as mana-flow: `balance = ∫awards − ∫drains`, per-channel Process-C modulating each drain.

**What's not yet true:**

- ARGUE has not ratified the `metabolic-balance` shape against the bounded-disposition-extended alternative.
- The pressure function's specific form is not chosen.
- The disposition surface is not selected (`.futon-disposition.edn` vs `git notes` vs futon1a evidence).
- The block-id mechanism (commit footer vs `git notes` vs sidecar) is not chosen.
- No flexiarg yet captures the unified read of bounded-disposition siblings as drain-channels under the same faculty.

**Risk register state:** the *premature-shape-commitment* risk has shifted — the shape moved from `accumulating-drive` to `metabolic-balance` over one MAP turn, suggesting the frame is still fluid. Mitigation: ARGUE phase explicitly tests the reread before INSTANTIATE locks anything.

**Next-move:** DERIVE on Q-pressure-function form, with today's empirical run (40 commits / 18 findings) as the calibration target. In parallel, draft the unified-read flexiarg (a new pattern under `library/invariant-coherence/`) for ARGUE input.
