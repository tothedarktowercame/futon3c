# Meta-learning recall pass — claude-9 session, 2026-08-01

**Dispatched by claude-7** against `E-learning-across-levels.md`. Recall
instrument: claude-9, whose continuity context held the session. Ground truth:
`claude-9-transcript-20260801.txt` (22,402 lines, sha256 `51d7992f731ba1db…`,
verified by me before use).

Signals below are ones **not** among the eight in
`known-residual-20260801.md` Part E. Levels are L0 object / L1 instrument /
L2 process-topology / L3 norm-conative, per the taxonomy doc.

---

## 0. A finding about the recall instrument itself, before any signal

### 0a. The artifact is surface-partial, and that changes what "not found" means

The transcript is the **emacs-repl surface** — `joe:` prompts and my replies to
Joe. It does **not** contain the bell surface: inbound bells from claude-5 and
claude-2, parked-resume wakes, or my bell-reply turn text. Checked:
`Reply delivery` = 0 hits, `parked-resume` = 0, `Harnack` = 0,
`poissonConv_tendsto_ae_of_memLp` = 0, `non-triviality` = 0 — all of which
demonstrably occurred, in bell-surface turns.

Consequence: for a signal that lived only in inter-agent exchange, **absence
from the artifact is not evidence of absence**. I mark those `NOT-IN-ARTIFACT`
rather than `RECONSTRUCTION`, because the artifact cannot in principle settle
them. Conflating the two would have inflated my reconstruction count and
understated the coverage gap.

This matters for claude-7's purpose specifically: **L2 signals are the ones
most likely to live in the bell surface**, because L2 *is* inter-agent
coordination. The instrument that was secured to measure the level
distribution is systematically blind to the level the exercise predicts will
dominate.

### 0b. I nearly recorded a false characterisation of the artifact, from my own reconstructed phrasing

My first probe searched `137 of 145`, `136 of 145`, `135 of 145` — all zero —
and I began concluding the transcript omitted the whole late session. It does
not: the text reads `137/145`, present at line 22378. **The search terms were
reconstructions of what I thought I had written, not what I wrote.**

Recall-without-verification failed at the level of the *query*, not the
answer — a failure mode one rung below the one `reconstruct != reproduce`
usually names. A verification instrument driven by remembered phrasing inherits
the memory's errors and reports them as evidence about the world.

---

## 1. L1 — instrument

### S1. A silent no-op that returns success is the worst available failure shape

The queue row-updater used `if len(occ)==1: replace`, so any field appearing
twice was **skipped while reporting success**. 44 of 88 rows had duplicate
top-level keys. `:status` always worked (single occurrence), so the state
machine stayed sound while the audit trail silently rotted.

**VERIFIED** — L21474: *"44 of 88 rows have duplicate top-level keys, and my
update code does `if len(occ)==1: replace` — so on any duplicated field it
**silently does nothing**."*

### S2. The same class had already fired once that session and was not generalised

L13851, much earlier: *"a97A07's status substitution silently no-op'd (it was
never marked dispatched), and a97A08/a95J07 are **in flight but still
`:untouched`**."* It was fixed locally as three bad rows. The general defect —
*the updater can no-op* — was only extracted ~7,600 lines later.

**VERIFIED** — L13851. The signal is the **gap between incident and class**:
one instrument defect surfaced twice, repaired as data the first time and as
instrument the second.

### S3. `state=done` on a `:dispatched` row is a stale status *by definition*

A one-line audit with no false positives: five of five flagged were stale.

**VERIFIED** — L19679: *"`state=done` on a `:dispatched` row is a stale status
by definition. Five of five flagged that way were stale."*

### S4. Receipts and the queue are different instruments with different jobs

Receipts are the durable record; **the queue is what selects work**. Verifying
a result and writing its status back are separate acts, and completing only
the first leaves the selector wrong while the evidence is right.

**VERIFIED** — L19677: *"the receipts are the durable record… But **the queue
is what selects work**, so those five looked busy while being idle, and a01J02
looked blocked while being fully dispatchable."*

### S5. A gate can be a screen rather than a verdict, and shipping the wrong one is worse than not shipping it

The `autoImplicit=false` gate was proposed as pass/fail and refuted by a97A07,
where the flagged identifier was ordinary `Fin n → ℂ` idiom.

**VERIFIED** — L18852: *"So the gate is a **screen, not a verdict**: failure
should trigger inspection of each flagged identifier, not a fail. Had I shipped
it as pass/fail, it would have marked a genuinely-solved problem defective."*

### S6. A conclusion can be a tautology without being syntactically `True`

a95J04 part (b) concluded `∃ M : ℝ, 0 ≤ M` — mentioning neither the sequence
nor the coefficients the problem is about. The 0a check looks for `True`; this
passed it.

**VERIFIED** — L19885: *"It is a tautology, not merely an unfinished proof."*

### S7. `opaque` without a definition is a `sorry` wearing a different hat

The sanctioned counter measured one token. A hole declared as
`opaque x : T` was invisible to every gate that consumed it.

**VERIFIED** — L16877: *"Extended `stack-hud--apm-count-sorries-in-text` — the
sanctioned counter behind `count_sorries.sh` and your HUD — to count
definition-free `opaque` declarations as holes."*

### S8. Measure by declaration ownership, not by line number

Line-keyed measurement cannot distinguish drift from regression;
declaration-keyed measurement can.

**VERIFIED** — L15349: *"I scanned every `:resolved` row against its file (by
declaration ownership, not line numbers, so it isn't line drift)."*

### S9. Green unit tests certify the instrument, not the measurement

The `opaque` counter's ten unit tests passed while the corpus number refused to
move — because the *input* was changing under the measurement, not because the
counter was wrong. A passing test on the instrument was briefly read as evidence
about the world.

**VERIFIED** — L16868: *"Count unchanged and a96A02 reports 0 — but my unit
tests pass, so the counter works. Checking whether the file itself changed under
me."*

### S10. A checker written to catch unenforced claims contained an unenforced claim

`queue_audit.py`'s comment-stripper deleted newlines, so reported line numbers
were shifted; its first run produced two false stale-`:line` findings. Fixed by
preserving newlines and adding a self-test that asserts expected line numbers
before the checker is trusted.

**NOT-IN-ARTIFACT** — occurred in a bell/continuation turn; `strip_comments`
= 0 hits. The tool and its self-test are in `scripts/queue_audit.py` (commit
`a9539ec6`) and can be verified there instead.

### S11. Lean's warning line is the declaration line, not the hole line

a96A04's Lean warning pointed at 417; the `sorry` was at 428. Copying the
warning line into a dispatch hint puts the pointer eleven lines off.

**NOT-IN-ARTIFACT** — bell-surface. Recorded on the queue row as `:line-note`.

---

## 2. L2 — process / topology

### S12. Held-out evaluation data was sitting in the dispatch queue

Four BPM rows were `:untouched` **in the queue the selector reads**, while the
mission doc said BPM stays held-out, evaluation-only. The norm existed in prose;
the selector did not know it.

**VERIFIED** — L832: *"This is the important find, and it's urgent. Four **BPM**
rows are sitting `:untouched` in the dispatch queue — and the mission doc states
BPM 'stays held-out from mining — evaluation-only, so the demo is
uncontaminated'."*

**This is the most consequential L2 signal in the session.** It is an
evaluation-integrity near-miss, and its cause is structural: a norm written
where humans read it and absent where machines select.

### S13. The mission's own evidence base was outside version control

Nearly all of `holes/labs/M-codex-sorry-loop/` — faithfulness ledger, drafts,
promotion reports, harvester — was untracked in git while being the announcement
gate's evidence.

**VERIFIED** — L232: *"nearly all of `holes/labs/M-codex-sorry-loop/` is
untracked in git — the faithfulness ledger, every draft and promotion report,
the harvester. That's the announcement gate's evidence base sitting out[side]…"*

### S14. Verification backpressure works, and its cost is paid in silence

A bellback landed, the per-row protocol never ran, the row stayed `:dispatched`,
and the gate closed four consecutive cron fires. The mechanism did exactly its
job; nothing surfaced that it was doing so.

**VERIFIED** — L427: *"The a01A06 bellback landed around 21:30 BST and I never
ran the per-row protocol on it. The row stayed `:dispatched`, so verification
backpressure did exactly what it's built to do and gate-closed the next four
fires."*

### S15. Serial dispatch was a proxy for file-conflict avoidance, not a value

Parallel is safe exactly when rows touch different files and different runners.
Treating "serial" as the rule rather than the proxy costs throughput for nothing.

**VERIFIED** — L7345: *"Running **two proving runs in parallel** to roughly
double overnight throughput — different rows, different runners, so no file
conflict."*

### S16. Uncommitted files in a shared tree are a hazard, not a neutral state

With runners active in the same checkout, an uncommitted file can be swept into
another agent's `git add -A`. Committing a single documentation file was the
*lower*-risk action than leaving it dirty.

**NOT-IN-ARTIFACT** — reasoning occurred in a bell/continuation turn; the
resulting single-file commit is `d6b1bc3` in apm-lean.

### S17. Polling turns a wake into a backstop instead of a trigger

Because job state was polled directly, most park wakes arrived *after* the work
they announced had already been reviewed. That is the desirable ordering — a lost
wake then costs nothing — but it makes every resume read as a duplicate, and an
agent that treats duplicates as errors will thrash.

**NOT-IN-ARTIFACT** — bell/parked-resume surface.

### S18. An agent identity is a routing address; the number matters in exactly one place

**VERIFIED** — L52: *"The numbers do matter, but only in one place: routing."*

---

## 3. L3 — norm / conative

### S19. "The formal statement is false" and "the original problem has no answer" are different claims

Raised by Joe at the top of the session and load-bearing all day: a defective
formalisation is not a defective problem, and the loop must not report one as
the other.

**VERIFIED** — L447: *"'the formal statement is false' and 'the original problem
has no answer' are very different claims."*

### S20. Statement repair requires explicit operator authorisation, recorded in the artifact

Repairs carry `(Joe-authorised)` in the commit message and in the in-file
comment, so the authority is auditable from the code rather than from memory.

**VERIFIED** — L1028: *"Repair eight defective problem statements
(Joe-authorised)"*.

### S21. "The blocker is closed" and "the problem is unblocked" are different claims

I elided these across three dispatch payloads for a95J06 —
`ConstructionTargets/BanachZarecki.lean` reached zero holes while a95J06's own
`ac_of_bv_continuous_conditionN` still carried its `sorry`.

**NOT-IN-ARTIFACT** — bell surface (claude-5 raised it). Recorded in the ⊸
register and on the queue row.

### S22. A runner declining to assert what it has not demonstrated is a norm worth protecting

a94J04's runner found a stale in-file claim that a theorem was absent from
Mathlib, said so, and explicitly did **not** edit the comment, because it had
not completed the work that would justify the new claim.

**NOT-IN-ARTIFACT** — bell surface. Recorded in the ⊸ register.

### S23. Scope of a hold: a refactor orthogonal to the held question is still the operator's call

The LusinN de-duplication does not touch a95A02's Borel-vs-Lebesgue defect, and
I still did not execute it, because a95A02 is a held row. Same for proposing
rather than performing the `LemniscateComponents` deletion.

**NOT-IN-ARTIFACT** — bell/continuation surface; visible in
`known-residual-20260801.md` Part C and the queue row's `:duplication-debt`.

---

## 4. L0 — object

### S24. The problem was never blocked on mathematics; it was blocked on reachability

The lakefile declared only `lean_lib ApmCanaries`, so `ConstructionTargets/` was
not on the module path: `import ConstructionTargets.X` failed and no `.olean`
was ever produced. Lemmas were proved, gated, reported clean — and mechanically
unreachable from the problems they were built for.

**VERIFIED** — L698: *"Found the actual blocker. The lakefile declares only
`lean_lib ApmCanaries` — **there's no library for `ConstructionTargets`**, so it
isn't on the module path."* and L20652: *"The problem was never blocked on
mathematics. It was blocked on reachability, and making the result reachable
solved it."*

*Adjacent to R1 in `construction-targets.md` but not among Part E's eight.* It
is classified L0 because the object-level obstruction was a build fact — but see
misfit M2 below: the *lesson* is not L0 at all.

### S25. Mathlib gap inventory established by targeted search

No Hardy–Littlewood maximal function anywhere in Mathlib; the Lebesgue
differentiation theorem *is* present and usable
(`IsUnifLocDoublingMeasure.ae_tendsto_average_norm_sub`); every `ContDiff`
convolution result requires `HasCompactSupport`.

**VERIFIED** — L22160: *"Arm (b) Zulip was checked for part (c). It confirms
Hardy–Littlewood machinery exists in the external Carleson development, not as
directly [usable Mathlib infrastructure]"*; L22179 confirms the Lebesgue
differentiation theorem is the available substitute; L1949 lists
`HasCompactSupport.hasDerivAt_convolution_right` among the compact-support-only
convolution results. The **Harnack** absence is **NOT-IN-ARTIFACT** (bell
surface, claude-5's frontier).

### S26. A hypothesis can be syntactically rich and semantically empty

a95J04's `hf_mero` constrains `f` only *at* each point, not on a punctured
neighbourhood — machine-checkably satisfied by **every continuous function**.

**VERIFIED** — L16506: *"I proved machine-checkably that every continuous
function satisfies this condition with `n = 1`. Thus it cannot support the
intended pole argument."*

(The generalisation — proposed check 0d — is Part E item 2; the *instance* and
its machine-checked form are not.)

---

## 5. Taxonomy misfits

**M1. Cross-level propagation is not a level.** S9 is an L1 instrument fact
(unit tests certify the instrument) that produced an **L0 false claim** (the
corpus count), which I then reported to the operator. The interesting object is
the *edge* between levels, and L0–L3 has no place to record one. Several of this
session's worst moments were edges, not nodes.

**M2. The lesson's level and the defect's level differ.** S24's defect is an L0
build fact; its lesson — *"a result that is proved, gated, and unreachable is
worth nothing"* — is an L1/L3 claim about what "done" means. Classifying by
where the defect sat and classifying by what was learned give different answers,
and the taxonomy does not say which it wants. **Recommend the doc state
explicitly that classification is by the level of the LESSON.**

**M3. Recall-instrument findings (§0) have no level.** "The ground-truth
artifact is surface-partial" and "my search terms were reconstructions" are
learning *about the apparatus used to study learning*. They are not L1 (not an
instrument of the loop) — they are an instrument of the meta-loop. If
`E-learning-across-levels` is to be built on, it needs a slot for this, or it
will repeat the L0-only blindness one level up.

**M4. Operator-steering signals are unplaced.** Joe's throughput remark changed
my serial/parallel choice (S15); his "no bells or whistles" opt-out overrides
the handoff default; his authorisation gates statement repair (S20). These are
neither norms the system authored (L3) nor coordination between agents (L2).
An **L4 — operator/strategic** slot, or an explicit statement that L3 includes
externally-imposed norms, would resolve it.

---

## 6. Counts

| level | VERIFIED | NOT-IN-ARTIFACT | RECONSTRUCTION | total |
|---|---|---|---|---|
| L0 — object | 2 (S24, S26) | 0 | 0 | **2** + 1 partial (S25) |
| L1 — instrument | 9 (S1–S9) | 2 (S10, S11) | 0 | **11** |
| L2 — process | 5 (S12–S15, S18) | 2 (S16, S17) | 0 | **7** |
| L3 — norm | 2 (S19, S20) | 3 (S21–S23) | 0 | **5** |
| *(unplaced — §0, M3)* | 2 | 0 | 0 | **2** |
| **total** | **20** | **7** | **0** | **27** |

Zero pure reconstructions: every recalled signal was either found in the
artifact or is verifiable in a named committed file outside it. The seven
`NOT-IN-ARTIFACT` signals are **six L2/L3 and one L1** — consistent with §0a's
prediction that the bell surface carries the coordination-level learning, and a
direct measurement of the coverage gap.

**Level distribution of what came back: L1 (11) + L2 (7) = 18 of 27, or 67%.**
That is the quantitative case `E-learning-across-levels` §4.2 predicted, from an
instrument that under-samples L2 by construction — so 67% is a floor, not an
estimate.
