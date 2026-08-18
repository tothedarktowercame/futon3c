# E-APM-f11-defects — defect inventory for the APM demonstration, frame 11 onward

**Opened 2026-08-18 by claude-2 (ground control, M-apm-demonstration) at Joe's
direction**, while frame-11 was running: *"Lets save the defects in
E-APM-f11-defects.md so that we don't forget them."*

**Continues `E-APM-f10-defects.md`, which covers everything through frame 10.**
Numbering continues rather than restarting: D1–D26 live in the f10 file and are
referenced by number in commit messages, park payloads and the series entries,
so restarting at D1 here would silently collide with those references.

Same conventions as the f10 file, and they are load-bearing:

- **[verified]** — re-derived at source by ground control.
- **[reported]** — taken from an agent's report and not independently
  re-derived. Several entries in the f10 file were first reported with a
  mechanism that turned out to be half right, which is why the distinction is
  kept.

Frame 11 is the first frame to run on the repaired write path (D1–D10, D24, D25
all fixed and gated). The defects below are what that frame exposed — including
two that are ground control's own, and one residual gap in a fix from the
previous round.

---

### D27. A registration whose predictions omit the objective **[verified, mine]**

Ground control registered f11 with **eight predictions, none of which concerned
solving the problem.** All eight were about plumbing: reviewed attachments,
disposition population, guidance count, card resolution, cascade routing,
refusal traceability, analyst tenure.

Joe caught it: *"what do you mean 'not the close'? You're going off map."*

**The error.** The corpus has ~207 open problems and the machine is being built
to go through them. The memory apparatus — cascade, promotion, disposition — is
**instrumental to that**. Registering only the instruments makes the instrument
the objective, and produces a frame where *failing to solve the problem while
populating a disposition field reads as success.* That is precisely backwards.

It is the same family as D17 (a prediction's premise computed over the wrong
set), but worse: there the premise was wrong, here the subject was.

**What was and was not lost in f11.** The close is still *measured* — 
`:decision-rule` outcomes are `[:closed :tier-a :tier-b :defective]` and the
required measurement fields include "terminal disposition", "residual executable
sorries", "attempts or closer hops" and "axiom cleanliness". So the primary
result is recorded and will be reported. What is missing is a **pre-registered
expectation** about it, which is weaker: the close becomes a measurement rather
than an adjudicated prediction.

**Why f11 was not re-registered.** The solver was already dispatched and working
when this was caught (`:dispatch-solver` recorded at v10). A registration is
frozen at open, and editing one mid-frame would break the freeze property the
whole experiment depends on — the property that stops predictions being fitted
to results. Correcting the specification would have cost the solver's live work
and the integrity of the freeze. f11 runs as registered, with this defect
recorded against it.

**Fixed in f12** (unopened, so legitimately editable), which now leads with:
- `:problem-closed` — the primary outcome, stated as such;
- `:memory-contributes-to-close` — a surfaced memory attested as USED in the
  work that closes or advances the problem, which is the link that makes the
  apparatus worth having;
- everything else explicitly ranked below, with `:reviewed-attachment-gained`
  relabelled "SUPPORTING, NOT HEADLINE".

**The standing rule:** every frame registration must predict the close first.
Instrument predictions are how the close is expected to be *achieved*, and are
ranked beneath it. A frame that repairs its own plumbing and does not solve its
problem has failed, however green its other measures read.

### D28. `:write-use` is reachable but order-dependent, and fails silently **[verified]**

Found by f11's guide **by reading, before it could bite**, and confirmed at
source by ground control. This is a residual gap in D25's fix.

- `:write-use` is legal in exactly one phase: `problem.clj:56`,
  `:adjudicate #{:write-disposition :write-use advance}`.
- `conductor/adjudicate!` (`conductor.clj:627`) does `write-disposition` and
  then **`(advance h1 {})` at line 632** — advancing out of `:adjudicate`.

So a guide that does the natural thing — adjudicate the cycle, then attend to
dispositions — finds the only window for `:write-use` already closed, gets no
error, and leaves `:memory-disposition-offer-ids` empty exactly as f8, f9 and
f10 did. **D25 made the operation reachable; it did not make it reachable at a
time a guide would naturally call it.**

f11's guide caught this while planning and sequenced `write-use` BEFORE
`adjudicate`. The next guide may not. Options, smallest first: have
`adjudicate!` disposition recorded offers itself before advancing; or refuse to
advance with undispositioned offers; or expose the ordering constraint in the
card. Not fixed here — f11 is live and the harness is frozen for its duration.

### D29. f11 cannot test the cascade predictions: the store has no reviewed attachment to surface **[verified]**

f11's dispatch recall returned `dispatch-recall-outcome=completed-empty`, and the
saved state confirms `recall-status :recall-empty`, `eligible-memory-ids []`,
`surfaced-ids []`, and — by structural walk deduped on `:offer/id` — **0 offers**.

Why: f10 deposited and "promoted" `e-17bd0295`, which still exists (8,168 bytes),
but D3/D4 prevented the review transition from completing, so its attachment edge
never became reviewed. Recall surfaces *reviewed* attachments. The store
therefore holds memories that are not reachable as reviewed attachments.

**Consequence for adjudication, which the Analyst must not misread.** Two of
f11's registered predictions are untestable in this frame's actual conditions:

- `:offer-disposition-populated` — there are no offers to disposition. An empty
  `:memory-disposition-offer-ids` here means **INAPPLICABLE**, not refuted, and
  specifically does NOT mean D25 failed.
- `:cascade-seeds-from-recall` — nothing was surfaced, so nothing seeded.
  INAPPLICABLE.

`:reviewed-attachment-gained` remains fully testable, and is now the sharper
question: **f11's job is to BOOTSTRAP.** It must deposit and complete a
promotion so that a reviewed attachment exists at all. Only then can f12 test
whether the cascade reaches it.

**This compounds D27.** Having registered predictions about the instruments
rather than the objective, ground control then failed to mark two of them
conditional — while explicitly marking two others conditional in the same
registration. The rule from D27 needs a second clause: **a prediction whose
precondition the frame itself must first create is a prediction about a LATER
frame.** f11 creates the store state that f12 can measure.

---

## Open at the time of writing (frame 11 still running)

Not defects yet — questions f11 will answer, listed so they are not lost if the
frame's own report is:

- **Does t01A05 close?** ANSWERED, and the answer is that **it cannot**: see
  the statement defect below.
- **Does `:reviewed-attachment-gained` hold?** This is the one memory-side
  prediction f11 CAN test (D29), and it is a bootstrap: the store currently has
  no reviewed attachment for recall to surface, so f11 must create the first one
  before f12 can measure whether the cascade reaches it.
- **Is the shipped `proof-outline.md` wrong in the two ways ground control
  found?** Four lemmas it claims are proved do not exist in `Main.lean`, and its
  central obstruction ("hypotheses omit `T2Space X`") is contradicted by the
  frozen statement, which carries `[T2Space X]` at line 86 and
  `[T2Space Xtilde]` at 89. Its *other* obstacles were NOT checked and may well
  stand — recorded that way deliberately, since inherited optimism would be as
  costly as the inherited pessimism that cost f10 three declined passes.

## Fix queue carried forward, none of it dispatched

- **D26** (in the f10 file) — the solver's attested use never reaches a machine
  receipt, measured in both f9 and f10. Still `[reported]`. Needs a discovery
  pass first: the D25 precedent makes "the receipt tool is unreachable from the
  solver's surface" the hypothesis worth checking before any fix is designed.
- **D28** — needs a ruling on which repair is right: have `adjudicate!`
  disposition recorded offers itself before advancing, refuse to advance with
  undispositioned offers, or state the ordering constraint in the guide card.
  The first is the most robust and the least honest about what the guide chose;
  the second is the most likely to strand a live frame. Not obvious, so not
  dispatched.
- **A gap in D1's fix** — `mint-seats` applies ONE model to all five seats
  (`frame_seats.clj` `mint-one!`), so a claude model cannot be set for a
  mixed-type frame; the two claude guides for f11 and f12 still needed
  re-registration by hand. Contrast D5's `:memory-domain`, which is per-seat in
  `seat-specs` and is the shape the model should have taken.


---

## STATEMENT DEFECT in t01A05 — verified by ground control

**This is f11's primary result, and it is not a close.** The frozen theorem
`apm_t01a05` is **false as stated**, so the problem is not merely unsolved — it
is unprovable.

f11's solver formalised the refutation as `t01A05_generalClause_isFalse`
(commit `396d4ee7`, branch `exp/frame-11-t01A05-solver`). Its hypothesis is the
frozen theorem's **second conjunct quoted verbatim** — identical typeclass
hypotheses and structure, differing only in the binder name `hk` → `_hk` — and
it concludes `→ False`.

**The counterexample.** Take `n = 0`, `k = 2`, `X` a point, `Xtilde` two points,
`π` the fold map. That is a genuine two-sheeted cover, and a 0-manifold is a
discrete set of points, so the instantiation is legal under the frozen
hypotheses. But upstairs `H₀ ≅ ℤ × ℤ`, which cannot admit the orientation
isomorphism to `ℤ` that `T01A05Orientation` requires. The clause promises a
lifted orientation that provably cannot exist.

**Root cause:** the frozen statement carries `[T2Space Xtilde]`,
`[CompactSpace Xtilde]`, `[ChartedSpace …]` and `[IsManifold …]` but **no
`[ConnectedSpace Xtilde]`**, so disconnected covers are admitted. Either that
hypothesis or a componentwise orientation definition is required.

**Verified by ground control, not accepted on report:**

| check | result |
|---|---|
| All five claimed commits | present |
| Frozen statement vs pin | **byte-identical** (20-line signature diffed) |
| Elaboration, re-run on a `/tmp` copy | `EXIT=0`, 0 errors — lean's own status, not a pipeline's |
| **Axioms of the refutation** | `[propext, Classical.choice, Quot.sound]` — **no `sorryAx`** |
| `t01A05_twoPointCover_isTwoSheeted` | same three axioms |
| Solver worktree | clean, untouched by the audit |

**Consequence.** The residual at `h_general_fundamental_class_multiple` is
structurally undischargeable; no amount of Mathlib API closes it. The other
residual, `h_torus_degree_multiple`, is a genuine API gap (no torus/circle
homology computation, Künneth, or covering-transfer degree theorem in Mathlib)
and is a different kind of open.

**Why this is a good outcome.** The frame text named a statement defect as "a
reportable result and not a failure to solve", and the registration carries
"statement defects at review" as a required measurement field and `:defective`
as a decision-rule outcome. For a corpus of ~207 open problems, a machine that
can *formally establish* that a problem is mis-stated is worth as much as one
that closes it — an unbounded search against a false statement is the most
expensive failure mode available.

It also vindicates the frame's instruction to treat a prior pass's conclusions
as evidence rather than verdict: the shipped `proof-outline.md` recorded this
residual as an API gap, and it is not one.
