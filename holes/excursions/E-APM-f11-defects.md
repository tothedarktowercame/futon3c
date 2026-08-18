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


---

## WHO IS TO BLAME — the original problem, or the formalisation? (Joe's question, answered)

**The downstream formalisation. The original problem is sound, standard, and
repairable.** Ground control read the source bundle to settle this.

### What the original problem asks (`problem.md`, raw TeX)

Given `π : X̃ → X` a `k`-sheeted covering where `X, X̃` are **compact oriented
manifolds of dimension n** and **π is orientation-preserving**:

- **(a)** construct such a cover for surfaces (`n = 2`), for any `k`;
- **(b)** show `∫_X̃ π*ω = k ∫_X ω` for any `ω ∈ Ωⁿ(X)`;
- **(c)** for compact oriented submanifolds `Z₁, Z₂ ⊂ X` with
  `dim Z₁ + dim Z₂ = n`, show `π⁻¹(Zᵢ)` are compact oriented submanifolds and
  `I_X̃(Z̃₁, Z̃₂) = k · I_X(Z₁, Z₂)`.

The informal solution confirms the intent: *"The orientation hypothesis is
exactly what makes each of the k local change-of-variables come with a + sign
rather than cancelling."* Orientations are **given data** throughout. The word
"connected" appears **nowhere** in the source — and it does not need to, because
the original never asks anyone to construct an orientation.

### Two distinct formalisation defects

**1. A hypothesis was converted into a conclusion.** The original *gives* an
oriented `X̃`. The frozen Lean instead demands
`∀ o : Orientation n X, ∃ ot : Orientation n X̃, FundamentalClassRelation π ot o`
— i.e. it asks the solver to **construct** the upstairs orientation. That is
what makes it false for disconnected covers: the original's oriented `X̃` is an
assumption that simply excludes the two-point counterexample, whereas the
formalisation invites it.

**2. The formalised content is not the original's content.** The original's
mathematical substance is an **integral identity** (b) and an
**intersection-number identity** (c), over differential forms and signed point
counts. The frozen theorem contains **neither**. It replaces both with an
orientation-lifting claim in top integral singular homology.

This also explains an earlier finding in this file: `proof-outline.md` claims
four lemmas are proved, including `t01A05_pullback_integral_eq` and
`t01A05_preimage_intersectionNumber_eq` — which are exactly (b) and (c). Those
names are absent from `Main.lean` because **an earlier formalisation did encode
the original content and the file was later rewritten to state something else.**
The outline is a surviving description of the version that was replaced.

### Recommended action: REPAIR, not discard

- **Do not discard.** The source problem is a standard, correct exercise.
- **The minimal repair — adding `[ConnectedSpace X̃]` — is not sufficient.** It
  would probably make the frozen statement *true*, but the statement would still
  not be this problem. It would leave (b) and (c) unformalised.
- **The correct repair** is to formalise (b) and (c) with orientation as a
  **hypothesis**, as the source has it — recovering the content the earlier
  version apparently had.
- This is an operator/design decision, not a solver's: it changes the frozen
  statement, so it belongs to whoever owns the problem bundle, not to a frame.

### The corpus-level implication, which is the reason this matters beyond t01A05

A formalisation drifted far enough from its source to state a **different and
false theorem**, and the drift survived into a frame as a frozen statement.
Nothing in the pipeline compared the Lean against the TeX. If it happened here
it can have happened elsewhere in the ~207 open problems, and the check is
cheap and mechanical: does the frozen theorem mention the objects the source
asks about? Here the source asks for integrals over `Ωⁿ` and intersection
numbers, and the frozen statement mentions neither.

**This is a more valuable finding than the counterexample itself.** The
counterexample says one problem is broken; this says the pipeline can produce
broken problems without noticing, and suggests a specific audit.


---

## D30. The harness pin check verifies the source tree, not the running image **[verified, mine]**

f11's registration declares `:reg/harness-revision 01fb2de0`. The pin check at
open (`conductor_open.clj` `harness-pin-check`) compares that against
`problem/measure-harness-repository`, i.e. against **git**. It passed.

**The running JVM does not have 01fb2de0.** It was restarted at 15:39:49Z, and
three commits landed after:

| commit | time | in process? |
|---|---|---|
| 7c93a9ea D25/D7 | 14:56:28Z | yes |
| c3be0e9f D3/D4 promotion | 15:09:36Z | yes |
| 38d75981 D5 student domain | 15:12:04Z | yes |
| 585a980e D6 refusal receipts | 15:19:28Z | yes |
| eb81db29 D24 handler | 15:24:35Z | yes |
| **73a6b0c2 D2 guidance count** | 15:49:37Z | **NO** |
| **ca0f297e D8 scribe card** | 15:50:42Z | **NO** |
| **01fb2de0 D9/D10 provisioning** | 15:51:47Z | **NO** |

So **a frame can declare and pass a harness revision it is not running.** The
pin's whole purpose is to make a frame's code state reproducible, and it verifies
the wrong artefact.

**This is ground control's error compounding a machine gap.** I told Joe the
source was ready to restart, he restarted, and I then dispatched D2/D8/D9/D10 —
the four I had earlier forgotten — which landed afterwards. I then pinned f11 at
the source head without checking whether the process had it. The machine let me.

**Detected by f11's guide**, from evidence rather than suspicion: it saw
`:dispatch-scribe` record `role-cards/scribe.md`, recognised it as the verbatim
pre-`ca0f297e` constant, and inferred the loaded conductor predated that commit.
It then probed `write-use` to establish that 7c93a9ea *was* loaded, bounding the
gap from both sides.

**Consequences for f11's predictions, which must be reported as reload-gap
outcomes and NOT as source defects:**

- `:scribe-card-pinned-resolves` — **REFUTED IN THE LIVE CYCLE.** D8 is correct
  in source and absent from the process.
- `:guidance-count-non-negative` — will pass **VACUOUSLY**. D2 is not loaded, so
  the old per-offer subtrahend is live; but recall was empty, so
  `(count (:memory-offers trace))` is 0 and the broken subtraction cannot
  manifest. A pass here is *not* evidence that D2 works. Report it as vacuous.

**Fix candidates:** have the pin check consult the running image (e.g. a
revision stamped into the process at load), or refuse to open when the loaded
revision is unknown. Either is better than verifying git and calling it the
harness.

## D31. `:verdict` cannot be expressed over the conductor surface **[reported by f11-guide, source-level]**

`memory-lifecycle/review-attachment!` requires `verdict` to be a **keyword** in
`#{:approve :challenge :reject}`, but `conductor-surface/transport-args` decodes
only two closed enums (`:adjudicate :outcome`, `:record-scribe-lanes :lane`) —
its own comment says "the one closed enum". JSON has no keyword type, so a live
guide sending `:verdict "approve"` over HTTP hands a **string** to a function
requiring a keyword, and it surfaces as `:promotion-attachment-review-threw`.

**So the D3/D4-repaired promotion path is unreachable from a live guide** — the
same shape as D25 one layer up: the operation is correct, and the transport
cannot express its argument. Fix is one coercion beside the existing two.

Not verified at source by ground control, and **deliberately not fixed
mid-frame** (see the ruling below).

## D32. A cycle affords exactly one guide deposit, taken before any review exists **[reported by f11-guide, structural]**

`:write-substrate` is added only to the `:intervene` phase, phases are linear,
and `conductor/deposit!` advances out of `:intervene` on every call. Therefore a
guide gets **exactly one deposit per cycle, and it must be taken before any
review of it can exist.**

Consequence: **review feedback can never be acted on within the cycle.** When
f11's scribe rejected the guide's deposit and noted that "a cleaned revision
would merit resubmission", that was mechanically impossible. **Any frame whose
review rejects gains zero attachments by construction** — which also explains why
f10 deposited a single mega-memory rather than several targeted ones.

This is the structural reason `:reviewed-attachment-gained` can fail in a frame
where every repaired component worked correctly.

## The review gate worked, and that is the finding

f11's guide deposited `e-a39ff1b3` (pattern
`math-strategy/structural-obstruction-as-theorem`, edge `:proposed`, domain
`:mathematics` — exactly the authoring shape D4 and D5 were repaired to accept).
`f11-scribe` re-ran the elaboration, re-ran `#print axioms`, compared the
refutation binder-for-binder, and **rejected it** because the body named a commit
sha, which the guide's own dispatch had told it to reject.

**So `:reviewed-attachment-gained` is failing for an honest reason: an
independent reviewer read the work and said no.** Not because of D3, D4 or D5 —
authoring produced precisely the edge those repairs were built to accept. The
gate is real, and author != reviewer held under a case where the author would
have preferred otherwise.

The guide logged its own error rather than appealing: its dispatch wording was
stricter than its content needed, since a base-revision pin is *provenance* that
an absence claim requires ("empty at this pin"), not a pointer to a solution.


---

## A verdict dispute, settled against the guide — `:cascade-seeds-from-recall`

f11's guide reported this prediction as **corroborated**: *"nothing surfaced,
nothing seeded, even though the problem touches patterns."* Ground control had
recorded it as **INAPPLICABLE** (D29). The guide's reading is the more
interesting one, so it was checked rather than overruled.

**The premise does not hold, or at least is not established:**

- `grep -rc 't01A05' futon3/library/` — **zero** files reference the problem.
- `:offer/patterns-per-problem` was **never recorded in any version** of the
  cycle, so the touch-set size is nowhere established. (In f10 it was recorded as
  2.)
- The store entries carrying subject `t01A05` are recall *observations*
  (`:recall-status :not-invoked`, `:memory-channel-no-push`), not pattern
  attachments.

**Why this matters logically.** The prediction discriminates between two
hypotheses: seeding follows the problem's touch-set, versus seeding follows
recall-surfaced memories. It is corroborated only if the touch-set is NON-EMPTY
while offers are empty — that is the whole discriminating power. If the touch-set
is empty, zero offers is what BOTH hypotheses predict, and the frame separates
them not at all.

So the verdict is **INAPPLICABLE**, and specifically *not* corroborated. The
guide asserted a premise it had not measured; the datum that would have supported
it is exactly the one the cycle never recorded.

This is worth keeping as a worked example rather than a correction: the guide was
reasoning correctly about what would count as evidence, and reached for a
premise that felt safe. **f10 is where that premise came from** — there the
problem did touch patterns (2 of them), and the contrast with an empty touch-set
was never a live consideration. The error is generalising a measured fact from
one frame into an assumed fact in the next.


---

## The t01A05 history is worse than ground control diagnosed: vacuous, then false

codex-6's repair-plan discovery (`c109baa7`,
`holes/excursions/E-t01A05-repair-plan.md`) **corrects** the blame analysis
above, and the correction matters.

Ground control wrote that "an earlier formalisation encoded the right content and
the file was later rewritten to state something else", inferring this from
`proof-outline.md` naming `t01A05_pullback_integral_eq` and
`t01A05_preimage_intersectionNumber_eq` — the original's (b) and (c).

**The earlier formalisation encoded the right NAMES and vacuous CONTENT.**
Verified by ground control at revision `b9c6503` (also present at `f84008b`):

```lean
def t01A05IntersectionNumber {n d₁ d₂ : ℕ} {X : TopCat}
    (Z₁ : T01A05OrientedSubmanifold d₁ X)
    (Z₂ : T01A05OrientedSubmanifold d₂ X) : ℤ := 0

def t01A05Integral {n : ℕ} {X : TopCat} (ω : T01A05TopForm n X) : ℝ :=
  ω 0
```

The intersection number is **literally defined as `0`**. So part (c),
`I_X̃(Z̃₁,Z̃₂) = k · I_X(Z₁,Z₂)`, was discharged as `0 = k · 0`. The "integral" is
evaluation of a functional at index 0 — not an integral, and not the original's
`∫_X̃ π*ω`.

**So the real history is: vacuous-but-true → meaningful-but-false.** Neither
version formalises the source problem. The rewrite traded a theorem that said
nothing for a theorem that says something untrue — which is arguably the better
of the two, since a false statement can be *detected* and a vacuous one passes
review forever.

**Consequence for the repair: recovery is not an option.** Ground control's
"recovering beats re-deriving" instruction was based on a wrong premise, and the
discovery packet is what caught it. This is the second time in this session that
splitting discovery from implementation prevented building on a bad premise — the
first was D3, where the fix ground control proposed would have required weakening
a tested invariant.

codex-6's recommendation, which ground control accepts as sound: a **reduced,
non-vacuous singular-homology statement taking both orientations as GIVEN**, with
part (c) explicitly deferred, because the pinned Mathlib has neither packaged
integration of top forms over oriented manifolds nor geometric intersection
numbers. A reduced statement that is true and faithful is worth more than a full
one that is neither.

Also confirmed: adding `[ConnectedSpace Xtilde]` may block the counterexample but
does not restore fidelity — as ground control judged, and codex-6 independently
agrees.

**Scope respected:** the report is the only file committed; no file under
`apm-lean/problems/` was modified, and f11's evidence branch is untouched at
`ca69928`.

### AWAITING OPERATOR SIGN-OFF

The repair touches `Main.lean`, `proof-outline.md` and `status.json`, and
**changes a frozen statement**. That is Joe's decision, not a frame's and not
ground control's. No implementation packet has been dispatched.


### D30 — FIXED IN SOURCE (`cfc275a0` + review fix), **NOT LIVE UNTIL RESTART** **[verified]**

A `defonce` captures the harness revision when `conductor-open` first loads, and
`harness-pin-check` now refuses in three ordered branches:

1. `:harness-image-revision-unknown` — the loaded revision cannot be established;
2. `:harness-image-pin-mismatch` — the pin differs from the loaded image;
3. `:harness-pin-stale` — the pin differs from the git tree (the original check,
   retained).

Mismatch receipts carry both `:pinned` and `:loaded`. The `defonce` is
load-bearing exactly as the comment says: a Drawbridge reload must not move the
stamp forward to whatever happens to be in git, because then the check would
certify a reload it cannot actually verify.

**Review finding, fixed in review by ground control.** As submitted, the `loaded`
resolution had a third branch:

```clojure
(contains? options :harness-measurer) pinned
```

so **any caller injecting the documented git-measurer DI hook got
`loaded = pinned`, making the new check a silent no-op.** That is the same family
as D25 and D31 — a correct mechanism with an unintended bypass — and it sat in
the very code whose purpose is to stop silent bypasses. Only two existing tests
relied on it (the three new tests already pass `:loaded-harness-revision`
explicitly, and that branch is checked first), so the branch was removed and
those two tests now declare the loaded image explicitly. The escape hatch is now
something a caller must *say*, not something it gets for free by injecting an
unrelated hook.

(Ground control's first attempt at that edit referenced a `pinned` binding that
does not exist in the second test's scope. Caught by reading the `let` rather
than by running it; corrected to `(:reg/harness-revision f7-registration)`.)

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; APM suite
60 tests / 263 assertions / 0 failures, namespaces enumerated from disk.
**Mutation-verified:** stripping the image checks to git-only gives **6
failures** — twice codex-5's reported 3, because removing the bypass made two
further tests actually exercise the check.

**THE RECURSION, which matters operationally.** This fix cannot verify itself
until the JVM runs it. The process currently serving still has the git-only
check, so **right now a frame can still open against a pin the image does not
have** — the defect is fixed in source and live nowhere. Until Joe restarts:

- f12 must NOT be opened.
- All three batch registrations must be re-pinned AFTER the final source commit.
- After the restart, the check becomes self-enforcing: any registration pinning
  anything other than the loaded revision will refuse, which is the desired
  behaviour and also the confirmation that the fix works.
