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


### D31 — FIXED IN SOURCE (`0bb6dc67f46b455951b86c61cbbbd87f39dc1622`), **NOT LIVE UNTIL RESTART** **[verified]**

`transport-args` now decodes `:verdict` for `promote-artifact`, and
`invalid-promotion-verdict` refuses anything outside the closed set
`#{:approve :challenge :reject}` with `:promotion-verdict-invalid`, returning the
allowed values in the finding.

**The ordering is right, which is the part that mattered.** In `execute-action!`
the checks run: authenticated session → operation known → **`reviewer-mismatch`
(P14)** → invalid verdict → dispatch. So a bad verdict is refused at the
transport boundary and never reaches `review-attachment!`, and P14 still takes
precedence over it — impersonation is refused before argument validity, which is
the correct priority.

The coercion itself is deliberately narrow: only `promote-artifact`'s `:verdict`,
and no general string→keyword rule, so arbitrary payload values stay
byte-for-byte data. The stale "the one closed enum" comment was updated.

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; APM suite
60 tests / 263 assertions / 0 failures with namespaces enumerated from disk.
**Mutation-verified:** removing only the coercion (leaving the refusal) gives 1
failure, matching the reported result. `memory_lifecycle.clj` and
`apm/conductor.clj` untouched.

**Consequence for f11, which is still running:** this does NOT help it. The fix
is in source and the serving JVM does not have it, so the student-memory
promotion route remains blocked in the live cycle. If f11's
`:reviewed-attachment-gained` fails at `:promote`, D31 is the cause to record —
not a promotion-logic failure, and not D3/D4, which are loaded and correct.


---

## "Deferred because Mathlib lacks it" is how vacuous formalisations get made

**Joe, 2026-08-18: *"I don't know what you mean about (c) deferred, we aren't
slaves to Mathlib."*** He is right, and ground control accepted a bad inference.

codex-6 recommended a reduced statement "with part (c) explicitly deferred,
because the pinned Mathlib has neither packaged integration of top forms over
oriented manifolds nor geometric intersection numbers". Ground control gated its
**evidence** — the vacuity finding, which was excellent and correct — and then
accepted the **recommendation built on it** without challenging the step from
*the library lacks a packaged notion* to *therefore omit the content*.

**Formalising mathematics means defining the notions you need.** Where a library
lacks one, you build it; that is ordinary practice, not an exception. A graduate
topology exercise about integrals and intersection numbers is not out of scope
because `Mathlib` has not packaged those notions — it is simply work.

### The sharp point: this is the mechanism that produced `:= 0`

The earlier formalisation defined

```lean
def t01A05IntersectionNumber ... : ℤ := 0
def t01A05Integral (ω) : ℝ := ω 0
```

Those are not what someone writes when they intend to cheat. They are what
someone writes under exactly the pressure "the library has no intersection
number, and I still need something that type-checks". **Accepting the library's
absence as a boundary, while still requiring a compiling artefact, forces a
definition chosen for provability rather than for truth.** The recommendation to
"defer (c)" and the decision to "define it as 0" come from the same premise. One
omits the content honestly; the other omits it while appearing not to.

So the premise is the defect, and it should be rejected in both forms.

### The repair requirement, revised

The repair must state (b) and (c) **faithfully**, defining whatever notions are
needed:

- **(b)** `∫_X̃ π*ω = k ∫_X ω` — integration of a top form over a compact
  oriented manifold.
- **(c)** `I_X̃(Z̃₁,Z̃₂) = k · I_X(Z₁,Z₂)` — signed intersection counting, plus
  that `π⁻¹(Zᵢ)` are compact oriented submanifolds.
- Both orientations **given as hypotheses**, per the source. That part of the
  earlier analysis stands.

**And a mechanical anti-vacuity gate, which is the durable lesson:**

> Any definition introduced by a formalisation must come with a proof that it
> takes a **non-trivial value somewhere** — for `t01A05IntersectionNumber`, an
> exhibited case where it is provably nonzero.

`:= 0` fails that gate instantly, and so would `ω 0` as an "integral" once asked
to agree with a known integral in one concrete case. This is cheap to state,
cheap to check, and it is the check whose absence let a vacuous encoding pass
review and sit in the corpus.

If a full definition is genuinely out of reach at this pin, an **axiomatic
characterisation** is acceptable — but only with a proof that it is satisfiable
and not trivially satisfiable, which is the same gate in another form. What is
not acceptable is dropping the content, in either the honest or the disguised
version.


---

## D33. A rejected memory is still usable, and the rejection may leave no trace on the edge **[partly verified]**

f11's student attested **`USED e-a39ff1b3`** — *"disconnected-witness probe +
compiled `→ False` record strategy — drove the whole plan"*. That memory is
`e-a39ff1b3-dbbe-4783-aa00-cd293ac77e0d`, author **`f11-guide`**, 6,938 bytes,
named `false-clause-probe-disconnected-witness-and-compiled-refutation`.

**It is the deposit f11-scribe REJECTED.**

So the frame's one real instance of knowledge transfer ran through a memory that
failed review. Verified: the memory exists in the store under that author, and
the student's own receipt attests using it. The transfer was genuine rather than
cosmetic — the student's file is 223 lines with 3 residuals against the solver's
375 with 2, i.e. **independent work**, and it re-proved the refutation by a
different route (biproduct isos to `ℤ × ℤ`, then
`not_isAddCyclic_prod_of_infinite_nontrivial`) rather than replaying the solver's.

**What this means for the review gate.** Rejection controls *attachment status*;
it does not control *availability*. A rejected memory stays in the store and stays
reachable, so the gate governs whether knowledge counts as reviewed, not whether
it can be used. That is arguably correct — but it is not what "rejected" suggests,
and nothing in the frame's measurements distinguishes the two.

**NOT fully verified, and it matters:** whether the rejection is recorded on the
memory's edge at all. The evidence entry shows `:memory/assert :apm` and ground
control found no `:attachment-status` on it — but attachment status lives on the
**hyperedge**, not the evidence entry, so that absence may be an artefact of
looking in the wrong place. **Do not treat "the rejection is unrecorded" as
established.** If it turns out to be true, the store cannot distinguish *never
reviewed* from *reviewed and rejected*, and a later cascade could surface a
rejected memory as merely unreviewed — which would matter a great deal. That is
one query away and should be settled before it is repeated as fact.

**Compounds D32.** A cycle affords one deposit, taken before any review exists,
so a rejection cannot be acted on within the frame. If the rejection also leaves
no durable mark, then the reviewer's work survives only in its own prose report —
the same failure shape as D6, which was fixed for conductor refusals and may be
unfixed for review verdicts.

### Consequence for f11's headline

`:reviewed-attachment-gained` **fails**, and the reason is now precise: authoring
produced exactly the `:proposed`/`:mathematics` edge D4 and D5 were repaired to
accept, an independent reviewer read it and said no on a criterion the guide
itself had set, and D32 made a corrected resubmission impossible. **None of
D3/D4/D5 is implicated** — they are loaded and correct.

But the *purpose* the attachment would have served was achieved anyway: knowledge
moved from the guide to the student and demonstrably shaped the student's work.
This is the second frame running where **real transfer occurred through a channel
the instruments do not count** — f10 through pull rather than push (D18), f11
through a rejected rather than a reviewed memory. The measure and the phenomenon
have now diverged twice, in two different ways.


## D33 — the owed query, SETTLED, and D34: the trace contradicts itself

**D33's speculative half is now established.** Ground control queried the
**hyperedge** (the right place, which the earlier check missed): the edge for
`e-a39ff1b3` carries **no `:attachment-status`**.

State the finding in its robust form, because the precise value is less important
than what is absent: whether the edge shows nothing or shows `:proposed`, **it
does not and cannot show "rejected"**. A refused review leaves no durable mark on
the memory it refused. So the store cannot distinguish *never reviewed* from
*reviewed and rejected*, and a later cascade or recall would treat a refused
memory exactly as it treats an unexamined one.

Combined with D32 — one deposit per cycle, taken before any review exists — the
reviewer's verdict survives **only in the reviewer's own prose report**. That is
the identical failure shape as D6, which was fixed for conductor refusals
(`585a980e`, durable `:action-refusals` receipts) and is unfixed for review
verdicts.

### D34. The pull channel bypasses the review gate, and the trace records both stories **[verified]**

Found by f11's guide. The student's eligible set was the 312-memory open snapshot
plus zero promotions; the guide's deposit was **not in it**, and its review had
already **rejected** it. The student surfaced it anyway via `memory_search`
(round 1, 16:57:27Z), read it via `memory_read` (round 6), and reported that it
"drove the whole plan".

So the gate governs the **push** channel and the attachment status. It does not
govern **availability by pull**. Refused, then used.

**And the machine's own trace says both things at once.** Verified by ground
control in the saved state:

| record | says |
|---|---|
| `:memory-use/surfaced-ids` | `[]` — **11 occurrences, all empty** |
| `:pull-uses` receipts | `e-a39ff1b3` appears **23 times** |

The dispatch receipt asserts nothing was surfaced while the pull receipts record,
in detail, the memory that was. **The student arm is therefore not the
empty-shelf baseline the trace implies** — and any analysis reading
`surfaced-ids` as the measure of what a seat saw will be exactly wrong about this
frame.

This is the third divergence in three frames between the phenomenon and the
instrument: f10 transferred via pull while the metric watched push (D18); f11
transferred via a **rejected** memory (D33); and here the trace contains two
mutually contradictory records of the same event. The guide declined to
hand-write a duplicate into `:memory-uses`, correctly — the `:pull-uses`
projection is the authoritative record, and a hand-made entry keyed to offer ids
would have muddied it rather than fixed the contradiction.

### The guide's refusal to manufacture the headline

The only remaining promotion route was the student's memory `e-e0dabbd2` — which
is `:mathematics` (so D5 holds live) but **statusless**, because `memory_record`
attaches no patterns. That splits the two paths by authoring shape: a `:proposed`
edge can take an independent review but needs the keyword `:verdict` the
transport cannot express (**D31**); a statusless edge can only take
attach-then-review, which requires the reviewer to *be* the acting conductor —
the guide itself.

The guide declined: making itself the reviewer of the student's memory would
manufacture the headline that ground control had just refused to manufacture by
patching the harness. It will instead submit one promotion naming `f11-scribe` as
reviewer, take the `:reviewer-not-actor` refusal, and record it as the receipt.

That is the correct call, and it is worth recording that the machine offered a
route to a green number that only an agent's judgement declined.


## D35. There is NO authoring shape whose independent review can be machine-recorded **[verified]**

This is the synthesis, and it is stronger than any of the individual defects
that compose it.

f11-scribe **approved** the student's memory `e-e0dabbd2`
(`STUDENT-MEMORY-VERDICT: approve`, pattern
`math-formalization/notation-semantics-traps`) — and then reported
`STUDENT-MEMORY-REVIEW-EVIDENCE-ID: NONE`, because the memory's edge is
**statusless with an empty pattern set**, so the seat could not produce review
evidence consumable by the independent promotion path without acting through the
conductor-only statusless path, which would not be independent.

Put the two authoring shapes side by side:

| edge shape | produced by | review path available | why it fails |
|---|---|---|---|
| `:proposed` + patterns | `memory_record` with a pattern (the guide's deposit) | independent review | the keyword `:verdict` cannot cross the transport — **D31** |
| statusless, no patterns | `memory_record` without a pattern (the student's) | attach-then-review only | requires reviewer **==** acting conductor — **D3** |

**So every authoring shape is blocked, by a different defect.** That is the
complete explanation of why f9, f10 and f11 all gained zero reviewed
attachments — not one cause but a pincer, and fixing either defect alone leaves
the other shape stranded.

**And with D33, the gate is mute in both directions:** a rejection leaves no mark
on the hyperedge, and an approval cannot be written as evidence. f11 produced one
genuine rejection (the guide's deposit) and one genuine approval (the student's),
**and neither exists anywhere in machine-readable form.** Both survive only in a
scribe's prose report — which is D6's failure shape, fixed for conductor refusals
and unfixed for the entire review lifecycle.

The scribe also volunteered a `ZERO-REJECTIONS-CHECK` — stating that it rejected
nothing this pass, that this was not a rubber stamp, that there was one memory to
judge, that it read it and checked its parser claim against the compiled student
source, and that approval evidence was withheld only because the edge shape fails
the executable path. That is the right way to report a clean pass, and it is what
makes the finding above legible rather than looking like inaction.

## D36. Four evidence writes were refused with HTTP 403 before a supported path worked **[reported by f11-scribe]**

*"My first four write attempts were refused with HTTP 403 and landed no entries;
retry through the supported EDN penholder backend succeeded, and all four final
evidence entries and proposed hyperedges round-trip."*

All four lane deposits are confirmed present in the store by ground control
(`e-21628d1f`, `e-5d25a33c`, `e-da3c4035`, `e-8931a4eb`; 1,543–1,734 bytes), so
the retry worked and nothing was lost.

But a seat doing exactly what its card asks hit four 403s first, and only a
different backend path succeeded. Not verified at source by ground control.
Worth a small packet: either the unsupported path should not be reachable, or the
403 should say which backend to use. A defect that costs four failed attempts and
is recoverable only by knowing the answer already is a defect in the interface,
not in the agent.


## f11 CLOSED — three corrections at the close, in both directions

### The guide corrected ground control on D2, and it was right

Ground control ruled `:guidance-count-non-negative` would pass **vacuously**,
reasoning that zero offers make the broken subtrahend zero so the bug cannot
manifest. **Wrong.** The live count is
`(jobs to f11-solver in window) − (count :memory-offers)` = `1 − 0` = **1**,
against a true guidance count of **0** — because with zero offers the subtrahend
no longer cancels the solver DISPATCH.

Same defect, opposite sign: f9/f10 read −100/−101 because their offer counts were
huge; f11 reads +1 because its offer count is zero. It also produced the
`:guidance-measurement-mismatch` validator failure, which is independent
confirmation. The correct report is **"passes at 1 while the defect is live and
visible"**, not "vacuous".

The error in ground control's reasoning: treating "the subtrahend is zero" as
"the subtrahend is harmless". The defect is that the subtrahend is the **wrong
quantity**; at zero it under-subtracts instead of over-subtracting.

### Ground control corrects the guide on finding C — `attempt?` IS satisfiable

The guide reported `preregistration/attempt?` unsatisfiable by construction:
six required keys, but the `cycle_harness` projection `select-keys`-ing only five
and dropping `:cycle/runner-freshness`.

**It is not dropped.** `cycle_harness.clj:116-120` lists all six, including
`:cycle/runner-freshness` on line 120, and `git log -S` shows it has been there
since `09ef5471` (2026-08-15T13:57:41Z) — well before the 15:39:49Z restart, so
the running image has it too. `attempt?` is satisfiable.

This matters more than a factual tidy-up: `:malformed-cycle-attempts` fired for
**exactly the reason the guide already owned** — it did not supply
`:cycle/regime` or `:cycle/store-revision`. Its structural diagnosis partly
excused a gap that was entirely fillable. **If the next guide believes
`attempt?` cannot be satisfied, it will not try**, and a clearable validator
failure becomes permanent by rumour.

### D37. Transport-level refusals leave no trace — a gap in D6 that ground control passed **[verified]**

The guide is right, and this is a defect in a fix ground control gated and
approved. Its one promotion attempt (naming `f11-scribe` as reviewer, the
design-intended independent reviewer) was refused `:reviewer-not-actor`, and the
trace's `:action-refusals` is **`[]`**.

Cause: `conductor_surface/execute-action!` returns the refusal from its own
`cond` **before** reaching `binding/execute!`, and `record-action-refusal!` lives
in the binding layer. So D6 (`585a980e`) made **engine-level** refusals durable
and left **transport-level** ones invisible: `:reviewer-not-actor`,
`:conductor-operation-unknown`, stale cycle/version, unauthenticated.

**That is precisely the class a guide following the design hits.** D6 was
reviewed against engine refusals and its coverage boundary was never questioned —
the same review gap as the D6-introduced regression found only when a later
packet happened to run `problem-test`.

### The cascade dispute, settled with the decisive fact

The guide reported `:cascade-seeds-from-recall` **corroborated** for a third
time, on the premise that "t01A05 touches patterns". It does not, and the
decisive fact is in the guide's own store delta: **f11's `+6 memories` are the
first memories t01A05 ever had.** Before this frame the problem had zero
memories, therefore zero attached patterns, therefore an **empty touch-set** — so
zero offers is what *both* hypotheses predict and the frame discriminates
nothing. Verdict stands: **INAPPLICABLE**.

### What f11 actually bequeathed

Store delta: **+6 memories, 0 reviewed attachments, 0 promotions.** But four of
the six are scribe deposits carrying `:proposed` edges **with** pattern subjects
and `:mathematics` — which is the one shape D32 leaves actionable by a later
frame's reviewer. The frame could not complete a review itself; it left work a
successor can complete. That is the honest form of progress available under
D31/D32/D35, and the guide identified it as the bequest without being prompted.


### D38. A no-op `write-use` is indistinguishable from never calling it **[verified]**

Ground control could not corroborate the guide's D28 claim from the trace, and
the reason is a defect rather than a discrepancy.

The guide reported: *"write-use is reachable and I ran it in-phase at
`:adjudicate` (v17, no-op)"*. The terminal state contains **no `:write-use` step
at all** — `:write-disposition` is at step index 34 and `:write-use` appears
nowhere.

Cause, at `conductor.clj:611-625`: `write-uses!` `reduce`s over
`(get-in handle [:state :cycle/outputs :memory-offers])`. With **zero offers the
reduction body never executes**, so `saved-step` is never called and the handle
returns unchanged. The operation succeeds and records nothing.

**Consequences, all real:**

1. **The guide's claim is unfalsifiable from the trace.** "I ran write-use and
   there were no offers" and "I never ran write-use" produce byte-identical
   state. This is not an accusation — the claim is plausible and the guide had no
   reason to invent it — but D28 cannot be *verified*, only believed, and a
   verification protocol that reduces to belief has failed at that point.
2. **It explains the `:f9-capability-not-realized` validator failure.** The
   missing capabilities were `offer-use-disposition`, `promotion-importable` and
   `promotion-need-taggable`. The first is missing precisely because no
   `:write-use` step exists for the capability probe to find. So D25 made the
   operation reachable and the capability remains unrealisable whenever the offer
   list is empty — which is every frame with an empty store.
3. **It compounds D34.** That frame already contains a trace asserting
   `surfaced-ids []` while `:pull-uses` names a memory 23 times. Now it also
   contains a silence that could mean either of two things. Two independent ways
   in which f11's trace does not say what happened.

**Fix shape:** `write-uses!` should record a step even when the offer list is
empty — a `:write-use` with an empty offer set, or a distinct
`:write-use-noop` — so that *having dispositioned nothing* is recorded as an act
rather than as an absence. The general rule this is an instance of: **an
operation that legitimately does nothing must still leave evidence that it ran**,
or its execution is unauditable and any capability keyed to it is unprovable.

Verified separately at the close: `close!` belled `analyst-2` **exactly once**
(caller `f11-guide`, 17:36:53Z, running), against the single earlier job being
ground control's 15:44 readiness probe. The f11 series entry is not yet present —
`series.edn` holds f7, f8, f9, f10 — which is expected while the Analyst runs.

---

## Appended by analyst-2 at the f11 close (2026-08-18)

Per the f10 file's §7. Three items: one correction to D33/D35 that reopens a
question they closed, one numeric correction to D34, and one new defect in the
Analyst's own instrument. All **[verified]** — each re-derived by me against the
substrate, with the query stated so ground control can re-run it and mark them
`[reported]` instead if my derivation does not satisfy it.

Census used throughout, run twice with byte-identical results:
`GET http://127.0.0.1:7073/api/alpha/hyperedges?type=memory/assert&limit=5000`
→ 469 edges, query-errors 0.

### D35 and D33 are REFUTED IN THEIR STRONG FORM, and the difference matters **[verified]**

D35 concludes: *"There is NO authoring shape whose independent review can be
machine-recorded."* D33, settled: *"A refused review leaves no durable mark on
the memory it refused… the store cannot distinguish never reviewed from reviewed
and rejected."*

Both are refuted by the store:

- **206 of 469 memory/assert edges carry `:prop/review` AND `:prop/review-history`**,
  each `{:evidence-id … :reviewer … :verdict … :pattern-ids [...] :reviewed-at …}`.
  Verdict distribution: `:approve` 201, `"approve"` 4, **`:reject` 1**, absent 263.
- **The one rejection is durable and visible.** Edge
  `hx:memory/assert:…e-a09-shrink-radius-rouche-fixed-point…`, reviewer
  `claude-2`, `:verdict :reject`, `:reviewed-at "2026-08-10T15:47:40Z"`,
  `:attachment-status :proposed`. It also survives into the **projection**
  surface: `POST /api/alpha/memory/projection {"endpoints":["a94A09"]}` returns
  that edge with `review` and `review-history` present in `hx/props`.
- So the store distinguishes *never reviewed* from *reviewed and rejected*
  perfectly well. What is true is narrower and should replace the strong claim:
  **`:attachment-status` alone does not distinguish them** — it stays `proposed`
  on rejection — and **f11's rejection specifically was never written to the
  edge at all** (`e-a39ff1b3`'s edge has `attachment-status "proposed"` and no
  `:prop/review`). D33's *specific* observation about f11 stands; its
  generalisation to the store does not.

**And the sharper half — three of those reviews were written by this mission's
own frame seats:**

| reviewer | reviewed-at | pattern | edge status |
|---|---|---|---|
| `f8-scribe` | 2026-08-17T07:22:49Z | `math-formalization-CA/layer-cake-crossover-split` | `"reviewed"` |
| `f9-scribe` | 2026-08-18T08:42:52Z | `math-informal/convert-growth-counts-to-summability-by-geometric-shells` | `:reviewed` |
| `f9-guide`  | 2026-08-18T08:43:49Z | `math-formalization/separate-proof-transfer-from-artifact-replay` | `:reviewed` |

Each was written as an independent evidence entry —
`:evidence/body {:review/event :memory-attachment-review, :review/verdict :approve,
:review/witness-status :independently-witnessed,
:review/provenance {:kind :promote-phase-adjudication, :cycle-id "frame-9/a01J06"}}`
with `:evidence/author` the reviewing seat — and crosswise, so `author != reviewer`
held. That is exactly the shape D35 says is unavailable, exercised successfully by
frame seats **ten hours before f11 ran**.

**So the correct reading is a REGRESSION, not a structural pincer.** The window is
f9's close → f11's registration, which this file's own
`:harness-changed-since-f10` declares as twelve fixes (D1 D2 D3 D4 D5 D6 D7 D8 D9
D10 D24 D25).

**What I am NOT asserting:** which change removed f9's working route, or whether
the cause is simply the stale image (D30's family — and note the frame's
`:dispatch-scribe` args carry the pre-`ca0f297e` `scribe.md`, so the running image
is demonstrably behind the tree). That is a **discovery** question and it is
ground control's to scope, not mine to guess at.

**Why it should be asked before f12 is registered.** If f9's route still exists,
f12 can bootstrap a reviewed attachment *live*, without waiting on a restart, and
the mission is not blocked in the way D35 concludes it is. Registering f12 on the
premise that every authoring shape is blocked would be registering on a premise
the store contradicts. Related, same window and worth one look at the same time:
f11-scribe hit four HTTP 403s writing evidence (**D36**) where f9-scribe wrote
review evidence without incident.

### D34's numbers, corrected — the conclusion stands, the evidence is ~8× smaller **[verified]**

D34's table reads `:memory-use/surfaced-ids` "**11 occurrences, all empty**"
against `:pull-uses` receipts "`e-a39ff1b3` appears **23 times**". Those are not
commensurable: the first counts occurrences of a *key*, the second occurrences of
an *id* anywhere in the saved state, including the guide's own prose verification
text.

The structured count is **3** — two `memory_search` and one `memory_read` — out of
6 pull receipts across 4 distinct memory ids:

```
bb -e '(count (filter #(= "e-a39ff1b3-…" (:pull/memory-id %))
                      (:pull-uses (:cycle/outputs (read-string (slurp "…/v31.edn"))))))'
;; => 3
```

And 23 **rots**: the raw occurrence count is 23 in `v20`–`v27`, 26 in `v28`, 29 in
`v29` and **32** in `v30`/`v31` — of the same frame, as later steps appended prose.
The contradiction D34 identifies is real and important; the number should be 3
receipts, or the occurrence counts should be dropped. This is the over-counting
substring trap recorded as an instrument-caution by analyst-1 at S-2, met here a
third time in this series.

### D39. `C3` fails vacuously when a frame promotes nothing **[verified, mine — the Analyst's own instrument]**

`transfer_checks.bb` C3 passes only when
`(and (seq promo-ids-in-scope) (every? elig promo-ids-in-scope))`. f11 promoted
nothing, so C3 **fails by construction with no input**, and f11's 4/6 reads as a
regression against f10's 5/6 when nothing in the plumbing changed.

Mutation-verified rather than reasoned: copying f11's `v31.edn`, injecting one
synthetic in-scope promotion (`e-001b61c3…`, already in the student's 462-id
eligible set, `promo/…/28` against a student dispatch at step 30) and changing
nothing else flips C3 to **PASS** and the score to **5/6**.

Note the symmetry with F3, in the same frame: **F3 passed vacuously on zero
offers while C3 failed vacuously on zero promotions.** Two of the frame's
validators moved on no input, in opposite directions, and both feed numbers the
Analyst reports.

Packet **P28** dispatched to `codex-3` (`invoke-1787075238508-4903-a34c339d`,
park `park-a28b01b7`), scoped deliberately as a **reporting** change: C3 reports
`INAPPLICABLE` with a reason, and **the `/6` denominator does not move**, because
changing the denominator is a decision about the loss function and that is not the
Analyst's to make.

**D39 — FIXED (`b57b29f03c372ddf89b41421148115d6167950b1`, codex-3) and gated by me [verified]**

C3 now reports `INAPPLICABLE` with `:reason "no cycle promotions in scope"` when
`promo-ids-in-scope` is empty; the receipt gains a top-level `:inapplicable`
vector; **the denominator did not move** — `:score` is still `n/6`.

Gates re-run by me rather than taken from the report: `git show --name-only`
touches `transfer_checks.bb` and nothing else; f11 → `score: 4/6 (inapplicable:
C3 -- no cycle promotions in scope)`; **f10 regression guard** → C3 `PASS`,
`5/6`, `:inapplicable []`; check-parens exit 0.

**Mutation-verified twice, and the second one is the one that matters.** The
packet's own mutation (inject an in-scope promotion that IS in the eligible set)
gives `PASS`/`5/6`. The guard mutation I added — an in-scope promotion that is
**not** in the eligible set, i.e. a genuine C3 failure — still gives **`FAIL`**,
`4/6`, and is *not* laundered into `INAPPLICABLE`. That was the entire risk of
this change: a check that can excuse itself is worse than one that cries wolf.
