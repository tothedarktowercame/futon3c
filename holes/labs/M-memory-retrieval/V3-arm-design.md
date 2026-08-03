# V3 cohort — arm design (opened 2026-08-03, claude-12)

**Status: draft, open for revision. Not a registration.** The registration is a
CLean artifact against claude-4's machinery; this is the design it will carry.

Joe, 2026-08-03: *"I think we should ensure that the capabilities get built too;
it's fair enough that we'd want to evaluate them rather than just turn them all
on and hope for the best — however I don't think we should wait for an arm
design to develop the features."*

## The build/ship distinction this design rests on

E-memory-resourcing-and-strategy §2 forbids shipping behaviour-side repairs **as
defaults**. It does not forbid building them. The codebase already has the
idiom: `zai_api.clj`'s `specs-for-mode` builds the whole memory tool family and
makes it *selectable* — `:full` / `:files` / `:none`, the M-custom-harness §8.4
comparison conditions. A capability built behind a named mode, defaulting to
current behaviour, **is** an arm.

So: build the capability, default it off, name the mode, randomize over the
modes. No feature waits on this document; this document says what the modes are
called and how they are assigned.

## Why the current single-axis arm is mis-specified

The cohort was framed as **memory availability, on/off**. But "memory on" today
means *≤4-term frequency-bag push, no pull invitation*. A flat primary would
license "memory access does not raise one-shot closure" when what was tested is
the weaker of two channels at its weakest retrieval setting. That is the
expensive null, paid for in fresh backlog problems — §2's one irreversible
mistake.

## The three axes

### Axis 1 — channel (`:memory-channel`)

| mode | meaning | status |
|---|---|---|
| `:none` | no memories, no tools | control |
| `:push` | recall-at-dispatch only — **today's behaviour, the default** | built |
| `:push+pull` | push, plus the packet explicitly invites `memory_search` / `pattern_memory` / `library_search` mid-session | **to build** (packet-level) |
| `:pull-only` | tools invited, no dispatch-time injection | to build |

**Rationale.** Push fires at minimum information — before any work, from problem
prose. Pull is available at maximum information — mid-proof, when the runner
knows what it lacks. S3 is the demonstration: at dispatch nobody could know the
problem needed harmonic-series divergence; by round 214 the runner was hunting
`not_summable_one_div_natCast` with `memory_search` unused in its tool list.
No improvement to the query builder reaches this, because the information does
not exist at dispatch time.

**Build cost is near zero** — the tools already ship at `:full` on every zai
invoke. `:push+pull` is a packet paragraph plus a mode that records which
paragraph was sent. That cheapness is itself a reason to test rather than
assume.

`:pull-only` is included because it is the arm that separates *retrieval
quality* from *retrieval timing*. If `:pull-only` beats `:push`, the query
builder was never the binding constraint — the dispatch moment was.

### Axis 2 — query construction (`:recall-query-mode`) — **RESOLVED BY E8, NO BUILDABLE LEVEL**

E8 ran (`E8-query-binding-spec.md`, frozen artifact `e8-query-binding-20260803.json`,
sha256 `07be2f39…`). Both proposed levels are now refuted:

| mode | E8 result | status |
|---|---|---|
| `:frequency-4` | 2/5, 40% | shipped baseline |
| `:frequency-N` (8/12/16) | 2/5, 40% at every cap | **DEAD — do not build** |
| `:structure-aware` (arm C) | 2/5, 40%, changed no case verdict | **refuted as operationalised** |
| oracle vocabulary (arm D) | 4/5, 80% | not available at dispatch, by definition |

**My cardinality hypothesis was wrong, and it was wrong twice over.** Widening
the cap moves nothing, because `query-ladder` does `(take 3 …)` — the ladder
truncates to three terms regardless of the builder's cap, so the fourth term was
already dead code. "A bag of at most four frequency-ranked words" was both the
wrong number and the wrong lever. The code had already said so: that ladder's
docstring records a 2026-07-30 measurement and names the cause — *"term
SELECTION is by statement order rather than by signal."*

**What survives is the vocabulary finding, and it points away from this axis.**
Oracle vocabulary doubles the hit rate, so *which* terms are chosen is decisive.
But oracle vocabulary requires knowing the answer, and the one structure-aware
construction we tested scored at baseline. So axis 2 currently has **no
buildable level** — not "collapses toward `:structure-aware`", which would
over-read arm C's 40%.

Reopening it needs a *different* construction of dispatch-time vocabulary than
arm C's, and that is a research question, not a build item. Until someone has
one, this axis is closed.

**Where the finding goes instead.** If vocabulary is decisive and cannot be
known before the work starts, the treatment with a mechanism behind it is the
channel that retrieves *during* the work, where the runner supplies the
vocabulary itself. E8 was designed to test axis 2 and its main effect is to
strengthen axis 1.

One case (lib-young) failed even under oracle vocabulary, and remains unresolved
between attachment starvation and pollution at the cutoff pending the
rank-instrumented rerun. That case is axis 3's live evidence.

### Axis 3 — graph population (C1's treatment)

`:star-forest` (current) vs `:populated`. This was the *original* headline
treatment, from V2 §4.6's finding that multi-attachment was always representable
and never written. It stays in the design, but it is now **third in line for
evidence**, not first: V2 §5.2 labels the attachment-layer bottleneck a
"Conjecture, not result", and axes 1 and 2 are rival explanations that V2's two
lexical falsifications do not exclude.

## Assignment

Full factorial is 4 × 3 × 2 = 24 cells against ~356 problems — under-powered and
over-ambitious. Proposed instead:

1. **Stage 1 (channel).** Randomize `:none` / `:push` / `:push+pull` /
   `:pull-only` at dispatch, holding query at `:frequency-4` and graph at
   `:star-forest`. Primary endpoint: one-shot closure (binary, no adjudication).
   This is the arm that most threatens the headline claim, so it goes first.
2. **Stage 2 (query or graph).** Chosen by Stage 1 and E8 — an *adaptive
   transition*, and therefore a `:disposition` of the arc registration rather
   than a free choice made after seeing the data.

Preregistered dispositions — **revised 2026-08-03 after E8, and the revision
is itself disclosed rather than silently applied.**

The original set was written before E8 reported and is preserved here, struck
through, because a disposition edited after seeing data is exactly the
forking-path hazard preregistration exists to prevent:

> ~~E8 says query binding **and** `:push+pull` ≈ `:push` → Stage 2 tests
> `:structure-aware`.~~
> ~~`:push+pull` or `:pull-only` beats `:push` → Stage 2 tests channel depth.~~
> ~~Both flat → Stage 2 tests `:populated` graph.~~

**Why this revision is legitimate and not a forking path:** the stale branch is
conditioned on "E8 says query binding", and E8 was *designed and run precisely
to settle that condition*, on frozen data, before any cohort dispatch. Its
answer eliminates a Stage-2 option rather than selecting one on the basis of
cohort outcomes. No cohort data exists yet; nothing about the primary endpoint
has been observed. Had E8 come out the other way, the original branch would
have fired unchanged.

Revised set, with axis 2 closed:

- `:push+pull` or `:pull-only` beats `:push` → Stage 2 tests **channel depth**
  (invitation strength, pull-only vs push+pull separation).
- All channel arms flat → Stage 2 tests **`:populated` graph**; V2 §5.2 survives
  its rivals and C1's treatment is vindicated by elimination rather than by
  assumption.
- Either way, axis 2 stays closed unless someone produces a dispatch-time
  vocabulary construction that beats arm C's baseline result. Reopening it
  requires a new construction, not a new cohort.

The rank-instrumented E8 rerun feeds axis 3's evidence, not Stage 1's: it
classifies the one oracle-failure between attachment starvation and pollution at
the cutoff.

## Non-negotiables carried from prior findings

- **Hold-outs excluded in the selector, not the doc** (BPM near-miss). The
  `bpm-*` set must be unreachable by the cohort's problem selector in code.
- **Rows re-derived at dispatch, never read from a snapshot** (S6, 2026-08-03:
  dispatched against a 07-27 assertion of "3 sorries" that had been false since
  08-01). Every dispatch re-derives the problem's state.
- **A commit forcing function, not a pacing instruction** (S5 and S3-first both
  died at the ~30-min cap with work uncommitted, against a packet that asked in
  as many words for an honest compiling partial). Two occurrences make it
  mechanism, not exhortation.
- **Arm assignment recorded at dispatch** in the offered receipt, alongside
  `recall-system` and `recall-query` — the seed-capture discipline (B2) applied
  to the arm itself.

## Operator decisions (Joe, 2026-08-03)

1. **`:pull-only` is in.** Rationale recorded as his, not mine: *"pull is more
   naturalistic than push."* That is a stronger reason than my timing-vs-quality
   argument — it makes `:pull-only` the arm closest to how retrieval is actually
   used, rather than a deliberately handicapped lane.
2. **Model and effort must be saved and pinned.** Measured 2026-08-03:
   `zai_api.clj:26` sets `default-model "glm-5.2"`; resolution is
   `(or model (getenv "ZAI_MODEL") default-model)`; `ZAI_MODEL` is **not set**
   in the serving JVM's environment. So S3 and S6 both ran **glm-5.2** — but
   that is an inference from code plus environment, because **no model, effort,
   temperature or max-tokens field appears anywhere in the session evidence**.
   Today's two runs are not attributable to a model *by receipt*. This is a
   recording-side gap of the same family as B2 seed capture, and it is
   shippable now. Queued rather than dispatched: `dispatch_with_recall.clj`'s
   receipt code is being edited by the axis-1 build
   (`invoke-1785743880768-874-31e0c670`), and two lanes in one file is how you
   get a merge conflict instead of a fix.
3. **No fresh Zai runs until the memory model is trustworthy.** *"I want to
   make sure that what we're doing in each Zai run can potentially move the
   research dimension forward, not just generate new solved mathematics
   problems."* Stage 1 is therefore **designed and built but not dispatched**.
   Everything below runs to completion first.

`E-futon-memories.md` makes the case for this ordering better than the arm
design does, and it should be read as the justification: *"More APM dispatching
will not fix this. The corpus grows as star-forests by construction — every
memory attaches to exactly one pattern … the operator theory is untestable for
want of a graph, and no amount of real work produces one."* Unlimited Zai would
not buy the structure the memory model needs. That is why not spending it costs
nothing.

## The no-Zai queue (what must land before Stage 1 dispatches)

| work | lane | costs Zai? | status |
|---|---|---|---|
| E8 — is the query binding | codex-3 | no (frozen data) | running |
| recording-side: refused writes diagnosable | codex-5 | no | running |
| axis-1 `:push+pull` / `:pull-only` build | codex-6 | no (build only) | running |
| record model + effort in the receipt | — | no | **queued** behind codex-6 |
| **E-futon-memories: the git corpus benchmark** | — | no | scoped, measured, **not started** |
| scribe pass on S3 | — | no (codex mines existing evidence) | packet ready, not dispatched |
| E2 ablation — e3 gate | — | no (codex on apmablate, historical problems) | one manipulation-check finding from discharge |

## Open contract decision — blocks the git corpus (raised 2026-08-03)

`ArtifactRefType` (`src/futon3c/social/shapes.clj:290`) enumerates twenty
reference types: `:pattern :mission :component :gate :session :agent :thread
:evidence :proof-path :task :portfolio :arse-thread :library :problem :language
:tool :service :script :memory :decision`. **`:git-commit` is not among them.**

That is the verified cause of the 2026-08-03 `memory-write-rejected` incident:
zai-1 tried to record a memory whose subject was the commit that witnessed its
proof, and `EvidenceEntry`'s `ArtifactRef` refused it. codex-5 correctly
declined to change the contract unilaterally (`3d86cf1d` makes the refusal
diagnosable instead).

It is tolerable today and stops being tolerable when `E-futon-memories` starts,
because that excursion's corpus **is** git commits and its headline benchmark is
"given a fix, retrieve the commit that caused it". Memories whose subjects are
commits are the entire point.

Two options, Joe's call:

- **Add `:git-commit`** (and probably `:git-file`, `:git-range`) to
  `ArtifactRefType`. Preferred: mapping commits onto `:evidence` or `:script`
  would put a false ref-type on every memory in the corpus and corrupt exactly
  the provenance the benchmark depends on.
- **Map onto an existing type**, accepting that corruption.

This is a change to a shared shape, so it is a contract decision rather than a
fix.

## Why the git corpus matters

The git corpus is the one that changes the memory model's epistemic position
rather than patching it. Measured in `E-futon-memories.md`: futon3c gives 1,828
commits, 1,447 files, **134 blame-able fix instances** against a required
n ≥ 20, a built-in positive control
(`full_loop_runner.clj ↔ full_loop_runner_test.clj` churning 61/62), five or six
genuine relation types against the deployment's one — and, decisively, **native
as-of**. V2 §2.3.2 records that our text index has no temporal capability, which
blocks the recall *denominator*: we know what surfaced, never what was findable.
On git that denominator is constructible. A retrieval system cannot be validated
without one, and the APM corpus cannot supply it at any dispatch volume.
