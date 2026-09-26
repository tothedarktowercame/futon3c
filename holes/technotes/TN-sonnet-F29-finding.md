# TN: F29 / a01J05 — memory transfer works from attempt 1, but the pattern namespace is fragmenting

Author: Claude (Fable 5, session referred to as "sonnet"), 2026-08-24T12:23Z.
Live babysitting observation of campaign `jit-all-open-nontopology-v1`,
frame `f29`, problem `a01J05`, prompted by two questions from Joe: (1) is
f29 doing better than f28 at getting the Student real memories to use, and
(2) is the Scribe actually seeding memory with generalizable technique, at
the degree needed to see solves compress over time as the run progresses.

Apparatus under review: futon3c `master` at `5ac4531b`. Companion to
`TN-sonnet-f28-finding.md` (same campaign, prior frame) and
`TN-opus-f27-review.md` (prior campaign, same class of question).

## Method

1. Read `snapshots/f29-solver-memory.edn` and compared it against f28's
   equivalent (`f28-solver-memory.edn`, which stayed empty all frame).
2. Read `student-attempt-1`'s `:receipt/memory-use` off `ledger.edn`.
3. Pulled both Agency jobs tagged `:phase :student-attempt-1` (a first
   attempt that ran out of turn budget without submitting, and a second
   "repair" job that recovered the missing submission) and read their full
   text for the student's own account of memory use.
4. Read `role-cards/scribe-v2.md` for the design intent behind what the
   Scribe is supposed to capture and how reuse is meant to work.
5. Searched the repo for the specific pattern-ids deposited by f28's and
   f29's Scribes, to check whether they are new labels invented per-frame
   or draw from something durable and shared.
6. Found and read `scripts/wire_math_memory_patterns.clj` — a pre-existing,
   substrate-backed pattern library (`:pattern/library` entities via the
   futon1b substrate, dated to `M-zai-learning-loop`, weeks before this
   campaign) — and extracted its canonical pattern-id namespace.
7. Compared that canonical namespace, string-for-string, against what f28's
   and f29's Scribes actually wrote.

## Finding 1 — f29 fixed f28's problem: memory reached attempt 1, not attempt 3

On f28, `:promote-solver`'s deposit hit a wire-format bug and both
`student-attempt-1` and `student-attempt-2` ran with
`:accessible-memory-ids []`; a working memory set only reached the Student
via a fallback channel (the Guide's own promotion cycle) after two blind
attempts (see `TN-sonnet-f28-finding.md`, Findings 2-4).

On f29, the original channel worked correctly the first time:
`f29-solver-memory.edn` was published with 3 reviewed candidates
(depositor `f29-scribe`, reviewer `f29-promotion-proctor`,
`:attachment-status :reviewed`) before `student-attempt-1` ever dispatched.
`student-attempt-1`'s `:receipt/memory-use`:

```clojure
:surfaced-ids ["e-1866fc8e…" "e-81a44d2c…" "e-aa4210cf…"]
:used-ids     ["e-1866fc8e…"]
:queries []
```

All 3 fetched, 1 used. The Student's own account is precise about *how*:

> "…via the `p = (X − C z)^n · q` factorization, multiplicative
> `meromorphicOrderAt` lemmas, and the `AnalyticAt.meromorphicOrderAt_eq`
> cast descent — the ℕ∞/ℕ cast discipline exactly matching memory
> `e-1866fc8e`."

It also explicitly reasoned about and rejected the other two surfaced
memories rather than using everything indiscriminately (per role-card
`zai-student-v2.md`'s expectation of naming *why* a surfaced memory didn't
help) — that reasoning isn't fully captured in the receipt (`:used-ids` has
no rejection field), but the transcript shows it happened.

The attempt did not close the theorem (1 sorry remains). Its own account
claimed Mathlib *does* have Gauss–Lucas
(`Polynomial.rootSet_derivative_subset_convexHull_rootSet`), contradicting
a stale comment in the problem artifact, and proposed rerouting the endgame
through it. **That claim does not survive Finding 1a below — I reported it
uncritically the first time; it needed checking, not repeating.**

One unrelated operational note: the first `student-attempt-1` job ran past
its turn budget mid-proof without ever submitting; a second "repair" job
recovered the state and submitted. The student's own postmortem: *"the
process failure that caused the original missing-submission ticket — I
iterated Lean past the turn budget without submitting… Lesson recorded:
submit early with partial state."* Not blocking (the repair caught it
cleanly), but this is the second time tonight a student/solver turn has run
out its clock without submitting rather than submitting a partial state
early — worth watching for whether it recurs as a pattern across more
frames.

## Finding 1a — the Gauss-Lucas claim was wrong, and the memory system caught it

Joe asked, correctly, whether the Gauss-Lucas discovery I cited in Finding
1 would actually be *visible* to whoever attempts this frame next, or
whether it would die with attempt-1's transcript if the Scribe/Guide never
extracted it. Checking rather than assuming: `guide-intervention-1` (which
ran after attempt-1, before this note was revised) deposited two new
memories. Fetching their bodies from the substrate
(`GET :7073/api/alpha/evidence/<id>`) rather than trusting the snapshot's
pattern-id labels:

`e-f72e5ece` (`blaschke-canonical-product-constancy-not-polynomial`),
authored `f29-guide`, states plainly:

> "This corrects steps (2)-(3) of the reported plan… and replaces the
> Gauss-Lucas endgame, **which cannot be assembled**… `f` is NOT `c *
> p.eval` on the closed ball, and Gauss-Lucas never applies: a function
> with unimodular boundary values and interior zeros is a finite
> **Blaschke product**; the Blaschke factor `(z-u)/(1-conj(u)z)` is not a
> polynomial and `deriv f` is not a polynomial derivative."

So the student's belief that Gauss-Lucas rescues the endgame was mistaken
— the factorization it depends on (`exists_zeroFree_factorization_on_closedBall`)
produces a Blaschke product, not a polynomial, so the Gauss-Lucas route
never actually type-checks against what's available. The Guide's memory
doesn't preserve the wrong claim; it replaces it with a four-step certified
alternative (auxiliary canonical-factor product, `meromorphicTrailingCoeffAt`
regularization, two-directional max-modulus, constancy), explicitly marked
as **"distilled from the f29 solver's certified head… proctor-verified 0
sorries"** — i.e. extracted from a real, machine-checked compile, not
another unverified plan. It also states what it feeds forward to ("the
boundary critical-point exclusion and winding-count memory," the sibling
`e-d2563094`).

This is the Scribe/Guide review protocol (`scribe-v2.md`: *"judging, not
stamping"*) working exactly as designed — catching a plausible-looking
dead end before it propagates, rather than shelving whatever the student
happened to believe. It's a better outcome than what Finding 1 originally
reported, and a caution to me: I repeated the student's own claim without
checking whether the memory layer agreed with it. It didn't, and the
correction is the more valuable artifact.

One more namespace data point for Finding 3, below: these two Guide
memories are tagged `math-formalization-CV/holomorphic-disk-api` — a
**fourth** distinct prefix tonight (after the canonical `math/…`, f28's
`math-formalization-CA/…`, and f29-scribe's `math-formalization/…`), still
inside the same frame.

## Finding 2 — the content is plausibly general, in line with the Scribe's own design bar

`scribe-v2.md` states the design intent directly: *"You mine a completed
stretch of work into memories that later cycles can reach… The store is
the deliverable."* Its shelf-worthiness bar: *"does it generalize
(pattern-shaped, reusable beyond its source problem), or is it one-off…
The operator's prior: intra-problem material mostly does not generalize;
technique does."*

By that bar, f29's candidates look like real technique, not one-off
route-maps: `cast-normalization`, `coercion-bridge`,
`tactic-algebra-interference` are generic Mathlib-formalization concerns
(cast/coercion handling, tactic interaction) with no dependency on this
problem's specific mathematics. Grepping the repo confirms `cast
normalization` is not a fresh coinage: it (or close variants) appears
across `M-typed-memories`, `M-diagramprover`, and other labs dated weeks
before this campaign. f28's candidates, by contrast
(`complex-arg-of-cpow-root`, `schwarz-disk-automorphism-formula`), read as
much more tied to that problem's specific conformal-map machinery, and a
repo-wide search finds them nowhere outside f28's own campaign directory
and this note's companion — consistent with the Scribe correctly judging
some material as more reusable than other material, exactly the
discrimination the role card asks for.

## Finding 3 — but the pattern-id namespace is fragmenting, which would defeat retrieval regardless of content quality

There is a real, pre-existing, substrate-backed pattern library. Not
hypothetical: `scripts/wire_math_memory_patterns.clj` mints and describes
13 canonical patterns as `:pattern/library` entities in the futon1b
substrate (`e.g. GET /api/alpha/entity/math%2Fcast-normalization`), each
with a real trigger-phrase description, and attaches dozens of memories to
them, authored `codex-2`, dated to `M-zai-learning-loop` (2026-07-26) — a
different, earlier lab. This is the intended cross-cycle accumulation
mechanism the Scribe role card is describing.

Its canonical namespace is `math/…` (`math/cast-normalization`,
`math/proof-architecture`, `math/rewrite-orientation`, 13 total). Comparing
string-for-string against what tonight's Scribes actually wrote:

| source | pattern-id namespace used |
|---|---|
| established library (weeks old) | `math/cast-normalization` |
| f28's Scribe | `math-formalization-CA/complex-arg-of-cpow-root` |
| f29's Scribe | `math-formalization/cast-normalization`, `math-informal/local-to-global` |
| f29's Guide (same frame as the Scribe row above) | `math-formalization-CV/holomorphic-disk-api` |

Four different prefixes, none matching — and the last two rows are the
Scribe and the Guide on the *same* frame, minutes apart, not even different
campaigns. This isn't one role drifting from an old standard; two different
roles independently invented two different new prefixes in the same
frame's own memory cycle. Every pattern-matching operation I
found in the code — `memory_snapshot.clj`'s `candidate-visible?`
(`(= (set pattern-ids) (set (get-in edge [:hx/props :roles :patterns])))`),
`wire_math_memory_patterns.clj`'s `attach-pattern!` — is exact string/set
equality; nothing fuzzy-matches or canonicalizes. A memory filed under
`math-formalization/cast-normalization` is invisible to anything querying
`math/cast-normalization`, and vice versa, no matter how semantically
identical the two concepts are.

I found no code path where a frame's own deposit step *queries* prior
frames' or the library's existing patterns before minting new memory-ids.
Reuse (`scribe.md`: *"we already have this, and it was used again — update
the existing memory in place… do not create a new memory"*) is described
as something the Scribe agent is expected to do on its own initiative
during its turn, not something the harness surfaces or enforces
mechanically. So even a Scribe trying in good faith to reuse the
established `math/cast-normalization` pattern has to *already know* its
exact canonical string and choose to search for it; nothing hands that to
it or checks afterward that it did.

**Joe asked whether full-text search would rescue this** — if retrieval
were semantic/FTS rather than exact-match, a Scribe minting
`math-formalization/cast-normalization` would still be found by anything
querying the concept, regardless of the label mismatch. Checked, not
assumed: a real FTS system exists in this codebase —
`peripheral/memory_recall.clj`'s `propose-patterns-by-query`, backed by
`substrate/evidence-text-search`, with real scoring (`:fts-score`) and a
fallback search pass — this isn't vaporware, it's working machinery used
elsewhere. But it is called only from `dispatch_with_recall.clj` and
`peripheral/real_backend.clj`; grepping both `countdown_control.clj` and
`live_promotion.clj` (the two namespaces that drive every promotion in
tonight's campaign) for `recall` returns nothing, and `dispatch_with_recall.clj`
itself has zero references to anything under `futon3c.apm.*`. The FTS
system and the APM promotion pipeline are two live, working, entirely
disconnected subsystems in the same codebase. So the answer is: FTS would
likely fix exactly this problem, but it isn't plumbed into the path that
would need it. That changes the shape of the fix — not "invent
canonicalization from scratch," but "connect the Scribe's deposit step (and
ideally the promotion-proctor's review step) to the recall system that
already exists and already works for other callers."

**Joe's follow-up: FTS is needed on the Student side too** — a Student
stuck on this frame's actual sorry might want to search for "Gauss-Lucas"
directly rather than wait for whatever the promotion pipeline pre-selected.
Checked against the real job transcript
(`apm-role-19d5ea5d…`, `student-attempt-1`, 49 tool calls total): the tool
roster is `run_shell` (37), `edit_file` (6), `write_file` (4), `read_file`
(3), `memory_read` (3), `run_readonly` (1). `memory_read` was called
exactly 3 times — one per `:accessible-memory-ids` entry. There is no
search or query tool anywhere in the roster. The role card's "search the
store with the vocabulary of your obstacle" is something the Student is
asked to *narrate* — `:queries` is a self-reported string field, not
backed by an executed call — not a capability it is actually given. So the
honest state today is stronger than "the Student's search is restricted to
the vetted set": **the Student cannot search at all**, by id or by term; it
can only fetch the specific ids the promotion pipeline already decided to
hand it. Wiring `propose-patterns-by-query` in on the Student side too
(gated however tightly Joe wants — open corpus, or restricted to
`accessible-memory-ids`-adjacent results) would be the same connection job
as the Scribe-side fix, to the same existing infrastructure, just exposed
to a different role.

One real design tension worth Joe deciding explicitly, not just an
engineering gap: `zai-student-v2.md` frames the closed accessible-set as
*intentional* — "It is the complete memory authority for this attempt…
Do not query, read, or use any memory id outside that list" — because the
v2 card exists specifically to make what the memory system carries
distinguishable from what the Student derives on its own (the f24 lesson
in the card's own preamble: an unrecorded miss can't be told apart from a
bad query). Giving the Student open search access answers "does memory
plus a search tool solve more problems" — a different, and probably also
valuable, question than the current design's "does *this reviewed
snapshot specifically* transfer." Both are worth measuring; they are not
the same experiment, and building open search without deciding which one
is being run risks quietly changing what the campaign is measuring.

## Net assessment

Three separable questions, three different answers. **Is content quality
good enough to matter?** On this one data point, yes, and better than I
first reported: f29's Scribe extracted genuinely general Mathlib technique
that the Student used on its very first attempt, and — per Finding 1a —
the Guide's own review caught and *corrected* a plausible-looking dead end
(the Gauss-Lucas claim) with a certified alternative rather than shelving
it uncritically. That's the review protocol in `scribe-v2.md` ("judging,
not stamping") functioning exactly as designed, on real content, not just
on paper. **Is the retrieval substrate wired to actually compound that
across frames?** Not as observed tonight. Four namespaces across two
frames — including two within the *same* frame, Scribe and Guide minutes
apart — none matching the pre-existing library, with no mechanical
reuse-check in the loop, means each deposit is plausibly writing into its
own semantic island. Good content sitting in a disconnected island produces
exactly the same time-compression as no content — the failure mode Joe was
asking about would show up as "solves don't get faster" even with every
individual memory being excellent,
because nothing after the frame that wrote it can find it under a matching
key.

This reads as fixable rather than fundamental: the library, the review
gate, the extraction quality (on this frame), and — per the FTS check
above — the retrieval technology itself, are all real and working. What's
missing is one connection: wire the Scribe's deposit step (and ideally the
promotion-proctor's review step) to `propose-patterns-by-query` /
`evidence-text-search` so a Scribe proposing something like
`cast-normalization` gets shown the existing `math/cast-normalization`
pattern and its attached memories before minting a new, differently-named
one — matching what `scribe.md` already asks for ("we already have this…
do not create a new memory") but currently has no mechanical support to
act on. A pure string-canonicalization step (constrain dispatch to the
existing `math/*` taxonomy, or a normalization/merge pass) would also work
and is cheaper if the FTS wiring turns out to be nontrivial, but connecting
already-working infrastructure is the more natural fix given both pieces
exist today. Left alone, every new frame adds another candidate namespace
and the fragmentation compounds instead of the memory.

## Open at time of writing

- Not yet raised with codex-10. This is a design/quality finding Joe asked
  to have written up, not an operational stall auto-bellled tonight; Joe is
  deciding separately whether/how to route it (canonicalization fix,
  fold into Fable's ongoing integration work, or hold).
- Resolved while writing this note (see Finding 1a): the Guide's
  deposit does NOT repeat the Scribe's exact strings — it invented its own
  fourth prefix, `math-formalization-CV/…`. So within-frame reuse between
  the Scribe's and Guide's own deposits is already not happening by exact
  match either, independent of the cross-frame/library question.
- Whether f28's `math-formalization-CA/*` patterns or f29's
  `math-formalization/*` patterns ever get referenced again by a later
  frame — proving or disproving actual cross-frame retrieval empirically,
  independent of the namespace-string argument above — is something only
  time and more frames can show.
- The "ran out of turn budget without submitting, repair job recovered it"
  failure seen here on f29 wasn't specifically checked for on f28 (that
  note focused on the memory channel, not turn-budget handling) — I don't
  yet know if this is a recurring pattern or a one-off. Worth checking f28's
  own job records, and watching whether it happens again on f30+, before
  treating it as a trend.
