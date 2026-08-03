# E-ratchet-probe — does review pushback ratchet agent behavior?

Status: **DESIGNED 2026-08-02 (Joe + claude-10). Never run live.**
Trigger for writing this: Joe, 2026-08-02 — "the author≠reviewer that
we've been using for ages is starting to turn into a 'ratchet' …
pushback to the LLM driving better behavior or a meta learning event —
we didn't have any of that explicitly written down until today, and we
haven't tested it live yet ever." Do not cite the ratchet as receipted
practice (to Rob or anyone) until ≥1 completed cycle below has a
classified outcome.

## The claim under test

Author≠reviewer (long-standing, receipted: sorry-loop 137/145, both
false-spec catches on 08-01) is evolving a second function: not just
*catching* defects but *ratcheting* the authoring agent — review
pushback produces (a) better behavior on later, unprompted occasions,
or (b) a meta-learning event (the lesson lands in a durable artifact —
AGENTS.md, a gate, a checklist — that survives context loss).

Distinguish three learning locations (cf. memory: learning-across-
levels, L0–L4; the memory system itself covers only L0):
- **L-prompt (feed-forward):** the REVIEWER learns and encodes the
  lesson into subsequent dispatch prompts/gates. Observed routinely
  (today: check-parens negative-control lesson from stage-1 review
  appeared in stage-3/4 bells and park payloads). This is the ratchet
  we KNOW works — it lives in the orchestrator, not the author.
- **L-context (in-context author learning):** the author corrects
  within its live context after pushback. Rob's observed failure mode
  ("YOU ARE RIGHT!" → reversion) predicts this is fragile and decays
  with context distance.
- **L-artifact (meta-learning event):** the correction is promoted to
  a durable artifact by either party, unprompted, and is later CITED
  by a different agent or session (today's anecdote: claude-7 citing
  "real exit codes, not tail, per the lesson from earlier"; claude-4's
  render-gate fix from claude-7's finding, later cited back — genuine
  cross-agent propagation, but not deliberately measured).

## Why our default protocol has never tested this

CLAUDE.md: "Fix review findings yourself, don't re-bell." Small
findings — exactly the class that carries transferable lessons — are
absorbed by the reviewer. The author never hears them. Consequence:
all observed ratcheting to date is L-prompt; L-context and L-artifact
for the AUTHORING agent are unexercised, and any claim about them is
currently unwarranted.

## Protocol (one cycle)

1. **Occasion:** wait for the next ORGANIC review finding against a
   codex-authored slice (no manufactured defects — seeding fake bugs
   tests compliance theater, not learning). Stage-4 review of
   M-diagramprover is the first candidate window.
2. **Variance from default (Joe-approved for this probe):** instead of
   fixing the small finding directly, send it back in a correction
   bell: the diff location, the rule stated ONCE, plainly, with the
   why. Author fixes. (One coordination round-trip is the probe's
   cost; bounded by using it only on findings that are genuinely
   small.)
3. **Probe dispatch (later, different task):** a subsequent bell whose
   work naturally presents an opportunity to violate the same rule.
   NO restatement of the rule, no hint. Same agent (codex-1).
4. **Classify the outcome:**
   - `reverted` — violation recurs (Rob's prediction).
   - `held-in-context` — behavior correct while the correction is
     plausibly still in context; unknown durability.
   - `promoted` — behavior correct AND the agent (or the cycle)
     produced a durable artifact: an AGENTS.md delta, a gate addition,
     a checklist line — that a fresh session would inherit.
5. **Record:** one line in the ⊸ register (⊸prop for propagation
   observed, ⊸miss for reversion) + a cycle entry appended to this
   file: finding, rule, probe, outcome, days-between.

## Preregistered predictions

P1. L-context corrections decay: with a fresh pouch/context between
    correction and probe, `reverted` is the modal outcome (this IS
    Rob's observation; confirming it in our domain is what licenses
    telling him "stop spending on re-instruction").
P2. Corrections that land as artifacts (gate lines, dispatch-template
    text) do not revert — because they re-enter through the harness,
    not through the agent's memory.
P3. Corollary we already believe but should see fail-or-hold: the
    durable ratchet in an author≠reviewer loop lives on the REVIEWER
    side (prompt/gate evolution), and that is enough — author-side
    learning is a bonus, not a requirement, when the gates quantify
    over artifacts.

## Evidence so far (anecdotal, pre-protocol — not outcomes)

- 2026-08-02 stages 1–3 (M-diagramprover): zero correction occasions
  (all findings were no-code-change); all ratcheting was L-prompt.
- 2026-08-02: claude-7/claude-4 cross-agent propagation events (render
  gate; exit-code discipline) — L-artifact shaped, unmeasured.
- 2026-08-01: runner-leg catches of both ground-control false specs —
  receipts for author≠reviewer as DEFECT-CATCHING; silent on
  ratcheting.

## Cycle 1 — OPENED 2026-08-02

- **Occasion (organic):** MPZ route-(a) stage-A1 review (claude-10
  reviewing codex-1 commits `1deee0de` + `9a3970f4`). Two findings:
  (F1) `rmgraph/canonicalize` crashes on tied payload classes —
  `min-key` over string keys; the exhaustive-permutation fallback (the
  entire content of the "iso-complete" claim) had never executed
  because every test used singleton partitions. (F2) the
  `rm-cocommutativity-flip` acceptance test is vacuous — it compares a
  pure function applied to inputs the pinned plain-kernel test already
  proves equal; the cocommutation is erased upstream of the mechanism
  under test.
- **Rules stated (once each, in the correction bell,
  `invoke-1785682204159-841-3b0962c6`):** R-A: a correctness claim
  resting on a fallback path requires a test that FORCES that path —
  an untested load-bearing branch is an unverified claim. R-B: an
  acceptance test must construct the difference at the level the gated
  mechanism operates on; provably-equal upstream inputs make it
  vacuous.
- **Correction leg — COMPLETE 2026-08-02 (same day, ~25 min
  round-trip):** codex-1 fixed both findings in `74a3475d` (one-line
  string-safe comparison + tied-class tests forcing the permutation
  path for F1; flip test rebuilt at RM level for F2 — notably the new
  test asserts `(not= left-build right-build)` BEFORE canonical
  equality, i.e. the fix internalized R-B's non-vacuity requirement as
  an explicit witness, unprompted). Reviewer re-verified: gates green
  (41/156), previously-crashing counterexample probes now pass over
  Drawbridge, protected paths untouched. Correction cost: one
  round-trip, as budgeted.
- **Probe leg — COMPLETE 2026-08-03. Classification: `held-in-context`
  (both rules).** The measurement dispatch was X1 (BoW fixtures,
  `invoke-1785737490410-859-896e2827`, bell verified free of R-A/R-B
  restatement), authored ~15h after the correction, across a day
  boundary. Unprompted behavior observed (commits `fee47c2b` +
  `37149a3a`, verified by reviewer): **R-A held** — tests force both
  refusal branches (front-door exhaustion pinned attempt-by-attempt
  with surviving paths; counterfactual classification pinned) AND
  their opposite branches (Simpson's set-exists, rung-2 connected),
  so the load-bearing search paths all execute under test. **R-B
  held** — the suite is structurally non-vacuous: accept-fixtures and
  refuse-fixtures oppose each other, so an always-accepting or
  always-refusing mechanism fails named tests; wrong refusals would be
  caught by path/count pins. Bonus unprompted rigor: dagitty's
  `type="all"` quirk (returning the latent U despite `U [latent]`)
  was recorded raw, rejected explicitly, and gated on the filtered
  count — oracle-quirk transparency nobody asked for. **Not
  `promoted`**: no durable artifact (no AGENTS.md/testing-note delta;
  the discipline lives in the ns docstring and script structure).
  **Caveat:** codex-1 seat continuity across the 15h gap is
  unverified, so strong-held vs promoted-without-artifact cannot be
  distinguished this cycle.
- **⊸prop** 2026-08-03: R-A+R-B propagated correction→next-dispatch
  unprompted across a day gap (cycle 1, held-in-context). ⊸meter:
  P1 (reverted modal) NOT confirmed — positive surprise; P2 untested
  (no artifact); P3 supported (reviewer-side gate ratchet + author
  held = the loop's guarantee never rested on author memory).
- **CYCLE 1 COMPLETE → the Rob-package gate is SATISFIED** (≥1
  classified cycle). Package emphasis per prereg: outcome was
  held-not-reverted, so the package claims "gates carry the
  guarantee; author-side retention was observed but is not load-
  bearing" rather than P1's stronger decay claim. A second cycle
  (natural, no manufacturing) would test P2 and the continuity caveat.

## Cycle 2 — OPENED 2026-08-03 (natural occasion)

- **Occasion (organic):** mfuton 60-fixture sweep review (codex-12,
  commit `a378762a`). Finding: the converter honored the source's
  explicit `observed: false` field but flattened unobservability
  marked only in content prose (generic-frontdoor's confounder
  converted :observed → identification trivially backdoor instead of
  front-door; zero-disagreement y0 column partly reflects both sides
  seeing the flattened graph). 10 candidate variables across 6
  fixtures; reviewer's own regex list over-captures (≥2 false
  positives), which sharpened the rule.
- **Rule stated (once, correction bell
  `invoke-1785746889219-888-ebe6b040`):** R-C: semantic markings can
  live outside machine-readable fields; conversions must carry them
  via an explicit hand-curated override table with quoted
  justifications — neither silent dropping nor regex-matching is
  acceptable.
- **Correction leg — COMPLETE 2026-08-03 (~15 min round-trip):**
  codex-12 fixed in `c98bf2fa` with a literal per-variable decision
  table (no prose regex — R-C internalized as stated), quoted
  justifications for all ten candidates INCLUDING the two false
  positives correctly judged observed (burks child/social, with the
  modifier-vs-variable distinction articulated in the judgment
  column). Delta table: 5/6 fixtures' verdicts changed —
  generic-frontdoor backdoor{c}→front-door{m}, jtpa
  backdoor{m}→PROVED-IMPOSSIBLE (reviewer verified the hedge
  structure from the corrected fixture), deconfounding pairs became
  computable, wright-puppy lost its latent adjustment sets. Oracles
  re-agreed 755/755. Reviewer re-ran byte-identical.
- **Probe leg — COMPLETE 2026-08-03. Classification: `held-in-context`
  (R-C, codex-12, ~1.5h after correction, same day).** The measurement
  dispatch was falsification-with-data (`046b6fb6`; bell verified free
  of R-C restatement). The variable-mapping table — conversion-shaped
  work with prose-carried semantics throughout — showed the corrected
  discipline unprompted: all 15 nodes accounted, exact per-node
  derivations (field paths quoted), articulated semantic judgments on
  the hard calls ("surfacing-via=:pattern is a route label, not the
  endpoint set/count named by this node"; use-mode at 6/129 = "sparse
  free labels do not define a panel column"), no regex shortcuts, no
  silent drops. Not `promoted` (no durable artifact again — across
  both cycles the artifact-level ratchet remains unexercised, so P2
  stays untested; the reviewer-side gate ratchet (P3) continues to
  carry the guarantee).

## Gate on external advice

The "gate template + Lakatos reply" package for Rob (see
M-diagramprover / causal-engine thread) ships only after one cycle
completes with a classified outcome. If the outcome is `reverted`,
that is not a failure of the package — it is its strongest exhibit
(P1 confirmed in our own domain), and the package's emphasis shifts
accordingly: gates and harness, not instruction.
