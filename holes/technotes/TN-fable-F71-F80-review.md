# TN: F71–F80 review — the window after the repairs, and what the memory system teaches the pattern library

Author: Claude (Fable 5, claude-1), 2026-09-02 ~08:20Z, at Joe's request ("see what
we could learn by reviewing the latest APM frames … the memory system described
there could be a prototype for our how/why landscape, and … the scribe that learns
from zai failures is similar to what we just wrote down about pattern learning";
follow-up: "produce a review we can pass to codex to fix"). Scope: f71–f80 in
`jit-all-open-v2` (f80/b96J02 registered 07:55Z today and still at :solve), read
against `TN-fable-F66-F70-review.md` §6 and the f71–f79 sections of
`TN-opus-f47-observation.md`, which I checked rather than re-told. §8 lists the
dispatchable fixes; §7 is the design comparison Joe asked for. §9 lists what I ran.

## Short answer

1. **Ten frames, nine problems solved by the solver, six banked.** Every solver
   phase f71–f79 verified clean. f76/b95J02 closed fully (a2 proved it). f71, f74,
   f75, f77, f79 closed `:partial` on the student axis with `:problem/outcome
   :solved` where the solver earned it, and banked. f72, f73, f78 parked with
   decision records and did **not** bank — their three verified proofs sit in
   `refs/apm/rescued-solves`, bringing the stranded pile to **eight**. §1, §4.
2. **The repairs from the last note mostly landed** — the fingerprint audit now
   runs campaign-scoped at every close (973881e6, 89 rows, status `:ok` written
   07:54Z today), the cascade retries 503-busy (6dacd087), transport faults route
   to the apparatus budget — but recommendation 4's one-line backfill on the f34
   route memory is still not done, and recommendations 5/6/7 await Joe. §2.
3. **Memory use in the window keeps its shape: 22 use events, 8 fingerprinted,
   all eight within-frame.** Five cross-problem rows, zero fingerprinted. The one
   new thing: a `f69-zai-scribe` memory (mined from *student* attempts) crossed
   to f76/a2 typed `:regulative` and was verdicted `not-adjudicable-by-token` —
   the typed-kind regime working end-to-end on a failure-mined memory. §3.
4. **The sibling cascade delivered its 100-offer cap five times and zero offers
   were ever taken.** The Opus note established why (offers are bare by design;
   the hook channel is gated to domain-general patterns, and these frames route
   through CA/CV families) and what the zero-offer attempts actually were
   (substrate visibility timeouts excluding all ~200 candidates as
   `:unverifiable-depositor-provenance`). I verified the offer/used intersections
   and the f79/a1 exclusion record. §5.
5. **New finding of this note: frame dispositions land in three different places
   depending on how the frame stopped**, and one of the three is silent. Closed
   frames write `terminal/frame-terminal.edn`; parked-then-decided frames write a
   decision record only into `queue-state.edn`'s `:parked` vector; and
   `frame-park-decisions.edn` — the documented adjudication file — has f72 and
   f78 but **not f73**. My first extraction pass read the two documented places
   and concluded f73 stopped silently; it did not (decision at 07:52:00Z,
   `:disposition :partial`, residual `[:student-memory-used-without-surfacing]`).
   A reader following the docs reproduces my mistake. §6.

## 1. What the ten frames did

Arm file `memory-cascade-arm.edn` = `{:enabled? true :routes [:sibling] :cap 100}`;
every manifest in the window is a single-unit block, `:arm :treatment`. **No
control frames exist in f71–f80.**

| frame | problem | solve | a1 | a2 | a3 | frame result | banked |
|---|---|---|---|---|---|---|---|
| f71 | b93J03 | verified | open | partial | partial | :partial (:solved) | yes |
| f72 | b93J04 | verified | partial | — | — | parked→:partial (queue-state) | **no — rescued** |
| f73 | b94A01 | verified | partial | partial | partial | parked→:partial (queue-state) | **no — rescued** |
| f74 | b94J01 | verified | not-closed | not-closed | partial | :partial (:solved) | yes |
| f75 | b94J03 | verified | failed | partial | partial | :partial (:solved) | yes |
| f76 | b95J02 | verified | partial | **success** | success | **:closed** | yes |
| f77 | b95J04 | verified | failure | partial | partial | :partial (:solved) | yes |
| f78 | b96A02 | verified | not-closed | not-closed | — | parked→:partial (queue-state) | **no — rescued** |
| f79 | b96A03 | verified | **proof-complete** | **solved** | partial | :partial (:solved) | yes |
| f80 | b96J02 | round 1/50 at 07:59Z | — | — | — | live | — |

Notes, each verified in the frame dirs:

- **f79 is the window's second-cleanest memory case after f76**: a1 under the
  same-problem holdout reached `proof-complete`, a2 with the deposits `solved` —
  both student solves on a problem the solver had solved. `:frame/result` is
  `:partial` because the close receipt's `:receipt/result` says so (the closing
  seat's judgement over all three attempts; a3 regressed to partial), while
  `:problem/outcome :solved` records the mathematics. The two axes disagree by
  design, not by defect — my own extraction first misread this as a scoring
  artifact.
- **f77's one unremovable sorry is a statement defect**, per the a3 student's
  account: an under-hypothesized frozen bridge lemma. The student proved the main
  theorem. Statement-defect handling exists (`:claimed-defect`, still unexercised
  by any solver) but the *student* lane has no equivalent channel.
- **f77's zai-scribe had all 5 candidates rejected** (`:published-memory-ids []`)
  — the promotion gate holds against the failure-mining lane too, as it did
  0/11 in f32–f35 before the v2 card.
- Chronic 503s throughout (13–58 per ledger); f71/a1's failure account calls its
  memory channel "dry (cascade 503, searches empty, on-topic candidates withheld
  by same-problem holdout)" — a holdout attempt with a failed cascade is very
  close to the never-run full-ablation arm, unlabelled.

## 2. Repairs ledger (vs TN-fable-F66-F70 §6)

| # | recommendation | state 2026-09-02 |
|---|---|---|
| 1 | fix `audit!` path handling | **done** (973881e6, 09-01 06:07): campaign-scoped at close, artifact at campaign `analysis/`, 89 rows, tests assert non-empty fixture. Wired status `:ok` written 07:54Z today at f79's close. |
| 2 | recovery closes call the audit | subsumed by 1 (campaign scope means any close refreshes all frames); no recovery close occurred in the window to exercise it |
| 3 | record cascade status / decide C-6 | **half-done**: retry landed (6dacd087), failures now typed `:memory-cascade-unreachable` and charged to the apparatus budget; but no per-frame status line in `conditions.edn`, and the C-6 arm question is now sharper — 500 offers, zero uptake, offers bare by design (§5) |
| 4 | backfill `:memory-use/kind :regulative` on `e-63b7c7c1` | **not done** — f75/a1's row still reads kind `None` (verdict `already-in-base`) |
| 5 | sweep or refuse rescued solves | **not done, pile grew 4 → 8**: a98J03, a99J05, a99J06, aunk04, b90A03, b93J04, b94A01, b96A02. f78/b96A02 is the first stranded by pure apparatus (transport timeout), no role at fault. Joe's decision. |
| 6 | give the solver a shelf on one frame | not adopted; no amendment since 11 |
| 7 | size-matched holdout control | not adopted; `:confound/unresolved` stands |
| 8 | update defect register | not done (last entry A11, 08-28) |
| 9 | revisit 55ce42a4 (gate that cannot fail) | untouched (`git log -S` still shows one commit) |

## 3. Memory use, f71–f79, by the artifact standard

From the wired campaign audit (89 rows total; 22 in-window), depositor/subject
from the rows' origin fields:

| | fingerprinted | already-in-base | unwitnessed | not-adjudicable | no-source | total |
|---|---|---|---|---|---|---|
| within-frame | 8 | 8 | 2 | 2 | 1 | 21* |
| cross-problem | **0** | 4 | 0 | 1 | 0 | 5 |

(*two rows are double-listed across attempts; 22 distinct rows.) The five
cross-problem rows: f73/a2 ← f67-guide (b90A01), f75/a1 ← f34-guide (the route
memory, still kind-untyped), f75/a2 ← f59-scribe (b00J01), f76/a2 ←
**f69-zai-scribe** (b93A01, `:regulative`, `not-adjudicable-by-token`), f77/a2 ←
f73-scribe (b94A01). All verdicted `already-in-base` or `not-adjudicable`; the
token-witnessed transfer remains entirely within-frame — the solver's route,
mined minutes earlier, into a fresh student on the same problem. Fingerprinted
hits in-window include real API deltas (f76/a2: six novel tokens around
`Module.AEval'`; f74/a3: `IsCyclic.card_powMonoidHom_ker`).

The f76/a2 zai-scribe row is worth singling out: the first *failure-mined*
memory (scribe-reduce over f69's student attempts) to cross problems, and the
typed-kind regime handled it honestly — `:regulative`, so the audit says
"not adjudicable by token" instead of pretending a token test applies.

## 4. The pin-coupling defect, now quantified

In this window the split is exact: five frames reached a terminal receipt and
all five banked; three frames parked-with-decision and none banked, despite all
three solver proofs being verified clean (f73/b94A01 and f78/b96A02 additionally
rebuilt independently by Opus and pinned). Parking is a statement about the
learning protocol (f73: a student cited an unsurfaced memory, repair budget
exhausted — correct park) or the apparatus (f78: a futon1b visibility timeout —
no one's fault); in neither case did the mathematics change. Eight verified
proofs are now off master. The Opus note argued this; the window's 5/5 vs 0/3
is the measurement.

## 5. Cascade: verified numbers

Offers-vs-used intersection over every attempt since the retry fix (verified
against `:used-via-cascade` and the receipts): f75/a3 100→0, f76/a3 100→0,
f77/a2 100→0, f77/a3 100→0, f78/a2 100→0 — **500 offers, zero taken**; every
memory actually used arrived via the base shelf. Zero-offer attempts with an
`:offers []` + ~200 × `:unverifiable-depositor-provenance` exclusion record
(f79/a1 verified: 102 distinct ids, 100 of 100 overlapping f77/a2's offered set)
are futon1b visibility timeouts, not empty routing. And per the Opus 09-02
correction: sibling offers carry `:pattern-hook nil` *by construction* — the
hook-attaching path (`f302fee4`) serves only `:route :pattern` offers, which are
additionally gated to domain-general families, and f76–f79 route entirely
through `math-formalization-CA/CV`. Whether CA/CV patterns' `:source` lines
("Assemble Disk Theorems From the Library's Interface") should reach sibling
offers is a design question for Joe, not a bug.

## 6. Where dispositions live (new finding)

Three sinks, one silent:

1. `terminal/frame-terminal.edn` — closed frames (f71, f74–f77, f79).
2. `queue-state.edn` `:parked[].decision/record` — parked-then-decided frames
   (f54, f72, f73, f78): full records with `:not-done` and
   `:decision/recommended-route` text, `:frame-voided? false`.
3. `holes/labs/M-apm-demonstration/frame-park-decisions.edn` — the documented
   adjudication file: has f72 and f78, **missing f73**.

Anyone auditing from the documented files concludes f73 vanished. The decision
writer should append to the park-decisions file (or the file should say it is
non-exhaustive and point at queue-state).

## 7. What this teaches the pattern library (Joe's question)

The APM memory system and the futon3 how/why landscape are the same shape built
from opposite ends.

- **Same substrate.** Every APM memory must hang under a pattern (`:pattern-ids`
  non-empty, enforced at deposit; `memory/assert` hyperedge over
  `[memory-id problem-id pattern-ids…]`). Memories are `@how` leaves on a
  pattern graph. The cascade's three routes are landscape traversals — sibling
  (other leaves under your patterns), why-hop (BFS over authored
  `pattern/has-semantic-why` edges), co-incidence (pattern→problem→pattern).
- **The unbuilt-graph failure mode.** Why-hop measures *empty* and co-incidence
  *floods* because the graph is "a forest of stars" — one attachment per memory,
  few authored why-edges (PLAN-apm-cascade-demo-instance's own diagnosis).
  Traversal only narrows when the why-edges are authored and dense. This is O2's
  commitment (edges authored, never inferred) hitting its cost: authoring is the
  whole price of a landscape that routes. The library loop's directive-authoring
  stage is buying exactly this.
- **Hooks are what make an offer usable.** 500 bare sibling offers, zero taken;
  shelf memories carrying `:name` + `:hook` ("when to retrieve this") get used.
  For LA1's preferential-attachment prior: an attachment proposal must carry the
  pattern's tension text (the IF/HOWEVER), or the constructor has no basis to
  prefer — a bare ranked id list is demonstrated dead weight.
- **Mirror-image learning.** APM has *no* online weighting: selection priors are
  hand-stated (promoted-this-frame, then lexical identifier-overlap), and every
  instrument change goes through a preregistered amendment with a stated
  falsifier. LA1's weights-on-edges substrate is the piece APM deliberately
  lacks; APM's discipline — author≠reviewer at every deposit, content-addressed
  immutable snapshots, fail-closed visibility, per-use audit — is the piece LA1
  should copy. The ordering study (median position of a later-used memory
  18.5 → 3.0, computed *offline over archived frames before deploying*) is the
  template for validating any landscape prior: retrodict on archived runs first.
  That is the same move as P-validated-R5's retrodiction step.
- **The zai-scribe is AC8 running.** Its mandate is mining student failures —
  cue-based self-correction detection, six-field rewrite rules, unresolved
  corrections kept as open challenge-lane questions. And the recursion AC8
  formalizes has already happened here manually: v1's deposits went 0/11 at
  review, and the failure of the failure-miner was itself mined into the v2
  card. AC8's draft-pattern minting is that loop with the human step replaced by
  a typed, reviewed sweep.
- **Typed absence works at this site too.** `:regulative` →
  `not-adjudicable-by-token`, `:unverifiable-depositor-provenance` exclusions,
  `:cannot-judge` as an apparatus hold rather than a verdict — the same
  refuse-loudly-with-a-reason discipline the C130 ruling installed in the WM,
  independently converged on. Where it was *missing* (503 → `:outcome :failed`
  indistinguishable from a weak student; visibility timeout → "cascade had
  nothing to offer") the Opus note shows days of measurement charged to the
  wrong cause. Same lesson as Figure 4's class 6: untyped absence is not
  neutral, it actively miscounts.

## 8. Dispatchable fixes (small, one file / one behaviour each)

For codex, in priority order; items D1–D4 are mechanical enough to bell now.

- **D1 — park-decision sync.** Make the parked-frame decision writer append its
  record to `frame-park-decisions.edn` as well as `queue-state.edn`, and
  backfill f73's entry. Acceptance: f73 appears in the file; a new parked
  decision lands in both places.
- **D2 — backfill `:memory-use/kind :regulative` on `e-63b7c7c1`** (the f34
  route memory's reviewed edge), so its seven-plus rows read
  `not-adjudicable-by-token` instead of `unknown`/`already-in-base`. One
  substrate edit plus one audit re-run. (Carried over from the last note.)
- **D3 — dangling pattern entities.** Three patterns routed by the cascade in
  f77/a2 do not exist as entities (`complex-arg-of-cpow-root`,
  `schwarz-disk-automorphism-formula`, `surj-via-oriented-root-preimage` per the
  Opus note). Discovery first: enumerate all `memory/assert` pattern endpoints
  with no entity, report count and sample. Fix is a second, separate handoff.
- **D4 — cascade status line per frame.** One entry in the close path recording
  `:cascade :ok/:failed/:excluded-all` per attempt, so condition boundaries stop
  being archaeology (last note's rec 3, still open).
- **D5 (larger, already specified by f78's own decision record)** — promotion
  visibility as a durable asynchronous operation with heartbeats and a global
  deadline, instead of a synchronous 33-minute sweep inside the regulator tick.

Decisions that stay with Joe (restated, not new): the eight rescued solves /
pin decoupling (§4); solver shelf (rec 6); size-matched control (rec 7); whether
sibling offers should carry the CA/CV `:source` lines (§5); whether C-6 remains
an arm at all after 500/0.

## 9. What I checked

- Frame dirs f71–f80: ledgers, `terminal/frame-terminal.edn` (f79 and f76 by
  hand), `live/close-frame.edn` `:receipt/result` vs terminal `:problem/outcome`
  (f77, f79), `queue-state.edn` `:parked` records for f72/f73/f78 in full,
  `frame-park-decisions.edn` (f73 absent), `memory-cascade-arm.edn`,
  f71 ledger's block-plan arm. Two Explore agents did the fan-out reads; every
  claim above that reached a conclusion was re-verified by me against the
  primary file, and two of their claims were corrected (f73 "silent stop";
  f79 "scoring artifact").
- Wired fingerprint audit artifact (89 rows; 22-row window extraction with
  origin/kind/verdict per row), status file, and 973881e6's diff/stat.
- `queued_frame_adapter.clj:300-350` (frame-result and problem-outcome
  derivation), `frame_fingerprint_audit.clj` git log, prereg amendments 6–9 in
  full, banked/rescued refs in apm-lean (24 banked, 13 rescued refs → 8 distinct
  stranded problems).
- Read in full: TN-fable-F66-F70-review, the f71–f79 sections of
  TN-opus-f47-observation (through the 09-02 hook correction).
- Not checked: Lean recompilation of any attempt (relied on receipts and Opus's
  rebuilds); live transcripts; whether f80 closes; the memory-system map's
  file:line cites in `promotion_pipeline.clj`/`memory_snapshot.clj` beyond spot
  reads (`memory_access_gate.clj:1-30`, `conductor.clj:22` confirmed).
