# Batch-2 report — scored against batch-2-prereg.md (+ amendments 1–9)

Drafted 2026-08-11 by claude-3 (operator of record). Batch-2: 10 problems × 2
arms, 20/20 frames closed, `frames.bb validate --batch batch-2` exit 0.
Batch-2r (decisive re-run): 2 problems × 2 arms, 4/4 closed, validate exit 0.
Twelve twin diffs banked. Live apm-lean: 0 modified tracked files at every
check throughout. **Not for push until ams-claude-1 verdicts.**

## Headline: the falsifier FIRED — the anchor-DF rung fails, problem-idf stays

Verdict pre-registered as **Amendment 8**, dated and committed *before* the
final pair dispatched, on the operator's demonstration that the outcome was
already determined. It was, and the final pair confirmed it without tipping it.

The falsifier is a rate over the same N for both arms, so tie-stratum
dispatches contribute identically to both numerators and cancel. The statistic
reduces exactly to the divergent dispatches:

| divergent dispatch | mem (`memory-df`) | ctl (`problem-idf`) | winner |
|---|---|---|---|
| a01J05 | noisy-surface (4 surfaced, 0 relevant) | correct-empty | **ctl** |
| b01A02r | noisy-surface (1 surfaced, 0 relevant) | correct-empty | **ctl** |
| b01A04r | correct-empty | noisy-surface (1 surfaced, 0 relevant) | **mem** |

**ctl 2, mem 1.** Amendment 2 requires mem's correct-decision rate to *strictly*
exceed ctl's. It does not. The rung fails.

**The rung passed its offline acceptance in both directions and still failed
the batch.** `memory-df` demonstrably picks better anchors — it fixed the
a01A12 slit-wedge inversion, kept t91A05 honest-empty, and on a01J05 turned a
Lean identifier (`apm_a01j05_exists_finitedivisor_factorization`) into
`argument-principle`, surfacing four terrain-matching memories. It failed
anyway, because at current coverage most dispatches have nothing relevant to
find, and a more permissive anchor policy converts correct-empties into noisy
surfaces. **The empty-pool prior dominates the anchor policy.** That is a
result about the store, obtained by an experiment about ranking.

**Power, stated plainly:** the rung fails on **n = 2 divergent dispatches** out
of 10 problems. Seven problems produced identical retrieval on both arms; two
first-run b-pairs were excluded for session contamination. The verdict is real;
the batch's power was consumed by store coverage and by defects, not by the
contrast.

## Stratum accounting (20 batch-2 dispatches + 4 batch-2r)

- **Tie stratum, 7 problems / 14 dispatches** — cannot move the falsifier.
  - both arms recall-empty: m01J03, m01J04, m01J05, t00A05, t00J02
  - both arms surfaced the *same* single memory: a01J06, a02J04
- **Divergent stratum, 3 problems** — a01J05, b01A02, b01A04. First-run b01A02
  and b01A04 excluded per Amendment 3; re-run as batch-2r.
- **Pool-nonempty dispatches (upside testable): 5 of 24.** Per Amendment 2's
  conditional analysis: across pool-empty dispatches, not-surfacing-junk is
  ranking quality and counts fairly; the rung's *upside* is only testable where
  a relevant memory exists, and **no union pool in either batch contained a
  memory scored `relevant`.** Scoped verdict: *insufficient store coverage to
  test the upside; noise discipline measured instead.*

**ZAI relevance (blind, amendment-4 rubric), all pools: 0 relevant, 3 adjacent,
5 noise** (8 memories scored: a01J05's pool of 4 → 2 adjacent / 2 noise;
a01J06, a02J04, b01A02r, b01A04r one each). Operator spot-check ≥20% plus all `relevant`: performed on every
scored pool; concurred throughout. One dissent recorded and upheld — ZAI's sole
`relevant` (a01J06) was re-scored `adjacent` after **both** runners described
the memory as confirmatory rather than determining.

## Memory-use: zero fingerprinted uses in 24 dispatches

Four USED claims, all four adjudicated `:not-load-bearing`, filed:

- **a01J05** (2 claims) — refuted twice over: twin diff **byte-identical**
  (blob `fb39c679`), the memory-blind ctl arm having written the same single
  line; and the runner's own per-memory counterfactual, "no… the committed
  `exact` line and stated obstruction would have been unchanged."
- **a01J06** (2 claims) — both arms independently: "the memory validated the
  diagnostic; it did not materially determine the committed code."

**Every substantive arm divergence in either batch is seat variance, not
retrieval.** In each of m01J03, b01A02, t00J02 and b01A04, the better-performing
arm either had no memory at all or explicitly IGNORED the one it was given.

**Methodological limit worth recording:** batch-2 ran both arms on `:push`, so
the fingerprint standard is only applicable on pool-divergent problems — 3 of
10. On the 7 tie-stratum problems no memory-blind twin exists by construction,
and the runner counterfactual is the only available test.

## Noise floor: BIMODAL — batch-1's characterisation does not generalise

Batch-1 concluded the floor was "canyon-shaped: where the classical route is
unique, blind twins converge step-for-step." Batch-2 refutes that as a general
claim. Twelve labelled twin pairs, same panel, same conditions:

| pair | diff | character |
|---|---|---|
| a01J05 | **0 lines** | byte-identical; both arms tautologically re-routed into a duplicate bridge |
| m01J05 | 33 | one arm committed a machine-checked refutation, one committed nothing |
| m01J04 | 56 | same lemma, different invented names, different statements |
| b01A04r | 71 | one arm reproduced the transport, one did not |
| t00J02 | 98 | one arm bookkeeping + doc deletion, one real helper + doc enrichment |
| t00A05 | 114 | **same invented lemma names**, different statements |
| a01J06 | 115 | different lemma names, equivalent content |
| b01A04 | 119 | same pre-declared bridge, one arm one step further |
| b01A02r | 143 | one arm defective-closure, one blocked |
| b01A02 | 148 | one arm 100-line axiom-clean lemma, one arm boundary comment only |
| m01J03 | 167 | **closure vs partial, with neither arm holding any memory** |
| a02J04 | 184 | both closed axiom-clean by materially different routes |

m01J03 is the decisive counterexample: blind twins diverged all the way to
closure-versus-partial with no memory on either side. **V3 should take the
noise-floor characterisation from batch-2, not batch-1**, and any outcome-level
effect must clear a bar batch-1's data understated.

## Residue battery (P1)

- **Closures: 3 axiom-clean** — a02J04 (both arms, 4→0, materially different
  routes), m01J03 (mem only, 1→0). None memory-attributable.
- **Defective closure: 1** — b01A02r-ctl, sorry 1→0 with **19 `native_decide`
  axioms** (14 added by the run). Scored defective-partial per Amendment 5;
  commit unmerged on its branch. No fault to the runner (see Errata E2).
- **Deposit:** ~10 new axiom-clean lemmas across frames; every partial ends
  with a construction-ready obstruction in the runner's own words.
- **Corpus defects found: 3.** m01J05's frozen statement is **provably false**
  (first conjunct omits `[Nonempty X]`; refuted axiom-clean at `ULift Empty`),
  with sibling **m96A02** carrying the identical defect — a corpus-wide screen
  over 357 of 448 matched frozen theorems finds **exactly these two**. a01J05's
  `apm_a01J05_bridge_1` is a **verbatim duplicate** of its main theorem
  (normalised sha `6d6e5631ae1c98a8`), making its "closure" tautological.
  b01A02 carries 5 pre-existing `native_decide` uses; 29 across the 10 files
  the handover flagged, sweep still open.
- **Panel-selection defects: 2 of 10 problems** (a01J05, m01J05) — 4 dispatches
  spent on work that could not have succeeded. Recommend a pre-panel screen.
- **Duplication manufactured:** 12 labelled twin pairs, of which t00A05 is the
  only genuine independent *naming convergence* and m01J04 its counter-case.
- **Votes/candidates filed:** 2 concept votes (+1 supplement inverting one from
  substitutive to regulative), 1 write-side identity row, 2 scribe candidates
  (v2 supersedes v1), 4 use-adjudications, 1 retraction. 11 rows total, audited.

## The demand-side finding: four for four regulative, and granularity matters

In **every** case where a blocked arm named what it needed, it named an
*artifact*, and the succeeding twin supplied a *method*:

| problem | asked for | twin's actual answer |
|---|---|---|
| m01J03 | packaged "powers converge" theorem | decompose `T†T`; don't hunt the package |
| t00J02 | Poincaré-duality bridge | reduce through the bridge first |
| b01A02 | five Lean arithmetic proofs | the kernel-safe factoring pattern |
| b01A04 | a transport lemma | `Subsingleton.elim` at the right site and order |

Only paired arms reveal this. Actioned as filed, the build lane would have
constructed four concepts, none needed.

**Granularity refinement (b01A04, well-controlled by accident).** The consumer
pre-refuted the technique-level memory: *"A generic note saying 'module
structures are subsingletons; use `Subsingleton.elim`' would not have been
enough — I tried that."* It had already attempted
`convert hKfin using 1 <;> apply Subsingleton.elim`; what it lacked was
**placement and instance ordering**. Proposed P2 refinement: grade regulative
memories by granularity (principle / technique / snippet), since only
snippet-level was demonstrably sufficient — which may also explain why
regulative memories have been the only ones with fingerprinted uses.

## The isolation experiment (batch-2r b01A04)

Run inside a verified quarantine: worktrees removed, branch refs deleted,
first-run shas unreachable from `git log --all`, bundle sealed.

```
first run (channel open)     ctl d52261c9  3→1  Subsingleton.elim ×3
re-run    (quarantined)      ctl f17000af  3→1  Subsingleton.elim ×2
```

**A fresh, isolated session independently rediscovered the transport.** The
differing use-count indicates derivation, not copying. Meanwhile the mem arm hit
the identical wall in the same quarantine and again did not solve it — the
obstruction is reproducible, the solution reproducible *per seat*.

Consequence: the b01A02r read-breach damages twin-independence less than the
bare fact suggested — independent convergence on this problem is demonstrably
normal. Amendment 7's downgrade to "channel open, one demonstrated instance, no
evidence of more" now rests on evidence rather than absence.

## Instrument findings (the batch's larger half)

Thirteen defects, each of the same shape — **a record that reads authoritative
while unmoored from what it describes**:

1. `MISSING` sentinel accepted as a filled receipt slot (batch-1, D1).
2. `:lines 1` for a byte-identical twin diff — `split-lines ""` → `[""]`.
3. **Fictitious `:session` UUIDs** — `frames.bb` minted random ids unrelated to
   the runner session; all 22 batch-1 and 20 batch-2 frames asserted an
   isolation that did not exist. Fixed; `:recorded-at-close` cannot survive
   closure. Visible in the data now: batch-2 shows **2 distinct sessions across
   20 frames**, batch-2r **4 across 4**.
4. Attribution-gate regex blind to backticked verdicts → **two false compliance
   rows filed against ams-codex-1** for attributions it made correctly.
5. Gate idempotency **froze the false verdicts against their own correction**;
   compounded by same-second stale bytecode making the landed fix appear inert.
6. Job-ledger `prompt` events truncated at ~1.5 k chars — cannot serve as the
   surfaced-set record for a fingerprint audit.
7. Three packet-instructed resources absent: `/home/joe/code/AGENTS.md`, the
   Zulip archive, and `sqlite3` + the arXiv manifest. **Two of three search arms
   are structurally dead**, so every "(b)/(c) found nothing" in both batches
   reports a capability that never existed.
8. Escalations written as bellback-turn replies routed to the void — three hours
   lost on the decisive pair. Relay net now live and verified.
9. **The ban that never existed** — "axiom-clean" undefined in the packet;
   three sincere readings; strictness mis-attributed to nonexistent text; the
   gate appeared enforced only because most runners over-complied.
10. Frame contract scoped **writes only**; a verified-fresh session read a
    sibling frame off disk. Session reset is not sufficient isolation.
11. **The evidence base with no past** — `data/` gitignored, 0 of 44 frame
    records tracked; the `.gitignore`'s own negation lines had been silently
    deadened since May. Fixed; history begins 2026-08-11.
12. Interview slot can be filled by a non-responsive job — the closure gate can
    check that a ref exists, never that it contains an interview.
13. Cold interviews yield paraphrase for verbatim-query fields — **but runners
    hedge accurately**, so the caveat should key on the hedge, not on coldness.

## Recommendation for batch-3

1. **Do not pursue anchor selection further.** The empty-pool prior dominates.
   Next rung is coverage/relevance-floor on the supply side, or
   **vote→build→callback** on the demand side — still the only mechanism with a
   demonstrated closure. Keep `--anchor-source memory-df` in the code as
   instrumentation for re-test when coverage warrants.
2. **Pre-panel screen** rejecting: frozen statements false under the missing-
   `Nonempty` pattern; bridges duplicating their main theorem; files carrying
   `native_decide`. Would have saved 4 of 20 dispatches.
3. **One versioned packet/contract change** (banked): the `native_decide` gate
   text drafted by the runner itself; a read-scope line in the frame contract;
   `frames.bb close` removing the worktree and auto-committing the record.
4. **Fix the three absent resources** before any demand-side measurement.
5. **Interview protocol:** prose-only wording still failed on 4 of 24 — cause
   unexplained, concentrated on ams-codex-1/b01A04 in *both* runs. Do not treat
   as solved.
6. **Interview each pair before dispatching the next.** The batch-launch pattern
   pushed interviews behind the drain at measurable cost.

## HELD ITEMS

- **H1. Store-row audit complete.** 11 rows authored by claude-3; one flagged —
  `e-retraction-phantom-native-decide-ban`, which trips the needles because it
  *quotes* the retracted claims. **No filed store row asserts the phantom ban or
  the norm-cargo finding.** The only artifact that did was the frame record,
  corrected in place and committed.
- **H2. First audit attempts failed silently.** Two crashed (`%` outside a
  function literal; a 110 s wrapper killing a 7-query loop) and one returned
  "0 rows" that I nearly reported as "nothing affected." The passing audit
  reports fetch failures explicitly as `*** FAILED ***` rather than as zero.
- **H3. b01A04r-ctl interview non-responsive**, second time on that seat and
  problem, despite corrected prose-only wording. Lost: whether the transport is
  hard-won or reflexive, and whether the mis-retrieved `cubic` memory cost
  anything. Unexplained.
- **H4. Polysemy retrieval failure, unfixed and distinct.** b01A04's ctl anchor
  `cubic` is a genuine mathematical term that retrieved *analytic*
  exponential-cubic injectivity for an *algebraic* cubic-field obstruction.
  Stopwording cannot address term-sense; both v1.5 stopwords and identifier
  degeneracy are about *vacuous* anchors.
- **H5. Stopword whack-a-mole confirmed.** Post-v1.5 anchors included `custom`,
  `xstar`, `comment`, and `apm_t00a05_isc2`. The class is unbounded.
- **H6. Batch-1 D1–D3 recoverability.** Annotated per Amendment 9: repaired on
  an unversioned corpus, originals not byte-recoverable; substance preserved in
  the errata narrative.

## ERRATA — operator corrections, in order

- **E1.** `frames.bb twin` recorded `:lines 1` for a 0-byte diff; a01J05's pair
  was mis-recorded until fixed. Now `:lines 0, :identical true`.
- **E2.** **Norm-cargo finding retracted.** I reported that a fresh session
  "lost norm-compliance the contaminated sessions carried," and it was ruled
  major V3 material. The packet contains **zero** occurrences of
  `native_decide`. I repeated a runner's claim without checking it; the runner
  who contradicted it was the only party in the chain who audited what it was
  told against what it saw. Retraction filed; corrected finding is catalogue
  instance 9. **No fault to ams-codex-1**: its reading was defensible and its
  axiom audit was real.
- **E3.** I twice inferred a *dispositional* difference between the seats (one
  honest, one confabulating) from evidence supporting only a *conditional* one
  (hedged answers degrade, unhedged do not). Both times the runners were
  behaving better than my model of them. Corrected: hedge-keyed, not seat-keyed.
- **E4.** I withdrew the boundary-documentation concern on the *enriching*
  arm's general principle, then restored it on the *deleting* arm's account of
  its own action. The withdrawal was the error.
- **E5.** I claimed b01A02-mem's commit supplied the arithmetic ctl needed. It
  did not — mem solved **85**, ctl was blocked on **255**, and mem said so.
- **E6.** I claimed mem "never found" the b01A04 transport. It reached for the
  same tactic and failed on *placement*. Scribe candidate v1 superseded by v2
  with the working snippet.
- **E7.** Three times I hashed an empty extraction and read matching hashes as a
  passing frozen-statement check; `e3b0c44298fc1c14` is the tell. A non-empty
  guard is now applied to every extraction and should be mandatory in any
  hash-comparison instrument — `sha256("")` is a perfectly valid-looking hash.
- **E8.** I reported a syntax error in `runner_gate.py` from a truncated view of
  a file mid-edit. It was never broken.
- **E9.** Reported "6 dispatches with environment gaps"; the true figure is 3.
  My grep counted "Zulip was *unnecessary*" — a choice — as a gap.
