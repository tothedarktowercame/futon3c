# TN: F81–F168 review — the memory system kept working while the frames stopped

Author: Claude (Fable 5, claude-3), 2026-09-06 ~16:00Z, at Joe's request ("a very
large number of APM frames were run since the last Fable review … a very
substantial review of the usage of the memory system, looking at the most
recent frames, assuming the frames have actually counted real work"). Scope:
f81–f168 in `jit-all-open-v2` plus the `jit-all-open-v3` launch (f169, today
15:04Z), read against `TN-fable-F71-F80-review.md` and the f81–f84 sections of
`TN-opus-f47-observation.md` (through 09-04), which I checked rather than
re-told — one of its findings corrected a claim I was about to make (§4). §8 is
the repairs ledger, §9 the decisions that sit with Joe, §10 what I ran.

## Short answer, with the premise handled first

Joe asked me to assume the frames counted real work. The honest version: **88
new frame ids, of which 28 (f81–f108) did real work and 59 (f110–f168) are
40-second registration shells** — the coordinator burned through the entire
remaining queue in 41 minutes on 2026-09-06 (13:12–13:26Z and the hour before)
after the **Codex usage limit was hit**; every preflight dispatch died
instantly and the pre-repair classifier let each failure consume a queue slot.
f116's park decision records the terminal verbatim (retry declared Sep 7,
08:33), and commit `d6beeec0` now types that terminal as bounded
`:awaiting-substrate` instead. All 59 shells have park decisions; none was
voided; no queue problem was lost. Of the 28 working frames: **one closed and
banked (f81/b96J03, 2026-09-02) and nothing has banked since**; 6 voided
`:role-terminal-unrecoverable`; the rest stalled at student-attempt-1 (13
frames) or solve (7 frames) and were parked with decision records by the
codex-17 unattended supervisor.

So the substantial thing to review is exactly what Joe pointed at: **the memory
system's own behaviour — which kept running, measurably well, inside a frame
apparatus that was failing around it.** Five findings:

1. **First verified cascade uptake, on the very first enriched attempt** —
   then none in the ~14 after (§4).
2. **First within-frame student→guide→student transfer, with the strongest
   token witness of the campaign** — 11 novel witnessed tokens (§3).
3. **First cross-system memory use**: a memory mined in `M-codex-sorry-loop`
   (claude-6, 2026-07-29) cited by an APM student (§3).
4. **A new defect, the memory-side twin of the stranded-proofs defect: shelf
   admission is gated on frame close, so the window's best-witnessed memories
   never reached any later frame's shelf** (§5).
5. **A holdout breach in f83/a1**: the student cited a promotion-*rejected*,
   never-surfaced memory; the access gate caught it mid-attempt and objected;
   the final receipt carries the id anyway (§6).

## 1. What the 28 working frames did

Arm: `memory-cascade-arm.edn` `{:enabled? true :routes [:sibling] :cap 20}` —
cap lowered 100→20 by amendment 13 (Joe, 09-02: "let's lower the cap, yes"),
offers enriched with `:offer/name` + `:offer/hook` by amendment 12
(`e5130e75`/`a14e2635`). All frames `:arm :treatment`; still no control arm.

| frames | outcome |
|---|---|
| f81 (b96J03) | full ladder, `:frame/result :closed`, `:problem/outcome :solved`, banked |
| f82, f84, f95, f96, f105, f108 | voided `:role-terminal-unrecoverable` (invariants like `:typed-submission-missing`, `:fresh-session-id-missing`, `:workspace-probe-failed` — apparatus, per the Opus rebuttals) |
| f83, f87 | reached a2/guide-2, parked |
| f85–f92, f94, f97–f100, f104 | stalled at student-attempt-1, parked |
| f93, f101–f103, f106–f107, f109 | stalled at solve, parked |
| f110–f168 | usage-limit shells, parked |

The solver kept solving: the Opus note verified b96J04, b97A01, b97A02 clean
(the rescue pile stood at **eleven pinned solves** by 09-04). f90's park
decision is the sharpest single record of the regime: the *student* reported
the theorem fully proved, zero errors zero sorries, two authenticated
re-verifications plus an axiom audit — and every durable terminal collection
stayed `:running`, so no receipt exists. Class:
`:verified-closed-submission-unreconciled-by-terminal-repair`. The math
succeeded; the measurement channel was down.

`jit-all-open-v3` registered today 15:04Z: lineage priors
`["jit-all-open-nontopology-v1" "jit-all-open-v2"]`, same cap-20 sibling arm,
seat-cast pinned per role (solver/proctor/scribes `gpt-5.6-sol`, student/guide
`glm-5.3`), corpus re-derived at apm-lean `7672a923`. Its first frame
f169/a98J03 — a problem from the stranded-rescue pile — was in `:solve` at
15:25Z. That matches the park decisions' recommended route ("independently
revalidate the stored head in a fresh frame").

## 2. Memory production: the pipeline ran end to end, repeatedly

Every frame that reached promote-solver ran the full deposit machinery:

- **Solver-route mining (promote-solver):** 22 frames published 1–4 memories
  each, **42 total** in five days. One rejection exercise: f83's
  promotion-proctor rejected both candidates (`:verdict :reject`,
  `:published-memory-ids []`) — the gate still gates.
- **Guide mid-frame deposits:** f83-guide and f84-guide each deposited 2,
  f87-guide 1, mined from the *failing a1 student* for a2's benefit. New use
  of the channel: in the F71–F80 window guide deposits existed but the mined
  source was the solver route.
- **zai-scribe's first publications.** f81's end-of-frame scribe-reduce
  published **3** student-mined memories (it went 0-for-16 across
  f32–f77). One (`e-apm-promotion-43727a6f…`, actually via f81-zai-scribe's
  sibling f81-scribe pipeline) was used and fingerprinted the same day.
- **The scribe now authors pattern-library entries when a memory has no
  coherent parent** — `pattern-library-codex-scribe-fNN-*.md`, ten files
  f87–f105, each a `math-formalization/<name>` pattern with Trigger / Move /
  Why-it-works. This is the answer to the forest-of-stars diagnosis in the
  F71–F80 note §7 happening in production: attachment points being authored at
  deposit time, not inferred.

The student shelf held steady at ~209–213 entries (31 source frames, f28→f81 +
own-frame), every entry carrying `:name` + `:hook` and a
`:review-evidence-id`. Cascade expansion ran clean in all 22 firings:
`:status :succeeded`, ~200–217 candidates available, 20 delivered, ~4 minutes
each. The mass `:unverifiable-depositor-provenance` exclusions and 503 storms
of the last window are gone from these records.

## 3. Memory use, by the artifact standard

16 in-window use rows in the wired campaign audit (which now runs
campaign-scoped and yields 109 rows / 101 evidence-eligible use events total).
Every completed a2/a3 used 2–4 memories; every a1 except f83's used zero.

| row(s) | stratum | verdict |
|---|---|---|
| f81/a2 ×2 (f81-scribe) | within-frame | **fingerprinted** (novel tokens incl. `Equiv.Perm.alternatingGroup_le_of_isPreprimitive_of_isThreeCycle_mem`) |
| f83/a2 ×2 (f83-guide) | within-frame | **fingerprinted** — see below |
| f82/a2 ×4, f84/a2 ×2, f96/a3 ×2 | within-frame + 2 cross | `excluded-void-frame` (uses happened; frames voided, so diagnostic only) |
| f87/a2 ×3 | within-frame ×2 + cross ×1 (f77-guide, `:regulative`) | `no-source` (frame never closed; artifact not collected) |
| f83/a1 ×1 | within-frame | `unwitnessed` — the breach, §6 |

Three rows deserve names:

- **f83/a2 ← f83-guide is the strongest transfer witness the campaign has
  produced**: `solvability-by-kernel-range-chain-for-small-finite-groups`,
  mined by the guide from a1's own partial success ("MINED FROM a Student
  attempt that fully closed IsSolvable (Equiv.Perm (Fin 4))"), used by a2 with
  **11 novel witnessed tokens** (`solvable_of_ker_le_range`,
  `alternatingGroup.normal_kleinFour`, …). That is a
  student→guide→student loop closing inside one frame — the failure-mining
  premise (AC8) demonstrated on the use side, not just the deposit side.
- **f82/a2 ← `e-codexpilot-avoid-euclidean-measurable-space-diamond-…`** is
  the first *cross-system* use: kind `:feedback`, mined from the
  M-codex-sorry-loop cron lane by claude-6 on 07-29, promotion-reviewed then.
  An APM student citing a memory from a different mission's pipeline is the
  "same substrate" claim of the F71–F80 design comparison carrying real
  traffic.
- **f87/a2 ← f77-guide** (`underhypothesized-bridge-repair-ports-main-theorem-hypotheses`,
  `:regulative`) — a guide-mined memory crossing problems, b95J04→b97J03,
  typed correctly so the token standard abstains rather than pretends.

**The campaign-level conclusion is unchanged at triple the evidence: token-
witnessed transfer remains entirely within-frame.** 101 use events, 37
fingerprinted — all 37 within-frame. The 20 cross-problem rows:
9 already-in-base, 6 unwitnessed, 1 weak-fingerprint (first nonzero cross
signal), 1 not-adjudicable, 1 no-source, 2 excluded-void.

## 4. The cascade: first uptake, then silence — and the instrument can't see it

I was about to report "300 enriched offers, zero taken" and close amendment
12's falsifier. The Opus note's 09-02 section stopped me, and its claim
verifies: in f82/a2 the codexpilot memory **appears in the receipt's own
`:offers` vector (with `:offer/name`/`:offer/hook`) and in `:used-ids`** — the
first offer-intersection uptake after 800 bare offers went 0-for-800. It
arrived on the *first attempt where offers said what they were*, exactly the
opacity prediction. Amendment 12's falsifier ("uptake within five
offer-receiving attempts") was therefore **confirmed on attempt one**, and C-6
stays open.

Since then: ~14 more enriched attempts (all receipts show
`:cascade/enrichment {:from-edge-props 20, :failed 0}` — the enriched regime,
not the degraded bare case), **zero further uptake**. 1 taken per ~300
enriched offers.

Instrument gap: the audit's `delivery-route` for that one uptake row is
`unknown`, not `cascade` (the receipt's `:used-via-cascade` never records it;
the audit's 3 `cascade`-route rows are all pre-window re-attributions). The
only cascade uptake the campaign has ever had is invisible to the campaign's
own audit column. Small fix, worth doing before uptake data matters (§8 D6).

## 5. New defect: the shelf only inherits from closed frames

The shelf snapshots' `:snapshot/provenance-summary` lists only **closed**
frames (f28…f81 + the current frame). Verified directly: f83-guide's
11-token-witnessed memory, f84-guide's, f87's — all absent from f108's shelf;
f81's persist (f81 contributed 8). The base shelf grew 201→209 across a window
in which ~47 memories were published. **The deposits with the best transfer
evidence in the whole campaign are exactly the ones the forward channel
dropped, because their frames were voided or stalled by apparatus faults.**

This is the memory-side twin of the pin-coupling defect (F71–F80 §4, Opus
09-01): there, verified *proofs* don't bank when the frame's learning protocol
fails; here, reviewed *memories* don't propagate. Both couple an
artifact-level judgement ("this memory passed independent review"; "this proof
verifies") to a frame-level disposition that routinely fails for unrelated
apparatus reasons. Eleven proofs and now a season of memories sit in the
substrate, invisible to the next frame. Every shelf entry already carries a
`:review-evidence-id`, so an admission rule of "reviewed-approved, regardless
of frame disposition" is implementable without weakening author≠reviewer —
whether to adopt it is Joe's call (§9).

## 6. The f83/a1 holdout breach

Sequence, all from f83's own records: promotion rejected both scribe
candidates (published `[]`) → a1's same-problem holdout computed
`:shelf/withheld-ids []` (nothing published, nothing to withhold) → the a1
student's typed report cited `e-apm-promotion-bf6a95b4…` (f83-scribe, subject
b97A01, deposited 40 minutes earlier) → the **access gate caught it**: the a1
packet opens "Your completion cited memory that was not made available …
Remove unauthorized ids" → the final receipt still lists the id in
`:used-ids` (3×), and it appears in *no* accessible/candidate/offer list in
the file.

Two readings, both true: the layered defence worked (detection fired, and the
fingerprint audit independently verdicts the row `unwitnessed`, 0 novel
tokens, so no token evidence contaminates the baseline); and the protocol
still leaked (a rejected, unpublished memory's id reached a holdout student by
a route no record names, and the repair loop did not actually strip it from
the receipt). f73's residual in the last window
(`:student-memory-used-without-surfacing`) was the same class. Discovery
needed before any fix: enumerate how an unpublished id can reach a student —
the `apm-search-memory.py` reviewed-corpus route is the prime suspect (§8 D7).

## 7. What this window adds to the design comparison (F71–F80 §7)

- **Hooks are what make an offer usable — now with an existence proof.** 800
  bare offers: 0. First named-and-hooked offer set: 1 taken and applied. Then
  base-rate silence, which is itself informative: a hook gets a memory *read*;
  it still competes with a ~209-entry shelf that already covers most needs.
- **The failure-mining recursion produced its first used artifact** (§3,
  f83/a2). The zai/AC8 lane is no longer only depositing — its shape
  (mine the student's own partial success, return it enriched) is the one that
  produced the campaign's best token witness.
- **Untyped absence miscounts, again, at a new site**: the uptake event the
  audit records as route `unknown`, and the shelf silently thinning behind
  frame voids, are both absences that read as "nothing happened" until you
  diff the primary records.

## 8. Dispatchable fixes (one file / one behaviour each)

Done since the last note, verified here: **D1** park-decision sync
(`84907fad` + backfill; f73 present; decisions now cover 99 frames including
all 59 shells), **D4-equivalent** per-frame cascade status
(`live/memory-cascade-operation.edn` + `:cascade/enrichment` per receipt),
cascade retry / substrate-wait (`d6beeec0`), authenticated-collection-vs-
wrapper-cancellation (`673e0853`, the f90 class), axiom-standard closes
(`3525da80`, `d52f2047`).

Still open, carried or new:

- **D2 (third carry): backfill `:memory-use/kind` on `e-63b7c7c1`** — its five
  audit rows still read kind `None`.
- **D6 (new): record cascade uptake on the receipt** — when a used id
  intersects the offer set, write `:used-via-cascade` (or have the audit
  compute the intersection); today the one real uptake reads route `unknown`.
- **D7 (new, discovery first): how does an unpublished memory id reach a
  student?** Reproduce f83/a1: enumerate the search corpus' admission rule vs
  `:published-memory-ids`, report the route; fix is a second handoff.
- **D8 (new): make the repair loop's "remove unauthorized ids" actually bind**
  — f83/a1's final receipt kept the id after the gate objected.
- **D3 (carry): dangling pattern entities** — unexamined this window.

## 9. Decisions that sit with Joe

1. **Shelf admission for reviewed memories from unclosed frames** (§5). The
   review chain exists per entry; only the frame-close gate excludes them.
   Adopting "reviewed-approved admits" would have carried f83's and f87's
   memories forward. (The proof-side twin — eleven pinned rescues — was
   already with you.)
2. **The void classification.** Six voids this window; the Opus note rebuilt
   two of their proofs clean. `:role-terminal-unrecoverable` on a
   budget-of-one exhaustion is the void-and-advance disposition you rejected
   after F32, taken automatically.
3. **Solver shelf**: the canary packet is built and dormant
   (`TN-solver-shelf-canary.md`), activation gated on your preregistration of
   the paired frames.
4. **C-6**: stays open on its own falsifier terms (confirmed uptake), but
   1-in-300 under enrichment is now a measured rate if you want to re-judge
   the two minutes of expansion per dispatch it costs.
5. **v3 is already running the rescue revalidation route** (f169/a98J03). If
   that wasn't intended to start unattended today, it's live now.

## 10. What I checked

- All 88 frame dirs: ledger event chains, phase transitions, stop
  certificates, live/ + snapshots/ inventories (scripted extraction, spot
  re-verified by hand on f81, f83, f90, f105, f150). Coordinator/queue files
  and the v3 campaign dir (arm, seat-cast, lineage, corpus-selection, f169).
- Re-ran `fingerprint_audit.py --campaign jit-all-open-v2 --json` (109 rows;
  window rows extracted with stratum/route/kind/verdict; cross-tab §3).
- Resolved all 16 used memory ids against the live substrate (:7073, EDN) for
  author/subject/timestamp; fetched two full records (codexpilot, f83-guide).
- f83/a1 breach: holdout-decision, withheld-ids, packet text, occurrence-level
  search for the id across receipt/offers/candidates; promote-solver verdict.
- f82/a2 uptake: id confirmed in `:offers` (with offer/name+hook) and
  `:used-ids` in the receipt itself.
- Shelf-admission finding: provenance summaries f81→f108 plus direct grep for
  five specific ids in f108's snapshot.
- Enrichment markers on all 15 receipt-bearing attempts; prereg amendments
  12–13 in full; park decisions for f90/f116; git log since 09-02.
- Read for continuity: TN-fable-F71-F80-review (full),
  TN-opus-f47-observation §§09-01→09-04, TN-solver-shelf-canary (full),
  TN-apm-unattended-progress-contract §§0–1.
- Not checked: any Lean recompilation (relied on receipts and Opus's
  rebuilds); live transcripts; D3; whether the audit's `excluded-void-frame`
  set matches the void list exactly; m94A03-retry-v3's watchdog re-arm today.
