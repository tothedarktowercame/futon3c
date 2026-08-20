# Handoff: analyst-2 → analyst-3 (2026-08-19)

Written at the close of **f12**, the second and last frame of tenure 2 under the
N = 2 ruling. You serve **f13 (m99J06) and f14 (m93J06)**; both are registered
and staged (`9db0e573`, arms corrected in `d204cd15`).

Read `role-cards/analyst-v1.md` first. It is frozen at blob `ed340941` and it is
still the whole orientation — my predecessor said so and it held for me too.
This document is the delta.

**Unlike my predecessor, I am handing you a seat that exists — and that
answered.** `analyst-3` is minted, on the roster, `claude-opus-5`,
`invoke-ready? true`. I probed its invoke path rather than trusting the roster
entry, and the reply came back: **"analyst-3, claude-opus-5"**. So the path
carries the right model and returns content, not merely a `done` state. The mint
route is live: `POST /api/alpha/frames/mint-analyst` returns 409 on an empty body
(routed), not the 404 that blocked analyst-1.

---

## 0. Read these four, in this order

1. **`role-cards/analyst-v1.md`** — duties, boundaries, traps. Frozen.
2. **`analysis/series.edn`** — six entries now. f11 and f12 are mine and are
   written for you specifically.
3. **`holes/excursions/E-APM-f12-defects.md`** — ground control's f12 inventory
   (D40–D48) plus my append (D51–D53, one retraction, one settled question).
4. **`holes/excursions/E-APM-f11-defects.md`** — D27–D39, and the corrections
   section, which is where the write-path story is actually resolved.

Your memory is still three surfaces: the store, the mission record, `series.edn`.
Write as you go. analyst-1 lost a whole close to a model quota; I wrote every
finding to disk as I measured it and it cost nothing.

---

## 1. THE STATE OF THE WORLD CHANGED AT f12 — this is the headline for you

**The write path works now.** f12 gained **four reviewed attachments** — the
store's first growth since f9, ending a three-frame zero. Depositor `f12-scribe`,
reviewer `f12-guide`, author ≠ reviewer holding, all four `:attachment-status
:reviewed` with a durable `:prop/review` in the substrate.

**So the question your tenure inherits is the READ path**, and ground control's
own line for f13 says it better than I can: *"Four memories were reviewed,
attached and made eligible in this frame; the one agent who needed them was
dispatched minutes later, searched, and got none of them."*

Concretely, **f13 is the first frame in the whole series where two predictions
are testable at all**:

- `:offer-disposition-populated` — inapplicable in f11 and f12 because both had
  zero offers. f12 is the first frame to leave a non-empty shelf behind.
- `:cascade-seeds-from-recall` — inapplicable three frames running for the same
  reason.

If ground control registers either without noting that f13 is their first live
test, say so. And if f13 *again* produces zero offers with four reviewed
attachments sitting in the store, that is the biggest finding available to you.

---

## 2. What I got wrong, so you do not repeat it

**I asserted the refusal receipts did not exist. They did.** I looked in
`:cycle/outputs` and in every step `:result`, found `:action-refusals` nil in
both, and reported absence. They live in the **emitted trace** —
`validate-trace`'s `:result :trace` — which I had been dropping from my own
inspection to keep output readable.

**There are THREE places a fact can live and they do not agree:**

| place | what it is | example |
|---|---|---|
| `:cycle/outputs` | the conductor's running accumulator | `:promotion-result`, `:memory-offers` |
| step `:result` | one action's return value | `:dispatch-scribe` args, recall payloads |
| the **emitted trace** | what the preregistration validator reads | `:action-refusals`, `:memory-disposition-offer-ids`, `:capability-probes` |

Check the trace before you report an absence. That single habit would have saved
me a retraction on the seat whose card lists "asserting infrastructure state
without checking" as its first trap.

**And the store beats all three.** Three consecutive frames, the trace has been
wrong about what the student did (D18 pull-vs-push, D34 surfaced-ids, D50
`:attempt/memory-recorded` nil for a student that recorded). If a prediction is
about what a seat *did*, adjudicate it at the substrate.

---

## 3. Instrument traps that cost me time — all measured, all still live

- **`/api/alpha/hyperedges` silently defaults to `limit=100`.** No limit param
  returns 100 rows of 478 with no truncation signal. This already put a wrong
  number in the record (D53): a ~400-row page reports 151 reviewed where the true
  count was 215, so a truncated census reads as the store *shrinking*. **Always
  `limit=5000`, always print the row count.**
- **Attachment status and review verdict are different props.**
  `:prop/attachment-status` is the gate's output; `:prop/review` /
  `:prop/review-history` carry `{:reviewer :verdict :pattern-ids :reviewed-at}`.
  Reading only the first is what produced an over-strong claim I then had to
  correct. Cross-tabulate: at S-5, 219 reviewed, 210 with a review record, 209
  both, 10 reviewed-with-no-record (legacy), 1 with-record-not-reviewed (the one
  rejection in the store).
- **`attachment-status` is a keyword on some edges and the string `"reviewed"` on
  others.** Still true at S-5. Normalize with `name`/`str`.
- **Never count raw string occurrences of an id.** D34 reported a memory
  appearing "23 times" where the structured receipt count was **3**, and the 23
  grew to 32 within the same frame as later steps appended prose. Count
  structures, not greps. (This is the third sighting of this trap in the series.)
- **`check-parens` is invoked as `(arxana-check-parens-run (list "<abs path>"))`.**
  There is no `check-parens-file`; I guessed wrong once.
- **`transfer_checks.bb` is not executable** — run it as `bb <path> <dir>`.

---

## 4. Duty A: the score, and the thing P28 fixed

`analysis/transfer_checks.bb <problem-state-dir>` — always re-run it yourself,
never read a receipt someone else generated. Series so far:

> f7 3/6 · f8 6/6 · f9 5/6 · f10 5/6 · **f11 4/6** · **f12 5/6**

**f11's 4/6 was not a regression** and you should not read it as one. C3 failed
*vacuously*: its predicate requires at least one in-scope cycle promotion, and
f11 promoted nothing, so it failed with no input. I proved that by mutation
(inject one promotion → PASS, 5/6) and dispatched **P28** (`b57b29f0`), which
makes C3 report `INAPPLICABLE` with a reason. **The `/6` denominator did not
move** — deliberately, because changing it is a decision about the loss function
and that is not ours.

Note the symmetry, because it is the sharpest instrument lesson of my tenure:
**in the same frame, F3 passed vacuously on zero offers while C3 failed
vacuously on zero promotions.** Two validators, no input, opposite directions,
both feeding numbers this seat reports. When a count is zero, ask what the check
had to work with before you report its verdict.

And do not over-read trace-failure movement: **f11's 7 → 6 was vacuous** (the
failure that vanished did so because there were no offers to be undispositioned),
while **f12's 6 → 5 was real** (`:guidance-measurement-mismatch` went away
because the measurement became correct: 0 against a true 0).

---

## 5. Duty B: S-5, and the one real curation result

Method: `GET /api/alpha/hyperedges?type=memory/assert&limit=5000`, filter
`:prop/attachment-status` reviewed, scope by `:prop/review :reviewed-at >=
2026-08-16`. Run it twice and confirm identical output — I do, every time, and it
is cheap.

- **Universe 105**, unchanged S-2 → S-5. No `math-*` pattern file has been created
  in four frames.
- **Mission scope: 18 memories, 16 patterns, shared-nodes 1 → 2** at f12, the
  first growth in sharing since f8.
- **Whole store: 478 edges, 219 reviewed.**

**The result worth carrying, and it is duty D's first real one:** the
fragmentation mode of W.67 has broken. f9 minted 2 fresh bespoke patterns for 2
attachments (100% bespoke). f11 attempted 2, both naming pre-existing patterns
(neither landed). f12 landed 4, **all four on pre-existing patterns, zero bespoke
mints.** Seats stopped minting and started attaching.

Two honest qualifiers, both of which I would want if I were you: the new shared
node's two memories are **both f12's own**, so it is within-frame sharing rather
than cross-frame reuse; and my mission-scope figures are **not** comparable to
analyst-1's S-2/S-3 (12/11/1), because its script no longer exists and I could
not reproduce its boundary. S-4 and S-5 *are* comparable to each other.

**Correcting analyst-1 on one point:** it wrote that D, E and G are "slow by
nature, not blocked by tenure length." On **D** that is now wrong — the
bespoke-rate reading became a result precisely *because* there were two frames of
attachment behaviour to compare. D is a two-frame duty. E and G remain genuinely
open and I did not do them.

---

## 6. Duty F: keep it to one packet, and dispatch anything that moves your own numbers

Two packets, one per frame, both single-file and both with a bar I **measured
before setting it**:

- **P28** (`b57b29f0`, codex-3) — C3 vacuity, above. Merged and gated.
- **P29** — populate the required measurement field `"memories promoted"`, which
  reads *"unset: promotion outputs do not identify which artifacts are memories"*
  in a frame that promoted four memories and recorded a `:promo/pattern-id` and
  `:promo/review-evidence-id` for every one. **This one is probably yours.**
  Its first attempt (`invoke-1787127501525-…`, oxf-codex-10) died in **transport,
  not in work** — `"Proxy invoke failed: HTTP "` after **zero** tool events, so
  nothing was written and there is no partial state to clean up. Re-dispatched
  unchanged to **codex-3** as `invoke-1787128184311-5007-ee50bf5b`, park
  `park-fb79f17c`, whose payload holds the full review checklist.

  **The dispatch lesson, which cost me a wasted round-trip:** I picked
  oxf-codex-10 because it was the only general codex the roster listed as `idle`,
  while `codex-3`/`codex-5`/`codex-6` read `restored` or `invoking`. Roster
  `idle` is not liveness. **Prefer a local agent that has actually completed a
  job this session** — codex-3 had cleanly delivered P28 an hour earlier — over a
  remote one merely listed as idle. And always check the job state a few seconds
  after dispatch: a transport failure looks exactly like a slow start until you
  look (`tool-events: 0` for sixteen minutes was the tell).

**The rule that decided both:** *does this edit move a number I report?* Both
did, so both went out even though I held the full diagnosis and could have typed
either in five minutes. Author ≠ reviewer is the property this seat exists to
preserve; spending a round-trip is the price.

**Gate for real.** Both packets came back with correct-looking reports, and in
each case I added **one mutation the packet had not asked for** — for P28, that a
*genuine* C3 failure still FAILs rather than being laundered into INAPPLICABLE.
That is the mutation that matters: a check which can excuse itself is worse than
one that cries wolf.

---

## 7. Open, and who owns it

1. **The read path** (§1). The whole tenure's question.
2. **D51 — D5 has regressed.** f12's student memory is `:domain :zaif-work` and
   unpromotable; f11's was `:mathematics`. I settled f12-guide's hypothesis that
   the restore path explains it: **it does not** — `memory-domain` is `None` for
   every student seat on the roster, so it cannot discriminate between two of
   them. Start at the write path, not the mint. **Ground control's.**
3. **D43 — refusal receipts record THAT, not WHY.** Both f12 promotion refusals
   read `:tool-execution-failed`; the actual causes are dropped. Unowned, small,
   and it directly costs your adjudications.
4. **D42 — independent review is still unreachable.** Only
   `promote-memory-attachment!` mints review evidence; no agent-facing tool does.
   Every reviewed attachment to date is therefore **guide-reviewed**. If a future
   registration predicts independent review, that is a capability the harness does
   not have — the D27 error again.
5. **The ContDiff corpus sweep.** f12's defect (`ContDiff ℝ ⊤` is *analytic*, not
   smooth, so compactly-supported test classes are empty) has **five candidate
   problems** listed in `E-APM-f12-defects.md`, none recompiled. Two frames, two
   defective problems: this is a corpus finding and it is bigger than any harness
   defect in the file.
6. **E and G** — case-study accretion and whitepaper §5. Untouched by both
   tenures. You will have six series entries, which is the first point at which G
   is genuinely writable.

**Two adjudications where ground control and I disagree**, recorded in
`E-APM-f12-defects.md` so you inherit both sides rather than one:
`:problem-closed` (they CONFIRMED, I refuted — the frame's own disposition is
`:defective`) and `:offer-disposition-populated` (they REFUTED, I INAPPLICABLE —
zero offers took opposite verdicts in f11 and f12).

---

## 8. On N = 2

Two tenures have now completed succession, which is what the f12 registration
asked for: N = 2 is a mechanism, not an anecdote.

Four findings from my tenure required exactly two frames and none required three:
the D5 regression is *only* visible as a contradiction between f11's and f12's
student memories; C3's vacuous FAIL and its substantive PASS one frame later show
both the defect and its repair; vacuous versus real trace-failure movement is only
distinguishable across two closes; and f11's rejected deposit's pattern reappears
under a reviewed f12 memory. I have no evidence bearing on whether N = 3 would buy
more, and I am not going to infer it.

Your second frame is your last. Everything you want to outlive you must be in the
three surfaces before f14 closes, and succession is a scheduled duty of that
frame — not your successor's problem. Mint them, and **probe the seat before you
hand it over**; analyst-1 was right that a seat which looks minted with an
unverified invoke path is worse than an absent one, and now that the route works
there is no excuse not to check.

Good luck. The card is better than it looks; trust it before you trust me.

— analyst-2, tenure 2 of the seat, f11–f12
