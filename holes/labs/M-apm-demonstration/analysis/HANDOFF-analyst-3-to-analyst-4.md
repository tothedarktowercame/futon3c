# Handoff: analyst-3 → analyst-4 (2026-08-20)

Written at the close of **f17**, the second and last frame of tenure 3. You are
minted, on the roster, and — this matters more than the roster entry — **you
answered**. I probed your invoke path before writing this
(`invoke-1787239902600-5285-bb6f67a9`, state `done`) and the reply came back:
**"analyst-4, running on Claude Opus 5 (1M context)."** The path carries content,
not merely a `done` state. Three handovers have now held that rule; analyst-1 set
it when the mint route was unrouted and refused to hand-assemble a seat it could
not verify.

Read `role-cards/analyst-v1.md` first. It is frozen at blob `ed340941` and it is
still the whole orientation — that is now three seats saying so. This document is
the delta.

---

## 0. Read these, in this order

1. **`role-cards/analyst-v1.md`** — duties, boundaries, traps. Frozen.
2. **`analysis/series.edn`** — **ten** entries now. f15 and f17 are mine.
   f13 and f16 are void reports, not closes.
3. **`analysis/HANDOFF-analyst-2-to-analyst-3.md`** — my predecessor's. Still
   largely correct; §3's instrument traps are still live except where I flag
   otherwise below.
4. **`frame-17-frame/`** and the f17 registration — the vendor-rotation frame.

Your memory is still three surfaces: the store, the mission record, `series.edn`.
**Write as you go.** analyst-1 lost a whole close to a model quota. I wrote every
measurement to `/tmp/apm/f17-findings.md` as I took it and it cost nothing.

---

## 1. THE HEADLINE: the read-path diagnosis changed under me

My tenure inherited one sentence from analyst-2: *promotion works, retrieval is
the gap.* **f17 does not support it, and you should not inherit it uncritically.**

- **f15** supported it. The issued query was `exponent OR closing OR conjunct OR
  existence` — four prose words, not one naming the problem's mathematics (that
  is f15-guide's D60). Against that junk query the cascade still returned 105
  distinct memories and none were attested used.
- **f17's query is GOOD**: `hessian OR apm-m97a06 OR euler-lagrange OR variation
  OR ...`. Query construction improved between the frames. And recall returned
  `:recall-empty`, `proposal-count 0`.
- **Because the terrain is absent.** I measured it rather than inferring it: of
  538 `memory/assert` edges, the number containing *euler-lagrange* is 0.
  *variational* 0. *stationary* 0. *lagrangian* 0. *pendulum* 0. *geodesic* 0.
  *first-variation* / *second-variation* 0. *hessian* 1 — and that one is f17's
  own student memory, written after the recall ran.

So f17's empty recall is **correct behaviour on an empty shelf**, and at least
part of what two tenures have been calling a retrieval gap is a curriculum gap.
**f17 returning 0 for a good query is better conduct than f15 returning 105 for a
junk one**, and a reader comparing offer counts across the frames would conclude
the exact opposite. Never report an offer count without the query and the terrain
beside it.

**What that does NOT excuse** — three real defects, all receipted:

1. **The student's recall never fired.** `{:status :not-invoked, :reason
   :memory-channel-no-push}` against **523 eligible ids**. Not "ran and found
   nothing" — did not run. f15's student read 495 available / 0 retrieved, which I
   reported as retrieval failure; I do not know whether f15's was silently this
   same defect. **Do not merge the two without checking.**
2. **The lexical seed is 29/30 coordination evidence.** 30 entries, 29
   `:evidence-type :coordination`, ONE `:memory`, every score negative
   (−25.90 … −17.26), and `proposal-count 0` — so even the single memory-typed
   entry did not survive to candidacy. The retrieval front-end spends its whole
   seed budget on agent chatter when answering a mathematics query. **This is the
   best lead I am leaving you.**
3. **Every deposit lands `:domain :zaif-work` and unpromotable** (§4) — five of
   them, all scribe-authored.

---

## 2. What I got wrong, so you do not repeat it

**I registered a prediction in the f15 entry and it was wrong.** I wrote that P30
would clear `:malformed-cycle-attempts` and take the failure count 3 → 2. It did
not. P30 **landed and fired** — f17's attempts carry a machine-computed
`:cycle/store-revision` — but `attempt?` needs six keys, the machine stamps four,
and f17's guide omitted the other two (`:cycle/regime`,
`:cycle/runner-freshness`). f15's guide had supplied those two by hand and omitted
store-revision. **Two frames, two guides, two different omissions, one failure.**

My f15 mutation was sound — I injected a sha into f15's attempts, which already
carried the other four. **A mutation proves the check responds to the input you
gave it; it does not prove the input class is closed.** That is the transferable
form, not the field names.

**I also nearly shipped a wrong mechanism.** I first read f17's
`:anchor {:term "hessian" :satisfied? false}` as the cause of the empty recall.
It is not. `dispatch_with_recall.clj:680-705` is explicit — `eligible-memories`
annotates, `rank-with-anchor-boost` only sorts, *"the anchor influences surfacing
without becoming an eligibility filter."* The ranked set was already empty. Two
minutes reading the predicate saved the series a false mechanism. **Read the
predicate, not the field name.**

---

## 3. Instrument traps — inherited, corrected, and new

- **`/api/alpha/hyperedges` IS ON 7073, NOT 7070.** analyst-2's handoff says 7070
  and that instruction has rotted; the substrate is its own JVM (the I-0 override
  in `CLAUDE.md`). It answers in **EDN, not JSON**. The `limit=100` default trap
  analyst-2 recorded is still live — always pass `limit=5000` and always print the
  row count.
- **`transfer_checks.bb` already points at 7073** (`(def substrate ...)` line 22)
  and that endpoint answers. I checked this before reporting C4's zero, because a
  moved endpoint would look exactly like a frame finding. **Verify your instrument
  before you report a zero.**
- **SERIES.EDN IS APPEND-*HOPEFUL*, NOT APPEND-ONLY.** The f16 void entry had been
  appended **after the closing `]`** (line 1503) since the day it was written, so
  it was a second top-level form and `read-string` — which returns the first form
  only — saw 8 entries and no f16. Present as text, absent as data, unnoticed
  because nothing had touched the file since. I repaired it in `69798714`.
  **Read the file back with `read-string` after every append.** One line; it is
  how I found this.
- **Stage what you author and commit it in the same breath.** My f15 series edit
  was swept into another seat's commit because I left it unstaged for the length
  of one tool call in a shared worktree. Bells cross in time; worktree commits
  cross in space.
- Still true from analyst-2, all still biting: attachment-status is a keyword on
  some edges and the string `"reviewed"` on others (normalise with `name`/`str`);
  never count raw string occurrences of an id, count structures;
  `transfer_checks.bb` is not executable, run it as `bb <path> <dir>`.
- **A pattern is a NAMESPACED endpoint (contains `/`).** My first S-6 pass counted
  problem ids, commit shas and session ids as patterns and reported 13 shared
  nodes; the true figure was 3. My method reproduces analyst-2's S-5 exactly
  (universe 105, mission 18/16/2) before reporting S-6, which is the only reason
  S-5 → S-6 → S-7 are comparable. **Reproduce your predecessor's last reading
  before you publish your first.**

---

## 4. Open, and who owns it

1. **`:both-channels-varied` HAS NEVER BEEN ABLE TO FIRE.** The series' two-channel
   experimental control. At f15 I showed it by mutation; **f17 confirmed it by
   observation** — the field is now populated and both attempts carry the
   *identical* digest `734fea7d…`, because there is one store snapshot per cycle.
   Populating the field was necessary and not sufficient. Whether each attempt
   re-snapshots is a **design ruling, ground control's**, and it is the one
   outstanding item that would change what this series can claim about its own
   experimental design. Surfaced twice; still open.
2. **`:measurement-populated` is undetecting.** It is satisfied by *declaring* a
   field unset with a reason — mutation-verified TRUE with zero measured values.
   f15-guide's substantive complaint stands: no conductor action computes nine of
   the seventeen required fields, including *"promoted then surfaced then used"*,
   the exact measurement the frames exist to make. Whether a required measurement
   may be discharged by declaring it unset is a **loss-function decision**, which
   is why analyst-2 refused to move C3's `/6` denominator and why I did not move
   this. **Not yours to decide; yours to keep reporting.**
3. **C4 — I could not determine whether its FAIL is real or vacuous, and I said
   so rather than guessing.** `pull-offers 0`, `pull-uses 0`. Either (a) no seat
   pulled, in which case C4 has exactly the defect P28 fixed in C3, or (b) an
   invitation *was* issued — the offer bodies carry
   `:memory-pull-invitation-version "memory-pull-invitation-v2"` — and the
   `:memory-pull-offer` receipt was not written, which is a pipeline failure.
   C4's own comment says the offer receipt is the denominator and should exist
   even for an empty search, which favours (b). **THE DISCRIMINATOR: does issuing
   a `memory-pull-invitation-v2` imply a `:memory-pull-offer` receipt?** Answer
   that and C4 resolves in one step. **Unowned — take it.**
4. **All five f17 deposits are `:domain :zaif-work`, `:attachment-status nil`,
   unpromotable** — `de5a932d`, `51f8aac4`, `df0f60df`, `9152cdbd`, `c1fb54ab`, and
   they are the **scribe's**, not the student's.
   **READ THIS AS A CORRECTION OF MINE, BECAUSE THE MISTAKE IS INSTRUCTIVE.** I first
   filed this as D5's third sighting on the student, with a vendor correlation. f17-
   guide challenged it and was right: `:evidence/author` on the deposit reads
   `"f17-scribe"`. I had inferred "student" from a `zai-*` session id — and the scribe
   is *also* a zai seat this frame, so that field cannot identify a seat at all. It is
   the same error class analyst-2 documented when it found `memory-domain` is `None`
   for every student seat and therefore cannot discriminate between two of them.
   **Fetch the author; do not infer the seat.**
   The corrected finding is stronger than the one I filed: the domain pin is a **zai
   write-path** problem, not a student problem — which is exactly where analyst-2 said
   to look, and where my framing would have sent you away from. D5 proper stays at
   n = 2 (f10, f12) and stays ground control's.

5. **C2 has failed four consecutive frames on the guide's own `:intervene`
   store-write** (f12, f15, f17 — three guides, two vendors, identical statusless
   deposit). It is not an incident and not a pattern; it is the tool's normal
   output, and nothing downstream picks it up.
6. **The corpus.** Four void frames now (f13, f16, plus f11/f12 dispositioned
   `:defective`) on four different mechanisms. analyst-2's line still holds and has
   more evidence: this apparatus is currently a better detector of broken
   formalisations than a solver of good ones. Ground control's ContDiff sweep
   (five candidates, `E-APM-f12-defects.md`) is still unrecompiled.
7. **E and G** — case-study accretion and whitepaper §5. **Untouched by three
   tenures.** You will inherit ten series entries, eight of them closes. If G is
   ever writable it is writable now, and I am the third seat to fail to write it.

---

## 5. P31 — LANDED AND MERGED BEFORE HANDOVER; one limitation is yours

Not in flight after all. **P31 merged as `c5971a27`** (codex-3's `294b4679`), and
`attempt?` is now satisfiable by machine stamping alone — I mutation-checked that
an **empty caller map** produces a valid attempt. `:malformed-cycle-attempts`
should clear at f18, taking the failure count with it.

**codex-3 answered the `runner-freshness` semantics question better than the
bell-back I told it to send.** Rather than inventing a definition, it found the
place where the machine already knows: `frames.bb open` rejects an existing
record *and* an existing checkout before `git worktree add -b`, so reaching that
line means a fresh tree. I read `open-frame!` and confirmed the two `die` calls.
It also matched a convention already in that map — `:session :recorded-at-close`,
commented "a minted UUID here asserted isolation that did not exist."

**THE LIMITATION, AND IT IS YOURS TO WATCH:** there is exactly one writer of
`:runner-freshness` and it writes the literal `true`. **No path can write
`false`.** The field is truthful for the path that writes it and
**non-discriminating** until a runner-reuse path exists — and freshness is an
experimental control, so a control that cannot vary detects nothing. That is the
same shape as `:both-channels-varied` one level down, and this series now has two
instances of it. Not a defect in the packet: the stamp preserves an explicitly
supplied `false`, so the slot works the day a reuse path is added. **Watch for the
pattern, not the field** — a schema slot whose only writer emits a constant is a
control in name only.

## 6. Duty-by-duty, briefly

**A.** `analysis/transfer_checks.bb <problem-state-dir>` — always re-run it
yourself, never read someone else's receipt. Series:
`f7 3/6 · f8 6/6 · f9 5/6 · f10 5/6 · f11 4/6 · f12 5/6 · f13 VOID · f15 5/6 ·
f16 VOID · f17 3/6`. **When a count is zero, ask what the check had to work
with** — this is analyst-2's lesson and f17 produced two more instances of it
(C3 correctly INAPPLICABLE, which is P28 paying off two tenures later; and
`:f9-capability-probe-missing` returning purely because there were no offers or
promotions to probe).

**Failure counts are not counts of independent defects.** f17's five are four
causes: `:f7-missed-available-artifact` and the `need-retrieval` capability are
the same subset test; `:f1-scaffold-identical-frame` and `created-frame-worked`
are the same hash comparison; and `:f9-capability-not-realized` fires once while
covering two failing capabilities. f15's three were two causes. **The units of
this number change between frames.** Say so every time you report it.

**B.** No script exists; the method is in §6 of the succession handoff and in my
f15/f17 entries. S-7: universe **106**, mission scope **22/19/3**, whole store
**538 edges / 223 reviewed / 213 with a review record**. **The reviewed set did
not move between S-6 and S-7** — +40 edges, zero of them reviewed. The store is
growing and the curated store is not.

**C.** Cascade receipts ran for the first time at f15 (105 distinct memories,
315 offers = ×3 across three dispatches, no dedup — **report 105**). f17 had zero
offers. **The `used × route` join the card asks for still cannot be computed**:
`:write-use` takes only an `:offer-id` and there is no verdict field anywhere.

**D.** Do not take a curation reading every close; I skipped f17's deliberately
because zero attachments is zero data, and I will not dress a zero as a trend.

**F.** *Does this edit move a number I report?* P31 covers **both** remaining
attempt fields precisely because half the fix moves no number.

---

## 7. On N = 2, and the thing nobody has said

**A void frame inside a tenure has now happened twice** — analyst-2's tenure did
not have one, mine did (f16), and analyst-3's predecessor lost f13 the same way.
The f17 registration records the crossing explicitly:
`:reg/analyst-tenure {:n 2 :frame-index 2 :frames ["f15" "voided" "f17"]}`.

So under N = 2, **a single defective problem can reduce a tenure to one closed
frame** — and cross-frame drift detection, the entire reason this seat is not
fresh each time, needs two. Four of my predecessors' sharpest findings required
exactly two frames. I got two, barely.

That is a parameter question for the operator and **not mine to rule on**. But it
has now bitten twice and I do not think anyone has said it out loud, so I am
saying it here: *should a void frame count against a tenure?*

---

Your second frame is your last. Everything you want to outlive you must be in the
three surfaces before it closes, succession is a **scheduled duty of that frame**,
and you should **probe your successor before you hand it over**. It costs one bell
and it is the difference between handing over a seat and handing over a hope.

The card is better than it looks. Trust it before you trust me.

— analyst-3, tenure 3 of the seat, f15 and f17
