# Handoff: analyst-1 → analyst-2 (2026-08-18)

Written at the close of **f10**, the second and last frame of tenure 1 under the
N = 2 ruling. You serve **f11 and f12**.

I held this seat for two frames and nothing else. My whole orientation was
`role-cards/analyst-v1.md`, frozen at blob `ed340941`, and it was sufficient —
that is worth knowing before you distrust it. Read the card first; this document
is the delta, not a replacement.

---

## 0. Read these three, in this order

1. **`role-cards/analyst-v1.md`** — your duties, boundaries and traps. Frozen.
2. **`analysis/series.edn`** — four entries now (f7, f8, f9, f10). The f9 and
   f10 entries are mine and are written to be read by you specifically.
3. **`holes/excursions/E-APM-f10-defects.md`** — ground control's 21-entry
   defect inventory, opened while I was still running, with a `[verified]` /
   `[reported]` distinction. §7 says how to extend it; I appended to it.

Your memory is still exactly three surfaces: the store, the mission record, and
`series.edn`. My f9 close died mid-analysis on an exhausted model quota having
computed real findings and written none of them, and **all of it was lost**.
That is not a cautionary tale, it is the recorded behaviour of this seat.
Write as you go.

---

## 1. What is broken at the handover, and who owns it

### YOUR SEAT DOES NOT EXIST YET — this is the one thing to fix first

`POST /api/alpha/frames/mint-analyst {"tenure": 2}` returns **404**. I did not
work around it. Diagnosis, verified:

- `POST /api/alpha/frames/mint-seats` with `{}` returns **409
  missing-frame-id** — routed.
- `POST /api/alpha/frames/mint-analyst` with `{}` returns **404 Unknown
  endpoint** — not routed.
- Yet both `futon3c.transport.http/mint-analyst-seat!` and
  `futon3c.agency.frame-seats/mint-analyst!` **resolve in the running image**
  (checked over Drawbridge), and the route branch exists in the source at
  `transport/http.clj:7271`.

So the handler is not missing: **the running router was captured before the
branch existed, and reloading the namespace does not rebuild it.** This is a
second sighting of the capture-by-value family recorded as D7
(`conductor-surface` captures operation functions by value) on a different
surface. Two sightings make it a class, not an incident.

I deliberately did **not** hand-assemble a mint over Drawbridge.
`mint-analyst!` needs a `:prepare-seat-fn` built from the live server config,
which is not reachable from any deref-able var in the running image; a
hand-built preparer would have produced a seat that *looks* minted while
carrying an invoke path I could not verify. The card's first recorded trap is
asserting infrastructure state without checking, and a broken seat that reports
as healthy is worse than an absent one. **Owned by ground control.**

One-line check before you trust it:

```bash
curl -s -o /dev/null -w '%{http_code}\n' -X POST \
  localhost:7070/api/alpha/frames/mint-analyst \
  -H 'Content-Type: application/json' -d '{}'
# 409 = routed and mintable.  404 = still stale.
```

### The promotion pipeline refuses both honest authoring paths

f10 added **zero** reviewed attachments to the store. Not "few" — zero. Both
routes are refused (E-APM-f10-defects D3 and D4, verified at source there):

- guide-authored deposits are refused because `promote-memory-attachment!`
  refuses `reviewer == depositor` and the conductor's acting identity is always
  the guide;
- scribe-authored deposits are refused because writing the pattern at record
  time leaves the edge non-statusless, and the gate demands statusless.

**Doing the right thing at write time is what disqualifies it.** Expect your S
reading to be flat until this is fixed, and do not read flatness as a curation
finding when it is a gate finding.

### Every frame closes `:closed` while its own validator says `:launchable? false`

f8: 6 invariant failures. f9 and f10: 7. Nothing surfaced it and **I missed it
at f9 myself**. P27 (`9ef132d4`, plus my review fix `cc5a0ae1`) now prints it
and writes it into every receipt as a **non-scoring** `:trace-validation` block.
It is non-scoring on purpose: the `:score` is `n/6` across five frames and a
seventh check would silently move the denominator.

The delta between f8 and f9/f10 is exactly one failure,
`:f3-undispositioned-offer`, and it appears exactly when the cascade is enabled.
`:memory-disposition-offer-ids` is **empty in all three frames** — f8 passed F3
vacuously because it had no offers. The cascade did not break dispositioning; it
removed the vacuity that was hiding its absence. Unfixed, and it contradicts
`:required-capabilities :offer-use-disposition` in both registrations.

---

## 2. What is SETTLED — do not re-litigate these

**The offer × use join is empty by wiring.** 203 offers across f9 and f10, zero
joins. Offers seed only from the solver dispatch; the student is
`:memory-channel :none` and its recall is `:not-invoked` with reason
`:memory-channel-no-push`. I registered this at f9 as a falsifiable hypothesis
("if the cascade is not given a pull-side surface, f10 reproduces f9's empty
join exactly") and f10 reproduced it, across a different family and a different
edgedness. Ground control re-derived it independently as D18. **It is wiring.**
The transfer that actually happened in both frames came through the *pull*
channel, which the offer instrument does not watch.

*Label note, resolved by me because D18 flagged it as open:* the f9/f10
`:memory-channel` labels do **not** differ. Both frame records read solver
`:push`, student `:none`; both saved states read the student `:not-invoked /
:memory-channel-no-push`. The `:pull-only` / `:push+pull` labels appearing in
D18 come from some other surface. The discrepancy is in the reporting, not the
data.

**The cascade seeds from what RECALL surfaced, not from what the problem
touches.** I recorded this as a diagnosis in the f9 entry after f9's `:why-hop`
route fired zero times despite the frame being *chosen* for its edged pattern.
f10's registration then built a negative prediction
(`:cascade-cannot-why-hop`) on the confusion, and f10 produced **48** `:why-hop`
offers on the problem chosen for having none. The asymmetry is worth carrying:
**f9, chosen for edges, got 0; f10, chosen for none, got 48.** If you author or
review a prediction about cascade reach, compute its premise over the
recall-surfaced set.

---

## 3. What each duty actually cost, so you can budget

- **A (checks + series entry)** — cheap, ~2 minutes of instrument, and the entry
  is most of the work. `analysis/transfer_checks.bb <problem-state-dir>` prints
  the score, the new trace-validation line, and overwrites
  `<dir>/transfer-checks.edn`. Always re-run it yourself; do not read the file
  ground control may have generated.
- **B (S reading)** — still no script, still ~10 minutes by hand. My method and
  its three earned cautions are in the f9 and f10 entries under
  `:instrument-cautions`. The three that cost me time: `attachment-status` is a
  **keyword on some edges and the string `"reviewed"` on others**; a swallowed
  substrate query silently shrank my census from 205 to 198 between two runs of
  the same script, so **count query errors, never `(or … [])` them away**; and
  **`unused-ids` contains the substring `used-ids`**, so grepping for uses reads
  every non-use as a use. I hit all three.
- **C (cascade bookkeeping)** — the receipts have now run in two frames, so this
  duty is no longer untested. The join is the whole of it and it is empty; see §2.
- **F (packets)** — one per frame is a sustainable rate. P26 at f9
  (`f5c68a5b`), P27 at f10 (`9ef132d4`). Both were single-file, single-behaviour
  packets with a bar I had **measured before setting it**, and both came back
  correct. Keep them that small.
- **D, E, G** — I did not do these. They are the slower duties and two frames
  was not enough to make them non-noise. They are genuinely open.

**On dispatching vs doing it yourself:** P26 changed the instrument that
produces my own headline number, in the direction of improving it. That is
exactly the edit that must not be self-authored, so I sent it out even though I
held the entire diagnosis in context and the workspace carve-out (b) would have
covered doing it myself. Use the same test: *does this edit move a number I
report?* If yes, dispatch it however small it is.

**Gate it for real.** Both packets came back with correct-looking reports and I
still re-ran every acceptance command, mutated the guard by hand (P26: forcing
the dispatch index to 0 must make C3 *fail*; P27: a state with the
`:validate-trace` step removed must degrade, not throw), and checked
`git show --name-only`. P27 came back clean and I still found one thing worth
fixing myself — it printed `failures: 0` when the truth was "never measured".
That is the seat's whole job in one line.

---

## 4. Traps I fell into that the card does not already list

- **I read a frame's close without checking whether its trace validated.** The
  wake payload carries `:launchable?`; f9's did not include it and I did not
  look. One field, three frames, missed. The receipt now carries it — but the
  lesson generalises: *read what the machine already computed before computing
  anything yourself.*
- **I trusted my own first script.** My initial S-reading script swallowed
  failed substrate queries and reported two different whole-store counts on two
  runs. I caught it only because I ran it twice. Run your census twice.
- **I nearly reported a use that was a non-use**, because `grep used-ids`
  matched `unused-ids`. The card warns about over-counting substring matches in
  the abstract; here is the concrete instance.

---

## 5. On N = 2 — the evidence exists now, so say something about it

The ruling pinned N at 2 as "the minimum tenure at which drift detection across
frames is possible at all", explicitly a parameter to be revised on evidence.
Two frames produced four findings that a single-frame seat could not have made:

1. the f9 `:why-hop` diagnosis became f10's confirmed prediction;
2. the empty offer × use join went from one observation to settled wiring;
3. the tags-vs-subject interface difference (f9's student searched by tags,
   f10's by subject, unprompted, with no interface change between them);
4. the series' own f8 entry names two pattern ids the math-split renamed away —
   findable only by re-querying a prior frame's claims against the store.

**All four required exactly two frames. None required three.** That is the
honest reading: N = 2 is sufficient for the drift detection it was chosen for,
and I have no evidence bearing on whether N = 3 would buy anything more. The
duties it was *not* sufficient for are D, E and G — but those are slow by
nature, not blocked by tenure length.

---

## 6. Your inbox, in priority order

1. **Get yourself minted** (§1). Nothing else works until you exist.
2. **`:memory-disposition-offer-ids` is empty** — F3 fails in every
   cascade-enabled frame, contradicting a declared required capability. Nobody
   owns this yet; it is a good first packet if it is scoped to *reporting* the
   dispositions rather than *deciding* them (deciding is a design ruling and not
   yours).
3. **The promotion gate refuses both authoring paths** (D3/D4). Not yours to
   rule on, but your S readings are meaningless until it moves — say so in the
   entry rather than reporting a flat store as a curation result.
4. **The solver's attested use never reaches a machine receipt.** Seen in both
   my frames: the solver attests USED in `:attempt/verification` prose while
   `:memory-use/used-ids` stays `[]`. The loss function's numerator is dropping
   real uses, twice measured.
5. **D and E and G are untouched.** Bespoke-rate, case-study accretion,
   whitepaper §5. You have four series entries to read them across now, which is
   the first point at which they are not noise.

Good luck. The card is better than it looks; trust it before you trust me.

— analyst-1, tenure 1 of the seat, f9–f10
