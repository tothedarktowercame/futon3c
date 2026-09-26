# Fingerprint audit, f28–f34 — the witness standard applied to every recorded USE

Claude (claude-12), 2026-08-25. Instrument: `fingerprint_audit.py` (this
directory). Reading: `fingerprint-2026-08-25.json`. Campaign
`jit-all-open-nontopology-v1`. Requested by Joe as item 3-offline of
`TN-fable-F32-F35-bank-review.md`, ahead of wiring the same check into
close-frame.

## What was measured

`retrieval-whitepaper-v3` §3.1 fixes the standard: *a USE claim counts when the
committed artifact carries the memory's fingerprint; prose attribution alone is
design signal, never outcome data.* Nothing in the frame machine applies it —
`:used-ids` is a Student self-report validated only for membership in the
controller-derived surfaced set. This applies it offline to all 35 recorded use
events (19 attempts, 13 with a non-empty `:used-ids`, frames f28–f34; f31 has
no student attempts).

Per (attempt, memory): the Lean identifiers the memory names are extracted from
its body, then looked for in the attempt's archived closing source; separately,
the longest run of consecutive memory-body lines appearing verbatim in that
source is measured.

**Both are measured against the base file the Student was handed**, resolved
from the attempt receipt's own `:base-revision` + `:problem-path` via apm-lean.
This is the step that makes the number mean anything: memory, base file and
certified head all discuss the same mathematics in the same vocabulary, so an
identifier already in the base is not evidence a memory put it there. Without
the subtraction the audit reports 29/35 fingerprinted; with it, 23/35. The six
events it moves are the honest difference.

Verdicts: **paste** (≥3 consecutive novel body lines verbatim in the artifact),
else **fingerprinted** (≥1 named identifier novel to the artifact), else
**already-in-base** (identifiers matched but all were in the base file), else
**unwitnessed**.

## Results

| | events | |
|---|---|---|
| fingerprinted | 23 | 18 distinct memories |
| paste | 6 | 6 distinct memories |
| already-in-base | 6 | |
| unwitnessed | 0 | |
| **total** | **35** | over 13 attempts with a USE claim |

**No use claim was unwitnessed.** Every `:used-ids` entry left some trace.
That is a real finding about the Students' honesty and it is not what the
batch-1 adjudications found for prose attribution (whitepaper §3.2, where both
attributed USEs adjudicated as marginal).

### The paste events are confined to f29/f30, and they are exactly the six known blocks

| frame | paste events | longest verbatim run |
|---|---|---|
| f29 a3 | 4 | 17–25 lines |
| f30 a2 | 1 | 16 lines |
| f30 a3 | 1 | 8 lines |
| f28, f32, f33, f34 | 0 | — |

The six memories are `e-83ece32c`, `e-021bf80a`, `e-c86a7d45`, `e-f6b4c56b`,
`e-f0db6e27`, `e-20851bde` — the same six `E-early-memories-review` §finding 1
identified as proof-text blocks still being seeded campaign-wide. The audit
reaches that set independently, from artifacts rather than from bodies, and
strengthens the claim: they were not merely paste-*shaped*, they were pasted,
up to 25 consecutive lines at a time.

**All six remained on every subsequent shelf** (checked: 6/6 present in f32,
f33, f34 and f35 attempt-1 `:accessible-memory-ids`) **and were never used
again after f30.** Availability held constant; the behaviour changed when the
scribe cards split. That is the closest thing to a controlled before/after the
corpus offers on the paste failure mode, and it is evidence the
`codex-scribe-v1` proof-text limits did what they were written to do. It is not
clean — different problems, different Students, and the promotion pipeline
changed too — but the availability of the specific pasted memories is held
fixed, which is more than a bare before/after.

### The already-in-base rows are the instrument working

Six events resolve to "the memory's identifiers were in the artifact, but all
of them were already in the base file": f29 a1, f29 a2 (one of five), f32 a2
(both), f34 a2 (both). Independently: f34 attempt-2's own failure account says
its budget went on signature verification with **no Lean written**, and f32
attempt-2 was the repair turn that died on the mistyped UUID
(`TN-fable-F32-model`). The audit did not know either fact. It says nothing
landed in exactly the attempts where nothing landed — convergent validation of
the base-subtraction step.

### Cross-problem reuse: 1, and it was reading as 0

f33 attempt-1 remains the only USE of a memory mined from a different problem:
`e-codexpilot-force-a-sublinear-entire-function-constant-by-Cauchy-derivative-estimates`
(codex-5, 2026-07-31, witness commit apm-lean `662b9ec` = a94J08) used on
a94A07, novel hits
`Complex.analyticAt_of_differentiable_on_punctured_nhds_of_continuousAt` and
`is_const_of_deriv_eq_zero`. The audit reproduces, mechanically, the two APIs
found by hand in the earlier review — which is the calibration that says the
token extractor is looking at the right things.

`memory_shape.py`'s origin rule has been extended (this pass) with a third
fallback: when a memory carries neither a problem-ref subject nor a problem id
in its text, resolve the commit it cites as its witness against apm-lean. The
codex-pilot memories are precisely the ones with clean hygiene — mission
subject, no problem id anywhere — so the old rule filed them under unknown
origin and no use of one could ever count as reuse. Re-reading with the fix
(`memory-shape-2026-08-25.json`): **`used-on-problem-!=-mined-from` = 1**
(was `{}`), `used-with-unknown-origin` = 0 (was 1), 25 used only on their own
problem, 26 distinct used.

### Era split

| | f28–f30 (pre-split cards) | f32–f34 (codex/zai scribe split) |
|---|---|---|
| fingerprinted | 16 | 7 |
| paste | 6 | 0 |
| already-in-base | 2 | 4 |

## Limits of the method — stated so the number is not over-read

- **A fingerprint is not causation.** An identifier novel to the artifact and
  named by an accessible memory is consistent with the Student having found it
  independently. The standard was always a *necessary* condition; it rules out
  the unwitnessed claim, it does not establish the counterfactual. Only the
  ablation arm (prereg level-2b, still `:not-yet-run`) does that.
- **A hit can be a refutation.** f33 a3 counts `div_le_div_of_nonneg_right`
  as a novel hit for `e-3411c0c2`, but that frame's close-frame finding F8
  records this item as *contradicted* by attempt 3. Presence in the artifact
  can mean the Student engaged with the memory and corrected it. The audit
  cannot tell use from correction; a human reading the row can.
- **Some tokens are local hypothesis names** (`hf_entire.analyticAt`), present
  because the memory quoted proof context. Weaker evidence than a Mathlib
  lemma name. Reported in `novel-hits` so they can be inspected, not filtered
  away silently.
- **Counts are events, not memories**: 35 events over 24 distinct memories;
  a memory used in two attempts of one frame counts twice.
- f31 contributes nothing (no student attempts); f35 was still running.

## What this implies for the wired version (review item 3-wired)

The offline pass answers the question it was run to answer: **the wired check
must record the base-difference, not a raw match.** A close-frame check that
greps the artifact alone would have reported 29/35 and called f34 attempt-2 —
an attempt that wrote no Lean — a double fingerprinted success.

Concretely, the field to add per used id: `{:tokens-named N :tokens-novel N
:novel-hits [...] :paste-longest-run N}`, computed against
`:base-revision`+`:problem-path`, which the receipt already carries. The Guide
already recompiles every attempt at close-frame and holds the source blobs, so
the inputs are all in hand. Note this adds a field a role's receipt carries,
so per the F32 lesson it is Lean-first: the emitter owns receipt schemas since
`hole-generated-receipt-schemas-v1` closed.

## Reproducing

```
python3 fingerprint_audit.py --write fingerprint-2026-08-25.json     # this reading
python3 memory_shape.py --write memory-shape-2026-08-25.json         # origin/reuse
```
Reads only: campaign frame records, the substrate evidence endpoint (7073),
and `git show` in apm-lean.
