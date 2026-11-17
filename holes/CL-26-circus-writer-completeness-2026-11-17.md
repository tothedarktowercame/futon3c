# CL-26 — Circus writer completeness survey (2026-11-17)

## Result

The three-animal join is small because 狗 rarely reaches the GCC on the same
turn, not because 馬 usually rejects 驴's pattern.  At the final survey snapshot:

| Pair | Distinct shared turns |
|---|---:|
| 驴 proposal + 馬 occurrence, any patterns | 61 |
| 驴 proposal + 馬 occurrence, same pattern | 61 |
| 驴 proposal + 狗 recall | 8 |
| 馬 occurrence + 狗 recall | 4 |

The 馬–狗 pair is the bottleneck.  The three-animal witness has one fewer turn
than 馬–狗 because one of those four turns has no 驴 proposal.  “Any pattern”
and “same pattern” giving the same 61 turns also shows that there is no hidden
population where both writers ran but chose disjoint patterns.

This is a public aggregate.  Turn and pattern IDs were held only in memory
during the survey; examples below use invented labels such as `T-alpha` and
`P-one`.  No operator, memory, mission, or model-response text is reproduced.

## Method and bounds

The graph census used the live, read-only `:edge-join` operation on :7073.
Self-joins enumerated each writer's distinct endpoint set within the operation's
500-row bound; direct joins established the same-pattern set.  All final reads
reported `:truncated? false`.  Set intersections over the returned endpoint IDs
gave the any-pattern pair counts without a Cartesian join.  Local turn files
were then joined by reconstructing their content-addressed evidence-text
reference locally; no evidence body or graph record was written.

Across exploratory and final snapshots the survey made 23 bounded read-only
`:edge-join` requests, below the 40-query budget.  It made no command request,
store write, reload, deep-health request, or process signal.  The live graph
was changing during the first exploratory reads; all numbers in this note are
from the final internally consistent four-query snapshot.

The final populations were 176 proposal `(turn, pattern)` pairs on 72 turns,
286 occurrence pairs on 61 turns, and 215 recall `(turn, memory)` pairs on 88
turns.  Local source-derived references matched 70 of
the 72 proposal turns, 59 of the 61 occurrence turns, and 85 of the 88 recall
turns.

## 驴 present, 馬 same-pattern occurrence absent

There were 31 missing `(turn, pattern)` pairs on 12 turns.  The exclusive
classification is:

| Class | Missing pairs | Turns | Design or loss? |
|---|---:|---:|---|
| `:pattern_occurrences` = `occurrence-write-exception` | 22 | 8 | loss |
| 馬 verdict absent after a stored 驴 proposal | 8 | 3 | loss |
| `:model_outcome.failure_kind` = `model/unparsed-reply` | 1 | 1 | loss; this turn also recorded `occurrence-write-exception` |
| 驴 proposals pending | 0 | 0 | — |
| verdict says pattern did not occur | 0 | 0 | not an absence class in the current writer |
| predates occurrence writer | 0 | 0 | — |

The earliest available 馬 writer timestamp attached to a live occurrence was
2026-11-15T04:15:38Z.  None of the missing local turns qualified as predating
that writer under the survey rule (created before that timestamp and lacking
verdict metadata).

The “did not occur” hypothesis is false for this schema.  The planner maps
every verdict pattern, then makes a `[:pattern/occurrence 1]` clusion for every
valid pattern reference; occurrence strength or voidness is retained in the
attestation's `:status`, `:turn_mapped`, and `:chain_mapped` fields
(`src/futon3c/arxana/pattern_occurrences.clj:21-60`).  It rejects only a pending
or unavailable verdict before planning (`:11-18`) and reports batch refusal
explicitly (`:63-71`).  Thus a non-occurring/void reading is by design a stored
occurrence record, not a missing edge.

The live listener makes the loss mechanism visible.  It stores the verdict,
submits the occurrence writer asynchronously, catches a thrown writer as
`occurrence-write-exception`, and records that outcome on the turn
(`src/futon3c/xiang/ma_occurrence.clj:451-475`).  No retry is attached to that
write-exception outcome.  The three verdict-missing turns are a separate loss:
the proposal exists in the GCC, but no 馬 verdict was durable in the local turn
record.

## 驴 present, 狗 recall absent

There were 65 proposal turns without a recall edge:

| Class | Turns | GCC projection field | Design or loss? |
|---|---:|---|---|
| typed `not-ready` | 59 | 57 missing, 2 `written: 0` | immediate typed absence is designed; persisting it after completion is a loss |
| typed `none` | 2 | 2 `written: 0` | by design: nothing to recall |
| typed `failed` | 1 | `written: 0` | loss |
| local turn record did not resolve from graph reference | 3 | missing | unresolved survey identity gap |
| predates recall writer | 0 | — | — |
| nonempty recall with explicit GCC write failure | 0 | — | — |

The aggregate `:dog_recall :dropped` map across these 65 turns was empty, so
CL-18-style candidate dropping explains zero missing recall edges in this
population.  The earliest available 狗 timestamp attached to a live recall was
2026-11-11T20:10:12Z; no locally resolved missing turn was classified as
predating the writer.

The dominant cause is a precise cache/projection race.  The provider is
deliberately nonblocking: it reads only its cache and records `not-ready` when
the background result has not arrived (`src/futon3c/agency/dog_provider.clj:18-29,1338-1355`).
The background path subsequently publishes the real result only into
`!recalls` (`:456-503`).  It does not replace the turn's `:dog_recall` field or
schedule its GCC projection.  The GCC writer correctly treats an empty typed
absence as `written: 0`, while a real item would create `[:memory/recall 1]`
(`src/futon3c/arxana/dog_memory.clj:82-133`).  Its update is intentionally
replaceable until a positive write (`:135-145`), but no completion currently
invokes that replacement for the 59 turns.

## Design absences versus losses

Designed absences:

1. 狗 `status: none` with no recalled items (2 turns).  No memory edge should
   be invented.
2. 狗 `status: not-ready` at render time.  Blocking the agent is not required;
   the typed immediate absence is correct.  Only its remaining final state is
   wrong.
3. A 馬 verdict may say a pattern is void/non-occurring, but the current schema
   intentionally stores that result as an occurrence clusion plus status.  It
   therefore accounts for no missing edges.

Losses, ordered as one-behaviour fix packets:

1. **狗 completion projection (59 turns):** when `prepare-async!` publishes a
   completed recall, atomically replace a still-`not-ready` turn field and
   schedule `dog-memory/write-turn!`; never overwrite a later real recall.
2. **馬 occurrence retry (23 pairs on 9 turns):** make an
   `occurrence-write-exception`/refusal retryable and idempotently resubmit the
   already-durable verdict batch; this includes the one unparsed model result,
   because failure verdict rows are still meant to be represented.
3. **馬 verdict coverage (8 pairs on 3 turns):** reconcile a durable 驴 proposal
   with an absent 馬 verdict and enqueue the existing verdict path once; do not
   synthesize a verdict or bypass the model outcome.
4. **狗 failed recall (1 turn):** retry only the typed failed background recall,
   preserving its failure receipt until a later completion replaces it.

The three graph turn references that did not resolve to a local source-derived
reference are not assigned a behavioural fix from this survey.  First compare
the authoritative evidence-text reference with the local record's historical
source revision; treating an identity mismatch as a writer failure would guess
at the cause.

## Answer

The records are joinable; completeness is the problem.  驴 and 馬 overlap on
61 turns and on the same patterns.  狗 overlaps 驴 on only 8 turns and 馬 on
only 4.  Most of that gap is one nonblocking implementation seam: `not-ready`
is correctly emitted immediately, but background completion is not projected
back to the turn or GCC.  The remaining material gap is nine 馬 occurrence
write exceptions plus three proposal turns with no verdict.
