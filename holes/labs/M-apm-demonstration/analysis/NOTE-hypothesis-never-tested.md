# The hypothesis has never been tested (Joe's question, 2026-08-19)

> "I don't see what we expect to learn from f13 now. We'll have patterns for how
> to address broken problems ...?"

Measured from the persisted cycle state of every frame in the series:

| frame | problem | sound? | closed? | memories surfaced | memories attested USED |
|---|---|---|---|---|---|
| f9  | a01J06 | YES | yes | 0 | **0** |
| f10 | m93J02 | YES | yes | 0 | **0** |
| f11 | t01A05 | no — statement FALSE | — | 0 | 0 |
| f12 | m03J01 | no — statement VACUOUS | closed vacuously | 0 | 0 |
| f13 | m99J06 | no — model INCONSISTENT | closed vacuously | 5 | **2** |

`a01J06` and `m93J02` record `memory-use steps = 0` outright — not zero uses, no
memory-use instrumentation ran at all.

## The consequence, stated plainly

**Every frame that closed a sound problem used zero memories. The only frame that
attested memory use closed a broken one.**

So the series has never tested its own hypothesis. The two arms never met:

- f9 and f10 had sound problems and a **broken read path** — they predate
  `7b188d8c` and the recall fixes, and recorded no memory-use instrumentation.
- f11, f12 and f13 had a **working read path** (by f13) and **broken problems**.

f13's transfer result is real and it is transfer of **defect-detection**, not of
mathematics. The two memories attested `USED` are procedural — "run a consequence
pass, test whether the carrier becomes empty" — and what f13 harvests in return
is more of the same. On a corpus that keeps supplying broken problems, that
capability compounds on an axis nobody asked for.

## What f13 has left to give: essentially nothing

Everything f13 can contribute is recorded. The scribe harvest still in flight
produces further defect-detection patterns. That is not worthless — the corpus
demonstrably needs them — but it is not the capability the demonstration exists
to establish, and it should not be counted as progress toward it.

## The decisive experiment, which is cheap and has never been run

**A SOUND problem, with the read path working, and memory-use instrumentation
recording.** That configuration has not occurred once in thirteen frames.

Neither probe mode nor f14-as-registered is that experiment. f14 (m93J06) would
be it ONLY if m93J06 is sound — which is exactly what we cannot currently assert
about any problem in the pool, and which a probe would establish in about seven
minutes.

That is the honest case for probe mode, and it is narrower than the one I gave
earlier: not "make attempts cheap in general", but **"establish soundness before
spending a frame, so that the transfer measurement is made on a problem where
transfer is possible."**
