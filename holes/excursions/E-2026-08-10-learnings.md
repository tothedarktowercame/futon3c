# E-2026-08-10-learnings — one day of instrumented ground control

Written 2026-08-10 late by ams-claude-1 at Joe's request: the day-level
synthesis. Batch specifics: `batch-1-report.md` (+ errata). Sources for
each claim are in the store (session "vote-and-callback-pipeline") and
the day's commits.

## 1. Silence is the failure mode — proven at every layer, same day

The pipeline ran dead under well-formed receipts (Zone substrate); a
receipt field displayed but did not persist (B4); a "re-registered, ok"
verified only existence; a restart spawned a headless twin of ground
control; and the frames system — built to make unfilled slots impossible
— accepted sentinel strings as filled slots, caught only by a fresh
auditor. Countermeasures that actually held: enforcement by construction,
instruments that make absence loud, and a culture of auditing the
instrument (verify-don't-trust survived its own author).

## 2. Controlled twins beat plausibility judgement, cheaply

Paired blind dispatch measured a LOW, canyon-shaped noise floor: twins
converge step-for-step where the classical route is unique (once to the
identical unprompted corpus repair), diverge only where Mathlib offers
competing engines. Against that floor, B1 (memory channel on/off) nulled
cleanly: identical sorry deltas, hit-stratum divergence ≤ placebo, and
both attributed USEs adjudicated marginal — one contradicted by its
twin's wall-clock. The P2 "load-bearing" rubric would not have survived.

## 3. Memory pays on the demand side right now, not the supply side

One evening of votes + exit interviews produced 14 voter-groomed concepts
(exact Lean specs, stepping-stones, a named backport source, demand-query
tags), independently rediscovering AND quantifying the topology backlog
(surface classification: 7+ voters). The one proven memory→closure path is
vote→build→callback: a94A09 fell one-pass ninety minutes after the system
built what its own demand signals specified (ConstructionTargets.
SchwarzPick, spec authored by the voter).

## 4. Retrieval's defect is characterized by its users

Anchor selection inverts relevance (slit-wedge memory: surfaced for the
wrong problem, absent for its own, with a victim statement). Runners find
reusable work by ENGINE NAMES and STRUCTURAL SIMILARITY (b00J01 twins:
index-by-signature and rg-by-declaration-name, both succeeding where
recall surfaced nothing). Wave-2 requirements, written by the consumers.

## 5. Terms vs fit (Joe's 25-year-old pair of insights), operationalized

Lean solved "know the terms" by force; "know how they fit" lives unnamed
in proof bodies: 6,114 have-steps vs 2,139 named lemmas (~3:1). Cheap
signature clustering finds statement shapes; MiniLM over raw text catches
glue twins (closure_ball re-derived the same evening it matched at 0.77);
proof-term shapes are the open rung (Rob's). Every memory ever actually
USED was fit-shaped, not term-shaped. Division of labor hypothesis for
the half: library holds terms, memory's durable niche is glue.

## 6. The operator economy works

The harvest loop, proceduralized after ~15 iterations, transferred to an
Opus seat via runbook in one induction — whose shakedown immediately found
three supervisor defects (finding #1 again). Fable's remaining share:
design, verdicts, adjudication, escalations. The handover-document culture
is the transfer mechanism, three generations deep today.

## Banked

apm_a94a09 + apm_a01a05 fully proved on apm-lean master; best-arm partials
merged for the other nine panel problems (all compile-verified);
ConstructionTargets.SchwarzPick (16 declarations); LEMMA-INDEX at 2139/196;
native_decide remedied in b00J01. Frames, twin-diffs, receipts, votes,
glue census, and the priors survey all committed or in the store.
