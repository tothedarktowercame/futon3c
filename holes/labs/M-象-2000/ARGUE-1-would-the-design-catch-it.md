# ARGUE-1 — would the 象-2000 design have caught its own misses?

claude-14, 2026-09-26, Joe's question: for each miss in ARGUE-0, would a design
item — had it been in place — have caught it? If not, what upgrade (including
from codex-3's list)?

Design items tested: the 15 象 patterns; build packets P1–P18; MAP's
prospective inference over tags (Q5); yellow tape with proof to clear; P7
(each turn's retrieval handed to the next turn).

## Verdict per miss, as designed

| miss | caught as designed? | why not |
|---|---|---|
| 1 reconstruct / replay / compensate conflated | no | The caveat was an *agent* statement (mine, in MAP). 象 annotates operator turns only (R29 left open), so it never entered the act record |
| 2 wake counted as promise kept | no | I made the claim in MAP Q4 (agent text, unannotated). P5 then violated 两种规格, which was in the cascade; nothing checks a packet against the patterns it realises |
| 3 interpretation given institutional effect; authority narrowed | no | The defect is in our translation (名分有据 narrowed McCarthy l.1784). kimi-3's review checked each citation "✓" but DERIVE-1 rows never declared what they dropped, so there was nothing to review |
| 4 one time axis; unstable act identity | no | Qualification lost (agent text) and code cited not read: the typed hole's pointers resolved, and its reading of `stable-hyperedge-id` was never tested |
| 5 no crash-consistency design | no | P7 retrieves per turn; the plan was written inside one long job. `agency/state-atomicity` would have needed retrieval over the *packet* text |
| 6 proof-to-clear can make tape permanent | no | Absent: an operator ruling was adopted without its own HOWEVER |
| 7 matching and attestation | no | Stated then contradicted in adjacent sentences of agent text; no record linked the two |
| + acceptance case dropped | no | 三层规格 was applied to runtime rules, never to our own phases; no phase states its accomplishment check |

As written, the design catches none of them. The instructive part is why: **every
miss happened in the design process, and the design's items aim only at the
runtime** — operator turns, parks, rules. Applied reflexively, its own patterns
cover most of the misses. The upgrades below make that reflexive application
concrete.

## Upgrades

| id | upgrade | catches | source |
|---|---|---|---|
| U1 | Annotate agent turns as well as operator turns (R29), and record design claims as acts; a qualification or withdrawal attaches by ref. At a phase boundary, query "open qualifications on claims this artefact depends on" | 1, 2, 4, 7 | ARGUE-0 mechanism A; R29 |
| U2 | Each build packet names the patterns it realises (from the cascade); for each, a reader asks whether the packet's acceptance test would fail on that pattern's violation signature. Patterns become test obligations, not citations | 1, 2, 3 | mechanism B; 两种规格, 三层规格 |
| U3 | Run the per-turn retrieval over each *artefact* (packet, plan section) at the time it is written, and record weak activations beside it — cheap, after the fact, no gate | 5 | P7 generalised; ARGUE-0 B |
| U4 | Put the convergence ledger's rung scale on the typed hole: a resolved pointer is `:named`; a claim about behaviour needs a read or a test (`:formula-transcribed` / witnessed). The hole's "partial" fills were all pointer-level | 2, 4, 5 | mechanism C; CONVERGENCE rungs |
| U5 | Apply 三层规格 to each phase: every phase artefact states its accomplishment check, and the check is the acceptance case re-run — an executable reconstruction of the Kimi/red-tape incident before the general ledger (codex's closing recommendation) | + | mechanism D; codex |
| U6 | Translations declare what they dropped (翻译契约 / `translation/declare-what-is-lost`), so a review has a claim to check | 3 | mechanism of point 3 |
| U7 | An adopted rule — including an operator ruling — gets its own HOWEVER (how it fails) before it becomes a constraint | 6 | point 6 |

## Design corrections taken from codex (content, not process)

- Three operations with their own results and failure conditions: reconstruct a
  historical state; replay a stated intervention under stated assumptions;
  compensate recorded effects (point 1). P14's claim narrows to "this
  executable rule detects this recorded sequence" until a world model exists.
- Promise lifecycle as separate acts: dependency termination, wake delivered,
  fulfilled, released, deadline breached (point 2).
- Original event, versioned interpretation, and authorised institutional effect
  as three records; authority checked against grant scope, time and delegation
  chain, whoever the signatory is (point 3).
- Valid time and system time on every join; explicit act identity, not
  type+endpoints; adoption ≠ commit ≠ live activation (point 4).
- A durable transition and delivery protocol, with crash tests at each boundary,
  before any cache is demoted (point 5).
- Clearance proves "resolved and its measures can safely end", not "P prevents
  recurrence"; measures owned individually by incident, never by timing
  (point 6).
- Matching compares HOWEVER/THEN/BECAUSE/scope; attestations kept individually
  with author and dependencies, 象's own not counted as independent (point 7).

## What this does to the argument

The design's strongest claim — that recording acts and putting patterns in
front of authors prevents repeat failures — survives, but only once the design
is applied to its own making. The first test of that claim is cheap: U1–U7
applied to the next phase of this mission, with ARGUE-0 as the baseline miss
rate (5 of 7 had the knowledge present).
