# SHARD-NOTE — move-grammar shard 3 (zai-3, 2026-10-11)

30 sidecars, `validate.py` exit 0, **void 0** on all 30 (`valid=173` total).

## Counts

- 202 acts labelled (113 operator fragments/acts, 89 reply acts), 0 unlabelled.
- Answers: 56 closes / 27 keeps / 0 unclear (83 incidences).
- Scoped acts: 25 (opens 13: no-reinvention, crystallise-on-write,
  write-time-structure, M-象-2000-checkpoint, one-step-at-a-time,
  merge-with-M-象-2000, FUTON-debugger, weave-together, milestone-2-versions,
  technical-aspects, memory-writes-as-acts, plan-going-forward, files-as-view,
  hook-it-all-up, no-second-reader; no explicit `closes` — nothing said
  "back to X" in this shard).

## Vocabulary

Seeds used (69 acts): ask-action/one-decision x14, report/done x9,
qualify/caveat x10, verify/checked-live x6, delegate/to-agent x3,
ask-action/commission x3, ask-action/status-check x3, approve/plain x6,
propose/alternative, propose/next-step, clarify/check-understanding x2,
report-problem/observed, explain/mechanism, report/partial,
clarify/ask-decision.  Invented: 117 distinct labels over 133 acts —
mostly `report/*` diagnostics (status, dispatched, built, findings,
first-findings, gap-status, parked, ...) and `propose/*` designs, as in the
pilot; full list in `labels.py`.

## Multi-opener incidences worth a second look

- turn-oQdwzD r-1 answers three openers at once (f-s3-0 hook-it-all-up,
  f-s1-0 no-second-reader, f-s8-0 crime-scene-case) — the pipeline paragraph
  settles all three.
- turn-MLCYwI r-1 answers f-s3-0 (include faults) and f-s4-0 (signal scope);
  r-4 answers f-s7-0 and f-s6-0.
- turn-MDwJCW r-4 answers f-s5-0 (closes), f-s3-0 (closes), f-s2-0 (keeps).
- Several paragraphs → one opener: turn-eebQ4p f-s3-0 answered by r-1 and
  r-2 (both close); turn-bgSPxR f-s1-0 by r-0 and r-1.

## Input oddities

- The kernel's `:acts` texts are 120-char prefixes of the reply paragraphs;
  the validator prefers them, so quotes for acts the kernel recognised are
  drawn from the paragraph's first ~110 characters (the builder fits them;
  authored longer quotes are in `labels.py`).  Acts the kernel did not
  recognise (e.g. turn-4Vnjnj r-0) validate against the full `reply_acts`
  text.
- turn-8VoJLF r-0 and r-6 are not in the kernel's act list; kinds inferred
  (noted per act).
- turn-MDwJCW r-3 ("relevant to your earlier question") bears on an operator
  question of a previous turn that is neither a fragment nor in
  `previous_reply_acts`, so it carries no answer link.
- Typo in the operator text kept verbatim: "otphaned" (turn-eebQ4p f-s3-0),
  "reade" (turn-UHz87W f-s2-0).
- 6 records have no reply_acts at all (UBmb3D, UHz87W, zb5phn, zjFdHQ,
  g0WX7n, n5EIxj): fragments only, no answers possible.

## Time

About 2.5 h: reading the 30 inputs ~1 h, labelling + builder iteration ~1 h,
validator fitting, note and commit ~0.5 h.
