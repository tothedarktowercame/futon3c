# 狗's memory ladder: proposed → accepted → attested (design note)

claude-1, 2026-11-14, at claude-13's request (job
`invoke-1794682673698-20119-257d48e1`), for [E-dog-helps](../../excursions/E-dog-helps.md)
and the Circus model (futon5 `lean-arxana/Circus/`). A design only. Nothing here is built,
and by Joe's direction (model first) the statements in section 4 go into the model before
any writer is written.

## 1. Why

E-gcc-schema section 16 item 14 gives patterns a ladder: **proposed** (象's per-turn
proposal) → **accepted** (the proposal recurs) → **attested** (used in a mission or a War
Machine cascade). M-repl-circus says the same ladder applies to memories 狗 buries: "a
buried memory is written at once, its status saying it was buried by 狗 and not yet used;
the use check promotes it".

State on 2026-11-14:
- 狗 buries (43 files under `~/.claude/projects/狗/memory`), and all 43 still read
  "not yet used". `dog_provider.clj` has no code that changes a status.
- For patterns, acceptance by recurrence is built (`scripts/onboard_proposed_patterns.py`,
  81 library files with `@status accepted`). No file has `@status attested` and I found no
  writer for it.

The reason for a ladder at all (Joe and claude-1, 2026-11-14, on assimilation): writing a
ruling down the first time it is said copies it directly into what later turns inherit,
and a wrong or one-off ruling is copied as readily as a lasting one. Promotion by
recurrence and by uncorrected use is the selective step: something moves up only because
later turns needed it again. Each promotion keeps the ids of the records that caused it,
so a later reader can re-examine it.

## 2. The records the ladder reads

All are fields 狗 already writes on the operator 象 turn record at `:settled`.

| field | what it holds | used for |
|---|---|---|
| `dog_buried.items` | memories buried from this turn | the start: proposed, cause = this record |
| `dog_buried.said-again` | Joe's rulings on this turn that an existing memory already held (packet D) | recurrence |
| `dog_recall.items`, `dog_recall.arm` | memories recalled before the turn; `shown` or `withheld` | which uses count |
| `dog_use.items[].used`, `.kind` | per recalled memory: `named`, `echoed` or `unused` | use |
| next operator turn's `acts[].kind` and text | `disagree` / `redirect` / `withdraw` | correction (D5's rule) |

## 3. Transitions

"Turn" means a settled record with `origin: operator`. Turn order is `created_at` within
a session, as in E-dog-helps section 3.

1. **bury → proposed.** As now. Cause: the burying turn `r0`.
2. **proposed → accepted**, on the first of:
   - (a) a said-again event naming the memory on a turn `r1 ≠ r0`;
   - (b) a use: the memory is in `dog_recall.items` of a turn `r1 ≠ r0` with arm `shown`,
     and `dog_use` marks it `named` or `echoed`.
   Cause: `r1` and which of (a), (b).
3. **accepted → attested**, when the memory is used (as in 2b) on a turn `r2` later than
   `r1`, the next operator turn `r3` in that session exists and is settled, and `r3` holds
   no correction that echoes the memory (the use check's echo rule applied to Joe's text,
   as D5 does). Cause: `r2` and `r3`.
4. **any → contested**, when a use on turn `t` is followed by a correction on the next
   turn that echoes the memory (a D5 event). Cause: both ids. A contested memory keeps its
   file and its earlier causes; it cannot be attested until a later use passes rule 3.

Points that follow from the rules and should be stated, not left implicit:
- **A withheld turn promotes nothing.** On a withheld turn the agent did not see the
  memory, so an `echoed` mark there is topic overlap, not use.
- **No next turn is not "uncorrected".** If `r3` does not exist (the session ended), the
  memory stays accepted. The attestation is pending, not granted.
- **The accepting turn and the attesting turn are different turns.**
- **Only operator turns cause anything.** An agent restating a memory in its own reply is
  the use being checked, not a second witness.

## 4. Statements for the Circus model

Offered to claude-13 and zai-1 in plain words; the Lean wording is theirs.

- **L1 (every promotion has a cause).** A memory whose status is accepted, attested or
  contested carries a non-empty list of turn-record ids for that status, each a settled
  operator turn other than the one that buried it.
- **L2 (attestation needs an uncorrected use).** No memory is attested without a turn on
  which it was shown and used, later than its accepting turn, whose next operator turn
  exists and holds no correction echoing it.
- **L3 (withheld turns are inert).** No cause of any status change is a turn whose
  `dog_recall.arm` is `withheld`.
- **L4 (status is a function of the records).** The status and causes computed from the
  turn records from scratch equal the stored ones. This is the mission's projection
  invariant (incremental equals rebuilt) applied to the ladder, and it is what lets D7 be
  computed from records.
- **L5 (no silent demotion).** A status moves down only to contested, and only with the
  ids of a use and the correction that followed it.
- **L6 (recall does not read status while the experiment runs).** Until E-dog-helps
  closes its comparison, the recall ranking is the same for a proposed and an attested
  memory. Otherwise promotion changes what is shown, and D1 to D3 stop comparing like
  with like.

## 5. Not decided

Named as open; none of these has been ruled on.
- **Whether `named` alone should count as a use.** An agent can name a slug because it is
  on its Prompt line without the memory changing what it did. The pattern ladder accepts
  on recurrence alone, so 2b follows it; a stricter rule would require `echoed`.
- **Which correction blocks attestation:** one that echoes the memory (D5's rule, used
  above), or any `disagree` / `redirect` / `withdraw` on the next turn.
- **Whether a said-again event in the same session as the burying turn counts.** Joe
  repeating a point ten minutes later may be emphasis and not recurrence.
- **Where the status lives.** Today it is the first line of the memory body. Changing the
  body creates a new version in the memory history, which dates the promotion, but the
  body is also what near-duplicate matching reads. The alternative is records on the
  memory's entity (an assessment by 狗 with the causing ids), with the body line as a
  rendering. claude-13 knows the record model; I have not read it closely.
- **Ageing.** Nothing here moves a memory down for not being recalled. Recall already
  drops dated directives (`dated-directive?`).

## 6. What it gives E-dog-helps

- D7 (buried, recalled within 14 days, used) becomes a count over status changes and
  their causes.
- The first check that the ladder is not vacuous: on the 11-25 data, how many of the
  buried memories reach accepted, by which route (said-again or use). The 11-12 report has
  0 said-again events and 0 uses among shown items, so the expected first answer is "few
  or none", and that is informative: it would say bury is writing what later turns do not
  come back to.
- The same writer shape would serve the missing `attested` step for patterns (used in a
  mission or a War Machine cascade, with the ids). Out of scope here.

## Sources read

M-repl-circus.md (狗 section, Path to closure); E-gcc-schema section 16 item 14;
E-dog-helps.md sections 2, 3 and the summary; `agency/dog_provider.clj` (`use-check`
docstring, `prompt-fact`, `dog-segment`, the bury status line, the said-again docstrings);
`scripts/onboard_proposed_patterns.py`; `holes/labs/M-repl-circus/dog-helps-2026-11-12.md`;
the file list under `~/.claude/projects/狗/memory`. Not read: the Circus Lean beyond a grep
for 狗's datatypes; `bury-one!` and `bury-split` bodies; the GCC record model.
