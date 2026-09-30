# INTERACT-1 — interactive features for Elephant-2000 compliance

claude-17, 2026-09-30, after Joe: "the mission should have given us the core
functionality we need based on the paper, but we haven't built demos of all of
that … spec out a range of interactive features that would make 象-2000
'interactionally compliant' with Elephant-2000 … then we could set about getting
a kimi agent to build and test them."

**Draft for Joe.** Nothing here is dispatched until he has read the list.

## What "interactionally compliant" means here

McCarthy's program talks to people: it is asked, answers, promises, is
authorised, and can say what it is committed to (DERIVE-1 R1, R5, R6, R10, R26).
The mission built most of that as **records and queries**: grants, promises and
their outcomes, withdrawals, agreements, the owes/owed ledger, the rule
timeline, and the constraint matcher. Its acceptance was about stores, so a
query run by an agent counted as met. Compliance in the interactional sense
needs one more thing. **Each speech act that Joe performs or receives in the REPL
creates or reads the record without anyone running a script, and Joe can see
the result where he is working.**

A feature below counts as done when a **dramaturge scene** (§0) drives it in a
scratch Emacs. The scene types what Joe would type and waits for the record.
It then asserts on the buffer and on the store, and plants the bad case the
feature is named for.

Sources: DERIVE-1 (the R-numbers cite `elephant.tex` lines), BUILD-PLAN v2 (the
P-numbers), and the mission's INSTANTIATE log for what is built. Built-but-not-
interactive is the common case.

## §0 Prerequisite: the dramaturge as a scene driver

**What exists.** `futon4/dev/arxana-dramaturge.el` (2026-05-04, 528 lines) is a
registry of read-time tests. It has `deftest`, `assert-true`, `assert-equal`,
`assert-buffer-contains`, `await` (poll a predicate with a timeout),
`fetch-json` and `run-all`. It asserts on what surfaces render. It does not drive
them: it cannot type, press keys, or run in an Emacs other than Joe's.

**What to add** (`arxana-dramaturge-scene`, a thin layer over emacsclient):

- **A scratch Emacs.** Scenes run in `emacs --daemon=dramaturge`, which loads
  `futon3c/emacs` and `futon4/dev` from the checkouts. They never run in Joe's
  Emacs, because tonight's stepper work took over two of his windows while it
  was being tested there.
- **Steps:**
  - `(open-repl AGENT)`, `(type TEXT)` and `(key "RET")`, all issued through
    `execute-kbd-macro` in the target buffer, so keymaps are exercised (the r/R
    keymap bug tonight was invisible to function-level tests);
  - `(await PRED :timeout S)`;
  - `(assert-buffer REGEXP)`, `(assert-store QUERY EXPECTED)`;
  - `(snapshot NAME)`, which saves the buffer text with its faces as an
    `.htmlize` file for Joe to look at afterwards.
- **A stub agent.** By default the REPL's call function is replaced by a
  replayer of canned NDJSON streams (`scenes/streams/*.ndjson`), so a scene
  costs no tokens and is deterministic. One smoke scene per feature runs
  against a real cheap seat (a kimi) and is marked `:live`.
- **A scratch store.** Store assertions read futon1b through the existing
  as-of reads. Writes go to a scene-tagged session id that each scene deletes or
  ignores afterwards. A hermetic futon1b is not in scope; see the memory note
  on runner probes.
- **Output:** one line per scene and step (pass, fail, or timed out), plus the
  snapshots. `M-x arxana-dramaturge-run-scenes` from Joe's Emacs starts the
  daemon and shows the report, so Joe watches results rather than driving.

**Acceptance:** a scene re-enacts tonight's r/R bug. It loads the stepper
twice, as a live reload does, then presses `r`. With the bug planted (keys
bound inside the defvar), the scene fails at the keypress, and with the fix it
passes. Also, a scene whose `await` never comes true reports "timed out"
rather than hanging.

## The feature list

Each row gives McCarthy's requirement, what is built, the interactive feature,
the scene that accepts it, and the bad case the scene plants.

| # | McCarthy (R) | Built (packets) | Interactive feature | Scene / bad case |
|---|---|---|---|---|
| I1 | R26, R32: the program can say what its commitments are | P9 ledger (owes/owed, `:current`; as-of times out on futon1b) | `M-x 象-ledger` in a REPL: what this agent owes and is owed, now or as of the stepper's frame (`L` in the stepper). Promises, parks and agreements, each with status and deadline. | Agent parks with a deadline; the ledger shows it `:open`, then `:overdue` after the deadline. Bad case: a wait-release shown as paid (DERIVE-2 item 8). |
| I2 | R6, R28: a promise is a record; its outcome is a second act | P1, P2a–c, P5, P8 (fulfilment checks) | Prompt-line segment `owes 2 (1 overdue)`. When a check fires, one line in the REPL: `promise … fulfilled / not fulfilled / cannot tell`, with its ref. | A canned promise whose criterion is not met at the deadline, with a wake delivered. The scene expects "not fulfilled". Bad case: wake counted as fulfilment. |
| I3 | R10, R11: acts are authorised; permissions are given | P3 grants, P4 overreach report, act stamps | In the stepper's HAPPENED rows, each agent act shows `signer / authority` and is flagged `overreach` or `unverified executor`. In the REPL, `C-c C-g` on an agent's request writes a grant with the scope shown, and the scope is echoed back. | An act signed for joe with no grant appears flagged in the stepper. Bad case: an interpretation id offered as authority is accepted. |
| I4 | R12: offer and acceptance make an agreement | P11, live (agreement act:ba40d9aa…) | Already interactive. The scene makes it repeatable: the agent offers options, the rendered scopes appear, Joe types `2`, and the agreement plus a bounded grant appear in the stepper frame. | Bad case: an option without `:grant-until` yields a grant. |
| I5 | R14, R19: withdrawal is an act; commitments end only by one | P10 (explicit withdrawals, rule families); the provisional path is deferred (17.4) | Joe says "withdraw that" about the active card: `withdrawn? … (act id)` on the prompt line, and the card is inactive from that time; `undo` reverses it. **Deferred:** the provisional grant (17.4). Only the explicit, stamped withdrawal is in scope now. | An explicit withdrawal of the active card, then an as-of read before and after. Bad case: a withdrawal aimed at an obsolete rule version ends the current one. |
| I6 | R27: the implementer states its assumptions; a correction is a recorded act | P12 (assumption list, operator negation, escalation live) | A report's `Assumed X, took Y` lines are clickable. `n` on one records Joe's negation and sends it back as the next turn's first line. The stepper shows negations in the frame. | A canned report with two assumptions; Joe negates one; the store has the negation act referring to it. Bad case: a negation stored as free text with no ref. |
| I7 | R4, R5: answers truthful and responsive; "I don't know" admissible | P18 answer envelope (P9 only), P16 not built | When an agent answers a question from Joe, the envelope (the population, when the data was read, and the store read) shows as a fold under the answer. Joe's `k` ("now I know") records P16's confirmation. The stepper lists questions answered but not confirmed. **Needs Joe:** P16 is a new act type (plan: 需 Joe 决定). | Question → answer → `k`, forming a chain. Bad case: a question with an answer and no confirmation counted as resolved. |
| I8 | R8: hearing that vs learning that | P15 not built; 象's reading is the "learning" record for operator turns | The stepper shows, per turn, **received** (turn record written) and **understood** (象's reading landed), with times. xiang-trace already records both, and this makes them visible per frame. **Needs Joe:** P15 is a new record type. | A turn whose reading failed shows received but not understood. Bad case: received rendered as understood (tonight's 504 turns). |
| I9 | R20, R21: rules are relations over history; state them without knowing data structures | P17 matcher (built; batch) | Joe states a constraint in the REPL (`!rule no bell without --from`). It becomes a rule record, is evaluated on each new act (the elephantKanren standing-relation loop), and a violation appears as one line in the REPL within a turn. | Inject a bell without `--from`; the violation line appears, and a legal bell produces none. Bad case: a violation created by a withdrawal is missed (the P17 plan's bad case). |
| I10 | R13: institutions are dated; judge an act under the rule in force | P13a/b rule records, rule-timeline as-of | `I` in the stepper: the rules in force at this frame's time, with version and family. An act in HAPPENED can be judged against them in place. | Step across a rule's adoption; the frame before does not list it and the frame after does. Bad case: the current version used for an old frame. |
| I11 | R17–R19: one history; as-of; exists(t, x) | Stepper, r/R, turn_frames (tonight) | Done as a view and as git rewind, and the REPL cut sends the notice. Remaining: `a` in the stepper is an as-of query at this frame ("what was open, granted and in force"), which combines I1, I3 and I10 at one time. | The scene steps back three frames and asserts that the ledger, grants and rules all read at that frame's time. Bad case: any one of the three read at "now". |
| I12 | R29: communications among parts of the program are acts | P19 not built | 象 reads agent turns as well as operator turns (promises made in prose, design claims with their qualifications). The stepper shows them in the frame. | Run over the M-象-2000 MAP turns; the "only by correction" qualification is listed (P19's own acceptance). Bad case: a promise stated only in prose not listed in I1. |
| I13 | R3: correctness conditions generated from the program text | xiang_acceptance.py (6 checks), xiang-trace rules (4) | The checks run on each new event (the elephantKanren loop), not on demand. A violation lights 象's modeline segment with the rule name, and clicking it opens `*象 trace check*`. | Plant a reply end with no dispatch; the modeline shows it within one event. Bad case: an open step (<15 min) reported as a violation. |

## Suggested order for Kimi

Each item is one packet, with one feature and one scene. §0 is first, since
every later acceptance depends on it.

1. §0 scene driver, including the stub-stream replayer (futon4/dev +
   futon3c/emacs/scenes).
2. I13, then I11. Both are small and build on tonight's code; they also check
   the driver against features claude-17 knows well.
3. I1, I2 (the ledger and promises), then I9 (the standing rule loop).
4. I3, I4, I6, I10 (read-mostly views of built records).
5. I5 (explicit path only), I12.
6. I7 and I8 wait for Joe's decisions on P15/P16.

## Deferred, with re-arm conditions (war-room/wr-26)

- **Rewind by session fork** (Joe, 2026-09-30: "would create a cascade of things
  to fix, let's not build it right now"). Tested and working in print mode:
  `--resume S --resume-session-at <entry> --resume-drops-turn <prompt>
  --fork-session`. The fork forgot the dropped turn, and the original file was
  unchanged. Not built, because a fork gives the seat a new session id, and the
  stepper, the evidence rows and the Agent-Session commit trailer all key on the
  session id. **Re-arm when** any one of these holds: the session lineage is a
  recorded edge (fork-of) that `turn_frames.py` and the signing hook follow;
  Claude Code can truncate a resume in place without a new id; or the stepper
  keys on the seat plus a lineage rather than the session. Today's substitute
  is the "Operator reverted frame N" notice.
- **I5 provisional withdrawals** and P10's re-label review list: deferred by
  Joe (item 17.4). Re-arm on his user-side demonstration condition, as
  recorded in the mission.
- **I7, I8** are not deferred. They wait for Joe's decision on the new record
  types P15/P16.
