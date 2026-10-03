# P24 — logic.xiang: the semantics stated once, in relations

Joe (2026-10-03), after porting Elephant-2000 into element-web: "we really
don't have a clearly defined Elephant-2000 semantics defined in code … we
should be able to create a bunch of automated acceptance tests, e.g., using
core.logic to certify end-to-end behaviour." Fixtures are to be plausible
agent-to-human and multi-agent dialogues, exercising more of the intents than
McCarthy's airline; the namespace is `xiang`, not `elephant`, so it can be
extended away from the source material.

## Where the semantics were

Four engines, none the authority, none checking another:
`agency/history_constraints.clj` (its own relational evaluator, two
constraints), `agency/rule_timeline.clj` + `rules_in_force.clj` +
`obligations.clj` (plain Clojure readers), `logic/*.clj` (core.logic
invariants, none about acts), `emacs/xiang-trace.el` (reazon relations over
the turn lifecycle). The Element port is a fifth with no spec.

## The kernel: `src/futon3c/logic/xiang.clj`

core.logic over a pldb fact base. Facts per act: `act id kind author at sys`
(valid time and system time, both as epoch seconds), `to`, `target`, `seat`,
`cites`, `carries-out`, `beneficiary`, `option`, `grantee`, `scope`. The
`kinds` table says what each of the 30 act kinds does to a standing: says /
creates / cancels / reverses / closes; an unknown kind is refused, never
ignored.

Relations: `visibleo a t s` (at ≤ t and sys ≤ s); `cancelledo` (a visible
cancelling act not undone by a visible `reversedo`); `closedo`;
`promulgatedo` (created, visible, not cancelled); `carried-outo`;
`in-forceo` (promulgated, and for a proposal also carried out by a commit:
Joe's P13b ruling that in force means applied); `visible-offero`;
`agreemento accept offer` (the offer was standing in the acceptance's own
seat at the acceptance's own valid and system time); `obligationo debtor
creditor source t s` (a standing unclosed promise, or a standing agreement
with the offeror owing the acceptor); `authorityo caller kind t s` (a
standing grant); `derivationo r a` (cites and carries-out, transitively).
Plain-data wrappers `in-force`, `promulgated`, `obligations`,
`agreement-status`, `authority`, `derivation`, `by-intent`.

One core.logic lesson, recorded so it is not relearned: `l/pred` closes
over its bound argument only. `visibleo` is called from `agreemento` with
the acceptance's own time as a fresh variable, and a closure over the raw
variable compared an LVar to a number. Both sides of the comparison are
projected at run time now.

## Fixtures: `test/futon3c/logic/xiang_fixtures/*.edn`

Each is `{:name :parties :history [acts] :expect {...}}`; `:expect` is the
acceptance test, keyed by as-of time (a string, or `[valid system]`).

- `red-tape.edn`: the mission's completion criterion as a history of joe,
  claude-11, the harness and claude-14. In force at 16:00 / 16:30 / 17:00 /
  09-25 21:00: `#{}` / `#{r1}` / `#{r1 r2 r3}` / `#{r1 r3}`; derivation of
  the withdrawal reaches back through the notices and the commit to the
  first report-problem.
- `offer-promise.edn`: codex-10 offers two options, joe says yes (typed
  09:07, stored 09:30: the 964fa778 chain gap, replayed), claude-17 promises
  a sha by 18:00 and delivers, 象 infers a withdrawal of claude-17's pattern
  card under a grant, joe undoes it, the grant is withdrawn, a second
  inference has no authority. Obligations at four as-of points including a
  two-axis one; authority true then false.
- `handoff.edn`: joe → claude-17 → codex-16 delegation; codex-16 promises
  and qualifies; a proposal deferred; claude-17 retracts its request and
  re-delegates to kimi-2, joe disagrees and says continue, claude-17
  retracts again; codex-16 fulfils; verify, explain, approve. The retracted
  request does not end the promise it elicited. Thirteen intents.

## Differential checks and laws

- `rule-timeline/as-of` over records adapted from `red-tape` agrees with the
  kernel at all four times. Where they differ is recorded as a test: the
  reader has no word for "adopted, not yet committed" and says "no committed
  rule observed" at 16:30 where the kernel has r2 promulgated.
- `obligations/obligations-as-of` over the `offer-promise` agreement agrees
  at three times; the test also records that the reader has one time axis
  (at valid 09:10 it counts the agreement the store learned of at 09:30, the
  kernel with s = t does not). This test is skipped where
  `agency/obligations` cannot load (it pulls the futon1b backend), so it did
  not run in this container; it runs on the box.
- Two test.check laws, 60 cases each over random histories: appending an
  unrelated later act never changes an earlier as-of answer; withdraw then
  reversal restores the in-force set exactly, and the withdrawal alone
  removes exactly the target.

Run here: 10 tests, 45 assertions, 0 failures (one skipped as above).

## What certification means from here

A frontend is certified when a fixture's history, replayed through that
frontend's own entry point, yields facts the kernel reads to the fixture's
`:expect`. The entry points: the Clojure readers (done for two), the HTTP
routes against a temp store, Emacs through batch ERT and agent-chat.el,
Element through the TypeScript client in Playwright. Each replay is a
warrant in the test registry. Not built yet: the replay harnesses for the
routes, Emacs and Element, and the fixture-to-futon1b-record adapter the
HTTP replay needs.

## Second packet (2026-10-03): the horizontal relation, flow, and the transitions census

Joe: the adjacency table is a prior for the dialogue game's transition
matrix; the evidence should tune it; `answerso`/`openo` matter because
dialogical connections are sometimes explicit; the flow can be classical.

- **Adjacency** (`logic.xiang/adjacency`): opener intent → {answer intent →
  :closes | :keeps}. Nine openers (propose, ask-action, offer, delegate,
  promise, report-problem, clarify, constrain, verify). Select-card, grant and
  withdraw were tried as openers and removed: they are standings, answered by
  the vertical relations, and as ports they never close. The table is the
  operator's to strike or extend; the python census mirrors it.
- **Links.** `explicit-answers` derives `answers` facts from a history: an
  act answers what it targets, what a commit carries out, and what it cites
  when the table says that pair is an answer. `answers-inferred` is a
  separate fact so the two grades never mix. `answerso r a effect`,
  `closes-porto` (a reversed closer reopens the port), `openo a t s`:
  the horizontal counterpart of `in-forceo`.
- **Flow.** `flow history & {:matrix :threshold}`: per act in time order,
  whether it opens a port, its links (explicit or inferred, with effect),
  the ports it closed, the ports open after it, its deposits (created,
  ended, restored, closed standings; commits with what they carry out),
  and whether it is annotative. `infer-links` lands an unlinked answering
  act on the most probable open port under the matrix at or above the
  threshold. `transition-counts` and `posterior` (table as pseudo-counts)
  live in the kernel too, so a fixture can be its own evidence.
- **Fixtures** gained `:open-at`, `:flow`, `:closes`, `:annotative`. Two of
  my first expectations were wrong and the kernel right: proposals are open
  until the commit answers them, and a promise is a port until fulfilled.
- **Census**: `scripts/xiang_transitions.py` over the readings store:
  consecutive-turn intent pairs within a session and explicit pairs by reply
  link; posterior with the mirrored table as prior; a report of pairs the
  data has that the table lacks and rows the table has that no reading
  shows; `--json` writes the matrix for `flow`'s `:matrix`. Runs on the box;
  4 tests here over synthetic records.

Kernel suite: 13 tests, 107 assertions, 0 failures. One known gap: the
mirrored table in the python script is checked against the Clojure one only
by eye; a test that loads both belongs with the HTTP replay packet.
