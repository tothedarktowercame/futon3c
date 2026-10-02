# P23 — the 象 frontend ported: server-side pipeline, TypeScript client

Joe (2026-10-02): "I'd like to discuss porting the M-象-2000 frontend to
TypeScript so that we can use it from the web / inside Element / etc."; then
"Let's execute your plan." Built in a cloud session on branch
`claude/modest-newton-p7guja`, with no futon3c JVM, no futon1b and no Emacs
reachable. What was verified is stated below; what was not is stated too.

## The finding that shaped the port

The Emacs frontend is not only a view. `session-turn-analysis.el` records the
turn to `~/.emacs-graph/session-turn-analysis/turn-*.json`, redacts secrets by
shelling out to `secret_scan.py`, dispatches to a 象 seat through
`agency_send.py`, polls the job through `turn_dispatch_reap.py`, resets the
seat through `reset_seat_if_idle.py`, and then posts inferred withdrawals,
negations and notices to the JVM. A browser or Element widget has no
filesystem and no python3, so none of that can run in it. The port therefore
has two halves:

1. **Move the recorder, dispatcher and reaper into the JVM**, keeping the
   store directory and the file shapes exactly, so every existing consumer
   (`turn_frames.py`, `xlate.py census`, feed.html, the Emacs stepper and
   painter, `turn_dispatch_reap.py` itself) reads what it read before.
2. **Write the client as a pure consumer** of the resulting routes.

## What was built

### Server (`src/futon3c/xiang/`)

| namespace | ports | notes |
|---|---|---|
| `turn-record` | `session-mode--record-turn`, `--structure-turn`, `--sentence-spans`, `--elide-quotes`, `--turn-matches`, `agent-chat-split-surface-marker`, the analysis brief (instruction + the dispatch wrapper verbatim), `session_turn_analysis.py validate()`, `turn_dispatch_reap.py`'s classification, `secret_scan.py`'s rules | pure; every offset in codepoints (Clojure strings are UTF-16, converted at the boundary) |
| `turn-store` | the file store | same directory (`FUTON3C_TURN_ANALYSIS_DIR` overrides), same `turn-XXXXXX.json` / `.analysis.json` / `.candidates.json` names; atomic rewrite; exclusive-create publish |
| `turn-service` | `--dispatch-analysis`, `--reap-dispatch`, `--process-withdrawals`, seat selection/benching, the happened summary, `--retry` | every effect injected (bell, job status, the three routes, seat reset, scheduler, clock); the Emacs cadence (180 s ×3, then 600 s ×6; store-busy 60/180/600 s; bench 60 min; reset every 20) |

Routes, in `transport/http.clj` (`handle-xiang`, dispatched from `extra-routes`):

| route | replaces |
|---|---|
| `POST /api/alpha/xiang/turns` | `session-mode--record-turn` at send time |
| `POST /api/alpha/xiang/turns/:id/happened` | `--dispatch-analysis-after-reply` at reply end (summary built server-side from `{reply, commits}`) |
| `POST /api/alpha/xiang/turns/:id/dispatch`, `/reap` | `agency_send.py`, `turn_dispatch_reap.py --apply` |
| `POST /api/alpha/xiang/turns/:id/analysis` | `session_turn_analysis.py complete` (validated, published once) |
| `GET /api/alpha/xiang/turns/:id`, `GET …/turns?session=&agent=`, `GET /api/alpha/xiang/health` | reading the files; `session-mode--analysis-health` |

The effects are the in-process handlers the Emacs side reached over HTTP,
called with a synthetic request: `handle-bell`, the job ledger,
`handle-provisional-withdrawal`, `handle-negation-interpretation`,
`handle-turn-notice`, `reg/reset-session!` guarded by the same idle check as
`reset_seat_if_idle.py`. Their validation is therefore the same validation.

### Client (`packages/xiang-client/`)

TypeScript, no runtime dependencies. `types.ts` from the seam schema;
`offsets.ts`; `recognisers.ts` (acceptance, undo, surface marker, with the
cases from `agent-chat.el` and `agreement_record.clj`); `marks.ts` (the
painter's checks: ≤ 80 chars, ≤ 8 words, exact text at the offsets);
`stream.ts` (`invoke-stream` NDJSON); `client.ts`; `lines.ts` (the ✓/✗/?
lines); `matrix.ts` (an `m.room.message` with marks in `formatted_body`);
`conformance.ts` (`conformance.py` ported). README has the Element routes.

## Verified here

- Clojure: `futon3c.xiang.turn-record-test` 15 tests / 101 assertions,
  `turn-store-test` 4 / 22, `turn-service-test` 13 / 103; 0 failures. Run on
  a standalone classpath (clojure 1.12 + cheshire) because the repo's
  `local/root` deps are not in this container.
- `turn-record` reproduces the seam's example record's spans exactly, and
  four records the store writes (an emoji + CJK turn with a `>>>` quote, a
  dictated turn with a paragraph break, a turn with a GitHub token and
  Japanese, the seam example's own text) pass
  `packages/turn-seam/conformance.py`: 4 records, 4 conform. That is the
  codepoint test, run by the other implementation's checker.
- TypeScript: 27 tests, 0 failures (`npm test`), including the seam example
  through `conformance.ts` and an NDJSON chunk split inside a CJK character.
- `transport/http.clj` reads cleanly with the Clojure reader (570 top-level
  forms, 558 before; the 12 new definitions). Every symbol the new code uses
  is defined earlier in the file or required.

## Not verified here, and what Joe or a seat on the box should do

- `transport/http.clj` was not compiled or loaded: the full classpath is not
  available in this container. First step on the box: reload
  `futon3c.xiang.turn-record`, `turn-store`, `turn-service`, then
  `futon3c.transport.http` over Drawbridge, and `GET /api/alpha/xiang/health`.
- No live turn went through the routes. The acceptance case is the Emacs
  one: a typed turn → `POST …/turns` → `…/happened` → a bell to 象-1 → a
  reap that finds the reading → `withdrawal_effects` on the record.
- The Emacs side is untouched. Emacs still records and dispatches its own
  turns; the two producers share the directory and the id space
  (`turn-` + 6 alphanumerics, exclusive create on both sides), so they do not
  collide, but a turn typed in Emacs is not visible to a web client until the
  next session lists it, and vice versa. Switching Emacs to the routes is a
  later packet.

## Deviations from the Emacs code, deliberate

- **Failover order.** `session-mode--reap-dispatch` chose the other seat and
  then benched; with a pool (added 2026-09-30) the other seat is another pool
  seat, which the bench then covers, so the failover the pool docstring
  promises ("a usage limit benches the whole pool and the alternate takes
  over") never ran: the record was marked refused instead. The service benches
  first and chooses second; the test `a-seat-out-of-usage-is-benched-and-the-
  turn-goes-elsewhere` is the case. The Emacs code should get the same
  two-line swap.
- **REPL notices.** Emacs inserted "withdraw inferred: …" as a system line
  into the exact seat's buffer and recorded `repl_notice_*` on the outcome.
  The server has no buffer; `GET …/turns/:id` returns `notices` derived from
  `withdrawal_effects` and the client shows them. The header notice
  (`turn-notice`) is published exactly as before. The open fault from
  964fa778 (a notice landing under a later exchange) does not arise on this
  path: the notice is attached to its record, and the client shows it under
  that turn.
- **Pattern refs.** `validate()` resolved pattern ids against
  `futon3/library` on disk; the service does the same through
  `FUTON3_LIBRARY_DIR` (default `~/code/futon3/library`), and the analysis
  records `pattern_check: "resolved" | "unresolved"` so a reading validated
  without the library says so.
- **R-node fields** pass only when an `:rnode-contract` is supplied; the
  HTTP route supplies none yet, so they are dropped and counted under
  `rnode_validation.dropped.no_contract` rather than silently accepted.
- **Secrets.** The scanner's structural rules, keyword rule and entropy net
  are ported; the `.jsonl` escape handling is not (a turn is not a log line).
  In-process there is no subprocess to fail closed on; an exception from the
  redactor aborts the record, which is the same guarantee.

## Open

- The acceptance and undo grammars now live in three places (Elisp, Clojure,
  TypeScript). The cheaper arrangement is for `invoke-stream`'s `done` event
  to say "this was an acceptance", as it already carries `prompt-line`; then
  the client sends every turn and recognises nothing. Not done here.
- The Matrix bridge does not yet call the routes; `matrix.ts` is the
  renderer it would use.

## Second packet (2026-10-02): proforma marks, the bridge, the widget

Joe: "the proforma-to-象 would be good to add in any case. Beyond that, I
approve the build plan" (a local widget behind Caddy, friends in a Matrix room,
annotations for co-operators and contributing agents, a side pane of open
obligations).

- **Proforma marks read, not inferred.** `turn-record/proforma-marks` is the
  23-mark table from `session-mode--marks`; `reply-marks` finds the marked
  paragraphs of a reply with codepoint offsets. A record with `origin
  "agent"` carries them as `proforma_marks` and `author` = the agent;
  `agent-brief` tells 象 the marks are the author's declarations and that
  withdraw is never labelled on an agent turn. Withdrawal processing was
  already operator-only. An operator turn carries `operator_id` and
  `author` when the transport knows who typed it.
- **Identity.** `POST /api/alpha/xiang/turns` takes `operator-id`; with
  `FUTON3C_TRUST_FORWARDED_USER=1` the JVM takes `X-Forwarded-User` instead,
  which `deploy/Caddyfile` sets from the login and strips from clients. Acts
  (agreement, undo, provisional withdrawal) still assume the one operator;
  an allowlist for co-operators is the next identity packet.
- **Bridge.** `scripts/xiang_turns.py` (6 pytest cases) plus two hooks in
  `ngircd_bridge.py`, inherited by `MatrixBot`: the routed message is
  recorded as an operator turn when the job is queued (turn id
  `<transport>:<job-id>`, operator id the sender), and at reply end the
  record gets `happened` and the reply is recorded as an agent turn
  (`…:reply`, dispatched at once). Best effort, logged, never in the
  conversation's path; `FUTON3C_XIANG_BRIDGE=0` disables it. Not done: the
  bridge does not pass an evidence id, so a "yes" typed in a room is a turn,
  not an agreement, until the identity packet.
- **Widget.** `packages/xiang-client/widget/` (model.ts tested: 6 cases, 33
  TypeScript tests in all), bundled by esbuild to `dist/widget`;
  `deploy/Caddyfile` with basic_auth, same-origin proxy of the six routes
  the widget and bridge use, NDJSON unbuffered, everything else under
  `/api` refused. The obligations pane reads `GET /api/alpha/obligations`
  (`owes`/`owed`); the health pane reads `/api/alpha/xiang/health`.

Verified here: Clojure 36 tests / 250 assertions; TypeScript 33; pytest 6;
`ngircd_bridge.py` and `matrix_bridge.py` byte-compile; http.clj reads
(571 forms). Not verified: anything live, as before.

## Third packet (2026-10-02): the 小象 draft tier ("BNF at the level of turns")

Joe: 象 is slow; 小象 does basic annotation fast; can a classical best-effort
pre-parse speed up the LLM pass, as proforma compliance is free for the
coding agent? Yes, and the mechanism is the proforma move on the reader's
side: the structure is given, 象 confirms or corrects.

- **Draft.** `turn-record/validate-draft` canonicalises `xiaoxiang_preview.py`'s
  output against the record (exact codepoint spans; an intent only when 小象
  was sure, else the two guesses). `turn-store/write-draft!` keeps it as
  `turn-X.json.draft.json` and flags the record `draft_status`. The service
  runs the `:draft` effect at record time; http binds it to the preview
  script over stdin (a helper process, 20 s cap, nil on any failure) and
  also takes drafts at `POST …/turns/:id/draft`.
- **Brief.** With a draft, `analysis-brief` appends a section listing the
  fragments with their proposed intent and precision, saying offsets are
  firm and intents are proposals, and that the reading is recorded against
  the draft fragment by fragment.
- **Basis.** `annotate-with-draft` at publish stamps every fragment
  `xiaoxiang` / `xiang-relabelled` / `xiang-resegmented` / `xiang` and adds
  `draft_agreement` counts (plus `dropped` and `unsure`). This is the field
  that keeps a rubber stamp visible and lets 小象's editions exclude readings
  that were confirmed from its own draft. `GET /api/alpha/xiang/agreement`
  sums it over the store.
- **Skip policy.** `routine-draft?` is deliberately conservative (every
  fragment sure, none in the act-bearing set withdraw/retract/ask-action/
  delegate/disagree/constrain/redirect, every sentence covered, at most 3
  sentences, operator origin, not `yes`/`undo`, not tagging-failed). Behind
  `FUTON3C_XIANG_SKIP_ROUTINE`, off by default: the service test's fake 小象
  shows the failure mode, a mislabelled act would be swallowed, so the
  agreement numbers come first.
- **Widget.** Third tier of marks and a basis column; the health pane shows
  the agreement line.

Not done: precomputing pattern candidates per fragment (BM25 via `xlate.py
find`) before dispatch. It is the largest remaining chunk of the reading's
wall clock and needs only a JSON output mode on `xlate.py find` plus a
`:pattern-candidates` effect in the service; next packet.

Verified here: Clojure 43 tests / 293 assertions; TypeScript 35; the rest as
before. Nothing live.

## Fourth packet (2026-10-02): pattern candidates precomputed before dispatch

The largest remaining chunk of a reading's wall clock was the seat's own
pattern search: two or three `xlate.py find` calls per fragment, inside the
LLM loop. Now:

- `xlate.py find-many [-n N] [--with-candidates]` reads a JSON array of
  queries on stdin and answers one JSON object, one index load; `find --json`
  for a single query. Each hit carries id, score, title and the pattern's
  context and conclusion, read from its file (`excerpt`), since the index
  keeps tokens only and a reader deciding fit needs the IF/THEN text.
  `scripts/test_xlate_find_many.py` (4 cases) runs it over a three-pattern
  fake index.
- `turn-service/find-pattern-candidates!` runs the `:pattern-candidates`
  effect at dispatch over the draft's fragments (else the sentences; a
  one-word query is skipped), stores the result as `turn-X.json.patterns.json`,
  and `analysis-brief` appends a section listing the hits per query with
  the instruction to read them first, cite only a fit, and record near
  misses in `pattern_rejections`. A failing finder is logged in health and
  the brief goes without the section. http binds the effect to
  `python3 xlate.py find-many -n 5` over stdin, 30 s cap.
- `GET …/turns/:id` returns `pattern_candidates`; the widget shows the ids
  beside each draft fragment.

Verified here: Clojure 45 tests / 306 assertions; TypeScript 36; pytest 10;
http.clj reads (575 forms). Nothing live. What to measure on the box once
it runs: the reading's duration before and after (the job ledger has
started-at and finished-at), and how often the published refs are among the
precomputed hits, which says whether five per fragment is enough.

## Aside (2026-10-02): xiaoxiang-local.py made fast

Joe: make the downloadable log reader fast as well as good. Measured on
this container's one real Claude Code log (4.5 MB, 1,079 lines, 9 typed
turns): 2.4 s, of which the secret scan was 2.3 s (1.8 MB/s), JSON 0.03 s,
classification 0.1 ms per turn. Three changes, findings identical before
and after on the same corpus (97 findings, compared line by line):

- `secret_scan.py`: each structural rule declares the literal it cannot
  match without (`AKIA`/`ASIA`, `ghp_`…, `sk-`, `xox`, `AIza`, `eyJ`,
  `bearer`, `://`, `-----BEGIN`) and is skipped when the line lacks it; the
  keyword rule runs only when a keyword is followed by `:` or `=` somewhere
  in the line (so `"input_tokens": 4096` on every assistant record no
  longer costs a position walk); the entropy net runs only when a 32-run of
  token characters exists. A pattern opening with a lookbehind gets no
  literal fast path from `re`, which is why each rule cost 0.2 s per 4 MB.
  Scanner alone: 2.8× faster.
- `xiaoxiang_reader.py`: `read_file` + `merge`, with `--jobs` (default the
  CPU count) reading files in a `multiprocessing` pool, largest first.
  Four-way split of the same log: 1.0 s with one worker, 0.47 s with four.
  Serial and parallel reports are equal (tested).
- Line-level dedupe, which I had proposed, measured useless: 1,041 distinct
  of 1,145 lines but 99.8% of the bytes distinct. Dropped. What is real is
  a resumed session copying its transcript into a new file; `merge` counts
  such a file's turns and tokens once, keyed on the replies' message ids,
  while its secret occurrences still count (each copy is a place the value
  sits). Tested with a copied fixture file.

Not changed yet, from the earlier assessment: credential provenance
(typed / agent-written / tool output), precision shipped in the bundle,
and the end-of-window gap.

## Aside (2026-10-02): xiaoxiang-local.py made good

The quality changes from the assessment, plus Rob's report that `--days`
"doesn't actually filter the session file", which was true: files were
chosen by mtime and old token events dropped, but every turn in a chosen
file was counted and classified whatever its date.

- `--days` now keeps only turns and agent work inside the window;
  credentials are still counted wherever they sit in a chosen file, and the
  help text says so. Test: a file with turns at 00:00 and a window from
  04:00 reports one turn, not three.
- Provenance: each finding is grouped by where it sits (`where_in`): what
  you typed, tool output, files or commands the agent wrote, test fixtures
  (an agent write to a path saying test/spec/fixture/example, or a
  documented example key containing EXAMPLE), the agent's prose, elsewhere.
  On this session's log: 0 in what was typed, 15 distinct in tool output,
  9 in files the agent wrote, 6 fixtures.
- Precision ships in the model: `export` adds each intent's cross-validated
  precision; the reader labels a turn only when that is at least 0.5 and
  counts the rest as "not sure", and the table shows the precision column
  instead of the blanket "often wrong". An old model without precision
  renders as before.
- The gap after the last typed turn, closed by the last agent event, now
  counts and is marked "after your last turn"; likewise before the first.
  The chart's axis extends to it.

Tests: 67 passed across the five python suites (2 skipped for want of
labelled turns here). Not in this packet: a deterministic "phrases you use"
column from the cue vocabulary.
