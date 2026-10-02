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
