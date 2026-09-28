# P10-2d-3 — where an inferred withdrawal should be shown

Discovery only, 2026-09-28. No runtime or source change is made here.

## 1. Current-turn header

`futon3c.transport.http/wrap-surface-header` builds the authoritative block at
`src/futon3c/transport/http.clj:4593-4635`. It writes `Surface`, `From`, `To`,
`Origin`, optional `Edge`, and `Caller`, then calls `prompt-facts-line` before
the reply-delivery contract (`http.clj:4614-4634`). The six-argument arity is
the one that has the exact agent and session (`http.clj:4604-4605`).

`prompt-facts-line` calls `prompt-line/render!` with that exact identity and the
surface (`http.clj:4573-4582`). It turns the returned segments, in registry
order, into the single `Prompt:` line (`http.clj:4583-4590`). The registry runs
bounded providers and composes their values/markers in
`src/futon3c/agency/prompt_line.clj:83-139`; `render!` also remembers the last
render by `[agent session]` (`prompt_line.clj:141-153`). The HTTP prompt-line
GET uses the same renderer with `:surface :http`
(`http.clj:9491-9499`). Emacs fetches that endpoint for the REPL prompt at
`emacs/agent-chat.el:864-929`.

Therefore a normal prompt-line provider is not quite the right once-only
carrier. A GET used to draw Joe's shell prompt can run it before the next turn
header exists. Consumption must occur at header construction, not at generic
prompt rendering.

## 2. Existing exact-seat publication paths

Two JVM paths already publish observations directly into exact-seat caches:

* Context retrieval calls `pattern-card-provider/observe-results!` before the
  evidence append (`dev/futon3c/dev.clj:992-1018`). The provider stores it by
  `[agent session]` (`src/futon3c/agency/pattern_card_provider.clj:158-168`).
* A verified pattern-card write calls `publish-card-result!` through
  `publish-pattern-card-write!` (`http.clj:9279-9283`); the cache key is again
  the exact seat (`pattern_card_provider.clj:41-63`). This is the path the
  provisional-withdrawal route uses after an effect is actually written.

`observe-entry!` is narrower: it accepts only persisted `context-retrieval`
evidence (`pattern_card_provider.clj:126-149`). The generic Emacs evidence POST
does not feed arbitrary records into a prompt provider. Thus a successful
provisional effect can reach the current pattern-card cache, but the two cases
that need explanation most — local unresolved target and 403 no grant — have
no existing JVM observation. The JVM need not read
`~/.emacs-graph/session-turn-analysis/`; Emacs can publish the already recorded
outcome over a small route, as the card route publishes its verified result.

## 3. Smallest bounded design

Add a dedicated exact-seat **turn notice**, separate from the shell prompt
segments:

1. `futon3c.agency.turn-notice` holds one FIFO per `[agent session]`. A notice
   has a stable id (the interpretation record id plus fragment id), exact seat,
   observed time, and exactly one of these texts:
   * `withdraw inferred: unresolved (no target)`
   * `withdraw inferred: effect act:… (undo to reverse)`
   * `withdraw inferred: off (no grant)`
   It keeps a bounded set of accepted ids, so a repeated Emacs POST cannot
   enqueue the same notice twice. `take!` atomically removes one notice.
2. Add `POST /api/alpha/turn-notice` in `src/futon3c/transport/http.clj`.
   It validates caller `xiang`, the exact identity, notice id, and the three
   allowed outcome shapes, then calls `turn-notice/publish!`. It does not accept
   arbitrary header prose.
3. `wrap-surface-header` calls `turn-notice/take!` only in its six-argument
   exact-seat path and inserts the returned text as its own line after the
   ordinary `Prompt:` facts and before the reply-delivery contract. Prompt-line
   GETs and Emacs prompt redraws cannot consume it. Atomic removal means a
   notice cannot appear in a later header after it appeared in one header.
4. In `emacs/session-turn-analysis.el`, after an outcome is durably added to
   `withdrawal_effects` (`session-turn-analysis.el:540-578`), publish its notice
   id to that route. Record `header_notice_published_at` on that outcome only
   after a 2xx response. Independently find the live buffer whose buffer-local
   `agent-chat--agent-id` and `agent-chat--session-id` equal the record, insert
   the same text once with `agent-chat-insert-message`, and then record
   `repl_notice_delivered_at`. Missing buffers remain pending for a later reap;
   neither destination uses `message` as its delivery receipt.

This gives two separately inspectable deliveries. Reaping the same analysis
again sees the recorded timestamps and does neither again. A JVM restart after
publication but before header construction can lose an in-memory notice; it
cannot duplicate one because Emacs has recorded successful publication. If
crash-durable delivery is required, the next packet must persist a notice and a
consumption receipt in an append-only store rather than claiming that an atom
provides it.

### Files and tests

The smallest implementation packet changes:

* new `src/futon3c/agency/turn_notice.clj`;
* `src/futon3c/transport/http.clj`;
* `emacs/session-turn-analysis.el`;
* `test/futon3c/agency/turn_notice_test.clj`;
* `test/futon3c/transport/prompt_line_header_test.clj` and a focused HTTP route
  test;
* `test/session-mode-test.el`.

Tests pin exact-seat isolation; duplicate publish idempotence; FIFO `take!`;
header consumption exactly once; prompt-line GET not consuming; the three
fixed texts; exact-buffer insertion once; absent-buffer pending state; and a
repeated reap producing neither a second header notice nor a second REPL line.
