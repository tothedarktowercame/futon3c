# Code Review: codex-repl.el

2039 lines. Last reviewed against current HEAD, 2026-03-10.

## Structure

| Section | Lines | Purpose |
|---|---|---|
| Config | 21–156 | 20 defcustom vars + 1 defgroup |
| Face | 158–163 | Single face for "codex:" prefix |
| Internal state | 165–224 | 18 vars + 5 consts (dashboard, timers, caches) |
| Progress helpers | 226–244 | Thinking elapsed time, progress line formatting |
| Drawbridge integration | 246–316 | Token reading, eval, API base discovery |
| Registry heartbeat | 340–382 | Report/clear/maybe-heartbeat invoke state |
| Thinking heartbeat | 384–406 | 1-second timer for progress + dashboard refresh |
| UI state repair | 408–452 | Validity check, defaults, marker restoration |
| Invoke dashboard | 454–590 | Buffer, display, trace, refresh, truncation |
| Tool humanization | 592–683 | titleize, humanize tool/item names, detail extraction |
| Stream event summary | 685–779 | One-line summaries for all event types |
| Stream inline display | 781–799 | Render selected events inline in chat |
| Stream event parser | 801–920 | Core NDJSON parser, progress status updates |
| JSON utilities | 922–937 | `codex-repl--parse-json-string` |
| Evidence logging | 939–1305 | 10 functions, ~365 lines |
| Session management | 1307–1382 | Refresh header, persist, session-start evidence |
| Output parsing (dead) | 1383–1444 | `--extract-agent-text`, `--parse-codex-json-output` |
| CLI arg building (dead) | 1446–1465 | `--build-codex-args` |
| Error classification | 1467–1501 | Stale session detection, progress status, clear state |
| Surface contract | 1503–1523 | Prompt injection for surface rules |
| Invoke finalization | 1525–1603 | Done event extraction, `--finish-invoke` |
| Async invoke | 1605–1689 | `--call-codex-async` (core HTTP client) |
| Modeline system | 1691–1804 | Compute, render, world-view, header-line (7 functions) |
| Modeline commands | 1806–1850 | describe, copy, insert-world, diagnose-routing |
| Invoke trace commands | 1852–1864 | show-invoke-trace, clear-invoke-trace |
| Interrupt | 1866–1882 | Custom interrupt with trace logging |
| Session reset | 1884–1961 | API reset, Drawbridge reset, new-session |
| Mode & entry | 1963–2039 | Keymap, mode def, send, clear, init, entry point |

---

## Issues

### D1. Dead CLI code (lines 1383–1465) — **cut, ~80 lines**

Three functions are never called:

- `codex-repl--extract-agent-text` (1385–1401) — extracts text from
  `item.completed` payloads from the old `codex exec --json` CLI flow
- `codex-repl--parse-codex-json-output` (1403–1444) — parses JSONL from
  `codex exec --json`
- `codex-repl--build-codex-args` (1448–1465) — builds argv for
  `codex exec --json`

All three are artifacts from before codex-repl switched to the HTTP
`/api/alpha/invoke-stream` endpoint. The docstrings even say "codex exec
--json" which is no longer the invoke path.

Grep confirms: none of these function names appear in any call site.

**Fix**: Delete all three functions. Also delete `codex-repl-codex-command`
(lines 32–38), `codex-repl-sandbox` (51–54), `codex-repl-approval-policy`
(56–59), `codex-repl-model` (61–65), and `codex-repl-reasoning-effort`
(67–71) if they are only used by the dead CLI path and the invoke trace
start message. Check first — `codex-repl-model` appears in the trace
message at line 1632 but that's purely cosmetic.

### D2. Duplicated event matching — summary vs parser (~80 lines wasted)

`codex-repl--stream-event-summary` (685–769) and
`codex-repl--parse-stream-event` (803–920) both contain large `cond` blocks
matching the same event types: `started`, `tool_use`, `text`, `done`,
`thread.started`, `item.started`, `item.completed`, `reasoning`,
`command_execution`, `turn.failed`, `error`.

The summary function produces a one-line string for the invoke trace.
The parser produces a progress status string. Both extract the same fields
from the same JSON structures, with slightly different formatting.

The actual call chain is: `--parse-stream-event` calls `--log-stream-event`
which calls `--stream-event-summary`. So every event is matched **twice**.

**Fix**: Merge into one function that returns both the progress status and
the trace summary, or have the parser call the summary function and derive
its progress from it. Either approach eliminates ~60–80 lines of redundant
pattern matching.

### D3. Evidence functions duplicated with claude-repl (~150 lines)

These functions are structurally identical to their claude-repl counterparts,
differing only in variable names and tag strings ("codex" vs "claude"):

| codex-repl | claude-repl | Lines |
|---|---|---|
| `--evidence-enabled-p` (939) | `--evidence-enabled-p` (209) | 3 |
| `--evidence-base-url` (944) | `--evidence-base-url` (214) | 3 |
| `--evidence-request-json` (957) | `--evidence-request-json` (219) | 20 |
| `--evidence-post-entry-id` (1217) | `--evidence-post-entry-id` (243) | 10 |
| `--evidence-fetch-latest-id` (1228) | `--evidence-fetch-latest-id` (254) | 18 |
| `--sync-evidence-anchor!` (1249) | `--sync-evidence-anchor!` (274) | 10 |
| `--emit-turn-evidence!` (1260) | `--emit-turn-evidence!` (313) | 40 |
| `--emit-session-start-evidence!` (1346) | `--emit-session-start-evidence!` (285) | 30 |

Total: ~135 lines of nearly identical code in each file.

**Fix**: Move into `agent-chat.el` parameterized by agent-name and
transport-name. Each REPL provides values at init. Saves ~120 lines per
file.

### D4. Invoke dashboard is ~135 lines of rarely-used UI

Lines 454–590 contain:

- `codex-repl--invoke-buffer` (454–461)
- `codex-repl--display-invoke-buffer` (463–474)
- `codex-repl--truncate-single-line` (476–482)
- `codex-repl--append-invoke-trace` (531–538)
- `codex-repl--refresh-invoke-dashboard` (540–590)

Plus 30 lines of state variables (196–224) and 12 lines of commands
(1852–1864).

The dashboard erases and redraws its entire buffer on every event (line 554:
`erase-buffer`). During active tool use, this fires multiple times per
second. The spinner animates on a 1-second timer. The buffer shows
session, prompt preview, activity, trace entries, and status — but all of
this information is already visible in the progress line of the main chat
buffer.

Nobody uses `C-c C-v` during normal operation. The dashboard is a debugging
aid that became permanent furniture.

**Suggestion**: Make it opt-in (default `codex-repl-show-invoke-buffer` to
nil instead of t). Or remove entirely and let users tail the process buffer
if they need raw trace. Saves ~135 lines + 30 lines of state.

### D5. Routing diagnostics are ~115 lines of developer tooling

Lines 1069–1181 contain:

- `codex-repl--routing-bases` (1069–1077)
- `codex-repl--probe-agent-routing` (1079–1114)
- `codex-repl--routing-diagnostics` (1116–1127)
- `codex-repl--routing-summary-text` (1129–1150)
- `codex-repl--routing-diagnostics-report` (1152–1181)

Plus `codex-repl-diagnose-routing` command (1836–1850) and the
`C-c C-d` binding.

This probes Agency HTTP endpoints, fetches agent metadata, formats
multi-line reports, and caches results with TTL. It's useful when
debugging transport issues but has nothing to do with chatting with Codex.

**Suggestion**: Extract to a separate `codex-repl-diag.el` or
`agent-chat-diag.el` that can be loaded on demand. This would also benefit
claude-repl.

### D6. Modeline system is ~115 lines for a header-line

Lines 1691–1804 contain 8 functions:

- `codex-repl--compute-modeline-state` (1696–1723) — builds a plist with
  transport entries, session, availability
- `codex-repl-modeline-state` (1725–1730) — cached access
- `codex-repl--render-modeline` (1732–1745) — plist → string
- `codex-repl--world-view-string` (1747–1767) — plist → multi-line
- `codex-repl--header-line` (1769–1783) — header-line-format eval
- `codex-repl-refresh-header-line` (1785–1794) — force update
- `codex-repl--ensure-header-line!` (1796–1800) — set format + refresh
- `codex-repl--build-modeline` (1802–1804) — entry point for system prompt

Plus 4 interactive commands (1806–1850): `describe-modeline`,
`copy-modeline`, `describe-world`, `insert-world-view`.

Compare with claude-repl's modeline: **9 lines** (725–733), one function.

The state computation calls `codex-repl--base-reachable-p` (an HTTP call)
on every header-line eval unless cached. That means a blocking synchronous
HTTP request can fire during Emacs redisplay.

**Fix**: At minimum, the header-line eval should never make network calls
synchronously. Cache the state and refresh only on explicit request or
after an invoke completes. Longer term, this belongs in a shared diagnostic
module.

### D7. Registry heartbeat is 43 lines of Drawbridge RPC (lines 340–382)

`codex-repl--report-registry-invoke-state!` formats a Clojure expression
string and evals it via Drawbridge HTTP POST every 5 seconds while Codex is
thinking. The only purpose is to update the JVM registry's `*agents*` view
with "emacs-codex-repl is invoking...".

This is infrastructure for infrastructure — the `*agents*` view itself is
a dev diagnostic. And it makes synchronous HTTP calls on a 1-second timer
(the heartbeat fires from `--start-thinking-heartbeat` at line 394, which
also calls `--refresh-invoke-dashboard`).

**Question for Joe**: Is anyone monitoring the `*agents*` view during
normal operation? If not, this can be removed or made opt-in.

### D8. Thinking heartbeat timer is over-defensive (lines 384–406)

`codex-repl--start-thinking-heartbeat` runs a lambda every 1 second that:
1. Updates the progress line in the chat buffer
2. Refreshes the invoke dashboard
3. (via `--maybe-heartbeat-registry-invoke!` in the dashboard) heartbeats
   the JVM registry

The progress line already updates on every stream event from the filter.
The 1-second timer only adds value during periods with no events (model
thinking time). But the cost is nontrivial: every second it redraws the
entire dashboard buffer (erase + rebuild).

**Fix**: Increase interval to 3–5 seconds, or remove and accept slightly
stale progress during thinking gaps.

### D9. `codex-repl--drawbridge-eval` double-parses with regex fallback (lines 268–304)

The function first tries `codex-repl--parse-json-string`, then falls back
to regex extraction of `:ok` and `:value` from EDN-like text (lines
288–302). This dual-parse path exists because Drawbridge returns EDN, not
JSON, and the JSON parse fails.

But line 287 calls `codex-repl--parse-json-string` which will always return
nil for EDN. So every Drawbridge call goes through the regex fallback. The
JSON parse attempt is wasted work.

**Fix**: If Drawbridge always returns EDN, just use the regex/EDN parser.
Drop the `codex-repl--parse-json-string` call from this function.

### D10. `codex-repl--parse-json-string` has unnecessary Emacs version compat (lines 922–937)

```elisp
(if (fboundp 'json-parse-string)
    (json-parse-string ...)
  (let ((json-object-type 'plist) ...)
    (json-read-from-string text)))
```

`json-parse-string` was added in Emacs 27.1 (2020). The fallback to
`json-read-from-string` supports Emacs 26. This file already uses
`json-serialize` (line 1613) which is also Emacs 27+, so the compat branch
is dead code.

**Fix**: Drop the `fboundp` check and the `json-read-from-string` branch.

### D11. `codex-repl--evidence-query-string` is a 6-line reimplementation (lines 949–955)

This reimplements `url-build-query-string` / manual query encoding. Emacs
has `url-build-query-string` in `url-util` (already required at line 19).
claude-repl avoids this by inlining the `format` call directly.

**Fix**: Use `url-build-query-string` or inline like claude-repl does.

### D12. Streaming display functions duplicated with claude-repl

`codex-repl--begin-streaming-message` (484–498),
`codex-repl--stream-text` (500–514), and
`codex-repl--end-streaming-message` (516–529) are near-identical to the
corresponding functions in claude-repl (369–410), which were originally
adapted from codex-repl.

Both versions:
- Insert name with face overlay at prompt marker
- Set a streaming marker
- Append text with face overlay
- Finalize with `\n\n` and clear state

The only difference: codex-repl's `--stream-text` accepts an optional
`face` parameter, and codex-repl tracks `--last-stream-summary`.

**Fix**: Move base streaming into `agent-chat.el`. Each REPL can wrap
or extend.

### D13. Surface contract is injected but never appears in the prompt (lines 1503–1523)

`codex-repl--surface-contract` builds a multi-line string but is only called
from `codex-repl--build-modeline` → `codex-repl-modeline-state` →
`codex-repl--compute-modeline-state`. Wait — actually checking the call
chain: `--surface-contract` is called from... let me trace:

It's called from `--build-modeline` (line 1802–1804): no, that calls
`--render-modeline`. Searching for `surface-contract`:

Line 1503: defined. Where called? Searching... It's called from within
`codex-repl--call-codex-async`? No, searching the function at 1605 — the
prompt is just `text`, no surface contract prepended.

**If `--surface-contract` is never injected into the actual prompt sent to
Codex**, then the entire function and its supporting `--routing-summary-text`
call are dead code in the invoke path. It may only be used in the modeline
render for display purposes.

**Fix**: Verify the call site. If unused in the prompt, remove or document
why it exists.

### D14. `codex-repl--stale-session-error-p` is half-live (lines 1467–1480)

This function detects stale session errors using string patterns from the
old Codex CLI protocol: "action.type", "invalid value: 'other'",
"supported values are: 'search', 'open_page', and 'find_in_page'" (line
1473–1474). These are very specific OpenAI-protocol-drift signatures.

It's called from `codex-repl--stream-error-progress-status` (line 1486),
which is called from `--parse-stream-event` (lines 849, 907, 911). So it
IS used — but the patterns may be stale if the Codex protocol has moved on.

The function name and patterns suggest this was written for a specific
failure mode that may no longer occur. If it still occurs, fine. If not,
these are confusing archeological artifacts.

**Fix**: Add a comment documenting when these patterns were last seen, or
simplify to just the generic patterns (400 bad request, unknown thread,
not found).

### D15. `codex-repl--ui-state-valid-p` and `--restore-ui-state` are defensive against stale buffers (lines 408–452)

These 45 lines exist to repair buffer state when someone kills and reopens
`*codex-repl*` without reinitializing. The restore function regex-searches
for `"^> "` to find the prompt marker.

claude-repl doesn't need this because it creates workspace-specific buffer
names and always runs init on first use. codex-repl's entry point (line
2027–2032) has a complex fallback chain: check validity → try restore →
erase and reinit.

**Question**: How often does this actually trigger? If it's rare, the 45
lines of repair code could be replaced with always-reinit (erase + init),
which is simpler and more reliable.

### D16. IRC send machinery is ~90 lines of niche functionality

Lines 979–1067 plus 1183–1215:

- `codex-repl--irc-send-regex` (979–981)
- `codex-repl--extract-irc-send-directive` (983–993)
- `codex-repl--strip-irc-send-directives` (995–1002)
- `codex-repl--normalize-base-url` (1004–1007)
- `codex-repl--health-irc-send-base` (1009–1020)
- `codex-repl--irc-send-candidate-bases` (1022–1034)
- `codex-repl--send-irc-via-base` (1036–1052)
- `codex-repl--send-irc-via-agency` (1054–1067)
- `codex-repl--apply-irc-send-directive` (1183–1200)
- `codex-repl--irc-send-request-regexes` (1202–1209)
- `codex-repl--likely-irc-send-request-p` (1211–1215)

This is real functionality — Codex can post to IRC via directive. But it's
self-contained and could be a separate module loaded by both REPLs if
claude-repl ever needs it.

**Suggestion**: Extract to `agent-chat-irc.el`. The REPL just calls
`agent-chat-apply-irc-directive` in its finalize path.

### D17. Config vars `codex-repl-codex-command` is documented as legacy (lines 32–38)

The docstring says: "Retained for diagnostics and legacy helpers;
`codex-repl' no longer shells out to it directly for normal turns."

If it's retained for diagnostics, what diagnostics? Nothing in the file
calls this variable. It was the path to the `codex` binary for the old
`codex exec` flow.

**Fix**: Delete it along with the other dead CLI config (D1).

### D18. `codex-repl--clear-session-state!` is mostly dead (lines 1491–1501)

This function clears all session state and deletes the session file. It's
not called from anywhere in the file — `codex-repl-new-session` (line 1926)
does the same work inline (lines 1935–1942).

**Fix**: Either use this function from `new-session`, or delete it.

---

## Structural observations

### What's clean

- **`codex-repl--call-codex-async`** (1605–1689) is well-structured.
  The sentinel delegates to `--finish-invoke` instead of being a 97-line
  lambda (unlike claude-repl). Good factoring.

- **`codex-repl--finish-invoke`** (1541–1603) cleanly separates finalization
  from the process sentinel. It handles streaming/non-streaming, evidence,
  session persistence, and cleanup in one place with `unwind-protect`.

- **`codex-repl-new-session`** (1926–1961) has a nice guard against
  interrupting a live process (line 1929–1930), which claude-repl lacks.

- **`codex-repl--init`** (2004–2016) is refreshingly simple — 12 lines.
  Compare with claude-repl's `--init` which has the workspace tangles.

- **Streaming inline events** (781–799) is a clean dispatch that reuses
  `--stream-event-summary` for both text and tool annotations.

### What's problematic

- **The file has no clear layering.** Evidence, IRC, Drawbridge, dashboard,
  modeline, routing diagnostics, streaming, and session management are all
  interleaved. There's no separation between "core chat" and "diagnostics/
  infrastructure."

- **State variable count (18)** is high and most are global `defvar`s, not
  `defvar-local`. This means running two `*codex-repl*` buffers (which
  can't happen currently due to the fixed buffer name) would share state.
  Not a current bug, but a latent one.

- **Synchronous HTTP in redisplay paths.** The header-line eval chain
  (`codex-repl--header-line` → `codex-repl-modeline-state` →
  `codex-repl--compute-modeline-state` → `codex-repl--base-reachable-p`)
  can block Emacs redisplay with a synchronous HTTP health check. If the
  server is down or slow, the UI freezes.

---

## Summary

| Priority | Issue | Lines saved | Effort |
|---|---|---|---|
| **High** | D1: Delete dead CLI code | ~100 | trivial |
| **High** | D2: Merge duplicated event matching | ~70 | moderate |
| **High** | D6: Fix synchronous HTTP in redisplay | 0 (correctness) | moderate |
| **Medium** | D3: Extract evidence to agent-chat.el | ~120 | moderate |
| **Medium** | D4: Make invoke dashboard opt-in or remove | ~165 | easy |
| **Medium** | D5: Extract routing diagnostics | ~115 | easy |
| **Medium** | D12: Extract streaming to agent-chat.el | ~45 | moderate |
| **Medium** | D16: Extract IRC send to module | ~90 | easy |
| **Medium** | D18: Delete dead `--clear-session-state!` | ~10 | trivial |
| **Low** | D7: Make registry heartbeat opt-in | ~43 | easy |
| **Low** | D8: Reduce heartbeat frequency | 0 (perf) | trivial |
| **Low** | D9: Remove wasted JSON parse in Drawbridge | ~5 | trivial |
| **Low** | D10: Remove Emacs 26 compat code | ~5 | trivial |
| **Low** | D11: Use url-build-query-string | ~5 | trivial |
| **Low** | D13: Verify surface contract call site | ~20 | investigate |
| **Low** | D14: Document or simplify stale-session patterns | 0 (clarity) | trivial |
| **Low** | D15: Consider always-reinit vs repair | ~45 | needs decision |
| **Low** | D17: Delete legacy codex-command var | ~7 | trivial |

### Reduction potential

| Action | Lines saved |
|---|---|
| Delete dead code (D1, D17, D18) | ~117 |
| Merge event matching (D2) | ~70 |
| Extract evidence to agent-chat.el (D3) | ~120 |
| Default dashboard off or remove (D4) | ~165 |
| Extract routing diagnostics (D5) | ~115 |
| Simplify modeline to claude-repl level (D6) | ~100 |
| Extract streaming to agent-chat.el (D12) | ~45 |
| Extract IRC send (D16) | ~90 |
| **Total** | **~822** |

**Current: 2039 lines. After cleanup: ~1217 lines.**

With shared evidence and streaming in agent-chat.el, and diagnostics/IRC
in optional modules, the core codex-repl would be ~1000 lines — comparable
to claude-repl's 945.

### Cross-reference with claude-repl review

Issues shared between both reviews:

| Issue | claude-repl | codex-repl |
|---|---|---|
| Evidence duplication | B7 | D3 |
| Streaming duplication | (in claude-repl) | D12 |
| Drawbridge token reading | B8 | D9 |
| Dead code | B2 (130 lines) | D1 (100 lines) |

Both files would benefit from the same refactoring: extract evidence,
streaming, and diagnostics into `agent-chat.el` and optional modules.
The two REPLs would converge to ~700–800 lines each of genuinely
REPL-specific code.
