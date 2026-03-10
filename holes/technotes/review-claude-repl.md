# Code Review: claude-repl.el

945 lines. Last reviewed against current HEAD, 2026-03-10.

## Structure

| Section | Lines | Purpose |
|---|---|---|
| Config | 19–56 | 5 defcustom vars + 1 defgroup |
| Face | 58–63 | Single face for "claude:" prefix |
| Workspace detection | 65–71 | `claude-repl--workspace` |
| Internal state | 72–80 | 3 evidence-tracking vars |
| Auto-registration | 82–205 | Find/reuse/register agents |
| Evidence logging | 207–359 | 7 functions, ~150 lines |
| Streaming display | 361–410 | begin/stream/end streaming |
| Streaming invoke | 411–590 | `claude-repl--call-claude-streaming` |
| Non-streaming invoke | 592–721 | `claude-repl--call-claude-async` |
| Modeline | 723–733 | Transport description string |
| Mode & keymap | 735–766 | Mode definition, RET/C-c bindings |
| Drawbridge config | 768–788 | 2 defcustom + token reader |
| Session reset | 790–878 | API reset, Drawbridge reset, new-session |
| Init & entry | 881–945 | Buffer init, `claude-repl`, reconnect |

---

## Issues

### B1. Hardcoded API URL (line 25) — **bug**

```elisp
(defcustom claude-repl-api-url "http://localhost:7070"
```

This is the only REPL that hardcodes a port. `codex-repl` uses
`agent-chat-agency-base-url` which reads `FUTON3C_PORT` from the environment.
This breaks on the laptop where `FUTON3C_PORT=47070`.

**Fix**: Default to `agent-chat-agency-base-url` like codex-repl does.

### B2. Non-streaming fallback is dead code (lines 592–721) — **cut**

`claude-repl--call-claude-async` (130 lines) is never called. The send path
is `claude-repl-send-input` → `claude-repl--call-claude-streaming`. Nothing
references the non-streaming function.

This is 14% of the file doing nothing. It also duplicates the retry logic,
session-id handling, and error formatting from the streaming path — so if
bugs are fixed in one, the other drifts.

**Fix**: Delete `claude-repl--call-claude-async` entirely. If a non-streaming
fallback is ever needed, add it back then.

### B3. Drawbridge defcustoms misplaced (lines 768–777) — **style**

Two `defcustom` declarations (`claude-repl-drawbridge-url`,
`claude-repl-drawbridge-token`) appear at line 768, after the mode definition
(line 745) and send-input (line 754). All other defcustoms are at lines 25–56.

**Fix**: Move to the configuration block at the top of the file.

### B4. `claude-repl--workspace` called 5 times redundantly in init (lines 881–918)

```elisp
(defun claude-repl--init ()
  ...
  (claude-repl--auto-register)                     ; calls it internally
  (when-let ((ws (claude-repl--workspace)))         ; call 1
    ...)
  (let* (...
         (ws (claude-repl--workspace))              ; call 2 (shadows let above)
         (title (if (claude-repl--workspace)        ; call 3
                    (format "..." (claude-repl--workspace))  ; call 4
                  "claude repl")))
```

The function is trivial (just `(daemonp)`) but this is messy — `ws` is bound
twice in nested lets, then `claude-repl--workspace` is called two more times
instead of using the `ws` binding.

**Fix**: Bind `ws` once at the top of the function, use it throughout.

### B5. `full-prompt` wraps user text in agent metadata (lines 421–422, 601–602)

```elisp
(full-prompt (format "Agent: %s\n\nUser message:\n%s"
                     claude-repl-agent-id text))
```

This client-side prompt wrapping is odd. The server knows which agent it's
invoking (the agent-id is in the JSON body). Prepending "Agent: claude-1"
to every user message wastes tokens and could confuse the model.

**Question for Joe**: Is this intentional? Does the server-side invoke-fn
expect this format, or is this a leftover from an earlier protocol?

### B6. Retry logic uses recursive call (lines 564, 577–578)

On agent-not-found, the streaming sentinel re-registers and then:

```elisp
(when retried
  (claude-repl--call-claude-streaming text callback))
```

This works but has no retry limit. If registration succeeds but the agent
immediately becomes unregistered again, this recurses forever. The
non-streaming path (line 699–705) has the same pattern.

**Fix**: Add a retry counter (even just `(unless retried ...)` to limit to
one retry).

### B7. Evidence functions are near-identical to codex-repl (lines 209–359)

These 7 functions are structurally identical between claude-repl and
codex-repl, differing only in tag values ("claude" vs "codex") and transport
name ("emacs-claude-repl" vs "emacs-codex-repl"):

- `claude-repl--evidence-enabled-p` / `codex-repl--evidence-enabled-p`
- `claude-repl--evidence-base-url` / `codex-repl--evidence-base-url`
- `claude-repl--evidence-request-json` / `codex-repl--evidence-request-json`
- `claude-repl--evidence-post-entry-id` / `codex-repl--evidence-post-entry-id`
- `claude-repl--evidence-fetch-latest-id` / `codex-repl--evidence-fetch-latest-id`
- `claude-repl--sync-evidence-anchor!` / `codex-repl--sync-evidence-anchor!`
- `claude-repl--emit-turn-evidence!` / `codex-repl--emit-turn-evidence!`

This is ~150 lines duplicated across both files. The transport/tag differences
could be parameterized.

**Fix**: Move the evidence helpers into `agent-chat.el` with agent-name and
transport-name parameters. Each REPL provides its values at init time.

### B8. Drawbridge reset uses raw Clojure string (lines 820–821)

```elisp
(format "(let [r (swap-vals! futon3c.agency.registry/!registry update \"%s\" ...)]
```

Embedding a Clojure expression as a format string in Elisp is fragile. If the
registry namespace or atom name changes, this breaks silently. Also, the
Drawbridge reset path (lines 817–845) duplicates the HTTP boilerplate from
`claude-repl--reset-via-api` (lines 790–815) — both do url-retrieve, parse
JSON, extract status.

**Suggestion**: The API reset endpoint exists and works. The Drawbridge
fallback is a safety net from before the API endpoint was reliable. Consider
whether it's still needed. If kept, extract the Clojure eval into a shared
Drawbridge helper (codex-repl already has `codex-repl--drawbridge-eval`).

### B9. Session file race with IRC relay (line 35)

```elisp
(defcustom claude-repl-session-file "/tmp/futon-session-id"
```

This file is documented as "shared with IRC relay." Both claude-repl and
the IRC relay read/write it. There's no locking — if the relay writes a new
session-id between when Emacs reads it and when Emacs writes it back, the
session can desync.

**Risk**: Low in practice (writes are infrequent), but worth noting. The
workspace-specific session files (line 894) avoid this for multi-daemon
setups.

### B10. `json-parse-buffer` vs `json-parse-string` inconsistency

The file uses two different JSON parsing patterns:
- `json-parse-buffer` with `:object-type 'alist` (line 98, in auto-registration)
- `json-parse-string` with `:object-type 'alist` (lines 453, 519, 644, 808)

Both work, but the `json-parse-buffer` call in `claude-repl--find-idle-agent`
doesn't handle hash-table vs alist consistently — note the explicit hash-table
check at lines 102–106. This suggests the server response format varies or the
`:object-type` argument isn't being respected in all Emacs versions.

**Question**: Is the hash-table branch (lines 102–106) still needed? If
`:object-type 'alist` is always used, agents-val should always be an alist.

---

## Structural observations

### What's clean

- **Auto-registration** (lines 84–205) is well-designed: workspace-aware,
  respects I-1 (never steals agents from other workspaces), falls back
  gracefully. This is the strongest section of the file.

- **Streaming display** (lines 369–410) is minimal and correct: three
  functions, clear lifecycle (begin → stream → end), proper overlay
  management.

- **`claude-repl-send-input`** (lines 754–761) is beautifully concise — just
  delegates to `agent-chat-send-input` with the right hooks. This is what
  good shared infrastructure looks like.

- **Entry point** (lines 921–934) handles workspace naming, buffer reuse,
  and lazy init cleanly.

### What could be tighter

- **The streaming sentinel** (lines 493–589) is a 97-line lambda. The nesting
  is 6–7 levels deep in places. This is the hardest part of the file to read
  and modify. Extracting the done-event parsing and error handling into named
  functions would help.

- **Session reset** (lines 790–878) has two code paths (API and Drawbridge)
  that share structure but aren't factored. The `claude-repl-new-session`
  function at line 847 is fine but the two reset implementations above it
  are boilerplate-heavy.

---

## Summary

| Priority | Issue | Lines saved | Effort |
|---|---|---|---|
| High | B2: Delete dead non-streaming code | 130 | trivial |
| High | B1: Fix hardcoded port | 0 (1-line change) | trivial |
| Medium | B7: Extract evidence to agent-chat.el | ~120 (in this file) | moderate |
| Medium | B4: Clean up workspace binding | 0 (clarity) | trivial |
| Medium | B3: Move misplaced defcustoms | 0 (organization) | trivial |
| Low | B6: Add retry limit | 2 | trivial |
| Low | B5: Question prompt wrapping | 0 | needs answer |
| Low | B8: Consider dropping Drawbridge reset | ~30 | needs decision |
| Low | B10: Check hash-table branch | ~5 | needs testing |

**If B2 and B7 are addressed, claude-repl drops to ~695 lines.** The
non-streaming fallback and duplicated evidence code account for 280 lines
that provide no unique value.
