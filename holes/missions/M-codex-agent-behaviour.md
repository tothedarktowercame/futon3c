# M-codex-agent-behaviour — Codex Execution Enforcement Across Invoke Paths

**Status:** IN-PROGRESS (IDENTIFY phase, 2026-03-08)
**Predecessor:** M-codex-irc-execution (DONE — established job state machine and evidence gating)

## Problem Statement

M-codex-irc-execution solved the *plumbing*: durable job records, execution
evidence gating, delivery receipts. But codex-1 still doesn't execute on IRC.
It writes plans, preps files, and claims completion — with zero tool events.

Claude-1 on IRC is near-perfect. Codex-1 is a disaster. Same bridge, same
surface headers, same HTTP handler. The divergence is downstream.

## 1. IDENTIFY

### Root Cause: Side-by-Side Comparison (claude-1 vs codex-1)

| Aspect | Claude-1 (works) | Codex-1 (broken) |
|--------|-------------------|-------------------|
| **Registry dispatch** | PATH A: local `invoke-fn` | PATH B: `ws-invoke/invoke!` (WS bridge) |
| **CLI invocation** | `claude -p --permission-mode bypassPermissions --output-format stream-json --verbose --resume SID -- PROMPT` | `codex exec --json --skip-git-repo-check --sandbox danger-full-access -c "approval_policy=never" resume SID -` |
| **Permission bypass** | `--permission-mode bypassPermissions` — full tool access | No equivalent flag — codex has sandbox but no permission bypass |
| **Output parsing** | Sophisticated NDJSON stream parser (dev.clj:2996-3039) — tracks tool use blocks, text, session IDs | Brittle: looks for last `agent_message` block in JSON output |
| **Enforcement retry** | Not needed — Claude just executes | Exists in `make-codex-invoke-fn` (dev.clj:3307-3336) but **only runs on local invoke path, NOT via WS bridge** |
| **Execution evidence** | Tracked via stream events, always populated | Same shape, but extraction fragile; often reports `executed?=false` when tools were used |

### The Enforcement Gap

`make-codex-invoke-fn` in dev.clj has enforcement logic:
- `codex-work-claim-without-execution?` — detects "I did X" with zero tool events
- `codex-task-reply-without-execution?` — detects task-mode replies with no execution
- `codex-format-refusal?` — detects format/planning refusals
- On detection: re-invokes with `codex-execution-followup-prompt` forcing execution

**This enforcement ONLY runs when codex is registered with a local invoke-fn
(the `:laptop` role path).** When codex-1 is invoked via IRC on Linode:

1. IRC bridge → HTTP `/api/alpha/invoke` → `registry/invoke-agent!`
2. Registry sees no `invoke-fn`, falls through to WS bridge
3. WS bridge sends frame to laptop
4. Laptop's `codex_ws_invoke_bridge.clj` calls bare `invoke-codex!` (sh/sh)
5. **No enforcement, no retry, no follow-up** — raw result returned as-is
6. Codex says "I prepped a file" → shown on IRC verbatim

### Evidence from Session (2026-03-08)

```
<joe> @codex you can now ask a question on ArSE with !ask <question>
<codex> [done codex-1772986872-1] Prepped the outgoing ArSE prompt in
    `/tmp/arse-fm001.txt`; ready to paste as soon as we fire `!ask`.
    refs: /tmp/arse-fm001.txt (session 019ccdc0)
```

Codex "prepped a file" instead of running the command. Zero tool events.

### Scope

**In scope:**
1. Port enforcement retry logic into the WS bridge invoke path
2. Ensure execution evidence is properly extracted from codex NDJSON
3. Make the enforcement prompt effective (codex must actually execute on retry)
4. Verify `--sandbox danger-full-access` + `approval_policy=never` is sufficient
   for autonomous execution, or identify what additional flag/config is needed

**Out of scope:**
1. Changing the Codex CLI itself
2. Redesigning the WS bridge architecture
3. Multi-agent scheduling

### Completion Criteria

1. Codex IRC invokes with task-mode prompts produce `executed?=true` with
   non-zero `tool-events` (same standard as M-codex-irc-execution)
2. When codex returns a plan-without-execution, the bridge automatically
   retries with an enforcement prompt before returning to IRC
3. Enforcement works identically whether codex is invoked locally or via WS
4. End-to-end demo: `@codex <task>` on IRC → codex executes tools → result
   with execution evidence shown on IRC

## 2. MAP

### Files Involved

| File | Role | Change Needed |
|------|------|---------------|
| `scripts/codex_ws_invoke_bridge.clj` | Standalone WS bridge (laptop) | Add enforcement retry after `invoke-codex!` |
| `dev/futon3c/dev.clj` (lines 3216-3382) | `make-codex-invoke-fn` | Reference implementation of enforcement logic |
| `dev/futon3c/dev.clj` (lines 314-530) | `start-codex-ws-bridge!` | In-process WS bridge — already uses `make-codex-invoke-fn` |
| `src/futon3c/agents/codex_cli.clj` | Codex CLI adapter | May need output parsing fixes |
| `src/futon3c/transport/http.clj` | `codex-task-no-execution?` | Downstream check — should align with enforcement |

### Strategy Options

**Option A: Port enforcement into standalone WS bridge**
- Copy `codex-work-claim-without-execution?` and retry logic into
  `codex_ws_invoke_bridge.clj`
- Pro: Minimal change, works with existing architecture
- Con: Logic duplication, two places to maintain

**Option B: Use in-process WS bridge (`start-codex-ws-bridge!`) instead**
- `start-codex-ws-bridge!` already uses `make-codex-invoke-fn` which has enforcement
- Pro: No duplication, enforcement "for free"
- Con: Requires laptop to run the in-process bridge (JVM), not standalone bb script

**Option C: Move enforcement into codex-cli adapter**
- Make `codex-cli/make-invoke-fn` itself retry on no-execution
- Pro: Single source of truth, works everywhere
- Con: Deeper change, needs careful testing

### Recommendation

**Option A first** (quick fix), then **Option C** (proper fix) as follow-up.
The laptop-side claude can implement Option A by reading the enforcement
patterns in `make-codex-invoke-fn` and porting them to the bb script.

## Handoff Notes for Laptop-Side Claude

The key code to read:
1. `dev/futon3c/dev.clj` lines 3270-3382 — enforcement detection + retry
2. `scripts/codex_ws_invoke_bridge.clj` lines 130-150 — `invoke-codex!` (needs wrapping)
3. `src/futon3c/agents/codex_cli.clj` lines 90-160 — output parsing (may be fragile)

The pattern: after `invoke-codex!` returns, check if the result has
`executed?=false` and the prompt was task-mode. If so, re-invoke with a
follow-up prompt like:

```
Your previous response described work but showed no tool execution.
Please actually execute the task now — run the commands, not just describe them.
```

Then return the retry result instead.
