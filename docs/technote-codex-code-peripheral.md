# Technote: Codex Code Peripheral

Date: 2026-02-28

## Problem

When Codex is invoked through the IRC bridge, it behaves like a chatbot:
it acknowledges the task and returns. It does not execute the task within the
same invocation. This creates a pattern where the orchestrator (Tickle) asks
Codex to do something, Codex says "will do", and nothing happens — because
there is no second turn.

```
<tickle-1> @codex-1 pick 5 new arXiv math.CT entries, write .tex files, open PR
<codex>    Acknowledged — will pick 5 fresh eprints and bundle them into a PR
           when ready.
```

This is not a Codex capability problem. Codex has full tool access (shell,
file I/O, git). The problem is the **invocation model**: the bridge sends
one prompt, reads one response, and disconnects. The response is Codex's
conversational output, not the result of tool execution.

## How Claude Code Differs

Claude Code (the `claude` CLI) completes multi-step tasks within a single
invocation. When asked "write 5 .tex files and open a PR", a single invoke
call produces this sequence:

```
1. Read repo contents                    (tool: glob, read)
2. Search arXiv metadata                 (tool: grep, read)
3. Write 5 .tex files                    (tool: write × 5)
4. Stage and commit                      (tool: bash — git add, git commit)
5. Push branch                           (tool: bash — git push)
6. Open PR                               (tool: bash — gh pr create)
7. Return: "PR #12 opened: <url>"        (final text response)
```

The agent loop continues until the task is done. Tool calls and reasoning
iterations interleave freely. The final text response references concrete
artifacts because the artifacts were produced during the same invocation.

### Key properties of the Claude Code execution model

**1. Tool-use-first.** The agent defaults to acting, not narrating. When it
encounters "write .tex files", it reaches for the write tool, not for words
about writing.

**2. Inner loop persistence.** Within a single invocation, the agent makes
arbitrarily many tool calls. Each tool result feeds into the next reasoning
step. The loop runs until the agent decides it's done — not until it has
produced N tokens of text.

**3. Artifact-gated exit.** The agent naturally terminates when it has produced
what was asked for. "Open a PR" means the invocation doesn't end until `gh pr
create` has run and returned a URL.

**4. Error recovery within the loop.** If `git push` fails (wrong branch,
auth error), the agent reads the error, fixes the problem, and retries — all
within the same invocation. There is no need for an external orchestrator to
detect the failure and re-invoke.

**5. Context accumulation.** Each tool result adds to the agent's context
within the invocation. By the time it writes the 5th .tex file, it has seen
the repo structure, existing entries, and the content of the first 4 files.
This prevents duplicates and maintains consistency.

## The Codex Code Peripheral

A futon3c peripheral that wraps the Codex CLI to emulate the Claude Code
execution model. The peripheral provides Codex with the multi-turn,
tool-use-first, artifact-gated execution loop that the one-shot bridge
invocation cannot.

### Architecture

```
IRC/Tickle                    Codex Code Peripheral              Codex CLI
───────────                   ─────────────────────              ─────────
                              ┌──────────────────┐
  "write 5 .tex,    ────►    │ Task intake       │
   open PR"                   │ Parse task spec   │
                              │ Set exit criteria  │
                              └────────┬─────────┘
                                       │
                              ┌────────▼─────────┐
                              │ Invoke Codex      │──────►  Turn 1
                              │                   │◄──────  "Will pick 5..."
                              │ Artifact check:   │
                              │   PR opened? NO   │
                              │   Files created?   │
                              │   Commits made?    │
                              └────────┬─────────┘
                                       │ (no artifacts)
                              ┌────────▼─────────┐
                              │ Retry with:       │──────►  Turn 2
                              │ "You acknowledged  │◄──────  (tool use + result)
                              │  but did not       │
                              │  execute. Do the   │
                              │  work now."        │
                              └────────┬─────────┘
                                       │ (artifacts detected)
                              ┌────────▼─────────┐
  "DONE #10 ::       ◄────   │ Return artifacts  │
   <pr-url>"                  │ Post to IRC       │
                              └──────────────────┘
```

### Peripheral spec

```clojure
{:peripheral/id   :codex-code
 :peripheral/type :execution
 :tools           #{:shell :read :write :glob :grep}
 :entry           {:task string? :repo string? :exit-criteria map?}
 :exit            {:artifacts vector? :evidence map?}
 :max-turns       10
 :turn-timeout-ms 120000
 :total-timeout-ms 600000}
```

### Execution loop

The inner loop is the core of the peripheral. It replaces the bridge's
one-shot invoke with a multi-turn conversation that continues until
artifacts are produced.

```
fn codex-code-execute(task, repo, exit-criteria):
    session-id = resume-or-create-session()
    context = format-task-prompt(task, repo)

    for turn in 1..max-turns:
        response = invoke-codex(context, session-id)
        artifacts = detect-artifacts(response, repo)

        if satisfies-exit-criteria(artifacts, exit-criteria):
            return {:ok true :artifacts artifacts}

        if turn == 1 and is-verbal-only(response):
            context = nudge-prompt(response)
            // "You said you would do X. Execute now. Use your
            //  shell and file tools. Do not describe what you
            //  plan to do — do it."
            continue

        if has-errors(response):
            context = error-recovery-prompt(response)
            continue

        if has-partial-progress(artifacts):
            context = continuation-prompt(response, remaining-work)
            continue

    return {:ok false :reason :max-turns-exceeded :partial artifacts}
```

### Artifact detection

The peripheral monitors for concrete outputs, not verbal claims:

| Artifact type | Detection method |
|---------------|-----------------|
| Files created | `git status --porcelain` in the target repo |
| PR opened | `gh pr list --state open --head <branch>` |
| Commits made | `git log --oneline -5` after invocation |
| Branch pushed | `git branch -r --contains HEAD` |
| Issue comment | GitHub API check for new comments |

The execution guard from `codex_cli.clj` (`enforce-execution-guard`) can
feed into this: if the stream shows zero tool calls, the response is
classified as verbal-only regardless of content.

### Nudge prompt design

The retry prompt after a verbal-only response:

```
Your previous response was planning-only — no tools were executed.

Task: {original-task}
Repo: {repo-path}

Execute now. Use your shell to:
1. cd {repo-path}
2. Create/edit the files
3. git checkout -b {branch}, git add, git commit
4. git push -u origin {branch}
5. gh pr create --title "..." --body "..."

Respond only after you have completed these steps.
Report the PR URL.
```

### Integration with the conductor

Tickle's conductor invokes the peripheral instead of sending IRC messages:

```
Current (one-shot, verbal ack):
  tickle → send-irc! "@codex-1 pick 5 entries..."
  bridge → invoke codex → "will do"
  (nothing happens)

With Codex Code peripheral:
  tickle → codex-code-execute! {:task "pick 5 entries..."
                                :repo "/home/joe/code/18_Cat..."
                                :exit-criteria {:pr-opened? true}}
  peripheral → invoke codex (turn 1) → verbal ack
  peripheral → nudge codex (turn 2) → tool execution
  peripheral → detect PR → return {:artifacts [{:type :pr :url "..."}]}
  tickle → send-irc! "DONE #N :: <pr-url>"
```

The conductor no longer needs to interpret IRC acks. It invokes the
peripheral, which returns concrete artifacts or a failure reason.

### Session continuity

The Codex CLI supports session resume (`--session-id`). The peripheral
maintains session IDs per task:

- **New task**: fresh session. Clean context, no carry-over from previous
  work.
- **Retry after failure**: resume session. Codex has context from the
  failed attempt and can pick up where it left off.
- **Follow-up work**: resume session. "The PR was reviewed with
  REQUEST_CHANGES. Address the feedback." — Codex has context about
  what it wrote and why.

### Differences from the existing codex_cli.clj invoke

The existing `make-invoke-fn` in `codex_cli.clj` is the right foundation.
It already provides:

- ProcessBuilder-based execution with streaming
- NDJSON event parsing with `on-event` callback
- Execution evidence tracking (tool call detection)
- `enforce-execution-guard` for verbal-ack detection
- Session ID management

What the peripheral adds:

| Capability | codex_cli.clj | Codex Code peripheral |
|------------|--------------|----------------------|
| Invocation | Single turn | Multi-turn loop |
| Verbal ack handling | Prepend disclaimer | Auto-retry with nudge |
| Artifact detection | None (text only) | git status + gh pr + commit check |
| Exit condition | Codex says it's done | Artifacts satisfy criteria |
| Error recovery | Return error | Retry with error context |
| IRC integration | None (returns text) | Posts DONE signal on completion |

### Failure modes and mitigations

**Codex loops without progress.** The `max-turns` limit (default 10)
prevents infinite loops. After max turns, the peripheral returns partial
results and the conductor can escalate.

**Codex produces wrong artifacts.** (Wrong repo, wrong file path.) The
artifact detection checks the *specific target repo*, not Codex's working
directory. If `git status` in the target repo shows no changes, the turn
is classified as no-progress regardless of what Codex claims.

**WS connection drops mid-execution.** The peripheral should run on the
same host as Codex (the laptop), not on Linode. This eliminates the WS
hop for the critical execution path. The peripheral reports results back
to the conductor via IRC or WS after completion.

**Codex CLI crashes.** The ProcessBuilder wrapper already handles this
(timeout + `destroyForcibly`). The peripheral treats it as a failed turn
and can retry.

## Relationship to existing architecture

The Codex Code peripheral fits into the futon3c peripheral system
(see `technote-codex-harness-comparison.md`):

- It implements the PeripheralRunner protocol (start → step* → stop)
- Each turn is a step that produces evidence
- The tool set is structural: shell, read, write, glob, grep
- The exit condition is artifact-gated, not turn-count-gated
- Session state persists across steps via the Codex session ID

It bridges the gap identified in the harness comparison: the Codex Harness
has the agent loop machinery, but the futon bridge was bypassing it by
treating Codex as a chatbot. The peripheral re-establishes the agent loop
by wrapping the CLI in a multi-turn execution envelope.

## Implementation priority

The peripheral has three implementation tiers:

**Tier 1 (immediate value):** Multi-turn retry loop wrapping the existing
`codex_cli.clj` invoke. Detect verbal-only responses, auto-retry with nudge
prompt. No new infrastructure — just a loop around the existing invoke-fn.

**Tier 2 (artifact awareness):** Add `git status` / `gh pr list` checks
after each turn. Return structured artifact maps instead of raw text. Wire
into the conductor as a replacement for `send-irc!` + bridge invoke.

**Tier 3 (full peripheral):** Implement PeripheralRunner protocol with
evidence emission. Support hop-in from other peripherals. Expose via WS
for remote invocation.

## Sources

- `src/futon3c/agents/codex_cli.clj` (existing ProcessBuilder invoke)
- `dev/futon3c/dev.clj` (conductor, execution guard, IRC integration)
- `scripts/ngircd_bridge.py` (current one-shot invocation model)
- `docs/technote-codex-harness-comparison.md` (harness/peripheral comparison)
- Claude Code source behavior (this agent's own execution model)
