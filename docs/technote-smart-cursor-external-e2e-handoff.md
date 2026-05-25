# Smart Cursor External E2E Handoff

This note is for running smart-cursor E2E investigations from an
external shell process, not from the live Emacs/agency agent session.

## Scope

The current external harness is intentionally non-editing. It proves
cursor control by:

- switching among live REPL buffers
- reading the word at point
- moving back one word
- reading again
- capturing a final snapshot

That respects the current invariant: no buffer edits are required to
prove cursor ownership.

## Relevant code

- Emacs script runner: [emacs/smart-cursor.el](/home/joe/code/futon3c/emacs/smart-cursor.el:783)
- Timed `run-script` executor: [emacs/smart-cursor.el](/home/joe/code/futon3c/emacs/smart-cursor.el:847)
- Surface projection contract: [src/futon3c/peripheral/emacs_cursor.clj](/home/joe/code/futon3c/src/futon3c/peripheral/emacs_cursor.clj:114)
- Drawbridge reload path: [README-drawbridge.md](/home/joe/code/futon3c/README-drawbridge.md:1)

## Preconditions

1. `futon3c` is already running.
2. Emacs is already running and reachable via `emacsclient`.
3. `smart-cursor-mode` is already enabled in that Emacs.
4. The target REPL buffers already exist.

## One-command external cycle

Default buffer set:

- `*codex-repl:codex-8*`
- `*claude-repl:claude-1*`
- `*claude-repl:claude-10*`
- `*claude-repl:claude-9*`
- `*claude-repl:claude-7*`

Run:

```bash
cd /home/joe/code/futon3c
bash scripts/smart-cursor-e2e-cycle.sh
```

Artifacts:

- structured reply: `/tmp/smart_cursor_e2e_cycle_reply.sexp`
- trace tail: `/tmp/smart_cursor_e2e_cycle_reply.trace.txt`

Override output paths:

```bash
SMART_CURSOR_E2E_OUTFILE=/tmp/my-reply.sexp \
SMART_CURSOR_E2E_TRACE_OUTFILE=/tmp/my-trace.txt \
bash scripts/smart-cursor-e2e-cycle.sh
```

Override buffer list:

```bash
bash scripts/smart-cursor-e2e-cycle.sh \
  "*codex-repl:codex-8*" \
  "*claude-repl:claude-1*"
```

## If code changed

Reload the Emacs client code from an external shell:

```bash
emacsclient --eval '(load-file "/home/joe/code/futon3c/emacs/smart-cursor.el")'
```

If server-side smart-cursor code changed, reload via Drawbridge from a
separate shell:

```bash
cd /home/joe/code/futon3c
bash scripts/proof-eval.sh '
(do
  (load-file "src/futon3c/peripheral/emacs_cursor.clj")
  (load-file "src/futon3c/transport/http.clj")
  :reloaded)'
```

Do not restart `futon3c` from the live agency-routed agent session.

## Known finding as of 2026-05-25

The external autonomous cycle succeeds and is fast. In the captured run
on `2026-05-25`, the whole five-buffer cycle completed in about `14ms`,
with each `switch-buffer` step taking `1-2ms`. That means the observed
"slow to get control" feeling is not yet reproduced by the primitive
external cursor drive itself.

The next structural step is to instrument boundary timestamps across:

- command issue time in the external process
- first point/buffer change observed in Emacs
- first WS/context acknowledgment back from smart-cursor
