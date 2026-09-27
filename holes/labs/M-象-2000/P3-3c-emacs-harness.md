# P3-3c — Emacs turn-capture harness

Date: 2026-09-27

## Result

The Emacs evidence payload builder now writes `harness` for chat-turn and
turn-commits records from the turn's write-time provenance. It does not use the
displayed author or caller name.

- Operator input is `none`, based on the producer context, with the session as
  `source-ref`.
- An explicitly job-bound bell copies that job's harness. A bound job without
  a harness is `none`, with the job id as `source-ref`.
- A bell that is not bound to a job is `unknown` with the reason `bell turn has
  no bound Agency job id`.
- Parked-resume, followup, continuation, and explicitly typed delivery are
  `none`, with their source id or session as `source-ref`.
- Any other source is `unknown`; no author or caller string changes the result.

## Job-id availability

The Emacs turn producer does not currently receive a durable Agency job id.
`agent-chat-send-unsolicited-input` and the pending-turn queue preserve only
the supplied provenance plist (`emacs/agent-chat.el:2468-2502`). Park and
followup delivery supply their own source ids, not an invoke job id
(`emacs/agent-chat.el:2515-2601`). The bell envelope's `Edge:` line is prompt
text; no turn-capture code parses it or fetches the corresponding job.
`agent-follow-mode.el` does fetch job records, but that display poller does not
bind a job to a captured turn.

Therefore the implemented seam accepts `:delivery bell`, `:job-id`, and
`:job-harness` only when a trusted delivery producer supplies them together.
Until that producer exists, a bell turn must be stamped `unknown`; the code
does not infer `war-machine` from `Caller: wm-full-loop` or another name.

## Batch payload demonstration

The real `agent-chat-emit-turn-evidence!` payload builder produced:

```elisp
(:operator ((kind . "none") (basis . "producer-context")
            (source-ref . "p3-session"))
 :bell ((kind . "war-machine") (basis . "producer-context")
        (execution-id . "wm-run-7")))
```

## Validation

- `agent-turn-origin-test.el`: 9/9 passed, including the four requested
  harness cases and the `wm-full-loop` bad case.
- `futon4/dev/check-parens.el`: OK for both changed Emacs sources and the test.
- Byte compilation completed. The changed helper and test added no warnings;
  `agent-chat.el` retains its existing doc-width, optional-function, duplicate
  definition, and free-variable warnings.

## Loading

Joe can load the committed code in the running Emacs with:

```elisp
(progn
  (load "/home/joe/code/futon3c/emacs/agent-turn-origin.el" nil nil t)
  (load "/home/joe/code/futon3c/emacs/agent-chat.el" nil nil t))
```

No running Emacs was reloaded for this packet.
