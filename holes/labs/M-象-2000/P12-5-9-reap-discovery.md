# P12-5-9: why Joe's negation was not posted by the reaper

## Finding

For `turn-g5VNfb.json`, the reaper had not yet reached its first scheduled
check.  This case does not establish a silent failure in
`session-mode--process-withdrawals`.

The turn record says it was created at `2026-09-29T00:39:58Z`.  Its analysis
file was published at `00:41:36.999Z`.  The manual call rewrote the turn record
with both outcomes at `00:41:59.139Z`.  Emacs waits 180 seconds after dispatch
before its first reap
([session-turn-analysis.el:440](../../../emacs/session-turn-analysis.el#L440),
[session-turn-analysis.el:996](../../../emacs/session-turn-analysis.el#L996)),
so the normal first check was due at about 00:42:58, nearly a minute after the
manual call.

There is still a real reliability gap: a job found `running` is checked only
three times, 180 seconds apart.  After the third `running` result no timer is
left
([session-turn-analysis.el:793](../../../emacs/session-turn-analysis.el#L793),
[session-turn-analysis.el:841](../../../emacs/session-turn-analysis.el#L841)).
An analysis that finishes later can say `analyzed` forever without Emacs
processing its withdrawals.

## Who writes `analyzed`, and who processes withdrawals

The Python completion tool publishes the immutable `.analysis.json`, then
sets `analysis_status` to `analyzed` on the request record itself
([session_turn_analysis.py:205](../../../scripts/session_turn_analysis.py#L205),
[session_turn_analysis.py:229](../../../scripts/session_turn_analysis.py#L229)).
That write does not call Emacs.

Separately, the Emacs dispatch sentinel records the job id and schedules the
first reap
([session-turn-analysis.el:986](../../../emacs/session-turn-analysis.el#L986),
[session-turn-analysis.el:996](../../../emacs/session-turn-analysis.el#L996)).
Only a reap whose output contains `analyzed` calls
`session-mode--handle-reap-output`; that calls
`session-mode--process-withdrawals`
([session-turn-analysis.el:839](../../../emacs/session-turn-analysis.el#L839),
[session-turn-analysis.el:783](../../../emacs/session-turn-analysis.el#L783)).
Thus the record can become `analyzed` before the first reap, as happened here,
or after the last bounded reap.

The handler currently discards every processing exception and nevertheless
marks analysis health `ok`
([session-turn-analysis.el:785](../../../emacs/session-turn-analysis.el#L785)).
The existing ERT test explicitly preserves that behaviour by making
`session-mode--process-withdrawals` throw and expecting health `ok`
([session-mode-test.el:263](../../../test/session-mode-test.el#L263)).

## Sentinel buffer check

The route functions obtain agent and session from the JSON record, not from
the current buffer
([session-turn-analysis.el:511](../../../emacs/session-turn-analysis.el#L511),
[session-turn-analysis.el:550](../../../emacs/session-turn-analysis.el#L550)).
They bind their JSON parsing choices while reading the record
([session-turn-analysis.el:721](../../../emacs/session-turn-analysis.el#L721)).
`agent-chat-agency-base-url` is a global `defcustom`, not buffer-local
([agent-chat.el:27](../../../emacs/agent-chat.el#L27)); the HTTP helper also
binds its request variables lexically for each call
([agent-chat.el:3230](../../../emacs/agent-chat.el#L3230)).  The only relevant
chat-buffer lookup is for inserting the optional local notice, and it searches
all live buffers by the record's exact seat
([session-turn-analysis.el:612](../../../emacs/session-turn-analysis.el#L612)).

I reproduced `session-mode--process-withdrawals` inside a temporary buffer
named like ` *session-analysis-reap*`, with the HTTP helper stubbed.  It wrote
the 403 provisional outcome successfully; neither the base URL nor JSON
variables were buffer-local there.  The test did not reproduce a
sentinel-buffer exception.  The existing record tests also exercise route
posting and lossless rewrite independently of a chat buffer
([session-mode-test.el:796](../../../test/session-mode-test.el#L796)).

## Logs and other records

`*Messages*` contains the message produced by the manual call, “象: inferred
withdrawals are off until Joe’s grant exists,” but no reap error for this turn.
That absence cannot prove success: the handler deliberately swallows errors
without logging them.

A read-only scan of every `*.analysis.json` under
`~/.emacs-graph/session-turn-analysis/` found exactly one record containing a
`withdraw` fragment: `turn-g5VNfb.json`.  It now has one
`withdrawal_effects` row and one `negation_interpretations` row, both written
by the stated manual call.  There is therefore no earlier corpus example that
demonstrates this path working unaided since P10.

## Minimal fix

First, replace the nil error handler with one that writes a bounded
`withdrawal_processing_error` onto the turn record (time, error class and
message), leaves analysis health non-OK, and emits one diagnostic.  Change the
ERT test at line 263 to require that durable error instead of accepting silent
success.

Second, make completion eventually observable after the three polling checks:
run a bounded periodic reconciliation over records whose status is `analyzed`
and which have unprocessed withdraw fragment ids.  Processing is already
idempotent by fragment id
([session-turn-analysis.el:739](../../../emacs/session-turn-analysis.el#L739)),
so reconciliation can call the same function without duplicating effects.
Tests should cover (1) completion before the first 180-second reap, (2)
completion after all three `running` results, and (3) a thrown processing error
persisted on the record and cleared only after a successful retry.
