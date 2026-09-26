# RUN4 once-closed terminal projection refusal — discovery, 2026-09-12

**Verdict: SPURIOUS producer/consumer checkpoint-contract mismatch.** The
projection rejects legitimate typed unreached checkpoints. It is not a
success-only rule, v1-series restriction, or repair-successor-link check.
No fix, reload, restart, configuration change or runtime write was performed.
Only this requested note is written and committed.

Inspected canonical sources: futon3c `c3118cabfcfae369c1aa52abdc16002f26e4271e`,
futon2 `6496c0fe0c0b86ccbb8b9d16f3099605425dfffc`. The service journal identifies
PID 1942869. No worktree source or shared-JVM evaluation was used.

## 1. Exact refusing check

`src/futon3c/wm/runner_service.clj:219` calls `run4-terminal/persist!` while
publishing the click/run binding. `result-summary` calls that publication before
`close-click!` can publish its successful service close. Exceptions propagate to
`run-click!`, then `fail-click!` (lines 385–426), which retains only the exception
message, losing `ex-data`'s reason/checkpoint in the service summary.

`src/futon3c/wm/run4_terminal_projection.clj:57–66` requires every **present**
checkpoint to contain map-valued `:judgment` AND `:ground`. Otherwise line 61
calls `refuse! :malformed-checkpoint {:checkpoint phase}`. `refuse!` at lines
13–15 raises the exact message “RUN4 terminal projection refused”. Missing keys
have a separate supported `:absent` representation; typed unreached cells do not.

## 2. Why cycle 2 refused

The producer deliberately uses two checkpoint forms. In
`futon2/src/futon2/aif/full_loop_runner.clj:2316–2320`, `term` creates
`{:judgment ... :ground ...}`, while `sorry` creates `{:sorry ...}`. Its `close!`
at lines 2630–2635 fills every unreached phase with a typed sorry, writes that
same cell into the checkpoint atom and cohort (lines 2622–2628), closes the cohort,
and returns that checkpoint map (lines 2794–2806).

For cycle 2, the actual persisted file
`/home/joe/run4/F11-production-successor-20260912-v2/cohort/run4-f11-production-successor-20260912-v2/attempt-001/006-adjudication.edn`
contains:

```clojure
:payload {:sorry {:outcome :build-failed :kind :not-reached-adjudication}}
```

`007-closed.edn` records `:build-failed`, `:grounded? false`, at
`2026-09-12T17:34:24.317918603Z`. The run record is
`/home/joe/run4/F11-production-successor-20260912-v2/run-records/tick-run-record-90feda63-9932-4678-9b62-7929402eab51.edn`.
It contains the matching click/run/task-pin identities. The committed
`RUN4-F11-production-successor-2026-09-12-v2/LIVE-CYCLE-2026-09-12.edn` preserves
both the cohort terminal and service error.

Read-only replay in a **fresh JVM** (`clojure -M -`, exit 0) required only
`clojure.edn`, `clojure.java.io`, and `futon3c.wm.run4-terminal-projection`.
For each cycle it read the seven cohort EDNs, mapped nonclosed checkpoint types
to their exact `:payload`, took outcome/attempt identity from the closed record,
and called **`projection`, not `persist!`**, with the actual click ID, run ID and
run-record path. The reconstructed envelope used `:data {}` because complete
returned diagnostic data is not persisted; it neither alters checkpoint contents
nor affects this refusing check. Exact caught `ex-data`:

```clojure
;; v2, outcome :build-failed
{:error :run4-terminal-projection-refused
 :reason :malformed-checkpoint :checkpoint :adjudication}
;; v1, outcome :guardrail-refusal
{:error :run4-terminal-projection-refused
 :reason :malformed-checkpoint :checkpoint :build}
```

This is an offline reconstruction of the reason, not a claim that the live
journal retained `ex-data`. Reaching this check also confirms that reconstructed
identity/pin and run-record binding checks passed. It fails before projection
persistence checks its directory, so the absent default projection directory is
not the demonstrated cause. Series execution can bind a different directory
(`run4_series_service.clj:242–249`).

## 3. Contract verdict and smallest repair shape

The projection describes itself as recording facts, not classifying outcomes.
It only requires a keyword outcome. A separate consumer
`run4_terminal_evidence.clj:223` applies the grounded-change success requirements.
Rejecting a producer-defined not-reached checkpoint during factual projection is
therefore spurious. It affects RUN4 terminals with any returned sorry checkpoint;
this is **not** proof that every non-success outcome fails (some can reach every
phase). Nor should preserving a failed terminal make its successor successful.

Smallest coherent repair: specify and validate a distinct projected
**not-reached** checkpoint carrying the producer's typed reason/outcome; accept
only the documented producer shape, preserve missing-versus-not-reached, and
continue refusing genuinely malformed/conflicting cells. Update the strict
consumer checkpoint schema (`run4_terminal_evidence.clj:197–220`) with the same
variant, versioning/migrating the schema if required. Keep all grounded-success
checks requiring completed evidence. Do not discard sorry cells, fabricate
judgment/ground maps, or weaken identity/digest checks. Preserve structured
exception data in service failures as a separate diagnostic improvement.

Required regression controls: these two real checkpoint shapes; a fully completed
success; an actually absent checkpoint; malformed sorry/ambiguous mixed cells;
unchanged source-pin mismatch rejection; round-trip projection/consumer validation
that records honest failures without classifying them as grounded success.
No tests or repair were added in this discovery.

## 4. Cycle 1

**Yes, the same message fired:** journal at `2026-09-12T16:52:05+00:00` records
`:service-failed` / “RUN4 terminal projection refused” for
`wm-click-b118a6da-12b5-4fe5-aacd-43eee6dc2245`; its build and adjudication payloads
are typed sorry cells. Replay refuses first at build. Cycle 2 logs the same
message at `2026-09-12T17:34:25+00:00` for
`wm-click-77112280-b4a8-4127-b1a7-308f0bb1097f`.

Journal command executed (exit 0):

```sh
journalctl --since '2026-09-12 16:50:00' --until '2026-09-12 17:35:00' --no-pager -o short-iso --grep='RUN4 terminal projection'
```

The available user journal contains both events; no claim is made about system
journal entries outside this account's visibility. Static discovery additionally
used `rg`, `sed`, `nl`, and Babashka EDN reads against the canonical files.
