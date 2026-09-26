# E-zaif-life-findings — manual resume steps in the zai stepper

**Status: IDENTIFY (2026-07-22). Owner: Joe + agents. Emerged from the
M-apm-capability-ratchet MAP evidence-capture test (2026-07-22).**

## The gap

The conductor v3 stepper (`futon3c/dev/futon3c/dev/apm_conductor_v3.clj`)
dispatches APM problems to zai agents via `reg/invoke-agent!`, but when a
turn completes, the stepper **goes idle and waits for a human to decide
what happens next**. Two functions are the manual recovery surface:

### Line 584 — `make-on-idle` does not re-dispatch

```clojure
(defn- make-on-idle [agent-id]
  (fn [idle-agent-id outcome]
    (when-let [stepper (get @!steppers agent-id)]
      (when (and (= idle-agent-id agent-id) (:waiting? stepper))
        ;; ... records output, sets :waiting? false, prints message ...
        ;; AND STOPS. No automatic re-dispatch.
        ))))
```

When the agent finishes a turn (all tool rounds complete, final answer
returned), `make-on-idle` fires via the Agency idle callback. It records
the output in the stepper atom, logs it, appends to the mirror buffer, and
prints a message. Then it returns. The stepper is now `:waiting? false`
and idle. Nothing happens next unless a human intervenes.

### Line 636 — `continue!` is the manual re-dispatch

```clojure
(defn continue!
  "Accept and continue."
  ([agent-id] (continue! agent-id "Continue. Close any remaining sorry. Return everything inline."))
  ([agent-id prompt]
   ;; ... checks stepper exists and is not waiting ...
   (dispatch! agent-id prompt)))
```

To make the agent work further on the same problem, a human must call
`(continue! "zai-1")` from the REPL. This sends a generic "continue,
close remaining sorry" prompt and re-dispatches. The companion `backup!`
(line 649) is the correction variant: `(backup! "zai-1" "that approach is
wrong because...")`.

## Why this matters for the capability ratchet

The APM capability ratchet (`M-apm-capability-ratchet`) needs proof
episodes captured as evidence. The evidence substrate (turn-round
persistence, marks recognition) works — confirmed in the 2026-07-22 test
where `zai-1` produced 55 `:turn-round` entries for `a94J06`. But a single
invoke turn may not complete a Lean proof: the tool-round budget (24
rounds, extendable to ~216 via auto-continue) can be exhausted mid-proof,
or the model may stop calling tools and return a partial result.

When that happens, the conductor goes idle. The proof sits incomplete.
The evidence captures what the agent did so far, but the episode is
unfinished — no `status.json` update, no commit, no completion bell. The
agent needs to be manually kicked to continue.

For autonomous operation (cron dispatch, overnight runs, evidence
capture at scale), this manual step is a blocker. The agent should be
able to:

1. **Detect that the proof is incomplete** (sorry count > 0, or the
   target file doesn't validate, or the agent's own final text says
   "partial").
2. **Re-dispatch itself** with a targeted continuation prompt — not the
   generic "continue, close remaining sorry" but one that references the
   specific blocker.
3. **Know when to stop** — after N continuation attempts, or when the
   agent reports a genuine Mathlib boundary, or when the same sorry
   persists across attempts.

## Related: the bell path has the same shape

The `POST /api/alpha/bell` dispatch path (used by
`apm_formal_zai_cron.py` and by the 2026-07-22 test) creates a single
invoke-job that runs to completion and fires a completion bell. There is
no auto-continuation built into the bell path either. The cron script
handles this by running every 15 minutes and dispatching one new problem
per invocation — it never continues an in-progress one. Problems that
need multiple turns are left partial until a human intervenes or the cron
re-dispatches to a fresh agent (which loses the prior session context).

## What this is NOT

This is not the session-contamination issue (where a resumed session
bleeds prior task context into a new dispatch). That is a separate
concern about session isolation. This excursion is specifically about
the **missing auto-continuation loop**: the conductor knows the agent is
idle, knows the proof is incomplete, but does not act on that knowledge.

## Cross-refs

- `M-zaif-harness` — the zaif harness upgrade track (Joe + claude-2)
- `M-apm-capability-ratchet` §2.5 — MAP findings on the evidence substrate
- `apm_conductor_v3.clj` lines 584, 636, 649 — the manual recovery surface
- `zai_api.clj` `run-tool-rounds!` — the intra-turn auto-continue mechanism
  (24 rounds × up to 8 continues), which handles round-budget exhaustion
  within a single turn but not turn-level re-dispatch
