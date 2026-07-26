# Ground Control

*The role, the instruments, and how to summon a mechanic. Written 2026-07-18,
the day the loop first ran end-to-end with a non-human ground control.*

## The job

Ground control is the agent (human or Claude) who **verifies that the machine
actually works, rather than trusting its labels** — and routes repairs when it
doesn't. The War Machine self-heals (repair obligations, stop-lines, the
CI-bounce cure loop) and its workers author and review changes; ground control
sits outside that loop and owns three things:

1. **Verification.** A green label is a claim, not a fact. Re-run the gates,
   read the diffs, exercise the feature, check the loaded value in the JVM
   rather than the source file. A failure label is also a claim: diagnose the
   *mechanism* before acting on the label (attempt-028 said
   `:substrate-unavailable`; the substrate was healthy — the preflight had
   convoyed behind transient load).
2. **Review as a real gate.** Work is belled out; the review is the job.
   Read the diff, re-run every claimed PASS (zai PASSes especially), state
   what you checked. Fix findings yourself (you hold the context); re-bell
   only substantial new work.
3. **The record.** Every diagnosis, repro, and verdict goes into the Morning
   Brief notebook (`POST :7070/api/alpha/morning-brief/addendum`) so the
   attempt records tell their own story. If you learned it and didn't record
   it, ground control didn't happen.

The full-loop runner also names a ground-control *reviewer role*: repairs of
machine failures are reviewed by `--repair-reviewer` (default `codex-1`;
override to any idle, distinct agent — the default is remote and often not
invoke-ready).

## The instruments

- **Roster:** `GET :7070/api/alpha/agents` — check before every dispatch;
  only registered, invoke-ready agents can receive bells.
- **Bells + parks:** `futon3c/scripts/agency_send.py --from <you> --to <agent>
  --kind bell --mode work --park --park-deadline 2700` — a bell without a park
  is a protocol violation. Pass `--mode work` on every coding dispatch (brief
  mode skips execution-evidence enforcement). After any bell that matters,
  check the job state (~5s) rather than trusting `accepted`.
- **Job ledger:** `GET :7070/api/alpha/invoke/jobs/<job-id>` — states, the
  durable `result-summary` prefix (~200 chars), `execution` evidence
  (`executed`, `tool-events`), `artifact-ref`.
- **Drawbridge:** `POST http://127.0.0.1:6768/eval` with
  `x-admin-token: $(cat futon3c/.admintoken)`, body = Clojure — plain eval in
  the serving JVM. Use for namespace reloads (never restart :7070) and for
  in-JVM verification. Reload the serving namespace from here, never through
  a request it serves.
- **Field Desk / notebook:** `M-x arxana-field-desk` (operator);
  `GET :7070/api/alpha/morning-brief/pending`,
  `POST .../review`, `POST .../addendum` (agents). Reads work off local disk;
  writes go only through the serving JVM.
- **Clicks (M-omni-wm-runner, 2026-07-26):** clicks run IN-PROCESS in the
  serving JVM — `POST :7070/api/alpha/wm/click` with
  `{"author":..., "reviewer":..., "repair-reviewer":...}`; poll
  `GET :7070/api/alpha/wm/click` for `{running? phase attempt-id last-result}`.
  Single-flight: a second POST while one runs returns 409 with the active
  click id. Status is registry-direct (the `*agents*` war-machine row is
  synchronous — no freshness gap). **Do NOT launch `clojure -M:wm-full-loop`
  as a process: per I-0 there is no third JVM, ever** (the old click-JVM
  pattern caused classpath vintage skew, per-click evidence-env divergence,
  and the external-invoke visibility seam — see the mission doc
  `holes/missions/M-omni-wm-runner.md`). Selection runs through the
  in-process bounded-autonomy-validated seam; phase events land in the
  runner's durable phase log as before.

## When the machine is broken

1. **Diagnose read-only first.** Probe the actual route, `ss -tlnp`,
   `systemctl --user status`, journal tail, `free`. Match against the known
   playbooks (e.g. futon1b :7073 OOM-zombie signature vs. plain load convoy)
   before any state-changing action.
2. **Small fixes with full context: do them yourself** and note them in the
   review report / notebook (carve-out b).
3. **Substantial or unfamiliar repairs: summon a mechanic** (below), with an
   inline spec — goal, files, exact errors observed, acceptance bar, gates,
   "bell <you> back with commit shas + gate results".
4. **Record the diagnosis as an addendum** on the affected attempt either way.

## Summoning a zai mechanic

Anyone with a shell can do this — it does not require the operator:

```bash
cd <workdir-for-the-agent>            # becomes the agent's cwd
~/code/futon0/scripts/cz new          # fresh zai-N lane, registered with Agency
# or: cz new M-foo                    # clocked into a mission
# or: cz headless                     # use the ops/headless Emacs daemon —
#                                     # preferred for ground control: it leaves
#                                     # the operator's interactive Emacs alone
```

`cz` allocates the next free `zai-N`, registers it (`/api/alpha/agents/restore`)
and opens its `*zai-repl:zai-N*` buffer in the chosen Emacs. Then:

```bash
# 1. Operator visibility (part of the contract, not a courtesy):
emacsclient -s <socket> -e \
  '(with-current-buffer "*zai-repl:zai-N*" (agent-follow-mode 1))'
# follow-mode renders the server-side job events into the buffer so a human
# can watch the mechanic work; without it, bell-seeded turns are invisible.

# 2. Verify it is on the roster and invoke-ready, then bell the work to it
#    (inline spec, --mode work, --park).
```

Zai specifics: ~30-minute Agency job cap (park deadline ≥ 45 min catches
silent kills); inline the whole spec (don't make it fetch); its job result is
its reply channel — tell it explicitly not to bell pipeline personas
(`wm-full-loop` is not a registered recipient) or reroute reports to held
agents; **re-run its claimed PASSes at review**.

For a **codex** mechanic there is no clean spawn API yet:
`futon3c/scripts/codex-picker --repl|--new` is a terminal op — run it
yourself or ask the operator. `invoke` only dispatches to already-registered
agents.

## The bar

Ground control ends its turn when the finding is verified, recorded, and the
next actor (machine, mechanic, or operator) has what it needs to act without
reconstructing context. "I'll get back to you" is not a state this role has.
