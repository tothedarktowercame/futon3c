# T-stepper-pending-records — 433 turn records carry session_id "pending"

**Status:** OPEN (opened 2026-11-11 by claude-cs at Joe's request; Joe will
sort it out later)

**Date:** 2026-11-11

## What happens

A REPL that has no session id yet sets `agent-chat--session-id` to the
placeholder `"pending"` (`emacs/codex-repl.el:2665`; `agent-chat.el:3880`
does the same). A turn recorded in that window is written to
`~/.emacs-graph/session-turn-analysis/turn-*.json` with
`session_id: "pending"`. On 2026-11-11 there were 433 such records from 72
agents (codex, zai, kimi, lemma, blueprint, claude-cs and others), from
2026-09-26 to the present. Most are a seat's first turn (`<agent>-turn-1`).

Nothing rewrites them later. A record's session id can also be updated once
the real id is known: codex-6's `turn-wWZNam` (2026-11-11 18:57) carries its
real session `01a1cbbe…` now, but its stepper had already followed it
as `"pending"`. Which writer does that update, and why it misses the 433, is
not yet known.

## How it showed up

The `*象 stepper*` for codex-6 opened on session `"pending"` (26 frames in
Joe's Emacs frame cache). Frame 1 was codex-16's turn of 2026-10-29 ("I am
seeing messages to this effect…"). The path was the follow-the-active-REPL
hook, `turn-stepper--turn-recorded`, which took `"pending"` as a session.
Fixed on the stepper side in futon3c 187e8eca5: a turn recorded under
`"pending"` no longer moves a stepper. The same incident's misnaming
(`*象 stepper:claude-1*` for codex-6) was fixed in 983896667.

## What is left (this ticket)

1. **The 433 records.** Each belongs to one real session, but the record does
   not say which. They could be re-attached by `agent_id` and time (the
   seat's next session in the evidence store at :7073), or by `evidence_id`
   where there is one. This rewrites 象's records, so it waits for Joe.
2. **The writer.** Either hold the turn record until the session id arrives,
   or update it when it does. Today these turns drop out of every per-session
   reading: the stepper, `turn_frames.py <session>`, and any session timeline
   built on them.
3. **Other readers.** `turn_frames.py` and anything keyed on `session_id`
   should treat `"pending"` as no session, as the interactive `M-x
   turn-stepper` already does (`turn-stepper.el`,
   `turn-stepper--session-id-in-buffer`).

## Check

```
python3 - <<'EOF'
import json, glob
n = 0
for f in glob.glob('/home/joe/.emacs-graph/session-turn-analysis/turn-*.json'):
    if f.count('.json') > 1 or '/turn-end-' in f: continue
    try: r = json.load(open(f))
    except Exception: continue
    n += r.get('session_id') == 'pending'
print(n)
EOF
```
