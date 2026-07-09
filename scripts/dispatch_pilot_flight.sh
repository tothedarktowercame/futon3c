#!/usr/bin/env bash
# dispatch_pilot_flight.sh — OUTER-LOOP dispatch: send an agent through one
# War Machine *pilot* cycle (READ -> EVAL -> PRINT -> LOOP -> VERIFY).
#
# Unlike author_deposit_for.sh (the INNER loop: author a fold-turn deposit for a
# fixed mission), this dispatches the FULL loop: the agent self-ascertains the
# recommendation from the WM, does the earned work (advances the recommended
# mission's open holes), and verifies the field moved. No mission is baked in —
# the agent reads it. Consent is per-action consent-gates (Pilot-I1), not the
# fold-authoring mana ledger.
#
# Usage: dispatch_pilot_flight.sh [--from <reviewer>] [--focus <mission-id>]
#   AUTHOR_AGENT=<zai-id>  (env) — target agent; else first idle zai.
set -euo pipefail

usage() { echo "usage: $0 [--from <reviewer>] [--focus <mission-id>]" >&2; }

REVIEWER=""
FOCUS=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --from)  [[ $# -lt 2 ]] && { usage; exit 64; }; REVIEWER="$2"; shift 2 ;;
    --focus) [[ $# -lt 2 ]] && { usage; exit 64; }; FOCUS="$2"; shift 2 ;;
    *) echo "unknown argument: $1" >&2; usage; exit 64 ;;
  esac
done

LOG=${PILOT_FLIGHT_LOG:-/home/joe/code/futon2/logs/pilot-flights.log}
AGENCY=/home/joe/code/futon3c/scripts/agency_send.py

pick_agent() {
  if [[ -n "${AUTHOR_AGENT:-}" ]]; then printf '%s\n' "$AUTHOR_AGENT"; return; fi
  python3 - <<'PY'
import json, urllib.request, sys
data = json.load(urllib.request.urlopen("http://localhost:7070/api/alpha/agents", timeout=5))
agents = data.get("agents", {})
ids = sorted(aid for aid, a in agents.items()
             if aid.startswith("zai-") and a.get("invoke-ready?") and a.get("status") == "idle")
if not ids: sys.exit("dispatch_pilot_flight: no idle zai pilot agent available")
print(ids[0])
PY
}

AGENT="$(pick_agent)"
FOCUS_LINE="none — self-ascertain from the WM."
[[ -n "$FOCUS" ]] && FOCUS_LINE="operator suggests focusing on ${FOCUS}, but still READ the WM first and say if it disagrees."
echo "[$(date -Is)] pilot flight dispatch reviewer=${REVIEWER:-none} focus=${FOCUS:-none} -> ${AGENT}" >> "$LOG"

FROM_ARGS=()
[[ -n "$REVIEWER" ]] && FROM_ARGS=(--from "$REVIEWER")

python3 "$AGENCY" --to "$AGENT" --kind bell "${FROM_ARGS[@]}" <<EOF >> "$LOG" 2>&1
${AGENT}: WAR MACHINE PILOT FLIGHT — inhabit the OUTER loop for ONE cycle. READ /home/joe/code/futon3c/README-pilot.md first (the loop apparatus + hard disciplines). Focus hint: ${FOCUS_LINE}

Completion route: bell ${REVIEWER:-the dispatcher ledger} with your frame — the READ recommendation, the hole you advanced, PROOF the field moved (before/after), and honest partials. Do not dispatch follow-on work.

You are the pilot (:war-machine-pilot). Run ONE cycle, SELF-ASCERTAINING — no mission is handed to you; you read it from the machine.

STEP 1 — READ: get the WM's current recommendation.
  curl -s http://localhost:7070/api/alpha/war-machine
Record the top decision \`v\` = (:judgement :decision): its :target (a mission), :open-hole-count, :mission-path, and its rank in :ranked-actions. THIS is what the machine says to work on — you did not choose it. State it back.

STEP 2 — EVAL: accept \`v\`. Before any substrate edit, MINT A CONSENT-GATE (Pilot-I1: every substantive action cites a :consent-gate-event-id) — see README-pilot's consent-gate-emit. Record predicted-discharge = v's :G-total.

STEP 3 — PRINT (the EARNED work): advance ONE of v's open holes FOR REAL. Read the mission doc at :mission-path, pick one concrete open hole, and do the actual work to close or advance it (edit the mission doc / substrate), citing your cg-id. THE DISCHARGE MUST BE EARNED — if you claim a hole is closed, it must actually be gone (V2 no-teleport; no fake-finished). Partial progress is fine and honest; fabricated closure is a design error.

STEP 4 — LOOP: make the machine re-observe. Prefer request-tick! (async; NEVER call tick! synchronously — README-pilot). Then re-query the WM and check whether THE FIELD MOVED: did v's :open-hole-count drop, or did v rotate off :ranked-actions[0]? Compute realised-discharge vs predicted; record the prediction-error. If the field did NOT move, say so and why — that is a real, reportable outcome, not something to hide.

STEP 5 — VERIFY + record: write the cycle as a frame (repl_trace / data/repl-traces). Commit your substrate edits with specific git add paths; do not commit unrelated dirty files. Bell ${REVIEWER:-the dispatcher} with: the READ recommendation, the hole advanced + earned-closure proof, the LOOP before/after, and any honest partials.

If every open hole is genuinely blocked, report "field did not move because X" honestly rather than fabricating work. An honest no-op with a reason is the correct outcome.
EOF

echo "[$(date -Is)] pilot flight dispatch complete reviewer=${REVIEWER:-none} author=${AGENT}" >> "$LOG"
