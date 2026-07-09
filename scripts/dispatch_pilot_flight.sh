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
${AGENT}: WAR MACHINE PILOT FLIGHT — inhabit the OUTER loop for ONE cycle. READ /home/joe/code/futon3c/README-pilot.md first (the loop apparatus + hard disciplines), plus README-missions.md (the Mission Doc section format — how a PRINT-turn edit actually discharges a phase hole) and README-drawbridge.md (how to eval in the live JVM). Focus hint: ${FOCUS_LINE}

Completion route: bell ${REVIEWER:-the dispatcher ledger} with your frame — the READ recommendation, the hole you advanced, PROOF the field moved (before/after), and honest partials. Do not dispatch follow-on work.

You are the pilot (:war-machine-pilot). Run ONE cycle, SELF-ASCERTAINING — no mission is handed to you; you read it from the machine.

STEP 1 — READ: get the WM's current recommendation.
  curl -s http://localhost:7070/api/alpha/war-machine
Record the top decision \`v\` = (:judgement :decision). Its recommended work lives under v's :action: (:action :target) is the mission, plus (:action :open-hole-count) and (:action :mission-path); note v's rank in (:judgement :ranked-actions). (If the endpoint returns 503 "snapshot not ready", the JVM is warming its first tick — wait for next-tick-at, do NOT restart the JVM.) THIS is what the machine says to work on — you did not choose it. State it back.

STEP 1b — MODEL (upon entry): turn v's mission into its DarkTower-spec CLean — the OUTER-loop tracker. A mission is modelled as its 8-phase lifecycle (head→identify→map→derive→argue→verify→instantiate→document), a BV.seq spine of typed holes: every UNWRITTEN phase is a hole hungry to be written, \`document\` is graded :payoff (the goal), and the spine forbids discharging out of order.
  bash /home/joe/code/futon3c/scripts/emit_mission_clean.sh <v's (:action :target)>
RECORD the printed \`holes-at\` set and \`holes=N\` — this is the mission's live frontier BEFORE your work. Those open holes ARE what "advance a hole" means in the phase sense. (The .clean.edn passes clean_argcheck and renders 0-sorry; you do not need to run those gates — just record the HOLES line.)

STEP 2 — EVAL: accept \`v\`. Before any substrate edit, MINT A CONSENT-GATE (Pilot-I1: every substantive action cites a :consent-gate-event-id) — see README-pilot's consent-gate-emit. Record predicted-discharge = v's :G-total.

STEP 3 — PRINT (the EARNED work): advance ONE of v's open holes FOR REAL. Read the mission doc at :mission-path, pick one concrete open hole, and do the actual work to close or advance it (edit the mission doc / substrate), citing your cg-id. To DISCHARGE a phase hole you must put the work under a section header whose first word is the phase name (\`## 3. DERIVE\`, not buried under \`## Checkpoints\`) — see README-missions.md; content under a generic header will NOT move the hole. THE DISCHARGE MUST BE EARNED — if you claim a hole is closed, it must actually be gone (V2 no-teleport; no fake-finished). Partial progress is fine and honest; fabricated closure is a design error.

STEP 4 — LOOP: make the machine re-observe. Prefer request-tick! (async; NEVER call tick! synchronously — README-pilot). Then check whether THE FIELD MOVED, against the OUTER-loop tracker you emitted in STEP 1b — this is the primary signal. Pass --refresh with v's mission-path so the emitter SYNCHRONOUSLY reingests your just-made edit + busts the 30s cache — the discharge is observable NOW, do NOT sleep/poll:
  bash /home/joe/code/futon3c/scripts/emit_mission_clean.sh <v's (:action :target)> --refresh <v's (:action :mission-path)>
Re-emit the mission CLean and DIFF its \`holes-at\` against STEP 1b's. A real advance is a typed DISCHARGE — a phase flips Obligation → Empty (it drops out of \`holes-at\`, so \`holes=N\` decreases), or the \`document\` :payoff hole gets closer. A phase that merely gained prose without satisfying its \`:produces\` does NOT discharge — that is honest partial progress, not a moved field. Corroborate with the (:judgement :ranked-actions) entry for v's mission — its :action :structural-hole-count should track the holes-at drop (it ALSO counts held Open-questions, so it need not exactly equal holes=N) — and whether v rotated off :ranked-actions[0]; :open-hole-count is a lexical scanner and may NOT move on a phase advance, so do not judge by it alone. Compute realised-discharge vs predicted; record the prediction-error. If NO hole discharged, say so and why — an honest no-op is a real, reportable outcome, not something to hide.

STEP 5 — VERIFY + record: write the cycle as a frame (repl_trace / data/repl-traces). Commit your substrate edits with specific git add paths; do not commit unrelated dirty files. Bell ${REVIEWER:-the dispatcher} with: the READ recommendation, the hole advanced + earned-closure proof, the LOOP before/after, and any honest partials.

If every open hole is genuinely blocked, report "field did not move because X" honestly rather than fabricating work. An honest no-op with a reason is the correct outcome.
EOF

echo "[$(date -Is)] pilot flight dispatch complete reviewer=${REVIEWER:-none} author=${AGENT}" >> "$LOG"
