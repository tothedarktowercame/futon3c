#!/usr/bin/env bash
# author_deposit_for.sh — mana-gated fold-turn authoring for one explicit mission.
#
# Callable actuator leg for M-actuator-spec (c): selection -> authoring.
# Unlike overnight_zai_flight.sh, the mission is fixed by the caller and the
# completion bell routes to the reviewer supplied with --from.
set -euo pipefail

usage() {
  echo "usage: $0 <mission-id> [--from <reviewer>]" >&2
}

if [[ $# -lt 1 ]]; then
  usage
  exit 64
fi

MISSION="$1"
shift
REVIEWER=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --from)
      if [[ $# -lt 2 ]]; then
        usage
        exit 64
      fi
      REVIEWER="$2"
      shift 2
      ;;
    *)
      echo "unknown argument: $1" >&2
      usage
      exit 64
      ;;
  esac
done

LOG=${AUTHOR_DEPOSIT_LOG:-/home/joe/code/futon2/logs/selection-authoring-coupling.log}
AGENCY=/home/joe/code/futon3c/scripts/agency_send.py

pick_agent() {
  if [[ -n "${AUTHOR_AGENT:-}" ]]; then
    printf '%s\n' "$AUTHOR_AGENT"
    return
  fi
  python3 - <<'PY'
import json, urllib.request, sys
data = json.load(urllib.request.urlopen("http://localhost:7070/api/alpha/agents", timeout=5))
agents = data.get("agents", {})
ids = sorted(
    aid for aid, a in agents.items()
    if aid.startswith("zai-") and a.get("invoke-ready?") and a.get("status") == "idle"
)
if not ids:
    sys.exit("author_deposit_for: no idle zai authoring agent available")
print(ids[0])
PY
}

AUTHOR="$(pick_agent)"
echo "[$(date -Is)] author deposit dispatch mission=${MISSION} reviewer=${REVIEWER:-none} -> ${AUTHOR}" >> "$LOG"

FROM_ARGS=()
if [[ -n "$REVIEWER" ]]; then
  FROM_ARGS=(--from "$REVIEWER")
fi

python3 "$AGENCY" --to "$AUTHOR" --kind bell "${FROM_ARGS[@]}" --mission "$MISSION" <<EOF >> "$LOG" 2>&1
${AUTHOR}: REVIEWED AUTHORING FLIGHT — author exactly one fold-turn deposit for ${MISSION}.

Completion route: bell ${REVIEWER:-the dispatcher ledger} with the deposit path, commit SHA(s), load-deposits proof, tamper-copy rejection proof, and 2f gate output. Do not dispatch follow-on work.

STEP 1 — CONSENT, before anything else:
Write to /tmp/${AUTHOR}-mana.clj the form
  (do (require (quote [futon2.aif.mana-gate :as mg])) (mg/consume! "fold-authoring" "selection-authoring deposit for ${MISSION}"))
and run:
  cd /home/joe/code/futon2 && clojure -M -e '(println (load-file "/tmp/${AUTHOR}-mana.clj"))'
If the result is NOT {:ok true ...}: append one line "REFUSED - budget exhausted, run aborted for ${MISSION}" to /home/joe/code/futon2/holes/selection-authoring-flights-2026-07-07.md and END YOUR TURN. The refusal is the consent system working.

STEP 2 — mission is fixed; the question is "is the WORK done," NOT "does a deposit exist":
Author for ${MISSION}. Do not pick from wm-scheduled.log. Cross-check /home/joe/code/futon6/data/fold-turns/ before writing. If a deposit for ${MISSION} already exists, ASSESS it rather than stopping blindly:
- Is the work DONE? — its fold's terminal :discharges the want-signature AND no open :policy-holes remain AND :eval has a realized leg. If DONE: STOP and report skipped-work-done.
- Is the deposit PARTIAL? — open :policy-holes remain, or the terminal does not discharge, or :eval has no realized leg. Then do NOT author a duplicate; report partial-needs-advancement, naming exactly which holes/legs are still open, so ${REVIEWER:-the dispatcher} can route it to a PILOT flight (advance the holes) instead of re-authoring.
Only author a fresh deposit if NONE exists at all.

STEP 3 — author the fold-turn deposit, full contract (exemplars: ft-autoclock-in-001 = golden v1/v2 bar; ft-peradam-mechanization-006 = v2 shape; READ both first):
- psi: sorry-grain from the mission via the L2 recipe (held-work items if any; else the mission doc's actual tension). No seal exists -> record no-blind-scoring honestly.
- cascade: cd /home/joe/code/futon3a && .venv/bin/python holes/labs/M-memes-arrows/cascade_serve.py "<your psi>" (budget 20 default; the F key in the JSON is F-free-energy).
- fold: honest boxes (fits-pattern + addresses-however against real pattern prose from futon3/library/<id>.flexiarg), surfaced holes (honest hole beats decorative box), v2 wires with :connective, terminal :discharges the want-signature, dG = -(boxes/(boxes+holes)) hand-shown.
- CLean: embed a top-level :clean block in the deposit. READ the exemplars first:
  /home/joe/code/futon6/holes/clean/autoclock-in.clean.edn
  /home/joe/code/futon6/holes/clean/M-learning-loop.clean.edn
  Box-type every box with :id :method :text :produces and, for non-source boxes, :consumes.
  Write :clean/wires as {:from :to :carries}; :carries MUST equal the upstream box's
  :produces and appear in the downstream box's :consumes. Typed holes use
  :satiety in #{:parse :payoff :canon :bundling :role} and :discharge in
  #{:sorryProof :queryAnswer :ungroundedBinder}. Include :clean/seq as the box
  methods in order, :clean/copar, and :clean/shape {:macro :holes-at :discharges-at :note}.
  The fold is NOT done until this passes:
    cd /home/joe/code/futon6 && bb scripts/clean_argcheck.bb /tmp/<your-clean>.clean.edn
  The exact checked CLean must be embedded under :clean in the final deposit, not left
  as a separate artifact.
- pins: prompt-sha via the real fold-prompt fn (proof-eval -f, the 005/006 practice); prose shas; :arming cites the MANA SPEND from step 1 (gate fold-authoring, your consume result), NOT any blanket grant.
- deposit ft-<mission-stem>-0NN.edn (next free number) to /home/joe/code/futon6/data/fold-turns/; prove loader acceptance AND one tampered-copy rejection (in /tmp, never the real dir).

STEP 4 — record:
Append the full run log (mission, psi, cascade numbers, dG, holes, pins, mana ref) to /home/joe/code/futon2/holes/selection-authoring-flights-2026-07-07.md; commit the deposit + log with specific git add paths; do not commit unrelated dirty files. If stuck 3+ variations on one obstacle: log it honestly and bell ${REVIEWER:-codex-1} with the blocker rather than thrashing.
EOF

echo "[$(date -Is)] dispatch complete mission=${MISSION} reviewer=${REVIEWER:-none} author=${AUTHOR}" >> "$LOG"
