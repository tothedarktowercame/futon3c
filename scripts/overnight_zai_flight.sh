#!/usr/bin/env bash
# overnight_zai_flight.sh — one unattended deposit-authoring run (zai only).
#
# Consent: futon2.aif.mana-gate "fold-authoring" — awarded by the operator
# (Joe, 2026-07-05, overnight-zai directive). THE AGENT consumes 1 mana as
# its FIRST act and exits on refusal: exhausted budget stops runs
# mechanically, no human in the loop. Every spend is on the gate ledger.
#
# Fable-safety: the bell is sent WITHOUT --from, so no auto-bellback routes
# to any claude agent — zero Claude/Fable tokens overnight. zai narrations
# land in the invoke-jobs ledger + the dated log file for morning review
# (autorunner-inspectable: no silent turns).
#
# Cron: 30 0,2 * * * and 45 4,6 * * * (4 runs ≤ 4 mana).
set -uo pipefail
LOG=/home/joe/code/futon2/logs/overnight-zai-flights.log
echo "[$(date -Is)] overnight flight dispatch -> zai-3" >> "$LOG"

python3 /home/joe/code/futon3c/scripts/agency_send.py --to zai-3 --kind bell <<'EOF' >> "$LOG" 2>&1
zai-3: UNATTENDED OVERNIGHT RUN (no reviewer awake — do NOT bell claude-16 or any claude agent; write everything to files; your narration is reviewed in the morning).

STEP 1 — CONSENT, before anything else: write to /tmp/zai3-mana.clj the form
  (do (require (quote [futon2.aif.mana-gate :as mg])) (mg/consume! "fold-authoring" "overnight deposit run"))
and run: cd /home/joe/code/futon2 && clojure -M -e '(println (load-file "/tmp/zai3-mana.clj"))'
If the result is NOT {:ok true ...}: append one line "REFUSED - budget exhausted, run aborted" to /home/joe/code/futon2/holes/overnight-flights-2026-07-06.md and END YOUR TURN. The refusal is the consent system working.

STEP 2 — pick the mission: tail -3 /home/joe/code/futon2/logs/wm-scheduled.log for the latest tick; the lane missions with :abstain-missing-leg need deposits. Cross-check /home/joe/code/futon6/data/fold-turns/ — pick the top-ranked lane mission WITHOUT a deposit. If every lane mission already has one, instead pick the highest-G candidate mission from the tick that lacks a deposit. State your choice + reasoning in the log file.

STEP 3 — author the fold-turn deposit, full contract (exemplars: ft-autoclock-in-001 = the golden bar; ft-peradam-mechanization-006 = the v2 shape; READ both first):
- psi: sorry-grain from the mission via the L2 recipe (held-work items if any; else the mission doc's actual tension). No seal exists -> record no-blind-scoring honestly.
- cascade: cd /home/joe/code/futon3a && .venv/bin/python holes/labs/M-memes-arrows/cascade_serve.py "<your psi>" (budget 20 default; the F key in the JSON is F-free-energy).
- fold: honest boxes (fits-pattern + addresses-however against real pattern prose from futon3/library/<id>.flexiarg), surfaced holes (honest hole beats decorative box), v2 wires with :connective, terminal :discharges the want-signature, dG = -(boxes/(boxes+holes)) hand-shown.
- pins: prompt-sha via the real fold-prompt fn (proof-eval -f, the 005/006 practice); prose shas; :arming cites the MANA SPEND from step 1 (gate fold-authoring, your consume result), NOT the blanket grant.
- deposit ft-<mission-stem>-0NN.edn (next free number) to /home/joe/code/futon6/data/fold-turns/; prove loader acceptance AND one tampered-copy rejection (in /tmp, never the real dir).
STEP 4 — record: append the full run log (mission, psi, cascade numbers, dG, holes, pins, mana ref) to /home/joe/code/futon2/holes/overnight-flights-2026-07-06.md; commit the deposit + log (git add the specific files, commit message "overnight flight: ft-... (mana-gated)"); PAR via par_punctuate if available, else in the log file.
Budget your rounds: reading 4, psi+cascade 4, fold+deposit 6, proofs+log+commit 4. If stuck 3+ variations on one obstacle: log it honestly and end the turn — morning review handles it. Fly.
EOF
echo "[$(date -Is)] dispatch complete" >> "$LOG"
