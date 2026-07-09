#!/usr/bin/env bash
# emit_mission_clean.sh — model a mission as a DarkTower-spec CLean (the
# OUTER-loop tracker) in the live futon3c JVM via Drawbridge. This is the
# "upon entry" step of the War Machine pilot cycle: given the mission the WM
# recommends, emit its 8-phase lifecycle as a typed-hole comb whose OPEN holes
# are the mission's live frontier (unwritten phases, hungry to be written;
# `document` graded :payoff = the goal). Re-run after the earned work to see
# whether a phase-hole discharged (Obligation -> Empty).
#
# Usage: emit_mission_clean.sh <mission-id> [out-path]
#   <mission-id>  the WM decision's :target (e.g. M-learning-loop, or a
#                 <repo>/mission/<id> path — the file is named by its last segment).
#   [out-path]    default: data/mission-clean/<id>.clean.edn
#
# Prints a one-line HOLES summary for before/after diffing in STEP 4:
#   [mission-clean] <id> path=<out> holes=N holes-at=#{...} open-questions=K
#
# The emitted .clean.edn passes clean_argcheck (G1-G8) and renders 0-sorry via
# futon6/scripts/clean_to_lean.py. See futon3c.logic.mission-clean +
# holes/excursions/E-scope-organism-copar.md (the deferred domain copar).
set -euo pipefail

[[ $# -lt 1 ]] && { echo "usage: $0 <mission-id> [out-path]" >&2; exit 64; }
MISSION="$1"
BASE="${MISSION##*/}"                                   # last path segment
OUT="${2:-/home/joe/code/futon3c/data/mission-clean/${BASE}.clean.edn}"

cd /home/joe/code/futon3c

# Always load the on-disk emitter first (a live ns may predate an edit — the
# reingest scripts learned this the hard way). Then emit + print a HOLES line.
FORM="$(mktemp /tmp/emit-mission-clean.XXXXXX.clj)"
trap 'rm -f "$FORM"' EXIT
cat > "$FORM" <<CLJ
(do
  (load-file "src/futon3c/logic/mission_clean.clj")
  (require '[futon3c.logic.mission-clean :as mc])
  ;; RETURN the HOLES summary as the value (not println — that goes to the JVM
  ;; console, not back over Drawbridge). proof-eval prints {:ok true :value "…"}.
  (let [{:keys [clean path]} (mc/emit-mission-clean! "$MISSION" "$OUT")
        shape (:clean/shape clean)
        holes (:holes-at shape)]
    (str "[mission-clean] $MISSION"
         " path=" path
         " holes=" (count holes)
         " holes-at=" (pr-str holes)
         " open-questions=" (:open-questions shape))))
CLJ

bash scripts/proof-eval.sh -f "$FORM"
