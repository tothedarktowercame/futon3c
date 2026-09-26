#!/usr/bin/env bash
# travel_offramp.sh — the OUTER->INNER offramp, as one command for the pilot.
# Mint the have->want magnet from a mission's IDENTIFY, pick the argmax-F cascade
# (F>0 gate), and FOLD its semilattice (descent=BV.seq, co_app=BV.copar) into a
# non-degenerate wiring via the LIVE futon2 semilattice-fold. This is what STEP 3
# does for a buildable phase instead of authoring prose: it crosses into the
# inner loop and comes back with a construction.
#
# Usage: travel_offramp.sh <mission-id> <mission-doc-path>
# Prints:  [offramp] <id> F=.. size=.. boxes=.. seq=.. copar=.. wholeness=.. dG=.. -> <wiring.edn>
#   or:    [offramp] <id> WEAK-MAGNET F=.. -> abstain (do not fold a Bayesian-rejected cascade)
set -euo pipefail

[[ $# -lt 2 ]] && { echo "usage: $0 <mission-id> <mission-doc-path>" >&2; exit 64; }
MISSION="$1"; DOC="$2"
BASE="${MISSION##*/}"
PY=/home/joe/code/futon3a/.venv/bin/python
CASCADE_EDN="/tmp/offramp-${BASE}.edn"
WIRING="/home/joe/code/futon3c/data/offramp-wirings/${BASE}.wiring.edn"

# --- cascade stage: magnet -> argmax-F cascade + semilattice (EDN out) ---
SUMMARY="$("$PY" /home/joe/code/futon3a/holes/labs/M-memes-arrows/offramp_cascade.py "$DOC" "$CASCADE_EDN")"
echo "$SUMMARY" >&2
F="$(printf '%s' "$SUMMARY" | grep -oE 'F=[-0-9.]+' | head -1 | cut -d= -f2)"
# F>0 magnet-quality gate — do not fold a Bayesian-rejected cascade
if ! awk "BEGIN{exit !(${F:-0} > 0)}"; then
  echo "[offramp] $MISSION WEAK-MAGNET F=$F -> abstain (magnet-quality gate; sharpen IDENTIFY or work the informal legs)"
  exit 0
fi

# --- fold stage: live futon2 semilattice-fold over the cascade's semilattice ---
mkdir -p "$(dirname "$WIRING")"
FORM="$(mktemp /tmp/travel-offramp.XXXXXX.clj)"; trap 'rm -f "$FORM"' EXIT
cat > "$FORM" <<CLJ
(do
  (require '[clojure.edn :as edn] '[clojure.pprint :as pp])
  (require '[futon2.aif.fold-semilattice :as fs] '[futon2.aif.fold :as fold])
  (let [c (edn/read-string (slurp "$CASCADE_EDN"))
        cascade (:shown c)
        circ {:want-signature "MissionState -> {Wiring, PolicyHoles}"
              :semilattice (:semilattice c)}
        out (fs/semilattice-fold cascade circ)
        w (:wiring out)]
    (spit "$WIRING" (with-out-str (pp/pprint w)))
    (str "[offramp] $MISSION"
         " F=" (:F c) " size=" (:size c) " wholeness=" (:wholeness c)
         " boxes=" (count (:boxes w))
         " seq=" (count (filter #(= :wire/seq (:type %)) (:wires w)))
         " copar=" (count (filter #(= :wire/copar (:type %)) (:wires w)))
         " dG=" (:delta-g out)
         " valid=" (fold/valid-fold-output? out)
         " -> $WIRING")))
CLJ
bash /home/joe/code/futon3c/scripts/proof-eval.sh -f "$FORM"
