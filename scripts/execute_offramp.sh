#!/usr/bin/env bash
# execute_offramp.sh — the Stage-2 / A3 EXECUTION witness for the offramp.
#
# travel_offramp.sh crosses outer->inner and comes back with a *predicted*
# construction (data/offramp-wirings/<base>.wiring.edn). That is a paper design.
# This step EXECUTES the same cascade through the deterministic futon3a fold
# engine (the Car-3 apply-cascade! executor, via futon2.aif.enact/engine-wiring)
# and RE-OBSERVES what the executor actually reproduced. Only when the executor
# genuinely builds boxes do we mint the INSTANTIATE formal witness:
#
#     data/offramp-wirings/<base>.executed.edn
#
# which futon3c.logic.mission-clean/instantiate-witness? reads to discharge the
# INSTANTIATE phase (V2 earned-closure: a live instance, not a paper design).
#
# ARTIFACT-ONLY (enact.clj's sense): the executor produces a construction and we
# re-observe it. NO substrate write, NO outward action — promoting a wiring to
# :constructed / FIRING substantive actions stay operator-gated (WM-I4/R16-ARM).
# This script is the operator's explicit "earn the witness" step, kept SEPARATE
# from travel_offramp so producing the design and promoting it to an executed
# instance are two decisions, not one.
#
# Usage: execute_offramp.sh <mission-id> <mission-doc-path>
# Prints: [execute-offramp] <id> EXECUTED boxes=.. realized-G=.. expected-G=.. -> <executed.edn>
#     or: [execute-offramp] <id> NO-REPRODUCTION boxes=0 -> abstain (INSTANTIATE stays a hole, honestly)
set -euo pipefail

[[ $# -lt 2 ]] && { echo "usage: $0 <mission-id> <mission-doc-path>" >&2; exit 64; }
MISSION="$1"; DOC="$2"
BASE="${MISSION##*/}"
PY=/home/joe/code/futon3a/.venv/bin/python
CASCADE_EDN="/tmp/offramp-${BASE}.edn"
WIRING="/home/joe/code/futon3c/data/offramp-wirings/${BASE}.wiring.edn"
EXECUTED="/home/joe/code/futon3c/data/offramp-wirings/${BASE}.executed.edn"
TICK="$(date +%s)"

[[ -f "$WIRING" ]] || { echo "[execute-offramp] $MISSION no predicted wiring at $WIRING -> run travel_offramp.sh first" >&2; exit 65; }

# --- cascade stage: regenerate the cascade EDN so :shown is fresh/deterministic ---
"$PY" /home/joe/code/futon3a/holes/labs/M-memes-arrows/offramp_cascade.py "$DOC" "$CASCADE_EDN" >&2

# --- execution stage: run the SAME cascade through the deterministic executor,
#     then re-observe (futon2.aif.enact/engine-wiring + fold-realized) ---
FORM="$(mktemp /tmp/execute-offramp.XXXXXX.clj)"; trap 'rm -f "$FORM"' EXIT
cat > "$FORM" <<CLJ
(do
  (require '[clojure.edn :as edn] '[clojure.pprint :as pp]
           '[futon2.aif.enact :as enact]
           '[futon2.aif.fold-realized :as fr]
           '[futon2.aif.fold-eval :as fe])
  (let [cascade   (edn/read-string (slurp "$CASCADE_EDN"))
        shown     (:shown cascade)
        predicted (edn/read-string (slurp "$WIRING"))
        ;; THE DETERMINISTIC EXECUTOR — byte-identical to the pilot's enact path.
        ;; Reusing the private var guarantees the witness runs the SAME executor
        ;; γ's realized leg runs; no reimplementation, no drift.
        enacted   (#'enact/engine-wiring shown)
        boxes     (count (:boxes enacted))
        exp-cov   (fe/coverage predicted)
        outcome   (fr/realized-outcome-of
                    {:policy "$MISSION" :fold {:delta-g (fe/coverage->delta-g exp-cov)}}
                    enacted $TICK)]
    ;; HONESTY BAR: mint the witness ONLY if the executor really reproduced the
    ;; construction. enacted nil / boxes=0 => nothing was built => abstain, and
    ;; INSTANTIATE stays a :sorry hole (the honest no-op — no laundered prose).
    (if (and enacted (pos? boxes))
      (do (spit "$EXECUTED"
                (with-out-str
                  (pp/pprint {:mission "$MISSION"
                              :enacted-wiring enacted
                              :realized-outcome outcome
                              :executor :futon3a-fold-engine/apply-cascade
                              :provenance {:cascade-shown shown
                                           :predicted-coverage exp-cov
                                           :tick $TICK
                                           :source :classical-engine
                                           :artifact-only true}})))
          (str "[execute-offramp] $MISSION EXECUTED boxes=" boxes
               " realized-G=" (:realized-G outcome)
               " expected-G=" (:expected-G outcome)
               " -> $EXECUTED (INSTANTIATE witnessed)"))
      (str "[execute-offramp] $MISSION NO-REPRODUCTION boxes=" boxes
           " -> abstain (executor built nothing; INSTANTIATE stays a hole, honestly)"))))
CLJ
bash /home/joe/code/futon3c/scripts/proof-eval.sh -f "$FORM"
