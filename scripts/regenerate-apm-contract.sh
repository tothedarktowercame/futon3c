#!/usr/bin/env bash
set -euo pipefail

control_root=$(cd "$(dirname "$0")/.." && pwd)
lean_root=/home/joe/code/apm-lean
contract="$control_root/holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v4.json"
plan="$control_root/holes/labs/M-apm-demonstration/apm-qualification-v1.edn"
report="$control_root/data/apm-validation/qualification-report-v1.edn"
generated=$(mktemp)
trap 'rm -f "$generated"' EXIT

cd "$lean_root"
lake build DarkTower.APMCycleMachine DarkTower.APMCycleContractEmitter \
  DarkTower.APMCampaignTraceChecker DarkTower.APMQualification
lake env lean --run DarkTower/APMCycleContractEmitter.lean > "$generated"
mv "$generated" "$contract"

cd "$control_root"
clojure -M -e "
(require '[clojure.edn :as edn]
         '[futon3c.apm.qualification :as qualification])
(let [plan-path \"$plan\"
      report-path \"$report\"
      contract-path \"$contract\"
      plan (assoc (edn/read-string (slurp plan-path))
                  :generated-contract-digest
                  (qualification/file-digest contract-path))]
  (spit plan-path (str (pr-str plan) \"\\n\"))
  (let [result (qualification/run-qualification! plan-path report-path)]
    (prn result)
    (shutdown-agents)
    (when-not (:ok result) (System/exit 1))))"
