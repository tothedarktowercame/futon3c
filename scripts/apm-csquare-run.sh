#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
clojure -M -e '(require (quote futon3c.apm.csquare-synthetic-campaign)) (let [s (futon3c.apm.csquare-synthetic-campaign/start!)] (println (pr-str s)) (loop [n 0] (let [r (futon3c.apm.csquare-synthetic-campaign/result)] (if (or (:ok r) (>= n 240)) (do (println (pr-str r)) (System/exit (if (:ok r) 0 2))) (do (Thread/sleep 250) (recur (inc n)))))))'
