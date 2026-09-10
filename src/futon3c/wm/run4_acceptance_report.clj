(ns futon3c.wm.run4-acceptance-report
  "Pure, read-only preregistration report.  It never grants RUN4 acceptance."
  (:require [clojure.string :as str]))

(defn- sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))
(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))

(defn report
  "Describe evidence against current RUN4 preregistration criteria. Operator
  acceptance remains a separate action even when every machine check passes."
  [{:keys [visibility expected-series-sha256 observed-series-sha256
           route-evidence recording-evidence]}]
  (let [source-current? (and (sha? expected-series-sha256)
                             (= expected-series-sha256 observed-series-sha256))
        terminal? (and (= "wm/run-visibility-v1" (:schema visibility))
                       (contains? #{"complete" "failed" "blocked"} (:stage visibility))
                       (seq (:trials visibility))
                       (every? #(contains? #{"passed" "failed" "blocked"} (:result %))
                               (:trials visibility)))
        route? (and (map? route-evidence)
                    (= :wm/run4-route-conformance-v1 (:schema route-evidence))
                    (seq (:routes route-evidence))
                    (every? seq (:routes route-evidence))
                    (empty? (:unmapped-hops route-evidence))
                    (empty? (:refutations route-evidence)))
        recording? (and (map? recording-evidence)
                         (= :wm/run4-recording-completeness-v1
                            (:schema recording-evidence))
                         (true? (:complete? recording-evidence))
                         (nonblank? (:battery-ref recording-evidence)))]
    {:schema :wm/run4-acceptance-report-v1
     :run-id (when (map? visibility) (:run_id visibility))
     :checks {:source-current source-current?
              :terminal-task-results terminal?
              :route-conformance route?
              :recording-completeness recording?}
     :decision (cond
                 (not source-current?) :refused-source-drift
                 (not terminal?) :unsupported-or-incomplete-terminal-state
                 (not route?) :missing-route-to-preregistration-bridge
                 (not recording?) :missing-acceptance-battery-record
                 :else :operator-decision-required)
     :accepted? false
     :acceptance-authority :operator-reserved}))
