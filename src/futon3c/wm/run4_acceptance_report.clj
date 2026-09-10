(ns futon3c.wm.run4-acceptance-report
  "Pure, read-only preregistration report.  It never grants RUN4 acceptance."
  (:require [clojure.string :as str]))

(defn- sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))
(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))

(defn- terminal-visibility? [visibility]
  (and (map? visibility)
       (every? #(contains? visibility %) [:schema :run_id :stage :result :trials])
       (= "wm/run-visibility-v1" (:schema visibility))
       (nonblank? (:run_id visibility))
       (contains? #{"complete" "failed" "blocked"} (:stage visibility))
       (contains? #{"passed" "failed" "blocked"} (:result visibility))
       (vector? (:trials visibility)) (seq (:trials visibility))
       (every? (fn [trial]
                 (and (map? trial) (nonblank? (:trial_id trial))
                      (contains? #{"complete" "failed" "blocked"} (:stage trial))
                      (contains? #{"passed" "failed" "blocked"} (:result trial))))
               (:trials visibility))))

(defn report
  "Describe evidence against current RUN4 preregistration criteria. Operator
  acceptance remains a separate action even when every machine check passes."
  [{:keys [visibility expected-series-sha256 observed-series-sha256]}]
  (let [source-current? (and (sha? expected-series-sha256)
                             (= expected-series-sha256 observed-series-sha256))
        terminal? (terminal-visibility? visibility)
        ;; The current visibility schema binds series/trial IDs but carries no
        ;; full-loop run ID per trial.  Therefore no run-record route can be
        ;; joined to a visible trial, and the existing U49 transcriber/battery
        ;; cannot be invoked as evidence for this series.  Fail closed until
        ;; that producer join exists; caller-supplied route/battery assertions
        ;; are deliberately not accepted by this report.
        route? false
        recording? false]
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
     :acceptance-authority :operator-reserved
     :missing-evidence
     [:trial-to-full-loop-run-id
      :u49-route-transcription-for-exact-run
      :shared-step-acceptance-battery-record]}))
