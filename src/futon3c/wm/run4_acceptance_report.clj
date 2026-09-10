(ns futon3c.wm.run4-acceptance-report
  "Pure, read-only preregistration report.  It never grants RUN4 acceptance."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.run4-route-conformance :as route]))

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

(defn- control-map [text expected-sha]
  (when (and (string? text) (sha? expected-sha)
             (= expected-sha (digest/sha256 text)))
    (try
      (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
        (let [v (edn/read {:eof ::empty} r)]
          (when (and (map? v) (= ::end (edn/read {:eof ::end} r))) v)))
      (catch Throwable _ nil))))

(defn- joined-route? [visibility bundles cmap]
  (and cmap (vector? bundles)
       (= (count bundles) (count (:trials visibility)))
       (let [by-trial (into {} (map (juxt #(get-in % [:identity :trial-id]) identity)
                                     bundles))]
         (and (= (count bundles) (count by-trial))
              (every?
               (fn [trial]
                 (let [bundle (get by-trial (keyword (:trial_id trial)))
                       record (:run-record bundle)]
                   (and (= :wm/run4-terminal-evidence-bundle-v1 (:schema bundle))
                        (= (:run_id visibility) (str (get-in bundle [:identity :series-id])))
                        (nonblank? (:projection-digest bundle))
                        (nonblank? (:run-record-digest bundle))
                        (nonblank? (:run/id record))
                        (true? (:conforms? (route/verdict cmap record))))))
               (:trials visibility))))))

(defn report
  "Describe evidence against current RUN4 preregistration criteria. Operator
  acceptance remains a separate action even when every machine check passes."
  [{:keys [visibility expected-series-id expected-series-sha256 observed-series-sha256
           terminal-bundles control-map-text expected-control-map-sha256]}]
  (let [source-current? (and (sha? expected-series-sha256)
                             (= expected-series-sha256 observed-series-sha256))
        terminal? (and (nonblank? expected-series-id)
                       (= expected-series-id (:run_id visibility))
                       (terminal-visibility? visibility))
        cmap (control-map control-map-text expected-control-map-sha256)
        route? (boolean (and terminal? (joined-route? visibility terminal-bundles cmap)))
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
     (cond-> [:shared-step-acceptance-battery-record]
       (not route?) (conj :validated-terminal-bundle-route-conformance))}))
