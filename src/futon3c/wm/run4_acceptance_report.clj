(ns futon3c.wm.run4-acceptance-report
  "Pure, read-only preregistration report.  It never grants RUN4 acceptance."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.run4-route-conformance :as route]
            [futon3c.wm.run4-battery :as battery]
            [futon3c.wm.run4-realized-recording :as realized]
            [futon3c.wm.run4-terminal-evidence :as terminal]))

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
       (let [visible-ids (mapv :trial_id (:trials visibility))
             by-trial (into {} (map (juxt #(str (get-in % [:identity :trial-id])) identity)
                                    bundles))]
         (and (= (count bundles) (count by-trial))
              (= (count visible-ids) (count (distinct visible-ids)))
              (= (set visible-ids) (set (keys by-trial)))
              (every?
               (fn [trial]
                 (let [bundle (get by-trial (:trial_id trial))
                       record (:run-record bundle)]
                   (and (= :wm/run4-terminal-evidence-bundle-v1 (:schema bundle))
                        (= (:run_id visibility) (str (get-in bundle [:identity :series-id])))
                        (nonblank? (:projection-digest bundle))
                        (nonblank? (:run-record-digest bundle))
                        (nonblank? (:run/id record))
                        (= ({"passed" :succeeded "failed" :failed "blocked" :blocked}
                            (:result trial))
                           (get-in bundle [:classification :task-result]))
                        (true? (:conforms? (route/verdict cmap record))))))
               (:trials visibility))))))

(defn- calculate-report
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
        recordings (when route? (mapv realized/from-terminal-bundle terminal-bundles))
        recording? (boolean (and recordings (= (count recordings) (count terminal-bundles))))
        battery-artifact (when recording?
                           (battery/produce expected-series-id expected-series-sha256
                                            expected-control-map-sha256 terminal-bundles cmap))
        battery? (boolean (and battery-artifact
                               (battery/validate battery-artifact expected-series-id
                                                 expected-series-sha256
                                                 expected-control-map-sha256
                                                 terminal-bundles)))]
    {:schema :wm/run4-acceptance-report-v1
     :run-id (when (map? visibility) (:run_id visibility))
     :checks {:source-current source-current?
              :terminal-task-results terminal?
              :route-conformance route?
              :recording-completeness recording?
              :route-battery-complete battery?}
     :decision (cond
                 (not source-current?) :refused-source-drift
                 (not terminal?) :unsupported-or-incomplete-terminal-state
                 (not route?) :missing-route-to-preregistration-bridge
                 (not recording?) :missing-realized-recording
                 (not battery?) :missing-acceptance-battery-record
                 :else :operator-decision-required)
     :accepted? false
     :acceptance-authority :operator-reserved
     :battery battery-artifact
     :missing-evidence
     (cond-> []
       (not route?) (conj :validated-terminal-bundle-route-conformance)
       (not recording?) (conj :realized-recording)
       (not battery?) (conj :shared-step-acceptance-battery-record))}))

(defn report-durable
  "Read every bundle through the strict durable join before calculating a
  non-authoritative acceptance report."
  [{:keys [roots trials control-map-text] :as input}]
  (when-not (and (map? roots) (vector? trials) (seq trials)
                 (string? control-map-text))
    (throw (ex-info "Invalid RUN4 acceptance authority"
                    {:reason :invalid-acceptance-authority})))
  (let [bundles
        (mapv (fn [{:keys [admission-request started]}]
                (or (terminal/read-terminal-evidence-bundle
                     roots admission-request started)
                    (throw (ex-info "Missing RUN4 terminal bundle"
                                    {:reason :missing-terminal-bundle}))))
              trials)]
    (calculate-report (assoc input :terminal-bundles bundles))))
