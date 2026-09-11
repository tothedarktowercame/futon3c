(ns futon3c.wm.run4-historical-successor
  "Server-owned bridge from strict RUN4 terminal evidence to historical repair
  resolution. No request map or terminal bundle is an admission API."
  (:require [clojure.string :as str]
            [futon2.aif.repair-obligation :as repair]
            [futon3c.wm.run4-terminal-evidence :as terminal]))

(deftype TerminalSuccessorAuthority [record]
  repair/HistoricalSuccessorAuthority
  (historical-successor-record [_] record))

(defn resolve-from-durable!
  [{:keys [repair-root evidence-roots admission-request started repair-id
           verification-id verification-attempt] :as server-config}]
  (when-not (= #{:repair-root :evidence-roots :admission-request :started
                 :repair-id :verification-id :verification-attempt}
               (set (keys server-config)))
    (throw (ex-info "Historical successor server configuration invalid" {})))
  (let [bundle (terminal/read-terminal-evidence-bundle
                evidence-roots admission-request started)
        class (:classification bundle)
        projection (:terminal-projection bundle)
        cohort (:execution-cohort (:run-record bundle))
        local-attempt (get-in projection [:attempt/id])
        cohort-id (:cohort-id cohort)
        cohort-sha (:sha256 cohort)
        successor-attempt (when (and (keyword? cohort-id) (string? local-attempt))
                            {:kind :runner-execution
                             :id (str (name cohort-id) "--" local-attempt)})]
    (when-not (and bundle (= {:task-result :succeeded :infrastructure :safe
                              :evidence-id (:projection-digest bundle)} class)
                   (keyword? cohort-id)
                   (string? cohort-sha) (re-matches #"[0-9a-f]{64}" cohort-sha)
                   (string? local-attempt) (not (str/blank? local-attempt))
                   (not= successor-attempt verification-attempt)
                   (= :grounded-change (:outcome projection)))
      (throw (ex-info "Historical production successor is absent or unqualified" {})))
    (repair/commit-historical-resolution!
     repair-root repair-id
     (TerminalSuccessorAuthority.
      {:schema :wm/historical-repair-resolution-v1
       :repair/id repair-id :repair/status :resolved
       :verification-id verification-id
       :verification-attempt verification-attempt
       :validation-attempt successor-attempt
       :validation-execution {:cohort-id cohort-id
                              :cohort-sha256 cohort-sha
                              :attempt-id local-attempt}
       :controller-attempt (:attempt-id bundle)
       :click-id (get-in bundle [:started :click-id])
       :run-id (get-in bundle [:terminal-projection :run/id])
       :task-result :succeeded :infrastructure :safe
       :projection-digest (:projection-digest bundle)
       :run-record-digest (:run-record-digest bundle)}))))
