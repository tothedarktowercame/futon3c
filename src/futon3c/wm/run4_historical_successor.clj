(ns futon3c.wm.run4-historical-successor
  "Server-owned bridge from strict RUN4 terminal evidence to historical repair
  resolution. No request map or terminal bundle is an admission API."
  (:require [clojure.string :as str]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon3c.wm.run4-terminal-evidence :as terminal]))

(deftype TerminalSuccessorAuthority [record]
  repair/HistoricalSuccessorAuthority
  (historical-successor-record [_] record))

(defn resolve-from-durable!
  [{:keys [repair-root evidence-roots admission-request started repair-id
           verification-id verification-attempt verification-cohort
           successor-cohort] :as server-config}]
  (when-not (= #{:repair-root :evidence-roots :admission-request :started
                 :repair-id :verification-id :verification-attempt
                 :verification-cohort :successor-cohort}
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
        historical-execution (cohort/closed-execution
                              verification-cohort (:id verification-attempt))
        successor-execution (cohort/closed-execution successor-cohort local-attempt)
        successor-attempt (select-keys successor-execution [:kind :id])]
    (when-not (and bundle (= {:task-result :succeeded :infrastructure :safe
                              :evidence-id (:projection-digest bundle)} class)
                   (keyword? cohort-id)
                   (string? cohort-sha) (re-matches #"[0-9a-f]{64}" cohort-sha)
                   (string? local-attempt) (not (str/blank? local-attempt))
                   (= cohort-id (:cohort-id successor-execution))
                   (= cohort-sha (:cohort-sha256 successor-execution))
                   (= (:id verification-attempt) (:attempt-id historical-execution))
                   (not= (select-keys successor-execution [:cohort-id :attempt-id])
                         (select-keys historical-execution [:cohort-id :attempt-id]))
                   (= :grounded-change (:outcome projection)))
      (throw (ex-info "Historical production successor is absent or unqualified" {})))
    (repair/commit-historical-resolution!
     repair-root repair-id
     (TerminalSuccessorAuthority.
      {:schema :wm/historical-repair-resolution-v1
       :repair/id repair-id :repair/status :resolved
       :verification-id verification-id
       :verification-attempt verification-attempt
       :verification-execution
       (select-keys historical-execution [:cohort-id :cohort-sha256 :attempt-id])
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
