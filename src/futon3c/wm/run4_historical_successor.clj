(ns futon3c.wm.run4-historical-successor
  "Server-owned bridge from strict RUN4 terminal evidence to historical repair
  resolution. No request map or terminal bundle is an admission API."
  (:require [clojure.string :as str]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon3c.wm.run4-historical-projection :as historical]
            [futon3c.wm.run4-terminal-evidence :as terminal]))

(deftype TerminalSuccessorAuthority [record]
  repair/HistoricalSuccessorAuthority
  (historical-successor-record [_] record))

(defn resolve-from-durable!
  [{:keys [repair-root evidence-roots admission-request started repair-id
           verification-id verification-attempt verification-cohort
           successor-cohort historical-evidence] :as server-config}]
  (when-not (= #{:repair-root :evidence-roots :admission-request :started
                 :repair-id :verification-id :verification-attempt
                 :verification-cohort :successor-cohort :historical-evidence}
               (set (keys server-config)))
    (throw (ex-info "Historical successor server configuration invalid" {})))
  (let [historical-bundle (historical/read-bundle!
                           (:roots historical-evidence)
                           (:admission-request historical-evidence)
                           (:started historical-evidence))
        bundle (terminal/read-terminal-evidence-bundle
                evidence-roots admission-request started)
        class (:classification bundle)
        projection (:terminal-projection bundle)
        cohort (:execution-cohort (:run-record bundle))
        local-attempt (get-in projection [:attempt/id])
        cohort-id (:cohort-id cohort)
        cohort-sha (:sha256 cohort)
        historical-execution (:closed-execution historical-bundle)
        configured-historical-execution
        (cohort/closed-execution
         verification-cohort
         (get-in historical-bundle [:projection :runner-attempt/id]))
        successor-execution (cohort/closed-execution successor-cohort local-attempt)
        successor-attempt (select-keys successor-execution [:kind :id])
        historical-authority? (= 1 (:identity-version historical-execution))
        run-record (:run-record bundle)
        record-identity-present? (contains? run-record :runner-execution/identity)
        record-provenance-present? (contains? run-record :runner-execution/provenance)
        successor-authority? (= 1 (:identity-version successor-execution))]
    (when-not (and historical-bundle
                   (= repair-id (get-in historical-bundle [:projection :repair :id]))
                   (= verification-id
                      (get-in historical-bundle [:projection :repair :verification-id]))
                   (= verification-attempt
                      (get-in historical-bundle [:projection :execution-attempt]))
                   (= verification-attempt
                      (if historical-authority?
                        (select-keys historical-execution [:kind :id])
                        {:kind :runner-execution
                         :id (:attempt-id historical-execution)}))
                   (= historical-execution configured-historical-execution)
                   (= (:cohort-id historical-execution)
                      (get-in historical-bundle [:projection :cohort :cohort-id]))
                   (= (:cohort-sha256 historical-execution)
                      (get-in historical-bundle [:projection :cohort :sha256]))
                   (= (:attempt-id historical-execution)
                      (get-in historical-bundle [:projection :runner-attempt/id]))
                   bundle (= {:task-result :succeeded :infrastructure :safe
                              :evidence-id (:projection-digest bundle)} class)
                   (keyword? cohort-id)
                   (string? cohort-sha) (re-matches #"[0-9a-f]{64}" cohort-sha)
                   (string? local-attempt) (not (str/blank? local-attempt))
                   (= cohort-id (:cohort-id successor-execution))
                   (= cohort-sha (:cohort-sha256 successor-execution))
                   (= record-identity-present? record-provenance-present?)
                   (if successor-authority?
                     (and record-identity-present?
                          (= (:runner-execution/identity run-record)
                             (select-keys successor-execution [:kind :id]))
                          (= (:runner-execution/provenance run-record)
                             (dissoc successor-execution :outcome)))
                     (not record-identity-present?))
                   (= :historical-verification-awaiting-validation
                      (:outcome historical-execution))
                   (= :grounded-change (:outcome successor-execution))
                   (not= (select-keys successor-execution [:kind :id])
                         (select-keys historical-execution [:kind :id]))
                   (= :grounded-change (:outcome projection)))
      (throw (ex-info "Historical production successor is absent or unqualified" {})))
    (repair/commit-historical-resolution!
     repair-root repair-id
     (TerminalSuccessorAuthority.
      {:schema :wm/historical-repair-resolution-v2
       :repair/id repair-id :repair/status :resolved
       :verification-id verification-id
       :verification-attempt verification-attempt
       :verification-execution
       (dissoc historical-execution :outcome)
       :validation-attempt successor-attempt
       :validation-execution (dissoc successor-execution :outcome)
       :controller-attempt (:attempt-id bundle)
       :click-id (get-in bundle [:started :click-id])
       :run-id (get-in bundle [:terminal-projection :run/id])
       :task-result :succeeded :infrastructure :safe
       :projection-digest (:projection-digest bundle)
       :run-record-digest (:run-record-digest bundle)}))))
