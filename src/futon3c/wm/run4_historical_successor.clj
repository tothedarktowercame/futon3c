(ns futon3c.wm.run4-historical-successor
  "Server-owned bridge from strict RUN4 terminal evidence to historical repair
  resolution. No request map or terminal bundle is an admission API."
  (:require [futon2.aif.repair-obligation :as repair]
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
        successor-attempt {:kind :runner-execution
                           :id (get-in bundle [:terminal-projection :attempt/id])}
        projection (:terminal-projection bundle)]
    (when-not (and bundle (= {:task-result :succeeded :infrastructure :safe
                              :evidence-id (:projection-digest bundle)} class)
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
       :controller-attempt (:attempt-id bundle)
       :click-id (get-in bundle [:started :click-id])
       :run-id (get-in bundle [:terminal-projection :run/id])
       :task-result :succeeded :infrastructure :safe
       :projection-digest (:projection-digest bundle)
       :run-record-digest (:run-record-digest bundle)}))))
