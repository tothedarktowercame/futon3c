(ns futon3c.apm.live-orchestration-contract
  "Fail-closed launch audit for live countdown orchestration."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [futon3c.apm.campaign-machine :as machine]))

(defn read-spec [path]
  (try {:ok true :spec (edn/read-string (slurp path))}
       (catch Throwable t
         {:ok false :error/code :live-orchestration-spec-unreadable
          :finding {:message (.getMessage t)}})))

(defn validate
  [{:keys [spec registration-body handlers apparatus-artifacts]}]
  (let [phase-kinds (set (map :kind (vals (:obligation-plan registration-body))))
        administrative (set (:administrative-kinds spec))
        required-handlers (set/union phase-kinds administrative)
        supplied-handlers (set (keep (fn [[kind handler]]
                                       (when (fn? handler) kind)) handlers))
        phase-specs (:phase-kinds spec)
        referenced-cards (set (map :role-card (vals phase-specs)))
        checks
        {:spec-content-addressed?
         (= (:orchestration/id spec)
            (machine/ledger-digest [(dissoc spec :orchestration/id)]))
         :phase-kinds-complete? (= phase-kinds (set (keys phase-specs)))
         :handlers-complete? (set/subset? required-handlers supplied-handlers)
         :role-cards-pinned? (every? #(string? (get-in apparatus-artifacts [% :blob]))
                                     referenced-cards)
         :dispatch-shapes-declared?
         (every? #(and (= :agency-invoke (get-in % [:dispatch :endpoint]))
                       (seq (get-in % [:dispatch :required])))
                 (vals phase-specs))
         :timeouts-explicit?
         (every? #(and (pos-int? (get-in % [:timeouts :request-ms]))
                       (>= (get-in % [:timeouts :turn-ms] 0) 3600000))
                 (vals phase-specs))
         :terminal-validation-declared?
         (every? #(and (= #{:done} (get-in % [:terminal :states]))
                       (true? (get-in % [:terminal :require-job-id?]))
                       (true? (get-in % [:terminal :require-own-exit?])))
                 (vals phase-specs))
         :typed-receipt-providers?
         (every? #(and (keyword? (get-in % [:receipt :type]))
                       (= :terminal-job-report (get-in % [:receipt :provider])))
                 (vals phase-specs))
         :durable-continuations?
         (every? #(and (= :park (get-in % [:continuation :mode]))
                       (true? (get-in % [:continuation :durable?])))
                 (vals phase-specs))}
        missing (set/difference required-handlers supplied-handlers)]
    {:ok (every? true? (vals checks)) :checks checks
     :required-handler-kinds required-handlers
     :supplied-handler-kinds supplied-handlers
     :missing-handler-kinds missing}))
