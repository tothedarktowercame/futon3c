(do
  (require 'clojure.edn)
  (let [directory "holes/labs/M-apm-demonstration/analysis/apm-bookkeeping-repair-2026-09-11/deployment/"
        read-doc #(clojure.edn/read-string (slurp %))
        applied (read-doc (str directory "applied.edn"))
        state (read-doc "data/apm-campaigns/jit-all-open-v3/coordinator.edn")
        registry "data/apm-coordinators/registry.edn"
        id "jit-queue:jit-all-open-v3"]
    (cond
      (not (and (= :loaded (:status applied))
                (:promotion-runtime-matches-source? applied)
                (:configured-address-in-role-command? applied)))
      {:status :blocked :reason :reload-unconfirmed}
      (:regulator/tick-claim state) {:status :blocked :reason :tick-still-claimed}
      (= :failed (:regulator/status state))
      {:status :blocked :reason :coordinator-failed-during-drain}
      :else
      (let [quiescent ((requiring-resolve 'futon3c.apm.durable-coordinator/stop!)
                       registry id {:stop-cause/type :operator
                                    :stop-cause/reason-code :bookkeeping-repair-deployment})]
        (if-not (and (:ok quiescent) (= :stopped (:status quiescent)))
          (select-keys quiescent [:ok :status :error/code])
          (let [resumed ((requiring-resolve 'futon3c.apm.durable-coordinator/resume!) registry id)
                receipt (assoc (select-keys resumed [:ok :status :error/code :coordinator/id])
                               :at (str (java.time.Instant/now))
                               :source-commit "17720cc6"
                               :quiescence-witness (:quiescence-witness quiescent))]
            (spit (str directory "resumed.edn") (str (pr-str receipt) "\n"))
            receipt))))))
