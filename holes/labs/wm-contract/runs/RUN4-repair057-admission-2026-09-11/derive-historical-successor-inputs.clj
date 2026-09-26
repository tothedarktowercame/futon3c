(require '[clojure.edn :as edn] '[futon3c.wm.run4-historical-projection :as historical]
         '[futon2.aif.full-loop-cohort :as cohort])
(let [base "/home/joe/run4/repair057-admission"
      reservation (edn/read-string (slurp (str base "/controller/historical-verification-admission-001/reservation.edn")))
      started (edn/read-string (slurp (str base "/controller/001-started.edn")))
      prereg "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/authority/holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/cohort.edn"
      binding {:preregistration prereg :data-root (str base "/cohort-data")
               :cohort-id :run4-repair057-admission-20260911-v1
               :sha256 "2c4f3004c32f5d9f978dcb8cc8aa7d2fee118933ebed41cb940e63c1147113ef"}
      inputs {:roots {:admission (str base "/controller") :bindings (str base "/bindings")
                      :projections (str base "/projections") :run-records (str base "/run-records")
                      :repair-root "/home/joe/code/futon2/data/wm-repair-obligations"
                      :cohort-preregistration prereg :cohort-data-root (str base "/cohort-data")}
              :admission-request (select-keys reservation [:attempt-id :identity])
              :started started}
      b (historical/read-bundle! (:roots inputs) (:admission-request inputs) (:started inputs))
      raw (get-in b [:projection :execution-attempt])
      execution (cohort/closed-execution binding (:id raw))
      artifact {:schema :wm/run4-receipt-derived-historical-inputs-v1
                :repair-id (get-in b [:projection :repair :id])
                :verification-id (get-in b [:projection :repair :verification-id])
                :verification-attempt raw :verification-cohort binding
                :historical-evidence inputs :validated-execution execution}]
  (assert (= "repair-attempt-057-untyped-failure" (:repair-id artifact)))
  (assert (= :historical-verification-awaiting-validation (:outcome execution)))
  (assert (= (get-in b [:projection :cohort :sha256]) (:cohort-sha256 execution)))
  (assert (= (get-in b [:projection :runner-attempt/id]) (:attempt-id execution)))
  (spit "holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/HISTORICAL-SUCCESSOR-INPUTS.edn" (str (pr-str artifact) "\n"))
  (prn {:historical-bundle :validated :execution execution :successor-link :not-yet-frozen}))
(shutdown-agents)
