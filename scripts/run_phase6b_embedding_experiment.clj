#!/usr/bin/env clojure -M
;; Phase 6b optional embedding experiment demo (informative only).
;; Usage: clojure -M scripts/run_phase6b_embedding_experiment.clj
(require '[clojure.edn :as edn]
         '[futon3c.peripheral.strategic-embedding-experiment :as exp])

(def fixture
  (edn/read-string (slurp "holes/labs/M-typed-memories/phase6b-embedding-experiment.edn")))

(let [result (exp/run-experiment fixture)]
  (println "Phase 6b embedding experiment —" (:status result))
  (println "algorithm:" (:algorithm result))
  (println "held-out edges:" (:held-out-edge-count result)
           "| train edges:" (:train-edge-count result))
  (doseq [[proposer report] (:proposers result)]
    (println proposer "->" (:metrics report)))
  (println "bypass-guard promotable?:" (-> result :bypass-guard :promotable?))
  (println "promote?:" (:promote? result)
           "| architectural-effect:" (:architectural-effect result)))
