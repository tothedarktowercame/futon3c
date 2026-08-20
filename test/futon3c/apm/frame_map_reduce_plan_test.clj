(ns futon3c.apm.frame-map-reduce-plan-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

(def plan
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-map-reduce-plan-v1.edn")))

(defn stage [id]
  (first (filter #(= id (:stage/id %)) (:map/stages plan))))

(deftest guide-is-the-frame-local-memory-intervention
  (let [policy (:mode-policy (stage :guide-interventions))]
    (is (= #{:store-mode :harness-mode} (:exactly-one-of policy)))
    (is (contains? (get-in policy [:store-mode :must-not])
                   :change-retrieval-harness))
    (is (contains? (get-in policy [:harness-mode :must-not])
                   :write-memory))))

(deftest analyst-is-a-post-close-meta-reducer
  (let [analyst (:series/reduce plan)]
    (is (= :terminal-frame-close (:wake analyst)))
    (is (contains? (:outputs analyst) :memory-store-architecture-change))
    (is (contains? (:outputs analyst) :learning-loop-structure-change))
    (is (not (contains? (get-in plan [:frame/reduce :precondition-map-roles])
                        :analyst)))))

(deftest unresolved-card-conflict-is-machine-visible
  (is (= :operator-ruling-required-before-next-registration
         (get-in plan [:contract/conflicts 0 :status]))))
