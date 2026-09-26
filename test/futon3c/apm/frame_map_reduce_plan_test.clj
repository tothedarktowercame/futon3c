(ns futon3c.apm.frame-map-reduce-plan-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

(def plan
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-map-reduce-plan-v1.edn")))

(def analyst-kit
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/analyst-tool-concept-kit-v1.edn")))

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
    (is (= :every-terminal-frame-close (:observation-cadence analyst)))
    (is (= 10 (get-in analyst [:review-policy :initial-window-closed-frames])))
    (is (= :series (:state-scope analyst)))
    (is (contains? (:outputs analyst) :memory-store-architecture-change))
    (is (contains? (:outputs analyst) :learning-loop-structure-change))
    (is (not (contains? (get-in plan [:frame/reduce :precondition-map-roles])
                        :analyst)))))

(deftest analyst-architecture-is-opportunity-driven-not-per-frame
  (let [review (get-in plan [:series/reduce :review-policy])]
    (is (= #{:batch-diagnosis :instrument-health-report}
           (:required-output review)))
    (is (contains? (:opportunity-driven-output review)
                   :memory-store-architecture-change))
    (is (contains? (:early-triggers review)
                   :end-to-end-canary-failure))))

(deftest failed-global-spectral-threshold-cannot-gate-a-batch
  (is (contains? (get-in analyst-kit [:instrument-policy :forbidden-gates])
                 :global-lambda2-threshold))
  (is (= #{:declared-graph-universe :declared-operator
           :declared-step-size :component-accounting}
         (get-in analyst-kit
                 [:instrument-policy :descriptive-only :lambda2 :requires]))))

(deftest analyst-kit-distinguishes-concepts-from-seat-certified-tools
  (is (contains? (:concepts analyst-kit) :route-relative-reachability))
  (is (contains? (:concepts analyst-kit) :artifact-fingerprint-witness))
  (is (some #(= :specified-not-yet-certified-for-analyst-seat
                (:availability %))
            (:tools analyst-kit)))
  (doseq [{:keys [path]} (:evidence-sources analyst-kit)]
    (is (.isFile (java.io.File. path)) path)))

(deftest unresolved-card-conflict-is-machine-visible
  (is (= :operator-ruling-required-before-next-registration
         (get-in plan [:contract/conflicts 0 :status]))))
