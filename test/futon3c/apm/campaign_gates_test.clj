(ns futon3c.apm.campaign-gates-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-gates :as gates]))

(def specs
  (:qualification/gates
   (edn/read-string
    (slurp "holes/labs/M-apm-demonstration/frame-18-step-plan.edn"))))

(def passing-facts
  {:timeouts {:request-minutes 5 :turn-minutes 60 :solver-minutes 60
              :student-minutes 60 :frame-minutes 240 :explicit? true}
   :pins {:coherent? true :complete? true}
   :cast {:ready? true :attributed? true}
   :continuations {:durable? true :wake-tested? true}
   :projection {:ledger-derived? true :frame-matches? true}
   :legs {:required {:solver true :student true :guide true :scribe true
                     :proctor true :analyst true}}
   :memory {:recall-invoked? true :terrain-measured? true
            :dispositions-complete? true :promotion-reviewed? true}
   :separation {:author-reviewer-distinct? true :arms-isolated? true}
   :receipts {:durable? true :replayable? true}
   :apparatus {:unchanged-since-open? true}})

(deftest frame-18-qualification-is-fully-data-driven
  (let [results (gates/evaluate specs passing-facts)]
    (is (= 10 (count results)))
    (is (every? #(= :pass (:gate/status %)) results))))

(deftest frame-17-baseline-fails-before-dispatch
  (let [frame-17 (-> passing-facts
                      (assoc-in [:timeouts :turn-minutes] 5)
                      (assoc-in [:timeouts :explicit?] false)
                      (assoc-in [:legs :required :scribe] false)
                      (assoc-in [:memory :recall-invoked?] false)
                      (assoc-in [:apparatus :unchanged-since-open?] false))
        results (gates/evaluate specs frame-17)
        failed (set (map :gate/id
                         (filter #(= :fail (:gate/status %)) results)))]
    (is (= #{:seat-budgets :required-legs :memory-path :apparatus-frozen}
           failed))))

(deftest missing-facts-fail-closed-with-visible-evidence
  (let [result (first (gates/evaluate specs {}))]
    (is (= :fail (:gate/status result)))
    (is (every? #(nil? (:actual %))
                (get-in result [:gate/evidence :requirements])))))

(deftest only-gates-applicable-to-the-next-obligation-are-evaluated
  (let [obligation {:obligation/action {:kind :open-frame}}
        results (gates/evaluate-obligation specs passing-facts obligation)]
    (is (= #{:seat-budgets :pin-coherence :cast-ready
             :continuation-control :projection-coherence
             :experimental-separation :durable-replay}
           (set (map :gate/id results))))))
