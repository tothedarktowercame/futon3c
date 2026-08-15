(ns futon3c.diagramprover.wiring-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.wiring :as wiring]))

(def apm-round1-pre-fix
  {:spec/id :apm-round1-pre-fix
   :boxes
   [{:box/id :registration-edn
     :writes [:reg/environment-revision :reg/harness-revision :reg/solver-seat
              :lean-revision :required-measurement-fields]}
    {:box/id :assign-checkouts :writes [:environment-checkouts]}
    {:box/id :stamp-environment-outputs
     :reads [:environment-checkouts] :writes [:environment-revision]}
    {:box/id :environment-arms-match :reads [:environment-revision]}
    {:box/id :validate-trace :reads [:reg/solver-seat :lean-revision]}
    {:box/id :record-measurement :reads [:required-measurement-fields]}]})

(deftest recorded-apm-orphan-writers-are-found
  (let [g (wiring/ingest apm-round1-pre-fix)
        findings (wiring/written-never-read g)]
    (is (= [{:finding :written-never-read
             :field :reg/environment-revision
             :writers [:registration-edn]}
            {:finding :written-never-read
             :field :reg/harness-revision
             :writers [:registration-edn]}]
           findings))
    (is (empty? (filter (comp #{:reg/solver-seat
                                :lean-revision
                                :required-measurement-fields
                                :environment-checkouts
                                :environment-revision}
                              :field)
                        findings)))))

(deftest ingest-uses-one-field-vertex-and-one-box-edge
  (let [g (wiring/ingest apm-round1-pre-fix)]
    (is (= 7 (graph/num-vertices g)))
    (is (= 6 (graph/num-edges g)))
    (is (= #{:environment-checkouts :environment-revision :lean-revision
             :reg/environment-revision :reg/harness-revision :reg/solver-seat
             :required-measurement-fields}
           (set (map (comp :field #(graph/vertex-data g %))
                     (graph/vertices g)))))))

(deftest empty-spec-has-no-findings
  (is (= [] (wiring/written-never-read
             (wiring/ingest {:spec/id :empty :boxes []})))))

(deftest same-box-read-and-write-counts-as-read
  (testing "self-loop incidence is a read, not an orphan writer"
    (is (= []
           (wiring/written-never-read
            (wiring/ingest
             {:spec/id :self-read
              :boxes [{:box/id :self
                       :reads [:field/x]
                       :writes [:field/x]}]}))))))
