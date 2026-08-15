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

(def apm-close-pre-fix
  {:spec/id :apm-close-pre-fix
   :boxes
   [{:box/id :assign-checkouts :writes [:environment-checkouts]}
    {:box/id :advance-payload :writes [:environment-checkouts]}
    {:box/id :stamp-environment-outputs
     :reads [:environment-checkouts] :writes [:solver-attempt]}
    {:box/id :emit-trace :reads [:retrieval-probes :solver-attempt] :writes [:trace]}
    {:box/id :validate-trace :reads [:trace] :writes [:validation]}
    {:box/id :write-authorization :reads [:validation]}]})

(deftest recorded-apm-missing-producer-is-found
  (let [findings (wiring/read-never-written
                  (wiring/ingest apm-close-pre-fix))]
    (is (= [{:finding :read-never-written
             :field :retrieval-probes
             :readers [:emit-trace]}]
           findings))
    (is (empty? (filter (comp #{:solver-attempt :trace :validation} :field)
                        findings)))))

(deftest recorded-apm-double-writer-is-found
  (let [findings (wiring/multiply-written
                  (wiring/ingest apm-close-pre-fix))]
    (is (= [{:finding :multiply-written
             :field :environment-checkouts
             :writers [:advance-payload :assign-checkouts]}]
           findings))
    (is (empty? (filter (comp #{:solver-attempt :trace :validation} :field)
                        findings)))))

(deftest empty-spec-has-no-reader-or-multiple-writer-findings
  (let [g (wiring/ingest {:spec/id :empty :boxes []})]
    (is (= [] (wiring/read-never-written g)))
    (is (= [] (wiring/multiply-written g)))))

(deftest slice-one-fixture-has-only-single-writers
  (is (= [] (wiring/multiply-written
             (wiring/ingest apm-round1-pre-fix)))))

(def site-sample
  {:file "test/futon3c/diagramprover/fixtures/site_sample.clj"})

(deftest conformance-finds-declaration-drift
  (let [spec {:spec/id :declaration-drift
              :boxes [{:box/id :present :site site-sample
                       :reads [:f/declared-and-present]}
                      {:box/id :absent :site site-sample
                       :reads [:f/declared-but-absent]}]}
        findings (wiring/conformance "." spec)]
    (is (= [{:finding :declaration-without-occurrence
             :box/id :absent
             :field :f/declared-but-absent
             :role :reads
             :site site-sample}]
           findings))
    (is (not-any? #(= :f/declared-and-present (:field %)) findings))))

(deftest conformance-finds-per-site-undeclared-occurrence
  (let [spec {:spec/id :undeclared-occurrence
              :boxes [{:box/id :at-site :site site-sample
                       :reads [:f/declared-and-present]}
                      {:box/id :universe-only
                       :reads [:f/present-not-declared]}]}]
    (is (= [{:finding :occurrence-without-declaration
             :field :f/present-not-declared
             :site site-sample
             :declared-by []}]
           (wiring/conformance "." spec)))))

(deftest conformance-groups-boxes-sharing-a-site
  (let [spec {:spec/id :shared-site
              :boxes [{:box/id :reader :site site-sample
                       :reads [:f/declared-and-present]}
                      {:box/id :comment-owner :site site-sample
                       :reads [:f/present-not-declared]}]}
        findings (wiring/conformance "." spec)]
    (is (empty? (filter #(= :occurrence-without-declaration (:finding %))
                        findings)))
    (is (= [] findings))))

(deftest conformance-keyword-match-is-boundary-aware
  (let [spec {:spec/id :keyword-boundary
              :boxes [{:box/id :site-anchor :site site-sample}
                      {:box/id :universe-only
                       :reads [:environment-revision]}]}]
    (is (= [] (wiring/conformance "." spec)))))

(deftest declaration-without-site-is-exempt
  (is (= []
         (wiring/conformance
          "."
          {:spec/id :declaration-only
           :boxes [{:box/id :no-site
                    :reads [:f/absent-reader]
                    :writes [:f/absent-writer]}]}))))

(deftest unreadable-site-is-a-finding-not-an-exception
  (let [findings (wiring/conformance
                  "."
                  {:spec/id :unreadable
                   :boxes [{:box/id :ghost
                            :site {:file "no/such/file.clj"}
                            :reads [:f/x]}]})]
    (is (= 1 (count findings)))
    (is (= :site-unreadable (:finding (first findings))))
    (is (= {:file "no/such/file.clj"} (:site (first findings))))
    (is (string? (:error (first findings))))
    (is (not-any? #(= :declaration-without-occurrence (:finding %)) findings)
        "an unreadable site must not also emit drift findings")))

(deftest namespace-sites-resolve-under-src
  (is (= []
         (wiring/conformance
          "."
          {:spec/id :namespace-site
           :boxes [{:box/id :wiring-source
                    :site {:ns "futon3c.diagramprover.wiring"}
                    :reads [:declaration-without-occurrence]}]}))))
