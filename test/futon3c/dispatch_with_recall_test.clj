(ns futon3c.dispatch-with-recall-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.dispatch-with-recall :as dispatch]))

(deftest recall-query-uses-terrain-and-subjects
  (let [query (dispatch/recall-query
               {:problem "bpm-1-5-1"
                :subjects ["nonnegative integral"]}
               "A continuous function has integral zero."
               {"1.5.1" "nonneg integral zero → zero"})]
    (is (= "nonneg integral zero → zero" (:terrain query)))
    (is (= :v1-enriched (:recall-system query)))
    (is (some #{"bpm-1-5-1"} (:terms query)))
    (is (some #{"nonnegative integral"} (:terms query)))))

(deftest recall-query-prioritizes-problem-files-and-records-sources
  (let [root (.toFile
              (java.nio.file.Files/createTempDirectory
               "dispatch-recall-test"
               (make-array java.nio.file.attribute.FileAttribute 0)))
        bundle (java.io.File. root "a-test")
        problem-file (java.io.File. bundle "problem.md")
        outline-file (java.io.File. bundle "proof-outline.md")]
    (.mkdirs bundle)
    (spit problem-file
          "## Problem Statement\nUniformly continuous epsilon quotient bound")
    (spit outline-file
          "Use the Cauchy criterion and contradiction sequence")
    (let [query
          (dispatch/recall-query
           {:problem "a-test" :subjects [] :problem-root (.getPath root)}
           "generic dispatch packet boilerplate"
           {})
          sources (:term-sources query)]
      (is (= [:problem-md :proof-outline-md :stdin-packet]
             (mapv :source sources)))
      (is (= (.getPath problem-file) (:path (first sources))))
      (is (some #{"uniformly"} (:terms query)))
      (is (some #{"cauchy"} (:terms query)))
      (is (< (.indexOf (:terms query) "uniformly")
             (.indexOf (:terms query) "generic"))))))

(deftest substrate-call-deadline-does-not-preempt-total-recall-budget
  (let [timeout-fn
        (ns-resolve 'futon3c.dispatch-with-recall 'per-call-timeout-ms)]
    (is (= 3000 (timeout-fn 3000)))
    (is (= 250 (timeout-fn 100)))))

(deftest default-recall-budget-covers-corpus-projection
  (let [opts (dispatch/parse-args [])]
    (is (= 30000 (:recall-timeout-ms opts)))
    (is (= 0.5 (:receipt-alpha opts)))
    (is (true? (:receipt-ranking? opts))))
  (is (false? (:receipt-ranking?
               (dispatch/parse-args ["--no-receipt-ranking"]))))
  (is (= 0.25 (:receipt-alpha
               (dispatch/parse-args ["--receipt-alpha" "0.25"])))))

(deftest use-receipts-aggregate-once-per-memory-and-pattern
  (let [used-id "e-used"
        peer-id "e-peer"
        memories [{:memory/id used-id
                   :memory/pattern-ids ["math/convolution"]}
                  {:memory/id peer-id
                   :memory/pattern-ids ["math/convolution"]}]
        entries
        [{:evidence/body
          {:event :memory-use
           :phase :offered
           :memory-use
           {:memory-use/surfaced-ids [used-id peer-id]}}}
         {:evidence/body
          {:event :memory-use
           :phase :outcome
           :outcome {:classification :partial-improved}
           :memory-use {:memory-use/used-ids [used-id]}}}]
        stats (dispatch/aggregate-use-receipts entries memories)]
    (is (= {:offered-count 1
            :used-count 1
            :outcome-count 1
            :outcome-quality {:partial-improved 1}}
           (get-in stats [:memories used-id])))
    (is (= 0 (get-in stats [:memories peer-id :used-count])))
    (is (= 1 (get-in stats [:patterns "math/convolution"
                            :offered-count]))
        "one receipt offers the pattern even when it surfaces two members")
    (is (= 1 (get-in stats [:patterns "math/convolution" :used-count])))))

(deftest receipt-factor-promotes-used-memories-and-keeps-cold-neutral
  (let [peer {:memory/id "e-peer"}
        used-a {:memory/id "e-used-a"}
        used-b {:memory/id "e-used-b"}
        stats {"e-peer" {:offered-count 1 :used-count 0}
               "e-used-a" {:offered-count 1 :used-count 1}
               "e-used-b" {:offered-count 1 :used-count 1}}
        ranked (dispatch/rank-memories
                [peer used-a used-b] stats 0.5)
        cold (dispatch/rank-memories
              [{:memory/id "e-cold"}] {} 0.5)]
    (is (= ["e-used-a" "e-used-b" "e-peer"]
           (mapv :memory/id ranked)))
    (is (= 1.5
           (get-in (first ranked)
                   [:dispatch/receipt-stats :ranking-factor])))
    (is (= 1.0
           (get-in (first cold)
                   [:dispatch/receipt-stats :ranking-factor])))))

(deftest packet-injection-is-conditional
  (let [memory {:memory/id "e-memory-1"
                :memory/kind :lemma-location
                :memory/body {:name "integral positivity"
                              :body {:problem-class "Nonnegative integral."}}}]
    (testing "a surfaced memory is prepended"
      (let [packet (dispatch/assemble-packet "PACKET" [memory])]
        (is (.startsWith packet "POTENTIALLY RELEVANT MEMORIES"))
        (is (.contains packet "e-memory-1"))
        (is (.endsWith packet "PACKET"))))
    (testing "empty recall leaves the packet byte-for-byte unchanged"
      (is (= "PACKET" (dispatch/assemble-packet "PACKET" []))))))

(deftest empty-offered-receipt-matches-shared-contract
  (let [entry (dispatch/offered-evidence
               {:problem "a95A04" :from "ground-control"}
               {:status :recall-empty
                :reason :store-unavailable
                :query {:query "a95A04"}
                :memories []}
               "job-1" "session-1")
        receipt (get-in entry [:body :memory-use])]
    (is (= :pattern-outcome (:type entry)))
    (is (= :v1-enriched (get-in entry [:body :recall-system])))
    (is (= :recall-empty (get-in entry [:body :recall-status])))
    (is (= [] (:memory-use/surfaced-ids receipt)))
    (is (= :pending-outcome (:memory-use/status receipt)))
    (is (some #{:recall-empty} (:tags entry)))))

(deftest offered-memories-are-surfaced-but-not-predeclared-used
  (let [entry (dispatch/offered-evidence
               {:problem "a95A04" :from "ground-control"}
               {:status :ok
                :trace-id "recall-1"
                :query {:query "a95A04 interval integral"}
                :memories [{:memory/id "e-memory-1"}]}
               "job-1" "session-1")
        receipt (get-in entry [:body :memory-use])]
    (is (= ["e-memory-1"] (:memory-use/surfaced-ids receipt)))
    (is (= [] (:memory-use/used-ids receipt)))
    (is (= ["e-memory-1"] (:memory-use/unused-ids receipt)))
    (is (= "recall-1" (:memory-use/cascade-id receipt)))))

(deftest offered-receipt-identifies-active-receipt-ranking
  (let [entry
        (dispatch/offered-evidence
         {:problem "a96A04" :from "ground-control"}
         {:status :ok
          :query {:recall-system :v1.1-receipt-ranked}
          :memories [{:memory/id "e-used"}]}
         "job-1" "session-1")]
    (is (= :v1.1-receipt-ranked
           (get-in entry [:body :recall-system])))))
