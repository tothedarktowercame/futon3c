(ns futon3c.dispatch-with-recall-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.dispatch-with-recall :as dispatch]))

(deftest recall-query-uses-terrain-and-subjects
  (let [query (dispatch/recall-query
               {:problem "bpm-1-5-1"
                :subjects ["nonnegative integral"]}
               "A continuous function has integral zero."
               {"1.5.1" "nonneg integral zero → zero"})]
    (is (= "nonneg integral zero → zero" (:terrain query)))
    (is (= :v1-enriched (:recall-system query)))
    (is (not-any? #{"bpm-1-5-1"} (:terms query))
        "problem ids are graph endpoints, not conjunctive lexical terms")
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
      (is (= ["bound" "continuous" "epsilon" "quotient"]
             (:terms query))
          "the bounded lexical query is filled from the highest-priority source")
      (is (not-any? #{"generic"} (:terms query))))))

(deftest substrate-call-deadline-does-not-preempt-total-recall-budget
  (let [timeout-fn
        (ns-resolve 'futon3c.dispatch-with-recall 'per-call-timeout-ms)]
    (is (= 3000 (timeout-fn 3000)))
    (is (= 250 (timeout-fn 100)))))

(deftest content-only-proposal-stops-the-query-ladder
  (is (true? (dispatch/proposal-hit?
              {:candidates []
               :content-matches [{:memory/id "e-content"}]})))
  (is (false? (dispatch/proposal-hit?
               {:candidates [] :content-matches []}))))

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
      (let [packet (dispatch/assemble-packet
                    "PACKET" {:status :ok :memories [memory]})]
        (is (.startsWith packet "DISPATCH-TIME RECALL STATUS"))
        (is (.contains packet "e-memory-1"))
        (is (.endsWith packet "PACKET"))))
    (testing "genuine empty recall is explicit"
      (let [packet
            (dispatch/assemble-packet
             "PACKET" {:status :recall-empty :memories []})]
        (is (.contains packet
                       "[dispatch-recall-outcome=completed-empty]"))
        (is (.contains packet "genuine empty retrieval result"))))))

(deftest dispatch-path-surfaces-reviewed-content-match-in-packet
  (let [reviewed-content
        {:memory/id "e-reviewed-content"
         :memory/attachment-status :reviewed
         :memory/kind :feedback
         :memory/body {:name "transfer the zero count"
                       :body {:summary "Use the reviewed transfer argument."}}
         :via :content-match}
        output
        (with-out-str
          (with-redefs
            [dispatch/safe-recall
             (fn [_ _]
               {:status :ok
                :memories [reviewed-content]})]
            (let [result
                  (dispatch/run-dispatch!
                   {:problem "a-test"
                    :to "codex-test"
                    :from "ground-control"
                    :dry-run? true
                    :allow-thin? true}
                   "PROBLEM PACKET")]
              (is (.contains (:assembled-packet result)
                             "e-reviewed-content"))
              (is (.contains (:assembled-packet result)
                             "transfer the zero count")))))]
    (is (.contains output "[dispatch-recall-outcome=completed-with-memories]"))))

(deftest live-dispatch-path-surfaces-a92j05-content-match
  (if (= "http://127.0.0.1:7073"
         (some-> (System/getenv "FUTON_SUBSTRATE_URL")
                 (str/replace #"/+$" "")))
    (let [result (atom nil)]
      (with-out-str
        (reset!
         result
         (dispatch/run-dispatch!
          {:problem "a92J05"
           :problem-root "/definitely/not/a/problem/root"
           :subjects ["roots outside unit"]
           :to "codex-test"
           :from "ground-control"
           :substrate-base "http://127.0.0.1:7073"
           :limit 5
           :recall-timeout-ms dispatch/default-recall-timeout-ms
           :receipt-ranking? true
           :dry-run? true
           :allow-thin? true}
          "roots outside unit")))
      (is (.contains
           (:assembled-packet @result)
           "e-codexpilot-close-a92J05-by-transferring-the-unit-disk-zero-count"))
      (is (.contains (:assembled-packet @result)
                     "[dispatch-recall-outcome=completed-with-memories]")))
    (is true
        "Live regression requires FUTON_SUBSTRATE_URL=http://127.0.0.1:7073")))

(deftest dispatch-timeout-is-runner-visible-and-distinct-from-empty
  (let [run-with
        (fn [recall-result]
          (let [captured (atom nil)]
            (with-out-str
              (with-redefs
                [dispatch/safe-recall (fn [_ _] recall-result)]
                (reset!
                 captured
                 (:assembled-packet
                  (dispatch/run-dispatch!
                   {:problem "a-test"
                    :to "codex-test"
                    :from "ground-control"
                    :dry-run? true
                    :allow-thin? true}
                   "PROBLEM PACKET")))))
            @captured))
        timeout-packet
        (run-with {:status :recall-empty :reason :timeout :memories []})
        empty-packet
        (run-with {:status :recall-empty :memories []})]
    (is (.contains timeout-packet "[dispatch-recall-outcome=timeout]"))
    (is (.contains timeout-packet "not evidence of a terrain or corpus gap"))
    (is (.contains empty-packet
                   "[dispatch-recall-outcome=completed-empty]"))
    (is (not= timeout-packet empty-packet))))

(deftest non-transport-recall-exception-is-persisted-and-honestly-labelled
  (let [result (atom nil)]
    (with-out-str
      (with-redefs
        [dispatch/bounded-recall
         (fn [_ _]
           (throw
            (IllegalStateException. "projection invariant exploded")))]
        (reset!
         result
         (dispatch/run-dispatch!
          {:problem "a-test"
           :to "codex-test"
           :from "ground-control"
           :dry-run? true
           :allow-thin? true}
          "PROBLEM PACKET"))))
    (is (= :recall-error
           (get-in @result [:evidence :body :recall-reason])))
    (is (= "projection invariant exploded"
           (get-in @result [:evidence :body :recall-error-message])))
    (is (.contains (:assembled-packet @result)
                   "[dispatch-recall-outcome=recall-error]"))
    (is (not (.contains (:assembled-packet @result)
                        "[dispatch-recall-outcome=store-unavailable]")))
    (is (.contains (:assembled-packet @result)
                   "not classified as an HTTP or transport failure"))))

(deftest evidenced-http-failure-remains-store-unavailable
  (let [recall-result
        (with-redefs
          [dispatch/bounded-recall
           (fn [_ _]
             (throw
              (ex-info "substrate request failed"
                       {:url "http://substrate.test/api/alpha/evidence"
                        :status 503})))]
          (dispatch/safe-recall
           {:problem "a-test" :recall-timeout-ms 1000}
           "PROBLEM PACKET"))
        packet (dispatch/assemble-packet "PROBLEM PACKET" recall-result)]
    (is (= :store-unavailable (:reason recall-result)))
    (is (.contains packet
                   "[dispatch-recall-outcome=store-unavailable]"))
    (is (.contains packet "HTTP or transport failure"))))

(deftest standard-packet-query-excludes-operator-and-template-prose
  (let [packet
        (str "DURÉE PREAMBLE: report your search concretely; know the route.\n"
             "--- standard packet follows ---\n"
             "CODEX SORRY LOOP — ONE AXIOM-CLEAN FORMALIZATION TASK\n"
             "Target statement(s):\n"
             "prove CauchyTransform tendsto tendsto zero outside the disk\n"
             "Downstream unblocks: none\n"
             "Available proved support: tendsto_zero\n"
             "Suggested route (if any): contour integral\n"
             "Binding rules:\n"
             "(a) DO YOU KNOW THE ROUTE? report search concretely\n")
        query
        (dispatch/recall-query
         {:problem "a00J05"
          :problem-root "/definitely/not/a/problem/root"
          :subjects ["cauchytransform"]}
         packet {})]
    (is (= :mathematical-fields
           (get-in query [:term-sources 0 :scope])))
    (is (some #{"cauchytransform"} (:terms query)))
    (is (some #{"tendsto"} (:terms query)))
    (is (not-any? #{"route" "search" "report" "concretely"}
                  (:terms query)))))

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
                :memories [{:memory/id "e-memory-1"
                            :via :content-match}]}
               "job-1" "session-1")
        receipt (get-in entry [:body :memory-use])]
    (is (= ["e-memory-1"] (:memory-use/surfaced-ids receipt)))
    (is (= [] (:memory-use/used-ids receipt)))
    (is (= ["e-memory-1"] (:memory-use/unused-ids receipt)))
    (is (= [{:memory-id "e-memory-1" :via :content-match}]
           (:memory-use/surfacing-via receipt)))
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
