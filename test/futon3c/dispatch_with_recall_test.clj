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
    (is (= :v1.2-receipt-instrumented (:recall-system query)))
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
      (is (= ["epsilon" "criterion" "boilerplate" "bound"]
             (:terms query))
          "the bounded lexical query round-robins across ranked sources")
      (is (not-any? #{"generic"} (:terms query))))))

(deftest recall-query-term-cap-is-parameterised-with-shipped-default-preserved
  (let [base {:problem "a-test"
              :subjects []
              :problem-root "/definitely/not/a/problem/root"
              :query-terms ["one" "two" "three" "four" "five"
                            "six" "seven" "eight" "nine"]}
        default-query (dispatch/recall-query base "packet" {})
        eight-query (dispatch/recall-query
                     (assoc base :query-term-limit 8) "packet" {})]
    (is (= dispatch/default-query-term-limit 4))
    (is (= ["one" "two" "three" "four"] (:terms default-query)))
    (is (= ["one" "two" "three" "four" "five" "six" "seven" "eight"]
           (:terms eight-query)))
    (is (= :explicit-analysis-terms
           (get-in eight-query [:term-sources 0 :source])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"positive integer"
         (dispatch/recall-query (assoc base :query-term-limit 0) "packet" {})))))

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
    (is (= dispatch/default-recall-timeout-ms (:recall-timeout-ms opts)))
    (is (= 0.5 (:receipt-alpha opts)))
    (is (true? (:receipt-ranking? opts))))
  (is (= ["e-one" "e-two"]
         (:withhold-ids
          (dispatch/parse-args
           ["--withhold" "e-one" "--withhold" "e-two"]))))
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

(deftest pre-cutoff-ranking-audit-retains-below-cutoff-candidates
  (let [ranked [{:memory/id "e-ranked"
                 :dispatch/pre-receipt-rank 2
                 :dispatch/base-score 0.95
                 :dispatch/ranking-score 1.14}
                {:memory/id "e-cold"}
                {:memory/id "e-below-cutoff"}]
        audit (dispatch/pre-cutoff-ranking-audit ranked 2)]
    (is (= 3 (count audit)))
    (is (= [1 2 3] (mapv :position audit)))
    (is (= [true true false] (mapv :within-cutoff? audit)))
    (is (= 1.14 (:score (first audit))))
    (is (= :receipt-ranked (:score-kind (first audit))))
    (is (= :deterministic-base-order
           (:score-kind (last audit))))
    (is (= 2 (:cutoff-position (last audit))))))

(deftest eligible-memory-observation-is-post-anchor-and-pre-cutoff
  (let [ranked [{:memory/id "e-first" :memory/body {:summary "anchor one"}}
                {:memory/id "e-second" :memory/body {:summary "anchor two"}}
                {:memory/id "e-third" :memory/body {:summary "anchor three"}}
                {:memory/id "e-ineligible" :memory/body {:summary "other"}}]
        eligible (vec (#'dispatch/eligible-memories ranked "anchor"
                                                     (constantly nil)))
        eligible-ids (->> eligible (map :memory/id) distinct vec)
        limit 2
        surfaced-ids (mapv :memory/id (take limit eligible))
        legacy-surfaced-ids
        (->> ranked
             (filter #(#'dispatch/memory-contains-term? % "anchor"))
             (take limit)
             (mapv :memory/id))]
    (is (= ["e-first" "e-second" "e-third"] eligible-ids))
    (is (= ["e-first" "e-second"] surfaced-ids))
    (is (> (count eligible-ids) (count surfaced-ids)))
    (is (= surfaced-ids (subvec eligible-ids 0 (count surfaced-ids)))
        "surfaced ids are the rank-order prefix of eligible ids")
    (is (= legacy-surfaced-ids surfaced-ids)
        "extracting eligibility does not change the previous surfaced ids")))

(deftest eligible-and-surfaced-ids-agree-without-truncation
  (let [ranked [{:memory/id "e-first" :memory/body {:summary "anchor one"}}
                {:memory/id "e-second" :memory/body {:summary "anchor two"}}]
        eligible (vec (#'dispatch/eligible-memories ranked "anchor"
                                                     (constantly nil)))
        eligible-ids (mapv :memory/id eligible)
        surfaced-ids (mapv :memory/id (take 5 eligible))]
    (is (= eligible-ids surfaced-ids))))

(deftest offered-receipt-persists-the-eligible-vector
  (let [entry (dispatch/offered-evidence
               {:problem "a-test" :from "ground-control"}
               {:status :ok
                :eligible-memory-ids ["e-first" "e-second" "e-third"]
                :memories [{:memory/id "e-first"} {:memory/id "e-second"}]}
               "job-1" "session-1")]
    (is (= ["e-first" "e-second" "e-third"]
           (get-in entry [:body :eligible-memory-ids])))
    (is (= ["e-first" "e-second"]
           (get-in entry [:body :memory-use :memory-use/surfaced-ids])))))

(deftest production-recall-records-post-eligibility-pre-cutoff-vector
  (let [candidates [{:memory/id "e-first"
                     :memory/body {:summary "anchor one"}}
                    {:memory/id "e-second"
                     :memory/body {:summary "anchor two"}}
                    {:memory/id "e-third"
                     :memory/body {:summary "anchor three"}}
                    {:memory/id "e-ineligible"
                     :memory/body {:summary "other"}}]
        recall-result
        (with-redefs-fn
          {#'dispatch/recall-query
           (fn [& _] {:required-term "anchor" :terms ["anchor"]})
           (ns-resolve 'futon3c.dispatch-with-recall 'substrate-seams)
           (fn [& _]
             {:search (constantly {})
              :projection (constantly {})
              :entry (constantly nil)})
           (ns-resolve 'futon3c.peripheral.memory-recall
                       'propose-patterns-by-query)
           (fn [& _]
             {:candidates []
              :content-matches candidates
              :lexical-seed []
              :index-as-of "test-index"})
           (ns-resolve 'futon3c.peripheral.memory-recall
                       'recall-by-endpoints)
           (fn [& _] {:recalls []})}
          #(deref
            (future
              (#'dispatch/recall-now
               {:problem "a-test"
                :subjects []
                :limit 2
                :recall-timeout-ms 3000
                :receipt-ranking? false}
               "PROBLEM PACKET"))))
        receipt (dispatch/offered-evidence
                 {:problem "a-test" :from "ground-control"}
                 recall-result "job-1" "session-1")
        eligible-ids (get-in receipt [:body :eligible-memory-ids])
        surfaced-ids (get-in receipt [:body :memory-use
                                      :memory-use/surfaced-ids])]
    (is (= ["e-first" "e-second" "e-third"] eligible-ids))
    (is (= ["e-first" "e-second"] surfaced-ids))
    (is (> (count eligible-ids) (count surfaced-ids)))
    (is (= surfaced-ids (subvec eligible-ids 0 (count surfaced-ids))))))

(deftest receipt-ranking-fetch-failure-is-loud-without-changing-fallback-order
  (let [candidates [{:memory/id "e-first"} {:memory/id "e-second"}]
        load (dispatch/load-receipt-ranking-stats
              {:enabled? true
               :candidates candidates
               :timeout-ms 1000
               :fetch-entries #(throw (java.net.SocketTimeoutException.
                                        "receipt stats timed out"))})
        fallback (if (seq (get-in load [:stats :memories]))
                   (dispatch/rank-memories
                    candidates (get-in load [:stats :memories]) 0.5)
                   candidates)
        audit (dispatch/receipt-ranking-audit
               {:enabled? true
                :alpha 0.5
                :candidates candidates
                :ranking-load load
                :stats-found? false
                :receipt-stats (:stats load)})
        entry (dispatch/offered-evidence
               {:problem "a-test" :from "ground-control"}
               {:status :ok
                :query {:receipt-ranking audit}
                :memories candidates}
               "job-1" "session-1")]
    (is (= ["e-first" "e-second"] (mapv :memory/id fallback))
        "a failed stats fetch leaves deterministic candidate order untouched")
    (is (true? (get-in entry [:body :receipt-ranking :degraded?])))
    (is (= :stats-fetch-timeout
           (get-in entry [:body :receipt-ranking :reason])))
    (is (= [:deterministic-base-order :deterministic-base-order]
           (mapv :score-kind
                 (get-in entry [:body :receipt-ranking
                                :per-surfaced-memory]))))))

(deftest successful-receipt-ranking-records-ranked-score-kind
  (let [candidate {:memory/id "e-used"}
        entries [{:evidence/body
                  {:event :memory-use
                   :phase :offered
                   :memory-use {:memory-use/surfaced-ids ["e-used"]}}}
                 {:evidence/body
                  {:event :memory-use
                   :phase :outcome
                   :memory-use {:memory-use/used-ids ["e-used"]}}}]
        load (dispatch/load-receipt-ranking-stats
              {:enabled? true
               :candidates [candidate]
               :supplied-entries entries
               :timeout-ms 1000})
        ranked (dispatch/rank-memories
                [candidate] (get-in load [:stats :memories]) 0.5)
        audit (dispatch/receipt-ranking-audit
               {:enabled? true
                :alpha 0.5
                :candidates [candidate]
                :ranking-load load
                :stats-found? true
                :scored-memory-stats {}
                :receipt-stats (:stats load)})
        entry (dispatch/offered-evidence
               {:problem "a-test" :from "ground-control"}
               {:status :ok
                :query {:receipt-ranking audit}
                :memories ranked}
               "job-1" "session-1")]
    (is (false? (get-in entry [:body :receipt-ranking :degraded?])))
    (is (= :receipt-ranked
           (get-in entry [:body :receipt-ranking :mode])))
    (is (= :receipt-ranked
           (get-in entry [:body :receipt-ranking
                          :per-surfaced-memory 0 :score-kind])))))

(deftest disabled-receipt-ranking-is-deliberate-not-degraded
  (let [called? (atom false)
        candidates [{:memory/id "e-first"}]
        load (dispatch/load-receipt-ranking-stats
              {:enabled? false
               :candidates candidates
               :timeout-ms 1000
               :fetch-entries #(reset! called? true)})
        audit (dispatch/receipt-ranking-audit
               {:enabled? false
                :alpha 0.5
                :candidates candidates
                :ranking-load load
                :stats-found? false
                :receipt-stats (:stats load)})]
    (is (false? @called?))
    (is (= :disabled-by-flag (:mode audit)))
    (is (= :ranking-disabled-by-flag (:reason audit)))
    (is (false? (:degraded? audit)))))

(deftest absent-receipt-stats-are-not-a-fetch-degradation
  (let [candidates [{:memory/id "e-cold"}]
        load (dispatch/load-receipt-ranking-stats
              {:enabled? true
               :candidates candidates
               :timeout-ms 1000
               :fetch-entries (constantly [])})
        audit (dispatch/receipt-ranking-audit
               {:enabled? true
                :alpha 0.5
                :candidates candidates
                :ranking-load load
                :stats-found? false
                :receipt-stats (:stats load)})]
    (is (= :ok (:status load)))
    (is (= :deterministic-base-order (:mode audit)))
    (is (= :stats-absent (:reason audit)))
    (is (false? (:degraded? audit)))))

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
        (is (.contains packet "USED <id>: <mechanism>"))
        (is (.contains packet "IGNORED <id>: <reason>"))
        (is (.endsWith packet "PACKET"))))
    (testing "genuine empty recall is explicit"
      (let [packet
            (dispatch/assemble-packet
             "PACKET" {:status :recall-empty :memories []})]
        (is (.contains packet
                       "[dispatch-recall-outcome=completed-empty]"))
        (is (.contains packet "genuine empty retrieval result"))))))

(deftest memory-channel-push-preserves-pre-channel-packet-bytes
  (let [recall-result {:status :recall-empty :memories []}
        expected
        (str
         "DISPATCH-TIME RECALL STATUS\n"
         "[dispatch-recall-outcome=completed-empty]\n"
         "Recall COMPLETED but found no reviewed memories to surface. "
         "Only this status is a genuine empty retrieval result.\n"
         "OUTCOME-RECEIPT REQUIREMENT: copy the bracketed "
         "dispatch-recall-outcome value verbatim into the final Memory usage "
         "section. Do not report an incomplete recall as \"none surfaced\" or "
         "as a terrain gap. When memories are supplied, give EACH surfaced "
         "memory id exactly one line in that section: `USED <id>: <mechanism>` "
         "or `IGNORED <id>: <reason>`. Missing per-id attribution makes the "
         "outcome incomplete and excludes it from use endpoints.\n\n\n"
         "--- PROBLEM PACKET ---\n\nPACKET")
        default-channel (:memory-channel (dispatch/parse-args []))
        explicit-channel
        (:memory-channel
         (dispatch/parse-args ["--memory-channel" ":push"]))]
    (is (= :push default-channel explicit-channel))
    (is (= expected (dispatch/assemble-packet "PACKET" recall-result)))
    (is (= expected
           (dispatch/assemble-packet
            "PACKET" recall-result explicit-channel)))))

(deftest memory-channel-packet-shapes-and-receipts-are-recorded
  (let [memory {:memory/id "e-channel-memory"
                :memory/body {:name "channel memory"
                              :body {:summary "channel summary"}}}
        recall-result {:status :ok
                       :query {:query "channel query"}
                       :memories [memory]}
        packets
        (into {}
              (map (fn [channel]
                     [channel
                      (dispatch/assemble-packet
                       "PACKET" recall-result channel)]))
              [:push :push+pull :pull-only :none])]
    (is (str/includes? (:push packets) "e-channel-memory"))
    (is (not (str/includes? (:push packets)
                            dispatch/memory-pull-invitation-version)))
    (is (str/includes? (:push+pull packets) "e-channel-memory"))
    (is (str/includes? (:push+pull packets)
                       dispatch/memory-pull-invitation-version))
    (is (not (str/includes? (:pull-only packets)
                            "DISPATCH-TIME RECALL STATUS")))
    (is (not (str/includes? (:pull-only packets) "e-channel-memory")))
    (is (str/includes? (:pull-only packets)
                       dispatch/memory-pull-invitation-version))
    (is (= "PACKET" (:none packets)))
    (doseq [channel [:push :push+pull :pull-only :none]]
      (let [entry
            (dispatch/offered-evidence
             {:problem "a-test"
              :from "ground-control"
              :memory-channel channel}
             (if (contains? #{:push :push+pull} channel)
               recall-result
               {:status :not-invoked
                :reason :memory-channel-no-push
                :query {:query "channel query"}
                :memories []})
             "job-1" "session-1")]
        (is (= channel (get-in entry [:body :memory-channel])))
        (is (= dispatch/memory-pull-invitation-version
               (get-in entry
                       [:body :memory-pull-invitation-version])))))))

(deftest dry-run-exercises-every-memory-channel-without-dispatch
  (let [recall-calls (atom [])]
    (with-out-str
      (with-redefs [dispatch/safe-recall
                    (fn [opts _]
                      (swap! recall-calls conj (:memory-channel opts))
                      {:status :recall-empty :memories []})]
        (doseq [channel [:push :push+pull :pull-only :none]]
          (let [result
                (dispatch/run-dispatch!
                 {:problem "a-test"
                  :problem-root "/definitely/not/a/problem/root"
                  :to "codex-test"
                  :from "ground-control"
                  :memory-channel channel
                  :dry-run? true
                  :allow-thin? true}
                 "PROBLEM PACKET")]
            (is (true? (:dry-run? result)))
            (is (= channel
                   (get-in result [:evidence :body :memory-channel])))))))
    (is (= [:push :push+pull] @recall-calls)
        "no-push arms do not execute dispatch-time retrieval")))

(deftest memory-channel-cli-rejects-unknown-arms
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must be one of"
       (dispatch/parse-args ["--memory-channel" ":improvised"]))))

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

(deftest withholding-is-applied-before-packet-and-offered-receipt
  (let [withheld {:memory/id "e-withheld"
                  :memory/body {:name "withheld" :body {:summary "do not show"}}}
        retained {:memory/id "e-retained"
                  :memory/body {:name "retained" :body {:summary "show this"}}}
        result (atom nil)]
    (with-out-str
      (with-redefs [dispatch/safe-recall
                    (fn [_ _] {:status :ok :memories [withheld retained]})]
        (reset! result
                (dispatch/run-dispatch!
                 {:problem "a-test"
                  :to "codex-test"
                  :from "ground-control"
                  :dry-run? true
                  :allow-thin? true
                  :withhold-ids ["e-withheld"]}
                 "PROBLEM PACKET"))))
    (is (not (.contains (:assembled-packet @result) "e-withheld")))
    (is (.contains (:assembled-packet @result) "e-retained"))
    (is (= ["e-retained"]
           (get-in @result [:evidence :body :memory-use
                            :memory-use/surfaced-ids])))
    (is (= ["e-withheld"]
           (get-in @result [:evidence :body :memory-use
                            :memory-use/withheld-ids])))
    (is (= ["e-withheld"]
           (get-in @result [:evidence :body :withholding-delivered-ids])))))

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
           :query-terms ["roots"]
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
    (is (= :v1.2-receipt-instrumented
           (get-in entry [:body :recall-system])))
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
                :lexical-seed [{:evidence-id "e-seed" :score -7.5}]
                :index-as-of "2026-08-01T12:00:00Z"
                :ladder-rung :pair
                :ladder-query "interval integral"
                :memories [{:memory/id "e-memory-1"
                            :via :content-match}]}
               "job-1" "session-1")
        receipt (get-in entry [:body :memory-use])]
    (is (= ["e-memory-1"] (:memory-use/surfaced-ids receipt)))
    (is (= [] (:memory-use/used-ids receipt)))
    (is (= ["e-memory-1"] (:memory-use/unused-ids receipt)))
    (is (= [{:memory-id "e-memory-1" :via :content-match}]
           (:memory-use/surfacing-via receipt)))
    (is (= [{:evidence-id "e-seed" :score -7.5}]
           (get-in entry [:body :recall-lexical-seed])))
    (is (= "2026-08-01T12:00:00Z"
           (get-in entry [:body :recall-index-as-of])))
    (is (= :pair (get-in entry [:body :recall-ladder-rung])))
    (is (= "interval integral"
           (get-in entry [:body :recall-ladder-query])))
    (is (= "recall-1" (:memory-use/cascade-id receipt)))))

(deftest offered-receipt-identifies-active-receipt-ranking
  (let [entry
        (dispatch/offered-evidence
         {:problem "a96A04" :from "ground-control"}
         {:status :ok
          :query {:recall-system :v1.2-receipt-ranked-instrumented}
          :memories [{:memory/id "e-used"}]}
         "job-1" "session-1")]
    (is (= :v1.2-receipt-ranked-instrumented
           (get-in entry [:body :recall-system])))))
