(ns futon3c.apm.memory-snapshot-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.memory-snapshot :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def candidate
  {:memory-id "e-solver-1" :depositor "solver"
   :reviewer "scribe" :review-evidence-id "e-review-1"
   :attachment-status :reviewed
   :pattern-ids ["math-formalization/example"]
   :name "solver memory" :hook "Use solverMemoryAnchor"
   :body "exact solverMemoryAnchor"})

(deftest candidates-are-ordered-by-promoted-frame-overlap-and-id
  (let [candidates [{:memory-id "z-promoted"
                     :provenance {:problem-id "p1"}
                     :body "unrelatedIdentifier"}
                    {:memory-id "a-inline" :body "sharedAlpha"}
                    {:memory-id "b-fetched"}
                    {:memory-id "c-failed"}]
        fetched (atom [])
        result (sut/order-candidates
                candidates
                {:problem-id "p1"
                 :base-text "sharedAlpha sharedBeta otherIdentifier"
                 :text-fn (fn [memory-id]
                            (swap! fetched conj memory-id)
                            (case memory-id
                              "b-fetched" {:evidence/body
                                           {:name "sharedAlpha"
                                            :hook "sharedBeta"}}
                              "c-failed" (throw (ex-info "store down" {}))))})]
    (is (= ["z-promoted" "b-fetched" "a-inline" "c-failed"]
           (mapv :memory-id (:ordered result))))
    (is (= ["b-fetched" "c-failed"] @fetched))
    (is (= {:signal [:promoted-this-frame :identifier-overlap :memory-id]
            :kind-stratification :observed-only
            :kind-counts {:unknown 4}
            :base-text-present? true
            :scores {"a-inline" 1 "b-fetched" 2 "c-failed" 0
                     "z-promoted" 0}
            :promoted-this-frame 1
            :textless-fetched 1
            :fetch-failed ["c-failed"]}
           (:ordering result)))))

(deftest typed-supply-is-observable-without-changing-default-rank
  (let [candidates [{:memory-id "reg" :memory/kind :regulative/process
                     :body "sharedAlpha sharedBeta"}
                    {:memory-id "legacy" :memory/kind :legacy-advice
                     :body "sharedAlpha"}
                    {:memory-id "sub" :memory/kind :substitutive-content
                     :body "unrelatedIdentifier"}]
        options {:problem-id "p1"
                 :base-text "sharedAlpha sharedBeta unrelatedIdentifier"}
        observed (sut/order-candidates candidates options)
        stratified (sut/order-candidates
                    candidates
                    (assoc options :kind-stratification :substitutive-first))]
    (is (= ["reg" "legacy" "sub"]
           (mapv :memory-id (:ordered observed)))
        "observability alone does not alter the established relevance order")
    (is (= ["sub" "legacy" "reg"]
           (mapv :memory-id (:ordered stratified))))
    (is (= {:regulative/process 1 :substitutive-content 1 :unknown 1}
           (get-in stratified [:ordering :kind-counts])))
    (is (= :observed-only
           (get-in observed [:ordering :kind-stratification])))
    (is (= :substitutive-first
           (get-in stratified [:ordering :kind-stratification])))
    (is (= [:memory-kind :promoted-this-frame :identifier-overlap :memory-id]
           (get-in stratified [:ordering :signal])))))

(deftest typed-supply-preserves-stable-order-and-unknown-bucket
  (let [candidates [{:memory-id "u1"}
                    {:memory-id "r" :memory/kind :regulative/process}
                    {:memory-id "u2" :memory/kind :unrecognized}
                    {:memory-id "s1" :memory/kind :substitutive-content}
                    {:memory-id "s2" :memory/kind :substitutive-content}]
        supply (sut/stratify-candidates candidates)]
    (is (= ["s1" "s2" "u1" "u2" "r"]
           (mapv :memory-id (:ordered supply))))
    (is (= ["u1" "u2"]
           (mapv :memory-id (get-in supply [:buckets :unknown]))))
    (is (= (mapv :memory-id candidates)
           (->> (:buckets supply) vals (mapcat identity)
                (sort-by #(get {"u1" 0 "r" 1 "u2" 2 "s1" 3 "s2" 4}
                               (:memory-id %)))
                (mapv :memory-id)))
        "stratification changes neither membership nor counts")))

(deftest missing-base-text-is-recorded-not-hidden
  (let [result (sut/order-candidates
                [{:memory-id "a" :body "sharedAlpha"}]
                {:problem-id "p1" :base-text nil})]
    (is (false? (get-in result [:ordering :base-text-present?])))
    (is (= {"a" 0} (get-in result [:ordering :scores])))))

(deftest atomic-snapshot-is-idempotent-and-student-access-is-exact
  (let [dir (Files/createTempDirectory "apm-snapshot-test"
                                       (make-array FileAttribute 0))
        path (.resolve dir "eligible.edn")
        args {:frame-id "f21" :problem-id "p1" :candidates [candidate]
              :path path :evidence-visible? (constantly true)
              :base-text "solverMemoryAnchor"
              :base-file-blob "1111111111111111111111111111111111111111"}
        first-result (sut/publish! args)
        replay (sut/publish! args)
        digest (get-in first-result [:snapshot :snapshot/digest])]
    (is (:ok first-result))
    (is (= 2 (get-in first-result [:snapshot :snapshot/version])))
    (is (= [:promoted-this-frame :identifier-overlap :memory-id]
           (get-in first-result [:snapshot :snapshot/ordering :signal])))
    (is (= {:unknown 1}
           (get-in first-result [:snapshot :snapshot/ordering :kind-counts])))
    (is (= "1111111111111111111111111111111111111111"
           (get-in first-result
                   [:snapshot :snapshot/ordering :base-file-blob])))
    (is (= {"e-solver-1" 1}
           (get-in first-result [:snapshot :snapshot/ordering :scores])))
    (is (false? (:idempotent? first-result)))
    (is (:ok replay))
    (is (:idempotent? replay))
    (is (:ok (sut/verify-student-access
              {:path path :expected digest :frame-id "f21" :problem-id "p1"
               :accessible-memory-ids ["e-solver-1"]})))
    (is (= :student-memory-access-invalid
           (:error/code
            (sut/verify-student-access
             {:path path :expected digest :frame-id "f21" :problem-id "p1"
              :accessible-memory-ids []}))))))

(deftest admission-requires-independent-visible-review
  (testing "self review is rejected before publication"
    (is (= :memory-snapshot-candidate-invalid
           (:error/code
            (sut/publish! {:frame-id "f21" :problem-id "p1"
                           :candidates [(assoc candidate :reviewer "solver")]
                           :path "/tmp/not-written-self-review.edn"
                           :evidence-visible? (constantly true)})))))
  (testing "a report cannot substitute for fresh substrate visibility"
    (is (= :memory-snapshot-review-not-visible
           (:error/code
            (sut/publish! {:frame-id "f21" :problem-id "p1"
                           :candidates [candidate]
                           :path "/tmp/not-written-invisible-review.edn"
                           :evidence-visible? (constantly false)}))))))

(deftest unanimous-rejection-publishes-an-empty-certified-snapshot
  (let [dir (Files/createTempDirectory "apm-empty-snapshot-test"
                                       (make-array FileAttribute 0))
        path (.resolve dir "eligible.edn")
        result (sut/publish! {:frame-id "f22" :problem-id "p2"
                              :candidates [] :path path
                              :evidence-visible? (constantly true)})
        digest (get-in result [:snapshot :snapshot/digest])]
    (is (:ok result))
    (is (= [] (get-in result [:snapshot :snapshot/memories])))
    (is (:ok (sut/verify-student-access
              {:path path :expected digest :frame-id "f22" :problem-id "p2"
               :accessible-memory-ids []})))))

(deftest fresh-visibility-check-joins-edge-memory-and-review-evidence
  (let [edge {:hx/type :memory/assert
              :hx/props {:state :current :attachment-status :reviewed
                         :roles {:patterns ["math-formalization/example"]}
                         :review {:evidence-id "e-review-1"}}}
        entries {"e-solver-1" {:evidence/author "solver"}
                 "e-review-1" {:evidence/author "scribe"
                               :evidence/body
                               {:review/reason "actionable exact API"
                                :review/residual "Main.lean:12"}
                               :evidence/subject
                               {:ref/type :memory :ref/id "e-solver-1"}}}]
    (is (sut/candidate-visible? candidate (constantly [edge]) entries))
    (is (false? (sut/candidate-visible?
                 (assoc candidate :reviewer "solver")
                 (constantly [edge]) entries)))))

(deftest cumulative-publication-drops-stale-priors-but-fails-closed-on-own
  (let [dir (Files/createTempDirectory "apm-cumulative-snapshot-test"
                                       (make-array FileAttribute 0))
        visible-prior (assoc candidate :memory-id "prior-ok" :depositor "f28-guide"
                             :provenance {:campaign-id "c1"
                                          :frame-id "f28" :problem-id "p28"})
        stale-prior (assoc candidate :memory-id "prior-stale" :depositor "f29-guide"
                           :review-evidence-id "missing-review"
                           :provenance {:campaign-id "c1"
                                        :frame-id "f29" :problem-id "p29"})
        own (assoc candidate :memory-id "own" :depositor "f31-guide"
                   :provenance {:campaign-id "c2"
                                :frame-id "f31" :problem-id "p31"})
        visible? #(not= "missing-review" (:review-evidence-id %))
        path (.resolve dir "union.edn")
        result (sut/publish-cumulative!
                {:frame-id "f31" :problem-id "p31"
                 :prior-candidates [visible-prior stale-prior]
                 :own-candidates [own] :path path :lineage ["c1" "c2"]
                 :evidence-visible? visible?})]
    (is (:ok result))
    (is (= ["own" "prior-ok"]
           (mapv :memory-id (get-in result [:snapshot :snapshot/memories]))))
    (is (= {"f28" 1 "f31" 1}
           (get-in result [:snapshot :snapshot/provenance-summary])))
    (is (= ["c1" "c2"] (get-in result [:snapshot :snapshot/lineage])))
    (is (= [{:memory-id "prior-stale"
             :provenance {:campaign-id "c1"
                          :frame-id "f29" :problem-id "p29"}
             :finding :snapshot-review-not-visible}]
           (:prior-dropped result)))
    (is (:ok (sut/verify-student-access
              {:path path :expected (get-in result [:snapshot :snapshot/digest])
               :frame-id "f31" :problem-id "p31"
               :accessible-memory-ids ["prior-ok" "own"]})))
    (is (= :memory-snapshot-review-not-visible
           (:error/code
            (sut/publish-cumulative!
             {:frame-id "f31" :problem-id "p31"
              :prior-candidates []
              :own-candidates [(assoc own :review-evidence-id "missing-review")]
              :path (.resolve dir "own-stale.edn")
              :evidence-visible? visible?}))))))

(deftest cumulative-dedup-preserves-earliest-depositor-origin
  (let [dir (Files/createTempDirectory "apm-provenance-dedup"
                                       (make-array FileAttribute 0))
        origin {:campaign-id "c1" :frame-id "f28" :problem-id "p1"}
        carrier {:campaign-id "c2" :frame-id "f46" :problem-id "p2"}
        earliest (assoc candidate :depositor "f28-guide" :provenance origin)
        republished (assoc candidate :depositor "f28-guide" :provenance carrier)
        result (sut/publish-cumulative!
                {:frame-id "f47" :problem-id "p3" :lineage ["c1" "c2"]
                 :prior-candidates [earliest republished]
                 :own-candidates [] :path (.resolve dir "snapshot.edn")
                 :evidence-visible? (constantly true)})]
    (is (:ok result) result)
    (is (= origin
           (get-in result [:snapshot :snapshot/memories 0 :provenance])))))
