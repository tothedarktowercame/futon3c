(ns futon3c.apm.memory-snapshot-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.memory-snapshot :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def candidate
  {:memory-id "e-solver-1" :depositor "solver"
   :reviewer "scribe" :review-evidence-id "e-review-1"
   :attachment-status :reviewed
   :pattern-ids ["math-formalization/example"]})

(deftest atomic-snapshot-is-idempotent-and-student-access-is-exact
  (let [dir (Files/createTempDirectory "apm-snapshot-test"
                                       (make-array FileAttribute 0))
        path (.resolve dir "eligible.edn")
        args {:frame-id "f21" :problem-id "p1" :candidates [candidate]
              :path path :evidence-visible? (constantly true)}
        first-result (sut/publish! args)
        replay (sut/publish! args)
        digest (get-in first-result [:snapshot :snapshot/digest])]
    (is (:ok first-result))
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
        visible-prior (assoc candidate :memory-id "prior-ok"
                             :provenance {:frame-id "f28" :problem-id "p28"})
        stale-prior (assoc candidate :memory-id "prior-stale"
                           :review-evidence-id "missing-review"
                           :provenance {:frame-id "f29" :problem-id "p29"})
        own (assoc candidate :memory-id "own"
                   :provenance {:frame-id "f31" :problem-id "p31"})
        visible? #(not= "missing-review" (:review-evidence-id %))
        path (.resolve dir "union.edn")
        result (sut/publish-cumulative!
                {:frame-id "f31" :problem-id "p31"
                 :prior-candidates [visible-prior stale-prior]
                 :own-candidates [own] :path path
                 :evidence-visible? visible?})]
    (is (:ok result))
    (is (= ["own" "prior-ok"]
           (mapv :memory-id (get-in result [:snapshot :snapshot/memories]))))
    (is (= {"f28" 1 "f31" 1}
           (get-in result [:snapshot :snapshot/provenance-summary])))
    (is (= [{:memory-id "prior-stale"
             :provenance {:frame-id "f29" :problem-id "p29"}
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
