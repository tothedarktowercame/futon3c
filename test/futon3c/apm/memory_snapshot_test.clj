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
