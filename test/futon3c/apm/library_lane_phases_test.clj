(ns futon3c.apm.library-lane-phases-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.frame-cycle-contract :as cycle]
            [futon3c.apm.library-lane-phases :as sut]))

(def contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-codex-only-v1.edn")))

(def request
  {:frame-id "f9002"
   :problem-id "t00J02"
   :agent-id "f9002-solver"
   :branch "library/t00J02"
   :base-revision "1111111111111111111111111111111111111111"
   :problem-path "problems/t00J02/lean/Main.lean"})

(def ticket {:job-id "job-library-solve"})

(defn fixture-job
  ([] (fixture-job {}))
  ([report-overrides]
   {:job-id (:job-id ticket)
    :agent-id (:agent-id request)
    :state :done
    :report
    (merge
     {:command-own-exit 0
      :branch (:branch request)
      :base-revision (:base-revision request)
      :final-head "2222222222222222222222222222222222222222"
      :committed? true
      :statement-unchanged? true
      :lean {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0}
      :axioms '[propext Classical.choice Quot.sound]
      :clean-before? true
      :clean-after? true
      :mutations [(:problem-path request)]}
     report-overrides)}))

(deftest closed-report-certifies-under-the-library-regime
  (let [result (sut/validate-solve-terminal request ticket (fixture-job))]
    (is (:ok result))
    (is (= :closed (:solve/result result)))))

(deftest reusable-library-progress-certifies-as-partial
  (let [job (fixture-job
             {:lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
              :library/modules ["ConstructionTargets/Foo.lean"]
              :solver/outcome :progress
              :residual "The keyed consumer theorem remains to be connected."
              :mutations ["ConstructionTargets/Foo.lean"
                          "ConstructionTargets/Foo.md"
                          "ConstructionTargets.lean"
                          "problems/t00J02/status.json"]})
        result (sut/validate-solve-terminal request ticket job)]
    (is (:ok result))
    (is (= :partial (:solve/result result)))))

(deftest partial-shaped-report-without-library-module-still-fails
  (let [job (fixture-job
             {:lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
              :library/modules []
              :solver/outcome :progress
              :residual "Only in-file progress exists."})
        result (sut/validate-solve-terminal request ticket job)]
    (is (false? (:ok result)))
    (is (some #{:lean-proof-invalid} (:findings result)))))

(deftest lane-mutation-allowlist-is-forward-only
  (testing "a ConstructionTargets Lean module is permitted"
    (let [result (sut/validate-solve-terminal
                  request ticket
                  (fixture-job {:mutations ["ConstructionTargets/Foo.lean"]}))]
      (is (:ok result))
      (is (= :closed (:solve/result result)))))
  (testing "source code outside the lane remains forbidden"
    (let [result (sut/validate-solve-terminal
                  request ticket
                  (fixture-job {:mutations ["src/whatever.clj"]}))]
      (is (false? (:ok result)))
      (is (some #{:mutation-outside-lane-allowlist} (:findings result)))
      (is (not (some #{:mutation-outside-problem-file} (:findings result)))))))

(deftest partial-receipt-is-content-addressed-and-contract-valid
  (let [job (fixture-job
             {:lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
              :library/modules ["ConstructionTargets/Foo.lean"]
              :solver/outcome :progress
              :residual "The final application remains."
              :mutations ["ConstructionTargets/Foo.lean"]})
        result (sut/solve-receipt contract request ticket job)
        certificate (:certificate result)]
    (is (:ok result))
    (is (= :partial (:receipt/result certificate)))
    (is (= ["ConstructionTargets/Foo.lean"]
           (:receipt/library-modules certificate)))
    (is (= "The final application remains."
           (:receipt/residual certificate)))
    (is (:ok (cycle/validate-receipt contract :solve certificate)))))
