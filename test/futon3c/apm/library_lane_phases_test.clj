(ns futon3c.apm.library-lane-phases-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
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

(deftest activated-library-preflight-does-not-tell-the-proctor-to-wait
  (let [rendered (sut/prompt (assoc request :phase :preflight))]
    (is (str/includes? rendered "Perform the registered read-only preflight."))
    (is (not (str/includes? rendered "Await activation")))))

(deftest solve-prompt-uses-the-authority-resolved-card-path-once
  (let [card "/qualified/control/role-cards/library-v1.md"
        rendered (sut/prompt (assoc request :phase :solve
                                    :role-card-path card
                                    :role-card-blob "card-blob"
                                    :solver/round 1))]
    (is (str/includes? rendered (str "at " card " (blob card-blob)")))
    ;; Once in frozen authority and once in the explicit instruction; neither
    ;; occurrence may be prefixed by a second checkout root.
    (is (= 2 (count (re-seq (re-pattern (java.util.regex.Pattern/quote card))
                            rendered))))
    (is (not (str/includes? rendered (str "/home/joe/code/futon3c/" card))))))

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

(def verify-request
  (assoc request
         :agent-id "f9002-proctor"
         :certified-final-head "2222222222222222222222222222222222222222"))

(defn- verify-job [overrides]
  (assoc (fixture-job overrides) :agent-id (:agent-id verify-request)))

(deftest verify-accepts-the-partial-head-the-solver-certified
  ;; The exact proctor report that blocked f9957156633803 on 2026-08-23:
  ;; sorry-backed problem target + library mutations on a clean head.
  (let [job (verify-job {:lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
                         :axioms '[propext sorryAx Classical.choice Quot.sound]
                         :mutations ["ConstructionTargets.lean"
                                     "ConstructionTargets/CircleHomology.lean"
                                     (:problem-path request)]})
        result (sut/validate-verify-terminal verify-request ticket job)]
    (is (:ok result) (pr-str result))))

(deftest verify-still-refuses-sorryAx-on-a-sorry-free-file
  (let [job (verify-job {:axioms '[propext sorryAx Classical.choice Quot.sound]})
        result (sut/validate-verify-terminal verify-request ticket job)]
    (is (not (:ok result)))
    (is (= [:axioms-not-permitted] (:findings result)))))

(deftest verify-still-refuses-mutations-outside-the-lane-allowlist
  (let [job (verify-job {:lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
                         :axioms '[propext sorryAx Classical.choice Quot.sound]
                         :mutations ["problems/t01A03/lean/Main.lean"]})
        result (sut/validate-verify-terminal verify-request ticket job)]
    (is (not (:ok result)))
    (is (= [:mutation-outside-lane-allowlist] (:findings result)))))

(deftest verify-still-refuses-errors
  (let [job (verify-job {:lean {:exit 1 :warnings 0 :sorry-warnings 0 :errors 2}})
        result (sut/validate-verify-terminal verify-request ticket job)]
    (is (not (:ok result)))))
