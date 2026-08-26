(ns futon3c.apm.solver-progress-rollover-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.queued-frame-terminal :as terminal]
            [futon3c.apm.solver-progress-rollover :as sut]))

(def head (apply str (repeat 40 "a")))

(defn round [ordinal]
  {:ordinal ordinal :dispatch/id (str "dispatch-" ordinal)
   :terminal-state :done
   :report {:final-head head :branch "exp/countdown-f26-m94A03-solver"
            :lean {:exit 0 :errors 0 :sorry-warnings 2}
            :committed? true :clean-after? true
            :failure-account [(str "residual " ordinal)]}})

(deftest frozen-f26-checkpoint-derives-addressed-retry-terminal
  (let [frame {:frame/id "f26" :problem/id "m94A03"}
        result (sut/derive-terminal
                {:frame frame
                 :solve-state {:state/type :solver-strategy-checkpoint-required
                               :rounds (mapv round (range 1 11)) :active nil}
                 :workspace-heads {:solver head :student head}})]
    (is (:ok result))
    (is (:ok (terminal/validate-terminal frame (:terminal-receipt result))))
    (is (= :unsolved (get-in result [:terminal-receipt :problem/outcome])))
    (is (true? (get-in result [:terminal-receipt :retry/same-problem?])))))

(deftest premature-rollover-is-refused
  (let [result (sut/derive-terminal
                {:frame {:frame/id "f26" :problem/id "m94A03"}
                 :solve-state {:state/type :solver-strategy-checkpoint-required
                               :rounds (mapv round (range 1 10)) :active nil}
                 :workspace-heads {:solver head :student head}})]
    (is (= :solver-progress-rollover-invalid (:error/code result)))
    (is (some #{:solver-round-ten-required} (:findings result)))))

(deftest retry-pin-names-the-retained-branch-not-master
  (let [problem (sut/retry-problem
                 {:problem/id "m94A03"} "exp/countdown-f26-m94A03-solver"
                 head (apply str (repeat 40 "b")))]
    (is (= "exp/countdown-f26-m94A03-solver" (:base-branch problem)))
    (is (= head (:revision problem)))))
