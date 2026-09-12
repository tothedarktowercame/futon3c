(ns futon3c.agents.cascade-verifier-witness-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.chip-board :as board]
            [futon3c.agents.cascade-verifier-board :as cascade]))

(deftest declared-verbs-constrain-effect-kinds
  (let [allowed #{:observe :verify-request :report :yield-turn :typed-none}]
    (doseq [debt [[] [{:id "item" :freshness {:basis-count 1}}]]]
      (let [r (cascade/run {:verification-debt debt} (fn [_] nil))]
        (is (every? allowed (map first (mapcat :effects (:trace r)))))
        (is (= :end/yield (:end-reason r)))))
    (let [missing ((get @board/verb-registry :look-debt)
                   {:shelf-debt []} {:id "missing"} {})]
      (is (= :false (:branch missing)))
      (is (= [:typed-none] (mapv first (:effects missing)))))))

(deftest board-without-zap-does-not-bind-registry
  ;; Isolated atom, no live reload/handler. Registration API itself permits it.
  (with-redefs [board/verb-registry (atom @board/verb-registry)]
    (let [inputs {:verification-debt []}
          original (cascade/run inputs (fn [_] nil))]
      (board/register-verb! :smell-backlog
                            (fn [s _ _] {:branch :false
                                         :effects [[:commit {:repo "counterexample-only"}]]
                                         :state' s}))
      (let [changed (cascade/run inputs (fn [_] nil))]
        (is (not-any? #(= :zap (:verb %)) (:chips cascade/board-v0)))
        (is (= (get-in original [:certificate :board/digest])
               (get-in changed [:certificate :board/digest])))
        (is (= :commit (ffirst (get-in changed [:trace 0 :effects]))))
        (is (true? (get-in changed [:certificate :verified?])))))))
