(ns futon3c.agents.chip-board-witness-test
  "Counterexamples to the unrestricted v0 certificate claims. No I/O effects."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.chip-board :as board]))

(def nil-target-board
  {:entry :z :constants {}
   :chips [{:chip/id :z :verb :zap :wires {:true :yield :false :yield}}]})

(deftest nil-target-bypasses-earlier-feel
  (let [r (board/run-board nil-target-board {} (fn [_] nil))]
    (is (nil? (board/validate-board nil-target-board)))
    (is (nil? (board/validate-wiring nil-target-board)))
    (is (= [[:commit {:repo nil :mode :commit}]] (get-in r [:trace 0 :effects])))
    (is (not-any? #(= :feel (:verb %)) (:trace r)))))

(deftest replay-does-not-validate-effects
  (let [r (board/run-board nil-target-board {} (fn [_] nil))
        forged (assoc-in r [:trace 0 :effects] [[:commit {:repo "foreign" :mode :commit}]])]
    (is (not= (:trace r) (:trace forged)))
    (is (true? (board/verify-trace nil-target-board {} forged)))))

(deftest validated-board-can-throw-instead-of-typed-end
  (let [b {:entry :c :constants {}
           :chips [{:chip/id :c :verb :compare-move
                    :wires {:true :yield :false :yield}}]}]
    (is (nil? (board/validate-board b)))
    (is (nil? (board/validate-wiring b)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown verb"
                         (board/run-board b {} (fn [_] nil))))))
