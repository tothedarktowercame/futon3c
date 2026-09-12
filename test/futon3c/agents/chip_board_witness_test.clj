(ns futon3c.agents.chip-board-witness-test
  "Repaired witness controls; old counterexamples remain in git history."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.chip-board :as board]))

(def nil-target-board
  {:entry :z :constants {}
   :chips [{:chip/id :z :verb :zap :wires {:true :yield :false :yield}}]})

(deftest nil-target-now-refuses
  (let [r (board/run-board nil-target-board {} (fn [_] nil))]
    (is (nil? (board/validate-board nil-target-board)))
    (is (nil? (board/validate-wiring nil-target-board)))
    (is (= :refusal (ffirst (get-in r [:trace 0 :effects]))))
    (is (not-any? #(= :feel (:verb %)) (:trace r)))))

(deftest replay-now-validates-effects
  (let [r (board/run-board nil-target-board {} (fn [_] nil))
        forged (assoc-in r [:trace 0 :effects] [[:commit {:repo "foreign" :mode :commit}]])]
    (is (board/verify-trace nil-target-board {} r))
    (is (not (board/verify-trace nil-target-board {} forged)))))

(deftest compare-move-now-returns-typed-end
  (let [b {:entry :c :constants {}
           :chips [{:chip/id :c :verb :compare-move
                    :wires {:true :yield :false :yield}}]}
        r (board/run-board b {} (fn [_] nil))]
    (is (nil? (board/validate-board b)))
    (is (nil? (board/validate-wiring b)))
    (is (= :true (get-in r [:trace 0 :branch])))
    (is (= :end/yield (:end-reason r)))))

(deftest arbitrary-unknown-verb-still-not-a-typed-end
  (let [b {:entry :u :constants {}
           :chips [{:chip/id :u :verb :unknown-verb
                    :wires {:true :yield :false :yield}}]}]
    (is (nil? (board/validate-board b)))
    (is (nil? (board/validate-wiring b)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown verb"
                         (board/run-board b {} (fn [_] nil))))))
