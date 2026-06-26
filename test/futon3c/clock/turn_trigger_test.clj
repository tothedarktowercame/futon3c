(ns futon3c.clock.turn-trigger-test
  "E-arxana-clock car 3: the click-counted turn-trigger fires every N clicks."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.clock.turn-trigger :as tt]))

(defn- with-counter
  "Reset the trigger to a controllable click source + given threshold/riders."
  [counter threshold riders]
  (reset! tt/!state {:threshold threshold
                     :click-count-fn (fn [] @counter)
                     :last-fired-count nil :last-fired-at nil
                     :fire-count 0 :riders riders :loop-running? false}))

(deftest baseline-then-fires-every-threshold
  (let [counter (atom 0) fired (atom 0)]
    (with-counter counter 100 {:count (fn [] (swap! fired inc))})
    (testing "first check sets the baseline, does not fire"
      (is (false? (:fired? (tt/check!))))
      (is (= 0 @fired)))
    (testing "below threshold: no fire"
      (reset! counter 50)
      (is (false? (:fired? (tt/check!))))
      (is (= 50 (:until-next (tt/check!))))
      (is (= 0 @fired)))
    (testing "crossing the threshold fires the rider once + advances baseline"
      (reset! counter 100)
      (is (true? (:fired? (tt/check!))))
      (is (= 1 @fired))
      (is (= 1 (:fire-count @tt/!state))))
    (testing "next window: not due again until another threshold elapses"
      (reset! counter 150)
      (is (false? (:fired? (tt/check!))))
      (reset! counter 205)
      (is (true? (:fired? (tt/check!))))
      (is (= 2 @fired)))))

(deftest rider-failure-is-isolated
  (let [counter (atom 0) ok (atom 0)]
    (with-counter counter 10 {:boom (fn [] (throw (ex-info "boom" {})))
                              :ok   (fn [] (swap! ok inc))})
    (tt/check!)                 ; baseline at 0
    (reset! counter 10)
    (is (true? (:fired? (tt/check!))) "fires despite a throwing rider")
    (is (= 1 @ok) "the healthy rider still ran")))

(deftest add-remove-rider
  (with-counter (atom 0) 5 {})
  (tt/add-rider! :x (fn [] nil))
  (is (contains? (:riders @tt/!state) :x))
  (tt/remove-rider! :x)
  (is (not (contains? (:riders @tt/!state) :x))))

(deftest state-snapshot-shape
  (let [counter (atom 30)]
    (with-counter counter 100 {:belly (fn [] nil)})
    (tt/check!)  ; baseline at 30
    (let [s (tt/state-snapshot)]
      (is (= 100 (:threshold s)))
      (is (= 30 (:clicks s)))
      (is (= [:belly] (:riders s)))
      (is (= 100 (:until-next s))))))
