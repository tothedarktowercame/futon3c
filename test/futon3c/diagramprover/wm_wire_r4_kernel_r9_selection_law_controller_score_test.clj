(ns futon3c.diagramprover.wm-wire-r4-kernel-r9-selection-law-controller-score-test
  (:require [futon3c.diagramprover.wm-wire-selection-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/score :law :none)))
(defn check [] @positive)
(def wire {:second-layer {:test `score-changes-selection-posterior :kind :value-varying
                                  :product [:selection-law :posterior] :intervention :before-reader}
           :wire [:r4-kernel :r9-selection-law :controller-score]
           :kind :witnessed-hermetically :test `the-reader-receives-the-kernel-value :check check
           :live-records-read support/live-records-read :note "Ranker over order-kernel-test chain fixture; selector re-emits controller-score at top level. Carrier mutated before real selector."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/score :law :absent)))))
(deftest different-value-before-reader
  (let [o (support/score :law :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))

(deftest score-changes-selection-posterior
  (let [{:keys [before after p p-prime]} (products/score-pair)
        action (:action (first before))
        [g h] (mapv :controller-score before)
        ;; Equal F and E, beta=1: p(chain)=1/(1+exp(G_chain-G_independent)).
        expected (/ 1.0 (+ 1.0 (Math/exp (- g h))))
        changed (/ 1.0 (+ 1.0 (Math/exp (- (+ g 2) h))))]
    (is (not= g h))
    (is (= before (update-in after [0 :controller-score] - 2)))
    (is (< (Math/abs (- expected (get p action))) 1e-12))
    (is (< (Math/abs (- changed (get p-prime action))) 1e-12))
    (is (< (get p-prime action) (get p action)))
    (println :score-posterior {:G [g h] :before p :after p-prime})))
