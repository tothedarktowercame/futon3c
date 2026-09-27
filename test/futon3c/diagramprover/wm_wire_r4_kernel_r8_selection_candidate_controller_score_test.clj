(ns futon3c.diagramprover.wm-wire-r4-kernel-r8-selection-candidate-controller-score-test
  (:require [futon3c.diagramprover.wm-wire-selection-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/score :candidate :none)))
(defn check [] @positive)
(def wire {:second-layer {:test `candidate-records-controller-score :kind :record
                                  :product [:g] :intervention :before-reader}
           :wire [:r4-kernel :r8-selection-candidate :controller-score]
           :kind :witnessed-hermetically :test `the-reader-receives-the-kernel-value :check check
           :live-records-read support/live-records-read :note "Real ranker chain G read by selection-candidate into :g; typed absence and changed score altered before reader."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/score :candidate :absent)))))
(deftest different-value-before-reader
  (let [o (support/score :candidate :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))

(deftest candidate-records-controller-score
  ;; policy/selection-candidate (policy.clj) records controller-score as :g;
  ;; it computes neither a posterior nor a new F from G.
  (let [{:keys [before after candidate candidate-prime]} (products/score-pair)]
    (is (= before (update-in after [0 :controller-score] - 2)))
    (is (= (:controller-score (first before)) (:g candidate)))
    (is (= (:controller-score (first after)) (:g candidate-prime)))
    (is (not= (:g candidate) (:g candidate-prime)))
    (is (= (dissoc candidate :g) (dissoc candidate-prime :g)))
    (println :candidate-record {:before (:g candidate) :after (:g candidate-prime)
                               :unchanged-f (:f candidate)})))
