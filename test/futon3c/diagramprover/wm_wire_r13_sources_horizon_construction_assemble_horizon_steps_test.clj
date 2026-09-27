(ns futon3c.diagramprover.wm-wire-r13-sources-horizon-construction-assemble-horizon-steps-test
  "Real calls with IO isolated; no live record carries both ends.
  See support/live-records-read for the pinned record survey."
  (:require [futon3c.diagramprover.wm-wire-precision-horizon-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-small-support :as support]))
(defn check [] (support/observe :horizon (fn [v _] v)))
(def wire {:second-layer {:test `sources-horizon-changes-assembly-and-g :kind :value-varying
                   :product [:scores] :intervention :before-reader}
   :wire [:r13-sources-horizon :construction-assemble [:horizon-steps {:record :sources}]] :kind :witnessed-hermetically
           :test `the-writer-reaches-the-reader :check check
           :live-records-read support/live-records-read})
(deftest the-writer-reaches-the-reader
  (is (w/received? (check))))
(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :horizon (fn [_ _] {:status :absent :reason :not-carried}))))))
(deftest different-carrier-before-reader-fails
  (let [o (support/observe :horizon (fn [_ other] other))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest live-records-do-not-carry-both-ends
  (support/assert-live-records))

(deftest sources-horizon-changes-assembly-and-g
  (let [a (products/horizon-product identity) b (products/horizon-product inc)]
    (is (= (:written a) (:written b)))
    (is (= (:carrier a) (update-in (:carrier b) [:sources :horizon-steps] dec)))
    (is (= [3 4] (mapv #(get-in % [:problem :horizon-steps]) [a b])))
    (is (= (:state a) (:state b)))
    (is (= (:candidates a) (:candidates b)))
    (is (= (dissoc (:opts a) :horizon-steps) (dissoc (:opts b) :horizon-steps)))
    (is (every? number? (concat (:scores a) (:scores b))))
    (is (not= (:scores a) (:scores b)))
    (prn :sources-horizon {:horizon [3 4] :G [(:scores a) (:scores b)]})))
