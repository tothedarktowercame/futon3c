(ns futon3c.diagramprover.wm-wire-r3-flight-ask-r3-test-library-root-test
  "Real calls with IO isolated; no live record carries both ends.
  See support/live-records-read for the pinned record survey."
  (:require [futon3c.diagramprover.wm-wire-ask-library-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-small-support :as support]))
(defn check [] (support/observe :ask-test (fn [v _] v)))
(def wire {:second-layer {:test `real-test-box-detects-root-change :kind :value-varying
                          :product [:reports] :intervention :before-reader}
           :wire [:r3-flight-ask :r3-test :library-root] :kind :witnessed-hermetically
           :test `the-writer-reaches-the-reader :check check
           :live-records-read support/live-records-read})
(deftest the-writer-reaches-the-reader
  (is (w/received? (check))))
(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :ask-test (fn [_ _] {:status :absent :reason :not-carried}))))))
(deftest different-carrier-before-reader-fails
  (let [o (support/observe :ask-test (fn [_ other] other))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest live-records-do-not-carry-both-ends
  (support/assert-live-records))

(deftest real-test-box-detects-root-change
  (products/with-libraries
    (fn [_ b]
      (let [a (products/assertion-report nil) changed (products/assertion-report b)
            counts #(frequencies (map :type (:reports %)))]
        (is (= (:written (first (:calls a))) (:carrier (first (:calls a)))))
        (is (= (:written (first (:calls a))) (:written (first (:calls changed)))))
        (is (= b (get-in changed [:calls 0 :carrier :library-root])))
        (is (= {:pass 3} (counts a)))
        (is (= {:pass 2 :fail 1} (counts changed)))
        (is (= '(str/includes? with "/fixture/library-root")
               (:expected (first (filter #(= :fail (:type %)) (:reports changed))))))
        (prn :library-test-reports {:before (counts a) :after (counts changed)})))))
