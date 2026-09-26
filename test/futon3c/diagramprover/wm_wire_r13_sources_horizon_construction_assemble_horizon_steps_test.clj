(ns futon3c.diagramprover.wm-wire-r13-sources-horizon-construction-assemble-horizon-steps-test
  "Real calls with IO isolated; no live record carries both ends.
  See support/live-records-read for the pinned record survey."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-small-support :as support]))
(defn check [] (support/observe :horizon (fn [v _] v)))
(def wire {:wire [:r13-sources-horizon :construction-assemble [:horizon-steps {:record :sources}]] :kind :witnessed-hermetically
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
