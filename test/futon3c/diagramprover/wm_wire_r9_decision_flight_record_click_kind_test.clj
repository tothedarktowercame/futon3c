(ns futon3c.diagramprover.wm-wire-r9-decision-flight-record-click-kind-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support]))
(defn check [] (support/kind-observe :none))
(def wire {:wire [:r9-decision :flight-record-click :kind]
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read (conj support/live-records-read support/two-record-live-pair)
           :note support/two-record-live-pair})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))))
(deftest absence-before-reader-fails
  (let [o (support/kind-observe :absent)]
    (is (not (w/received? o)))))
(deftest different-value-before-reader-fails
  (let [o (support/kind-observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest two-live-records-agree-without-becoming-verified
  (let [o (support/live-kind-pair)]
    (is (= :universe-not-admitted (:writer o)))
    (is (w/received? o))))
