(ns futon3c.diagramprover.wm-wire-r9-f-prefix-supply-r8-selection-candidate-f-prefix-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/prefix-read :none)))
(defn check [] @positive)
(def wire {:wire [:r9-f-prefix-supply :r8-selection-candidate [:f-prefix {:record :ranked-entry}]]
           :kind :witnessed-hermetically :test `the-reader-receives-the-kernel-value :check check
           :live-records-read support/live-records-read :note "Real persisted decision -> conditioning-step with canonical policy key -> prefix admission -> production-ranked -> selection-candidate. Computed F retained under :f-prefix and :f; no synthetic history."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/prefix-read :absent)))))
(deftest different-value-before-reader
  (let [o (support/prefix-read :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))
(deftest computed-prefix-changes-the-production-posterior
  (let [r (support/prefix-effect)]
    (is (= (+ 1 (:f r)) (:changed-f r)))
    (is (< (get-in r [:changed-posterior :observed]) (get-in r [:posterior :observed])))
    (is (= :computed (get-in (check) [:reader :status])))
    (is (= 1 (get-in (check) [:reader :steps])))))
