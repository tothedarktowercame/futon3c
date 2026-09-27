(ns futon3c.diagramprover.wm-wire-clock-in-r1-outer-cascade-clock-lineage-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-outer-inputs-support :as support]))
(defn check [] (support/observe :clock-lineage :none))
(def wire {:wire [:clock-in :r1-outer-cascade :clock-lineage]
           :kind :witnessed-hermetically :test `the-produced-input-is-received :check check
           :live-records-read support/live-records-read
           :note "Real writer through outer-cascade/select's :target-selection :inputs; recording only, :law-uses [:eligible :delta-g]. Clock uses serialized durable props with HTTP isolated, not a production read-back claim."})
(deftest the-produced-input-is-received
  (let [o (check)]
    (is (w/received? o) (pr-str o))
    (is (:unchanged-law? o))
    ;; futon2 5217cb619 (HG2-Ib): the selection law reads :eligible and the
    ;; per-entry :delta-g; this input is still recorded only.
    (is (= [:eligible :delta-g] (get-in o [:record :law-uses])))))
(deftest typed-absence-at-the-reader-door-is-not-received
  (let [o (support/observe :clock-lineage :absent)]
    (is (not (w/received? o)))
    (is (= {:absent :writer-unavailable} (:reader o)))
    (is (:unchanged-law? o))))
(deftest missing-input-is-recorded-without-changing-choice
  (let [o (support/observe :clock-lineage :missing)]
    (is (not (w/received? o)))
    (is (= {:absent :not-supplied} (:reader o)))
    (is (:unchanged-law? o))))
(deftest changed-value-at-the-reader-door-is-not-the-writers
  (let [o (support/observe :clock-lineage :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))
    (is (:unchanged-law? o))))
(deftest pinned-live-records-lack-the-reader-end
  (is (support/live-reader-absent?)))
