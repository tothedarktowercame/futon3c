(ns futon3c.diagramprover.wm-wire-flight-run-flight-steps-source-step-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support]))
(defn check [] (support/step-observe :none))
(def wire {:wire [:flight-run :flight-steps-source [:step {:record :enactment-entry}]]
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read support/live-records-read
           })
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))
    (is (= :present (get-in o [:writer :status])))))
(deftest absence-before-reader-fails
  (let [o (support/step-observe :absent)]
    (is (not (w/received? o)))))
(deftest different-value-before-reader-fails
  (let [o (support/step-observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
