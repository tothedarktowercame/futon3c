(ns futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-measurement-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support]))
(defn check [] (support/tick-observe :measurement :none))
(def wire {:wire [:r9-measured-a-version :flight-conditioning-step [:measurement {:record :measured-a}]]
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read support/live-records-read
           :note "Measurement is consumed by the real target-local/read and unmeasured-class check, not retained on the step; no downstream recorded measurement end (second-layer item)."})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))
    (is (= :present (get-in o [:step :status])))
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals (:rates o))))
    (is (every? #(= {:false-neg {:numerator 0 :denominator 5}
                    :false-pos {:numerator 0 :denominator 5}} %) (vals (:measurement o))))))
(deftest absence-before-reader-fails
  (let [o (support/tick-observe :measurement :absent)]
    (is (not (w/received? o)))
    (is (= :unmeasured-class (get-in o [:step :reason])))
    (is (= [(:token o)] (get-in o [:step :tokens])))))
(deftest different-value-before-reader-fails
  (let [o (support/tick-observe :measurement :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
