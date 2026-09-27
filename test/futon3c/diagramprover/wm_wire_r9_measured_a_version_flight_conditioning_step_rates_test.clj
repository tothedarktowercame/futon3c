(ns futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-rates-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support]))
(defn check [] (support/tick-observe :rates :none))
(def wire {:wire [:r9-measured-a-version :flight-conditioning-step [:rates {:record :measured-a}]]
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read support/live-records-read
           })
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))
    (is (= :present (get-in o [:step :status])))
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals (:rates o))))
    (is (every? #(= {:false-neg {:numerator 0 :denominator 5}
                    :false-pos {:numerator 0 :denominator 5}} %) (vals (:measurement o))))))
(deftest absence-before-reader-fails
  (let [o (support/tick-observe :rates :absent)]
    (is (not (w/received? o)))))
(deftest different-value-before-reader-fails
  (let [o (support/tick-observe :rates :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
