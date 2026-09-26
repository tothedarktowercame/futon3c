(ns futon3c.diagramprover.wm-wire-r3-aggregate-driver-r7-fold-call-driver-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-in-support :as support]))
(defn check [] (support/observe :driver :none))
(def wire {:wire [:r3-aggregate-driver :r7-fold-call [:driver {:record :r3d-driver}]]
           :kind :witnessed-hermetically :test `the-judge-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "Microstep loop consumes each signed driver; records :aggregated-signed-error and changes post-belief. Typed omission comes from the real aggregator over an absent observation."})
(deftest the-judge-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (w/received? o))))
(deftest absence-before-the-judge-does-not-witness-the-wire
  (let [o (support/observe :driver :absent)]
    (is (not (w/received? o)))
    (is (= :observation-absent (get-in o [:result :belief-aggregation-events 0 :reason])))
    (is (= :every-channel-omitted (get-in o [:result :micro-step-trace 0 :aggregated-driver-unknown])))
    (is (= (:fresh o) (get-in o [:result :belief])))))
(deftest different-carrier-changes-the-produced-value
  (let [o (support/observe :driver :different)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (not (w/received? o)))
    (is (neg? (:reader o)))
    (is (not= (get-in (check) [:result :belief]) (get-in o [:result :belief])))))
