(ns futon3c.diagramprover.wm-wire-trace-record-r7-fold-call-mu-post-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-in-support :as support]))
(defn check [] (support/observe :carry :none))
(def wire {:wire [:trace-record :r7-fold-call [:mu-post {:record :trace}]]
           :kind :witnessed-hermetically :test `the-judge-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "Real write-trace! -> temporary daily file -> recent-trace-records -> read-trace -> reconcile-belief-carry -> judge :belief-pre. This carry does not use read-history-strict."})
(deftest the-judge-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (w/received? o))))
(deftest absence-before-the-judge-does-not-witness-the-wire
  (let [o (support/observe :carry :absent)]
    (is (not (w/received? o)))
    (is (= (:fresh o) (:reader o)))))
(deftest different-carrier-changes-the-produced-value
  (let [o (support/observe :carry :different)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (not (w/received? o)))
    (is (not= (:fresh o) (:reader o)))))
