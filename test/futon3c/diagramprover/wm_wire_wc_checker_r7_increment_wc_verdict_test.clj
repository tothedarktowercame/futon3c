(ns futon3c.diagramprover.wm-wire-wc-checker-r7-increment-wc-verdict-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(def positive (delay (support/observe :verdict :none)))
(defn check [] @positive)
(def wire {:wire [:wc-checker :r7-increment :wc-verdict]
           :kind :witnessed-hermetically :test `the-real-reader-handoff :check check
           :live-records-read support/live-records-read
           :note "Real checker verdict into increment; delta 1 witnesses the empty verdict, wc-failures retains failed verdicts and missing verdict yields delta 0."})
(deftest the-real-reader-handoff
  (is (seq (support/census)))
  (let [o (check)] (is (w/received? o))
    (is (= 1 (get-in o [:result :delta])))))
(deftest absence-before-reader
  (let [o (support/observe :verdict :absent)]
    (is (not (w/received? o)))
    (is (= 0 (get-in o [:result :delta])))))
(deftest different-value-before-reader
  (let [o (support/observe :verdict :different)]
    (is (not (w/received? o)))
    (is (= ["different-checker-failure"] (get-in o [:result :wc-failures])))))
