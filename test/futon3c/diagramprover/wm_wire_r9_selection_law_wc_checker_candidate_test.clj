(ns futon3c.diagramprover.wm-wire-r9-selection-law-wc-checker-candidate-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(def positive (delay (support/observe :candidate-checker :none)))
(defn check [] @positive)
(def wire {:wire [:r9-selection-law :wc-checker :candidate]
           :kind :witnessed-hermetically :test `the-real-reader-handoff :check check
           :live-records-read support/live-records-read
           :note "WIRE-23-C1 declares the executable checker site. Real selector over pinned exemplar interpretations, real enact-fn, real proof2a_check.clj --wc --edn. Empty verdict proves selected id equals enacted id; absent id is join-unverifiable; different id fails the join."})
(deftest the-real-reader-handoff
  (is (seq (support/census)))
  (let [o (check)] (is (w/received? o))
    (is (= [] (:verdict o)))))
(deftest absence-before-reader
  (let [o (support/observe :candidate-checker :absent)]
    (is (not (w/received? o)))
    (is (= :join-unverifiable (get-in o [:verdict :status])))))
(deftest different-value-before-reader
  (let [o (support/observe :candidate-checker :different)]
    (is (not (w/received? o)))
    (is (some #(.contains % "differs from") (:verdict o)))))
