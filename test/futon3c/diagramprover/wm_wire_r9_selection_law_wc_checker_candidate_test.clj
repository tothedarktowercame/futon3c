(ns futon3c.diagramprover.wm-wire-r9-selection-law-wc-checker-candidate-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-selection-law :wc-checker :candidate])
(def producer (delay (producer-record/record "selection-out-observe")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:note "WIRE-23-C1 declares the executable checker site. Real selector over pinned exemplar interpretations, real enact-fn, real proof2a_check.clj --wc --edn. Empty verdict proves selected id equals enacted id; absent id is join-unverifiable; different id fails the join. The values are read from the producer record `selection-out-observe`."
           :second-layer {:test `different-value-before-reader :kind :value-varying :product [:verdict]
                          :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-real-reader-handoff
           :check check :live-records-read []})
(deftest the-real-reader-handoff
  (is (seq (:census @producer)))
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o))) (is (= [] (:verdict o)))))
(deftest absence-before-reader
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o))) (is (= :join-unverifiable (get-in o [:verdict :status])))))
(deftest different-value-before-reader
  (let [o (get-in (fields) [:interventions :different])]
    (is (not (w/received? o))) (is (some #(.contains % "differs from") (:verdict o)))))
