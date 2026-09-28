(ns futon3c.diagramprover.wm-wire-wc-checker-r7-increment-wc-verdict-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:wc-checker :r7-increment :wc-verdict])
(def producer (delay (producer-record/record "selection-out-observe")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:note "WIRE-23-C1 declares the executable checker site. Real checker verdict into increment; delta 1 witnesses the empty verdict, wc-failures retains failed verdicts and missing verdict yields delta 0. The values are read from the producer record `selection-out-observe`."
           :second-layer {:test `different-value-before-reader :kind :value-varying
                          :product [:result :wc-failures] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-real-reader-handoff
           :check check :live-records-read []})
(deftest the-real-reader-handoff
  (is (seq (:census @producer)))
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o))) (is (= 1 (:delta o)))))
(deftest absence-before-reader
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o))) (is (= 0 (:delta o)))))
(deftest different-value-before-reader
  (let [o (get-in (fields) [:interventions :different])]
    (is (not (w/received? o))) (is (= ["different-checker-failure"] (:wc-failures o)))))
