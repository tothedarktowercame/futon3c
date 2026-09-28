(ns futon3c.diagramprover.wm-wire-wc-checker-r7-test-wc-verdict-test
  "Wire [:wc-checker :r7-test :wc-verdict], read from the content-addressed
  wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture producer record.
  The producer ran the named futon2 test
  futon2.aif.selection-reads-fold-test/real-checker-verdict-into-increment
  with capture around the real checker-verdict (bb W_c over the two pinned
  exemplars) and the real increment; this reader loads no product code.

  WIRE-23-C3: the named futon2 test runs bb proof2a_check.clj --wc --edn,
  passes its parsed verdict through enactment-receipts to the real increment
  and asserts the fold count. Bad controls alter the verdict only at
  increment's door."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:wc-checker :r7-test :wc-verdict])
(def stem "wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture")
(def producer (delay (producer-record/record stem)))
(defn- fields [] (get-in @producer [:wires wire-id]))
(def live-records-read (delay (:live-records-read @producer)))
(defn observe [mutation]
  (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-wc-checker-r7-test-wc-verdict-test/different-verdict-before-the-real-reader :kind :value-varying
                  :product [:result :wc-failures] :intervention :before-reader}
   :wire wire-id
   :kind :witnessed-hermetically
   :test 'futon2.aif.selection-reads-fold-test/real-checker-verdict-into-increment
   :check check :live-records-read @live-records-read
   :note "WIRE-23-C3: the named futon2 test runs bb proof2a_check.clj --wc --edn, passes its parsed verdict through enactment-receipts to the real increment and asserts the fold count. Capture wraps the real checker call and increment; bad controls alter the verdict only at increment's door."})

(deftest real-checker-output-reaches-the-named-test
  (let [o (check)]
    (is (w/received? o) "[:primary] writer's verdict reached the reader")
    (is (= [] (:writer o)) "[:primary :writer]")
    (is (= [1 0] (mapv :result-delta (:observations o))) "[:primary :observations :result-delta]")
    (is (= 2 (count (:observations o))) "[:primary :observations] count")
    (is (every? w/received? (:observations o)) "[:primary :observations] each received")
    (is (pos? (get-in o [:reports :count])) "[:primary :reports :count]")
    (is (zero? (get-in o [:reports :bad-count])) "[:primary :reports :bad-count]")))

(deftest typed-absence-before-the-real-reader
  (let [o (observe :absent)]
    (is (not (w/received? o)) "[:interventions :absent] not received")
    (is (= 0 (:result-delta o)) "[:interventions :absent :result-delta]")
    (is (true? (get-in o [:reports :some-fail?])) "[:interventions :absent :reports :some-fail?]")))

(deftest different-verdict-before-the-real-reader
  (let [o (observe :different)]
    (is (not (w/received? o)) "[:interventions :different] not received")
    (is (= ["different-checker-verdict"] (:result-wc-failures o)) "[:interventions :different :result-wc-failures]")
    (is (true? (get-in o [:reports :some-fail?])) "[:interventions :different :reports :some-fail?]")))
