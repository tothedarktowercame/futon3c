(ns futon3c.diagramprover.wm-wire-r9-decision-r4-rank-dispatch-cascade-belief-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-hop :decision-dispatch)
(def producer (delay (producer-record/record "token-input-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-hop]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-r9-decision-r4-rank-dispatch-cascade-belief-test/belief-intervention-changes-reader-product
                  :kind :record :product [:incoming] :intervention :before-reader}
   :wire [:r9-decision :r4-rank-dispatch [:cascade-belief {:record :rank-state}]]
   :kind :witnessed-hermetically
   :test 'futon3c.diagramprover.wm-wire-r9-decision-r4-rank-dispatch-cascade-belief-test/the-real-reader-produces-the-writers-belief
   :check check
   :live-records-read []
   :note "Real writer and reader; the value is read from the reader's returned receipt or its scoring evaluation, never from the wrapper argument. The helper carry witness uses the retaining branch; overrides have distinct output scopes. The values are read from the producer record `token-input-observe`."})

(deftest the-real-reader-produces-the-writers-belief
  (let [r (check)]
    (is (some? (:writer r)) "writer")
    (is (not (w/typed-absence? (:writer r))) "writer-typed-absence")
    (is (w/received? r) (str "writer-reader " (pr-str r)))))

(deftest an-absent-carrier-is-not-the-writers-belief
  (let [result (get-in (wire-fields) [:interventions :absent])]
    (is (true? (:writer-present? result)) "absent writer-present?")
    (is (false? (:received? result)) "absent received?")))

(deftest a-different-carrier-is-not-the-writers-belief
  (let [result (get-in (wire-fields) [:interventions :different])]
    (is (true? (:writer-differs-from-carrier? result)) "different writer/carrier")
    (is (false? (:received? result)) "different received?")))

(deftest pinned-live-records-do-not-record-both-scoped-endpoints
  (is (true? (get-in @producer [:fields :live-reader-absent?]))))

(deftest belief-intervention-changes-reader-product
  (doseq [[field passed?] (get-in @producer [:fields :second-layer reader-hop])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
