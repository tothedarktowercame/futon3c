(ns futon3c.diagramprover.wm-wire-r1-token-initialization-r1-token-temporal-continuation-belief-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-hop :initialization-temporal)
(def producer (delay (producer-record/record "token-input-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-hop]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-r1-token-initialization-r1-token-temporal-continuation-belief-test/changed-continuation-product
                  :kind :record :product [:belief] :intervention :before-reader}
   :wire [:r1-token-initialization :r1-token-temporal [:continuation-belief {:record :initialized-token-belief-input}]]
   :kind :witnessed-hermetically
   :test 'futon3c.diagramprover.wm-wire-r1-token-initialization-r1-token-temporal-continuation-belief-test/the-real-reader-produces-the-writers-belief
   :check check
   :live-records-read []
   :note "Writer and reader values and intervention relations come from the content-addressed token-input-observe producer record."})

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

(deftest changed-continuation-product
  (doseq [[field passed?] (get-in @producer [:fields :second-layer reader-hop])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
