(ns futon3c.diagramprover.wm-wire-r9-decision-flight-record-click-kind-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-decision :flight-record-click :kind])
(def producer (delay (producer-record/record "measured-live-kind-pair")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id
           :second-layer {:test `abstention-kind-is-recorded-not-an-ask-status
                          :kind :record :product [:clicks 0 :abstention :kind]
                          :intervention :before-reader}
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read []})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))))
(deftest absence-before-reader-fails
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o)))))
(deftest different-value-before-reader-fails
  (let [o (get-in (fields) [:interventions :different])]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest two-live-records-agree-without-becoming-verified
  (let [o (:live-pair (fields))]
    (is (= :universe-not-admitted (:writer o)))
    (is (w/received? o))))

(deftest abstention-kind-is-recorded-not-an-ask-status
  ;; record-click:425,439 stores the kind. run!:587 consults asked :needs,
  ;; not the abstention kind, when deciding :awaiting-answer.
  (let [s (:second-layer (fields))]
    (is (= [:live-c-stale :pending] (:carrier-kinds s)))
    (is (:carrier-otherwise-equal? s))
    (doseq [path [:direct :loop] :let [r (get-in s [:paths path])]]
      (is (= [:live-c-stale :pending] (:click-kinds r))) (is (= [:live-c-stale :pending] (:need-kinds r)))
      (is (= [:no-progress :no-progress] (:statuses r))) (is (= [1 1] (:click-counts r))) (is (:otherwise-equal? r)))))
