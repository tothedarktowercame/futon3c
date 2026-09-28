(ns futon3c.diagramprover.wm-wire-r9-selection-law-decision-per-policy-argmax-test
  "Wire [:r9-selection-law :r9-decision :per-policy-argmax]: the selection
  law's posterior mode reaching the joint decision, which reads
  [:selection-law :per-policy-argmax :action] and hands it to
  candidate-derivations as :actions (war_machine.clj:6665-6670).

  The seventh flight's run record carries the writer's end
  ([:decision :selection-law :per-policy-argmax]) and an unrefused
  :candidate-derivations — the read succeeded — but the action value itself
  is not recorded there, so no live record carries both ends
  (live-records-read). The wire is WITNESSED-HERMETICALLY:
  select-action-cascades (writer) runs over the selection-law candidate
  roster; the reader's var cascade-decision-admitted cannot be called short
  of the full joint assembly (focus inputs, theta ledger, precision carry),
  so the witness performs its read verbatim — (get-in decision
  [:selection-law :per-policy-argmax :action]) — on the writer's live
  decision and observes the action the decision would hand on.

  The values are read from the producer record `wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit`."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer
  (delay (producer-record/record
          "wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit")))
(defn- fields [] (:fields @producer))
(defn observe [mutation] (get-in (fields) [:interventions mutation]))
(defn check [] (select-keys (fields) [:writer :reader :action]))

(def wire
  {:second-layer
   {:test 'futon3c.diagramprover.wm-wire-r9-selection-law-decision-per-policy-argmax-test/changed-selection-product
    :kind :record :product [:recorded] :intervention :before-reader}
   :wire [:r9-selection-law :r9-decision :per-policy-argmax]
   :kind :witnessed-hermetically
   :test `the-posterior-mode-reaches-the-decision
   :check check
   :live-records-read []})

(deftest the-posterior-mode-reaches-the-decision
  (let [recorded (fields)
        observed (check)]
    (is (:writer-present? recorded) "writer")
    (is (false? (:writer-typed-absence? recorded)) "writer is not a typed absence")
    (is (:mode-is-cas-b? recorded) "the mode is :cas/b")
    (is (:action-present? recorded) "the action handed to candidate-derivations")
    (is (:action-is-writer-action? recorded) "reader action is the writer action")
    (is (w/received? observed) (str "writer-reader " (pr-str observed)))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [result (observe :absent)]
    (is (:reader-typed-absence? result))
    (is (:action-absent? result))
    (is (false? (:received? result)))))

(deftest a-different-argmax-at-the-reader-fails-the-wire
  (let [result (observe :different)]
    (is (:reader-present? result))
    (is (:writer-reader-differ? result))
    (is (false? (:received? result)))))

(deftest the-live-record-carries-the-writers-end
  (doseq [[field passed?] (dissoc (:live-record (fields)) :source)]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))

(deftest changed-selection-product
  (doseq [[field passed?] (:second-layer (fields))]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
