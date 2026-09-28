(ns futon3c.diagramprover.wm-wire-r7-fold-call-enactment-fold-test
  "Wire [:r7-fold-call :habit-fold-call-test :enactment-fold], read from the
  content-addressed wm-wire-r7-fold-call-enactment-fold-test-literal-fixture
  producer record. The producer ran the real war-machine/judge over a temp
  store with one flight record carrying one increment receipt, captured the
  options judge hands select-and-record-cascade!, then ran the real
  select-and-record-cascade! over the cascade-decision fixture's tick-1
  family with the captured :enactment-fold. This reader loads no product
  code. Per-run temporary store paths appear in the record under the stated
  token <run-store>.

  No live record carries both ends (live-records-read, pinned and read):
  the live tick run records predate WM-HABIT-FOLD-CALL-I, so every habit
  read on them is {:status :absent :reason :no-enactment-fold}. So the
  wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r7-fold-call :habit-fold-call-test :enactment-fold])
(def stem "wm-wire-r7-fold-call-enactment-fold-test-literal-fixture")
(def producer (delay (producer-record/record stem)))
(defn- fields [] (get-in @producer [:wires wire-id]))
(def live-records-read (delay (:live-records-read @producer)))
(defn observe [supplied]
  (case supplied
    :judge (:primary (fields))
    :none (get-in (fields) [:interventions :absent])
    (get-in (fields) [:interventions :different])))
(defn check [] (observe :judge))

(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-enactment-fold-test/no-fold-passed-is-a-typed-absence-and-fails-the-wire :kind :refusal
                  :product [:reader :reason] :intervention :before-reader :expected :no-enactment-fold}
   :wire wire-id
   :kind :witnessed-hermetically
   :test `the-fold-judge-hands-reaches-the-selection
   :check check
   :live-records-read @live-records-read})

(deftest the-fold-judge-hands-reaches-the-selection
  (let [o (check)]
    (is (= 1 (:writer-record-count o)) "[:primary :writer-record-count]")
    (is (= 1 (:reader-samples o)) "[:primary :reader-samples]")
    (is (true? (:reader-folded-from-present? o)) "[:primary :reader-folded-from-present?] the consumed state names what was read")
    (is (w/received? o) "[:primary] the writer's fold reached the reader")))

(deftest no-fold-passed-is-a-typed-absence-and-fails-the-wire
  ;; the production state before WM-HABIT-FOLD-CALL-I
  (let [o (observe :none)]
    (is (= {:status :absent :reason :no-enactment-fold}
           (select-keys (:reader o) [:status :reason]))
        "[:interventions :absent :reader]")
    (is (not (w/received? o)) "[:interventions :absent] not received")))

(deftest a-different-fold-than-the-writers-fails-the-wire
  (let [o (observe :different)]
    (is (= 2 (:reader-record-count o)) "[:interventions :different :reader-record-count]")
    (is (not (w/received? o)) "[:interventions :different] present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-fold
  (doseq [{:keys [path sha256]} @live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [occ (get-in (w/read-record path) [:habit-reads :occurrences])]
      (is (seq occ))
      (is (every? #(= :no-enactment-fold (get-in % [:receipt :reason])) occ) path))))
