(ns futon3c.diagramprover.wm-wire-eligibility-r1-outer-cascade-eligible-test
  "Wire [:eligibility :r1-outer-cascade :eligible]: the target field's
  eligibility ruling reaching the outer cascade.

  Both ends exist. The writer is target-field/with-eligibility (private):
  :eligible true unless the target's requisition is in a state. The reader
  is outer-cascade/select (built by H-T-CALLER-I, futon2 3b449beb..b66369d3,
  after the map at 06d451d6 marked the box :not-built): its support is the
  eligible feasible entries (support-of filters :eligible), and the
  ineligible ones go on the record's :excluded list with their reason.

  No live record carries either end: nothing under
  holes/labs/M-wm-wiring/spike/ carries :eligible (the field's assess never
  ran in a flight or tick; see live-records-read, each pinned and read), so
  the wire is WITNESSED-HERMETICALLY: entries written by real
  with-eligibility calls are placed in a field, select runs over it, and
  the reader's value under :eligible is observed as the entry's target's
  membership of the record's :support — the reader's only use of the
  field."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.outer-cascade :as oc]
            [futon2.aif.target-field]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- with-eligibility [e req kind]
  (@(ns-resolve 'futon2.aif.target-field 'with-eligibility) e req kind))

(def eligible-entry
  ;; a pending target with no requisition in a state: eligible
  (with-eligibility {:target "M-a" :kind :mission :next-step :ready}
                    {:absent :no-requisition} :mission))

(def ineligible-entry
  ;; a requisition in a state: ineligible, with the reason recorded
  (with-eligibility {:target "M-b" :kind :mission :next-step :read-criteria}
                    {:state :in-progress :text "claimed"} :mission))

(defn- field-with [feasible]
  {:considered (mapv #(select-keys % [:target :kind]) feasible)
   :feasible feasible
   :exclusions []})

(defn observe
  "with-eligibility (writer) over two targets, then select (reader) over a
  field carrying the writer's real entries; TAMPER edits the eligible
  entry's field copy before select runs (the bad cases). {:writer the
  written :eligible of M-a, :reader the reader's value under :eligible for
  M-a — its membership of the record's :support, a typed absence when the
  field is not carried, :ineligible-excluded the reader's record of the
  ineligible twin}."
  ([] (observe identity))
  ([tamper]
   (let [w (:eligible eligible-entry)
         field (field-with [(tamper eligible-entry) ineligible-entry])
         r (oc/select {:field field :seed 1 :trigger :wallclock-cron})
         s (:target-selection r)
         carried? (contains? (tamper eligible-entry) :eligible)]
     {:writer w
      :reader (if carried?
                (contains? (set (:support s)) "M-a")
                {:absent :field-not-carried})
      :ineligible-excluded (some #(when (= "M-b" (:target %)) %) (:excluded s))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "no :eligible anywhere in the record (the :eligible-targets/:eligible-endpoints it carries are the adjudicator's, not the target field's ruling): the tick never assessed the field, and the outer cascade was not built when these records were written"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :eligible anywhere in the flight record; neither end"}]))

(def wire
  {:wire [:eligibility :r1-outer-cascade :eligible]
   :kind :witnessed-hermetically
   :test `the-eligibility-ruling-reaches-the-cascade
   :check check
   :live-records-read live-records-read})

(deftest the-eligibility-ruling-reaches-the-cascade
  (let [o (check)]
    (is (true? (:writer o)) "the writer's end, from a real with-eligibility call")
    (is (true? (:reader o)) "M-a is in the reader's support: it read :eligible true")
    (is (= {:target "M-b" :kind :ineligible :reason :requisition/in-progress}
           (:ineligible-excluded o))
        "the ineligible twin's ruling is read too: excluded, with the writer's reason")
    (is (w/received? o))))

(deftest an-uncarried-field-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :eligible))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-ruling-fails-the-wire
  ;; the writer wrote true; if the field the reader sees says false, the
  ;; reader's read (support membership) is false and the wire fails
  (let [o (observe #(assoc % :eligible false :ineligible-reason :requisition/completed))]
    (is (false? (:reader o)))
    (is (not (w/received? o))
        "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-eligible
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :eligible))
                    (tree-seq coll? seq r))
          (str path " carries no :eligible")))))
