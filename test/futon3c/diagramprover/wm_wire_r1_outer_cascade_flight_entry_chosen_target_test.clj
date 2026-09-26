(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-chosen-target-test
  "Wire [:r1-outer-cascade :flight-entry :chosen-target]: the outer
  cascade's choice of target reaching the flight entry.

  Both ends exist. The writer is outer-cascade/select (built by
  H-T-CALLER-I, futon2 3b449beb..b66369d3, after the map at 06d451d6 marked
  the box :not-built): with a non-empty eligible support and a seed it
  returns :chosen-target beside its :target-selection record (and records
  {:absent :no-eligible-target} / {:absent :no-seed} when it cannot
  choose). The reader is flight-driver/resolve-target: a given
  :chosen-target wins over --target and lands as the result's :target with
  :target-source :chosen.

  No live record carries :chosen-target: every flight under
  holes/labs/M-wm-wiring/spike/ was hand-placed ([:flight :target-source]
  :hand-placed; see live-records-read, each pinned and read), and the
  cascade was not built when they were written. So the wire is
  WITNESSED-HERMETICALLY: a real select chooses, and its :chosen-target is
  handed to a real resolve-target, whose :target is the reader's value
  under the field."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.outer-cascade :as oc]
            [futon3c.diagramprover.wm-wire :as w]))

(def field
  "A field of two eligible targets and one a requisition makes ineligible
  (outer_cascade_test.clj's fixture shape)."
  {:considered [{:target "M-a" :kind :mission} {:target "M-b" :kind :mission}
                {:target "M-c" :kind :mission}]
   :feasible [{:target "M-b" :kind :mission :next-step :ready :eligible true}
              {:target "M-c" :kind :mission :next-step :read-criteria :eligible false
               :ineligible-reason :requisition/mooted}
              {:target "M-a" :kind :mission :next-step :ask-interpretation :eligible true}]
   :exclusions []})

(defn- choose [field seed]
  (oc/select {:field field :seed seed :trigger :wallclock-cron}))

(defn observe
  "select (writer) over FIELD with seed 42, then resolve-target (reader)
  handed the writer's :chosen-target; TAMPER edits the opts the reader is
  called with (the bad cases). {:writer the written :chosen-target, :reader
  the reader's value under the field — its result's :target, or the typed
  absence the writer records when it cannot choose (the production shape:
  outer-loop/plan-from-field! then plans nothing)}."
  ([] (observe identity))
  ([tamper]
   (let [sel (choose field 42)
         w (:chosen-target sel)
         opts (tamper {:chosen-target w})
         r (driver/resolve-target opts)]
     {:writer w
      :reader (:target r)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :chosen-target anywhere; [:flight :target-source] :hand-placed — the flight's target came from --target, and the cascade was not built when this was written"}
     {:path (p "flight-ffcd772b.edn")
      :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
      :why "no :chosen-target anywhere; [:flight :target-source] :hand-placed, as every flown flight"}
     {:path (p "plan-before-run.edn")
      :sha256 "010541c497d4108d0ed6d35dc1d944cbfa83e1f2b6d5c77355f9e624696ca083"
      :why "the driver's plan: [:placement :target-source] :hand-placed, no :chosen-target"}]))

(def wire
  {:wire [:r1-outer-cascade :flight-entry :chosen-target]
   :kind :witnessed-hermetically
   :test `the-chosen-target-reaches-the-flight-entry
   :check check
   :live-records-read live-records-read})

(deftest the-chosen-target-reaches-the-flight-entry
  (let [o (check)]
    (is (contains? #{"M-a" "M-b"} (:writer o))
        "the writer's end, from a real select: an eligible target")
    (is (= :chosen (:target-source (driver/resolve-target {:chosen-target (:writer o)})))
        "and the reader records the placement as chosen")
    (is (w/received? o))))

(deftest an-unable-choice-is-a-typed-absence-and-fails-the-wire
  ;; an empty support: select records {:absent :no-eligible-target} and
  ;; returns no :chosen-target; plan-from-field! then plans nothing — the
  ;; reader never receives a value. Observed on the writer's real record.
  (let [f (update field :feasible
                  (fn [fs] (mapv #(assoc % :eligible false :ineligible-reason :requisition/mooted) fs)))
        sel (choose f 3)]
    (is (not (contains? sel :chosen-target)))
    (is (= {:absent :no-eligible-target} (get-in sel [:target-selection :chosen])))
    (is (not (w/received? {:writer (get-in sel [:target-selection :chosen])
                           :reader (get-in sel [:target-selection :chosen])}))
        "a typed absence at the reader fails the wire")))

(deftest a-different-target-fails-the-wire
  ;; the reader is handed a chosen target other than the one the writer wrote
  (let [w (:chosen-target (choose field 42))
        other (first (disj #{"M-a" "M-b"} w))
        o (observe (fn [_] {:chosen-target other}))]
    (is (some? (:reader o)))
    (is (not (w/received? o))
        "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-chosen-target
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :chosen-target))
                    (tree-seq coll? seq r))
          (str path " carries no :chosen-target")))))
