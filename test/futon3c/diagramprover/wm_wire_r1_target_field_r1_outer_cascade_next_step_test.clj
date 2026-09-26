(ns futon3c.diagramprover.wm-wire-r1-target-field-r1-outer-cascade-next-step-test
  "Wire [:r1-target-field :r1-outer-cascade :next-step]: the target field's
  next step for a target reaching the outer cascade.

  The writer exists: target-field/step (private) merges {:next-step ...}
  onto a feasible entry. The reader's var now exists too — outer-cascade/
  select, built by H-T-CALLER-I (futon2 3b449beb..b66369d3) AFTER the map
  at 06d451d6 marked the box :not-built — but select does not read
  :next-step: its support and excluded lists are built from :eligible,
  :target, :ineligible-reason, :reason and :what-would-make-feasible only,
  and its record carries no trace of :next-step even when every entry
  carries one (demonstrated below). The map declares the read; the
  component does not perform it. Per the requisition this is reported, and
  the map is not fixed here.

  No live record carries :next-step either: nothing under
  holes/labs/M-wm-wiring/spike/ records the field's per-target step (see
  live-records-read, each pinned and read). So the wire is UNVERIFIED — the
  ledger's truthful state, not a failure of this test.

  The typed absences this test's check returns are the test's own
  statements about what it observed, not values observed on a wire."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.outer-cascade :as oc]
            [futon2.aif.target-field]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- field-step [t next-step detail]
  (@(ns-resolve 'futon2.aif.target-field 'step) t next-step detail))

(def entries
  "Two real step outputs: the writer's own records, each carrying
  :next-step."
  [(field-step {:target "M-a" :kind :mission} :ready {:support 2})
   (field-step {:target "M-b" :kind :mission} :read-criteria {:finding {:kind :no-wants}})])

(def reader-absence
  {:absent :reader-does-not-read-the-field
   :why "outer-cascade/select (futon2 b66369d3) builds its record from :eligible, :target, :ineligible-reason, :reason and :what-would-make-feasible only; its record carries no :next-step, though the map at 06d451d6 declares the read"})

(defn observe
  "The writer run for real (step over two targets), then the reader run
  for real over a field whose entries are the writer's outputs, each
  eligible. {:writer the written :next-step of M-a, :reader what the
  reader's record carries of it — a typed absence, because select reads no
  :next-step}."
  []
  (let [field {:considered (mapv #(select-keys % [:target :kind]) entries)
               :feasible (mapv #(assoc % :eligible true) entries)
               :exclusions []}
        r (oc/select {:field field :seed 1 :trigger :wallclock-cron})]
    {:writer (:next-step (first entries))
     :reader reader-absence
     :reader-record (:target-selection r)}))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "no :next-step anywhere in the record: the tick never assessed the target field; the writer never ran in a flight or tick"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :next-step anywhere in the flight record; neither end"}]))

(def wire
  {:wire [:r1-target-field :r1-outer-cascade :next-step]
   :kind :unverified
   :test `the-wire-is-unverified-because-the-reader-reads-no-next-step
   :check check
   :live-records-read live-records-read})

(deftest the-wire-is-unverified-because-the-reader-reads-no-next-step
  (let [o (check)]
    (is (= :ready (:writer o)) "the writer's end, from a real step call")
    (is (some? (:reader-record o)) "the reader exists and ran")
    (is (not-any? #(and (map? %) (some (fn [k] (= :next-step k)) (keys %)))
                  (tree-seq coll? seq (:reader-record o)))
        "the reader's record carries no :next-step, though every input entry carried one")
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-value-than-the-writers-would-fail-anyway
  ;; even if the reader read the field, a read other than the writer's
  ;; value fails the first layer; here over the writer's real value
  (let [o (observe)]
    (is (not (w/received? {:writer (:writer o) :reader :construct})))
    (is (w/received? {:writer (:writer o) :reader :ready})
        "control: the writer's own value would be receivable")))

(deftest the-live-records-carry-no-next-step
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :next-step))
                    (tree-seq coll? seq r))
          (str path " carries no :next-step")))))
