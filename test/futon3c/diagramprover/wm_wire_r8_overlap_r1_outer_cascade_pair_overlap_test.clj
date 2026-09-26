(ns futon3c.diagramprover.wm-wire-r8-overlap-r1-outer-cascade-pair-overlap-test
  "Wire [:r8-overlap :r1-outer-cascade :pair-overlap]: the target field's
  pair overlap reaching the outer cascade.

  The writer exists: target-field/with-pair-overlap records :pair-overlap
  on each feasible entry ({:comparable true}, {:incommensurable
  {:shared-tokens [...]}}, or a typed absence). The reader's var now exists
  too — outer-cascade/select, built by H-T-CALLER-I (futon2
  3b449beb..b66369d3) AFTER the map at 06d451d6 marked the box :not-built —
  but select does not read :pair-overlap: its support and excluded lists
  are built from :eligible, :target, :ineligible-reason, :reason and
  :what-would-make-feasible only, and its record carries no trace of
  :pair-overlap even when every entry carries one (demonstrated below). The
  map declares the read; the component does not perform it. Per the
  requisition this is reported, and the map is not fixed here.

  No live record carries :pair-overlap either: nothing under
  holes/labs/M-wm-wiring/spike/ carries the key, and futon2's pinned live
  fixture target-field@futon2-7bd17dfb.edn (343 feasible entries) carries
  no :pair-overlap on any entry (see live-records-read, each pinned and
  read). So the wire is UNVERIFIED — the ledger's truthful state, not a
  failure of this test.

  The typed absences this test's check returns are the test's own
  statements about what it observed, not values observed on a wire."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.outer-cascade :as oc]
            [futon2.aif.target-field :as tf]
            [futon3c.diagramprover.wm-wire :as w]))

(def overlap-entries
  "The writer's real outputs: with-pair-overlap over two disjoint
  constructed candidates (comparable), each entry carrying :pair-overlap."
  (tf/with-pair-overlap
    [{:target "M-a" :kind :mission :constructed-candidate {:produces #{:x}} :universe #{:x}}
     {:target "M-b" :kind :mission :constructed-candidate {:produces #{:z}} :universe #{:z}}]))

(def reader-absence
  {:absent :reader-does-not-read-the-field
   :why "outer-cascade/select (futon2 b66369d3) builds its record from :eligible, :target, :ineligible-reason, :reason and :what-would-make-feasible only; its record carries no :pair-overlap, though the map at 06d451d6 declares the read"})

(defn observe
  "The writer run for real, then the reader run for real over a field
  whose entries are the writer's outputs, each eligible. {:writer the
  written [:pair-overlap of M-a][M-b], :reader what the reader's record
  carries of it — a typed absence, because select reads no :pair-overlap}."
  []
  (let [w (get-in (into {} (map (juxt :target :pair-overlap)) overlap-entries)
                  ["M-a" "M-b"])
        field {:considered (mapv #(select-keys % [:target :kind]) overlap-entries)
               :feasible (mapv #(assoc % :eligible true) overlap-entries)
               :exclusions []}
        r (oc/select {:field field :seed 1 :trigger :wallclock-cron})]
    {:writer w
     :reader reader-absence
     :reader-record (:target-selection r)}))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "no :pair-overlap anywhere in the record: the tick never computed the field's row-8 overlap"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :pair-overlap anywhere in the flight record; neither end"}
     {:path "/home/joe/code/futon2/test/fixtures/target-field/target-field@futon2-7bd17dfb.edn"
      :sha256 "760d5b3138dc4bf21526752f3e2f5a769f864fcea6d4204f4a46ec10db447f43"
      :why "the pinned live target field (343 feasible entries): no :pair-overlap key on any entry — the writer had not run when this was pinned"}]))

(def wire
  {:wire [:r8-overlap :r1-outer-cascade :pair-overlap]
   :kind :unverified
   :test `the-wire-is-unverified-because-the-reader-reads-no-pair-overlap
   :check check
   :live-records-read live-records-read})

(deftest the-wire-is-unverified-because-the-reader-reads-no-pair-overlap
  (let [o (check)]
    (is (= {:comparable true} (:writer o)) "the writer's end, from a real with-pair-overlap call")
    (is (some? (:reader-record o)) "the reader exists and ran")
    (is (not-any? #(and (map? %) (some (fn [k] (= :pair-overlap k)) (keys %)))
                  (tree-seq coll? seq (:reader-record o)))
        "the reader's record carries no :pair-overlap, though every input entry carried one")
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-value-than-the-writers-would-fail-anyway
  ;; even if the reader read the field, a read other than the writer's
  ;; value fails the first layer; here over the writer's real value
  (let [o (observe)]
    (is (not (w/received? {:writer (:writer o)
                           :reader {:incommensurable {:shared-tokens [:x]}}})))
    (is (w/received? {:writer (:writer o) :reader {:comparable true}})
        "control: the writer's own value would be receivable")))

(deftest the-live-records-carry-no-pair-overlap
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :pair-overlap))
                    (tree-seq coll? seq r))
          (str path " carries no :pair-overlap")))))
