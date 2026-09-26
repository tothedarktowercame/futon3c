(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-draw-seed-test
  "Wire [:r1-outer-cascade :flight-entry :draw-seed]: the seed of the outer
  cascade's draw reaching the flight entry.

  Both ends exist. The writer is outer-cascade/select (built by
  H-T-CALLER-I, futon2 3b449beb..b66369d3, after the map at 06d451d6 marked
  the box :not-built): with a non-empty eligible support and a seed it
  returns :draw-seed beside its :target-selection record (and the record's
  :draw is recomputable from the seed alone). The reader is
  flight-driver/resolve-target: it carries a given :draw-seed beside a
  :chosen-target, and records {:absent :no-draw-seed} when the choice came
  without one (pinned by futon2's loop_closure_test).

  No live record carries :draw-seed: every flight under
  holes/labs/M-wm-wiring/spike/ was hand-placed, and a hand-placed flight
  records no :draw-seed key at all (see live-records-read, each pinned and
  read); the cascade was not built when they were written. So the wire is
  WITNESSED-HERMETICALLY: a real select's :draw-seed is handed with its
  :chosen-target to a real resolve-target, whose :draw-seed is the reader's
  value under the field."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.outer-cascade :as oc]
            [futon3c.diagramprover.wm-wire :as w]))

(def field
  "A field of two eligible targets (outer_cascade_test.clj's fixture shape)."
  {:considered [{:target "M-a" :kind :mission} {:target "M-b" :kind :mission}]
   :feasible [{:target "M-b" :kind :mission :next-step :ready :eligible true}
              {:target "M-a" :kind :mission :next-step :ask-interpretation :eligible true}]
   :exclusions []})

(defn- choose [opts]
  (oc/select (merge {:field field :trigger :wallclock-cron} opts)))

(defn observe
  "select (writer) over FIELD with seed 42, then resolve-target (reader)
  handed the writer's :chosen-target and :draw-seed; TAMPER edits the opts
  the reader is called with (the bad cases). {:writer the written
  :draw-seed, :reader the reader's value under the field — its result's
  :draw-seed}."
  ([] (observe identity))
  ([tamper]
   (let [sel (choose {:seed 42})
         w (:draw-seed sel)
         opts (tamper {:chosen-target (:chosen-target sel) :draw-seed w})
         r (driver/resolve-target opts)]
     {:writer w
      :reader (:draw-seed r)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :draw-seed anywhere; [:flight :target-source] :hand-placed — no draw happened, and the cascade was not built when this was written"}
     {:path (p "flight-ffcd772b.edn")
      :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
      :why "no :draw-seed anywhere; [:flight :target-source] :hand-placed, as every flown flight"}
     {:path (p "plan-before-run.edn")
      :sha256 "010541c497d4108d0ed6d35dc1d944cbfa83e1f2b6d5c77355f9e624696ca083"
      :why "the driver's plan: [:placement :target-source] :hand-placed, no :draw-seed"}]))

(def wire
  {:wire [:r1-outer-cascade :flight-entry :draw-seed]
   :kind :witnessed-hermetically
   :test `the-draw-seed-reaches-the-flight-entry
   :check check
   :live-records-read live-records-read})

(deftest the-draw-seed-reaches-the-flight-entry
  (let [o (check)]
    (is (= 42 (:writer o)) "the writer's end, from a real select with seed 42")
    (is (w/received? o))))

(deftest a-choice-without-a-seed-is-a-typed-absence-and-fails-the-wire
  ;; select without a seed records {:absent :no-seed} and returns no
  ;; :draw-seed; resolve-target handed the choice without a seed records
  ;; {:absent :no-draw-seed} — a typed absence at the reader
  (let [sel (choose {})]
    (is (= {:absent :no-seed} (get-in sel [:target-selection :draw])))
    (is (not (contains? sel :draw-seed))))
  (let [o (observe #(dissoc % :draw-seed))]
    (is (= {:absent :no-draw-seed} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-seed-fails-the-wire
  (let [o (observe #(assoc % :draw-seed 43))]
    (is (some? (:reader o)))
    (is (not (w/received? o))
        "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-draw-seed
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :draw-seed))
                    (tree-seq coll? seq r))
          (str path " carries no :draw-seed")))))
