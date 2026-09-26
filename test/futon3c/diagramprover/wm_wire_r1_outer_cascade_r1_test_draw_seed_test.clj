(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-draw-seed-test
  "Wire [:r1-outer-cascade :r1-test :draw-seed]: the seed of the outer
  cascade's draw reaching the outer cascade's own test.

  Both ends exist (built by H-T-CALLER-I, futon2 3b449beb..b66369d3, after
  the map at 06d451d6 marked :r1-outer-cascade :not-built). The writer is
  outer-cascade/select: with a non-empty eligible support and a seed it
  returns :draw-seed beside its :target-selection record. The reader is
  futon2/test/futon2/aif/outer_cascade_test.clj (the :box/kind :test box),
  whose reads of this field are

    (is (= 42 (:draw-seed r)))                      ; r a select result
    (is (not (contains? r :draw-seed)))             ; an empty support
    (is (= {:absent :no-seed} (get-in r [:target-selection :chosen])))

  A test box has no runtime var to drive through, so the hermetic witness
  performs exactly those reads on a real select's results.

  No live record carries :draw-seed: every flight under
  holes/labs/M-wm-wiring/spike/ was hand-placed, and a hand-placed flight
  records no :draw-seed key at all (see live-records-read, each pinned and
  read). So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.outer-cascade :as oc]
            [futon3c.diagramprover.wm-wire :as w]))

(def field
  "The reader's own fixture field (outer_cascade_test.clj): two eligible
  targets, one a requisition makes ineligible, one exclusion."
  {:considered [{:target "M-a" :kind :mission} {:target "M-b" :kind :mission}
                {:target "M-c" :kind :mission} {:target "E-x" :kind :excursion}]
   :feasible [{:target "M-b" :kind :mission :next-step :ready :eligible true}
              {:target "M-c" :kind :mission :next-step :read-criteria :eligible false
               :ineligible-reason :requisition/mooted}
              {:target "M-a" :kind :mission :next-step :ask-interpretation :eligible true}]
   :exclusions [{:target "E-x" :kind :excursion :reason :not-lifecycle-shaped}]})

(defn observe
  "select (writer) over the reader's fixture with seed 42, then the
  reader's read (:draw-seed r) on the writer's result; TAMPER edits the
  result before the read (one bad case). {:writer the written :draw-seed,
  :reader the value the reader's read observes, a typed absence when the
  key is not carried}."
  ([] (observe identity))
  ([tamper]
   (let [w (:draw-seed (oc/select {:field field :seed 42 :trigger :wallclock-cron}))
         r (tamper (oc/select {:field field :seed 42 :trigger :wallclock-cron}))]
     {:writer w
      :reader (if (contains? r :draw-seed)
                (:draw-seed r)
                {:absent :field-not-carried})})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :draw-seed anywhere; [:flight :target-source] :hand-placed — no draw happened, and the cascade was not built when this was written"}
     {:path (p "flight-6cda5ee8.edn")
      :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34"
      :why "no :draw-seed anywhere; hand-placed, as every flown flight"}]))

(def wire
  {:wire [:r1-outer-cascade :r1-test :draw-seed]
   :kind :witnessed-hermetically
   :test `the-draw-seed-reaches-the-r1-test
   :check check
   :live-records-read live-records-read})

(deftest the-draw-seed-reaches-the-r1-test
  (let [o (check)]
    (is (= 42 (:writer o)) "the writer's end, from a real select with seed 42")
    (is (w/received? o))))

(deftest an-unseeded-choice-carries-no-field-and-fails-the-wire
  ;; the reader's own reads of the absence: no :draw-seed key, and the
  ;; record's typed absence
  (let [r (oc/select {:field field :trigger :wallclock-cron})]
    (is (not (contains? r :draw-seed)))
    (is (= {:absent :no-seed} (get-in r [:target-selection :chosen]))))
  (let [o (observe #(dissoc % :draw-seed))]
    (is (= {:absent :field-not-carried} (:reader o)))
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
