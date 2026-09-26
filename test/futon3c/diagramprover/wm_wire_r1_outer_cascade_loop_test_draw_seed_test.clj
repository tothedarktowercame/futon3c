(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-test-draw-seed-test
  "Wire [:r1-outer-cascade :loop-test :draw-seed]: the seed of the outer
  cascade's draw reaching the loop-closure test.

  Both ends exist. The writer is outer-cascade/select (built by
  H-T-CALLER-I, futon2 3b449beb..b66369d3, after the map at 06d451d6 marked
  the box :not-built): it returns :draw-seed beside its :target-selection
  record. The reader is futon2/test/futon2/aif/loop_closure_test.clj (a
  :box/kind :test box), whose reads of this field are

    (is (= 42 (:draw-seed (driver/resolve-target {:chosen-target \"M-autoclock-in\"
                                                  :draw-seed 42}))))
    (is (= {:target \"M-autoclock-in\" :target-source :chosen
            :draw-seed {:absent :no-draw-seed}}
           (driver/resolve-target {:chosen-target \"M-autoclock-in\"})))

  When the reader was written its reads could only run on hand-made inputs
  (nothing wrote :chosen-target or :draw-seed); now the writer exists, so
  the hermetic witness performs exactly those reads on a REAL select's
  outputs.

  No live record carries :draw-seed: every flight under
  holes/labs/M-wm-wiring/spike/ was hand-placed, and a hand-placed flight
  records no :draw-seed key at all (see live-records-read, each pinned and
  read). So the wire is WITNESSED-HERMETICALLY."
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
  "select (writer) over FIELD with seed 42, then the reader's read —
  (:draw-seed (driver/resolve-target {:chosen-target .. :draw-seed ..})) —
  evaluated on the writer's real outputs; TAMPER edits the opts the read is
  performed on (the bad cases). {:writer the written :draw-seed, :reader
  the value the reader's read observes}."
  ([] (observe identity))
  ([tamper]
   (let [sel (choose {:seed 42})
         w (:draw-seed sel)
         opts (tamper {:chosen-target (:chosen-target sel) :draw-seed w})]
     {:writer w
      :reader (:draw-seed (driver/resolve-target opts))})))

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
  {:wire [:r1-outer-cascade :loop-test :draw-seed]
   :kind :witnessed-hermetically
   :test `the-draw-seed-reaches-the-loop-test
   :check check
   :live-records-read live-records-read})

(deftest the-draw-seed-reaches-the-loop-test
  (let [o (check)]
    (is (= 42 (:writer o)) "the writer's end, from a real select with seed 42")
    (is (w/received? o))))

(deftest a-choice-without-a-seed-is-a-typed-absence-and-fails-the-wire
  ;; select without a seed records {:absent :no-seed}; the reader's second
  ;; read observes {:absent :no-draw-seed} when the choice carries none
  (let [sel (choose {})]
    (is (= {:absent :no-seed} (get-in sel [:target-selection :draw]))))
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
