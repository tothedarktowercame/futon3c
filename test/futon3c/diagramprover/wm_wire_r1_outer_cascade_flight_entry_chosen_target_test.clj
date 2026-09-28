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
  under the field.

  The values are read from the producer record: the producer ran the real
  select and resolve-target, and the second-layer products pipeline; this
  reader loads no product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r1-outer-cascade :flight-entry :chosen-target])
(def stem "wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit")
(def producer (delay (producer-record/record stem)))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation]
  (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))

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
   :second-layer {:test `target-controls-token-identity-and-store-lookup
                  :kind :value-varying :product [:clicks]
                  :intervention :before-reader}
   :check check
   :live-records-read live-records-read})

(deftest the-chosen-target-reaches-the-flight-entry
  (let [o (check)]
    (is (contains? #{"M-a" "M-b"} (:writer o))
        "[:primary :writer] the writer's end, from a real select: an eligible target")
    (is (= :chosen (:target-source o))
        "[:primary :target-source] and the reader records the placement as chosen")
    (is (w/received? o) "[:primary] the writer's value reached the reader")))

(deftest an-unable-choice-is-a-typed-absence-and-fails-the-wire
  ;; an empty support: select records {:absent :no-eligible-target} and
  ;; returns no :chosen-target; plan-from-field! then plans nothing — the
  ;; reader never receives a value. Observed on the writer's real record.
  (let [o (observe :unable)]
    (is (false? (:chosen-target-present? o)) "[:interventions :unable :chosen-target-present?]")
    (is (= {:absent :no-eligible-target} (:chosen o)) "[:interventions :unable :chosen]")
    (is (not (w/received? {:writer (:chosen o) :reader (:chosen o)}))
        "a typed absence at the reader fails the wire")))

(deftest a-different-target-fails-the-wire
  ;; the reader is handed a chosen target other than the one the writer wrote
  (let [o (observe :different)]
    (is (some? (:reader o)) "[:interventions :different :reader]")
    (is (not (w/received? o))
        "[:interventions :different] present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-chosen-target
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :chosen-target))
                    (tree-seq coll? seq r))
          (str path " carries no :chosen-target")))))

(deftest target-controls-token-identity-and-store-lookup
  (let [sl (:second-layer (fields))
        {:keys [tokens initial located criterion validated published]} (:products sl)
        targets (:targets sl)
        locator (:locator sl)
        [a b] (:clicks tokens)
        [fa fb] (:flights tokens)
        stated #(mapv :stated (vals (get-in % [:source :criteria-by-token])))
        [la lb] (:clicks located)
        token (:token criterion)]
    (is (true? (:flights-equal-modulo-target? sl)) "[:second-layer :flights-equal-modulo-target?]")
    (is (= targets (mapv :target (:flights tokens))) "[:second-layer :products :tokens :flights :target]")
    (is (= (dissoc fa :target) (dissoc fb :target))
        "Only chosen-target changed: path, text reader, store and all provenance are fixed.")
    (is (= 6 (count (:wants a)) (count (:wants b))) "[:second-layer :tokens :clicks :wants]")
    (is (= (set (stated a)) (set (stated b))) "[:second-layer :tokens :clicks :stated]")
    (is (= 6 (count (stated a))) "[:second-layer :tokens :clicks :stated count]")
    (is (nil? (some (set (:wants b)) (:wants a))) "[:second-layer :tokens :clicks :wants] disjoint")
    (is (= :valid (:status validated)) "[:second-layer :products :validated :status]")
    (is (= locator (get-in published [:locators token :locator])) "[:second-layer :products :published :locators]")
    (is (every? #(empty? (:locators %)) (:clicks initial)) "[:second-layer :products :initial :clicks :locators]")
    (is (= locator (get-in la [:locators token])) "[:second-layer :products :located :clicks :locators]")
    (is (empty? (:locators lb)) "[:second-layer :products :located :clicks 1 :locators]")
    (is (= [token] (get-in la [:source :machine-located])) "[:second-layer :products :located :machine-located]")
    (is (empty? (get-in lb [:source :machine-located])) "[:second-layer :products :located :machine-located 1]")
    (is (= (:wants (first (:clicks initial))) (:wants la)) "[:second-layer :wants initial->located]")
    (is (= (:wants (second (:clicks initial))) (:wants lb)) "[:second-layer :wants initial->located 2]")
    (println :target-tokens [(:wants a) (:wants b)]
             :machine-locators [(:locators la) (:locators lb)])))
