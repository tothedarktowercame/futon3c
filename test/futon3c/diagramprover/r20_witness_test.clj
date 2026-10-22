(ns futon3c.diagramprover.r20-witness-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.r20-witness :as witness]))

(def map-path "holes/labs/M-wm-wiring/r20-field-flow.edn")

;; X1's edge table, exported from DarkTower.WarMachine.R20Witness.
(def edge-path witness/default-edges)

(defn- real-input []
  (let [spec (witness/read-edn map-path)
        findings (witness/report (witness/materialise spec) spec)]
    [spec (witness/read-edn edge-path) findings]))

(defn- edge-hops [witnesses edge]
  (:hops (first (filter #(= edge (:edge %)) witnesses))))

(deftest r20-precision-chains-follow-the-map-and-its-confirmation-frontier
  (let [[spec edges findings] (real-input)
        witnesses (witness/witness-edges spec edges findings)]
    (testing "F is a direct confirmed retrospective-f to advance hop"
      (is (= [[{:writer "futon2/src/futon2/aif/policy_precision_carry.clj#retrospective-f"
                :field ":f"
                :record ":precision-retrospective"
                :field-type "quantity/free-energy"
                :reader "futon2/src/futon2/aif/policy_precision_carry.clj#advance"
                :confirmed true
                :sha "b6d557aa073ca52939f64c0267754bf8286d84ed"}]]
             (edge-hops witnesses :inferenceToPrecisionF))))
    (testing "G and habit pass through the family hops, confirmed since S4e"
      (doseq [edge [:selectionToPrecisionG :selectionToPrecisionHabit]]
        (let [chain (first (edge-hops witnesses edge))]
          (is (= ["futon2/src/futon2/aif/policy.clj#selection-candidate"
                  "futon2/src/futon2/aif/policy_precision_carry.clj#family"]
                 (mapv :writer chain)))
          (is (= [true true] (mapv :confirmed chain))))))))

(deftest a-second-g-writer-is-preserved-as-ambiguity
  (let [[spec edges findings] (real-input)
        extra {:box/id :another-selection-candidate
               :box/kind :producer
               :site {:file "futon2/src/futon2/aif/policy.clj" :var "selection-candidate"}
               :returns-record :selection-candidate
               :writes [[:g {:record :selection-candidate}]]}
        witnesses (witness/witness-edges (update spec :boxes conj extra) edges findings)]
    (is (= 2 (count (edge-hops witnesses :selectionToPrecisionG)))
        "the emitter does not choose between two source hops")))

(deftest scorer-read-at-advance-cannot-become-a-g-chain
  (let [[spec edges findings] (real-input)
        scorer [:scoring {:record :selection-certificate}]
        changed (update spec :boxes
                        (fn [boxes]
                          (mapv #(if (= :precision-carry-advance (:box/id %))
                                   (update % :reads conj scorer)
                                   %)
                                boxes)))
        witnesses (witness/witness-edges changed edges findings)]
    (is (= 1 (count (edge-hops witnesses :selectionToPrecisionG))))
    (is (= "quantity/expected-free-energy"
           (:field-type (first (first (edge-hops witnesses :selectionToPrecisionG))))))
    (is (not-any? #(= ":scoring" (:field %))
                  (first (edge-hops witnesses :selectionToPrecisionG))))))

(deftest emit-lean-has-the-r20witness-term-shape
  (let [spec {:repos {"futon2" "pin"}
              :r20/field-types {[:f {:record :r}] :quantity/free-energy}
              :boxes [{:box/id :writer :site {:file "f.clj" :var "w"}
                       :writes [[:f {:record :r}]]}
                      {:box/id :reader :site {:file "f.clj" :var "r"}
                       :reads [[:f {:record :r}]]}]}
        edge-spec {:edges [{:edge :inferenceToPrecisionF
                            :quantity :quantity/free-energy :carriers []
                            :source-sites ["f.clj#w"] :target-sites ["f.clj#r"]}]}
        text (witness/emit-lean spec
                                (witness/witness-edges spec edge-spec []))]
    (is (= (str "import DarkTower.WarMachine.R20Witness\n\n"
                "namespace DarkTower.WarMachine.R20Witness.Generated\n"
                "open DarkTower.WarMachine.R20Witness\n"
                "open DarkTower.WarMachine.R20Graph\n\n"
                "def futon2Witness : Witness :=\n"
                "  { sha := \"pin\"\n"
                "    edges := [\n"
                "    ⟨.inferenceToPrecisionF, [{ writer := \"f.clj#w\", field := \":f\", record := \":r\", fieldType := \"quantity/free-energy\", reader := \"f.clj#r\", confirmed := true, sha := \"pin\" }]⟩] }\n\n"
                "#eval verdicts futon2Witness\n\n"
                "end DarkTower.WarMachine.R20Witness.Generated\n")
           text))))

(deftest an-unreadable-site-confirms-nothing
  (let [[spec edges findings] (real-input)
        f-site {:file "futon2/src/futon2/aif/policy_precision_carry.clj" :var "retrospective-f"}
        findings (conj findings {:finding :var-not-found :site f-site :var "retrospective-f"})
        witnesses (witness/witness-edges spec edges findings)]
    (is (= [false] (mapv :confirmed (first (edge-hops witnesses :inferenceToPrecisionF)))))))

(deftest a-method-site-is-named-by-its-multimethod
  (is (= "m.clj#evaluate"
         (witness/site {:site {:file "m.clj" :var "evaluate :exact-enumeration"}})))
  (is (= "m.clj#evaluate" (witness/site {:site {:file "m.clj" :var "evaluate"}}))))
