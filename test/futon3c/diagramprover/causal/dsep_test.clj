(ns futon3c.diagramprover.causal.dsep-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.diagram :as diagram]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.surgery :as surgery]))

(defn motif [arrows]
  {:variables (into {} (map (fn [id] [id {:id id}])
                            (set (mapcat (juxt :from :to) arrows))))
   :arrows arrows :leak-edges [] :interventions [] :sensors [] :metadata {}})

(deftest canonical-three-node-motifs
  (testing "chain"
    (let [g (motif [{:from :a :to :b} {:from :b :to :c}])]
      (is (dsep/d-connected? g :a :c #{}))
      (is (dsep/d-separated? g :a :c #{:b}))))
  (testing "fork"
    (let [g (motif [{:from :b :to :a} {:from :b :to :c}])]
      (is (dsep/d-connected? g :a :c #{}))
      (is (dsep/d-separated? g :a :c #{:b}))))
  (testing "collider and a conditioned descendant"
    (let [g (motif [{:from :a :to :b} {:from :c :to :b}
                    {:from :b :to :d}])]
      (is (dsep/d-separated? g :a :c #{}))
      (is (dsep/d-connected? g :a :c #{:b}))
      (is (dsep/d-connected? g :a :c #{:d}))
      (is (= [[:a :b :c]]
             (:paths (dsep/connecting-paths g :a :c #{:d} {:limit 1})))))))

(deftest backdoor-and-surgery
  (let [g (motif [{:from :u :to :x} {:from :u :to :y}
                  {:from :x :to :y}])
        intervened (surgery/do-intervention g :x)]
    (is (not (dsep/backdoor-adjustment? g :x :y #{})))
    (is (dsep/backdoor-adjustment? g :x :y #{:u}))
    (is (empty? (dag/parents intervened :x)))
    (is (dsep/d-separated? intervened :u :x #{}))
    (is (diagram/canonical? (diagram/dag->diagram intervened)))))

(deftest leak-connectivity
  (let [base (dag/load-spec "docs/memory-causal-graph-spec.json")
        leaked (dag/with-leaks base)
        target (:to (first (:leak-edges base)))]
    (is (dsep/d-connected? leaked :L1 target #{}))
    (is (not (contains? (:variables (dag/without-leaks leaked)) :L1)))
    (is (= base (dag/without-leaks leaked)))))

(deftest bounded-implied-independencies
  (let [collider (motif [{:from :a :to :b} {:from :c :to :b}])
        implications (dsep/implied-independencies collider
                                                   {:max-conditioning 2})]
    (is (= [{:x :a :y :c :given #{}}] implications)))
  (let [memory (dag/load-spec "docs/memory-causal-graph-spec.json")
        implications (dsep/implied-independencies memory
                                                   {:max-conditioning 2})]
    (is (= 209 (count implications)))
    (is (some #(= {:x :V06 :y :V07 :given #{}} %) implications))))
