(ns futon3c.diagramprover.strategy-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.strategy :as strategy]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.host-pairs-test :as q87]))

;; Description-shaped fixtures, not imported from the Python printer.
(def host [:orelim [:h] [:lam [:P]] [:lam [:orelim [:bvar 0] [:lam [:Q]] [:lam [:R]]]]])
(def invalid-host (assoc-in host [3 1 1] [:k]))
(def a {:name :merge :priority 10
        :pattern '[:orelim ?h [:lam ?P] [:lam [:orelim ?s [:lam ?Q] [:lam ?R]]]]
        :guards [[:right-disjunct-is-bound-var #(= [:bvar 0] (get % '?s))]
                 ;; Fixed pattern already supplies three branches; guard names
                 ;; that premise, without pretending to have Lean arity metadata.
                 [:arity-matches #(every? (fn [k] (contains? % k)) '[?P ?Q ?R])]]
        :rewrite (fn [b] (into [:split3] (map b '[?h ?P ?Q ?R])))})
(def b {:name :nested :priority 0
        :pattern '[:orelim ?s [:lam ?X] [:lam ?Y]] :guards []
        :rewrite (fn [v] (into [:split2] (map v '[?s ?X ?Y])))})
(def merged [:split3 [:h] [:P] [:Q] [:R]])
(def nested [:split2 [:h] [:P] [:split2 [:bvar 0] [:Q] [:R]]])
(defn observe [term]
  (let [op (first term)]
    (into (cond (= :split3 op) [{:arity 3}]
                (= :split2 op) [{:arity 2 :second-branch-split? (boolean (#{:split2 :split3} (first (nth term 3))))}]
                :else [])
          (if (= :bvar op) [] (mapcat observe (rest term))))))
(defn ab-pairs [result]
  (filter #(and (= :non-joining (:status %)) (= #{:merge :nested} (set (:schemas %)))) (:pairs result)))

(deftest unordered-or-conflicts
  (let [pairs (ab-pairs (hp/host-pairs [a b] host {:observe observe}))]
    (is (some #(= :root (:kind %)) pairs))
    (is (some #(= :pattern-nested (:kind %)) pairs))
    (is (every? #(= :different (:observation-comparison %)) pairs))))

(deftest strategy-and-all-settling-influences
  (let [opts {:observe observe :settled-by? true}
        pairs (ab-pairs (hp/host-pairs [a b] host opts))]
    (is (= merged (:normal-form (strategy/normalise-strategy [a b] host opts))))
    (is (= nested (:normal-form (strategy/normalise-strategy [a b] host {:traversal :inner-first}))))
    (is (seq pairs))
    (doseq [pair pairs]
      (is (= #{:priority :traversal} (get-in pair [:settled-by :settled-by])))
      (is (not (contains? (get-in pair [:settled-by :settled-by]) :guard))))
    (is (every? #(not (contains? % :settled-by)) (:pairs (hp/host-pairs [a b] host {}))))
    (is (= nested (:normal-form (strategy/normalise-strategy [b] host {}))))))

(deftest invalid-scrutinee-and-guard-removal-mutation
  (let [without (update a :guards #(vec (remove (fn [[n _]] (= :right-disjunct-is-bound-var n)) %)))
        good (strategy/normalise-strategy [a b] invalid-host {})
        bad (strategy/normalise-strategy [without b] invalid-host {})]
    (is (= [:split2 [:h] [:P] [:split2 [:k] [:Q] [:R]]] (:normal-form good)))
    (is (= merged (:normal-form bad)))
    (is (not= (:normal-form good) (:normal-form bad)))
    (is (empty? (ab-pairs (hp/host-pairs [a b] invalid-host {}))))))

(deftest q87-strategy-regression
  (let [r2 (assoc q87/r2 :priority 10)
        result (strategy/normalise-strategy [q87/guarded r2] q87/host {:observe q87/observe})
        expected (:normal-forms (q87/run-host [q87/guarded r2]))
        pair (first (q87/discovered (hp/host-pairs [q87/r1 r2] q87/host {:observe q87/observe})))
        evidence (strategy/settled-by [q87/r1 r2] q87/host pair {:observe q87/observe})]
    (is (= [(:normal-form result)] expected))
    (is (= {:keys 0 :unreduced 0} (:observation result)))
    (is (contains? (:settled-by evidence) :priority))
    (is (not (contains? (:settled-by evidence) :nothing)))))

(deftest guard-can-block-a-live-pair-side-later-on-the-strategy-path
  (let [left {:name :left :priority 20 :pattern '[:f ?x]
              :guards [[:not-blocked #(not= [:blocked] (get % '?x))]]
              :rewrite (constantly [:left-result])}
        right {:name :right :priority 10 :pattern '[:f ?x] :guards []
               :rewrite (constantly [:right-result])}
        advance {:name :advance :priority 30 :pattern [:f [:start]] :guards []
                 :rewrite (constantly [:f [:blocked]])}
        schemas [left right advance]
        term [:f [:start]]
        pair (first (filter #(= [:left :right] (:schemas %)) (:pairs (hp/host-pairs schemas term {}))))
        evidence (strategy/settled-by schemas term pair {})
        probe (first (filter #(= :guard (:kind %)) (:variants evidence)))]
    (is (= :non-joining (:status pair)))
    (is (= #{:guard} (:settled-by evidence)))
    (is (= [:right-result] (get-in evidence [:baseline :normal-form])))
    (is (= [:left-result] (get-in probe [:result :normal-form])))
    (is (seq (:blocked probe)))))

(deftest bounded-and-cyclic-runs-do-not-claim-normal-forms
  (let [loop-rule {:name :loop :pattern [:x] :guards [] :rewrite (constantly [:x])}
        pair (first (ab-pairs (hp/host-pairs [a b] host {})))]
    (is (= :bound (:reason (strategy/normalise-strategy [a b] host {:max-states 0}))))
    (is (= :cycle (:reason (strategy/normalise-strategy [loop-rule] [:x] {}))))
    (let [evidence (strategy/settled-by [a b] host pair {:max-states 0})]
      (is (= :inconclusive (:status evidence)))
      (is (not (contains? (:settled-by evidence) :nothing))))))
