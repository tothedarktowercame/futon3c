(ns futon3c.diagramprover.term-schema-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.term-schema :as ts]
            [futon3c.diagramprover.critical-pairs :as cp]
            [futon3c.diagramprover.matcher :as matcher]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.strategy :as strategy]
            [futon3c.diagramprover.lean-import :as li]))

(def simple [:app [:lam [:spec [:bvar 0]]] [:i]])
(def nested [:icc simple [:c]])

(defn- rewritten? [schema host expected]
  (let [instances (ts/instances schema host)
        results (mapcat #(cp/applications % (ts/term-graph host)) instances)]
    (and (= 1 (count instances)) (= 1 (count results))
         (some? (matcher/find-iso (:graph (first results)) (ts/term-graph expected))))))

(deftest beta-ground-rule-applies
  (is (rewritten? ts/beta simple [:spec [:i]]))
  (let [r (first (ts/instances ts/beta simple))]
    (is (= :beta (:name r)))
    (is (= ts/beta (:schema r)))
    (is (= {'?B [:spec [:bvar 0]] '?A [:i]} (:binding r)))
    (is (= [] (:position r)))
    (is (= [] (:guards-passed r)))))

(deftest duplication-discard-and-free-index-lowering
  (is (= [:pair [:i] [:i]] (ts/subst [:pair [:bvar 0] [:bvar 0]] [:i])))
  (is (= [:c] (ts/subst [:c] [:i])))
  (is (= [:bvar 1] (ts/subst [:bvar 2] [:i])))
  (is (rewritten? ts/beta [:app [:lam [:pair [:bvar 0] [:bvar 0]]] [:i]]
                  [:pair [:i] [:i]]))
  (is (rewritten? ts/beta [:app [:lam [:c]] [:i]] [:c])))

(deftest substitution-avoids-capture
  (is (rewritten? ts/beta
                  [:app [:lam [:lam [:pair [:bvar 0] [:bvar 1]]]] [:i]]
                  [:lam [:pair [:bvar 0] [:i]]]))
  (is (= [:lam [:pair [:bvar 0] [:bvar 1]]]
         (ts/subst [:lam [:pair [:bvar 0] [:bvar 1]]] [:bvar 0])))
  (is (= [:lam [:lam [:pair [:bvar 0] [:bvar 2]]]]
         (ts/subst [:lam [:bvar 1]] [:lam [:pair [:bvar 0] [:bvar 1]]])))
  (is (= [:lam [:bvar 1]] (ts/subst [:lam [:bvar 2]] [:i]))))

(deftest below-root-through-existing-dpo
  (is (= [[0]] (mapv :position (ts/instances ts/beta nested))))
  (is (rewritten? ts/beta nested [:icc [:spec [:i]] [:c]]))
  (is (= [[0] [1]] (mapv :position (ts/instances ts/beta [:pair simple simple])))))

(def guarded
  (assoc ts/beta :guards
         [[:not-choose-head
           (fn [binding]
             (loop [body (get binding '?B)]
               (if (= :lam (first body)) (recur (second body))
                   (not= :choose (first body)))))] ]))

(deftest named-guard-refusals
  (doseq [body [[:choose [:h [:bvar 0]]] [:lam [:choose [:h [:bvar 1]]]]]]
    (let [host [:app [:lam body] [:i]]]
      (is (empty? (ts/instances guarded host)))
      (is (= [{:position [] :guard-failed :not-choose-head}]
             (mapv #(select-keys % [:position :guard-failed]) (ts/blocked guarded host))))))
  (is (rewritten? guarded simple [:spec [:i]]))
  (is (= [:not-choose-head] (:guards-passed (first (ts/instances guarded simple)))))
  (is (empty? (ts/blocked guarded simple))))

(deftest repeated-variables-and-bvar-labels
  (let [schema {:name :same :pattern '[:pair ?X ?X] :guards [] :rewrite #(get % '?X)}]
    (is (= 1 (count (ts/instances schema [:pair [:bvar 0] [:bvar 0]]))))
    (is (empty? (ts/instances schema [:pair [:bvar 0] [:bvar 1]]))))
  (is (nil? (matcher/find-iso (ts/term-graph [:bvar 0]) (ts/term-graph [:bvar 1]))))
  (let [schema {:name :zero :pattern [:bvar 0] :guards [] :rewrite (constantly [:i])}]
    (is (= [[0]] (mapv :position (ts/instances schema [:pair [:bvar 0] [:bvar 1]]))))))

(deftest broken-variants-are-distinguished
  (letfn [(unshifted [body argument depth]
            (if (= :bvar (first body))
              (let [k (second body)]
                (cond (= k depth) argument
                      (> k depth) [:bvar (dec k)]
                      :else body))
              (into [(first body)]
                    (map #(unshifted % argument (if (= :lam (first body)) (inc depth) depth))
                         (rest body)))))]
    (let [body [:lam [:pair [:bvar 0] [:bvar 1]]]
          expected [:lam [:pair [:bvar 0] [:bvar 1]]]]
      (is (= expected (ts/subst body [:bvar 0])))
      (is (not= expected (unshifted body [:bvar 0] 0)))))
  (let [root-only (fn [schema host] (filter #(empty? (:position %)) (ts/instances schema host)))]
    (is (empty? (root-only ts/beta nested)))
    (is (= 1 (count (ts/instances ts/beta nested))))))

(defn- old-subst
  "Mutation: pre-fix shift/subst, with the same index arithmetic."
  [body argument]
  (letfn [(shift [term amount cutoff]
            (if (= :bvar (first term))
              (let [k (second term)] [:bvar (if (>= k cutoff) (+ k amount) k)])
              (into [(first term)]
                    (map #(shift % amount (if (= :lam (first term)) (inc cutoff) cutoff))
                         (rest term)))))
          (walk [term depth]
            (if (= :bvar (first term))
              (let [k (second term)]
                (cond (= k depth) (shift argument depth 0)
                      (> k depth) [:bvar (dec k)]
                      :else term))
              (into [(first term)]
                    (map #(walk % (if (= :lam (first term)) (inc depth) depth))
                         (rest term)))))]
    (walk body 0)))

(deftest beta-preserves-body-and-copied-argument-metadata
  (let [untouched (with-meta [:constant] {:proof? false :node-id "body-child"})
        variable (with-meta [:bvar 0] {:proof? false :node-id "replaced-variable"})
        body (with-meta [:triple untouched variable variable]
               {:proof? false :prop? :unknown :binder-name "fixture" :node-id "body"})
        argument (with-meta [:argument] {:proof? true :arg-info 1 :node-id "argument"})
        term [:app [:lam body] argument]
        instance (first (ts/instances ts/beta term))
        result (hp/apply-instance term instance)
        strategy-result (:normal-form (strategy/normalise-strategy [ts/beta] term {}))
        broken (old-subst body argument)]
    (is (= result broken strategy-result)) ; metadata never changes structural equality
    (is (= (meta body) (meta result) (meta strategy-result)))
    (is (= false (li/proof? (nth result 1))))
    (is (= (meta untouched) (meta (nth result 1))))
    (doseq [i [2 3]]
      (is (= true (li/proof? (nth result i))))
      (is (= (meta argument) (meta (nth result i))))
      (is (not= (meta variable) (meta (nth result i)))))
    (is (= :unknown (li/proof? (nth broken 1))))
    (is (not= (li/proof? (nth result 1)) (li/proof? (nth broken 1))))))

(deftest shifting-and-lowering-preserve-node-metadata
  (let [free (with-meta [:bvar 0] {:proof? false :node-id "argument-free"})
        argument (with-meta [:wrap free] {:proof? true :node-id "argument-root"})
        lowered (with-meta [:bvar 2] {:proof? false :node-id "body-free"})
        body (with-meta [:lam [:pair [:bvar 1] lowered]] {:proof? false :binder-info 0})
        result (ts/subst body argument)
        broken (old-subst body argument)]
    (is (= [:lam [:pair [:wrap [:bvar 1]] [:bvar 1]]] result broken))
    (is (= (meta body) (meta result)))
    (is (= (meta argument) (meta (get-in result [1 1]))))
    (is (= (meta free) (meta (get-in result [1 1 1]))))
    (is (= (meta lowered) (meta (get-in result [1 2]))))
    (is (= false (li/proof? (get-in result [1 1 1]))))
    (is (= :unknown (li/proof? (get-in broken [1 1 1]))))
    (is (not= (li/proof? (get-in result [1 1 1])) (li/proof? (get-in broken [1 1 1]))))))

(deftest rewrite-root-metadata-belongs-to-the-rewrite-output
  (let [term (with-meta [:old] {:proof? true :node-id "old-root"})
        fresh {:name :fresh :pattern [:old] :guards [] :rewrite (constantly [:new])}
        explicit (assoc fresh :rewrite (constantly (with-meta [:new] {:proof? false :node-id "new-root"})))]
    (doseq [schema [fresh explicit]]
      (let [instance (first (ts/instances schema term))
            direct (hp/apply-instance term instance)
            normal (:normal-form (strategy/normalise-strategy [schema] term {}))]
        (is (= (meta direct) (meta normal)))
        (is (not= (meta term) (meta direct)))))
    (is (= :unknown (li/proof? (:normal-form (strategy/normalise-strategy [fresh] term {})))))
    (is (= false (li/proof? (:normal-form (strategy/normalise-strategy [explicit] term {})))))))
