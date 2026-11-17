(ns futon3c.diagramprover.host-pairs-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.term-schema :as ts]))

(def r1 ts/beta) ; This rule contains no choose-specific pattern or rewrite.
(def guarded
  (assoc r1 :guards [[:not-choose-head
                     (fn [b] (loop [t (get b '?B)]
                               (if (= :lam (first t)) (recur (second t))
                                   (not= :choose (first t)))))] ]))
(def r2 {:name :family :pattern '[:app [:lam [:choose ?S]] ?A] :guards []
         :rewrite (fn [b] [:family-at [:lam [:choose (get b '?S)]] (get b '?A)])})
(def host [:app [:lam [:icc [:app [:lam [:choose [:app [:h] [:bvar 0]]]] [:bvar 0]]
                           [:app [:lam [:choose [:app [:h2] [:bvar 0]]]] [:bvar 0]]]] [:i]])
(defn observe [term]
  (letfn [(walk [t parent]
            (let [op (first t)
                  own {:keys (if (and (= op :choose) (not= parent :lam)) 1 0)
                       :unreduced (if (and (= op :app) (= :lam (first (second t)))) 1 0)}]
              (reduce #(merge-with + %1 %2) own
                      (if (= op :bvar) [] (map #(walk % op) (rest t))))))]
    (walk term nil)))
(defn run-host [schemas] (hp/host-pairs schemas host {:observe observe}))
(defn discovered [report]
  (filter #(and (= :non-joining (:status %)) (seq (:path %)) (seq (:position %))
                (= #{:beta :family} (set (:schemas %)))
                (= [:i] (last (:overlap %)))) (:pairs report)))

(deftest q87-pair-is-discovered-after-a-step
  (let [result (run-host [r1 r2])
        found (discovered result)
        seed-only (assoc result :pairs (filter #(empty? (:path %)) (:pairs result)))
        root-only (assoc result :pairs (filter #(empty? (:position %)) (:pairs result)))]
    (is (= :complete (:status result)))
    (is (seq found))
    (is (some #(= [:app [:lam [:choose [:app [:h] [:bvar 0]]]] [:i]] (:overlap %)) found))
    (is (every? #(= :created (:kind %)) found))
    (is (some #(= :different (:observation-comparison %)) found))
    (is (empty? (discovered seed-only)))
    (is (empty? (discovered root-only)))
    ;; Review note (claude-5): the conflict is already visible in the host, before
    ;; any step, as the family applied to the outer binder ([:bvar 0]). "After a
    ;; step" above holds only for the family applied at [:i]; the seed pair is the
    ;; same R1/R2 conflict, and it is still found by an R1 with no choose in it.
    (is (some #(and (= :non-joining (:status %)) (empty? (:path %))
                    (= #{:beta :family} (set (:schemas %)))
                    (= [:bvar 0] (last (:overlap %))))
              (:pairs result)))))

(deftest three-stages-and-mutations
  (let [a (run-host [r1]) b (run-host [guarded]) c (run-host [guarded r2])
        anywhere (assoc r1 :guards [[:no-choose-anywhere
                                     (fn [bindings]
                                       (not-any? #{:choose} (flatten (get bindings '?B))))]])
        broken (run-host [anywhere r2])]
    (doseq [report [a b c]]
      (is (= :complete (:status report)))
      (is (not-any? #(= :non-joining (:status %)) (:pairs report))))
    (is (some #(and (= [:beta :beta] (:schemas %)) (= :variable-nested (:kind %))
                    (= :joining (:status %))) (:pairs a)))
    (is (some #(= :variable-nested (:kind %)) (:pairs c)))
    (is (= [{:keys 2 :unreduced 0}] (:observations a)))
    (is (= [{:keys 0 :unreduced 2}] (:observations b)))
    (is (= [{:keys 0 :unreduced 0}] (:observations c)))
    (is (= [[:icc [:family-at [:lam [:choose [:app [:h] [:bvar 0]]]] [:i]]
                  [:family-at [:lam [:choose [:app [:h2] [:bvar 0]]]] [:i]]]] (:normal-forms c)))
    (is (= 3 (count (set (map :normal-forms [a b c])))))
    (is (= (:normal-forms a) (:normal-forms (run-host [(assoc guarded :guards [])]))))
    (is (not= (:normal-forms c) (:normal-forms broken)))
    (is (= (:observations (hp/host-pairs [r1] host {}))
           (:observations (hp/host-pairs [guarded] host {}))))))

(deftest bound-is-not-nonjoining
  (let [report (hp/host-pairs [r1 r2] host {:max-states 0 :observe observe})
        broken (update report :pairs #(mapv (fn [p] (assoc p :status :non-joining)) %))]
    (is (= :inconclusive (:status report)))
    (is (seq (:pairs report)))
    (is (every? #(= :inconclusive (:status %)) (:pairs report)))
    (is (not= (map :status (:pairs report)) (map :status (:pairs broken))))))

(deftest constructed-guard-escape
  ;; Constructed only: claude-7 r1r2-fidelity-2026-11-17.md, Guard escape,
  ;; reports zero such occurrences in 18,277 instrumented corpus beta steps.
  (let [term [:app [:lam [:icc [:choose [:app [:h] [:bvar 0]]] [:c]]] [:i]]
        result (hp/host-pairs [guarded r2] term {:observe observe})
        broken (hp/host-pairs [guarded r2] term {})]
    (is (empty? (:pairs result)))
    (is (= [{:keys 1 :unreduced 0}] (:observations result)))
    (is (not= (:observations result) (:observations broken)))))

(deftest identical-subtrees-are-rewritten-at-the-recorded-position
  (let [redex [:app [:lam [:spec [:bvar 0]]] [:i]]
        term [:pair redex redex]
        instances (ts/instances r1 term)]
    (is (= [[0] [1]] (mapv :position instances)))
    (is (= [[:pair [:spec [:i]] redex] [:pair redex [:spec [:i]]]]
           (mapv #(hp/apply-instance term %) instances)))
    (is (empty? (:pairs (hp/host-pairs [r1] term {}))))))
