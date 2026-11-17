(ns futon3c.diagramprover.term-schema
  "First-order tree schemas instantiated as ordinary ground DiagramProver rules.
  Child positions are zero-based (excluding the operator). Bound-variable indices
  are labels, not child terms. No higher-order matching or Lean import is implied."
  (:require [clojure.string :as str]
            [futon3c.diagramprover.paper-choice :as paper]
            [futon3c.diagramprover.rule :as rule]))

(defn- bvar? [term] (= :bvar (first term)))

(defn term-graph
  "Wrap the existing tree encoder: [:bvar k] becomes a nullary hyperedge whose
  :value is [:bvar k]. Thus the existing matcher compares the index as well."
  [term]
  (letfn [(encode [t]
            (if (bvar? t)
              [t]
              (into [(first t)] (map encode (rest t)))))]
    (paper/term-graph (encode term))))

(defn- shift [term amount cutoff]
  (if (bvar? term)
    (let [k (second term)] [:bvar (if (>= k cutoff) (+ k amount) k)])
    (into [(first term)]
          (map #(shift % amount (if (= :lam (first term)) (inc cutoff) cutoff))
               (rest term)))))

(defn subst
  "Eliminate the outer binder of body, substituting argument for index zero.
  Beneath d lambdas, index d denotes that binder; smaller indices remain bound,
  larger indices lower by one, and the argument's free indices shift up by d."
  [body argument]
  (letfn [(walk [term depth]
            (if (bvar? term)
              (let [k (second term)]
                (cond (= k depth) (shift argument depth 0)
                      (> k depth) [:bvar (dec k)]
                      :else term))
              (into [(first term)]
                    (map #(walk % (if (= :lam (first term)) (inc depth) depth))
                         (rest term)))))]
    (walk body 0)))

(defn- variable? [p]
  (and (symbol? p) (str/starts-with? (name p) "?")))

(defn- match-term [pattern term binding]
  (cond
    (variable? pattern)
    (if (contains? binding pattern)
      (when (= (get binding pattern) term) binding)
      (assoc binding pattern term))

    (and (vector? pattern) (vector? term)
         (= (first pattern) (first term)) (= (count pattern) (count term)))
    (if (bvar? pattern)
      (when (= pattern term) binding)
      (reduce (fn [bindings [p t]]
                (if-let [matched (match-term p t bindings)] matched (reduced nil)))
              binding (map vector (rest pattern) (rest term))))
    :else nil))

(defn- positions [term path]
  (cons [path term]
        (when-not (bvar? term)
          (mapcat (fn [[i child]] (positions child (conj path i)))
                  (map-indexed vector (rest term))))))

(defn- candidates [schema host]
  (for [[position term] (positions host [])
        :let [binding (match-term (:pattern schema) term {})]
        :when (some? binding)]
    (merge {:name (:name schema) :schema schema :binding binding
            :position position :term term}
           (reduce (fn [result [guard-name guard]]
                     (if (guard binding)
                       (update result :guards-passed conj guard-name)
                       (reduced (assoc result :guard-failed guard-name))))
                   {:guards-passed []} (:guards schema)))))

(defn instances
  "Return ground rules in preorder, one per matching position with passing guards.
  :schema retains the original schema map. :position records where it was found;
  the ground rule itself can match any identical subtree via cp/applications."
  [schema host]
  (mapv (fn [{:keys [term binding] :as candidate}]
          (rule/make-rule (term-graph term)
                          (term-graph ((:rewrite schema) binding))
                          (dissoc candidate :term)))
        (remove #(contains? % :guard-failed) (candidates schema host))))

(defn blocked
  "Return pattern matches refused by guards, with :guard-failed naming the first
  failing guard in declared order. A pattern mismatch is not a blocked match."
  [schema host]
  (mapv #(dissoc % :term)
        (filter #(contains? % :guard-failed) (candidates schema host))))

(def beta
  {:name :beta
   :pattern '[:app [:lam ?B] ?A]
   :guards []
   :rewrite (fn [binding] (subst (get binding '?B) (get binding '?A)))})
