(ns futon3c.diagramprover.causal.open-theory
  "Open causal theories and their composition by gluing.

  Fong's causal theories present a causal model as syntax: a DAG generates
  the free category of processes on its variables, and a model is a
  structure-preserving assignment of mechanisms. His decorated cospans make
  such systems OPEN: each carries a boundary, and two open systems compose by
  identifying their shared boundary (a pushout). This namespace is the
  finite, deterministic fragment of that idea that the rest of `causal/`
  can already compute with:

  - an open theory is {:id … :variables #{…} :arrows [{:from :to}]
    :mechanisms {var \"equation\"} :interface #{…}}, with equations in the
    Boolean grammar of `causal.scm`;
  - a variable with a mechanism is OWNED by that theory; one without is an
    input the theory expects some other theory, or the world, to supply;
  - `glue` composes theories along shared names. A name may be shared only
    if every theory that mentions it lists it in its interface, and at most
    one theory may own it. The result is a closed causal DAG with structural
    equations, accepted by `dag/validate` and `scm/validate`, so every
    receipt in `causal/` (d-separation, surgery, identification,
    counterfactuals) applies to the composite.

  What this buys: a design pattern can be written as a small open theory,
  and an incident as the gluing of the patterns that bear on it. The
  composite's testable implications are then predictions the patterns make
  jointly, which no single pattern's prose states.

  Gluing refuses, as data, rather than guessing: :undeclared-sharing (a name
  shared outside an interface), :double-mechanism (two owners), :cycle.
  Probability, combs and the Markov-categorical semantics are not here; see
  M-diagramprover §Generalisation, rung R3/D3."
  (:require [clojure.set :as set]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.scm :as scm]))

(defn- mentioned [{:keys [variables arrows mechanisms]}]
  (into (set variables) (concat (mapcat (juxt :from :to) arrows) (keys mechanisms))))

(defn- refusal [reason details] {:glued? false :reason reason :details details})

(defn glue
  "Compose open THEORIES into one closed theory, or return a typed refusal.
  On success: {:glued? true :dag <causal dag with structural equations>
  :owners {var theory-id} :inputs #{vars no theory owns}}."
  [theories]
  (let [by-var (reduce (fn [m t] (reduce #(update %1 %2 (fnil conj []) t) m (mentioned t)))
                       {} theories)
        undeclared (into (sorted-map)
                         (for [[v ts] by-var
                               :when (> (count ts) 1)
                               :let [bad (remove #(contains? (:interface %) v) ts)]
                               :when (seq bad)]
                           [v (mapv :id bad)]))
        owners (reduce (fn [m t] (reduce #(update %1 %2 (fnil conj []) (:id t)) m
                                         (keys (:mechanisms t))))
                       {} theories)
        doubled (into (sorted-map) (filter #(> (count (val %)) 1)) owners)]
    (cond
      (seq undeclared) (refusal :undeclared-sharing undeclared)
      (seq doubled) (refusal :double-mechanism doubled)
      :else
      (let [vars (apply set/union (map mentioned theories))
            causal-dag {:variables (into (sorted-map)
                                         (for [v vars] [v {:id v :kind :observed}]))
                        :arrows (vec (distinct (mapcat :arrows theories)))
                        :metadata {:structural_equations
                                   (into (sorted-map) (mapcat :mechanisms theories))
                                   :glued-from (mapv :id theories)}}]
        (try
          (dag/validate causal-dag)
          (scm/validate causal-dag)
          {:glued? true
           :dag causal-dag
           :owners (into (sorted-map) (map (fn [[v [id]]] [v id])) owners)
           :inputs (into (sorted-set) (remove owners) vars)}
          (catch clojure.lang.ExceptionInfo e
            (refusal (if (:cycle (ex-data e)) :cycle :mechanism-mismatch) (ex-data e))))))))

(defn theory
  "Build an open theory from mechanisms alone: arrows are read off the
  equations, so a mechanism and its arrows cannot disagree."
  [id mechanisms & {:keys [interface inputs]}]
  (let [arrows (vec (for [[v src] (sort mechanisms)
                          input (sort (scm/equation-inputs (scm/parse-equation src)))]
                      {:from input :to (keyword v)}))]
    {:id id
     :variables (into (set (map keyword (keys mechanisms))) (map keyword inputs))
     :arrows arrows
     :mechanisms (into {} (map (fn [[v src]] [(keyword v) src])) mechanisms)
     :interface (set interface)}))
