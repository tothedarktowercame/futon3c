(ns futon3c.diagramprover.strategy
  "Deterministic tree strategy and finite sensitivity evidence, not a confluence
  or causal attribution proof. Priority ties use schema declaration order."
  (:require [futon3c.diagramprover.term-schema :as ts]))

(defn- ordered-instances [schemas term traversal]
  (let [is (mapcat #(ts/instances % term) schemas)
        ;; Vector comparison is length-first in Clojure; compare path sequences
        ;; explicitly so outer-first is preorder, not breadth-first.
        compare-path (fn compare-path [a b]
                       (cond
                         (empty? a) (if (empty? b) 0 (if (= traversal :inner-first) 1 -1))
                         (empty? b) (if (= traversal :inner-first) -1 1)
                         (= (first a) (first b)) (compare-path (rest a) (rest b))
                         :else (compare (first a) (first b))))]
    (sort (fn [a b]
            (let [p (compare-path (:position a) (:position b))]
              (if (zero? p)
                (compare (:priority (:schema b) 0) (:priority (:schema a) 0)) p))) is)))

(defn normalise-strategy
  "Fire at the first eligible position in preorder (:outer-first) or postorder
  (:inner-first), left-to-right; highest priority wins there. max-states bounds
  rewrite steps. Return :complete with :normal-form, or :inconclusive with :term
  and :reason (:bound/:cycle). :blocked retains guard refusals on visited states."
  [schemas host {:keys [traversal max-states observe]
                 :or {traversal :outer-first max-states 100}}]
  (when-not (#{:outer-first :inner-first} traversal)
    (throw (ex-info "Unknown traversal" {:traversal traversal})))
  (when-not (and (integer? max-states) (<= 0 max-states))
    (throw (ex-info "Invalid bound" {:max-states max-states})))
  (loop [term host path [] seen #{} blocked []]
    (let [refusals (mapcat #(ts/blocked % term) schemas)
          blocked (into blocked (map #(assoc % :term term :step (count path)) refusals))
          instance (first (ordered-instances schemas term traversal))]
      (cond
        (nil? instance) {:status :complete :normal-form term :path path :blocked blocked
                         :observation (when observe (observe term))}
        (or (contains? seen term) (>= (count path) max-states))
        {:status :inconclusive :term term :path path :blocked blocked
         :reason (if (contains? seen term) :cycle :bound)}
        :else
        (let [replacement ((:rewrite (:schema instance)) (:binding instance))
              position (:position instance)
              next (if (empty? position) replacement (assoc-in term (mapv inc position) replacement))]
          (recur next
                 (conj path {:schema (:name instance) :position position
                             :binding (:binding instance) :guards-passed (:guards-passed instance)
                             :from term :to next})
                 (conj seen term) blocked))))))

(defn- changed? [base variant]
  (when (and (= :complete (:status base)) (= :complete (:status variant)))
    (not= (select-keys base [:normal-form :observation])
          (select-keys variant [:normal-form :observation]))))

(defn settled-by
  "Sensitivity of the full host's strategy result for a non-joining pair.
  Swap the two named schemas' priorities, flip traversal, and drop each eligible
  guard separately. A guard is eligible if its schema loses at the pair state on
  the baseline path, or that named guard blocks a pair schema on that path.
  Blocked receipts are evidence even if removing the guard changes no result.
  Return every applicable label, variants and baseline; :nothing only when all
  probes complete and none apply. Bounds yield :inconclusive instead.

  Crude limits: names must be unique; probes act globally, one change at a time;
  result sensitivity can include effects beyond the particular pair. This does
  not establish that a guard/priority is necessary on every input."
  [schemas host pair opts]
  (when-not (= :non-joining (:status pair))
    (throw (ex-info "settled-by requires a non-joining pair" {:status (:status pair)})))
  (when-not (= (count schemas) (count (set (map :name schemas))))
    (throw (ex-info "Schema names must be unique" {})))
  (let [names (set (:schemas pair))
        baseline (normalise-strategy schemas host opts)
        pair-state (or (:to (last (:path pair))) host)
        winner (:schema (first (filter #(= pair-state (:from %)) (:path baseline))))
        losers (if winner (disj names winner) #{})
        blocked (filter #(contains? names (:name %)) (:blocked baseline))
        guard-probes
        (for [s schemas :when (contains? names (:name s))
              [g _] (:guards s)
              :let [receipts (vec (filter #(and (= (:name s) (:name %))
                                               (= g (:guard-failed %))) blocked))]
              :when (or (contains? losers (:name s)) (seq receipts))
              :let [variant-schemas (mapv #(if (= (:name s) (:name %))
                                            (update % :guards (fn [gs] (vec (remove (fn [[n _]] (= n g)) gs))))
                                            %) schemas)
                    result (normalise-strategy variant-schemas host opts)]]
          {:kind :guard :schema (:name s) :guard g :blocked receipts :result result
           :changed? (changed? baseline result)})
        [a b] (:schemas pair)
        priorities (into {} (map (fn [s] [(:name s) (:priority s 0)]) schemas))
        swapped (mapv #(cond (= a (:name %)) (assoc % :priority (get priorities b))
                             (= b (:name %)) (assoc % :priority (get priorities a))
                             :else %) schemas)
        flipped (assoc opts :traversal (if (= :inner-first (:traversal opts)) :outer-first :inner-first))
        probes (into (vec guard-probes)
                     (for [[kind ss oo] [[:priority swapped opts] [:traversal schemas flipped]]
                           :let [result (normalise-strategy ss host oo)]]
                       {:kind kind :result result :changed? (changed? baseline result)}))
        labels (set (map :kind (filter #(or (:changed? %) (seq (:blocked %))) probes)))
        complete? (every? #(= :complete (:status %)) (cons baseline (map :result probes)))]
    {:status (if complete? :complete :inconclusive)
     :settled-by (cond-> labels (and complete? (empty? labels)) (conj :nothing))
     :baseline baseline :variants probes}))
