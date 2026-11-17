(ns futon3c.diagramprover.host-pairs
  "Unordered, position-specific schema exploration on ordered term trees.
  Tree equality is canonical boundary-preserving graph isomorphism for this
  encoding (ordered children, no sharing, and bvar indices in edge labels)."
  (:require [futon3c.diagramprover.term-schema :as ts]
            [futon3c.diagramprover.strategy :as strategy]))

(defn- subterm [term position] (get-in term (mapv inc position)))

(defn apply-instance
  "Apply only at the recorded child-index position, never at another equal tree."
  [term instance]
  (let [replacement ((:rewrite (:schema instance)) (:binding instance))
        position (:position instance)]
    (if (empty? position) replacement
        (assoc-in term (mapv inc position) replacement))))

(defn- instances [schemas term]
  (vec (mapcat #(ts/instances % term) schemas)))

(defn- step [term instance result]
  {:schema (:name instance) :binding (:binding instance)
   :position (:position instance) :guards-passed (:guards-passed instance)
   :from term :to result})

(defn- explore [schemas initial limit observe]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY {:term initial :path []})
         seen #{} states [] normals []]
    (cond
      (empty? queue) {:status :complete :states states :normal-forms normals
                     :observations (if observe (mapv observe normals) [])}
      (contains? seen (:term (peek queue)))
      (recur (pop queue) seen states normals)
      (>= (count seen) limit)
      {:status :inconclusive :states states :normal-forms normals
       :observations (if observe (mapv observe normals) [])}
      :else
      (let [{:keys [term path] :as state} (peek queue)
            choices (instances schemas term)
            successors (mapv (fn [i]
                               (let [next (apply-instance term i)]
                                 {:term next :path (conj path (step term i next))})) choices)]
        (recur (into (pop queue) successors) (conj seen term)
               (conj states state) (cond-> normals (empty? choices) (conj term)))))))

(defn- prefix? [a b] (= a (vec (take (count a) b))))

(defn- nested-kind [outer inner]
  (let [relative (drop (count (:position outer)) (:position inner))]
    (loop [pattern (:pattern (:schema outer)) path relative]
      (cond
        (symbol? pattern) :variable-nested
        (empty? path) :pattern-nested
        :else (recur (nth pattern (inc (first path))) (rest path))))))

(defn- signature [term i] [(:schema i) (subterm term (:position i))])

(defn- pair-report [term path a b initial-signatures normalise observe]
  (let [[outer inner] (if (prefix? (:position a) (:position b)) [a b] [b a])
        results (mapv #(apply-instance term %) [a b])
        branches (mapv normalise results)
        reachable (mapv #(set (map :term (:states %))) branches)
        joins? (some (second reachable) (first reachable))
        complete? (every? #(= :complete (:status %)) branches)
        kind (cond
               (and (seq path) (not (contains? initial-signatures (signature term inner)))) :created
               (= (:position a) (:position b)) :root
               :else (nested-kind outer inner))]
    {:schemas (mapv :name [a b]) :instances [a b]
     :overlap (subterm term (:position outer)) :position (:position outer)
     :path path :kind kind :results results :normalisations branches
     :status (cond joins? :joining complete? :non-joining :else :inconclusive)
     :observation-comparison
     (cond (nil? observe) :not-requested
           (not complete?) :inconclusive
           (= (set (:observations (first branches)))
              (set (:observations (second branches)))) :equal
           :else :different)}))

(defn host-pairs
  "Explore all unordered instances and report equal/nested-position pairs,
  including same-schema pairs. Return host normal forms and observations even
  without pairs. max-states bounds each exploration independently; incomplete
  branches cannot establish non-joining. At zero, inspect seed pairs only and
  report inconclusive (no successor exploration). :created means the inner
  instance's schema/subterm signature was absent initially, irrespective of its
  old position; path records the first BFS witness. With :settled-by? true, non-joining pairs
  also carry strategy sensitivity evidence. No report merging."
  [schemas host {:keys [max-states observe settled-by?] :or {max-states 100} :as opts}]
  (when-not (and (integer? max-states) (<= 0 max-states))
    (throw (ex-info "max-states must be a nonnegative integer" {:max-states max-states})))
  (let [normalise (memoize #(explore schemas % max-states observe))
        exploration (normalise host)
        seed-instances (instances schemas host)
        signatures (set (map #(signature host %) seed-instances))
        states (if (zero? max-states) [{:term host :path []}] (:states exploration))]
    (assoc exploration :pairs
           (vec (mapcat
                 (fn [{:keys [term path]}]
                   (let [is (instances schemas term)]
                     (for [i (range (count is)) j (range (inc i) (count is))
                           :let [a (nth is i) b (nth is j)]
                           :when (or (prefix? (:position a) (:position b))
                                     (prefix? (:position b) (:position a)))]
                       (let [report (pair-report term path a b signatures normalise observe)]
                         (cond-> report
                           (and settled-by? (= :non-joining (:status report)))
                           (assoc :settled-by (strategy/settled-by schemas host report opts))))))) states)))))
