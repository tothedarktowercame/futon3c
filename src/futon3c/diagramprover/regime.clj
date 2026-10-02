(ns futon3c.diagramprover.regime
  "Typing laws for string diagrams whose wires are of different kinds.

  The plain kernel (`graph`) treats every wire alike, and the MPZ kernel
  (`rmgraph`) lets every wire be shared. Behaviour of agents needs both at
  once: an agent's session, or an obligation such as an unanswered bell, may
  be neither copied nor dropped, while evidence may be read by any number of
  consumers or by none. This is the linear/non-linear split (Benton's LNL
  models): a REGIME assigns each sort `:linear` or `:cartesian`, and the
  checks below say where a diagram breaks it.

  Everything is pure and parametric in two functions supplied by the caller:
  SORT-OF maps a vertex :vtype to its sort (the type without parameters),
  and OWNER-OF maps a :vtype to the identity it carries, or nil. A SIGNATURE
  maps each generator (an edge :value) to {:in [sorts] :out [sorts]},
  optionally with :identity :creates or :destroys; a generator without
  :identity must carry the same identities out as it takes in.

  Findings are data. Nothing here throws on a malformed diagram."
  (:require [futon3c.diagramprover.graph :as graph]))

(defn- occurrences [v coll] (count (filter #(= v %) coll)))

(defn producers
  "How many times VERTEX is produced: as an edge target or a boundary input."
  [g vertex]
  (+ (occurrences vertex (:inputs g))
     (reduce + (map #(occurrences vertex (graph/target g %))
                    (graph/in-edges g vertex)))))

(defn consumers
  "How many times VERTEX is consumed: as an edge source or a boundary output."
  [g vertex]
  (+ (occurrences vertex (:outputs g))
     (reduce + (map #(occurrences vertex (graph/source g %))
                    (graph/out-edges g vertex)))))

(defn- vtype [g vertex] (:vtype (graph/vertex-data g vertex)))

(defn- edge-summary [g edge]
  (select-keys (graph/edge-data g edge) [:value :event]))

(defn signature-findings
  "Edges whose generator is undeclared, or whose wire sorts differ from the
  declared signature."
  [g signature sort-of]
  (->> (sort (graph/edges g))
       (keep (fn [edge]
               (let [value (:value (graph/edge-data g edge))
                     actual {:in (mapv #(sort-of (vtype g %)) (graph/source g edge))
                             :out (mapv #(sort-of (vtype g %)) (graph/target g edge))}]
                 (if-let [declared (get signature value)]
                   (when (not= actual (select-keys declared [:in :out]))
                     (assoc (edge-summary g edge)
                            :finding :ill-typed
                            :expected (select-keys declared [:in :out])
                            :actual actual))
                   (assoc (edge-summary g edge) :finding :unknown-generator)))))
       vec))

(defn linearity-findings
  "Vertices of a :linear sort not produced exactly once and consumed exactly
  once. Boundary ports count, so an open obligation left on the outputs is
  not a finding here; it is reported by `open-ports`. Sorts absent from the
  regime are linear: sharing must be granted, never assumed."
  [g regime sort-of]
  (->> (sort (graph/vertices g))
       (keep (fn [vertex]
               (let [sort* (sort-of (vtype g vertex))]
                 (when (= :linear (get regime sort* :linear))
                   (let [p (producers g vertex) c (consumers g vertex)
                         kind (cond (> c 1) :duplicated
                                    (zero? c) :discarded
                                    (zero? p) :conjured
                                    (> p 1) :merged)]
                     (when kind
                       (merge {:finding kind :sort sort* :vtype (vtype g vertex)
                               :producers p :consumers c}
                              (select-keys (graph/vertex-data g vertex) [:ref]))))))))
       vec))

(defn identity-findings
  "Edges that do not carry the same identities out as in, unless the
  signature lets the generator create or destroy them. This is where cloning
  an agent, substituting one agent for another, or conjuring one out of a
  transport step shows up."
  [g signature owner-of]
  (let [owners (fn [vs] (frequencies (keep #(owner-of (vtype g %)) vs)))]
    (->> (sort (graph/edges g))
         (keep (fn [edge]
                 (let [value (:value (graph/edge-data g edge))
                       ins (owners (graph/source g edge))
                       outs (owners (graph/target g edge))
                       ok? (case (get-in signature [value :identity])
                             :creates (every? (fn [[o n]] (<= (get ins o 0) n)) outs)
                             :destroys (every? (fn [[o n]] (<= n (get ins o 0))) outs)
                             (= ins outs))]
                   (when-not ok?
                     (assoc (edge-summary g edge)
                            :finding :identity-not-preserved
                            :in ins :out outs)))))
         vec)))

(defn concurrency-findings
  "Identities that are active on two wires at once. Two vertices carrying the
  same identity are concurrent when neither is reachable from the other, so
  a singular identity is one whose vertices are totally ordered by the
  diagram. A restart must therefore be wired to what it restarts (a retired
  session feeding the new one), or it reads as a second, parallel session."
  [g owner-of]
  (let [by-owner (group-by #(owner-of (vtype g %))
                           (filter #(owner-of (vtype g %)) (graph/vertices g)))
        reach (memoize (fn [v] (graph/successors g [v])))]
    (->> (sort-by str (keys by-owner))
         (keep (fn [owner]
                 (let [vs (sort (get by-owner owner))
                       pairs (for [a vs b vs :when (< a b)
                                   :when (not (or (contains? (reach a) b)
                                                  (contains? (reach b) a)))]
                               [a b])]
                   (when (seq pairs)
                     {:finding :concurrent-identity :owner owner
                      :concurrent-pairs (count pairs)}))))
         vec)))

(defn open-ports
  "The diagram's boundary read as obligations. Outputs are what the diagram
  leaves for its future (an unanswered bell, a session still open); inputs
  are what it assumed from its past (a session already running, a bell
  rung before the window). Each port is {:sort … :vtype … :ref …}."
  [g sort-of]
  (let [port (fn [v] (merge {:sort (sort-of (vtype g v)) :vtype (vtype g v)}
                            (select-keys (graph/vertex-data g v) [:ref])))]
    {:inputs (mapv port (:inputs g))
     :outputs (mapv port (:outputs g))}))

(defn check
  "All four laws over one diagram. Clean when every vector is empty."
  [g {:keys [signature regime sort-of owner-of]}]
  {:signature (signature-findings g signature sort-of)
   :linearity (linearity-findings g regime sort-of)
   :identity (identity-findings g signature owner-of)
   :concurrency (concurrency-findings g owner-of)})

(defn clean? [report] (every? empty? (vals report)))
