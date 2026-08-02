(ns futon3c.diagramprover.matcher
  "Lazy backtracking matches of typed open hypergraphs."
  (:require [futon3c.diagramprover.graph :as graph]))

(defn make-match [domain codomain]
  {:vertex-map {} :edge-map {} :domain domain :codomain codomain})

(defn vertex-image [match]
  (set (vals (:vertex-map match))))

(defn edge-image [match]
  (set (vals (:edge-map match))))

(defn total? [{:keys [domain vertex-map edge-map]}]
  (and (= (count vertex-map) (graph/num-vertices domain))
       (= (count edge-map) (graph/num-edges domain))))

(defn surjective? [{:keys [codomain] :as match}]
  (and (= (count (vertex-image match)) (graph/num-vertices codomain))
       (= (count (edge-image match)) (graph/num-edges codomain))))

(defn injective? [{:keys [vertex-map] :as match}]
  (= (count vertex-map) (count (vertex-image match))))

(defn try-add-vertex
  "Return a match extended by one vertex, or nil when the map is illegal."
  [{:keys [domain codomain vertex-map] :as match}
   domain-vertex codomain-vertex]
  (if-let [mapped (find vertex-map domain-vertex)]
    (when (= (val mapped) codomain-vertex) match)
    (let [domain-data (graph/vertex-data domain domain-vertex)
          codomain-data (graph/vertex-data codomain codomain-vertex)
          same-type? (= [(:vtype domain-data) (:size domain-data 1)]
                        [(:vtype codomain-data) (:size codomain-data 1)])
          domain-boundary? (graph/is-boundary? domain domain-vertex)
          shared-preimages (for [[vertex image] vertex-map
                                 :when (= image codomain-vertex)]
                             vertex)
          noninjective-legal?
          (or (empty? shared-preimages)
              (and domain-boundary?
                   (every? #(graph/is-boundary? domain %) shared-preimages)))
          gluing-legal?
          (or domain-boundary?
              (and (= (count (graph/in-edges domain domain-vertex))
                      (count (graph/in-edges codomain codomain-vertex)))
                   (= (count (graph/out-edges domain domain-vertex))
                      (count (graph/out-edges codomain codomain-vertex)))))]
      (when (and domain-data codomain-data same-type?
                 (or domain-boundary?
                     (not (graph/is-boundary? codomain codomain-vertex)))
                 noninjective-legal? gluing-legal?)
        (assoc-in match [:vertex-map domain-vertex] codomain-vertex)))))

(defn try-add-edge
  "Return a match extended by one ordered hyperedge, or nil."
  [{:keys [domain codomain edge-map] :as match} domain-edge codomain-edge]
  (if-let [mapped (find edge-map domain-edge)]
    (when (= (val mapped) codomain-edge) match)
    (let [domain-data (graph/edge-data domain domain-edge)
          codomain-data (graph/edge-data codomain codomain-edge)
          domain-ends (into (graph/source domain domain-edge)
                            (graph/target domain domain-edge))
          codomain-ends (into (graph/source codomain codomain-edge)
                              (graph/target codomain codomain-edge))]
      (when (and domain-data codomain-data
                 (= (:value domain-data) (:value codomain-data))
                 (= (graph/edge-domain domain domain-edge)
                    (graph/edge-domain codomain codomain-edge))
                 (= (graph/edge-codomain domain domain-edge)
                    (graph/edge-codomain codomain codomain-edge))
                 (not (contains? (edge-image match) codomain-edge)))
        (reduce (fn [candidate [domain-vertex codomain-vertex]]
                  (when candidate
                    (try-add-vertex candidate domain-vertex codomain-vertex)))
                (assoc-in match [:edge-map domain-edge] codomain-edge)
                (map vector domain-ends codomain-ends))))))

(defn- scalar-edge? [graph edge]
  (and (empty? (graph/source graph edge))
       (empty? (graph/target graph edge))))

(defn- map-scalars [{:keys [domain codomain] :as match}]
  (let [available (filterv #(scalar-edge? codomain %) (graph/edges codomain))]
    (first
     (reduce
      (fn [[candidate remaining] edge]
        (if-not candidate
          (reduced [nil remaining])
          (if-not (scalar-edge? domain edge)
            [candidate remaining]
            (if-let [chosen (first (filter
                                    #(= (:value (graph/edge-data domain edge))
                                        (:value (graph/edge-data codomain %)))
                                    remaining))]
              [(assoc-in candidate [:edge-map edge] chosen)
               (filterv #(not= chosen %) remaining)]
              (reduced [nil remaining])))))
      [match available] (graph/edges domain)))))

(defn- neighbourhood-mapped? [{:keys [domain edge-map]} vertex]
  (every? #(contains? edge-map %)
          (into (graph/in-edges domain vertex)
                (graph/out-edges domain vertex))))

(defn- extensions [{:keys [domain codomain vertex-map edge-map] :as match}]
  (or
   (some
    (fn [[domain-vertex codomain-vertex]]
      (when-not (neighbourhood-mapped? match domain-vertex)
        (if-let [edge (first (remove #(contains? edge-map %)
                                     (graph/in-edges domain domain-vertex)))]
          (keep #(try-add-edge match edge %)
                (graph/in-edges codomain codomain-vertex))
          (when-let [edge (first (remove #(contains? edge-map %)
                                         (graph/out-edges domain domain-vertex)))]
            (keep #(try-add-edge match edge %)
                  (graph/out-edges codomain codomain-vertex))))))
    vertex-map)
   (some
    (fn [domain-vertex]
      (when-not (contains? vertex-map domain-vertex)
        (keep #(try-add-vertex match domain-vertex %)
              (graph/vertices codomain))))
    (graph/vertices domain))
   ()))

(defn convex?
  [{:keys [domain codomain vertex-map] :as match}]
  (and
   (injective? match)
   (let [output-images (keep vertex-map (:outputs domain))
         successors (graph/successors codomain output-images)]
     (not-any? #(contains? successors %)
               (keep vertex-map (:inputs domain))))))

(defn- search [stack require-convex?]
  (lazy-seq
   (when-let [match (peek stack)]
     (let [remaining (pop stack)]
       (if (total? match)
         (if (or (not require-convex?) (convex? match))
           (cons match (search remaining require-convex?))
           (search remaining require-convex?))
         (search (into remaining (extensions match)) require-convex?))))))

(defn matches
  "Return a lazy sequence of total matches from domain into codomain.

  Options are `:convex?` (default true) and `:initial-match`."
  ([domain codomain] (matches domain codomain {}))
  ([domain codomain {:keys [convex? initial-match]
                     :or {convex? true}}]
   (if-let [seed (map-scalars (or initial-match
                                  (make-match domain codomain)))]
     (search [seed] convex?)
     ())))

(defn match-rule
  ([rule host] (match-rule rule host {}))
  ([rule host options] (matches (:lhs rule) host options)))

(defn find-iso [domain codomain]
  (when (and (= (graph/domain domain) (graph/domain codomain))
             (= (graph/codomain domain) (graph/codomain codomain)))
    (let [pairs (concat (map vector (:inputs domain) (:inputs codomain))
                        (map vector (:outputs domain) (:outputs codomain)))
          initial (reduce (fn [match [domain-vertex codomain-vertex]]
                            (when match
                              (try-add-vertex match domain-vertex
                                              codomain-vertex)))
                          (make-match domain codomain) pairs)]
      (when initial
        (first (filter surjective?
                       (matches domain codomain
                                {:convex? false :initial-match initial})))))))
