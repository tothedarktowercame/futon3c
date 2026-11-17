(ns futon3c.diagramprover.lean-import
  "Neutral beta-probe DAGs to term-schema hosts. Metadata is import evidence,
  not a semantic guard: existing subst/shift may discard it. No flag-dependent
  rule is used here. Capture pins are retained, not independently authenticated."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]))

(defn load-hits [path]
  (with-open [r (io/reader path)]
    (let [rows (mapv #(json/parse-string % true) (remove empty? (line-seq r)))
          header (first rows) hits (subvec rows 1)]
      (when-not (and (:header header) (= (:hits header) (count hits)))
        (throw (ex-info "Missing or inconsistent capture header" {:path (str path)})))
      {:header header :hits hits})))

(defn proof? [term] (get (meta term) :proof? :unknown))
(defn- flag [v] (if (nil? v) :unknown v))

(defn host
  "Return a host or typed refusal. Counts are computed before unfolding: tree-nodes
  counts the full source DAG unfolded (including domains); projected-tree-nodes
  counts the operational tree (excluding metadata domains). Both must fit the
  bound, default 20000. Pins include caller loader-sha and all capture context."
  [hit {:keys [loader-sha max-tree-nodes] :or {max-tree-nodes 20000}}]
  (let [pins (merge (:pins hit) (select-keys hit [:module :declaration :site :hit :decision :stack])
                    {:loader-sha loader-sha})]
    (cond
      (:refused hit) {:refused :refused-upstream :reason (:refused hit) :pins pins :dag-nodes (:dag_nodes hit)}
      (:probe_error hit) {:refused :probe-error :reason (:probe_error hit) :pins pins}
      :else
      (try
        (when-not (and (string? loader-sha) (seq loader-sha))
          (throw (ex-info "loader-sha required" {})))
        (let [nodes (:nodes hit) cache (atom {}) projections (atom #{})]
          (letfn [(node [id] (or (get nodes (keyword id))
                                (throw (ex-info "Missing DAG node" {:node id}))))
                  (sizes [id active]
                    (when (contains? active id) (throw (ex-info "Cyclic DAG" {:node id})))
                    (or (get @cache id)
                        (let [{:keys [k c]} (node id)
                              children (mapv #(sizes % (conj active id)) c)
                              source (reduce +' 1 (map first children))
                              ps (mapv second children)
                              projected (case k
                                          "lam" (inc (nth ps 1))
                                          "pi" (reduce +' 2 ps)
                                          "let" (reduce +' 2 ps)
                                          "mdata" (first ps)
                                          "app" (reduce +' (dec (count ps)) ps)
                                          (reduce +' 1 ps))
                              result [source projected]]
                          (swap! cache assoc id result) result)))
                  (emit [id]
                    (let [{:keys [k c] :as n} (node id)
                          md {:proof? (flag (:is_proof n)) :prop? (flag (:is_prop n))
                              :binder-name (:binder_name n) :binder-info (:binder_info n) :node-id id}
                          child #(emit (nth c %))
                          term
                          (case k
                            "app" (do (swap! projections conj :left-fold-app)
                                      (reduce (fn [f [i arg]]
                                                [:app f (vary-meta (emit arg) assoc :arg-info (get (:arg_infos n) i))])
                                              (child 0) (map-indexed vector (rest c))))
                            "lam" (do (swap! projections conj :lambda-domain-in-metadata) [:lam (child 1)])
                            "pi" (do (swap! projections conj :pi-body-under-lam) [:pi (child 0) [:lam (child 1)]])
                            "let" (do (swap! projections conj :let-body-under-lam) [:let (child 0) (child 1) [:lam (child 2)]])
                            "const" [[:const (:name n)]]
                            "bvar" [:bvar (:index n)]
                            "fvar" [[:fvar (:name n)]]
                            "mvar" [[:mvar (:name n)]]
                            "lit" [[:lit (:kind n) (:value n)]]
                            "sort" [[:sort]]
                            "proj" [[:proj (:type_name n) (:index n)] (child 0)]
                            "mdata" (do (swap! projections conj :mdata-stripped) (child 0))
                            (throw (ex-info "Unknown constructor" {:kind k})))
                          md (cond-> md (= k "lam") (assoc :domain (child 0)))]
                      (if (= k "mdata")
                        (vary-meta term update :stripped-mdata (fnil conj []) n)
                        (with-meta term md))))]
            (let [[tree projected] (sizes (:root hit) #{})
                  counts {:tree-nodes tree :projected-tree-nodes projected :dag-nodes (count nodes)}]
              (if (> (max tree projected) max-tree-nodes)
                (merge counts {:refused :tree-too-large :pins (merge pins counts)})
                (let [term (emit (:root hit)) projection (vec (sort @projections))]
                  (merge counts {:host term :projection projection
                                 :pins (merge pins counts {:projection projection})}))))))
        (catch Exception e {:refused :probe-error :reason (.getMessage e) :pins pins})))))

(defn spine-head [term]
  (if (= :app (first term)) (recur (second term)) term))

(defn- body-head [body]
  (spine-head (loop [t body] (if (= :lam (first t)) (recur (second t)) t))))

(def lean-family
  {:name :family :pattern '[:app [:lam ?B] ?A]
   :guards [[:choose-family #(= [[:const "Classical.choose"]] (body-head (get % '?B)))]]
   :rewrite (fn [b] [:family-at [:lam (get b '?B)] (get b '?A)])})

(defn finding-shape
  "Abstract one pair report. Treat the transcribed :choose operator and the
  imported Classical.choose spine head as the same family predicate only here."
  [pair]
  (let [t (:overlap pair) body (when (and (= :app (first t)) (= :lam (first (second t))))
                               (second (second t)))
        head (when body (body-head body))]
    {:rules (set (:schemas pair)) :status (:status pair)
     :settled-by (get-in pair [:settled-by :settled-by])
     :family-applied? (boolean (and body (or (= [[:const "Classical.choose"]] head)
                                            (= :choose (first head)))))}))
