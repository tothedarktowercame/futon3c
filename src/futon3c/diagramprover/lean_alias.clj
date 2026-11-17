(ns futon3c.diagramprover.lean-alias
  "Captured printer symbols, with domain restrictions separate from exemptions."
  (:require [futon3c.diagramprover.lean-import :as li]))

(defn- children [t] (if (= :bvar (first t)) [] (rest t)))
(defn- nodes [t] (tree-seq #(seq (children %)) children t))
(defn- spine [t]
  (loop [f t args []]
    (if (= :app (first f)) (recur (second f) (cons (nth f 2) args)) [f (vec args)])))
(defn- binders [f]
  (if (= :lam (first f)) (inc (binders (second f))) 0))
(defn- family? [f]
  (and (pos? (binders f))
       (= [[:const "Classical.choose"]]
          (li/spine-head (loop [b f] (if (= :lam (first b)) (recur (second b)) b))))))
(defn- maximal-spines [t]
  (let [[f args] (spine t)]
    (if (seq args)
      (concat [[f args]] (mapcat maximal-spines (cons f args)))
      (mapcat maximal-spines (children t)))))
(defn annotate
  "Attach the capture's symbol evidence only to its root spine head. Nested
  maximal family applications without that evidence are counted, never guessed."
  [host hit]
  (let [[f args] (spine host)
        a (:alias hit)
        md {:tex (:known_tex a) :source (some-> (:source a) keyword)
            :suffix-match? (:suffix_match a) :binders (:pushed a)
            :args (:n_args a) :shape (some-> (:shape a) keyword)}
        path (vec (repeat (count args) 1))
        annotated (if (seq path) (assoc-in host path (vary-meta f assoc :family-symbol md))
                      (vary-meta host assoc :family-symbol md))]
    {:host annotated
     :unannotated-family-redexes
     (count (filter (fn [[head _]] (and (family? head) (nil? (:family-symbol (meta head)))))
                    (maximal-spines annotated)))}))
(defn spine-out-of-range
  "Count maximal lambda-headed application spines exceeding the schema bound."
  [host max-m]
  {:spine-arity-out-of-range
   (count (filter (fn [[f args]] (and (pos? (binders f)) (> (count args) max-m)))
                  (maximal-spines host)))})

(def alias-apply {:name :alias-apply :priority 0})
(def family-reindex {:name :family-reindex :priority 1
                     :exemption :symbol-has-binder-suffix})
(def family-reindex-provenance (assoc family-reindex :exemption :symbol-from-registry))
(defn spine-schemas
  "Each arity has a unique name for strategy sensitivity; :base-name identifies
  the printer rule. k comes from the actual lambda, not the probe's pushed count."
  [base ms]
  (mapv
   (fn [m]
     (let [av (mapv #(symbol (str "?A" %)) (range 1 (inc m)))
           info #(-> % (get '?F) meta :family-symbol)
           domain [[:choose-family #(family? (get % '?F))]
                   [:saturated #(<= (binders (get % '?F)) m)]
                   [:symbol-known #(some? (:tex (info %)))]
                   [:maximal-spine #(= m (:args (info %)))]]]
       (merge base
              {:name (keyword (str (name (:name base)) "-" m)) :base-name (:name base)
               :pattern (reduce (fn [f a] [:app f a]) '?F av)
               :domain-guards (set (map first domain))
               :guards (cond-> domain
                         (:exemption base)
                         (conj [(:exemption base)
                                (if (= :symbol-from-registry (:exemption base))
                                  #(contains? #{:registry :evidence-key :reserved} (:source (info %)))
                                  #(true? (:suffix-match? (info %))))]))
               :rewrite (fn [b]
                          (let [a (mapv b av) k (binders (get b '?F))
                                {:keys [tex source]} (info b) sym [[:sym tex source]]]
                            (if (= :alias-apply (:name base))
                              (into [:fn-app sym] a)
                              (reduce (fn [f x] [:app f x])
                                      (into [:indexed sym] (take k a)) (drop k a)))))}))) ms))
(defn without-exemptions [schemas]
  (mapv #(update % :guards (fn [gs] (filterv (fn [[g _]] (contains? (:domain-guards %) g)) gs))) schemas))
(defn observe [term]
  {:reindexed-source-symbols
   (count (filter #(and (= :indexed (first %)) (= :alias (get-in % [1 0 2]))) (nodes term)))})
