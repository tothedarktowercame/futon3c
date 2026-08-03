(ns futon3c.diagramprover.causal.idalg
  "Shpitser-Pearl ID for P(Y|do(X)) over an ADMG.

   The recursion follows the 2006 ID pseudocode lines: (1) marginalize when X
   is empty; (2) restrict to ancestors of Y; (3) add non-ancestors in G[V\\X]
   to the intervention; (4) factor across districts of G[V\\X]; (5) either
   fail on the single full-graph district, truncate-factor on a graph district,
   or recurse inside its unique containing district. Failure carries the
   recursive district/intervention subproblem as a partial hedge witness."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.diagramprover.causal.admg :as admg]))

(defn- sum-expr [variables expr]
  (if (seq variables)
    {:op :sum :variables (vec (sort variables)) :expr expr}
    expr))

(defn- product-expr [terms]
  (let [terms (vec terms)]
    (cond (empty? terms) {:op :constant :value 1}
          (= 1 (count terms)) (first terms)
          :else {:op :product :terms terms})))

(defn- factors [graph]
  (let [order (vec (admg/topological-sort graph))]
    (mapv (fn [index variable]
            {:op :cond :variable variable
             :given (vec (take index order))})
          (range) order)))

(defn- factors-for [all-factors nodes]
  (product-expr (filter #(contains? nodes (:variable %)) all-factors)))

(defn- kernel-factors [probability graph nodes]
  (let [order (vec (admg/topological-sort graph))]
    (keep-indexed
     (fn [index variable]
       (when (contains? nodes variable)
         {:op :cond :variable variable :given (vec (take index order))
          :source probability}))
     order)))

(declare id*)

(defn- id* [y x probability graph all-factors depth]
  (when (> depth 100)
    (throw (ex-info "ID recursion limit" {:graph graph :x x :y y})))
  (let [v (set (:nodes graph))]
    (cond
      (empty? x)
      (sum-expr (set/difference v y) probability)

      (not= v (admg/ancestors graph y))
      (let [ancestors (admg/ancestors graph y)]
        (id* y (set/intersection x ancestors)
             (sum-expr (set/difference v ancestors) probability)
             (admg/induced graph ancestors) all-factors (inc depth)))

      :else
      (let [without-x (set/difference v x)
            subgraph (admg/induced graph without-x)
            w (set/difference without-x (admg/ancestors subgraph y))]
        (if (seq w)
          (id* y (set/union x w) probability graph all-factors (inc depth))
          (let [components (admg/districts subgraph)]
            (if (> (count components) 1)
              (sum-expr
               (set/difference v (set/union y x))
               (product-expr
                (map #(id* % (set/difference v %) probability graph
                            all-factors (inc depth))
                     components)))
              (let [s (first components)
                    graph-components (admg/districts graph)]
                (cond
                  (and (= 1 (count graph-components))
                       (= v (first graph-components)))
                  (throw
                   (ex-info
                    "Effect is not identifiable"
                    {:witness {:type :failing-recursive-subproblem
                               :district (set s)
                               :graph-district v
                               :intervened (set x)
                               :outcome (set y)}}))

                  (some #{s} graph-components)
                  (sum-expr (set/difference s y)
                            (product-expr (kernel-factors probability graph s)))

                  :else
                  (let [containing
                        (first (filter #(set/subset? s %) graph-components))]
                    (id* y (set/intersection x containing)
                         (factors-for all-factors containing)
                         (admg/induced graph containing)
                         all-factors (inc depth))))))))))))

(defn identify-effect [graph treatment outcome]
  (let [x (if (set? treatment) treatment #{treatment})
        y (if (set? outcome) outcome #{outcome})
        all-factors (factors graph)]
    (try
      {:identifiable? true
       :estimand (id* y x (product-expr all-factors) graph all-factors 0)}
      (catch clojure.lang.ExceptionInfo failure
        (if-let [witness (:witness (ex-data failure))]
          {:identifiable? false :witness witness}
          (throw failure))))))

(defn formula [expr]
  (case (:op expr)
    :constant (str (:value expr))
    :cond (if-let [source (:source expr)]
            (str "Cond[" (name (:variable expr))
                 (when (seq (:given expr))
                   (str " | " (str/join "," (map name (:given expr)))))
                 "]{" (formula source) "}")
            (str "P(" (name (:variable expr))
                 (when (seq (:given expr))
                   (str " | " (str/join "," (map name (:given expr))))) ")"))
    :product (str "(" (str/join " * " (map formula (:terms expr))) ")")
    :sum (str "sum_{" (str/join "," (map name (:variables expr))) "} "
              (formula (:expr expr)))
    (pr-str expr)))
