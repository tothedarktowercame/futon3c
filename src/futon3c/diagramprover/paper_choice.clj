(ns futon3c.diagramprover.paper-choice
  "Ground, one-binder witness for a02J03 borel_pi_eq_generateFrom_Icc.
  This is an instantiated rule pair, not a Lean parser or higher-order matcher.
  Operators are ordered term hyperedges; var denotes the sole bound variable.
  R2's registered-family/data-valued assumptions are explicit premises of this
  witness. It models the distinct choice keys, not the printer's sentence state.

  mfuton-share efdeba34, ApmPaperPrinter.run: after stripping lambdas, a
  Classical.choose body head exempts beta reduction. The real code also handles
  multiple binders, alias lookup, proof flags, saturation and a depth limit;
  those are outside this single fully applied data-lambda witness."
  (:require [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.rule :as rule]))

(defn term-graph [term]
  (letfn [(emit [g [op & children]]
            (let [[g inputs] (reduce (fn [[g ids] child]
                                      (let [[g id] (emit g child)]
                                        [g (conj ids id)])) [g []] children)
                  [g output] (graph/add-vertex g {:vtype :term})
                  [g _] (graph/add-edge g inputs [output] {:value op})]
              [g output]))]
    (let [[g root] (emit (graph/make-graph) term)]
      (graph/set-outputs g [root]))))

(defn- substitute [body argument]
  (if (= body [:var]) argument
      (into [(first body)] (map #(substitute % argument) (rest body)))))

(defn- choose-body? [{:keys [domain codomain edge-map]}]
  (boolean
   (some (fn [[edge data]]
           (when (= :lam (:value data))
             (let [body (first (graph/source codomain (get edge-map edge)))]
               (some #(= :choose (:value (graph/edge-data codomain %)))
                     (graph/in-edges codomain body)))))
         (:edata domain))))

(def overlap-term [:app [:lam [:choose [:spec [:var]]]] [:index]])
(def beta-result [:choose [:spec [:index]]])
;; :index with an argument is the family-index reading x_i; nullary :index is i.
(def indexed-result [:index [:spec]])

(def r1
  (rule/make-rule (term-graph overlap-term)
                  (term-graph (substitute [:choose [:spec [:var]]] [:index]))
                  {:name :beta-data-instance}))
(def r2
  (rule/make-rule (term-graph overlap-term) (term-graph indexed-result)
                  {:name :registered-choice-family-instance}))
(def r1-guarded
  (assoc r1 :name :beta-data-guarded :guard (complement choose-body?)))
