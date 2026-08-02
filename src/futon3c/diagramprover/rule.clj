(ns futon3c.diagramprover.rule
  "Boundary-compatible string-diagram rewrite rules."
  (:require [futon3c.diagramprover.graph :as graph]))

(defn make-rule
  ([lhs rhs] (make-rule lhs rhs {}))
  ([lhs rhs payload]
   (when-not (= (graph/domain lhs) (graph/domain rhs))
     (throw (ex-info "Inputs must match on LHS and RHS of rule"
                     {:lhs (graph/domain lhs) :rhs (graph/domain rhs)})))
   (when-not (= (graph/codomain lhs) (graph/codomain rhs))
     (throw (ex-info "Outputs must match on LHS and RHS of rule"
                     {:lhs (graph/codomain lhs) :rhs (graph/codomain rhs)})))
   (merge {:lhs lhs :rhs rhs} payload)))

(defn left-linear? [{:keys [lhs]}]
  (let [boundary (concat (:inputs lhs) (:outputs lhs))]
    (= (count boundary) (count (set boundary)))))

(defn converse [rule]
  (assoc rule :lhs (:rhs rule) :rhs (:lhs rule)))
