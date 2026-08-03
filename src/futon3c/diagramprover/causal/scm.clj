(ns futon3c.diagramprover.causal.scm
  "Finite deterministic Boolean structural causal models.

   Equations use a deliberately tiny grammar: `parent`, `not parent`,
   `a and b`, or `a or b`. No host evaluation is involved. Counterfactuals
   follow Pearl's three named steps: enumerate evidence-consistent exogenous
   worlds (abduction), replace structural equations by constants (action), and
   evaluate every retained world (prediction). Agreement yields a Boolean
   answer; disagreement and inconsistent evidence remain computed refusals."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.diagramprover.causal.dag :as dag]))

(defn parse-equation
  "Parse the fixed Boolean equation grammar into pure data."
  [source]
  (let [tokens (str/split (str/trim source) #"\s+")
        variable #(keyword %)]
    (case (count tokens)
      1 {:op :variable :input (variable (first tokens))}
      2 (if (= "not" (first tokens))
          {:op :not :input (variable (second tokens))}
          (throw (ex-info "Unsupported structural equation"
                          {:source source :tokens tokens})))
      3 (let [[left op right] tokens]
          (case op
            "and" {:op :and :inputs [(variable left) (variable right)]}
            "or" {:op :or :inputs [(variable left) (variable right)]}
            (throw (ex-info "Unsupported structural equation"
                            {:source source :tokens tokens}))))
      (throw (ex-info "Unsupported structural equation"
                      {:source source :tokens tokens})))))

(defn equation-inputs [equation]
  (case (:op equation)
    :variable #{(:input equation)}
    :not #{(:input equation)}
    (:and :or) (set (:inputs equation))
    (throw (ex-info "Unknown structural equation operation"
                    {:equation equation}))))

(defn equations
  "Parse equations carried in a loaded causal fixture."
  [causal-dag]
  (into (sorted-map)
        (map (fn [[node source]] [(keyword node) (parse-equation source)]))
        (get-in causal-dag [:metadata :structural_equations] {})))

(defn validate
  "Validate total equation coverage and exact agreement with DAG parents."
  [causal-dag]
  (let [parsed (equations causal-dag)
        nodes (set (keys (:variables causal-dag)))
        endogenous (into #{} (remove #(dag/exogenous? causal-dag %)) nodes)
        exogenous (set/difference nodes endogenous)
        missing (set/difference endogenous (set (keys parsed)))
        extra (set/difference (set (keys parsed)) endogenous)
        mismatches
        (into (sorted-map)
              (keep (fn [node]
                      (let [expected (dag/parents causal-dag node)
                            actual (equation-inputs (get parsed node))]
                        (when (not= expected actual)
                          [node {:expected expected :actual actual}]))))
              (set/intersection endogenous (set (keys parsed))))]
    (when (or (seq missing) (seq extra) (seq mismatches))
      (throw (ex-info "Structural equations do not match causal DAG"
                      {:missing-equations missing
                       :equations-for-exogenous extra
                       :parent-mismatches mismatches})))
    {:dag causal-dag
     :equations parsed
     :exogenous (into (sorted-set) exogenous)
     :order (dag/topological-sort causal-dag)}))

(defn- eval-equation [equation values]
  (case (:op equation)
    :variable (boolean (get values (:input equation)))
    :not (not (boolean (get values (:input equation))))
    :and (every? #(boolean (get values %)) (:inputs equation))
    :or (boolean (some #(boolean (get values %)) (:inputs equation)))))

(defn evaluate
  "Evaluate one exogenous assignment, optionally after an intervention."
  ([model assignment] (evaluate model assignment {}))
  ([{:keys [equations exogenous order]} assignment intervention]
   (reduce (fn [values node]
             (assoc values node
                    (cond
                      (contains? intervention node) (boolean (get intervention node))
                      (exogenous node) (boolean (get assignment node))
                      :else (eval-equation (get equations node) values))))
           (sorted-map) order)))

(defn- assignments [variables]
  (reduce (fn [worlds variable]
            (mapv #(assoc % variable %2)
                  (mapcat (fn [world] [world world]) worlds)
                  (cycle [false true])))
          [(sorted-map)] variables))

(defn- evidence-consistent? [world evidence]
  (every? (fn [[node value]] (= (boolean value) (get world node))) evidence))

(defn capability-refusal [query]
  {:method :refusal
   :query-type :counterfactual
   :reason :unsupported-query-type
   :missing-capability :counterfactual-identification
   :query query})

(defn counterfactual
  "Compute a deterministic counterfactual query from a fixture DAG.

   QUERY contains `:evidence`, `:intervention`, and `:outcome`. When the
   fixture has no equations this function preserves the explicit stochastic/
   unspecified-SCM capability boundary."
  [causal-dag query]
  (if-not (seq (get-in causal-dag [:metadata :structural_equations]))
    (capability-refusal query)
    (let [model (validate causal-dag)
          evidence (into (sorted-map)
                         (map (fn [[node value]] [(keyword node) (boolean value)]))
                         (:evidence query))
          intervention (into (sorted-map)
                             (map (fn [[node value]] [(keyword node) (boolean value)]))
                             (:intervention query))
          outcome (keyword (:outcome query))
          consistent
          (->> (assignments (:exogenous model))
               (filterv #(evidence-consistent? (evaluate model %) evidence)))
          abduction {:step :abduction
                     :evidence evidence
                     :consistent-count (count consistent)
                     :assignments consistent}
          action {:step :action
                  :intervention intervention
                  :replaced-equations (set (keys intervention))}]
      (if (empty? consistent)
        {:method :refusal
         :query-type :counterfactual
         :reason :evidence-inconsistent
         :abduction abduction
         :action action}
        (let [worlds (mapv (fn [assignment]
                             {:assignment assignment
                              :value (get (evaluate model assignment intervention)
                                          outcome)})
                           consistent)
              by-value (group-by :value worlds)
              prediction {:step :prediction
                          :outcome outcome
                          :worlds worlds
                          :all-agree? (= 1 (count by-value))}]
          (if (= 1 (count by-value))
            {:method :deterministic-scm
             :query-type :counterfactual
             :abduction abduction
             :action action
             :prediction prediction
             :answer (:value (first worlds))}
            {:method :refusal
             :query-type :counterfactual
             :reason :counterfactual-underdetermined
             :abduction abduction
             :action action
             :prediction prediction
             :witness-worlds
             (into (sorted-map)
                   (map (fn [[value matching]] [value (first matching)]))
                   by-value)}))))))
