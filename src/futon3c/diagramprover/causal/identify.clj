(ns futon3c.diagramprover.causal.identify
  "Observed-only backdoor and front-door identification for small DAGs.

   For mediator set Z, FD1 removes every Z node and searches the remaining
   directed child relation for an X->Y path. FD2 evaluates X _||_ Z in G with
   X's outgoing arrows cut, which retains exactly paths entering X. FD3 cuts
   every Z member's outgoing arrows and evaluates Z _||_ Y | X, retaining
   exactly paths entering Z while blocking them by X. Failure paths are
   witnessed lower bounds, never claims of exhaustive path enumeration."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.surgery :as surgery]))

(def front-door-formula
  "sum_z P(z|x) * sum_x' P(y|x',z) P(x')")

(defn- subsets [items]
  (reduce (fn [sets item]
            (into sets (map #(conj % item) sets)))
          [#{}] items))

(defn- observed-candidates [causal-dag treatment outcome]
  (->> (:variables causal-dag)
       (keep (fn [[id variable]]
               (when (and (= :observed (keyword (:kind variable)))
                          (not (#{treatment outcome} id))) id)))
       sort vec))

(defn- witness [causal-dag x y given]
  (dsep/connecting-paths causal-dag x y given {:limit 1}))

(defn- directed-path [causal-dag start target]
  (letfn [(walk [path]
            (let [node (peek path)]
              (if (= node target)
                path
                (some #(walk (conj path %))
                      (sort (remove (set path) (dag/children causal-dag node)))))))]
    (walk [start])))

(defn observed-adjustment-search
  "Exhaustively test every observed non-X/non-Y subset for backdoor adjustment."
  [causal-dag treatment outcome]
  (mapv
   (fn [given]
     (let [holds? (dsep/backdoor-adjustment? causal-dag treatment outcome given)
           paths (if holds? {:paths [] :truncated? false}
                     (witness (surgery/cut-outgoing causal-dag treatment)
                              treatment outcome given))]
       {:given given :holds? holds? :surviving-paths (:paths paths)
        :paths-truncated? (:truncated? paths)}))
   (subsets (observed-candidates causal-dag treatment outcome))))

(defn front-door-conditions
  "Compute Pearl's three front-door conditions for mediator set Z."
  [causal-dag treatment outcome mediators]
  (let [mediators (set mediators)
        pruned (reduce surgery/remove-node causal-dag mediators)
        bypass (directed-path pruned treatment outcome)
        cut-x (surgery/cut-outgoing causal-dag treatment)
        fd2? (dsep/d-separated? cut-x treatment mediators #{})
        fd2-paths (if fd2? {:paths [] :truncated? false}
                      (witness cut-x treatment mediators #{}))
        cut-z (reduce surgery/cut-outgoing causal-dag mediators)
        fd3? (dsep/d-separated? cut-z mediators outcome #{treatment})
        fd3-paths (if fd3? {:paths [] :truncated? false}
                      (witness cut-z mediators outcome #{treatment}))]
    [{:condition :FD1 :holds? (nil? bypass)
      :claim :mediators-intercept-all-directed-paths
      :offending-paths (cond-> [] bypass (conj bypass))
      :paths-truncated? false}
     {:condition :FD2 :holds? fd2?
      :claim :no-unblocked-treatment-mediator-backdoor
      :offending-paths (:paths fd2-paths)
      :paths-truncated? (:truncated? fd2-paths)}
     {:condition :FD3 :holds? fd3?
      :claim :treatment-blocks-all-mediator-outcome-backdoors
      :offending-paths (:paths fd3-paths)
      :paths-truncated? (:truncated? fd3-paths)}]))

(defn front-door-sets
  "Exhaustively return satisfying observed mediator sets and every attempt."
  [causal-dag treatment outcome]
  (let [attempts
        (mapv (fn [mediators]
                (let [conditions (front-door-conditions
                                  causal-dag treatment outcome mediators)]
                  {:mediators mediators
                   :holds? (every? :holds? conditions)
                   :conditions conditions
                   :failed-conditions
                   (mapv :condition (remove :holds? conditions))}))
              (subsets (observed-candidates causal-dag treatment outcome)))]
    {:sets (filterv :holds? attempts) :attempts attempts}))

(defn identify
  "Identify P(Y|do(X)) by observed backdoor, then front-door, else refuse."
  [causal-dag treatment outcome]
  (let [backdoor-attempts (observed-adjustment-search
                           causal-dag treatment outcome)
        backdoor-sets (filterv :holds? backdoor-attempts)]
    (if (seq backdoor-sets)
      {:method :backdoor
       :adjustment-sets (mapv :given backdoor-sets)}
      (let [{:keys [sets attempts]} (front-door-sets
                                    causal-dag treatment outcome)]
        (if-let [identified (first sets)]
          {:method :front-door
           :mediators (:mediators identified)
           :conditions (:conditions identified)
           :estimand {:type :front-door :formula front-door-formula
                      :mediator-set (:mediators identified)}}
          {:method :refusal
           :backdoor-exhaustion {:candidate-set-count (count backdoor-attempts)
                                 :candidate-attempts backdoor-attempts}
           :front-door-exhaustion {:candidate-set-count (count attempts)
                                   :candidate-attempts attempts}
           :missing-capability :do-calculus-identification})))))
