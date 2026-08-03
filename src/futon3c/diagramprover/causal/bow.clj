(ns futon3c.diagramprover.causal.bow
  "Computed Book-of-Why fixture receipts at the current capability frontier.

   This fixture runner deliberately restricts adjustment candidates to observed
   non-treatment, non-outcome variables and exhaustively checks their power set.
   Front-door and counterfactual queries are classified and refused explicitly;
   they are not approximated by backdoor or rung-2 surgery."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.surgery :as surgery]))

(def fixture-directory "docs/bow-fixtures")
(def fixture-paths
  {:simpson (str fixture-directory "/simpson-kidney-stones.json")
   :sprinkler (str fixture-directory "/sprinkler-collider.json")
   :front-door (str fixture-directory "/smoking-tar-cancer.json")
   :monty (str fixture-directory "/monty-hall.json")
   :firing-squad (str fixture-directory "/firing-squad.json")})

(defn load-fixture [fixture]
  (dag/load-spec (get fixture-paths fixture fixture)))

(defn- question [causal-dag id]
  (first (filter #(= id (:id %))
                 (get-in causal-dag [:metadata :requested_receipts]))))

(defn- witness [causal-dag x y given]
  (dsep/connecting-paths causal-dag x y given {:limit 1}))

(defn simpson-receipt
  ([] (simpson-receipt (load-fixture :simpson)))
  ([causal-dag]
   (let [cut (surgery/cut-outgoing causal-dag :treatment)
         marginal (witness cut :treatment :recovery #{})]
     {:id "BOW-SIMPSON"
      :question (:question (question causal-dag "BOW-SIMPSON"))
      :verdicts
      [{:claim :marginal-identifies-effect
        :holds? (dsep/backdoor-adjustment? causal-dag :treatment :recovery #{})
        :method :backdoor :given #{} :paths (:paths marginal)}
       {:claim :severity-adjustment-identifies-effect
        :holds? (dsep/backdoor-adjustment? causal-dag :treatment :recovery
                                            #{:severity})
        :method :backdoor :given #{:severity} :paths []}]
      :adjustment-sets [#{:severity}]
      :refusals []})))

(defn- collider-receipt [causal-dag id x y collider]
  (let [marginal? (dsep/d-separated? causal-dag x y #{})
        conditional? (dsep/d-separated? causal-dag x y #{collider})
        opened (witness causal-dag x y #{collider})]
    {:id id
     :question (:question (question causal-dag id))
     :verdicts
     [{:claim :marginal-independence :holds? marginal?
       :method :d-sep :given #{} :paths []}
      {:claim :conditional-independence :holds? conditional?
       :method :d-sep :given #{collider} :paths (:paths opened)}]
     :adjustment-sets [] :refusals []}))

(defn sprinkler-receipt
  ([] (sprinkler-receipt (load-fixture :sprinkler)))
  ([causal-dag]
   (collider-receipt causal-dag "BOW-SPRINKLER"
                     :rain :sprinkler :wet-grass)))

(defn monty-receipt
  ([] (monty-receipt (load-fixture :monty)))
  ([causal-dag]
   (collider-receipt causal-dag "BOW-MONTY"
                     :choice :prize :host-opens)))

(defn- subsets [items]
  (reduce (fn [sets item]
            (into sets (map #(conj % item) sets)))
          [#{}] items))

(defn observed-adjustment-search
  "Exhaustively test every observed candidate subset for backdoor adjustment."
  [causal-dag treatment outcome]
  (let [candidates
        (->> (:variables causal-dag)
             (keep (fn [[id variable]]
                     (when (and (= :observed (keyword (:kind variable)))
                                (not (#{treatment outcome} id))) id)))
             sort vec)]
    (mapv (fn [given]
            (let [holds? (dsep/backdoor-adjustment?
                          causal-dag treatment outcome given)
                  paths (if holds? {:paths [] :truncated? false}
                            (witness (surgery/cut-outgoing causal-dag treatment)
                                     treatment outcome given))]
              {:given given :holds? holds? :surviving-paths (:paths paths)
               :paths-truncated? (:truncated? paths)}))
          (subsets candidates))))

(defn front-door-receipt
  ([] (front-door-receipt (load-fixture :front-door)))
  ([causal-dag]
   (let [attempts (observed-adjustment-search causal-dag :smoking :cancer)
         valid (filterv :holds? attempts)]
     {:id "BOW-FRONTDOOR"
      :question (:question (question causal-dag "BOW-FRONTDOOR"))
      :verdicts [{:claim :observed-backdoor-adjustment-exists
                  :holds? (boolean (seq valid))
                  :method :exhaustive-observed-backdoor-search
                  :candidate-set-count (count attempts)}]
      :adjustment-sets (mapv :given valid)
      :refusals
      (if (seq valid) []
          [{:claim :identify-total-effect
            :reason :observed-backdoor-exhausted
            :missing-capability :front-door-identification
            :candidate-set-count (count attempts)
            :candidate-attempts attempts}])})))

(defn firing-squad-receipt
  ([] (firing-squad-receipt (load-fixture :firing-squad)))
  ([causal-dag]
   (let [r2-question (question causal-dag "BOW-FIRING-R2")
         r3-question (question causal-dag "BOW-FIRING-R3")
         treatment :soldier-A outcome :death
         intervened (surgery/do-intervention causal-dag treatment)
         path (witness intervened treatment outcome #{})
         query-type (keyword (get-in r3-question [:query :type]))]
     {:id "BOW-FIRING-SQUAD"
      :question [(:question r2-question) (:question r3-question)]
      :verdicts [{:claim :rung-2-intervention-has-outcome-route
                  :holds? (dsep/d-connected? intervened treatment outcome #{})
                  :method :surgery :query-type
                  (keyword (get-in r2-question [:query :type]))
                  :paths (:paths path)}]
      :adjustment-sets []
      :refusals
      (if (= :counterfactual query-type)
        [{:claim :individual-counterfactual
          :query-type query-type
          :reason :unsupported-query-type
          :missing-capability :counterfactual-identification}]
        [])})))

(defn all-bow-receipts []
  [(simpson-receipt) (sprinkler-receipt) (front-door-receipt)
   (monty-receipt) (firing-squad-receipt)])
