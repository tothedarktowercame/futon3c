(ns futon3c.diagramprover.wm-wire-construction-products
  "Before-reader interventions over the real assembly's competing candidates."
  (:require [futon2.aif.cascade-problems :as cp]
            [futon2.aif.cascade-policy :as policy]
            [futon2.aif.cascade-model-manifest :as manifest]
            [futon2.aif.efe :as efe]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.report.war-machine :as wm] [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))

(def assembled
  (delay (cp/assemble {:targets [fixture/tick-1-target]
                       :sources (loc/locate-all fixture/tick-1-sources)})))

(defn score-product [field mutation]
  (let [a (if (= field :horizon-steps)
            (update-in @assembled [:problems 0 :cascade-problem field]
                       #(support/change field % mutation)) @assembled)
        p (get-in a [:problems 0 :cascade-problem])
        family (#'wm-cd/cascade-family-parameters (:problems a))
        spec (if (#{:want :cascade-spec} field)
               (update (:cascade-spec p) :want #(support/change :want % mutation))
               (:cascade-spec p))
        candidates (mapv (fn [pair]
                           {:kind :cascade-candidate :id (:candidate-id pair)
                            :precedence (mapv #(policy/token-interpretation % (get-in p [:interpretations %]))
                                              (:precedence pair))
                            :construction-receipt (:construction-receipt pair)})
                         (get-in a [:problems 0 :constructed-candidates]))
        ranked (efe/rank-cascade-actions
                 {:cascade-belief (manifest/observed-belief
                                    (set (for [[k v] (:facts p) :when (true? v)] k)))}
                 candidates {:cascade-spec spec :horizon-steps (:horizon-steps family)
                             :f-prefix-production? true})]
    {:scores (mapv :controller-score ranked) :candidates (count candidates)
     :horizon (:horizon-steps family)}))

(defn decision-product [field mutation]
  (let [a (update-in @assembled [:problems 0 :cascade-problem field]
                     #(support/change field % mutation))
        ranked-product (atom nil) real-rank efe/rank-actions
        result (with-redefs [efe/rank-actions
                             (fn [state candidates opts]
                               (let [r (real-rank state candidates opts)]
                                 (reset! ranked-product (mapv :controller-score r)) r))]
                 (wm-cd/cascade-decision a fixture/live-c-opts))
        d (:decision result)]
    {:scores @ranked-product
     :posterior (vec (sort (vals (get-in d [:selection-law :posterior]))))
     :status (:status d)}))
