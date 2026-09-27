(ns futon3c.diagramprover.wm-wire-belief-products-support
  "Belief-only interventions on the real decision's joint rank call."
  (:require [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire-token-input-support :as source]))

(defn observe [hop change]
  (let [reader-var (if (= hop :kernel) #'efe/rank-cascade-actions #'efe/rank-actions)
        real @reader-var captured (atom nil)
        execution (with-redefs-fn
      {reader-var
       (fn [state candidates opts]
         (if-not (:prediction-context opts)
           (real state candidates opts)
           (let [belief (:cascade-belief state)
                 carrier (change belief)
                 ranked (real (assoc state :cascade-belief carrier) candidates opts)]
             (reset! captured
                     {:writer belief :carrier carrier
                      :controls {:candidates candidates
                                 :state (dissoc state :cascade-belief)
                                 :rates (:adjudication-rates opts)
                                 :observation-model (:observation-model opts)
                                 :horizon (:horizon-steps opts)
                                 :beta (:beta opts)}
                      :incoming (source/incoming ranked)
                      :G (mapv :G-efe ranked)})
             ranked)))}
      #(source/decision-observe (if (= hop :kernel) :decision-kernel :decision-dispatch) :none))]
      (assoc-in @captured [:controls :beta] (get-in execution [:product :decision :beta]))))

(def pairs
  (delay (into {} (for [hop [:kernel :dispatch]]
                    [hop [(observe hop identity)
                          (observe hop (constantly {#{} 1}))]]))))
