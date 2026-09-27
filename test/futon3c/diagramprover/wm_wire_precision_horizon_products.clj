(ns futon3c.diagramprover.wm-wire-precision-horizon-products
  "Isolated before-reader interventions; fixture data comes from namespaces."
  (:require [clojure.java.io :as io]
            [futon2.aif.cascade-problems :as cp]
            [futon2.aif.cascade-policy :as policy]
            [futon2.aif.cascade-model-manifest :as manifest]
            [futon2.aif.efe :as efe]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.aif.wm.construction-inputs :as construction-inputs]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-in-support :as fold]))

(defn horizon-product [change]
  (let [root (w/tmp-dir "horizon-product-") assemble cp/assemble
        written (atom nil) carrier (atom nil)]
    (try
      (let [a (with-redefs [cp/assemble
                           (fn [input]
                             (reset! written input)
                             (let [v (update-in input [:sources :horizon-steps] change)]
                               (reset! carrier v) (assemble v)))]
                (construction-inputs/assemble-cascade-problems-with-published
                  root {:targets [fixture/tick-1-target]
                        :sources (loc/locate-all fixture/tick-1-sources)}))
            p (get-in a [:problems 0 :cascade-problem])
            candidates (mapv (fn [pair]
                               {:kind :cascade-candidate :id (:candidate-id pair)
                                :precedence (mapv #(policy/token-interpretation % (get-in p [:interpretations %]))
                                                  (:precedence pair))
                                :construction-receipt (:construction-receipt pair)})
                             (get-in a [:problems 0 :constructed-candidates]))
            state {:cascade-belief (manifest/observed-belief
                                    (set (for [[k v] (:facts p) :when (true? v)] k)))}
            opts {:cascade-spec (:cascade-spec p) :horizon-steps (:horizon-steps p)
                  :f-prefix-production? true}
            ranked (efe/rank-cascade-actions state candidates opts)]
        {:written @written :carrier @carrier :problem p :candidates candidates
         :state state :opts opts :scores (mapv :controller-score ranked)})
      (finally (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f))))))

(defn depth-product [mutation]
  (let [rank efe/rank-actions inputs (atom []) scores (atom [])
        r (with-redefs [efe/rank-actions
                        (fn [state candidates opts]
                          (swap! inputs conj [state candidates (select-keys opts [:horizon-steps :beta :rates :cascade-spec])])
                          (let [v (rank state candidates opts)]
                            (swap! scores conj (mapv :controller-score v)) v))]
            (fold/observe :horizon mutation))]
    (assoc r :rank-inputs @inputs :scores @scores
           :posterior (get-in r [:result :decision :selection-law :posterior]))))
