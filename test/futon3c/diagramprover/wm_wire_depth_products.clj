(ns futon3c.diagramprover.wm-wire-depth-products
  "Isolated before-reader intervention on policy depth. Drives the fold-in
  support's observe, which calls the report's judge; kept apart from
  wm-wire-precision-horizon-products so the horizon product loads no report."
  (:require [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire-fold-in-support :as fold]))

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
