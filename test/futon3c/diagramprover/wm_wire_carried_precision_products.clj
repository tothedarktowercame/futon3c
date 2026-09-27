(ns futon3c.diagramprover.wm-wire-carried-precision-products
  "Real decision and sealed producer; mutate only at the producer-return seam."
  (:require [futon2.aif.policy :as policy]
            [futon2.aif.policy-precision-carry :as precision]
            [futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test :as fixture]))

(defn observe [mode]
  (let [advance precision/advance select policy/select-action-cascades
        written (atom nil) carrier (atom nil) reader-input (atom nil)
        result (try
                 (with-redefs [precision/advance
                               (fn [input]
                                 ;; The coherent alternative is authored by advance,
                                 ;; including gamma, tau, initialized-beta and seal.
                                 (let [r (advance (cond-> input (= mode :coherent-three)
                                                    (assoc :initialized-beta 3)))
                                       v (cond-> r (= mode :beta-only) (update :beta inc))]
                                   (reset! written r) (reset! carrier v) v))
                               policy/select-action-cascades
                               (fn [ranked opts]
                                 ;; Compare actual selection inputs, not the scorer's
                                 ;; wall-clock occurrence in prediction metadata.
                                 (reset! reader-input {:candidates (mapv #'policy/selection-candidate ranked)
                                                      :opts (dissoc opts :beta :beta-state)})
                                 (select ranked opts))]
                   (fixture/beta-product 1))
                 (catch clojure.lang.ExceptionInfo e {:refusal (ex-data e)}))]
    {:written @written :carrier @carrier :reader-input @reader-input :result result}))
