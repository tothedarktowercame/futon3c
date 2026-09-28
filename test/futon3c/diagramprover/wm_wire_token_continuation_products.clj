(ns futon3c.diagramprover.wm-wire-token-continuation-products
  "Second-layer readbacks; no changes to the first-layer shared support."
  (:require [clojure.edn :as edn]
            [futon2.aif.efe :as efe]
            [futon2.report.war-machine :as wm] [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire-token-input-support :as support]))

(defn receipt-product [hop mutation]
  (let [r (support/helper-observe hop mutation)
        receipt (edn/read-string (pr-str (:product r)))]
    {:supplied (support/mutate mutation (:writer r))
     :belief (:continuation-belief receipt)
     :updates (:observation-updates receipt)
     :other-fields (dissoc receipt :continuation-belief)
     :inputs {:stage (support/stage (= hop :initialization-temporal))
              :inspection support/inspection :admission support/admission}}))

(defn mixed-belief [mode q]
  (if (= mode :different)
    (merge-with + (into {} (map (fn [[state mass]] [state (/ mass 2)]) q)) {#{} 1/2})
    q))

(defn decision-product [mutation]
  (let [calls (atom []) inputs (atom nil) real-decision wm-cd/cascade-decision
        real-rank efe/rank-actions
        r (with-redefs [support/mutate mixed-belief
                        wm-cd/cascade-decision
                        (fn [assembled opts]
                          (reset! inputs {:assembled assembled :opts opts})
                          (real-decision assembled opts))
                        efe/rank-actions
                        (fn [state candidates opts]
                          (let [ranked (real-rank state candidates opts)]
                            (when (:prediction-context opts)
                              (swap! calls conj
                                     {:other-inputs {:state (dissoc state :cascade-belief)
                                                     :candidates candidates
                                                     ;; Generated clock citation is not a scoring input.
                                                     :opts (update opts :prediction-context dissoc :occurrence-id)}
                                      :scores (mapv :controller-score ranked)}))
                            ranked))]
            (support/decision-observe :temporal-decision mutation))]
    {:supplied (mixed-belief mutation (:writer r))
     :incoming (:reader r) :calls @calls :decision-inputs @inputs
     :exception (get-in r [:product :exception])}))
