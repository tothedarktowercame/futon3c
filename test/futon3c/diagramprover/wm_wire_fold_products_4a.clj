(ns futon3c.diagramprover.wm-wire-fold-products-4a
  "Second-layer fold products. External ports remain isolated by fold-in;
   judge, the morning fold, trace writer and arena update all run unchanged."
  (:require [clojure.test :refer [is]]
            [futon2.report.war-machine :as wm]
            [futon2.aif.belief :as belief]
            [futon3c.diagramprover.wm-wire-fold-in-support :as in]
            [futon3c.diagramprover.wm-wire-fold-out-support :as out]))

(def scan {:annotation-graph {:health 0.9}})

(defn updated [kind mutation]
  (let [judge wm/judge update-events wm/apply-arena-belief-events
        updates (atom [])
        result (with-redefs [wm/judge (fn [_ opts] (judge scan opts))
                            wm/apply-arena-belief-events
                            (fn [prior events]
                              (let [posterior (update-events prior events)]
                                ;; Fixture QA events have an event-id; capture only the
                                ;; judge's subsequently synthesized microstep events.
                                (when (and (seq events) (not-any? :event-id events))
                                  (swap! updates conj {:prior prior :events events
                                                      :posterior posterior}))
                                posterior))]
                 (in/observe kind mutation))]
    (assoc (first @updates) :observation scan :result result)))

(defn assert-updated [kind]
  (let [a (updated kind :none) b (updated kind :different)]
    (is (nil? (get-in a [:result :result :wire-error])))
    (is (nil? (get-in b [:result :result :wire-error])))
    (is (= scan (:observation a) (:observation b)))
    (is (seq (:events a)))
    (is (seq (:events b)))
    (is (not= (:prior a) (:prior b)))
    (is (not= (:prior a) (:posterior a)))
    (is (not= (:prior b) (:posterior b)))
    (is (not= (:posterior a) (:posterior b)))
    (println :fold-product kind {:observation scan
                                :before (:posterior a) :after (:posterior b)})
    [a b]))

(defn assert-carried []
  ;; belief/reconcile-belief-carry (belief.clj:526): domain reconciliation,
  ;; no Bayesian update. Surviving entities retain the posterior verbatim.
  (let [a (out/simple :carry :none) b (out/simple :carry :different)
        other (wm/apply-arena-belief-events
               (belief/initial-belief-state ["known"])
               [(assoc (first in/events) :type :foreclosed)])]
    (is (= (:writer a) (:reader a) (get-in a [:result :belief-pre])))
    (is (= other (:reader b) (get-in b [:result :belief-pre])))
    (is (not= (:reader a) (:reader b)))
    ;; The domain product is unchanged; only the copied posterior varies.
    (is (= #{"known"} (set (keys (:reader a))) (set (keys (:reader b)))))
    (is (= (:fresh a) (:fresh b)))
    (println :carry-product {:before (:reader a) :after (:reader b)})
    [a b]))
