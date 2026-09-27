(ns futon3c.diagramprover.wm-wire-loop-products-12a
  "Intervene before outer_loop.clj:37-52 resolves the considered entry.
  Selection/seed are forwarded; flight_driver.clj:195-202 stores provenance.
  Plan content remains unchanged under those interventions."
  (:require [clojure.java.io :as io]
            [futon2.aif.outer-cascade :as cascade]
            [futon2.aif.outer-loop :as loop]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.flight-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]))

(def missions
  [{:target "M-futon-seams" :repo "futon3c"
    :path "fixtures/mission-criteria/M-futon-seams@futon3c-d05cb755.md"}
   {:target "M-omni-wm-runner" :repo "futon3c"
    :path "fixtures/mission-criteria/M-omni-wm-runner@futon3c-2114cb99.md"}])
(def field {:considered missions
            :feasible (mapv #(assoc % :eligible true :next-step :read-criteria) missions)
            :exclusions []})

(defn products [key]
  (let [root (w/tmp-dir "loop-products-12a-")
        select-real cascade/select plan-real driver/plan]
    (try
      (let [run (fn [mutate]
                  (let [written (atom nil) handed (atom nil)]
                    (with-redefs [cascade/select (fn [opts]
                                                  (let [s (select-real opts)]
                                                    (reset! written s) (mutate s)))
                                  driver/plan (fn [opts] (reset! handed opts) (plan-real opts))
                                  runner/latest-displacement (fn [& _] {:absent :offline})]
                      (let [r (loop/plan-from-field!
                                {:seed 42 :trigger :wallclock-cron :seat "fixture"
                                 :load-field-fn (fn [] {:field field :opts {:store root :sources {}}})
                                 :plan-opts {:id "loop-products-12a"
                                             :observe (constantly false)
                                             :read-text (fn [_ _ path]
                                                          (when path (slurp (io/resource path))))}})]
                        {:written @written
                         :handed (select-keys @handed [:chosen-target :repo :path :draw-seed :target-selection])
                         :result r}))))
            a (run identity)
            b (run (case key
                     :chosen-target #(assoc % :chosen-target
                                            (:target (first (remove (fn [m] (= (:target m) (:chosen-target %))) missions))))
                     :draw-seed #(assoc % :draw-seed 43)
                     :target-selection #(assoc-in % [:target-selection :trigger] :different-trigger)
                     :missing #(assoc % :chosen-target "M-not-in-field")))]
        [a b])
      (finally (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true))))))
