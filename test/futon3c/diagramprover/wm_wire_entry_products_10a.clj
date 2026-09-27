(ns futon3c.diagramprover.wm-wire-entry-products-10a
  "resolve-target carries provenance (flight_driver.clj:65-91);
  flight-for (100-110) starts the flight but gets repo/path from independent
  caller options. Provenance does not change its wants or observations."
  (:require [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.outer-cascade :as outer]
            [futon3c.diagramprover.wm-wire :as w]))

(def missions
  [{:target "M-futon-seams"
    :path "fixtures/mission-criteria/M-futon-seams@futon3c-d05cb755.md"}
   {:target "M-omni-wm-runner"
    :path "fixtures/mission-criteria/M-omni-wm-runner@futon3c-2114cb99.md"}])

(defn products [mission field]
  (let [root (w/tmp-dir "entry-products-10a-")]
    (try
      (let [{:keys [target path]} mission
            text (slurp (io/resource path))
            field-data {:considered [{:target target :kind :mission}]
                        :feasible [{:target target :kind :mission :eligible true
                                    :next-step :read-criteria}] :exclusions []}
            selection (outer/select {:field field-data :seed 42 :trigger :wallclock-cron})
            changed (case field
                      :draw-seed (assoc selection :draw-seed 43)
                      :target-selection (assoc-in selection [:target-selection :trigger] :different-trigger))
            opts {:repo "fixture" :path path :store root :id "entry-products-10a"
                  :read-text (fn [& _] text) :observe (constantly false)}
            a (#'driver/flight-for (merge opts selection))
            b (#'driver/flight-for (merge opts changed))
            ca (flight/click-wants a {})
            cb (flight/click-wants b {})]
        {:written [(get selection field) (get changed field)]
         :products [(get a field) (get b field)]
         :flights [(dissoc a field) (dissoc b field)]
         :wants [ca cb]
         :target [(:target a) (:target b)]
         :path [(get-in a [:want-source :path]) (get-in b [:want-source :path])]})
      (finally
        (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true))))))
