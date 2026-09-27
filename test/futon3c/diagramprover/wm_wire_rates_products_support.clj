(ns futon3c.diagramprover.wm-wire-rates-products-support
  "Second-layer interventions after the real source, before the real reader."
  (:require [clojure.test :as t]
            [futon2.aif.click-measurement-test :as click]
            [futon2.aif.observation-rates :as rates]
            [futon2.aif.efe :as efe]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire-rates-support :as fixture]))

(defn changed-rates [rs]
  (update rs :t/wanted assoc :false-neg 5/12 :false-pos 5/12))

(defn lane-product [seam mutate]
  (let [source rates/sourced-rates kernel efe/rank-cascade-actions
        result (with-redefs [rates/sourced-rates
                             (fn [& args]
                               (let [r (apply source args)]
                                 (if (= seam :rates) (update r :rates mutate) r)))
                             efe/rank-cascade-actions
                             (fn [state candidates opts]
                               (kernel state candidates
                                       (if (= seam :adjudication-rates)
                                         (update opts :adjudication-rates mutate) opts)))]
                 (fixture/lane))]
    {:G-efe (mapv :G-efe (:ranked result))}))

(defn measured-product [field mutate]
  (let [source rates/sourced-rates]
    (with-redefs [rates/sourced-rates
                  (fn [& args] (update (apply source args) field mutate))]
      (wm/measured-a-version
       [{:target "rates-wire" :cascade-problem fixture/problem}]
       {"rates-wire" (select-keys @fixture/admitted-view [:labels :subjects :prior])}))))

(defn test-report [mutate]
  (let [source rates/sourced-rates reports (atom []) calls (atom 0)]
    (with-redefs [rates/sourced-rates
                  (fn [& args]
                    (swap! calls inc)
                    (update (apply source args) :measurement mutate))
                  t/report #(swap! reports conj %)]
      (click/one-admitted-class-is-measured-the-others-absent))
    {:calls @calls :reports @reports}))
