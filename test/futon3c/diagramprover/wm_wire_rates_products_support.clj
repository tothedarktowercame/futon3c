(ns futon3c.diagramprover.wm-wire-rates-products-support
  "Second-layer interventions after the real source, before the real reader."
  (:require [clojure.test :as t]
            [futon2.aif.click-measurement-test :as click]
            [futon2.aif.observation-rates :as rates]
            [futon2.aif.efe :as efe]
             [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire-rates-support :as fixture]))

(defn changed-rates [rs]
  (update rs :t/wanted assoc :false-neg 5/12 :false-pos 5/12))

(defn lane-product [seam mutate]
  (let [source rates/sourced-rates kernel efe/rank-cascade-actions
        writer (atom nil) carrier (atom nil)
        result (with-redefs [rates/sourced-rates
                             (fn [& args]
                               (let [r (apply source args)]
                                 (if (#{:rates :measurement} seam)
                                   (let [v (get r seam) changed (mutate v)]
                                     (reset! writer v)
                                     (reset! carrier changed)
                                     (assoc r seam changed))
                                   r)))
                             efe/rank-cascade-actions
                             (fn [state candidates opts]
                               (kernel state candidates
                                       (if (= seam :adjudication-rates)
                                         (update opts :adjudication-rates mutate) opts)))]
                 (fixture/lane))]
    {:G-efe (mapv :G-efe (:ranked result))
     :writer @writer :carrier @carrier
     :cascade-scoring (:cascade-scoring (meta (:ranked result)))}))

(defn measured-product [field mutate]
  (let [source rates/sourced-rates]
    (with-redefs [rates/sourced-rates
                  (fn [& args] (update (apply source args) field mutate))]
      (wm-cd/measured-a-version
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

(defn measured-record [mutate]
  (let [writer (atom nil) carrier (atom nil)
        record (measured-product :rates
                                 (fn [v]
                                   (let [changed (mutate v)]
                                     (reset! writer v)
                                     (reset! carrier changed)
                                     changed)))]
    {:writer @writer :carrier @carrier :record record}))
