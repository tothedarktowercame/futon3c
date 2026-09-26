(ns futon3c.wm.run4-battery
  "Finite RUN4 preregistration battery derived from validated terminal bundles."
  (:require [futon2.aif.run4-route-conformance :as route]
            [futon2.aif.step-acceptance :as step]))

(def check-ids [:route/nonempty :route/no-empty :route/no-unmapped :route/no-refutation])

(defn produce [series-id series-sha control-sha bundles control-map]
  (let [rows
        (mapcat
         (fn [bundle]
           (let [record (:run-record bundle)
                 verdict (route/verdict control-map record)
                 trial-id (str (get-in bundle [:identity :trial-id]))
                 run-id (:run/id record)
                 values {:route/nonempty (seq (:hops verdict))
                         :route/no-empty (seq (:route-nodes verdict))
                         :route/no-unmapped (empty? (:unmapped-hops verdict))
                         :route/no-refutation (empty? (:refutations verdict))}]
             (mapv (fn [check-id]
                     {:row/run-id run-id :row/trial-id trial-id
                      :row/check-id check-id
                      :row/verdict (if (values check-id) :green :red)})
                   check-ids))) bundles)]
    {:schema :wm/run4-battery-v1 :series-id series-id
     :series-sha256 series-sha :control-map-sha256 control-sha
     :rows (vec rows)}))

(defn validate
  "Require the exact trial × preregistered-check population, identity joins,
  and green results, then exercise existing step-acceptance red semantics in
  memory. Returns false for every incomplete/foreign/duplicate population."
  [battery series-id series-sha control-sha bundles]
  (let [expected (set (for [b bundles c check-ids]
                        [(str (get-in b [:identity :trial-id]))
                         (get-in b [:run-record :run/id]) c]))
        rows (:rows battery)
        actual (when (vector? rows)
                 (mapv (juxt :row/trial-id :row/run-id :row/check-id) rows))]
    (boolean
     (and (= :wm/run4-battery-v1 (:schema battery))
          (= series-id (:series-id battery)) (= series-sha (:series-sha256 battery))
          (= control-sha (:control-map-sha256 battery))
          (seq expected) (= (count actual) (count (distinct actual)))
          (= expected (set actual))
          (every? (fn [run-id]
                    (try
                      (step/advance-pin {:pin/generation 0 :pin/accepted-steps []}
                                        rows :report-only run-id "read-only" nil "now")
                      true
                      (catch clojure.lang.ExceptionInfo _ false)))
                  (set (map :row/run-id rows)))
          (every? #(= :green (:row/verdict %)) rows)))))
