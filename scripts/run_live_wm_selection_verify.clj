(require '[clojure.edn :as edn]
         '[futon3c.evidence.futon1b-backend :as futon1b]
         '[futon3c.peripheral.live-wm-selection :as live])

(def root "holes/labs/M-typed-memories/")

(defn read-fixture [name]
  (edn/read-string (slurp (str root name))))

(let [input
      (merge
       (read-fixture "live-wm-selection-input-20260724.edn")
       {:phase5 (read-fixture "phase5-outer-cascade.edn")
        :phase6 (read-fixture "phase6-strategic-outcomes.edn")
        :phase7 (read-fixture "phase7-strategic-policy-shadow.edn")
        :rung2 (read-fixture "rung2-operator-update.edn")})
      result
      (live/run-verification
       {:evidence-store (futon1b/make-futon1b-backend)
        :trace-id (:trace-id input)}
       input)]
  (prn (dissoc result :components)))
