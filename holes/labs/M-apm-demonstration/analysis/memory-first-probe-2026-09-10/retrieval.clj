(require '[futon3c.peripheral.memory-recall :as recall]
         '[cheshire.core :as json])
(def mid "e-apm-promotion-5fdb99169bd788313841375c797c302c")
(def results
  (mapv (fn [q]
          (let [r (recall/propose-patterns-by-query {:domain :mathematics} q
                    {:limit 10 :trace-id "codex17-memory-first-diagnostic-20260910"})
                ids (mapv :memory/id (:content-matches r))]
            {:query q :result r :target-memory mid
             :target-content-rank (first (keep-indexed #(when (= %2 mid) (inc %1)) ids))}))
        ["ODE uniqueness endpoint" "Gronwall" "interior-gronwall-to-endpoint-by-one-sided-limit"]))
(spit "/tmp/apm-memory-first-search-20260910.json" (json/generate-string results {:pretty true}))
(doseq [r results] (prn (select-keys r [:query :target-content-rank])))
(shutdown-agents)
