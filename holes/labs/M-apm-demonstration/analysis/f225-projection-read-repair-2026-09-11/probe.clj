(require '[clojure.edn :as edn]
         '[clojure.pprint :as pp]
         '[futon3c.substrate.client :as substrate]
         '[futon3c.substrate.read-health :as health]
         '[futon3c.apm.memory-snapshot :as snapshot])
(def packet "holes/labs/M-apm-demonstration/analysis/f225-projection-read-repair-2026-09-11/")
(def queue (edn/read-string (slurp "data/apm-campaigns/jit-all-open-v3/queue-state.edn")))
(def hold (:store-read/hold queue))
(assert (= "bad82e9694d2ba65a3e0c6f9769f5bffd3863413fa38fcdeb273e0016bf05cdc" (:hold/id hold)))
(def ids ["e-apm-promotion-1d12442254ef2337685e79dfe079120b"
          "e-apm-promotion-e18b959d7fad5124eaf7dff7e872ba0d"])
(def candidates (->> (edn/read-string (slurp "data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f225/snapshots/f225-solver-memory.edn"))
                     :snapshot/memories (filter #(contains? (set ids) (:memory-id %))) vec))
(assert (= (set ids) (set (map :memory-id candidates))))
(def observations (atom []))
(def warnings (atom []))
(defn measured [operation id f summarize]
  (let [start (System/nanoTime) value (f)
        row {:operation operation :memory-id id
             :elapsed-ms (quot (- (System/nanoTime) start) 1000000)
             :result (summarize value)}]
    (swap! observations conj row)
    (locking observations (prn row))
    value))
(defn edge-summary [edges] (mapv #(select-keys % [:hx/id :hx/type]) edges))
(defn comparable [edges] (mapv #(select-keys % [:hx/id :hx/type :hx/endpoints :hx/props]) edges))
(binding [health/*context* {:diagnostic true}
          health/*record-warning!* #(swap! warnings conj %)]
  (doseq [id ids]
    (let [old (measured :generic-endpoint id
                        #(substrate/hyperedges-by-end id {:limit 10 :timeout-ms 5000 :request-budget 2})
                        edge-summary)
          new (measured :projection-endpoint id #(substrate/memory-assertions-by-end id {:timeout-ms 5000}) edge-summary)]
      (assert (not (substrate/partial-result? old)))
      (assert (= (comparable (filterv #(= :memory/assert (:hx/type %)) old)) (comparable new)))))
  ;; Eight full candidate checks, at the production visibility concurrency of four.
  ;; These read existing evidence only; they do not publish or replay the proof.
  (doseq [_ (range 2)]
    (let [jobs (mapv (fn [candidate]
                       (future
                         (measured :full-candidate-visibility (:memory-id candidate)
                                   #(snapshot/candidate-visible? candidate) boolean)))
                     (vec (take 4 (cycle candidates))))]
      (doseq [job jobs] (assert (true? @job))))))
(spit (str packet "diagnostic.edn")
      (with-out-str (pp/pprint {:at (str (java.time.Instant/now))
                               :diagnostic-only true :hold hold
                               :observations @observations :warnings @warnings})))
(assert (empty? @warnings))
(shutdown-agents)
