(ns futon3c.diagramprover.causal.guard
  "Live mechanism guard for E2 ablation dispatch (Q3 made executable).

  Discharges the E2 pilot's `:filter-equals-ship` runtime assertion: before
  each ablation dispatch, the CURRENT corpus topology is observed (a store
  query, supplied by the caller — this namespace is pure and takes the
  observation as data) and the filter-equivalence verdict is re-derived from
  the authored memory spec via the same variant machinery the Q3 receipt
  used. Mechanism (b) filter-at-dispatch is licensed exactly while the
  observed topology keeps M-in-store d-separated from the rest of the
  surfaced set; under multi-attachment it is refused with the connecting
  path named. Verdicts are computed per call, never cached — the guard's
  whole point is that its answer changes when the corpus does."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.receipts :as receipts]))

(def topology->variant
  {:star-forest :star-forest
   :populated-graph :populated-graph})

(defn mechanism-verdict
  "Compute the mechanism-(b) license for an observed corpus topology.

  `topology` is `:star-forest` or `:populated-graph` (the caller's store
  probe reduces the live corpus to one of these). Returns
  {:topology ... :licensed? ... :verdict <computed Q3-style verdict>}."
  ([topology] (mechanism-verdict topology
                                 (dag/load-spec receipts/memory-spec-path)))
  ([topology memory]
   (let [variant-key (or (topology->variant topology)
                         (throw (ex-info "Unknown corpus topology observation"
                                         {:topology topology
                                          :known (keys topology->variant)})))
         variant (get (receipts/q3-variants memory) variant-key)
         separated? (dsep/d-separated? variant :M-in-store :V12-minus-M #{})
         witness (if separated?
                   {:paths [] :count 0 :truncated? false}
                   (dsep/connecting-paths variant :M-in-store :V12-minus-M #{}
                                          {:limit 1}))]
     {:topology topology
      :licensed? separated?
      :verdict {:claim :filter-equivalence
                :method :d-sep
                :holds? separated?
                :paths (:paths witness)
                :paths-truncated? (:truncated? witness)}})))

(defn guard!
  "Failing-loudly form for dispatch wrappers.

  Returns the verdict map when mechanism (b) is licensed; throws ex-info
  when it is not, carrying the named divergence path so the refusal is
  self-explaining in the dispatch log."
  ([topology] (guard! topology (dag/load-spec receipts/memory-spec-path)))
  ([topology memory]
   (let [{:keys [licensed?] :as result} (mechanism-verdict topology memory)]
     (if licensed?
       result
       (throw (ex-info
               (str "E2 mechanism (b) filter-at-dispatch is NOT licensed on "
                    "this corpus topology; switch to mechanism (a) or "
                    "measure the (a)-vs-(b) gap")
               result))))))
