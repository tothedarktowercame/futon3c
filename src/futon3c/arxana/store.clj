(ns futon3c.arxana.store
  "D1 storage adapter boundary (ArxanaGraphStore) and a reversible mock.

  Increment 3 implements the interface plus an in-memory mock. Conditional
  persistence (the futon1b conditional command transaction, D7 A2) is owned
  by the futon1b route increment — this adapter reports its absence truthfully
  via `capabilities` and refuses conditional writes with :unsupported rather
  than simulating success (D1 invariant: a capability absent from the live
  adapter produces `unsupported`, never a simulated success)."
  (:require [futon3c.arxana.envelope :as env]))

(defprotocol ArxanaGraphStore
  "D1 storage adapter contract. Errors preserve futon1b layer/reason/context
  beneath :error/detail and add an Arxana category; they never flatten typed
  refusals into nil."
  (get-entity [store entity-id])
  (find-entities [store query])
  (put-entity! [store expected-shape])
  (get-hyperedge [store hyperedge-id])
  (find-hyperedges [store query])
  (put-hyperedges! [store expected-shapes])
  (retract-hyperedge! [store hyperedge-id valid-from])
  (capabilities [store])
  (conditional-write! [store command]
    "Stub seam for the futon1b conditional command transaction
    (expected-head compare + atomic commit). Not implemented here.")
  (bounded-query! [store query] "C3 bounded, watermarked graph read."))

(def mock-capabilities
  "Honest capability report for the in-memory mock: mirrors D1's current
  adapter truth table — no as-of, no CAS, no subscription."
  {:entity-read true
   :entity-write true
   :entity-as-of false
   :entity-compare-and-set false
   :hyperedge-read true
   :hyperedge-write true
   :hyperedge-valid-time true
   :subscription false
   :scope-family-census false})

(defrecord MockStore [state]
  ArxanaGraphStore
  (get-entity [_ entity-id]
    (if-let [e (get-in @state [:entities entity-id])]
      {:ok e}
      {:error (env/error :missing :arxana.store/entity-not-found
                         {:entity/id entity-id} nil)}))

  (find-entities [_ query]
    {:ok (->> (vals (:entities @state))
              (filter (fn [e]
                        (every? (fn [[k v]] (= v (get e k))) query)))
              (vec))})

  (put-entity! [_ expected-shape]
    (let [id (:entity/id expected-shape)]
      (swap! state assoc-in [:entities id] expected-shape)
      {:ok expected-shape}))

  (get-hyperedge [_ hyperedge-id]
    (if-let [hx (get-in @state [:hyperedges hyperedge-id])]
      {:ok hx}
      {:error (env/error :missing :arxana.store/hyperedge-not-found
                         {:hyperedge/id hyperedge-id} nil)}))

  (find-hyperedges [_ query]
    {:ok (->> (vals (:hyperedges @state))
              (filter (fn [hx]
                        (every? (fn [[k v]] (= v (get hx k))) query)))
              (vec))})

  (put-hyperedges! [_ expected-shapes]
    (doseq [hx expected-shapes]
      (swap! state assoc-in [:hyperedges (:hx/id hx)] hx))
    {:ok expected-shapes})

  (retract-hyperedge! [_ hyperedge-id valid-from]
    (if (get-in @state [:hyperedges hyperedge-id])
      (do (swap! state update-in [:hyperedges hyperedge-id]
                 assoc :hx/retracted-at valid-from)
          {:ok {:hx/id hyperedge-id :hx/retracted-at valid-from}})
      {:error (env/error :missing :arxana.store/hyperedge-not-found
                         {:hyperedge/id hyperedge-id} nil)}))

  (capabilities [_] mock-capabilities)

  (conditional-write! [_ command]
    ;; kimi-2 owns the futon1b conditional transaction route. Stub honestly:
    ;; typed :unsupported refusal, never a simulated commit.
    {:error (env/error :unsupported :arxana.store/conditional-write-unavailable
                       {:command/id (:message/id command)
                        :owner "futon1b conditional transaction route (increment 2)"
                        :capability :entity-compare-and-set}
                       [:retry-after-route-available])})

  (bounded-query! [_ query]
    {:error (env/error :unsupported :arxana.store/bounded-query-unavailable
                       {:query query} nil)}))

(defn mock-store []
  (->MockStore (atom {:entities {} :hyperedges {}})))

(defn require-capability
  "Truth gate: succeed only when the store actually reports cap. An absent
  capability yields :unsupported, never a simulated success."
  [store cap]
  (if (true? (get (capabilities store) cap))
    {:ok true}
    {:error (env/error :unsupported :arxana.store/capability-absent
                       {:capability cap
                        :reported (capabilities store)}
                       nil)}))
