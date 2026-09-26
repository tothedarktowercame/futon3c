(ns futon3c.peripheral.pull-receipts
  "Append-only receipts for memories offered through runner pull tools.

   These functions observe tool results; they do not alter retrieval, ranking,
   tool availability, or the result returned to the runner."
  (:require [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store])
  (:import [java.time Instant]
           [java.util UUID]))

(def pull-tool-names
  "Read tools whose returned identifiers form the pull-offered denominator.
   library_search is reserved here for the protocol surface even though the
   current ZAI tool catalogue does not expose it."
  #{"memory_search" "pattern_memory" "library_search" "evidence_graph"
    "psr_search"})

(def pull-use-tool-names
  "Memory tools whose returned ids are auditable pull uses. The execution site
   lacks cycle context; cycle traces join these observations through dispatch-id."
  #{"memory_search" "memory_read"})

(defn- returned-ids
  [tool-name result]
  (let [body (:result result)]
    (->> (case tool-name
           "psr_search" (map :pattern-id (:candidates body))
           "library_search" (concat (map :id (:items body))
                                    (map :pattern-id (:candidates body)))
           (map #(or (:id %) (:evidence/id %)) (:items body)))
         (remove nil?)
         (map str)
         distinct
         sort
         vec)))

(defn record-pull-offer!
  "Record one pull-tool result without changing that result.

   Returns the boundary delivery receipt, or nil for a non-pull tool. Empty
   successful result sets are recorded because they remain denominator data."
  [{:keys [evidence-store agent-id session-id dispatch-id turn-id round]}
   tool-name args result]
  (when (contains? pull-tool-names tool-name)
    (let [at (str (Instant/now))
          ids (if (true? (:ok result)) (returned-ids tool-name result) [])]
      (boundary/append!
       evidence-store
       {:evidence/id (str "e-pull-offer-" (UUID/randomUUID))
        :evidence/subject {:ref/type :task :ref/id (str dispatch-id)}
        :evidence/type :coordination
        :evidence/claim-type :observation
        :evidence/author (str agent-id)
        :evidence/at at
        :evidence/session-id (str dispatch-id)
        :evidence/body {:event :memory-pull-offer
                        :dispatch-id (str dispatch-id)
                        :turn-id (str turn-id)
                        :agent-id (str agent-id)
                        :session-id (str session-id)
                        :tool tool-name
                        :args args
                        :round round
                        :at at
                        :pull-surfaced-ids ids
                        :tool-ok? (true? (:ok result))}
        :evidence/tags [:memory-use :pull-offered :tool-call]}))))

(defn pull-offer-receipts
  "Return pull-offer entries for exactly one dispatch, ordered by call round."
  [evidence-store dispatch-id]
  (->> (store/query* evidence-store
                     {:query/subject {:ref/type :task :ref/id (str dispatch-id)}})
       (filter #(and (= :memory-pull-offer (get-in % [:evidence/body :event]))
                     (= (str dispatch-id)
                        (get-in % [:evidence/body :dispatch-id]))))
       (sort-by (juxt #(get-in % [:evidence/body :round]) :evidence/at))
       vec))

(defn pull-surfaced-ids
  "Derive the complete union of pull-offered ids for one dispatch."
  [evidence-store dispatch-id]
  (->> (pull-offer-receipts evidence-store dispatch-id)
       (mapcat #(get-in % [:evidence/body :pull-surfaced-ids]))
       distinct
       sort
       vec))

(defn record-pull-uses!
  "Record one use observation per memory id returned by a pull tool."
  [{:keys [evidence-store agent-id session-id dispatch-id turn-id round]}
   tool-name result]
  (if-not (and (contains? pull-use-tool-names tool-name)
               (seq (str dispatch-id)))
    []
    (mapv
     (fn [memory-id]
       (let [at (str (Instant/now))]
         (boundary/append!
          evidence-store
          {:evidence/id (str "e-pull-use-" (UUID/randomUUID))
           :evidence/subject {:ref/type :task :ref/id (str dispatch-id)}
           :evidence/type :coordination
           :evidence/claim-type :observation
           :evidence/author (str agent-id)
           :evidence/at at
           :evidence/session-id (str dispatch-id)
           :evidence/body {:event :memory-pull-use
                           :memory-id memory-id
                           :dispatch-id (str dispatch-id)
                           :turn-id (str turn-id)
                           :agent-id (str agent-id)
                           :session-id (str session-id)
                           :tool tool-name
                           :round round
                           :at at}
           :evidence/tags [:memory-use :pull-used :tool-call]})))
     (if (true? (:ok result)) (returned-ids tool-name result) []))))

(defn pull-use-receipts
  "Return actual pull-use observations for exactly one dispatch."
  [evidence-store dispatch-id]
  (->> (store/query* evidence-store
                     {:query/subject {:ref/type :task :ref/id (str dispatch-id)}})
       (filter #(and (= :memory-pull-use (get-in % [:evidence/body :event]))
                     (= (str dispatch-id)
                        (get-in % [:evidence/body :dispatch-id]))))
       (sort-by (juxt #(get-in % [:evidence/body :round]) :evidence/at))
       vec))
