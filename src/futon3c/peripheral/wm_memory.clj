(ns futon3c.peripheral.wm-memory
  "Dark War Machine adapter over the shared typed-memory seam.

   This namespace owns no store and defines no second record type. It stamps a
   WM controller episode into the ordinary memory_record payload, recalls
   concrete bodies through the ordinary endpoint reader, and hands them to the
   pure mission-control graph projection."
  (:require [clojure.string :as str]
            [futon2.aif.memory-contract :as memory-contract]
            [futon2.aif.mission-control-graph :as mission-graph]
            [futon3c.peripheral.memory-recall :as memory-recall]
            [futon3c.peripheral.memory-write :as memory-write])
  (:import [java.util UUID]))

(def episode-kinds #{:observation :intervention})

(defn- nonblank-string?
  [value]
  (and (string? value) (not (str/blank? value))))

(defn episode-payload
  "Validate and translate one controller episode into memory_record input.

   Pattern, mission, event-kind, and witness ids are all materialized as
   hyperedge endpoints by the shared writer."
  [{:keys [mission-id control-pattern-ids event-kind witness-ids
           name hook body why how-to-apply volatile? facets]}]
  (when-not (and (nonblank-string? mission-id)
                 (vector? control-pattern-ids)
                 (seq control-pattern-ids)
                 (every? mission-graph/valid-control-pattern-id?
                         control-pattern-ids)
                 (contains? episode-kinds event-kind)
                 (vector? witness-ids)
                 (seq witness-ids)
                 (every? nonblank-string? witness-ids)
                 (nonblank-string? name)
                 (nonblank-string? hook)
                 (or (string? body) (map? body)))
    (throw (ex-info "invalid War Machine memory episode"
                    {:mission-id mission-id
                     :control-pattern-ids control-pattern-ids
                     :event-kind event-kind
                     :witness-ids witness-ids})))
  (cond->
   {:name name
    :hook hook
    :kind event-kind
    :body body
    :subjects
    (vec
     (concat
      [{:ref/type :mission :ref/id mission-id}
       {:ref/type :wm-event
        :ref/id (str "wm/event/" (clojure.core/name event-kind))}]
      (map (fn [pattern-id]
             {:ref/type :pattern :ref/id pattern-id})
           control-pattern-ids)
      (map (fn [witness-id]
             {:ref/type :witness :ref/id witness-id})
           witness-ids)))
    :volatile? (boolean volatile?)
    :facets (vec (or facets []))}
    why (assoc :why why)
    how-to-apply (assoc :how-to-apply how-to-apply)))

(defn record-episode!
  "Write a WM episode through memory_write/record-memory!.

   Identity and domain are controller-stamped. The default self-asserted status
   is honest for a controller-authored episode; a reviewer/outcome path may
   later promote or supersede its attachment."
  [{:keys [record-memory-fn] :as ctx} episode]
  (let [record-memory-fn (or record-memory-fn memory-write/record-memory!)
        mission-id (:mission-id episode)]
    (record-memory-fn
     (-> ctx
         (dissoc :record-memory-fn)
         (assoc :domain :war-machine
                :mission-id mission-id
                :witness-status
                (or (:witness-status ctx) :self-asserted)))
     (episode-payload episode))))

(defn dark-candidate-projection
  "Recall full WM bodies for active p4ng endpoints and project dark candidates.

   LIVE ordering is untouched: the return value is a detached audit product.
   Callers may inject :recall-fn for fixtures; production uses the same
   memory-recall/recall-by-endpoint seam as Zaif selection."
  [{:keys [evidence-store recall-fn trace-id]}
   active-pattern-ids
   control-edges
   {:keys [limit decision-id session-id] :or {limit 10}}]
  (let [recall-fn (or recall-fn memory-recall/recall-by-endpoint)
        trace-id (or trace-id (str "wm-memory-" (UUID/randomUUID)))
        recalls
        (mapv
         #(recall-fn
           {:domain :war-machine :evidence-store evidence-store}
           %
           {:limit limit :include-bodies? true :trace-id trace-id})
         active-pattern-ids)
        memories (->> recalls
                      (mapcat :memories)
                      (reduce (fn [by-id memory]
                                (assoc by-id (:memory/id memory) memory))
                              {})
                      vals
                      (sort-by :memory/id)
                      vec)
        projection
        (mission-graph/candidate-projection
         active-pattern-ids control-edges memories)
        surfaced-ids (mapv :memory/id memories)
        used-ids (->> (:candidates projection)
                      (mapcat :memory-ids)
                      distinct
                      vec)
        receipt
        (when (and decision-id session-id)
          (memory-contract/use-receipt
           (cond-> {:decision-id decision-id
                    :session-id session-id
                    :domain :war-machine
                    :surfaced-memory-ids surfaced-ids
                    :used-memory-ids used-ids
                    :inclusion-reasons
                    (into {} (map (fn [memory-id]
                                    [memory-id
                                     "reviewed active p4ng endpoint attachment"])
                                  surfaced-ids))}
             (= 1 (count active-pattern-ids))
             (assoc :pattern-id (first active-pattern-ids))
             (not= 1 (count active-pattern-ids))
             (assoc :cascade-id trace-id))))]
    (cond-> {:status :dark
             :trace-id trace-id
             :query-code :shared-memory/recall-by-endpoint
             :receipt-code :futon2.aif.memory-contract/use-receipt
             :active-control-patterns (vec active-pattern-ids)
             :recalls recalls
             :projection projection
             :live-ordering-changed? false}
      receipt (assoc :memory-use-receipt receipt))))
