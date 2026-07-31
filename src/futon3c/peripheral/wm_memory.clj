(ns futon3c.peripheral.wm-memory
  "Dark War Machine adapter over the shared typed-memory seam.

   This namespace owns no store and defines no second record type. It stamps a
   WM controller episode into the ordinary memory_record payload, recalls
   concrete bodies through the ordinary endpoint reader, and hands them to the
   pure mission-control graph projection."
  (:require [clojure.string :as str]
            [futon2.aif.memory-contract :as memory-contract]
            [futon2.aif.mission-control-graph :as mission-graph]
            [futon3c.evidence.store :as evidence-store]
            [futon3c.peripheral.memory-recall :as memory-recall]
            [futon3c.peripheral.memory-write :as memory-write])
  (:import [java.util UUID]))

(def episode-kinds #{:observation :intervention})
(def external-witness-statuses #{:independently-witnessed :challenged})

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

(defn decision-keyed-external-check-entry
  "Construct an append-only WM external check keyed to one exact decision.

   A mission, session, or timestamp is not a substitute join key. Checkers
   must supply their own identity; this constructor never invents one."
  [{:keys [evidence-id decision-id author session-id at outcome witness-status
           checker]}]
  (when-not (and (every? nonblank-string?
                         [evidence-id decision-id author session-id at checker])
                 (keyword? outcome)
                 (contains? external-witness-statuses witness-status))
    (throw (ex-info "invalid decision-keyed WM external check"
                    {:evidence-id evidence-id
                     :decision-id decision-id
                     :author author
                     :session-id session-id
                     :at at
                     :outcome outcome
                     :witness-status witness-status
                     :checker checker})))
  {:evidence/id evidence-id
   :evidence/subject {:ref/type :decision :ref/id decision-id}
   :evidence/type :pattern-outcome
   :evidence/claim-type :observation
   :evidence/author author
   :evidence/session-id session-id
   :evidence/at at
   :evidence/body {:outcome outcome
                   :memory-outcome/witness-status witness-status
                   :checker checker}
   :evidence/tags [:war-machine :external-check]})

(defn record-decision-keyed-external-check!
  "Append a decision-keyed external check to the supplied evidence store."
  [{:keys [evidence-store]} check]
  (when-not evidence-store
    (throw (ex-info "WM external check requires an evidence store" {})))
  (evidence-store/append*
   evidence-store
   (decision-keyed-external-check-entry check)))

(defn- validated-decision-check
  [check]
  (let [decision-id (get-in check [:evidence/subject :ref/id])
        witness-status (get-in check
                               [:evidence/body
                                :memory-outcome/witness-status])]
    (when-not (and (= :decision (get-in check
                                         [:evidence/subject :ref/type]))
                   (nonblank-string? decision-id)
                   (= :pattern-outcome (:evidence/type check))
                   (= :observation (:evidence/claim-type check))
                   (nonblank-string? (:evidence/id check))
                   (nonblank-string? (:evidence/author check))
                   (some #{:external-check} (:evidence/tags check))
                   (keyword? (get-in check [:evidence/body :outcome]))
                   (contains? external-witness-statuses witness-status))
      (throw (ex-info
              "WM external checks must carry an exact decision subject"
              {:check-id (:evidence/id check)
               :subject (:evidence/subject check)})))
    check))

(defn witnessed-projection-triple
  "Join offered -> projection-selected -> witnessed by decision identity only."
  [projection-receipt external-check]
  (let [external-check (validated-decision-check external-check)
        projection-decision (:wm-projection/decision-id projection-receipt)
        check-decision (get-in external-check [:evidence/subject :ref/id])
        offered (:wm-projection/surfaced-ids projection-receipt)
        selected (:wm-projection/projection-selected-ids projection-receipt)]
    (when-not (= :algorithmic-selection
                 (:wm-projection/signal projection-receipt))
      (throw (ex-info "expected a typed WM projection receipt"
                      {:receipt projection-receipt})))
    (when-not (= projection-decision check-decision)
      (throw (ex-info "WM projection and external check decision ids differ"
                      {:projection-decision-id projection-decision
                       :check-decision-id check-decision})))
    (when-not (and (seq offered) (seq selected))
      (throw (ex-info "witnessed WM projection requires offered and selected memories"
                      {:offered offered :projection-selected selected})))
    {:wm-outcome-triple/type :offered-projection-selected-witnessed
     :wm-outcome-triple/decision-id projection-decision
     :wm-outcome-triple/offered-ids offered
     :wm-outcome-triple/projection-selected-ids selected
     :wm-outcome-triple/witness-evidence-id (:evidence/id external-check)
     :wm-outcome-triple/witness-status
     (get-in external-check [:evidence/body
                             :memory-outcome/witness-status])
     :wm-outcome-triple/outcome
     (get-in external-check [:evidence/body :outcome])
     :wm-outcome-triple/checker (:evidence/author external-check)}))

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
        projection-selected-ids (->> (:candidates projection)
                                     (mapcat :memory-ids)
                                     distinct
                                     vec)
        receipt
        (when (and decision-id session-id)
          (memory-contract/wm-projection-receipt
           (cond-> {:decision-id decision-id
                    :session-id session-id
                    :domain :war-machine
                    :surfaced-memory-ids surfaced-ids
                    :projection-selected-memory-ids
                    projection-selected-ids
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
             :receipt-code :futon2.aif.memory-contract/wm-projection-receipt
             :active-control-patterns (vec active-pattern-ids)
             :recalls recalls
             :projection projection
             :live-ordering-changed? false}
      receipt (assoc :projection-receipt receipt))))
