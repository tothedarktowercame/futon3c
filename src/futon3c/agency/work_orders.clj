(ns futon3c.agency.work-orders
  "W1 ledger for E-agency-work-orders: a token ledger of work orders.

   A work bell (mode work) from a registered agent OPENS an order (E1);
   the debtor's job reaching a terminal state DELIVERS it (E2) and the
   auto-bellback job's terminal state CLOSES it (:fulfil). When no bellback
   is sent (disabled/suppressed), the order closes at delivery.

   Order shape (holes/excursions/E-agency-work-orders.md, commit 6819c30d):

     {:id :requester :debtor :parent :job-id :text :opened-at
      :state (:open | :delivered | :closed) :closed-by :nudges [...]}

   :text is the first 400 chars of the prompt. A token is always held by
   exactly one agent: the debtor while :open, the requester while
   :delivered, nobody once :closed (holder-of).

   The transition functions (open-order, deliver, close, holder-of,
   orders-for, parent-order) are PURE over a map of id -> order; the
   impure wrappers (!orders, maybe-open-order!, job-terminal!, list-orders)
   are the only parts the HTTP transport touches. Every open/close also
   appends a P24 act to evidence through the single boundary, off the
   request path (a future), and never fails the bell that triggered it.

   W3 adds movement stamps, recorded nudge/escalation actions, and explicit
   authorized release; transport wiring owns notification delivery and HTTP."
  (:refer-clojure :exclude [deliver])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary])
  (:import [java.time Instant]
           [java.util UUID]))

(def order-states #{:open :delivered :closed})

(defn- now-str [] (str (Instant/now)))
(defn- now-ms [] (System/currentTimeMillis))

(defn- new-order-id [] (str "wo-" (UUID/randomUUID)))

(defn- truncate-text
  [text]
  (let [s (str (or text ""))]
    (subs s 0 (min 400 (count s)))))

;; ---------------------------------------------------------------------------
;; Pure transitions (tested directly; no atoms, no IO)

(defn open-order
  "Pure: add a fresh :open order to ORDERS (map id -> order).
   Returns [orders' order]."
  [orders {:keys [requester debtor parent job-id text]}]
  (let [order {:id (new-order-id)
               :requester requester
               :debtor debtor
               :parent parent
               :job-id job-id
               :text (truncate-text text)
               :opened-at (now-str)
               :moved-at (now-ms)
               :state :open
               :closed-by nil
               :nudges []}]
    [(assoc orders (:id order) order) order]))

(defn deliver
  "Pure: mark the open order ID :delivered, remembering BELLBACK-JOB-ID (the
   job carrying the result back to the requester; nil when none is sent).
   Orders not :open are returned unchanged."
  [orders id bellback-job-id]
  (if (= :open (get-in orders [id :state]))
    (-> orders
        (assoc-in [id :state] :delivered)
        (assoc-in [id :bellback-job-id] bellback-job-id)
        (assoc-in [id :moved-at] (now-ms)))
    orders))

(defn close
  "Pure: close order ID from :open or :delivered with CLOSED-BY (e.g.
   :fulfil). Already-closed or missing orders are returned unchanged."
  [orders id closed-by]
  (if (contains? #{:open :delivered} (get-in orders [id :state]))
    (-> orders
        (assoc-in [id :state] :closed)
        (assoc-in [id :closed-by] closed-by)
        (assoc-in [id :moved-at] (now-ms)))
    orders))

(defn holder-of
  "The agent currently holding the token: the debtor while :open, the
   requester while :delivered, nobody once :closed."
  [order]
  (case (:state order)
    :open (:debtor order)
    :delivered (:requester order)
    nil))

(defn orders-for
  "Pure: orders where AGENT is debtor or requester (all orders when AGENT is
   nil); STATE (keyword or string) filters when given. Sorted by :opened-at."
  [orders {:keys [agent state]}]
  (let [state-k (some-> state name keyword)]
    (->> (vals orders)
         (filter (fn [o]
                   (and (or (nil? agent)
                            (= agent (:debtor o))
                            (= agent (:requester o)))
                        (or (nil? state-k)
                            (= state-k (:state o))))))
         (sort-by :opened-at))))

(defn parent-order
  "Pure: the order the CALLER is working on — the open order whose debtor is
   CALLER and whose :job-id is RUNNING-JOB-ID; falling back to the caller's
   open root (debtor = caller, :parent nil). nil when the caller holds no
   open order."
  [orders caller running-job-id]
  (let [held (filter #(and (= caller (:debtor %))
                           (= :open (:state %)))
                     (vals orders))]
    (or (some (fn [o] (when (and (some? running-job-id)
                                 (= running-job-id (:job-id o)))
                        o))
              held)
        (some (fn [o] (when (nil? (:parent o)) o)) held))))

;; ---------------------------------------------------------------------------
;; Impure ledger (the atom the HTTP transport reads/writes)

;; The live ledger: map order-id -> order.
(defonce !orders (atom {}))

(def ^:dynamic *append-act!*
  "Off-request-path evidence append for P24 acts (rebound in tests). The
   default posts through the single evidence boundary on a future and only
   ever prints on failure — a work-orders evidence problem must never fail
   the bell or the job transition that triggered it."
  (fn [act-entry]
    (future
      (try
        (boundary/append-default! act-entry)
        (catch Throwable t
          (binding [*out* *err*]
            (println (str "[work-orders] act append failed: " (.getMessage t)))))))))

(defn- append-act!
  "Append one P24 act (:promise on open, :fulfil on close) for ORDER."
  [kind order extra]
  (*append-act!*
   {:evidence/id (str "e-wo-" (UUID/randomUUID))
    :evidence/subject {:ref/type :task :ref/id (:id order)}
    :evidence/type :coordination
    :evidence/claim-type :assert
    :evidence/author "work-orders"
    :evidence/at (now-str)
    :evidence/body (merge {:id (str "wo-act-" (UUID/randomUUID))
                           :kind kind
                           :author "work-orders"
                           :at (now-str)
                           :order-id (:id order)}
                          extra)
    :evidence/tags [:work-orders :p24]}))

(def non-order-callers
  "Bell callers that never open orders: loop-safety identities and the
   operator (whose requests arrive as chain roots opened for real agents,
   not as orders against him)."
  #{"auto-bellback" "http-caller" "joe"})

(defn maybe-open-order!
  "E1: open an order for a work bell. MODE must be \"work\", CALLER a
   registered agent outside non-order-callers. The new order's parent is the
   order the caller is currently running (parent-order over
   RUNNING-JOB-ID-FN); if the caller holds no open order at all, a chain root
   {:requester \"joe\" :debtor caller :parent nil :text \"chain root\"} is
   opened first and the new order hangs under it.

   Returns the vector of opened orders (child last), or nil when no order
   was opened. Never throws — a ledger failure must not fail the bell."
  [{:keys [caller debtor job-id prompt mode registered? running-job-id-fn]}]
  (try
    (when (and (= "work" (some-> mode str))
               (not (contains? non-order-callers (str caller)))
               (registered? caller))
      (let [running-job-id (try (running-job-id-fn caller)
                                (catch Throwable _ nil))
            [before after]
            (swap-vals! !orders
                        (fn [orders]
                          (let [parent (parent-order orders caller running-job-id)
                                [orders' parent']
                                (if parent
                                  [orders parent]
                                  (open-order orders {:requester "joe"
                                                      :debtor caller
                                                      :parent nil
                                                      :job-id nil
                                                      :text "chain root"}))
                                [orders'' _order]
                                (open-order orders' {:requester caller
                                                     :debtor debtor
                                                     :parent (:id parent')
                                                     :job-id job-id
                                                     :text prompt})]
                            (assoc-in orders'' [(:id parent') :moved-at] (now-ms)))))
            opened-ids (set/difference (set (keys after)) (set (keys before)))
            opened (mapv after (sort-by #(get-in after [% :opened-at]) opened-ids))]
        (doseq [o opened]
          (append-act! :promise o {:to (:debtor o)
                                   :requester (:requester o)
                                   :text (:text o)}))
        opened))
    (catch Throwable t
      (binding [*out* *err*]
        (println (str "[work-orders] open failed (bell unaffected): " (.getMessage t))))
      nil)))

(defn job-terminal!
  "E2: JOB-ID reached a terminal state.

   - An :open order whose :job-id is JOB-ID becomes :delivered, remembering
     BELLBACK-JOB-ID (the auto-bellback job carrying the result to the
     requester). When BELLBACK-JOB-ID is nil (bellback disabled or
     suppressed), the order closes at delivery (:closed-by :fulfil).
   - A :delivered order whose :bellback-job-id is JOB-ID closes
     (:closed-by :fulfil) — the delivery reached the requester.

   Returns the vector of orders closed by this call. Never throws."
  [{:keys [job-id bellback-job-id]}]
  (try
    (when (and (string? job-id) (not (str/blank? job-id)))
      (let [[before after]
            (swap-vals! !orders
                        (fn [orders]
                          (let [delivering (filter #(= job-id (:job-id %)) (vals orders))
                                closing (filter #(= job-id (:bellback-job-id %)) (vals orders))
                                orders' (reduce
                                         (fn [os o]
                                           (let [os' (deliver os (:id o) bellback-job-id)]
                                             (if (str/blank? (str bellback-job-id))
                                               (close os' (:id o) :fulfil)
                                               os')))
                                         orders delivering)]
                            (reduce (fn [os o] (close os (:id o) :fulfil))
                                    orders' closing))))
            newly-closed (->> (vals after)
                              (filter #(and (= :closed (:state %))
                                            (not= :closed (get-in before [(:id %) :state]))))
                              (sort-by :opened-at)
                              vec)]
        (doseq [o newly-closed]
          (append-act! :fulfil o {:requester (:requester o)
                                  :debtor (:debtor o)
                                  :closed-by (:closed-by o)}))
        newly-closed))
    (catch Throwable t
      (binding [*out* *err*]
        (println (str "[work-orders] terminal hook failed for " job-id ": "
                      (.getMessage t))))
      nil)))

(defn list-orders
  "Orders matching {:agent (debtor or requester) :state}, each with :holder
   computed (holder-of)."
  [{:keys [agent state escalated]}]
  (mapv (fn [o] (assoc o :holder (holder-of o)))
        (cond->> (orders-for @!orders {:agent agent :state state})
          escalated (filter #(some (fn [n] (= :escalate (:kind n))) (:nudges %))))))

(defn record-action!
  "Record a nudge or escalation on ID. Returns the updated order, or nil."
  [id {:keys [to kind] :as action}]
  (when (contains? #{:nudge :escalate} kind)
    (let [entry {:at (or (:at action) (now-ms)) :to to :kind kind}]
      (get (swap! !orders
                  (fn [orders]
                    (if (contains? orders id)
                      (update-in orders [id :nudges] (fnil conj []) entry)
                      orders)))
           id))))

(defn append-problem-report!
  "Append the operator-facing evidence act for a joe escalation."
  [id text]
  (when-let [order (get @!orders id)]
    (append-act! :report-problem order {:requester "joe"
                                        :debtor (:debtor order)
                                        :text text})))

(defn close-order!
  "Explicitly release ID. Debtor, requester, or joe may close it. Refuses an
   open child unless FORCE is true. Returns a result map for the HTTP boundary."
  [{:keys [id by reason force]}]
  (let [result (atom nil)
        [before after]
        (swap-vals! !orders
                    (fn [orders]
                      (let [order (get orders id)
                            authorized? (and order (contains? (hash-set (:debtor order)
                                                                        (:requester order) "joe") by))
                            open-child? (some #(and (= id (:parent %))
                                                    (contains? #{:open :delivered} (:state %)))
                                              (vals orders))]
                        (cond
                          (nil? order) (do (reset! result {:ok false :status 404 :error :not-found}) orders)
                          (not authorized?) (do (reset! result {:ok false :status 403 :error :forbidden}) orders)
                          (= :closed (:state order)) (do (reset! result {:ok true :status 200 :order order}) orders)
                          (and open-child? (not force))
                          (do (reset! result {:ok false :status 409 :error :open-child}) orders)
                          :else (let [closed (close orders id by)]
                                  (reset! result {:ok true :status 200 :order (get closed id)})
                                  closed)))))]
    (when (and (:ok @result)
               (not= :closed (get-in before [id :state]))
               (= :closed (get-in after [id :state])))
      (append-act! :release (get after id) {:by by :reason reason}))
    @result))

(defn reset-ledger!
  "Test support: empty the ledger."
  []
  (reset! !orders {}))
