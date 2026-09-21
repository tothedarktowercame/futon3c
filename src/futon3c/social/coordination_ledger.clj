(ns futon3c.social.coordination-ledger
  "Mesh-edge evidence for agent-to-agent coordination.

   This records direct invokes in the social layer without changing
   futon3c.agency.registry/invoke-agent!, which remains the hot path. Each
   wrapped invoke emits exactly one :invoke edge before the call and exactly one
   :invoke-result edge after the call, with nil/blank callers normalized to
   \"unknown\" rather than dropped."
  (:require [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]
           [java.util UUID]
           [futon3c.evidence.backend AtomBackend]
           [futon3c.evidence.futon1b_backend Futon1bBackend]))

(def ^:dynamic *test-evidence-store*
  "Explicit unit-test store binding. Production never falls back to an atom."
  nil)

(defn- dev-evidence-store
  "Read the existing boot authority without loading dev or creating a store."
  []
  (when-let [dev-ns (find-ns 'futon3c.dev)]
    (when-let [store-var (ns-resolve dev-ns '!evidence-store)]
      @(var-get store-var))))

(defn mesh-evidence-store
  "Resolve and validate the mesh backend before performing any work.
   Omitted stores use dev's boot-configured authority. Volatile stores require
   an explicit *test-evidence-store* binding; nil never selects estore/!store."
  ([] (mesh-evidence-store nil))
  ([store]
   (let [store (or store *test-evidence-store* (dev-evidence-store))]
     (if (or (instance? Futon1bBackend store)
             (and *test-evidence-store*
                  (or (instance? clojure.lang.IAtom store)
                      (instance? AtomBackend store))))
       store
       (throw (ex-info "Mesh evidence requires the configured durable backend"
                       {:error/code :mesh/non-durable-evidence-store
                        :store-kind (cond
                                      (nil? store) :missing
                                      (instance? AtomBackend store) :atom-backend
                                      (instance? clojure.lang.IAtom store) :raw-atom
                                      :else :unsupported)}))))))

(defn- now-str []
  (str (Instant/now)))

(defn normalize-agent-id
  "Return a non-blank agent identifier string, or nil."
  [x]
  (let [v (cond
            (nil? x) nil
            (map? x) (or (:id/value x) (:agent/id x) (:agent-id x) (:id x)
                         (get x "id/value") (get x "agent/id")
                         (get x "agent-id") (get x "id"))
            :else x)
        s (some-> v str str/trim)]
    (when-not (str/blank? s) s)))

(defn normalize-from
  "Nil/blank caller is still evidence: record it as \"unknown\"."
  [from]
  (or (normalize-agent-id from) "unknown"))

(defn normalize-surface
  [surface]
  (or (normalize-agent-id surface) "unknown"))

(defn- event-kind
  [kind]
  (case kind
    :invoke :invoke
    "invoke" :invoke
    :invoke-result :invoke-result
    "invoke-result" :invoke-result
    :invoke))

(defn make-mesh-edge-evidence
  "Build the social-layer mesh-edge evidence entry. Mirrors
   futon3c.social.bells/make-bell-evidence but uses :mesh-edge tags and typed
   edge fields consumed by /api/alpha/coordination/edges and mesh_trace.py."
  [{:keys [from to surface kind ok? error at session-id edge-id]}]
  (let [from* (normalize-from from)
        to* (or (normalize-agent-id to) "unknown")
        surface* (normalize-surface surface)
        kind* (event-kind kind)
        at* (or at (now-str))
        edge-id* (or edge-id session-id (str "mesh-edge-" (UUID/randomUUID)))]
    {:evidence/id (str "e-" (UUID/randomUUID))
     :evidence/subject {:ref/type :agent :ref/id to*}
     :evidence/type :coordination
     :evidence/claim-type :step
     :evidence/author from*
     :evidence/at at*
     :evidence/body (cond-> {:edge/id edge-id*
                             :edge/kind kind*
                             :edge/from from*
                             :edge/to to*
                             :edge/surface surface*
                             :edge/at at*}
                      (some? ok?) (assoc :edge/ok? (boolean ok?))
                      error (assoc :edge/error (str error)))
     :evidence/tags [:coordination :mesh-edge]
     :evidence/session-id edge-id*}))

(defn record-invoke-edge!
  "Append a mesh edge to the explicit or boot-configured durable backend."
  [{:keys [evidence-store] :as edge}]
  (boundary/append! (mesh-evidence-store evidence-store)
                    (make-mesh-edge-evidence edge)))

(defn- refuse-scheduled-dispatch!
  [code message data]
  (throw (ex-info message
                  (merge {:error/type :process-assurance-refusal
                          :error/code code
                          :node :R10}
                         data))))

(defn run-scheduled-dispatch!
  "R10 scheduled-entrypoint boundary: require a named commission, dispatch it,
   and durably record the receipt joined to that commission. DISPATCH-FN receives
   the R10-linked commission and must echo both :node and :commission/id in its
   receipt; an unlinked or missing receipt refuses the scheduled run."
  [{:keys [commission dispatch-fn evidence-store]}]
  (let [commission-id (some-> (:commission/id commission) str str/trim not-empty)]
    (when-not (and commission-id (fn? dispatch-fn))
      (refuse-scheduled-dispatch!
       :r10/invalid-commission
       "R10 scheduled dispatch requires a commission identity and dispatch function"
       {:commission commission}))
    (let [linked-commission (assoc commission :node :R10)
          receipt (dispatch-fn linked-commission)]
      (when-not (and (map? receipt)
                     (= :R10 (:node receipt))
                     (= commission-id (some-> (:commission/id receipt) str))
                     (some-> (:dispatch/id receipt) str str/trim not-empty))
        (refuse-scheduled-dispatch!
         :r10/unlinked-dispatch-receipt
         "R10 scheduled dispatch receipt must identify its dispatch and commission"
         {:commission/id commission-id :receipt receipt}))
      (let [at (now-str)
            entry {:evidence/id (str "e-" (UUID/randomUUID))
                   :evidence/subject {:ref/type :task :ref/id commission-id}
                   :evidence/type :coordination
                   :evidence/claim-type :step
                   :evidence/author (normalize-from (:commission/from commission))
                   :evidence/at at
                   :evidence/body {:node :R10
                                   :process/stage :dispatched
                                   :commission/id commission-id
                                   :commission linked-commission
                                   :dispatch/receipt receipt}
                   :evidence/tags [:coordination :scheduled-dispatch :R10]
                   :evidence/session-id (str (:dispatch/id receipt))}
            recorded (boundary/append! (or evidence-store estore/!store) entry)]
        (when-not (:ok recorded)
          (refuse-scheduled-dispatch!
           :r10/recording-failed
           "R10 scheduled dispatch receipt was not recorded"
           {:commission/id commission-id :dispatch/receipt receipt
            :recording-result recorded}))
        {:ok true :commission linked-commission :receipt receipt
         :evidence/id (:evidence/id entry)}))))

(defn invoke-with-edge!
  "Invoke an agent and record the social mesh edge around it.

   Preserves registry/invoke-agent!'s return value and exception semantics."
  [{:keys [from to surface prompt timeout-ms evidence-store]}]
  (let [from* (normalize-from from)
        to* (or (normalize-agent-id to) "unknown")
        surface* (normalize-surface surface)
        edge-id (str "mesh-edge-" (UUID/randomUUID))
        base {:from from* :to to* :surface surface* :edge-id edge-id
              :session-id edge-id :evidence-store evidence-store}]
    (record-invoke-edge! (assoc base :kind :invoke))
    (try
      (let [result (reg/invoke-agent!
                    to* prompt
                    (cond-> {:turn-id edge-id :surface surface*
                             :evidence-store evidence-store}
                      (some? timeout-ms) (assoc :timeout-ms timeout-ms)))]
        (record-invoke-edge! (assoc base
                                    :kind :invoke-result
                                    :ok? (true? (:ok result))
                                    :error (when-not (:ok result) (:error result))))
        result)
      (catch Throwable t
        (record-invoke-edge! (assoc base
                                    :kind :invoke-result
                                    :ok? false
                                    :error (.getMessage t)))
        (throw t)))))

(defn edge-public-view
  [entry]
  (let [body (:evidence/body entry)]
    {:id (:evidence/id entry)
     :edge-id (:edge/id body)
     :at (or (:edge/at body) (:evidence/at entry))
     :from (:edge/from body)
     :to (:edge/to body)
     :surface (:edge/surface body)
     :kind (:edge/kind body)
     :ok? (:edge/ok? body)
     :error (:edge/error body)
     :evidence-id (:evidence/id entry)}))

(defn recent-mesh-edges
  "Return recent social-layer mesh-edge records, newest first."
  ([] (recent-mesh-edges 50))
  ([limit] (recent-mesh-edges limit nil))
  ([limit evidence-store]
   (->> (estore/query* (mesh-evidence-store evidence-store)
                      {:query/type :coordination
                       :query/tags [:coordination :mesh-edge]
                       :query/limit (or limit 50)})
        (filter #(get-in % [:evidence/body :edge/from]))
        (mapv edge-public-view))))
