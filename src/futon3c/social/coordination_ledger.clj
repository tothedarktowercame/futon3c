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
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]
           [java.util UUID]))

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
  [{:keys [from to surface kind ok? error at session-id]}]
  (let [from* (normalize-from from)
        to* (or (normalize-agent-id to) "unknown")
        surface* (normalize-surface surface)
        kind* (event-kind kind)
        at* (or at (now-str))]
    {:evidence/id (str "e-" (UUID/randomUUID))
     :evidence/subject {:ref/type :agent :ref/id to*}
     :evidence/type :coordination
     :evidence/claim-type :step
     :evidence/author from*
     :evidence/at at*
     :evidence/body (cond-> {:edge/kind kind*
                             :edge/from from*
                             :edge/to to*
                             :edge/surface surface*
                             :edge/at at*}
                      (some? ok?) (assoc :edge/ok? (boolean ok?))
                      error (assoc :edge/error (str error)))
     :evidence/tags [:coordination :mesh-edge]
     :evidence/session-id (or session-id (str "mesh-edge-" (UUID/randomUUID)))}))

(defn record-invoke-edge!
  "Append one mesh-edge evidence entry. Accepts optional :evidence-store for
   tests; defaults to the process evidence store."
  [{:keys [evidence-store] :as edge}]
  (boundary/append! (or evidence-store estore/!store)
                    (make-mesh-edge-evidence edge)))

(defn invoke-with-edge!
  "Invoke an agent and record the social mesh edge around it.

   Preserves registry/invoke-agent!'s return value and exception semantics."
  [{:keys [from to surface prompt timeout-ms evidence-store]}]
  (let [from* (normalize-from from)
        to* (or (normalize-agent-id to) "unknown")
        surface* (normalize-surface surface)
        base {:from from* :to to* :surface surface* :evidence-store evidence-store}]
    (record-invoke-edge! (assoc base :kind :invoke))
    (try
      (let [result (if (some? timeout-ms)
                     (reg/invoke-agent! to* prompt timeout-ms)
                     (reg/invoke-agent! to* prompt))]
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
  ([limit]
   (->> (estore/query {:query/type :coordination
                       :query/tags [:coordination :mesh-edge]
                       :query/limit (or limit 50)})
        (mapv edge-public-view))))
