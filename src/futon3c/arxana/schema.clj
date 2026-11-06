(ns futon3c.arxana.schema
  "Versioned operation schema registry (D3 operations, O-D7d).

  Each registered operation names a schema version, its required and allowed
  body keys, and the semantic projection used for response digests. Adding or
  removing a semantic field requires a new schema/projection version; clients
  cannot choose their own equality field set."
  (:require [futon3c.arxana.envelope :as env]))

(def registry-v1
  "Increment-3 spine registrations. Operations absent from this registry are
  honestly :unsupported — never silently accepted."
  {:capabilities/handshake
   {:schema-version 1
    :required #{}
    :allowed #{}
    :projection [:operation]}

   :entity/get
   {:schema-version 1
    :required #{:entity/id}
    :allowed #{:entity/id}
    :projection [:operation :entity/id]}

   :graph/query
   {:schema-version 1
    :required #{:query/type}
    :allowed #{:query/type :query/args :query/limit}
    :projection [:operation :query/type :result/records]}

   :scope/resolve
   {:schema-version 1
    :required #{:artifact/revision-id :strategy/revision-id}
    :allowed #{:artifact/revision-id :strategy/revision-id :subject/id :options}
    :projection env/scope-resolve-projection-v1}

   :revision/ingest
   {:schema-version 1
    :required #{:artifact/id :bytes :media-type :expected/revision-id}
    :allowed #{:artifact/id :bytes :media-type :expected/revision-id}
    :projection [:operation :artifact/id :artifact/revision-id]}

   :assertion/promote
   {:schema-version 1
    :required #{:result/digest :result/evidence}
    :allowed #{:result/digest :result/evidence :acceptance}
    :projection [:operation :result/digest :assertion/id]}

   ;; Increment 6 (Project) registrations.
   :correspondence/query
   {:schema-version 1
    :required #{}
    :allowed #{:manifestation/id :artifact/revision-id :direction :methods
               :verification :verification-floor :role :assessment-verdict
               :current-only? :subject/id}
    :projection [:operation [:result/assertions]]}

   :subscription/open
   {:schema-version 1
    :required #{}
    :allowed #{:topics :resume/cursor}
    :projection [:operation :subscription/id :snapshot/cursor]}

   :subscription/poll
   {:schema-version 1
    :required #{:subscription/id}
    :allowed #{:subscription/id}
    :projection [:operation :subscription/id]}

   :subscription/ack
   {:schema-version 1
    :required #{:subscription/id :cursor}
    :allowed #{:subscription/id :cursor}
    :projection [:operation :subscription/id :acked]}})

(defn operation-schema
  "Look up the registered schema for op in registry (default registry-v1).
  Returns {:ok schema} or {:error typed-error} with :unsupported for unknown
  operations."
  ([op] (operation-schema registry-v1 op))
  ([registry op]
   (if-let [schema (get registry op)]
     {:ok schema}
     {:error (env/error :unsupported :arxana.schema/unknown-operation
                        {:operation op
                         :registered (vec (sort (keys registry)))}
                        [:capabilities/handshake])})))

(defn validate-body
  "Validate op's body against its registered schema: unknown body fields are
  refused; missing required fields are :missing. Returns {:ok body} or
  {:error typed-error}."
  ([op body] (validate-body registry-v1 op body))
  ([registry op body]
   (when-not (map? body)
     (throw (ex-info "body must be a map" {:body body})))
   (let [{:keys [ok error]} (operation-schema registry op)]
     (if error
       {:error error}
       (let [{:keys [required allowed]} ok
             unknown (remove allowed (keys body))
             missing (remove #(contains? body %) required)]
         (cond
           (seq unknown)
           {:error (env/error :invalid :arxana.schema/unknown-body-field
                              {:operation op :unknown-fields (vec (sort unknown))}
                              nil)}

           (seq missing)
           {:error (env/error :missing :arxana.schema/missing-body-field
                              {:operation op :missing-fields (vec (sort missing))}
                              nil)}

           :else {:ok body}))))))

(defn registered-versions
  "Map of operation -> schema version, for the capabilities handshake."
  ([] (registered-versions registry-v1))
  ([registry]
   (into {} (map (fn [[op schema]] [op (:schema-version schema)])) registry)))
