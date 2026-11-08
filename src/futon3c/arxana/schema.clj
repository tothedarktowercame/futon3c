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
    :projection [:operation :entity/id :record]}

   :graph/query
   {:schema-version 1
    :required #{:query/type}
    :allowed #{:query/type :edge/type :endpoint/role :endpoint/id :time/order
               :query/limit :start/id :traversal/mode :max-depth :max-nodes
               :subject/id :interpreter/id}
    :projection [:operation :query/type :result/status :result/edge-ids
                 :result/attestation-ids :result/receipts :result/assessments :result/nodes
                 :result/truncation :result/total :result/watermark]}

   :scope/resolve
   {:schema-version 1
    :required #{:artifact/revision-id :strategy/revision-id}
    :allowed #{:artifact/revision-id :strategy/revision-id :subject/id :options}
    :projection env/scope-resolve-projection-v1}

   ;; M-jvm-arxana two-pane demo (2026-11-07): position → smallest enclosing
   ;; REGISTERED scope, over stored correspondence assertions whose source
   ;; endpoint is a :codepoint-range locator on the requested revision. The
   ;; projection pins the semantic result fields; clients cannot widen them.
   :scope/at-position
   {:schema-version 1
    :required #{:artifact/revision-id :position/codepoint}
    :allowed #{:artifact/revision-id :position/codepoint :subject/id}
    :projection [:operation :result/status [:result/scopes] :result/innermost]}

   :revision/ingest
   {:schema-version 1
    :required #{:artifact/id :bytes :media-type :expected/revision-id}
    :allowed #{:artifact/id :bytes :media-type :expected/revision-id}
    :projection [:operation :artifact/id :artifact/revision-id]}

   :revision/get
   {:schema-version 1
    :required #{:artifact/revision-id}
    :allowed #{:artifact/revision-id}
    :projection [:operation :artifact/revision-id :revision/text :media-type]}

   :formal-f1/linear
   {:schema-version 1 :required #{:manifest/revision-id}
    :allowed #{:manifest/revision-id}
    :projection [:operation :rule/id :rule/version [:projection/fragments] :drops]}

   :formal-f1/outline
   {:schema-version 1 :required #{:manifest/revision-id}
    :allowed #{:manifest/revision-id}
    :projection [:operation :rule/id :rule/version [:projection/fragments] :drops]}

   :formal-f1/gcc
   {:schema-version 1 :required #{:manifest/revision-id :query/limit}
    :allowed #{:manifest/revision-id :query/limit}
    :projection [:operation :rule/id :rule/version :action-intents :result/status
                 :result/truncation :result/total [:projection/fragments] :drops]}

   :formal-f3/correspondence
   {:schema-version 1 :required #{:manifest/revision-id}
    :allowed #{:manifest/revision-id}
    :projection [:operation :rule/id :rule/version :result/status
                 [:projection/fragments] :drops]}

   :formal-f3/manifestations
   {:schema-version 1 :required #{:manifest/revision-id}
    :allowed #{:manifest/revision-id}
    :projection [:operation :rule/id :rule/version :result/status
                 [:projection/fragments] :drops]}

   :clusion/assert
   {:schema-version 1 :required #{:record} :allowed #{:record}
    :projection [:operation :record/id :created?]}

   :evidence/append
   {:schema-version 1 :required #{:record} :allowed #{:record}
    :projection [:operation :record/id :subject-clusion-id :created?]}

   :assessment/append
   {:schema-version 1 :required #{:record} :allowed #{:record}
    :projection [:operation :record/id :subject-clusion-id :created?]}

   :attempt/append
   {:schema-version 1 :required #{:record} :allowed #{:record}
    :projection [:operation :record/id :subject/id :outcome :created?]}

   :external-reference/assert
   {:schema-version 1 :required #{:record} :allowed #{:record}
    :projection [:operation :record/id :created?]}

   :mark-occurrence/assert
   {:schema-version 1 :required #{:record} :allowed #{:record}
    :projection [:operation :record/id :created?]}

   :external-reference/resolve
   {:schema-version 1 :required #{:reference} :allowed #{:reference}
    :projection [:operation :status :reference/id :revision/digest]}

   :aggregate/query
   {:schema-version 1 :required #{:pattern/id :window}
    :allowed #{:pattern/id :window}
    :projection [:operation :result/status :pattern/id :window :score
                 :result/watermark]}

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
    :projection [:operation :subscription/id :acked]}

   ;; Increment 7 (Compile): read-only intent-to-candidate compilation. The
   ;; projection deliberately excludes presentation and opaque extensions;
   ;; equality covers the semantic result and its non-executable candidate.
   :action/compile
   {:schema-version 1
    :required #{:source-act/id :intent :trust-profile :grounding}
    :allowed #{:source-act/id :intent :trust-profile :grounding
               :operation/overrides :extensions}
    :projection [:operation :status :candidate-action :alternatives
                 :obligations :trust]}})

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
