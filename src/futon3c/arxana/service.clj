(ns futon3c.arxana.service
  "Service spine: capabilities handshake and envelope dispatch plumbing.

  Increment 4 wired :scope/resolve and :assertion/promote. Increment 6
  (Project) adds :revision/ingest, :correspondence/query and the D3
  subscription operations (open/poll/ack) over the store-backed journal, and
  stamps :semantic/digest on operation responses per the registered D5
  projection. The 3-arity dispatch (store provider msg) remains for pre-6
  callers and answers :unsupported for increment-6 operations."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.compile.compiler :as compiler]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.external-ref :as external-ref]
            [futon3c.arxana.formal-f1-projection :as f1-projection]
            [futon3c.arxana.formal-f2 :as f2]
            [futon3c.arxana.formal-f3-projection :as f3-projection]
            [futon3c.arxana.formal-f5-query :as f5-query]
            [futon3c.arxana.formal-f5-render :as f5-render]
            [futon3c.arxana.journal :as journal]
            [futon3c.arxana.graph-read :as graph-read]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.resolve.promotion :as promotion]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.schema :as schema]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.subscribe :as subscribe]
            [futon3c.arxana.write :as write]))

(def ^:private fixed-event-time-formatter
  (-> (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSX")
      (.withZone java.time.ZoneOffset/UTC)))

(defn- fixed-event-time []
  (.format fixed-event-time-formatter (java.time.Instant/now)))

(defn handshake
  "Capabilities handshake (D1 compat rule 7: mandatory). Reports the store's
  truth table verbatim — evidence, not aspiration — plus registered operation
  schema versions and projection versions."
  ([graph-store] (handshake graph-store false))
  ([graph-store subscription?]
   {:protocol/version env/protocol-version
    :store/capabilities (store/capabilities graph-store)
    :operations (schema/registered-versions)
    :projections {:semantic/scope-resolve 1
                  :error 1}
    :consistency-mode :revision-precondition
    :subscription (boolean subscription?)}))

(defn register-strategy!
  "Register an executable strategy value in the graph store under its
  content-addressed revision ID (D2 grammar). Returns the revision ID. The
  value is stored verbatim; the revision ID covers finder vector order."
  [graph-store strategy]
  (let [srid (evaluator/strategy-revision-id strategy)]
    (store/put-entity! graph-store
                       {:entity/id srid
                        :type :arxana/strategy-revision
                        :external-id srid
                        :props {:strategy strategy}})
    srid))

(defn- strategy-source
  "Resolve a strategy revision ID to its executable value via the store."
  [graph-store]
  (fn [srid]
    (when-let [e (:ok (store/get-entity graph-store srid))]
      (get-in e [:props :strategy]))))

(defn- head-entity-id [artifact-id]
  (str "arxana:head:" artifact-id))

(defn- current-head [graph-store artifact-id]
  (get-in (:ok (store/get-entity graph-store (head-entity-id artifact-id)))
          [:props :head/revision-id]))

(defn- ingest-revision
  "revision/ingest handler (D3 write contract). Compares the expected head
  against the current head entity, ingests canonical bytes as a new immutable
  revision, and moves the head. Atomicity note: over the mock store this is a
  check-then-write whose non-atomicity is reported honestly via
  capabilities (:entity-compare-and-set false); the futon1b conditional
  transaction route owns real atomic refusal (increment 2 / D7 A2). A stale
  expected head is refused with the current head and recovery choices, never
  silently retried."
  [graph-store artifact-provider body]
  (let [{:keys [artifact/id bytes media-type expected/revision-id]} body
        ^bytes bs (cond
                    (bytes? bytes) bytes
                    (string? bytes) (.getBytes ^String bytes "UTF-8")
                    :else nil)]
    (cond
      (nil? bs)
      {:error (env/error :invalid :arxana.service/ingest-bytes
                         {:reason ":bytes must be a UTF-8 string or base64 bytes"}
                         nil)}

      (and (get (store/capabilities graph-store) :entity-compare-and-set)
           (string? bytes))
      (let [{route-ok :ok route-error :error}
            (store/conditional-write!
             graph-store
             {:protocol/version env/protocol-version
              :message/id (:message/id body)
              :operation :revision/ingest
              :principal/id "arxana-service"
              :body {:target/id id :expected/revision-id revision-id
                     :command {:revision/canonical-bytes bytes
                               :revision/media-type media-type}}})]
        (if route-error
          {:error route-error}
          (let [{provider-ok :ok} (artifact/ingest! artifact-provider id bs media-type)
                provider-rid (:artifact/revision-id provider-ok)
                store-rid (:revision/id route-ok)]
            (if (not= provider-rid store-rid)
              {:error (env/error :failed :arxana.service/revision-digest-disagreement
                                 {:provider/revision-id provider-rid
                                  :store/revision-id store-rid} nil)}
              {:ok {:artifact/id id :artifact/revision-id store-rid
                    :head/revision-id (:head/revision-id route-ok)
                    :duplicate? (:duplicate? route-ok)}}))))

      :else
      (let [current (current-head graph-store id)]
        (if (not= (if (= revision-id :absent) nil revision-id) current)
          {:error (env/error :stale :arxana.service/head-precondition-failed
                             {:target/id id
                              :expected/revision-id revision-id
                              :current/revision-id current}
                             [:refresh :rebase-and-resubmit :export-patch])}
          (let [{:keys [ok]} (artifact/ingest! artifact-provider id bs media-type)
                rid (:artifact/revision-id ok)]
            (store/put-entity! graph-store
                               {:entity/id (head-entity-id id)
                                :type :arxana/artifact-head
                                :external-id id
                                :props {:head/revision-id rid}})
            {:ok {:artifact/id id
                  :artifact/revision-id rid
                  :head/revision-id rid}}))))))

(defn- with-semantic-digest
  "Stamp :semantic/digest on an :ok payload using the operation's registered
  projection (D5 canonical semantic response digest). Errors use the error
  projection; both exclude message ID, principal and timestamps."
  [op payload]
  (if (:error/category payload)
    (assoc payload :semantic/digest (env/error-digest op payload))
    (let [projection (get-in (schema/operation-schema op) [:ok :projection])]
      (assoc payload :semantic/digest
             (env/semantic-digest projection (assoc payload :operation op))))))

(defn- publish!
  "Append a journal event when the config carries a journal; nil-safe."
  [cfg event]
  (when-let [j (:journal cfg)]
    (journal/append! j event)))

(defn dispatch
  "Validate an inbound envelope and its body against the schema registry and
  run the operation handler. Returns a D3 response envelope containing
  exactly one of :ok or :error.

  Config arity: {:graph-store s :artifact-provider p :correspondence-repo r
  :journal j :subscriptions subs}. The 3-arity form is the pre-increment-6
  seam: increment-6 operations answer :unsupported against it."
  ([graph-store artifact-provider msg]
   (dispatch {:graph-store graph-store :artifact-provider artifact-provider} msg))
  ([cfg msg]
   (let [{:keys [graph-store artifact-provider correspondence-repo
                 subscriptions]} cfg
         {:keys [ok error]} (env/validate-envelope msg)]
     (if error
       {:protocol/version env/protocol-version
        :message/id (:message/id msg)
        :operation (:operation msg)
        :error error}
       (let [op (:operation ok)
             {body :ok berr :error} (schema/validate-body op (:body ok))]
         (cond
           berr (env/response ok berr)

           (= op :capabilities/handshake)
           (env/response ok (handshake graph-store (some? subscriptions)))

           (= op :entity/get)
           (let [{record :ok read-error :error}
                 (store/get-entity graph-store (:entity/id body))]
             (env/response ok
                           (if read-error read-error
                               (with-semantic-digest
                                 op {:entity/id (:entity/id body)
                                     :record record}))))

           (= op :revision/get)
           (let [{local :ok}
                 (artifact/revision-bytes artifact-provider (:artifact/revision-id body))
                 stored (when-not local
                          (:ok (store/bounded-query!
                                graph-store {:query/type :revision-by-id
                                             :revision-id (:artifact/revision-id body)})))
                 revision (or local
                              (when (= :ok (:status stored))
                                {:bytes (.getBytes ^String (:revision/canonical-bytes stored) "UTF-8")
                                 :media-type (:revision/media-type stored)}))
                 read-error (when-not revision
                              (env/error :missing :arxana.artifact/revision-not-found
                                         {:artifact/revision-id (:artifact/revision-id body)} nil))]
             (env/response ok
                           (if read-error read-error
                               (with-semantic-digest
                                 op {:artifact/revision-id (:artifact/revision-id body)
                                     :revision/text (String. ^bytes (:bytes revision) "UTF-8")
                                     :media-type (:media-type revision)}))))

           (= op :revision/head)
           (let [{qok :ok qerr :error}
                 (store/bounded-query! graph-store {:query/type :artifact-head
                                                    :artifact-id (:artifact/id body)})]
             (env/response ok
                           (if qerr qerr
                               (with-semantic-digest
                                 op {:artifact/id (:artifact/id body)
                                     :head/revision-id (:head/revision-id qok)}))))

           (contains? #{:formal-f1/linear :formal-f1/outline :formal-f1/gcc} op)
           (let [read! (fn [read-op read-body]
                         (dispatch cfg {:protocol/version env/protocol-version
                                        :message/id (str (:message/id ok) ":" (name read-op)
                                                         ":" (hash read-body))
                                        :operation read-op
                                        :principal/id "formal-f1-projector"
                                        :body read-body}))
                 projected (f1-projection/project read! op body)]
             (env/response ok (with-semantic-digest op projected)))

           (contains? #{:formal-f3/correspondence :formal-f3/manifestations} op)
           (let [read! (fn [read-op read-body]
                         (dispatch cfg {:protocol/version env/protocol-version
                                        :message/id (str (:message/id ok) ":f3:" (hash read-body))
                                        :operation read-op :principal/id "formal-f3-projector"
                                        :body read-body}))
                 projected (f3-projection/project read! op body)]
             (env/response ok (with-semantic-digest op projected)))

           (= op :geometry/fibre)
           (let [read! (fn [read-op read-body]
                         (dispatch cfg {:protocol/version env/protocol-version
                                        :message/id (str (:message/id ok) ":f5:" (hash read-body))
                                        :operation read-op :principal/id "formal-f5-query"
                                        :body read-body}))]
             (env/response ok (with-semantic-digest op (f5-query/fibre read! body))))

           (= op :geometry/render)
           (let [read! (fn [read-op read-body]
                         (dispatch cfg {:protocol/version env/protocol-version
                                        :message/id (str (:message/id ok) ":f5-render:" (hash read-body))
                                        :operation read-op :principal/id "formal-f5-projector"
                                        :body read-body}))]
             (env/response ok (with-semantic-digest op (f5-render/render read! body))))

           (= op :geometry/select)
           (let [read! (fn [read-op read-body]
                         (dispatch cfg {:protocol/version env/protocol-version
                                        :message/id (str (:message/id ok) ":f5-select:" (hash read-body))
                                        :operation read-op :principal/id "formal-f5-selector"
                                        :body read-body}))
                 scene (f5-query/scene-record)
                 map-record (f5-query/named-record read! (:map/id body) :map)]
             (cond
               (not (f5-query/iso-instant? (:selected-at body)))
               (env/response ok (with-semantic-digest op
                                  {:error/category :invalid
                                   :error/code :arxana.geometry/selected-at-not-iso-instant
                                   :error/detail {:selected-at (:selected-at body)
                                                  :expected "yyyy-MM-ddTHH:mm:ss.SSSZ"}}))
               (nil? map-record)
               (env/response ok (with-semantic-digest op
                                  {:error/category :missing
                                   :error/code :arxana.geometry/unknown-map
                                   :error/detail {:map/id (:map/id body)}}))
               :else
               (let [endpoint (fn [role r] {:role role :kind :clusion
                                             :target {:clusion/id (:id r)}})
                     selected-at (:selected-at body)
                     selection (assoc (records/make-clusion
                                       {:type-revision [:geometry/selection 1]
                                        :endpoints [(endpoint :scene scene) (endpoint :map map-record)]
                                        :asserted-content {:selected-at selected-at
                                                           :theory/version "formal-arxana/geometry-v1"}})
                                      :event-time selected-at)
                     {wok :ok werr :error} (write/execute graph-store ok :clusion/assert selection)]
                 (env/response ok (with-semantic-digest op (or werr wok))))))

           (= op :graph/query)
           (let [res (if (= :traverse (:query/type body))
                       (graph-read/traverse graph-store
                                            {:start/id (:start/id body)
                                             :edge/type (:edge/type body)
                                             :endpoint/role (:endpoint/role body)
                                             :traversal/mode (:traversal/mode body)
                                             :max-depth (:max-depth body)
                                             :max-nodes (:max-nodes body)})
                       (let [{qok :ok qerr :error}
                             (store/bounded-query!
                              graph-store {:query/type (:query/type body)
                                           :edge-type (:edge/type body)
                                           :endpoint-role (:endpoint/role body)
                                           :endpoint-id (:endpoint/id body)
                                           :order (:time/order body)
                                           :limit (:query/limit body)
                                           :subject-id (:subject/id body)
                                           :interpreter-id (:interpreter/id body)})]
                         (or qerr
                             (if (= :stale-index (:status qok))
                               (env/error :stale :arxana.graph/index-behind qok [:retry])
                               (cond-> {:result/status (if (:truncated? qok) :truncated :complete)
                                        :result/edge-ids (:edge-ids qok)
                                        :result/attestation-ids (:attestation-ids qok)
                                        :result/receipts (:receipts qok)
                                        :result/assessments (:assessments qok)
                                        :result/watermark (:watermark qok)}
                                 (some? (:total qok)) (assoc :result/total (:total qok))
                                 (:truncated? qok) (assoc :result/truncation :limit))))))]
             (env/response ok (with-semantic-digest op
                                                    (if (:error/category res) res
                                                        (cond-> res
                                                          (:status res)
                                                          (assoc :result/status (:status res)
                                                                 :result/nodes (:nodes res)
                                                                 :result/truncation (:truncation/reason res)))))))

           (= op :scope/resolve)
           (let [res (evaluator/evaluate
                      {:artifact-provider artifact-provider
                       :strategy-source (strategy-source graph-store)}
                      (assoc body :request/id (:message/id ok)))]
             (env/response ok (with-semantic-digest op res)))

           (= op :scope/at-position)
           (if-not correspondence-repo
             (env/response ok (env/error :unsupported :arxana.service/operation-not-implemented
                                         {:operation op :missing :correspondence-repo}
                                         nil))
             (let [{:keys [artifact/revision-id position/codepoint subject/id]} body
                   subject-mids (when id
                                  (set (map :manifestation/id
                                            (corr/all-manifestations correspondence-repo id))))
                   enclosing
                   (->> (corr/query correspondence-repo {})
                        (filter (fn [a]
                                  (or (nil? subject-mids)
                                      (contains? subject-mids
                                                 (get-in a [:source :manifestation/id])))))
                        (keep (fn [a]
                                (let [ep (:source a)
                                      loc (:locator ep)]
                                  (when (and (= revision-id (:artifact/revision-id ep))
                                             (= :codepoint-range (:kind loc))
                                             (<= (:start loc) codepoint (dec (:end loc))))
                                    {:assertion/id (:id a)
                                     :subject/id (get-in (corr/get-manifestation
                                                          correspondence-repo
                                                          (:manifestation/id ep))
                                                         [:manifestation/subject])
                                     :verification (:verification a)
                                     :direction (:direction a)
                                     :source {:artifact/revision-id (:artifact/revision-id ep)
                                              :range {:start (:start loc) :end (:end loc)}}
                                     :target {:manifestation/id
                                              (get-in a [:target :manifestation/id])
                                              :artifact/revision-id
                                              (get-in a [:target :artifact/revision-id])
                                              :locator (get-in a [:target :locator])}
                                     :size (- (:end loc) (:start loc))}))))
                        (sort-by (juxt :size :assertion/id)))
                   minimal-count (some-> (first enclosing) :size)
                   innermost-candidates (filter #(= minimal-count (:size %)) enclosing)]
               (cond
                 (empty? enclosing)
                 (env/response ok (with-semantic-digest
                                    op (env/error :missing :arxana.scope/no-enclosing-scope
                                                  {:artifact/revision-id revision-id
                                                   :position/codepoint codepoint}
                                                  nil)))
                 ;; Ambiguity is preserved, never resolved by ordering: two
                 ;; distinct innermost scopes of equal size stay a candidate set.
                 (> (count innermost-candidates) 1)
                 (env/response ok (with-semantic-digest
                                    op {:result/status :ambiguous
                                        :result/scopes (mapv #(dissoc % :size) enclosing)
                                        :result/innermost nil}))
                 :else
                 (env/response ok (with-semantic-digest
                                    op {:result/status :resolved
                                        :result/scopes (mapv #(dissoc % :size) enclosing)
                                        :result/innermost (dissoc (first innermost-candidates) :size)})))))

           (= op :assertion/promote)
           (let [{rok :ok rerr :error}
                 (promotion/promote
                  {:graph-store graph-store
                   :artifact-provider artifact-provider
                   :strategy-source (strategy-source graph-store)}
                  (assoc body :principal/id (:principal/id ok)))]
             (if rerr
               (env/response ok (with-semantic-digest op rerr))
               (do
                 (publish! cfg {:topic :assertion/promoted
                                :cause/message-id (:message/id ok)
                                :affected/ids [(:assertion/id rok)]
                                :payload {:assertion/id (:assertion/id rok)
                                          :acceptance/id (:acceptance/id rok)
                                          :created? (:created? rok)}})
                 (env/response ok (with-semantic-digest op rok)))))

           (#{:clusion/assert :evidence/append :assessment/append
              :attempt/append :external-reference/assert :mark-occurrence/assert} op)
           (let [{wok :ok werr :error}
                 (write/execute graph-store ok op (:record body))]
             (env/response ok (with-semantic-digest op (or werr wok))))

           (= op :text/submit-patch)
           (let [result (f2/submit!
                         {:graph-store graph-store
                          :artifact-provider artifact-provider
                          :clock (or (:text/clock cfg) fixed-event-time)
                          :before-cas (:text/before-cas cfg)
                          :ingest #(ingest-revision graph-store artifact-provider
                                                    {:artifact/id (:artifact/id body)
                                                     :bytes (if (bytes? %1) (String. ^bytes %1 "UTF-8") %1)
                                                     :media-type "text/markdown; charset=utf-8"
                                                     :expected/revision-id %2
                                                     :message/id (str (:message/id ok) ":ingest")})}
                         ok body)]
             (env/response ok (with-semantic-digest op (or (:error result) (:ok result)))))

           (= op :text/choose)
           (let [result (f2/choose!
                         {:graph-store graph-store
                          :clock (or (:text/clock cfg) fixed-event-time)
                          :before-cas (:text/before-cas cfg)
                          :ingest #(ingest-revision graph-store artifact-provider
                                                    {:artifact/id (:artifact/id body)
                                                     :bytes (if (bytes? %1) (String. ^bytes %1 "UTF-8") %1)
                                                     :media-type "text/markdown; charset=utf-8"
                                                     :expected/revision-id %2
                                                     :message/id (str (:message/id ok) ":choose-ingest")})}
                         ok body)]
             (env/response ok (with-semantic-digest op (or (:error result) (:ok result)))))

           (= op :external-reference/resolve)
           (env/response ok (with-semantic-digest
                              op (external-ref/resolve-reference (:reference body))))

           (= op :aggregate/query)
           (let [{qok :ok qerr :error}
                 (store/bounded-query! graph-store {:query/type :pattern-score
                                                    :pattern-id (:pattern/id body)
                                                    :window (:window body)})]
             (env/response ok
                           (with-semantic-digest
                            op
                            (or qerr
                                (if (= :stale-index (:status qok))
                                  (env/error :stale :arxana.aggregate/index-behind
                                             qok [:retry])
                                  {:result/status :complete
                                   :pattern/id (:pattern/id body)
                                   :window (:window body)
                                   :score (:score qok)
                                   :result/watermark (:watermark qok)})))))

           (= op :revision/ingest)
           (let [{rok :ok rerr :error} (ingest-revision graph-store artifact-provider
                                                         (assoc body :message/id (:message/id ok)))]
             (if rerr
               (env/response ok (with-semantic-digest op rerr))
               (do
                 (publish! cfg {:topic :revision/ingested
                                :cause/message-id (:message/id ok)
                                :affected/ids [(:artifact/id rok)]
                                :before/revision-id (:expected/revision-id body)
                                :after/revision-id (:artifact/revision-id rok)
                                :payload rok})
                 (env/response ok (with-semantic-digest op rok)))))

           (= op :correspondence/query)
           (if-not correspondence-repo
             (env/response ok (env/error :unsupported :arxana.service/operation-not-implemented
                                         {:operation op :increment 6
                                          :missing :correspondence-repo}
                                         nil))
             (let [filters (dissoc body :subject/id)
                   assertions (if-let [subj (:subject/id body)]
                                (let [ms (corr/all-manifestations correspondence-repo subj)
                                      ids (set (map :manifestation/id ms))]
                                  (filterv (fn [a]
                                             (or (ids (:manifestation/id (:source a)))
                                                 (ids (:manifestation/id (:target a)))))
                                           (corr/query correspondence-repo filters)))
                                (corr/query correspondence-repo filters))]
               (env/response ok (with-semantic-digest
                                 op
                                 {:result/assertions assertions
                                  :result/count (count assertions)}))))

           (= op :subscription/open)
           (if-not subscriptions
             (env/response ok (env/error :unsupported :arxana.service/operation-not-implemented
                                         {:operation op :increment 6
                                          :missing :subscriptions}
                                         nil))
             (let [{rok :ok rerr :error}
                   (subscribe/open subscriptions
                                   (assoc body :principal/id (:principal/id ok)))]
               (env/response ok (or (some->> rerr (with-semantic-digest op)) (with-semantic-digest op rok)))))

           (= op :subscription/poll)
           (if-not subscriptions
             (env/response ok (env/error :unsupported :arxana.service/operation-not-implemented
                                         {:operation op :increment 6
                                          :missing :subscriptions}
                                         nil))
             (let [{rok :ok rerr :error} (subscribe/poll subscriptions body)]
               (env/response ok (or (some->> rerr (with-semantic-digest op)) (with-semantic-digest op rok)))))

           (= op :subscription/ack)
           (if-not subscriptions
             (env/response ok (env/error :unsupported :arxana.service/operation-not-implemented
                                         {:operation op :increment 6
                                          :missing :subscriptions}
                                         nil))
             (let [{rok :ok rerr :error} (subscribe/ack subscriptions body)]
               (env/response ok (or (some->> rerr (with-semantic-digest op)) (with-semantic-digest op rok)))))

           (= op :action/compile)
           (env/response ok
                         (with-semantic-digest op
                           (compiler/compile-action body)))

           :else
           (env/response
            ok
            (env/error :unsupported :arxana.service/operation-not-implemented
                       {:operation op
                        :increment 7
                        :note "operation is registered but has no public handler"}
                       nil))))))))
