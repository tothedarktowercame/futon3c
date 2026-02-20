(ns futon3c.transport.http
  "HTTP REST adapter — thin Ring handler wired to the social pipeline.

   Adapter as boundary — not business logic. The handler only translates
   between HTTP requests/responses and pipeline function calls. No routing
   logic, mode classification, or evidence emission lives here.

   Routes:
     POST /dispatch  — classify + dispatch, return receipt or error
     POST /presence  — verify presence, return record or error
     GET  /session/:id — retrieve session by ID
     GET  /api/alpha/evidence — query evidence entries
     GET  /api/alpha/evidence/count — count evidence entries by filters
     GET  /api/alpha/evidence/:id — retrieve single evidence entry
     GET  /api/alpha/evidence/:id/chain — retrieve ancestor reply chain
     GET  /health    — liveness check with agent/session counts

   Pattern references:
   - realtime/verify-after-start (L4, L7): start-server! probes port after
     start to confirm it is actually listening. Async startup can hide binding
     failures (port in use, permission denied).
   - realtime/request-param-resilience (L1, L3): delegates param extraction
     to protocol/extract-params for consistency across HTTP and WS."
  (:require [futon3c.transport.protocol :as proto]
            [futon3c.transport.encyclopedia :as enc]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.federation :as federation]
            [futon3c.social.mode :as mode]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.presence :as presence]
            [futon3c.social.persist :as persist]
            [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.net Socket InetSocketAddress]
           [java.nio.channels AsynchronousCloseException ClosedChannelException ClosedSelectorException]
           [org.httpkit.logger ContextLogger]))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- read-body
  "Read request body as a string. Handles both InputStream and String."
  [request]
  (when-let [body (:body request)]
    (if (string? body) body (slurp body))))

(defn- json-response
  "Build a Ring response with JSON content-type."
  [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (if (string? body) body (json/generate-string body))})

(defn- error?
  "True if result is a SocialError (has :error/code key)."
  [result]
  (and (map? result) (contains? result :error/code)))

(defn- error-response
  "Convert a SocialError into a Ring response with mapped HTTP status."
  [social-error]
  (let [{:keys [status body]} (proto/render-error social-error)]
    {:status status
     :headers {"Content-Type" "application/json"}
     :body body}))

(defn- parse-query-params
  "Parse request query string into map of string -> string."
  [request]
  (let [query (:query-string request)]
    (if (or (nil? query) (str/blank? query))
      {}
      (->> (str/split query #"&")
           (remove str/blank?)
           (map (fn [pair]
                  (let [[k v] (str/split pair #"=" 2)]
                    [(enc/decode-uri-component k)
                     (enc/decode-uri-component (or v ""))])))
           (into {})))))

(defn- encyclopedia-opts
  "Resolve encyclopedia config options for handlers."
  [config]
  {:corpus-root (or (get-in config [:encyclopedia :corpus-root])
                    (System/getenv "FUTON3C_PLANETMATH_ROOT"))})

(defn- parse-int
  "Parse integer string to int; return nil when invalid."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (try
      (int (Long/parseLong s))
      (catch Exception _ nil))))

(defn- parse-bool
  "Parse boolean query values.
   Accepts true/false, 1/0, yes/no."
  [s]
  (when (string? s)
    (let [v (str/lower-case (str/trim s))]
      (cond
        (#{"true" "1" "yes"} v) true
        (#{"false" "0" "no"} v) false
        :else nil))))

(defn- parse-keyword
  "Parse an API parameter into a keyword (accepts optional leading :)."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (let [v (str/trim s)
          raw (if (str/starts-with? v ":") (subs v 1) v)]
      (when (seq raw)
        (keyword raw)))))

(defn- parse-tags
  "Parse comma-separated tag list to vector of keywords."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (->> (str/split s #",")
         (map str/trim)
         (remove str/blank?)
         (map parse-keyword)
         (remove nil?)
         vec)))

(defn- parse-subject
  "Parse subject params into ArtifactRef."
  [params]
  (let [subject-type (parse-keyword (get params "subject-type"))
        subject-id (get params "subject-id")]
    (when (and subject-type (string? subject-id) (not (str/blank? subject-id)))
      {:ref/type subject-type
       :ref/id (str subject-id)})))

(defn- evidence-store-for-config
  "Resolve configured evidence store from runtime config."
  [config]
  (or (:evidence-store config)
      (get-in config [:registry :peripheral-config :evidence-store])))

(defn- expected-http-kit-shutdown-close?
  "True when `http-kit` accept-loop emitted an expected close exception while stopping."
  [msg ex]
  (and (= "accept incoming request" msg)
       (or (instance? ClosedChannelException ex)
           (instance? ClosedSelectorException ex)
           (instance? AsynchronousCloseException ex))))

(defn- expected-http-kit-stop-race?
  "True when `http-kit` stop hit the known JDK selector close race (NPE in removeKey)."
  [ex]
  (and (instance? NullPointerException ex)
       (let [msg (some-> ex .getMessage)
             top (first (.getStackTrace ex))]
         (and (some? top)
              (= "java.nio.channels.spi.AbstractSelectableChannel" (.getClassName ^StackTraceElement top))
              (= "removeKey" (.getMethodName ^StackTraceElement top))
              (or (nil? msg)
                  (str/includes? msg "this.keys"))))))

(defn- make-http-kit-error-logger
  "Create an error logger that suppresses expected shutdown close races only."
  [!server]
  (fn [msg ex]
    (let [status (some-> @!server hk/server-status)
          suppress? (and (not= :running status)
                         (expected-http-kit-shutdown-close? msg ex))]
      (when-not suppress?
        (.log ContextLogger/ERROR_PRINTER msg ex)))))

(defn- normalize-artifact-ref
  "Normalize subject map (string or keyword :ref/type accepted)."
  [subject]
  (when (map? subject)
    (let [subject-type (or (parse-keyword (get subject :ref/type))
                           (parse-keyword (get subject "ref/type")))
          subject-id (or (get subject :ref/id)
                         (get subject "ref/id"))]
      (when (and subject-type (some? subject-id) (not (str/blank? (str subject-id))))
        {:ref/type subject-type
         :ref/id (str subject-id)}))))

(defn- normalize-tags
  "Normalize vector/list of tags to keyword vector."
  [tags]
  (when (coll? tags)
    (->> tags
         (map #(cond
                 (keyword? %) %
                 (string? %) (parse-keyword %)
                 :else nil))
         (remove nil?)
         vec)))

(defn- non-blank-string
  "Return trimmed string when non-blank; otherwise nil."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (str/trim s)))

;; =============================================================================
;; Route handlers
;; =============================================================================

(defn- live-registry
  "Build a registry that merges the static config snapshot with live-registered agents.
   HTTP-registered and federated agents appear in the live registry atom but not
   in the startup config snapshot. Merge ensures dispatch can reach them."
  [config]
  (let [static-reg (:registry config)
        live-status (reg/registry-status)
        live-agents (into {}
                         (map (fn [[id {:keys [capabilities type]}]]
                                [id (cond-> {:capabilities (vec capabilities)}
                                      (some? type) (assoc :type type))]))
                         (:agents live-status))
        merged-agents (merge (:agents static-reg) live-agents)]
    (assoc static-reg :agents merged-agents)))

(defn- handle-dispatch
  "POST /dispatch — parse JSON body, classify message, dispatch to agent.
   Returns 200 + DispatchReceipt JSON or error status + SocialError JSON.
   Uses live registry (merged with config snapshot) so federated agents are reachable."
  [request config]
  (let [body (read-body request)
        parsed (proto/parse-dispatch-request (or body ""))]
    (if (error? parsed)
      (error-response parsed)
      (let [classified (mode/classify parsed (:patterns config))
            registry (live-registry config)]
        (if (error? classified)
          (error-response classified)
          (let [result (dispatch/dispatch classified registry)]
            (if (error? result)
              (error-response result)
              (json-response 200 (proto/render-receipt result)))))))))

(defn- handle-presence
  "POST /presence — parse JSON body, verify agent presence via S-presence.
   Returns 200 + PresenceRecord JSON or error status + SocialError JSON.
   Uses live registry so federated agents are verifiable."
  [request config]
  (let [body (read-body request)
        parsed (proto/parse-presence-request (or body ""))]
    (if (error? parsed)
      (error-response parsed)
      (let [result (presence/verify parsed (live-registry config))]
        (if (error? result)
          (error-response result)
          (json-response 200 (proto/render-ws-frame result)))))))

(defn- handle-get-session
  "GET /session/:id — retrieve session by ID from persist store.
   Returns 200 + SessionRecord JSON or 404."
  [request _config]
  (let [uri (:uri request)
        session-id (when (str/starts-with? (str uri) "/session/")
                     (subs uri (count "/session/")))]
    (if (or (nil? session-id) (str/blank? session-id))
      (json-response 400 {"error" true
                          "code" "invalid-request"
                          "message" "Missing session ID in path"})
      (let [result (persist/get-session session-id)]
        (if (error? result)
          (error-response result)
          (json-response 200
            {"session_id" (:session/id result)
             "agent_id" (when-let [aid (:session/agent-id result)]
                          {"value" (:id/value aid)
                           "type" (name (:id/type aid))})
             "state" (:session/state result)
             "at" (str (:session/at result))}))))))

(defn- handle-health
  "GET /health — return agent, session, and evidence counts.
   Reads both live registry and config snapshot, reports the larger count.
   Live registry reflects HTTP-registered and federated agents;
   config snapshot reflects agents wired at startup."
  [config started-at]
  (let [now (Instant/now)
        uptime-seconds (max 0 (.getSeconds (java.time.Duration/between started-at now)))
        live-status (reg/registry-status)
        live-count (:count live-status)
        config-count (count (get-in config [:registry :agents]))
        agent-summary (into {}
                            (map (fn [[id info]]
                                   [id {:type (:type info)
                                        :last-active (:last-active info)
                                        :capabilities (:capabilities info)}]))
                            (:agents live-status))
        evidence-store (evidence-store-for-config config)
        evidence-count (count (estore/query* evidence-store {}))]
    (json-response 200 {"status" "ok"
                         "agents" (max live-count config-count)
                         "sessions" (count (persist/list-sessions {}))
                         "agent-summary" agent-summary
                         "evidence" evidence-count
                         "started-at" (str started-at)
                         "uptime-seconds" uptime-seconds})))

(defn- handle-encyclopedia-corpuses
  "GET /fulab/encyclopedia/corpuses — list available corpuses."
  [config]
  (json-response 200 {:ok true
                      :corpuses (enc/list-corpuses (encyclopedia-opts config))}))

(defn- handle-encyclopedia-entries
  "GET /fulab/encyclopedia/:corpus/entries — list paginated entry summaries."
  [request config corpus-name]
  (let [params (parse-query-params request)
        limit (enc/parse-int (get params "limit") 100)
        offset (enc/parse-int (get params "offset") 0)
        opts (encyclopedia-opts config)]
    (if-let [page (enc/page-entries opts corpus-name limit offset)]
      (json-response 200 (assoc page :ok true))
      (json-response 404 {:ok false :err "corpus-not-found" :corpus corpus-name}))))

(defn- handle-encyclopedia-entry
  "GET /fulab/encyclopedia/:corpus/entry/:id — fetch full entry."
  [config corpus-name entry-id]
  (let [opts (encyclopedia-opts config)]
    (if-let [entry (enc/find-entry opts corpus-name entry-id)]
      (json-response 200 {:ok true :entry entry})
      (json-response 404 {:ok false
                          :err "entry-not-found"
                          :corpus corpus-name
                          :entry-id entry-id}))))

(defn- handle-evidence-query
  "GET /api/alpha/evidence — query evidence entries."
  [request config]
  (let [params (parse-query-params request)
        limit (parse-int (get params "limit"))
        subject (parse-subject params)
        evidence-type (parse-keyword (get params "type"))
        claim-type (parse-keyword (get params "claim-type"))
        include-ephemeral? (parse-bool (get params "include-ephemeral?"))
        pattern-id (parse-keyword (get params "pattern-id"))
        tags (parse-tags (get params "tag"))
        author (non-blank-string (get params "author"))
        session-id (non-blank-string (get params "session-id"))
        query (cond-> {}
                subject (assoc :query/subject subject)
                evidence-type (assoc :query/type evidence-type)
                claim-type (assoc :query/claim-type claim-type)
                (get params "since") (assoc :query/since (get params "since"))
                (some? include-ephemeral?)
                (assoc :query/include-ephemeral? include-ephemeral?))
        evidence-store (evidence-store-for-config config)
        entries (cond->> (estore/query* evidence-store query)
                  true
                  (filter (fn [entry]
                            (and
                             (or (nil? author)
                                 (= author (:evidence/author entry)))
                             (or (nil? session-id)
                                 (= session-id (:evidence/session-id entry)))
                             (or (nil? pattern-id)
                                 (= pattern-id (:evidence/pattern-id entry)))
                             (or (empty? tags)
                                 (let [entry-tags (set (:evidence/tags entry))]
                                   (every? entry-tags tags))))))
                  (and (int? limit) (pos? limit))
                  (take limit)
                  true
                  vec)]
    (json-response 200 {:ok true
                        :count (count entries)
                        :entries entries})))

(defn- handle-evidence-count
  "GET /api/alpha/evidence/count — return filtered evidence count."
  [request config]
  (let [params (parse-query-params request)
        subject (parse-subject params)
        evidence-type (parse-keyword (get params "type"))
        claim-type (parse-keyword (get params "claim-type"))
        include-ephemeral? (parse-bool (get params "include-ephemeral?"))
        pattern-id (parse-keyword (get params "pattern-id"))
        tags (parse-tags (get params "tag"))
        author (non-blank-string (get params "author"))
        session-id (non-blank-string (get params "session-id"))
        query (cond-> {}
                subject (assoc :query/subject subject)
                evidence-type (assoc :query/type evidence-type)
                claim-type (assoc :query/claim-type claim-type)
                (get params "since") (assoc :query/since (get params "since"))
                (some? include-ephemeral?)
                (assoc :query/include-ephemeral? include-ephemeral?))
        evidence-store (evidence-store-for-config config)
        count* (->> (estore/query* evidence-store query)
                    (filter (fn [entry]
                              (and
                               (or (nil? author)
                                   (= author (:evidence/author entry)))
                               (or (nil? session-id)
                                   (= session-id (:evidence/session-id entry)))
                               (or (nil? pattern-id)
                                   (= pattern-id (:evidence/pattern-id entry)))
                               (or (empty? tags)
                                   (let [entry-tags (set (:evidence/tags entry))]
                                     (every? entry-tags tags))))))
                    count)]
    (json-response 200 {:ok true
                        :count count*})))

(defn- handle-evidence-entry
  "GET /api/alpha/evidence/:id — fetch one entry."
  [config evidence-id]
  (let [evidence-store (evidence-store-for-config config)
        entry (estore/get-entry* evidence-store evidence-id)]
    (if entry
      (json-response 200 {:ok true
                          :entry entry})
      (json-response 404 {:ok false
                          :err "evidence-not-found"
                          :evidence-id evidence-id}))))

(defn- handle-evidence-chain
  "GET /api/alpha/evidence/:id/chain — fetch ordered ancestor chain."
  [config evidence-id]
  (let [evidence-store (evidence-store-for-config config)
        entry (estore/get-entry* evidence-store evidence-id)]
    (if (nil? entry)
      (json-response 404 {:ok false
                          :err "evidence-not-found"
                          :evidence-id evidence-id})
      (let [chain (estore/get-reply-chain* evidence-store evidence-id)]
        (json-response 200 {:ok true
                            :evidence-id evidence-id
                            :chain chain})))))

(defn- parse-json-map
  "Parse BODY as JSON map; return nil on parse/shape failure."
  [body]
  (try
    (let [parsed (json/parse-string (or body "") true)]
      (when (map? parsed)
        parsed))
    (catch Exception _ nil)))

(defn- append-error-status
  "Map evidence append error codes to HTTP status."
  [code]
  (case code
    :duplicate-id 409
    :reply-not-found 409
    :fork-not-found 409
    :invalid-entry 400
    :invalid-input 400
    400))

(defn- normalize-evidence-payload
  "Normalize write payload (string fields to keywords where needed)."
  [payload]
  (let [subject (or (normalize-artifact-ref (:subject payload))
                    (let [subject-type (parse-keyword (or (:subject-type payload) (get payload "subject-type")))
                          subject-id (or (:subject-id payload) (get payload "subject-id"))]
                      (when (and subject-type (some? subject-id) (not (str/blank? (str subject-id))))
                        {:ref/type subject-type
                         :ref/id (str subject-id)})))
        entry {:evidence-id (or (:evidence-id payload) (get payload "evidence-id"))
               :subject subject
               :type (parse-keyword (or (:type payload) (get payload "type")))
               :claim-type (parse-keyword (or (:claim-type payload) (get payload "claim-type")))
               :author (or (:author payload) (get payload "author"))
               :body (or (:body payload) (get payload "body"))
               :pattern-id (parse-keyword (or (:pattern-id payload) (get payload "pattern-id")))
               :session-id (or (:session-id payload) (get payload "session-id"))
               :in-reply-to (or (:in-reply-to payload) (get payload "in-reply-to"))
               :fork-of (or (:fork-of payload) (get payload "fork-of"))
               :conjecture? (or (:conjecture? payload) (get payload "conjecture?"))
               :ephemeral? (or (:ephemeral? payload) (get payload "ephemeral?"))
               :tags (or (normalize-tags (:tags payload))
                         (normalize-tags (get payload "tags")))}]
    (-> entry
        (cond-> (or (nil? (:evidence-id entry))
                    (str/blank? (str (:evidence-id entry))))
          (dissoc :evidence-id))
        (cond-> (nil? (:subject entry))
          (dissoc :subject))
        (cond-> (nil? (:pattern-id entry))
          (dissoc :pattern-id))
        (cond-> (or (nil? (:session-id entry))
                    (str/blank? (str (:session-id entry))))
          (dissoc :session-id))
        (cond-> (or (nil? (:in-reply-to entry))
                    (str/blank? (str (:in-reply-to entry))))
          (dissoc :in-reply-to))
        (cond-> (or (nil? (:fork-of entry))
                    (str/blank? (str (:fork-of entry))))
          (dissoc :fork-of))
        (cond-> (nil? (:conjecture? entry))
          (dissoc :conjecture?))
        (cond-> (nil? (:ephemeral? entry))
          (dissoc :ephemeral?))
        (cond-> (empty? (:tags entry))
          (dissoc :tags)))))

(defn- handle-evidence-create
  "POST /api/alpha/evidence — append one evidence entry."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false
                          :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [evidence-store (evidence-store-for-config config)
            normalized (normalize-evidence-payload payload)
            result (estore/append* evidence-store normalized)]
        (if (:ok result)
          (json-response 201 {:ok true
                              :evidence/id (get-in result [:entry :evidence/id])
                              :entry (:entry result)})
          (json-response (append-error-status (:error/code result))
                         {:ok false
                          :err (name (:error/code result))
                          :error result}))))))

;; =============================================================================
;; Agent registration endpoints
;; =============================================================================

(def ^:private default-capabilities
  {:claude [:explore :edit :test :coordination/execute]
   :codex  [:edit :test :coordination/execute]
   :tickle [:mission-control :discipline :coordination/execute]})

(defn- handle-agents-register
  "POST /api/alpha/agents — register an agent via HTTP.
   Body: {\"agent-id\": \"codex-1\", \"type\": \"codex\",
          \"origin-url\": \"http://...\", \"proxy\": true}
   Local registration (no origin-url): no-op invoke-fn, agent connects via WS later.
   Proxy registration (with origin-url): invoke-fn forwards to origin Agency."
  [request _config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            agent-type-str (or (:type payload) (get payload "type"))
            agent-type (parse-keyword agent-type-str)
            origin-url (or (:origin-url payload) (get payload "origin-url"))
            proxy? (or (:proxy payload) (get payload "proxy") (some? origin-url))
            caps-raw (or (:capabilities payload) (get payload "capabilities"))
            capabilities (if (sequential? caps-raw)
                           (mapv keyword caps-raw)
                           (get default-capabilities agent-type []))]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? agent-type)
          (json-response 400 {:ok false :err "missing-type"
                              :message "type is required (claude, codex, tickle, mock)"})

          :else
          (let [invoke-fn (if (and proxy? origin-url (not (str/blank? origin-url)))
                            (federation/make-proxy-invoke-fn origin-url agent-id)
                            (fn [_prompt _session-id]
                              {:result "registered-via-http" :session-id nil}))
                result (reg/register-agent!
                        {:agent-id {:id/value (str agent-id) :id/type :continuity}
                         :type agent-type
                         :invoke-fn invoke-fn
                         :capabilities capabilities
                         :metadata (cond-> {}
                                     proxy? (assoc :proxy? true)
                                     origin-url (assoc :origin-url origin-url))})]
            (if (and (map? result) (:ok result) (= false (:ok result)))
              (json-response 409 {:ok false
                                  :err "duplicate-registration"
                                  :message (str "Agent already registered: " agent-id)})
              (if (and (map? result) (:agent/id result))
                (json-response 201 {:ok true
                                    :agent-id (get-in result [:agent/id :id/value])
                                    :type (name (:agent/type result))
                                    :proxy proxy?})
                (json-response 409 {:ok false
                                    :err "registration-failed"
                                    :message (str "Could not register: " agent-id)
                                    :detail result})))))))))

(defn- handle-agents-list
  "GET /api/alpha/agents — list all registered agents."
  [_config]
  (let [status (reg/registry-status)]
    (json-response 200 {:ok true
                        :count (:count status)
                        :agents (:agents status)})))

(defn- handle-agent-get
  "GET /api/alpha/agents/:id — return a single agent's details."
  [_config agent-id]
  (let [status (reg/registry-status)
        agent (get-in status [:agents agent-id])]
    (if agent
      (json-response 200 {:ok true :agent-id agent-id :agent agent})
      (json-response 404 {:ok false :error (str "Agent not found: " agent-id)}))))

(defn- handle-agent-delete
  "DELETE /api/alpha/agents/:id — deregister an agent."
  [_config agent-id]
  (let [result (reg/deregister-agent! agent-id)]
    (if (:ok result)
      (json-response 200 {:ok true :agent-id agent-id :deregistered true})
      (json-response 404 {:ok false :error (str "Agent not found: " agent-id)}))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn make-handler
  "Create an HTTP request handler wired to the social pipeline.

   config:
     :registry  — AgentRegistryShape (may include :peripheral-config)
     :patterns  — PatternLibrary

   Returns a Ring handler fn that routes to the social pipeline."
  [config]
  (let [started-at (Instant/now)]
    (fn [request]
      (let [method (:request-method request)
            uri (:uri request)]
        (cond
          (and (= :post method) (= "/dispatch" uri))
          (handle-dispatch request config)

          (and (= :post method) (= "/presence" uri))
          (handle-presence request config)

          (and (= :get method) (string? uri) (str/starts-with? uri "/session/"))
          (handle-get-session request config)

          (and (= :post method) (= "/api/alpha/evidence" uri))
          (handle-evidence-create request config)

          (and (= :get method) (= "/api/alpha/evidence" uri))
          (handle-evidence-query request config)

          (and (= :get method) (= "/api/alpha/evidence/count" uri))
          (handle-evidence-count request config)

          (and (= :get method) (re-matches #"/api/alpha/evidence/(.+)/chain" uri))
          (let [[_ raw-id] (re-find #"/api/alpha/evidence/(.+)/chain" uri)
                evidence-id (enc/decode-uri-component raw-id)]
            (handle-evidence-chain config evidence-id))

          (and (= :get method) (re-matches #"/api/alpha/evidence/(.+)" uri))
          (let [[_ raw-id] (re-find #"/api/alpha/evidence/(.+)" uri)
                evidence-id (enc/decode-uri-component raw-id)]
            (handle-evidence-entry config evidence-id))

          (and (= :post method) (= "/api/alpha/agents" uri))
          (handle-agents-register request config)

          (and (= :delete method) (re-matches #"/api/alpha/agents/(.+)" uri))
          (let [[_ agent-id] (re-find #"/api/alpha/agents/(.+)" uri)]
            (handle-agent-delete config agent-id))

          (and (= :get method) (re-matches #"/api/alpha/agents/(.+)" uri))
          (let [[_ agent-id] (re-find #"/api/alpha/agents/(.+)" uri)]
            (handle-agent-get config agent-id))

          (and (= :get method) (= "/api/alpha/agents" uri))
          (handle-agents-list config)

          (and (= :get method) (= "/health" uri))
          (handle-health config started-at)

          (and (= :get method) (= "/fulab/encyclopedia/corpuses" uri))
          (handle-encyclopedia-corpuses config)

          (and (= :get method) (re-matches #"/fulab/encyclopedia/([^/]+)/entries" uri))
          (let [[_ corpus] (re-find #"/fulab/encyclopedia/([^/]+)/entries" uri)
                corpus-name (enc/decode-uri-component corpus)]
            (handle-encyclopedia-entries request config corpus-name))

          (and (= :get method) (re-matches #"/fulab/encyclopedia/([^/]+)/entry/(.+)" uri))
          (let [[_ corpus raw-entry-id] (re-find #"/fulab/encyclopedia/([^/]+)/entry/(.+)" uri)
                corpus-name (enc/decode-uri-component corpus)
                entry-id (enc/decode-uri-component raw-entry-id)]
            (handle-encyclopedia-entry config corpus-name entry-id))

          :else
          (json-response 404 {"error" true
                              "code" "not-found"
                              "message" (str "Unknown endpoint: "
                                            (some-> method name str/upper-case)
                                            " " uri)}))))))

(defn start-server!
  "Start HTTP server on port. Returns {:server stop-fn :port p :started-at t}.

   Per realtime/verify-after-start (L4, L7): after start(), sleeps briefly
   then probes the port by attempting a TCP connection. Converts silent binding
   failures into immediate, actionable errors.

   Call (:server result) to stop the server (it's the stop function)."
  [handler port]
  (let [!server (atom nil)
        !stopped? (atom false)
        server (hk/run-server handler {:port port
                                       :legacy-return-value? false
                                       :error-logger (make-http-kit-error-logger !server)})
        _ (reset! !server server)
        stop-fn (fn [& {:keys [timeout] :or {timeout 100}}]
                  ;; Keep `:server` return value as a callable stop fn.
                  ;; Stop is idempotent and suppresses the known http-kit/JDK
                  ;; selector close race (NPE in AbstractSelectableChannel/removeKey).
                  (when (compare-and-set! !stopped? false true)
                    (try
                      (hk/server-stop! server {:timeout timeout})
                      (catch NullPointerException ex
                        (when-not (expected-http-kit-stop-race? ex)
                          (throw ex)))))
                  nil)
        _ (Thread/sleep 100)
        listening? (try
                     (with-open [sock (Socket.)]
                       (.connect sock (InetSocketAddress. "localhost" (int port)) 1000)
                       true)
                     (catch Exception _ false))]
    (if listening?
      {:server stop-fn :port port :started-at (str (Instant/now))}
      (do (stop-fn)
          (throw (ex-info "Server started but port is not listening (L7: verify-after-start)"
                          {:port port}))))))
