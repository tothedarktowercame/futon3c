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
     POST /api/alpha/irc/send — post a line to IRC via server relay
     GET  /api/alpha/reflect/namespaces — list loaded Clojure namespaces
     GET  /api/alpha/reflect/ns/:ns — public vars in a namespace
     GET  /api/alpha/reflect/ns/:ns/full — all vars (public + private)
     GET  /api/alpha/reflect/var/:ns/:var — full var metadata (envelope)
     GET  /api/alpha/reflect/deps/:ns — namespace dependency graph
     GET  /api/alpha/reflect/java/:class — Java class reflection
     POST /api/alpha/todo — lightweight todo management (add/list/done)
     POST /api/alpha/portfolio/step — run one AIF portfolio step
     POST /api/alpha/portfolio/heartbeat — weekly heartbeat with bid/clear
     GET  /api/alpha/portfolio/state — current portfolio belief state
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
            [futon3c.social.whistles :as whistles]
            [futon3c.mission-control.service :as mcs]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.reflection.core :as reflection]
            [cheshire.core :as json]
            [cheshire.generate :as json-gen]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as str]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.net Socket InetSocketAddress]
           [java.nio.channels AsynchronousCloseException ClosedChannelException ClosedSelectorException]
           [java.util UUID]
           [org.httpkit.logger ContextLogger]))

;; =============================================================================
;; JSON encoders for java.time types (Cheshire doesn't handle them natively)
;; =============================================================================

(json-gen/add-encoder Instant
  (fn [val jg]
    (.writeString jg (str val))))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- read-body
  "Read request body as a string. Handles both InputStream and String."
  [request]
  (when-let [body (:body request)]
    (if (string? body) body (slurp body))))

(defn- json-response
  "Build a Ring response with JSON content-type.
   If serialization fails, returns a 500 with the error message
   rather than letting the exception propagate and wedge the server."
  [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (if (string? body)
           body
           (try
             (json/generate-string body)
             (catch Exception e
               (json/generate-string
                {:ok false
                 :error "json-serialization-failed"
                 :message (.getMessage e)}))))})

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
        evidence-count (count (estore/query* evidence-store {}))
        irc-send-base (some-> (:irc-send-base config) str str/trim not-empty)
        irc-relay-configured? (fn? (:irc-send-fn config))]
    (json-response 200 {"status" "ok"
                         "agents" (max live-count config-count)
                         "sessions" (count (persist/list-sessions {}))
                         "agent-summary" agent-summary
                         "irc-relay-configured" irc-relay-configured?
                         "irc-send-base" irc-send-base
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
          \"origin-url\": \"http://...\", \"proxy\": true,
          \"ws-bridge\": true}
   Local registration (no origin-url): default no-op invoke-fn.
   Set ws-bridge=true to register with no local invoke-fn and use WS fallback.
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
            ws-bridge? (boolean (or (:ws-bridge payload)
                                    (get payload "ws-bridge")
                                    (:ws_bridge payload)
                                    (get payload "ws_bridge")))
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
          (let [invoke-fn (cond
                            (and proxy? origin-url (not (str/blank? origin-url)))
                            (federation/make-proxy-invoke-fn origin-url agent-id)

                            ws-bridge?
                            nil

                            :else
                            (fn [_prompt _session-id]
                              {:result "registered-via-http" :session-id nil}))
                result (reg/register-agent!
                        {:agent-id {:id/value (str agent-id) :id/type :continuity}
                         :type agent-type
                         :invoke-fn invoke-fn
                         :capabilities capabilities
                         :metadata (cond-> {}
                                     proxy? (assoc :proxy? true)
                                     ws-bridge? (assoc :ws-bridge? true)
                                     origin-url (assoc :origin-url origin-url))})]
            (if (and (map? result) (= false (:ok result)))
              (json-response 409 {:ok false
                                  :err "duplicate-registration"
                                  :message (str "Agent already registered: " agent-id)
                                  :detail result})
              (if (and (map? result) (:agent/id result))
                (json-response 201 {:ok true
                                    :agent-id (get-in result [:agent/id :id/value])
                                    :type (name (:agent/type result))
                                    :proxy proxy?
                                    :ws-bridge ws-bridge?})
                (json-response 409 {:ok false
                                    :err "registration-failed"
                                    :message (str "Could not register: " agent-id)
                                    :detail result})))))))))

(defn- emit-invoke-evidence!
  "Emit a forum-post evidence entry for an invoke prompt or response.
   Mirrors the pattern used by the IRC transport so chat messages from
   all surfaces are queryable in the same evidence landscape.
   When mission-id is provided, tags evidence with that mission subject."
  [evidence-store author text session-id & {:keys [mission-id]}]
  (when evidence-store
    (try
      (let [subject (if mission-id
                      {:ref/type :mission :ref/id mission-id}
                      {:ref/type :thread :ref/id "emacs/chat"})
            tags (cond-> [:emacs :chat :transport/emacs-chat]
                   mission-id (conj :mission-focused))]
        (estore/append* evidence-store
                        {:evidence/id (str "e-" (UUID/randomUUID))
                         :evidence/subject subject
                         :evidence/type :forum-post
                         :evidence/claim-type :observation
                         :evidence/author author
                         :evidence/at (str (Instant/now))
                         :evidence/body {:channel "emacs-chat"
                                         :text text
                                         :from author
                                         :transport :emacs-chat}
                         :evidence/tags tags
                         :evidence/session-id (or session-id "pending")}))
      (catch Exception e
        (println (str "[invoke] evidence emit warning: " (.getMessage e)))))))

(defn- emit-review-snapshot!
  "Emit a portfolio snapshot evidence entry after a successful review.
   Stores compact mission id+status pairs for diffing between reviews."
  [evidence-store author review-result]
  (when evidence-store
    (try
      (let [missions (get review-result "portfolio/missions"
                       (:portfolio/missions review-result))
            summary (get review-result "portfolio/summary"
                      (:portfolio/summary review-result))
            coverage (get review-result "portfolio/coverage"
                       (:portfolio/coverage review-result))
            ;; Compact form: just id + status per mission
            compact-missions (vec (for [m (or missions [])]
                                   (let [mid (or (get m "mission/id")
                                                 (:mission/id m) "?")
                                         status (or (get m "mission/status")
                                                    (:mission/status m) "unknown")]
                                     {:mission/id mid :mission/status status})))]
        (estore/append* evidence-store
                        {:evidence/id (str "e-review-" (UUID/randomUUID))
                         :evidence/subject {:ref/type :portfolio :ref/id "global"}
                         :evidence/type :coordination
                         :evidence/claim-type :observation
                         :evidence/author (or author "mission-control")
                         :evidence/at (str (Instant/now))
                         :evidence/body {:portfolio/missions compact-missions
                                         :portfolio/summary summary
                                         :portfolio/coverage coverage}
                         :evidence/tags [:review :portfolio-snapshot]}))
      (catch Exception e
        (println (str "[review] snapshot emit warning: " (.getMessage e)))))))

(defn- handle-invoke
  "POST /api/alpha/invoke — invoke a registered agent directly.
   Body: {\"agent-id\": \"claude-1\", \"prompt\": \"hello\"}
   Returns the invoke-fn result: {\"ok\": true, \"result\": \"...\", \"session-id\": \"...\"}
   or error: {\"ok\": false, \"error\": \"...\", \"message\": \"...\"}

   This is the direct invocation endpoint — the futon3c equivalent of futon3's
   POST /agency/page. Both Emacs and IRC peripherals route through this.
   Emits evidence entries for both prompt and response (parity with IRC transport)."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            prompt (or (:prompt payload) (get payload "prompt"))
            evidence-store (evidence-store-for-config config)]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? prompt)
          (json-response 400 {:ok false :err "missing-prompt"
                              :message "prompt is required"})

          :else
          (let [caller (or (some-> payload :caller str)
                           (some-> payload (get "caller") str)
                           "http-caller")
                mission-id (or (:mission-id payload) (get payload "mission-id"))
                timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                                   long)
                ev-opts (when mission-id [:mission-id mission-id])]
            (let [result (reg/invoke-agent! (str agent-id) prompt timeout-ms)
                  sid (:session-id result)]
              (apply emit-invoke-evidence! evidence-store caller (str prompt) sid
                     (or ev-opts []))
              (if (:ok result)
                (do
                  (apply emit-invoke-evidence! evidence-store (str agent-id) (str (:result result)) sid
                         (or ev-opts []))
                  (json-response 200 {:ok true
                                      :result (:result result)
                                      :session-id sid}))
                (let [err (:error result)
                      code (if (map? err) (:error/code err) :invoke-failed)
                      msg (if (map? err) (:error/message err) (str err))]
                  (json-response (if (= :agent-not-found code) 404 502)
                                 {:ok false
                                  :error (name code)
                                  :message msg}))))))))))

(defn- handle-whistle
  "POST /api/alpha/whistle — synchronous request-response to a registered agent.
   Body: {\"agent-id\": \"codex-1\", \"prompt\": \"...\", \"timeout-ms\": 60000}
   Delegates to whistles/whistle! which wraps invoke-agent! with evidence."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [agent-id (or (:agent-id payload) (get payload "agent-id"))
            prompt (or (:prompt payload) (get payload "prompt"))
            timeout-ms (some-> (or (:timeout-ms payload) (get payload "timeout-ms"))
                               long)
            caller (or (some-> payload :caller str)
                       (some-> payload (get "caller") str)
                       "http-caller")
            evidence-store (evidence-store-for-config config)]
        (cond
          (or (nil? agent-id) (str/blank? (str agent-id)))
          (json-response 400 {:ok false :err "missing-agent-id"
                              :message "agent-id is required"})

          (nil? prompt)
          (json-response 400 {:ok false :err "missing-prompt"
                              :message "prompt is required"})

          :else
          (let [result (whistles/whistle!
                        {:agent-id (str agent-id)
                         :prompt prompt
                         :author caller
                         :timeout-ms timeout-ms
                         :evidence-store evidence-store})]
            (if (:whistle/ok result)
              (json-response 200 {:ok true
                                  :response (:whistle/response result)
                                  :agent-id (:whistle/agent-id result)
                                  :session-id (:whistle/session-id result)})
              (let [err (:whistle/error result)]
                (json-response
                 (if (and (string? err) (.contains ^String err "not registered")) 404 502)
                 {:ok false
                  :error err
                  :agent-id (:whistle/agent-id result)})))))))))

(defn- handle-irc-send
  "POST /api/alpha/irc/send — send a one-line IRC message via configured relay.
   Body: {\"channel\": \"#futon\", \"text\": \"...\", \"from\": \"codex\"}."
  [request config]
  (let [payload (parse-json-map (read-body request))
        send-fn (:irc-send-fn config)]
    (cond
      (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})

      (not (fn? send-fn))
      (json-response 503 {:ok false :err "irc-unavailable"
                          :message "IRC relay is not configured on this node"})

      :else
      (let [channel (or (:channel payload) (get payload "channel"))
            text (or (:text payload) (get payload "text"))
            from (or (:from payload) (get payload "from") "codex")]
        (cond
          (or (nil? channel) (str/blank? (str channel)))
          (json-response 400 {:ok false :err "missing-channel"
                              :message "channel is required"})

          (or (nil? text) (str/blank? (str text)))
          (json-response 400 {:ok false :err "missing-text"
                              :message "text is required"})

          :else
          (try
            (send-fn (str channel) (str from) (str text))
            (json-response 200 {:ok true
                                :channel (str channel)
                                :from (str from)
                                :text (str text)})
            (catch Exception e
              (json-response 502 {:ok false :err "irc-send-failed"
                                  :message (.getMessage e)}))))))))

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
  (let [result (reg/unregister-agent! agent-id)]
    (if (:ok result)
      (json-response 200 {:ok true :agent-id agent-id :deregistered true})
      (let [err (:error result)
            code (if (map? err) (:error/code err) :deregister-failed)]
        (json-response (if (= :agent-not-found code) 404 502)
                       {:ok false
                        :error (if (= :agent-not-found code)
                                 (str "Agent not found: " agent-id)
                                 (if (map? err)
                                   (:error/message err)
                                   (str err)))})))))

(defn- handle-agent-reset-session
  "POST /api/alpha/agents/:id/reset-session — clear an agent's session so the
   next invoke starts a fresh conversation. Useful when a session becomes
   poisoned (e.g. invalid tool-use in conversation history)."
  [_config agent-id]
  (let [result (reg/reset-session! agent-id)]
    (if (:ok result)
      (json-response 200 {:ok true
                          :agent-id (:agent-id result)
                          :old-session-id (:old-session-id result)})
      (let [err (:error result)
            code (if (map? err) (:error/code err) :reset-failed)]
        (json-response (if (= :agent-not-found code) 404 502)
                       {:ok false
                        :error (if (= :agent-not-found code)
                                 (str "Agent not found: " agent-id)
                                 (if (map? err)
                                   (:error/message err)
                                   (str err)))})))))

;; =============================================================================
;; Mission-control endpoints
;; =============================================================================

(defn- compute-portfolio-diff
  "Compute diff between two portfolio snapshots (newest first).
   Returns {:added [...] :removed [...] :changed [...] :summary {...}}."
  [new-snapshot old-snapshot]
  (let [new-missions (:portfolio/missions (:evidence/body new-snapshot))
        old-missions (:portfolio/missions (:evidence/body old-snapshot))
        new-by-id (into {} (map (juxt :mission/id identity)) new-missions)
        old-by-id (into {} (map (juxt :mission/id identity)) old-missions)
        new-ids (set (keys new-by-id))
        old-ids (set (keys old-by-id))
        added (vec (for [id (sort (cset/difference new-ids old-ids))]
                     (get new-by-id id)))
        removed (vec (for [id (sort (cset/difference old-ids new-ids))]
                       (get old-by-id id)))
        changed (vec (for [id (sort (cset/intersection new-ids old-ids))
                           :let [new-status (:mission/status (get new-by-id id))
                                 old-status (:mission/status (get old-by-id id))]
                           :when (not= new-status old-status)]
                       {:mission/id id
                        :old-status old-status
                        :new-status new-status}))
        new-summary (:portfolio/summary (:evidence/body new-snapshot))
        old-summary (:portfolio/summary (:evidence/body old-snapshot))
        new-coverage (:portfolio/coverage (:evidence/body new-snapshot))
        old-coverage (:portfolio/coverage (:evidence/body old-snapshot))]
    {:added added
     :removed removed
     :changed changed
     :new-count (count new-missions)
     :old-count (count old-missions)
     :new-summary new-summary
     :old-summary old-summary
     :new-coverage new-coverage
     :old-coverage old-coverage}))

(defn- handle-mission-control
  "POST /api/alpha/mission-control — multiplexed mission-control RPC.
   Actions: review, status, sessions, step, start, stop, diff.
   Wraps futon3c.mission-control.service functions."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [action (or (:action payload) (get payload "action"))
            evidence-store (evidence-store-for-config config)]
        (case (str action)
          "review"
          (let [author (or (:author payload) (get payload "author"))
                session-id (or (:session-id payload) (get payload "session-id"))
                close? (boolean (or (:close payload) (get payload "close")))
                result (mcs/run-review!
                        {:session-id session-id
                         :author author
                         :close? close?})]
            ;; Emit portfolio snapshot on successful review
            (when (and (:ok result) evidence-store)
              (let [lr (:last-result result)
                    review-data (if (map? lr) (:result lr) {})]
                (when (map? review-data)
                  (emit-review-snapshot! evidence-store author review-data))))
            (json-response 200 result))

          "diff"
          (if-not evidence-store
            (json-response 500 {:ok false :err "no-evidence-store"
                                :message "Evidence store not configured"})
            (let [snapshots (estore/query* evidence-store
                                          {:query/subject {:ref/type :portfolio
                                                           :ref/id "global"}
                                           :query/type :coordination
                                           :query/limit 100})
                  ;; Filter to portfolio-snapshot tagged entries
                  snapshots (->> snapshots
                                 (filter (fn [e]
                                           (let [tags (set (:evidence/tags e))]
                                             (and (contains? tags :review)
                                                  (contains? tags :portfolio-snapshot)))))
                                 (take 2)
                                 vec)]
              (if (< (count snapshots) 2)
                (json-response 200 {:ok true
                                    :diff nil
                                    :message "Not enough review history — run !mc review at least twice"})
                (let [diff (compute-portfolio-diff (first snapshots) (second snapshots))]
                  (json-response 200 {:ok true :diff diff})))))

          "status"
          (json-response 200 (mcs/status))

          "sessions"
          (json-response 200 {:ok true :sessions (mcs/list-sessions)})

          "step"
          (let [session-id (or (:session-id payload) (get payload "session-id"))
                tool (parse-keyword (or (:tool payload) (get payload "tool")))
                args (or (:args payload) (get payload "args") [])]
            (if (or (nil? session-id) (nil? tool))
              (json-response 400 {:ok false :err "missing-params"
                                  :message "step requires session-id and tool"})
              (json-response 200 (mcs/step! session-id tool (vec args)))))

          "start"
          (let [session-id (or (:session-id payload) (get payload "session-id"))
                author (or (:author payload) (get payload "author"))]
            (json-response 200 (mcs/start-session!
                                (cond-> {}
                                  session-id (assoc :session-id session-id)
                                  author (assoc :author author)))))

          "stop"
          (let [session-id (or (:session-id payload) (get payload "session-id"))
                reason (or (:reason payload) (get payload "reason") "stopped via API")]
            (if (nil? session-id)
              (json-response 400 {:ok false :err "missing-session-id"
                                  :message "stop requires session-id"})
              (json-response 200 (mcs/stop-session! session-id reason))))

          ;; default
          (json-response 400 {:ok false :err "unknown-action"
                              :message (str "Unknown action: " action
                                            ". Valid: review, status, sessions, step, start, stop, diff")}))))))

(defn- load-mission-wiring-edn
  "Load a per-mission wiring diagram from holes/missions/.
   Convention: M-foo → foo-wiring.edn or M-foo-wiring.edn"
  [mission-id]
  (let [cwd (System/getProperty "user.dir")
        ;; Try {name}-wiring.edn (strip M- prefix)
        short-name (str/replace (str mission-id) #"^M-" "")
        candidates [(io/file cwd "holes" "missions" (str short-name "-wiring.edn"))
                    (io/file cwd "holes" "missions" (str "M-" short-name "-wiring.edn"))
                    (io/file cwd "holes" "missions" (str mission-id "-wiring.edn"))]]
    (some (fn [^java.io.File f]
            (when (.exists f)
              (try (edn/read-string (slurp f))
                   (catch Exception _ nil))))
          candidates)))

(defn- handle-missions
  "GET /api/alpha/missions — cross-repo mission inventory."
  [_request _config]
  (let [missions (mcb/build-inventory)]
    (json-response 200 {:ok true
                        :missions missions
                        :count (count missions)})))

(defn- handle-mission-detail
  "GET /api/alpha/missions/:id — single mission info + wiring diagram."
  [_config mission-id]
  (let [missions (mcb/build-inventory)
        mission (first (filter #(= mission-id (:mission/id %)) missions))
        wiring (load-mission-wiring-edn mission-id)]
    (if mission
      (json-response 200 {:ok true
                          :mission mission
                          :wiring wiring})
      (json-response 404 {:ok false
                          :err "mission-not-found"
                          :message (str "No mission found: " mission-id)}))))

(defn- handle-mission-wiring
  "GET /api/alpha/missions/:id/wiring — per-mission wiring diagram."
  [_config mission-id]
  (let [wiring (load-mission-wiring-edn mission-id)]
    (if wiring
      (json-response 200 {:ok true :wiring wiring})
      (json-response 404 {:ok false
                          :err "not-found"
                          :message (str "No wiring diagram for mission " mission-id)}))))

;; =============================================================================
;; Todo endpoints — lightweight task management via evidence entries
;; =============================================================================

(defn- handle-todo
  "POST /api/alpha/todo — lightweight todo management.
   Actions: add, list, done.
   Todos are stored as evidence entries with tags [:todo :pending]."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :err "invalid-json"
                          :message "Request body must be a JSON object"})
      (let [action (or (:action payload) (get payload "action"))
            evidence-store (evidence-store-for-config config)]
        (case (str action)
          "add"
          (let [text (or (:text payload) (get payload "text"))
                author (or (:author payload) (get payload "author") "anonymous")
                todo-id (str "todo-" (java.util.UUID/randomUUID))
                entry {:subject {:ref/type :task :ref/id todo-id}
                       :type :coordination
                       :claim-type :goal
                       :author author
                       :body {:text text}
                       :tags [:todo :pending]}
                result (estore/append* evidence-store entry)]
            (if (and (map? result) (:evidence/id result))
              (json-response 201 {:ok true
                                  :id todo-id
                                  :evidence-id (:evidence/id result)
                                  :text text})
              (if (:ok result)
                (json-response 201 {:ok true
                                    :id todo-id
                                    :evidence-id (get-in result [:entry :evidence/id])
                                    :text text})
                (json-response 400 {:ok false :err "append-failed"
                                    :error result}))))

          "list"
          (let [author (or (:author payload) (get payload "author"))
                all-goals (estore/query* evidence-store {:query/claim-type :goal})
                pending (->> all-goals
                             (filter (fn [e]
                                       (let [tags (set (:evidence/tags e))]
                                         (and (contains? tags :todo)
                                              (contains? tags :pending)))))
                             (filter (fn [e]
                                       (or (nil? author)
                                           (= author (:evidence/author e))))))
                done-ids (->> (estore/query* evidence-store {:query/claim-type :conclusion})
                              (filter (fn [e]
                                        (let [tags (set (:evidence/tags e))]
                                          (and (contains? tags :todo)
                                               (contains? tags :done)))))
                              (map (fn [e] (get-in e [:evidence/subject :ref/id])))
                              set)
                todos (->> pending
                           (remove (fn [e] (done-ids (get-in e [:evidence/subject :ref/id]))))
                           (map (fn [e]
                                  {:id (get-in e [:evidence/subject :ref/id])
                                   :text (get-in e [:evidence/body :text])
                                   :author (:evidence/author e)
                                   :at (:evidence/at e)}))
                           (sort-by :at)
                           vec)]
            (json-response 200 {:ok true :todos todos :count (count todos)}))

          "done"
          (let [todo-id (or (:id payload) (get payload "id"))
                author (or (:author payload) (get payload "author") "anonymous")]
            (if (or (nil? todo-id) (str/blank? (str todo-id)))
              (json-response 400 {:ok false :err "missing-id"
                                  :message "done requires id"})
              (let [entry {:subject {:ref/type :task :ref/id (str todo-id)}
                           :type :coordination
                           :claim-type :conclusion
                           :author author
                           :body {:completed true}
                           :tags [:todo :done]}
                    result (estore/append* evidence-store entry)]
                (if (or (:ok result) (:evidence/id result))
                  (json-response 200 {:ok true :id (str todo-id)})
                  (json-response 400 {:ok false :err "append-failed"
                                      :error result})))))

          ;; default
          (json-response 400 {:ok false :err "unknown-action"
                              :message (str "Unknown action: " action ". Valid: add, list, done")}))))))

;; =============================================================================
;; Reflection endpoints — Clojure runtime introspection
;; =============================================================================

(defn- handle-reflect-namespaces
  "GET /api/alpha/reflect/namespaces — list loaded namespaces."
  [request]
  (let [pattern (get-in request [:query-params "pattern"])
        result (if pattern
                 (reflection/list-namespaces pattern)
                 (reflection/list-namespaces))]
    (json-response 200 {:ok true :namespaces result :count (count result)})))

(defn- handle-reflect-ns
  "GET /api/alpha/reflect/ns/:ns — public vars in a namespace."
  [ns-str]
  (let [ns-sym (symbol ns-str)
        result (reflection/reflect-ns ns-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :vars result :count (count result)}))))

(defn- handle-reflect-ns-full
  "GET /api/alpha/reflect/ns/:ns/full — all vars (public + private)."
  [ns-str]
  (let [ns-sym (symbol ns-str)
        result (reflection/reflect-ns-full ns-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :vars result :count (count result)}))))

(defn- handle-reflect-var
  "GET /api/alpha/reflect/var/:ns/:var — full metadata for one var."
  [ns-str var-str]
  (let [ns-sym (symbol ns-str)
        var-sym (symbol var-str)
        result (reflection/reflect-var ns-sym var-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :envelope result}))))

(defn- handle-reflect-deps
  "GET /api/alpha/reflect/deps/:ns — namespace dependency graph."
  [ns-str]
  (let [ns-sym (symbol ns-str)
        result (reflection/reflect-deps ns-sym)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :deps result}))))

(defn- handle-reflect-java-class
  "GET /api/alpha/reflect/java/:class — reflect on a Java class."
  [class-name]
  (let [result (reflection/reflect-java-class class-name)]
    (if (:error result)
      (json-response 404 {:ok false :error (:error result)})
      (json-response 200 {:ok true :class result}))))

;; =============================================================================
;; Portfolio inference handlers
;; =============================================================================

(defn- handle-portfolio-step
  "POST /api/alpha/portfolio/step — run one AIF step, return recommendation."
  [request config]
  (let [evidence-store (evidence-store-for-config config)
        payload (or (parse-json-map (read-body request)) {})
        opts (cond-> {}
               (:emit-evidence payload)
               (assoc :emit-evidence? (boolean (:emit-evidence payload))))]
    (try
      (let [result (portfolio/portfolio-step! evidence-store opts)]
        (json-response 200
          {:ok true
           :recommendation (portfolio/format-recommendation result)
           :action (some-> (:action result) name)
           :diagnostics (:diagnostics result)
           :abstain (get-in result [:policy :abstain?])
           :structure (:structure result)}))
      (catch Exception e
        (json-response 500 {:ok false :error "portfolio-step-failed"
                            :message (.getMessage e)})))))

(defn- handle-portfolio-heartbeat
  "POST /api/alpha/portfolio/heartbeat — run heartbeat with bid/clear data."
  [request config]
  (let [payload (parse-json-map (read-body request))]
    (if (nil? payload)
      (json-response 400 {:ok false :error "invalid-json"
                          :message "Request body must be a JSON object with heartbeat data"})
      (let [evidence-store (evidence-store-for-config config)
            ;; Parse heartbeat-data from payload, keywordizing effort/outcome/mode
            heartbeat-data
            (cond-> {}
              (:bids payload)
              (assoc :bids (mapv (fn [b]
                                  (cond-> {:action (keyword (:action b))
                                           :mission (:mission b)
                                           :effort (keyword (:effort b))}))
                                (:bids payload)))
              (:clears payload)
              (assoc :clears (mapv (fn [c]
                                    (cond-> {:action (keyword (:action c))
                                             :mission (:mission c)
                                             :effort (keyword (:effort c))
                                             :outcome (keyword (:outcome c))}))
                                  (:clears payload)))
              (:mode-prediction payload)
              (assoc :mode-prediction (keyword (:mode-prediction payload)))
              (:mode-observed payload)
              (assoc :mode-observed (keyword (:mode-observed payload))))
            opts {}]
        (try
          (let [result (portfolio/portfolio-heartbeat! evidence-store heartbeat-data opts)]
            (json-response 200
              {:ok true
               :recommendation (portfolio/format-recommendation result)
               :action (some-> (:action result) name)
               :diagnostics (:diagnostics result)
               :heartbeat (:heartbeat result)}))
          (catch Exception e
            (json-response 500 {:ok false :error "portfolio-heartbeat-failed"
                                :message (.getMessage e)})))))))

(defn- handle-portfolio-state
  "GET /api/alpha/portfolio/state — return current belief state."
  [_config]
  (let [state @portfolio/!state
        mu (:mu state)
        prec (:prec state)]
    (json-response 200
      {:ok true
       :state {:mu mu
               :prec prec
               :mode (:mode mu)
               :urgency (:urgency mu)
               :tau (:tau prec)
               :step-count (:step-count state)
               :pending (:pending state)}})))

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

          (and (= :post method) (= "/api/alpha/invoke" uri))
          (handle-invoke request config)

          (and (= :post method) (= "/api/alpha/whistle" uri))
          (handle-whistle request config)

          (and (= :post method) (= "/api/alpha/irc/send" uri))
          (handle-irc-send request config)

          (and (= :post method) (= "/api/alpha/mission-control" uri))
          (handle-mission-control request config)

          (and (= :post method) (= "/api/alpha/todo" uri))
          (handle-todo request config)

          ;; Portfolio inference
          (and (= :post method) (= "/api/alpha/portfolio/step" uri))
          (handle-portfolio-step request config)

          (and (= :post method) (= "/api/alpha/portfolio/heartbeat" uri))
          (handle-portfolio-heartbeat request config)

          (and (= :get method) (= "/api/alpha/portfolio/state" uri))
          (handle-portfolio-state config)

          (and (= :get method) (= "/api/alpha/missions" uri))
          (handle-missions request config)

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/missions/")
               (str/ends-with? uri "/wiring"))
          (let [raw (subs uri (count "/api/alpha/missions/")
                         (- (count uri) (count "/wiring")))]
            (handle-mission-wiring config (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/missions/"))
          (let [raw (subs uri (count "/api/alpha/missions/"))]
            (handle-mission-detail config (enc/decode-uri-component raw)))

          (and (= :post method) (= "/api/alpha/agents" uri))
          (handle-agents-register request config)

          (and (= :post method) (string? uri)
               (str/starts-with? uri "/api/alpha/agents/")
               (str/ends-with? uri "/reset-session"))
          (let [raw (subs uri (count "/api/alpha/agents/")
                         (- (count uri) (count "/reset-session")))]
            (handle-agent-reset-session config (enc/decode-uri-component raw)))

          (and (= :delete method) (re-matches #"/api/alpha/agents/(.+)" uri))
          (let [[_ agent-id] (re-find #"/api/alpha/agents/(.+)" uri)]
            (handle-agent-delete config agent-id))

          (and (= :get method) (re-matches #"/api/alpha/agents/(.+)" uri))
          (let [[_ agent-id] (re-find #"/api/alpha/agents/(.+)" uri)]
            (handle-agent-get config agent-id))

          (and (= :get method) (= "/api/alpha/agents" uri))
          (handle-agents-list config)

          ;; Reflection endpoints
          (and (= :get method) (= "/api/alpha/reflect/namespaces" uri))
          (handle-reflect-namespaces request)

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/ns/")
               (str/ends-with? uri "/full"))
          (let [raw (subs uri (count "/api/alpha/reflect/ns/")
                         (- (count uri) (count "/full")))]
            (handle-reflect-ns-full (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/ns/"))
          (let [raw (subs uri (count "/api/alpha/reflect/ns/"))]
            (handle-reflect-ns (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/var/"))
          (let [raw (subs uri (count "/api/alpha/reflect/var/"))
                idx (.indexOf raw "/")]
            (if (pos? idx)
              (handle-reflect-var
                (enc/decode-uri-component (subs raw 0 idx))
                (enc/decode-uri-component (subs raw (inc idx))))
              (json-response 400 {:ok false :error "Expected /api/alpha/reflect/var/:ns/:var"})))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/deps/"))
          (let [raw (subs uri (count "/api/alpha/reflect/deps/"))]
            (handle-reflect-deps (enc/decode-uri-component raw)))

          (and (= :get method) (string? uri)
               (str/starts-with? uri "/api/alpha/reflect/java/"))
          (let [raw (subs uri (count "/api/alpha/reflect/java/"))]
            (handle-reflect-java-class (enc/decode-uri-component raw)))

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
