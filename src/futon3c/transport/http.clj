(ns futon3c.transport.http
  "HTTP REST adapter — thin Ring handler wired to the social pipeline.

   Adapter as boundary — not business logic. The handler only translates
   between HTTP requests/responses and pipeline function calls. No routing
   logic, mode classification, or evidence emission lives here.

   Routes:
     POST /dispatch  — classify + dispatch, return receipt or error
     POST /presence  — verify presence, return record or error
     GET  /session/:id — retrieve session by ID
     GET  /health    — liveness check with agent/session counts

   Pattern references:
   - realtime/verify-after-start (L4, L7): start-server! probes port after
     start to confirm it is actually listening. Async startup can hide binding
     failures (port in use, permission denied).
   - realtime/request-param-resilience (L1, L3): delegates param extraction
     to protocol/extract-params for consistency across HTTP and WS."
  (:require [futon3c.transport.protocol :as proto]
            [futon3c.social.mode :as mode]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.presence :as presence]
            [futon3c.social.persist :as persist]
            [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.net Socket InetSocketAddress]))

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

;; =============================================================================
;; Route handlers
;; =============================================================================

(defn- handle-dispatch
  "POST /dispatch — parse JSON body, classify message, dispatch to agent.
   Returns 200 + DispatchReceipt JSON or error status + SocialError JSON."
  [request config]
  (let [body (read-body request)
        parsed (proto/parse-dispatch-request (or body ""))]
    (if (error? parsed)
      (error-response parsed)
      (let [classified (mode/classify parsed (:patterns config))]
        (if (error? classified)
          (error-response classified)
          (let [result (dispatch/dispatch classified (:registry config))]
            (if (error? result)
              (error-response result)
              (json-response 200 (proto/render-receipt result)))))))))

(defn- handle-presence
  "POST /presence — parse JSON body, verify agent presence via S-presence.
   Returns 200 + PresenceRecord JSON or error status + SocialError JSON."
  [request config]
  (let [body (read-body request)
        parsed (proto/parse-presence-request (or body ""))]
    (if (error? parsed)
      (error-response parsed)
      (let [result (presence/verify parsed (:registry config))]
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
  "GET /health — return agent and session counts."
  [config]
  (json-response 200 {"status" "ok"
                       "agents" (count (get-in config [:registry :agents]))
                       "sessions" (count (persist/list-sessions {}))}))

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

        (and (= :get method) (= "/health" uri))
        (handle-health config)

        :else
        (json-response 404 {"error" true
                            "code" "not-found"
                            "message" (str "Unknown endpoint: "
                                          (some-> method name str/upper-case)
                                          " " uri)})))))

(defn start-server!
  "Start HTTP server on port. Returns {:server stop-fn :port p :started-at t}.

   Per realtime/verify-after-start (L4, L7): after start(), sleeps briefly
   then probes the port by attempting a TCP connection. Converts silent binding
   failures into immediate, actionable errors.

   Call (:server result) to stop the server (it's the stop function)."
  [handler port]
  (let [stop-fn (hk/run-server handler {:port port})
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
