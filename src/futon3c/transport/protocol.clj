(ns futon3c.transport.protocol
  "Protocol translation layer — pure functions for HTTP/WebSocket ↔ pipeline.

   Translates between wire formats (JSON over HTTP/WS) and internal pipeline
   shapes (ClassifiedMessage, DispatchReceipt, SocialError, AgentConnection).

   This module is the boundary layer. It contains NO routing logic, mode
   classification, peripheral selection, or evidence emission. All of that
   lives in the social pipeline. This module only translates protocols.

   Pattern references:
   - realtime/request-param-resilience (L1, L3): extract-params handles
     missing :query-string on WS upgrade by falling back to :request-uri.
     Three-tier extraction: query-string → request-uri → caller fallback.
   - realtime/structured-events-only (R9): all rendered WS frames are typed
     JSON maps with a 'type' field. No free-text frames."
  (:require [cheshire.core :as json]
            [futon3c.social.shapes :as shapes]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]
           [java.net URLDecoder]))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- now-str []
  (str (Instant/now)))

(defn- transport-error
  "Create a SocialError from the transport layer."
  [code message & {:as context}]
  (cond-> {:error/component :transport
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- parse-json
  "Parse a JSON string, returning parsed map or SocialError."
  [json-str]
  (cond
    (not (string? json-str))
    (transport-error :invalid-json "Expected JSON string")

    (str/blank? json-str)
    (transport-error :invalid-json "Empty JSON string")

    :else
    (try
      (json/parse-string json-str true)
      (catch Exception e
        (transport-error :invalid-json
                         (str "Failed to parse JSON: " (.getMessage e)))))))

(defn- error? [x]
  (and (map? x) (contains? x :error/code)))

(defn- coerce-agent-id
  "Coerce a JSON agent-id to TypedAgentId.
   Accepts string (shorthand for :continuity type) or map with :value/:type."
  [raw]
  (cond
    (string? raw)
    {:id/value raw :id/type :continuity}

    (map? raw)
    (let [v (or (:value raw) (:id/value raw))
          t (or (:type raw) (:id/type raw))]
      (when (and (string? v) t)
        {:id/value v
         :id/type (if (keyword? t) t (keyword t))}))

    :else nil))

;; =============================================================================
;; Query string parsing — per realtime/request-param-resilience (L1, L3)
;;
;; Three-tier extraction:
;; 1. :query-string from request map (standard HTTP GET/POST)
;; 2. :request-uri parsed directly (handles WS upgrade where :query-string
;;    is nil — L3: http-kit WebSocket upgrade drops query params)
;; 3. Falls through to nil (caller can accept from message payload — L1:
;;    clients that cannot send query params at all)
;; =============================================================================

(defn- parse-query-string
  "Parse a URL query string into a map of string→string.
   Returns empty map on nil or blank input."
  [qs]
  (if (or (nil? qs) (str/blank? qs))
    {}
    (->> (str/split qs #"&")
         (keep (fn [pair]
                 (let [parts (str/split pair #"=" 2)]
                   (when (= 2 (count parts))
                     [(URLDecoder/decode (first parts) "UTF-8")
                      (URLDecoder/decode (second parts) "UTF-8")]))))
         (into {}))))

(defn- extract-query-from-uri
  "Extract query string portion from a request-uri.
   '/path?foo=bar' → 'foo=bar', '/path' → nil.
   Also handles malformed URIs like '?foo=bar' (L1: missing leading slash)."
  [uri]
  (when (and (string? uri) (str/includes? uri "?"))
    (second (str/split uri #"\?" 2))))

(defn extract-params
  "Extract agent-id and session-id from an HTTP/WS request.

   Three-tier extraction per realtime/request-param-resilience (L1, L3):
   1. :query-string from request map (standard HTTP)
   2. :request-uri parsed directly (handles WS upgrade where :query-string
      is nil — http-kit populates request-uri but not query-string for WS)
   3. Falls through to nil values (caller can accept from message payload)

   Returns {:agent-id <string-or-nil> :session-id <string-or-nil>}."
  [request]
  (let [qs-params (parse-query-string (:query-string request))
        uri-params (when (empty? qs-params)
                     (parse-query-string (extract-query-from-uri (:request-uri request))))
        params (if (seq qs-params) qs-params uri-params)]
    {:agent-id (or (get params "agent_id") (get params "agent-id"))
     :session-id (or (get params "session_id") (get params "session-id"))}))

;; =============================================================================
;; JSON → pipeline input (parsing)
;; =============================================================================

(defn parse-dispatch-request
  "Parse a JSON dispatch request into a pre-classified message map.

   The returned map is suitable as input to mode/classify, which adds :msg/mode.
   Includes :msg/to (needed by dispatch but not part of ClassifiedMessage shape).

   JSON format:
     {\"msg_id\": \"...\", \"payload\": ...,
      \"from\": {\"value\": \"...\", \"type\": \"...\"},
      \"to\": {\"value\": \"...\", \"type\": \"...\"}}

   Both 'from' and 'to' accept string shorthand (assumes :continuity type)."
  [json-str]
  (let [parsed (parse-json json-str)]
    (if (error? parsed)
      parsed
      (let [msg-id (or (:msg_id parsed) (:msg-id parsed))
            payload (:payload parsed)
            from-raw (:from parsed)
            to-raw (:to parsed)
            from-id (coerce-agent-id from-raw)
            to-id (coerce-agent-id to-raw)]
        (cond
          (or (nil? msg-id) (not (string? msg-id)))
          (transport-error :invalid-message "Missing or invalid msg_id" :field "msg_id")

          (nil? from-id)
          (transport-error :invalid-message "Missing or invalid 'from' agent id" :field "from")

          (nil? to-id)
          (transport-error :invalid-message "Missing or invalid 'to' agent id" :field "to")

          :else
          {:msg/id msg-id
           :msg/payload payload
           :msg/from from-id
           :msg/to to-id
           :msg/at (now-str)})))))

(defn parse-presence-request
  "Parse a JSON presence request into an AgentConnection.

   JSON format:
     {\"agent_id\": {\"value\": \"...\", \"type\": \"...\"},
      \"transport\": \"websocket\",
      \"metadata\": {\"ready\": true}}

   agent_id accepts string shorthand (assumes :continuity type).
   transport defaults to \"http\" if omitted."
  [json-str]
  (let [parsed (parse-json json-str)]
    (if (error? parsed)
      parsed
      (let [agent-raw (or (:agent_id parsed) (:agent-id parsed))
            agent-id (coerce-agent-id agent-raw)
            transport-str (or (:transport parsed) "http")
            transport-kw (keyword transport-str)
            metadata (:metadata parsed)]
        (cond
          (nil? agent-id)
          (transport-error :invalid-message
                           "Missing or invalid agent_id" :field "agent_id")

          (not (#{:websocket :irc :http :peripheral} transport-kw))
          (transport-error :invalid-message
                           (str "Invalid transport: " transport-str
                                ". Expected: websocket, irc, http, peripheral")
                           :field "transport")

          :else
          (cond-> {:conn/id (str "conn-" (UUID/randomUUID))
                   :conn/transport transport-kw
                   :conn/agent-id agent-id
                   :conn/at (now-str)}
            metadata (assoc :conn/metadata metadata)))))))

(defn parse-ws-message
  "Parse a WebSocket JSON frame into a typed message.

   Per realtime/structured-events-only (R9), all WS frames must be typed JSON
   with a 'type' field.

   Expected frame types:
     {\"type\": \"ready\", \"agent_id\": \"...\", \"session_id\": \"...\"}
     {\"type\": \"message\", \"msg_id\": \"...\", \"payload\": ..., \"to\": ...}
     {\"type\": \"peripheral_start\", \"peripheral_id\": \"explore\"}
     {\"type\": \"tool_action\", \"tool\": \"read\", \"args\": [...]}
     {\"type\": \"peripheral_stop\", \"reason\": \"done\"}

   Returns:
     {:ws/type :ready :agent-id \"...\" :session-id \"...\"}
     or {:ws/type :message :msg/id ... :msg/payload ... :msg/to ... :msg/at ...}
     or SocialError for invalid frames."
  [json-str]
  (let [parsed (parse-json json-str)]
    (if (error? parsed)
      parsed
      (let [frame-type (:type parsed)]
        (cond
          (nil? frame-type)
          (transport-error :invalid-frame "Missing 'type' field in WS frame")

          (= "ready" (str frame-type))
          (let [agent-id (or (:agent_id parsed) (:agent-id parsed))
                session-id (or (:session_id parsed) (:session-id parsed))]
            (if (or (nil? agent-id) (not (string? agent-id)))
              (transport-error :invalid-frame "Ready frame missing agent_id")
              {:ws/type :ready
               :agent-id agent-id
               :session-id session-id}))

          (= "message" (str frame-type))
          (let [msg-id (or (:msg_id parsed) (:msg-id parsed))
                payload (:payload parsed)
                from-raw (:from parsed)
                to-raw (:to parsed)
                to-id (coerce-agent-id to-raw)]
            (cond
              (or (nil? msg-id) (not (string? msg-id)))
              (transport-error :invalid-frame "Message frame missing msg_id")

              (nil? to-id)
              (transport-error :invalid-frame "Message frame missing 'to' field")

              :else
              {:ws/type :message
               :msg/id msg-id
               :msg/payload payload
               :msg/from (or (coerce-agent-id from-raw)
                             {:id/value "unknown" :id/type :transport})
               :msg/to to-id
               :msg/at (now-str)}))

          (= "irc_response" (str frame-type))
          (let [channel (:channel parsed)
                text (:text parsed)]
            (cond
              (or (nil? channel) (not (string? channel)))
              (transport-error :invalid-frame "irc_response frame missing 'channel'")

              (or (nil? text) (not (string? text)))
              (transport-error :invalid-frame "irc_response frame missing 'text'")

              :else
              {:ws/type :irc-response
               :irc/channel channel
               :irc/text text}))

          ;; --- Peripheral session frames (Seam 4) ---
          (= "peripheral_start" (str frame-type))
          (let [pid-raw (or (:peripheral_id parsed) (:peripheral-id parsed))]
            {:ws/type :peripheral-start
             :peripheral-id (when (string? pid-raw) (keyword pid-raw))})

          (= "tool_action" (str frame-type))
          (let [tool-raw (:tool parsed)
                args-raw (:args parsed)]
            (cond
              (nil? tool-raw)
              (transport-error :invalid-frame "tool_action frame missing 'tool'")

              :else
              {:ws/type :tool-action
               :tool (if (keyword? tool-raw) tool-raw (keyword (str tool-raw)))
               :args (if (sequential? args-raw) (vec args-raw) [])}))

          (= "peripheral_stop" (str frame-type))
          {:ws/type :peripheral-stop
           :reason (or (:reason parsed) "client-requested")}

          :else
          (transport-error :invalid-frame
                           (str "Unknown WS frame type: " frame-type)
                           :type (str frame-type)))))))

;; =============================================================================
;; Pipeline output → JSON (rendering)
;;
;; Per realtime/structured-events-only (R9): every rendered frame is a typed
;; JSON map. Receipts carry type="receipt", errors carry type="error".
;; =============================================================================

(defn- typed-agent-id->json
  "Convert a TypedAgentId to a JSON-friendly map."
  [agent-id]
  (when (map? agent-id)
    {"value" (:id/value agent-id)
     "type" (name (:id/type agent-id))}))

(defn render-receipt
  "Render a DispatchReceipt as a JSON string."
  [receipt]
  (let [m (cond-> {"msg_id" (:receipt/msg-id receipt)
                    "to" (typed-agent-id->json (:receipt/to receipt))
                    "delivered" (:receipt/delivered? receipt)
                    "at" (str (:receipt/at receipt))}
            (:receipt/route receipt)
            (assoc "route" (:receipt/route receipt))
            (:receipt/session-id receipt)
            (assoc "session_id" (:receipt/session-id receipt))
            (:receipt/peripheral-id receipt)
            (assoc "peripheral_id" (name (:receipt/peripheral-id receipt)))
            (some? (:receipt/fruit receipt))
            (assoc "fruit" (:receipt/fruit receipt)))]
    (json/generate-string m)))

(def ^:private error-code->status
  "Map SocialError :error/code to HTTP status code.
   Per mission spec error mapping table."
  {:invalid-message    400
   :invalid-json       400
   :invalid-frame      400
   :invalid-connection 400
   :invalid-input      400
   :invalid-registry   500
   :agent-not-found    404
   :session-not-found  404
   :not-ready          403
   :invoke-failed      502
   :peripheral-failed  502})

(defn render-error
  "Render a SocialError as a JSON response with HTTP status code.

   Returns {:status <int> :body <json-string>}.
   Maps :error/code to HTTP status per the transport adapter error table."
  [social-error]
  (let [code (:error/code social-error)
        status (get error-code->status code 500)
        body {"error" true
              "code" (name code)
              "message" (:error/message social-error)
              "component" (name (:error/component social-error))
              "at" (str (:error/at social-error))}]
    {:status status
     :body (json/generate-string body)}))

(defn render-ws-frame
  "Render a pipeline result (DispatchReceipt or SocialError) as a WS JSON frame.

   Per realtime/structured-events-only (R9): every frame has a 'type' field.
   Receipts → type='receipt', errors → type='error',
   presence records → type='presence'."
  [pipeline-result]
  (cond
    ;; SocialError
    (contains? pipeline-result :error/code)
    (json/generate-string
     {"type" "error"
      "code" (name (:error/code pipeline-result))
      "message" (:error/message pipeline-result)})

    ;; DispatchReceipt
    (contains? pipeline-result :receipt/msg-id)
    (json/generate-string
     (cond-> {"type" "receipt"
              "msg_id" (:receipt/msg-id pipeline-result)
              "delivered" (:receipt/delivered? pipeline-result)
              "at" (str (:receipt/at pipeline-result))}
       (:receipt/route pipeline-result)
       (assoc "route" (:receipt/route pipeline-result))
       (:receipt/session-id pipeline-result)
       (assoc "session_id" (:receipt/session-id pipeline-result))
       (:receipt/peripheral-id pipeline-result)
       (assoc "peripheral_id" (name (:receipt/peripheral-id pipeline-result)))))

    ;; PresenceRecord
    (contains? pipeline-result :presence/ready?)
    (json/generate-string
     {"type" "presence"
      "agent_id" (typed-agent-id->json (:presence/agent-id pipeline-result))
      "ready" (:presence/ready? pipeline-result)
      "transport" (name (:presence/transport pipeline-result))
      "at" (str (:presence/at pipeline-result))})

    ;; Fallback — unknown result type
    :else
    (json/generate-string {"type" "unknown" "data" (str pipeline-result)})))

(defn render-ready-ack
  "Render a ready-ack WS frame.
   Sent by the server after successful readiness handshake (R7)."
  []
  (json/generate-string {"type" "ready_ack"}))

(defn render-irc-message
  "Render an IRC message as a WS frame for relay to an agent.
   Sent when a human posts a PRIVMSG in an IRC channel the agent is in."
  [channel from text]
  (json/generate-string {"type" "irc_message"
                          "channel" channel
                          "from" from
                          "text" text
                          "transport" "irc"}))

;; =============================================================================
;; Peripheral session frames (Seam 4: WS ↔ peripheral lifecycle)
;; =============================================================================

(defn render-peripheral-started
  "Render a peripheral_started ack frame."
  [peripheral-id session-id]
  (json/generate-string {"type" "peripheral_started"
                          "peripheral_id" (name peripheral-id)
                          "session_id" session-id}))

(defn render-tool-result
  "Render a tool_result frame from a peripheral step."
  [tool ok? result]
  (json/generate-string
   (cond-> {"type" "tool_result"
            "tool" (name tool)
            "ok" ok?}
     (some? result) (assoc "result" result))))

(defn render-peripheral-stopped
  "Render a peripheral_stopped frame with fruit."
  [peripheral-id fruit reason]
  (json/generate-string
   (cond-> {"type" "peripheral_stopped"
            "peripheral_id" (name peripheral-id)
            "reason" reason}
     (some? fruit) (assoc "fruit" fruit))))
