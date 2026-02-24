;; clj-kondo: ignore-file
(ns scripts.claude-ws-invoke-bridge
  "Claude WS invoke bridge.

   Connects to Agency WS, performs ready handshake, receives invoke frames,
   runs Claude CLI, and replies with invoke_result frames.

   Mirrors codex_ws_invoke_bridge.clj but invokes `claude -p --resume`
   instead of `codex exec`.

   Usage:
     clojure -M scripts/claude_ws_invoke_bridge.clj

   Env:
     AGENCY_WS_BASE      ws://127.0.0.1:7070
     AGENCY_WS_PATH      /agency/ws
     AGENT_ID            claude-1
     CLAUDE_BIN          claude
     CLAUDE_CWD          <pwd>
     CLAUDE_PERMISSION   bypassPermissions
     CLAUDE_SESSION_FILE /tmp/futon-session-id
     CLAUDE_SESSION_ID   optional startup session id
     INVOKE_MODE         claude|mock (default claude)
     HEARTBEAT_INTERVAL_MS  30000"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import [java.io BufferedReader File InputStreamReader]
           [java.lang ProcessBuilder$Redirect]
           [java.util.concurrent TimeUnit]
           [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent CompletableFuture]))

;; =============================================================================
;; Config
;; =============================================================================

(defn- env [k default] (or (System/getenv k) default))
(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))
(defn- now-str [] (str (Instant/now)))

(def agency-ws-base (env "AGENCY_WS_BASE" "ws://127.0.0.1:7070"))
(def agency-ws-path (env "AGENCY_WS_PATH" "/agency/ws"))
(def agency-http-base (env "AGENCY_HTTP_BASE" "http://127.0.0.1:7070"))
(def agent-id (env "AGENT_ID" "claude-1"))
(def agent-type "claude")

(def claude-bin (env "CLAUDE_BIN" "claude"))
(def claude-cwd (env "CLAUDE_CWD" (System/getProperty "user.dir")))
(def claude-permission (env "CLAUDE_PERMISSION" "bypassPermissions"))
(def session-file (env "CLAUDE_SESSION_FILE" "/tmp/futon-session-id"))
(def startup-session-id (System/getenv "CLAUDE_SESSION_ID"))
(def invoke-mode (str/lower-case (env "INVOKE_MODE" "claude")))
(def auto-register? (not (contains? #{"0" "false" "no" "off"}
                                     (str/lower-case (env "AUTO_REGISTER" "true")))))

;; =============================================================================
;; Session helpers
;; =============================================================================

(defn- load-session-id [path]
  (let [f (File. path)]
    (when (.exists f)
      (let [sid (str/trim (slurp f))]
        (when-not (str/blank? sid)
          sid)))))

(defn- persist-session-id! [path sid]
  (when-not (str/blank? sid)
    (spit path sid)))

;; =============================================================================
;; Claude CLI invocation
;; =============================================================================

(defn- extract-text-from-assistant-message
  "Extract text content from a stream-json assistant message.
   Shape: {\"type\":\"assistant\", \"message\":{\"content\":[{\"type\":\"text\",\"text\":\"...\"}]}}"
  [parsed]
  (when-let [content (get-in parsed [:message :content])]
    (when (sequential? content)
      (->> content
           (keep (fn [block] (when (= "text" (:type block)) (:text block))))
           (str/join "\n")))))

(defn- invoke-claude!
  "Invoke claude -p with stream-json output. Reads stdout line by line,
   calls on-text for each text chunk as it arrives. Returns final
   {:ok bool :result str :session-id str :error str}."
  [prompt prior-session-id sid* & {:keys [on-text]}]
  (if (= "mock" invoke-mode)
    (let [text (str "mock: " prompt)]
      (when on-text (on-text text))
      {:ok true
       :result text
       :session-id (or prior-session-id @sid*)})
    (let [sid (or prior-session-id @sid*)
          new-sid (when-not sid (str (UUID/randomUUID)))
          args (cond-> [claude-bin "-p" (str prompt)
                        "--permission-mode" claude-permission
                        "--output-format" "stream-json" "--verbose"]
                 sid     (into ["--resume" sid])
                 new-sid (into ["--session-id" new-sid]))
          used-sid (or sid new-sid)
          timeout-ms 120000
          pb (doto (ProcessBuilder. ^java.util.List (vec args))
               (.directory (File. claude-cwd))
               (.redirectErrorStream false)
               (.redirectInput (ProcessBuilder$Redirect/from (File. "/dev/null"))))
          proc (.start pb)
          ;; Accumulate text and session-id from the stream
          text-acc (StringBuilder.)
          result-sid (atom nil)
          result-ok (atom true)
          ;; Read stream-json lines as they arrive
          stdout-fut (future
                       (with-open [r (BufferedReader. (InputStreamReader. (.getInputStream proc)))]
                         (loop []
                           (when-let [line (.readLine r)]
                             (when-not (str/blank? line)
                               (try
                                 (let [parsed (json/parse-string line true)]
                                   (case (:type parsed)
                                     ;; Text from Claude — extract and callback immediately
                                     "assistant"
                                     (when-let [text (extract-text-from-assistant-message parsed)]
                                       (when-not (str/blank? text)
                                         (.append text-acc text)
                                         (when on-text (on-text text))))

                                     ;; Final result — grab session-id
                                     "result"
                                     (do
                                       (reset! result-sid (:session_id parsed))
                                       (when (:is_error parsed)
                                         (reset! result-ok false)))

                                     ;; system, rate_limit_event, etc — ignore
                                     nil))
                                 (catch Exception e
                                   (println "[bridge] stream parse error:" (.getMessage e))
                                   (flush))))
                             (recur)))))
          stderr-fut (future
                       (with-open [r (BufferedReader. (InputStreamReader. (.getErrorStream proc)))]
                         (let [sb (StringBuilder.)]
                           (loop []
                             (let [line (.readLine r)]
                               (when line
                                 (.append sb line)
                                 (.append sb "\n")
                                 (recur))))
                           (str sb))))]
      (let [finished? (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)]
        (when-not finished?
          (println "[bridge] TIMEOUT after 120s — killing process")
          (flush)
          (.destroyForcibly proc)
          (.waitFor proc 5000 TimeUnit/MILLISECONDS)))
      @stdout-fut
      (let [exit (.exitValue proc)
            _err @stderr-fut
            text (str text-acc)
            final-sid (or @result-sid used-sid)]
        (when (and (string? final-sid) (not (str/blank? final-sid)))
          (reset! sid* final-sid)
          (persist-session-id! session-file final-sid))
        (if (and (zero? exit) @result-ok)
          {:ok true
           :result (if (str/blank? text)
                     "[Claude used tools but produced no text response]"
                     text)
           :session-id final-sid}
          {:ok false
           :error (str "claude-exit-" exit ": " (str/trim (or text "")))
           :session-id used-sid})))))

;; =============================================================================
;; HTTP registration + evidence emission
;; =============================================================================

(defn- ws-url [sid]
  (str (str/replace agency-ws-base #"/$" "")
       agency-ws-path
       "?agent-id=" agent-id
       (when (and (string? sid) (not (str/blank? sid)))
         (str "&session-id=" sid))))

(defn- ensure-registered! []
  (when auto-register?
    (let [url (str (str/replace agency-http-base #"/$" "") "/api/alpha/agents")
          body (json/generate-string {"agent-id" agent-id
                                      "type" agent-type
                                      "ws-bridge" true})
          resp @(http/post url {:headers {"Content-Type" "application/json"}
                                :body body
                                :timeout 10000})
          status (:status resp)
          err (:error resp)]
      (cond
        err
        (do (println "[bridge] registration request error:" err) false)

        (= 201 status)
        (do (println "[bridge] registered agent via HTTP:" agent-id) true)

        (= 409 status)
        (do (println "[bridge] agent already registered:" agent-id) true)

        :else
        (do
          (println "[bridge] registration failed:" status (:body resp))
          false)))))

(def ^:private evidence-url
  (str (str/replace agency-http-base #"/$" "") "/api/alpha/evidence"))

(def ^:private heartbeat-interval-ms
  (parse-int (env "HEARTBEAT_INTERVAL_MS" "30000") 30000))

(defn- emit-evidence!
  "POST an evidence entry to the Agency evidence store. Fire-and-forget."
  [event-type body-map & {:keys [session-id tags]}]
  (future
    (try
      (let [payload (json/generate-string
                     {"subject" {"ref/type" "agent" "ref/id" agent-id}
                      "type" "coordination"
                      "claim-type" "step"
                      "author" agent-id
                      "session-id" (or session-id "ws-bridge")
                      "body" (assoc body-map "event" event-type
                                             "agent-id" agent-id
                                             "at" (now-str))
                      "tags" (into ["invoke" "ws-bridge" agent-id]
                                   (or tags []))})
            resp @(http/post evidence-url
                             {:headers {"Content-Type" "application/json"}
                              :body payload
                              :timeout 5000})]
        (when (:error resp)
          (println "[bridge] evidence emit error:" (:error resp))
          (flush)))
      (catch Exception e
        (println "[bridge] evidence emit failed:" (.getMessage e))
        (flush)))))

(defn- start-heartbeat!
  "Start a background thread that emits heartbeat evidence every N seconds.
   Returns a stop function."
  [invoke-id prompt-preview session-id]
  (let [running (atom true)
        start-ms (System/currentTimeMillis)
        thread (Thread.
                (fn []
                  (while @running
                    (try
                      (Thread/sleep heartbeat-interval-ms)
                      (when @running
                        (let [elapsed-s (quot (- (System/currentTimeMillis) start-ms) 1000)]
                          (emit-evidence! "invoke-heartbeat"
                                          {"invoke-id" invoke-id
                                           "elapsed-seconds" elapsed-s
                                           "prompt-preview" prompt-preview}
                                          :session-id session-id
                                          :tags ["heartbeat"])))
                      (catch InterruptedException _
                        (reset! running false))
                      (catch Exception _))))
                (str "heartbeat-" invoke-id))]
    (.setDaemon thread true)
    (.start thread)
    (fn [] (reset! running false) (.interrupt thread))))

;; =============================================================================
;; WS invoke handler
;; =============================================================================

(defn- send-json! [^WebSocket ws payload]
  (.join (.sendText ws (json/generate-string payload) true)))

(defn- handle-invoke-frame!
  [^WebSocket ws sid* frame]
  (let [invoke-id (:invoke_id frame)
        prompt (:prompt frame)
        incoming-session (:session_id frame)]
    (when (string? invoke-id)
      (future
        (let [prompt-str (str prompt)
              prompt-preview (subs prompt-str 0 (min 200 (count prompt-str)))]
          ;; Evidence: invoke started
          (emit-evidence! "invoke-start"
                          {"invoke-id" invoke-id
                           "prompt-preview" prompt-preview}
                          :session-id incoming-session
                          :tags ["invoke-start"])
          ;; Start heartbeat
          (let [stop-heartbeat! (start-heartbeat! invoke-id prompt-preview incoming-session)
                outcome (try
                          (invoke-claude! prompt-str incoming-session sid*
                                          :on-text (fn [text]
                                                     (try
                                                       (send-json! ws {"type" "invoke_text"
                                                                       "invoke_id" invoke-id
                                                                       "text" text})
                                                       (catch Exception e
                                                         (println "[bridge] send invoke_text failed:" (.getMessage e))
                                                         (flush)))))
                          (finally
                            (stop-heartbeat!)))
                payload (cond-> {"type" "invoke_result"
                                 "invoke_id" invoke-id}
                          (:session-id outcome) (assoc "session_id" (:session-id outcome))
                          (:ok outcome) (assoc "result" (or (:result outcome) ""))
                          (not (:ok outcome)) (assoc "error" (:error outcome)))]
            ;; Evidence: invoke complete
            (emit-evidence! "invoke-complete"
                            {"invoke-id" invoke-id
                             "ok" (:ok outcome)
                             "result-preview" (when (:ok outcome)
                                                (let [r (str (:result outcome))]
                                                  (subs r 0 (min 300 (count r)))))
                             "error" (when-not (:ok outcome) (:error outcome))}
                            :session-id (or (:session-id outcome) incoming-session)
                            :tags ["invoke-complete"])
            (try
              (send-json! ws payload)
              (catch Exception e
                (println "[bridge] send invoke_result failed:" (.getMessage e))
                (flush)))))))))

;; =============================================================================
;; WS connection loop
;; =============================================================================

(defn- connect-loop! [sid*]
  (let [client (HttpClient/newHttpClient)]
    (loop []
      (let [closed (promise)
            url (ws-url @sid*)]
        (try
          (println "[bridge] connecting:" url)
          (flush)
          (let [listener (reify WebSocket$Listener
                           (onOpen [_ ws]
                             (println "[bridge] ws open, sending ready")
                             (flush)
                             (send-json! ws {"type" "ready"
                                             "agent_id" agent-id
                                             "session_id" (or @sid* (str "sess-" (System/currentTimeMillis)))})
                             (.request ws 1))
                           (onText [_ ws data _last]
                             (try
                               (let [frame (json/parse-string (str data) true)]
                                 (when (= "invoke" (:type frame))
                                   (handle-invoke-frame! ws sid* frame)))
                               (catch Exception e
                                 (println "[bridge] bad frame:" (.getMessage e))
                                 (flush)))
                             (.request ws 1)
                             (CompletableFuture/completedFuture nil))
                           (onClose [_ _ws code reason]
                             (println "[bridge] ws closed:" code reason)
                             (flush)
                             (deliver closed true)
                             (CompletableFuture/completedFuture nil))
                           (onError [_ _ws e]
                             (println "[bridge] ws error:" (.getMessage e))
                             (flush)
                             (deliver closed true)))
                ws (.join (.buildAsync (.newWebSocketBuilder client)
                                       (URI/create url)
                                       listener))]
            (deref closed 600000 nil)
            (try
              (.join (.sendClose ws WebSocket/NORMAL_CLOSURE "reconnect"))
              (catch Exception _)))
          (catch Exception e
            (println "[bridge] connect error:" (.getMessage e))
            (flush))))
      (Thread/sleep 5000)
      (recur))))

;; =============================================================================
;; Main
;; =============================================================================

(let [initial-sid (or startup-session-id (load-session-id session-file))
      sid* (atom initial-sid)]
  (when (and (string? startup-session-id) (not (str/blank? startup-session-id)))
    (persist-session-id! session-file startup-session-id))
  (println "=== claude ws invoke bridge ===")
  (println "  at:" (now-str))
  (println "  agency:" (str (str/replace agency-ws-base #"/$" "") agency-ws-path))
  (println "  agency-http-base:" agency-http-base)
  (println "  agent-id:" agent-id)
  (println "  agent-type:" agent-type)
  (println "  auto-register:" auto-register?)
  (println "  invoke-mode:" invoke-mode)
  (println "  claude-bin:" claude-bin)
  (println "  cwd:" claude-cwd)
  (println "  permission:" claude-permission)
  (println "  session-file:" session-file)
  (println "  session:" (or @sid* "(new session on first invoke)"))
  (println "  heartbeat-interval-ms:" heartbeat-interval-ms)
  (println)
  (flush)
  (ensure-registered!)
  (connect-loop! sid*))
