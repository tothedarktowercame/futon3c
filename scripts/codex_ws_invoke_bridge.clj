;; clj-kondo: ignore-file
(ns scripts.codex-ws-invoke-bridge
  "Codex WS invoke bridge.

   Connects to Agency WS, performs ready handshake, receives invoke frames,
   runs Codex CLI, and replies with invoke_result frames.

   Usage:
     clojure -M scripts/codex_ws_invoke_bridge.clj

   Env:
     AGENCY_WS_BASE      ws://127.0.0.1:7070
     AGENCY_WS_PATH      /agency/ws
     AGENT_ID            codex-1
     CODEX_BIN           codex
     CODEX_CWD           <pwd>
     CODEX_MODEL         gpt-5-codex
     CODEX_SANDBOX       danger-full-access
     CODEX_APPROVAL      never
     CODEX_REASONING_EFFORT low|medium|high (optional)
     CODEX_SESSION_FILE  /tmp/futon-codex-session-id
     CODEX_SESSION_ID    optional startup session id
     INVOKE_MODE         codex|mock (default codex)"
  (:require [cheshire.core :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import [java.io File]
           [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent CompletableFuture]))

(defn- env [k default] (or (System/getenv k) default))
(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))
(defn- now-str [] (str (Instant/now)))

(def agency-ws-base (env "AGENCY_WS_BASE" "ws://127.0.0.1:7070"))
(def agency-ws-path (env "AGENCY_WS_PATH" "/agency/ws"))
(def agency-http-base (env "AGENCY_HTTP_BASE" "http://127.0.0.1:7070"))
(def agent-id (env "AGENT_ID" "codex-1"))
;; codex|claude|tickle|mock (used for optional HTTP registration)
(def agent-type (str/lower-case (env "AGENT_TYPE" "codex")))

(def codex-bin (env "CODEX_BIN" "codex"))
(def codex-cwd (env "CODEX_CWD" (System/getProperty "user.dir")))
(def codex-model (env "CODEX_MODEL" "gpt-5-codex"))
(def codex-sandbox (env "CODEX_SANDBOX" "danger-full-access"))
(def codex-approval (env "CODEX_APPROVAL" "never"))
(def codex-reasoning-effort (System/getenv "CODEX_REASONING_EFFORT"))
(def session-file (env "CODEX_SESSION_FILE" "/tmp/futon-codex-session-id"))
(def startup-session-id (System/getenv "CODEX_SESSION_ID"))
(def invoke-mode (str/lower-case (env "INVOKE_MODE" "codex")))
(def auto-register? (not (contains? #{"0" "false" "no" "off"}
                                     (str/lower-case (env "AUTO_REGISTER" "true")))))

(defn- load-session-id [path]
  (let [f (File. path)]
    (when (.exists f)
      (let [sid (str/trim (slurp f))]
        (when-not (str/blank? sid)
          sid)))))

(defn- persist-session-id! [path sid]
  (when-not (str/blank? sid)
    (spit path sid)))

(defn- extract-agent-text [item]
  (let [content (or (:text item) (:content item))]
    (cond
      (string? content) content
      (sequential? content)
      (->> content
           (keep (fn [part]
                   (when (and (map? part) (= "text" (:type part)))
                     (:text part))))
           (remove str/blank?)
           (str/join ""))
      :else nil)))

;; =============================================================================
;; Execution evidence — count tool/command events from NDJSON (H-2)
;; =============================================================================

(defn- tool-event?
  [evt]
  (let [t (:type evt)
        item (:item evt)]
    (or (= "command_execution" t)
        (and (contains? #{"item.started" "item.completed"} t)
             (= "command_execution" (:type item)))
        (and (contains? #{"item.started" "item.completed"} t)
             (= "tool_call" (:type item))))))

(defn- command-event?
  [evt]
  (let [t (:type evt)
        item (:item evt)
        name (:name item)]
    (or (= "command_execution" t)
        (and (contains? #{"item.started" "item.completed"} t)
             (= "command_execution" (:type item)))
        (and (contains? #{"item.started" "item.completed"} t)
             (= "tool_call" (:type item))
             (contains? #{"command_execution" "command-execution" "bash" "shell"}
                        (some-> name str/lower-case))))))

(defn- parse-codex-output [raw-output prior-session-id]
  (let [events (keep (fn [line]
                       (try (json/parse-string line true)
                            (catch Exception _ nil)))
                     (str/split-lines (or raw-output "")))
        session-id (or (some (fn [evt]
                               (when (= "thread.started" (:type evt))
                                 (or (:thread_id evt) (:session_id evt))))
                             events)
                       prior-session-id)
        tool-events (count (filter tool-event? events))
        command-events (count (filter command-event? events))
        text (or (some->> events
                          (filter #(= "item.completed" (:type %)))
                          (map :item)
                          (filter #(= "agent_message" (:type %)))
                          (map extract-agent-text)
                          (remove str/blank?)
                          last)
                 (some->> events
                          (filter #(= "error" (:type %)))
                          (map :message)
                          (remove str/blank?)
                          last)
                 (some-> raw-output str/trim not-empty)
                 "[No assistant message returned]")]
    {:session-id session-id
     :text text
     :execution {:tool-events tool-events
                 :command-events command-events
                 :executed? (pos? (+ tool-events command-events))}}))

;; =============================================================================
;; Enforcement detection predicates (H-1) — ported from dev.clj
;; =============================================================================

(def ^:private work-claim-re
  #"(?i)\b(i['']?ll|i will|we['']?ll|we will|claiming|i claim|taking|i(?:'m| am) taking|proceeding|starting|kicking off|working on|i(?:'m| am) on it)\b")

(def ^:private planning-only-re
  #"(?i)\b(planning-only|not started|need clarification|need more context|cannot execute yet|blocked)\b")

(def ^:private task-mode-re
  #"(?i)\bmode:\s*task\b")

(def ^:private mission-work-re
  #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b")

(def ^:private separate-message-request-re
  #"(?i)\b(separate\s+irc\s+messages?|one\s+per\s+message|each\s+of\s+them\s+in\s+a\s+separate\s+irc\s+message|post\s+each\s+.*\s+separate\s+message)\b")

(def ^:private format-excuse-re
  #"(?i)\b(surface|interface|mode|channel|irc)\b.{0,60}\b(cap|caps|limit|limited|can't|cannot|unable)\b|\b\d+\s+lines?\s+per\s+turn\b")

(defn- no-execution-evidence?
  [result]
  (let [execution (:execution result)
        tool-events (long (or (:tool-events execution) 0))
        command-events (long (or (:command-events execution) 0))]
    (and (nil? (:error result))
         (not (:executed? execution))
         (zero? tool-events)
         (zero? command-events))))

(defn- work-claim-without-execution?
  [result]
  (let [text (str/trim (or (:result result) ""))]
    (and (no-execution-evidence? result)
         (not (str/blank? text))
         (boolean (re-find work-claim-re text)))))

(defn- task-reply-without-execution?
  [prompt result]
  (let [text (str/trim (or (:result result) ""))
        prompt-str (str (or prompt ""))]
    (and (or (boolean (re-find task-mode-re prompt-str))
             (boolean (re-find mission-work-re prompt-str)))
         (no-execution-evidence? result)
         (not (str/blank? text))
         (not (boolean (re-find planning-only-re text))))))

(defn- format-refusal?
  [prompt result]
  (let [text (str/trim (or (:result result) ""))
        prompt-str (str (or prompt ""))]
    (and (boolean (re-find separate-message-request-re prompt-str))
         (not (str/blank? text))
         (boolean (re-find format-excuse-re text)))))

(defn- enforcement-needed?
  [prompt result]
  (or (work-claim-without-execution? result)
      (task-reply-without-execution? prompt result)
      (format-refusal? prompt result)))

(defn- enforcement-reason
  "Returns a keyword describing why enforcement triggered, for evidence (H-2)."
  [prompt result]
  (cond
    (work-claim-without-execution? result)        :work-claim-without-execution
    (task-reply-without-execution? prompt result)  :task-reply-without-execution
    (format-refusal? prompt result)                :format-refusal
    :else                                          nil))

;; =============================================================================
;; Enforcement prompt builders
;; =============================================================================

(defn- clip [s max-len]
  (let [txt (str (or s ""))]
    (if (<= (count txt) max-len)
      txt
      (str (subs txt 0 (max 0 (- max-len 3))) "..."))))

(defn- execution-followup-prompt
  [original-prompt prior-reply]
  (str "Your previous reply made a work/progress claim without execution evidence.\n"
       "Execute one concrete first step now (tool/command activity is required).\n"
       "Then reply in one short line with actual status and artifact refs.\n\n"
       "Original request:\n"
       (clip original-prompt 900)
       "\n\nPrevious reply:\n"
       (clip prior-reply 600)))

(defn- format-followup-prompt
  [original-prompt prior-reply]
  (str "Your previous reply refused a feasible output format due invented surface limits.\n"
       "Do not claim per-turn line caps or transport limits unless an actual tool call failed.\n"
       "Complete the user request directly.\n"
       "If they asked for separate IRC messages, output newline-separated one-line items,\n"
       "one intended post per line.\n\n"
       "Original request:\n"
       (clip original-prompt 900)
       "\n\nPrevious reply:\n"
       (clip prior-reply 600)))

(defn- build-codex-cmd [sid]
  (let [exec-opts ["--json"
                   "--skip-git-repo-check"
                   "--sandbox" codex-sandbox
                   "-c" (format "approval_policy=\"%s\"" codex-approval)]
        exec-opts (if (str/blank? codex-model)
                    exec-opts
                    (concat exec-opts ["--model" codex-model]))
        exec-opts (if (str/blank? codex-reasoning-effort)
                    exec-opts
                    (concat exec-opts ["-c" (format "model_reasoning_effort=\"%s\"" codex-reasoning-effort)]))]
    (let [base (if (str/blank? sid)
                 (into [codex-bin "exec"] (concat exec-opts ["-"]))
                 (into [codex-bin "exec"] (concat exec-opts ["resume" sid "-"])))]
      (if (-> (System/getProperty "os.name" "")
              str/lower-case
              (str/includes? "windows"))
        (into ["cmd.exe" "/c"] base)
        base))))

(defn- invoke-codex!
  [prompt prior-session-id sid*]
  (if (= "mock" invoke-mode)
    {:ok true
     :result (str "mock: " prompt)
     :session-id (or prior-session-id @sid*)
     :execution {:tool-events 0 :command-events 0 :executed? false}}
    (let [sid (or prior-session-id @sid*)
          cmd (build-codex-cmd sid)
          result (apply sh/sh (concat cmd [:in (str prompt "\n") :dir codex-cwd]))
          parsed (parse-codex-output (str (:out result) (:err result)) sid)
          new-sid (:session-id parsed)
          execution (:execution parsed)]
      (when (and (string? new-sid) (not (str/blank? new-sid)))
        (reset! sid* new-sid)
        (persist-session-id! session-file new-sid))
      (if (zero? (:exit result))
        {:ok true
         :result (some-> (:text parsed) str/trim not-empty)
         :session-id new-sid
         :execution execution}
        {:ok false
         :error (str "codex-exit-" (:exit result) ": " (str/trim (:text parsed)))
         :session-id sid
         :execution execution}))))

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

(defn- send-json! [^WebSocket ws payload]
  (.join (.sendText ws (json/generate-string payload) true)))

;; =============================================================================
;; Evidence emission — POST to /api/alpha/evidence during long invokes
;; =============================================================================

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
        (when (and (:error resp))
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
                initial (try
                          (invoke-codex! prompt-str incoming-session sid*)
                          (finally
                            (stop-heartbeat!)))
                ;; ---- H-1: Enforcement retry ----
                outcome (if (and (:ok initial) (enforcement-needed? prompt-str initial))
                          (let [reason (enforcement-reason prompt-str initial)
                                exec-enforce? (not= :format-refusal reason)
                                retry-prompt (if exec-enforce?
                                               (execution-followup-prompt prompt-str (:result initial))
                                               (format-followup-prompt prompt-str (:result initial)))
                                retry-sid (or (:session-id initial) incoming-session)]
                            (println (str "[bridge] " agent-id
                                          " enforcement triggered: " (name reason)
                                          " — retrying"))
                            (flush)
                            ;; Evidence: enforcement retry
                            (emit-evidence! "enforcement-retry"
                                            {"invoke-id" invoke-id
                                             "reason" (name reason)
                                             "initial-result-preview" (clip (:result initial) 300)}
                                            :session-id (or retry-sid incoming-session)
                                            :tags ["enforcement" "retry"])
                            (let [retry (invoke-codex! retry-prompt retry-sid sid*)]
                              (if (enforcement-needed? prompt-str retry)
                                ;; Retry also failed enforcement — return structured error
                                (do
                                  (println (str "[bridge] " agent-id
                                                " enforcement retry also failed — returning error"))
                                  (flush)
                                  {:ok false
                                   :result nil
                                   :session-id (or (:session-id retry) retry-sid)
                                   :execution (assoc (or (:execution retry) {})
                                                     :enforced-retry? true
                                                     :executed? false)
                                   :error (str (name reason) " after enforcement retry")})
                                ;; Retry succeeded
                                (update retry :execution
                                        #(assoc (or % {}) :enforced-retry? true)))))
                          initial)
                ;; ---- Build WS response payload ----
                execution (:execution outcome)
                payload (cond-> {"type" "invoke_result"
                                 "invoke_id" invoke-id}
                          (:session-id outcome) (assoc "session_id" (:session-id outcome))
                          (:ok outcome) (assoc "result" (or (:result outcome) ""))
                          (not (:ok outcome)) (assoc "error" (:error outcome)))]
            ;; ---- H-2: Structured evidence with execution fields ----
            (emit-evidence! "invoke-complete"
                            {"invoke-id" invoke-id
                             "ok" (boolean (:ok outcome))
                             "executed" (boolean (:executed? execution))
                             "tool-events" (long (or (:tool-events execution) 0))
                             "command-events" (long (or (:command-events execution) 0))
                             "enforced-retry" (boolean (:enforced-retry? execution))
                             "result-preview" (when (:ok outcome)
                                                (clip (:result outcome) 300))
                             "error" (when-not (:ok outcome) (:error outcome))}
                            :session-id (or (:session-id outcome) incoming-session)
                            :tags (cond-> ["invoke-complete"]
                                    (:enforced-retry? execution) (conj "enforcement")))
            (try
              (send-json! ws payload)
              (catch Exception e
                (println "[bridge] send invoke_result failed:" (.getMessage e))
                (flush)))))))))

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

(let [initial-sid (or startup-session-id (load-session-id session-file))
      sid* (atom initial-sid)]
  (when (and (string? startup-session-id) (not (str/blank? startup-session-id)))
    (persist-session-id! session-file startup-session-id))
  (println "=== codex ws invoke bridge ===")
  (println "  at:" (now-str))
  (println "  agency:" (str (str/replace agency-ws-base #"/$" "") agency-ws-path))
  (println "  agency-http-base:" agency-http-base)
  (println "  agent-id:" agent-id)
  (println "  agent-type:" agent-type)
  (println "  auto-register:" auto-register?)
  (println "  invoke-mode:" invoke-mode)
  (println "  codex-bin:" codex-bin)
  (println "  cwd:" codex-cwd)
  (println "  model:" codex-model)
  (println "  sandbox:" codex-sandbox)
  (println "  approval:" codex-approval)
  (println "  session-file:" session-file)
  (println "  session:" (or @sid* "(new session on first invoke)"))
  (println)
  (flush)
  (ensure-registered!)
  (connect-loop! sid*))
