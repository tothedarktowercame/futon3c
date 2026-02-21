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
     CODEX_SANDBOX       workspace-write
     CODEX_APPROVAL      never
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
           [java.util.concurrent CompletableFuture]))

(defn- env [k default] (or (System/getenv k) default))
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
(def codex-sandbox (env "CODEX_SANDBOX" "workspace-write"))
(def codex-approval (env "CODEX_APPROVAL" "never"))
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
     :text text}))

(defn- build-codex-cmd [sid]
  (let [exec-opts ["--json"
                   "--skip-git-repo-check"
                   "--sandbox" codex-sandbox
                   "-c" (format "approval_policy=\"%s\"" codex-approval)]
        exec-opts (if (str/blank? codex-model)
                    exec-opts
                    (concat exec-opts ["--model" codex-model]))]
    (if (str/blank? sid)
      (into [codex-bin "exec"] (concat exec-opts ["-"]))
      (into [codex-bin "exec"] (concat exec-opts ["resume" sid "-"])))))

(defn- invoke-codex!
  [prompt prior-session-id sid*]
  (if (= "mock" invoke-mode)
    {:ok true
     :result (str "mock: " prompt)
     :session-id (or prior-session-id @sid*)}
    (let [sid (or prior-session-id @sid*)
          cmd (build-codex-cmd sid)
          result (apply sh/sh (concat cmd [:in (str prompt "\n") :dir codex-cwd]))
          parsed (parse-codex-output (str (:out result) (:err result)) sid)
          new-sid (:session-id parsed)]
      (when (and (string? new-sid) (not (str/blank? new-sid)))
        (reset! sid* new-sid)
        (persist-session-id! session-file new-sid))
      (if (zero? (:exit result))
        {:ok true
         :result (some-> (:text parsed) str/trim not-empty)
         :session-id new-sid}
        {:ok false
         :error (str "codex-exit-" (:exit result) ": " (str/trim (:text parsed)))
         :session-id sid}))))

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

(defn- handle-invoke-frame!
  [^WebSocket ws sid* frame]
  (let [invoke-id (:invoke_id frame)
        prompt (:prompt frame)
        incoming-session (:session_id frame)]
    (when (string? invoke-id)
      (future
        (let [outcome (invoke-codex! (str prompt) incoming-session sid*)
              payload (cond-> {"type" "invoke_result"
                               "invoke_id" invoke-id}
                        (:session-id outcome) (assoc "session_id" (:session-id outcome))
                        (:ok outcome) (assoc "result" (or (:result outcome) ""))
                        (not (:ok outcome)) (assoc "error" (:error outcome)))]
          (try
            (send-json! ws payload)
            (catch Exception e
              (println "[bridge] send invoke_result failed:" (.getMessage e))
              (flush))))))))

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
