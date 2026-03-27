(ns futon3c.agents.codex-cli
  "Codex CLI invoke adapter for runtime agent registration.

   Produces invoke-fn values compatible with the registry contract:
   (fn [prompt session-id] -> {:result string|nil :session-id string|nil :error string?})."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- coerce-prompt
  [prompt]
  (cond
    (string? prompt) prompt
    (map? prompt) (or (:prompt prompt)
                      (:text prompt)
                      (json/generate-string prompt))
    (nil? prompt) ""
    :else (str prompt)))

(defn- extract-agent-text
  [item]
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

(defn- parse-json-line
  [line]
  (try
    (json/parse-string line true)
    (catch Exception _
      nil)))

(def ^:private activity-placeholder-texts
  #{"using bash"
    "using bash (done)"
    "reading files"
    "reading files (done)"
    "editing files"
    "editing files (done)"
    "searching code"
    "searching code (done)"
    "inspecting files"
    "inspecting files (done)"
    "preparing response"})

(def ^:private no-assistant-message-sentinel
  "[No assistant message returned]")

(defn- meaningful-agent-text
  [text]
  (when-let [t (some-> text str str/trim not-empty)]
    (when-not (or (= no-assistant-message-sentinel t)
                  (contains? activity-placeholder-texts (str/lower-case t)))
      t)))


(defn- titleize-token
  [token]
  (if (and (string? token) (not (str/blank? token)))
    (->> (str/replace token #"[-_]+" " ")
         (#(str/split % #"\s+"))
         (remove str/blank?)
         (map str/capitalize)
         (str/join " "))
    "Unknown"))

(defn- humanize-tool-name
  [name]
  (let [tool (some-> name str/lower-case)]
    (cond
      (contains? #{"command_execution" "command-execution" "bash" "shell"} tool) "using bash"
      (contains? #{"read_file" "read-files"} tool) "reading files"
      (contains? #{"write_file" "edit_file" "apply_patch"} tool) "editing files"
      (contains? #{"search" "grep" "ripgrep"} tool) "searching code"
      (contains? #{"list_files" "list_directory"} tool) "inspecting files"
      (string? tool) (str "using " (str/lower-case (titleize-token tool)))
      :else "using tool")))

(defn event->activity
  "Map a Codex NDJSON event to a short human-readable activity string, or nil."
  [evt]
  (let [evt-type (:type evt)]
    (cond
      (= "reasoning" evt-type)
      "preparing response"

      (= "command_execution" evt-type)
      "using bash"

      (or (= "item.started" evt-type)
          (= "item.completed" evt-type))
      (let [item (:item evt)
            item-type (:type item)
            name (:name item)]
        (cond
          (= "command_execution" item-type)
          (if (= "item.completed" evt-type)
            "using bash (done)"
            "using bash")

          (= "tool_call" item-type)
          (if (= "item.completed" evt-type)
            (str (humanize-tool-name name) " (done)")
            (humanize-tool-name name))

          (contains? #{"agent_message" "reasoning"} item-type)
          "preparing response"

          (string? item-type)
          (str/lower-case (titleize-token item-type))

          :else nil))

      :else nil)))

(defn parse-output
  "Parse `codex exec --json` output into {:session-id :text}.
   Falls back to PRIOR-SESSION-ID when no thread.start event appears."
  [raw-output prior-session-id]
  (let [events (keep parse-json-line
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
                          (keep meaningful-agent-text)
                          last)
                 (some->> events
                          (filter #(= "error" (:type %)))
                          (map :message)
                          (remove str/blank?)
                          last)
                 (when-not (seq events)
                   (some-> raw-output str/trim not-empty))
                 no-assistant-message-sentinel)]
    {:session-id session-id
     :text text}))

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

(defn- summarize-execution
  [tool-events command-events]
  {:tool-events tool-events
   :command-events command-events
   :executed? (pos? (+ tool-events command-events))})

(defn execution-evidence?
  "True when execution summary contains concrete runtime tool/command evidence."
  [execution]
  (boolean (and (map? execution) (:executed? execution))))


(defn- stale-action-type-error?
  [error-text]
  (let [t (some-> error-text str/lower-case)]
    (boolean (and (string? t)
                  (str/includes? t "action.type")
                  (or (str/includes? t "invalid value: 'other'")
                      (str/includes? t "supported values are: 'search', 'open_page', and 'find_in_page'"))))))

(defn build-exec-args
  "Build argv for codex execution.
   When SESSION-ID is present, uses `codex ... exec resume <sid> -`."
  [{:keys [codex-bin profile model sandbox approval-policy reasoning-effort session-id]
    :or {codex-bin "codex"
         sandbox "danger-full-access"
         approval-policy "never"}}]
  (let [global-opts (cond-> []
                      (and (string? profile) (not (str/blank? profile)))
                      (into ["-p" profile]))
        exec-opts (cond-> ["--json"
                           "--skip-git-repo-check"
                           "--sandbox" sandbox
                           "-c" (format "approval_policy=\"%s\"" approval-policy)]
                     (and (string? model) (not (str/blank? model)))
                     (into ["--model" model])
                     (and (string? reasoning-effort) (not (str/blank? reasoning-effort)))
                     (into ["-c" (format "model_reasoning_effort=\"%s\"" reasoning-effort)]))]
    (if (and (string? session-id) (not (str/blank? session-id)))
      (into [codex-bin] (concat global-opts ["exec"] exec-opts ["resume" session-id "-"]))
      (into [codex-bin] (concat global-opts ["exec"] exec-opts ["-"])))))

(defn- windows?
  []
  (-> (System/getProperty "os.name" "")
      str/lower-case
      (str/includes? "windows")))

(defn- process-cmd
  "Normalize command argv for ProcessBuilder execution across OSes.
   On Windows, route through cmd.exe so PATH/PATHEXT resolves codex.cmd."
  [cmd]
  (if (windows?)
    (into ["cmd.exe" "/c"] cmd)
    cmd))

(defn run-codex-stream!
  "Run CMD with PROMPT-STR on stdin, streaming Codex JSONL output.
   Returns {:exit :timed-out? :session-id :text :error-text :stderr :raw-output :execution}."
  [cmd prompt-str {:keys [timeout-ms cwd on-event on-runtime-event on-process-started on-process-exit]}]
  (let [pb (ProcessBuilder. ^java.util.List (vec (process-cmd cmd)))
        _ (when (and (string? cwd) (not (str/blank? cwd)))
            (.directory pb (io/file cwd)))
        emit-runtime! (fn [evt]
                        (when on-runtime-event
                          (try
                            (on-runtime-event evt)
                            (catch Throwable _))))
        proc (try
               (.start pb)
               (catch Exception e
                 (emit-runtime! {:kind :launch-error
                                 :argv (vec cmd)
                                 :cwd cwd
                                 :error (.getMessage e)
                                 :at (str (java.time.Instant/now))})
                 (throw e)))
        sid* (atom nil)
        text* (atom nil)
        error* (atom nil)
        tool-events* (atom 0)
        command-events* (atom 0)
        output-lines* (atom 0)
        output-bytes* (atom 0)
        out-buf (StringBuilder.)
        err-buf (StringBuilder.)
        handle-event! (fn [evt]
                        (when (tool-event? evt)
                          (swap! tool-events* inc))
                        (when (command-event? evt)
                          (swap! command-events* inc))
                        (when on-event
                          (try
                            (on-event evt)
                            (catch Throwable _)))
                        (case (:type evt)
                          "thread.started"
                          (when-let [sid (or (:thread_id evt) (:session_id evt))]
                            (reset! sid* sid))

                          "item.completed"
                          (let [item (:item evt)
                                item-type (:type item)]
                            (when (= "agent_message" item-type)
                              (when-let [msg (meaningful-agent-text (extract-agent-text item))]
                                (reset! text* msg))))

                          "error"
                          (when-let [msg (some-> (:message evt) str not-empty)]
                            (reset! error* msg))

                          "turn.failed"
                          (when-let [msg (some-> (get-in evt [:error :message]) str not-empty)]
                            (reset! error* msg))

                          nil))
        append-line! (fn [^StringBuilder sb line]
                       (.append sb line)
                       (.append sb "\n"))
        consume-lines! (fn [stream line-handler]
                         (with-open [rdr (java.io.BufferedReader.
                                          (java.io.InputStreamReader. stream))]
                           (loop []
                             (when-let [line (.readLine rdr)]
                               (line-handler line)
                               (recur)))))
        _ (emit-runtime! {:kind :process-started
                          :pid (.pid proc)
                          :argv (vec cmd)
                          :cwd cwd
                          :at (str (java.time.Instant/now))})
        _ (when on-process-started
            (try
              (on-process-started proc)
              (catch Throwable _)))
        stdout-fut (future
                     (consume-lines!
                      (.getInputStream proc)
                      (fn [line]
                        (append-line! out-buf line)
                        (let [bytes (count (.getBytes (str line "\n") java.nio.charset.StandardCharsets/UTF_8))]
                          (swap! output-lines* inc)
                          (swap! output-bytes* + bytes)
                          (emit-runtime! {:kind :output
                                          :pid (.pid proc)
                                          :stream :stdout
                                          :bytes bytes
                                          :total-lines @output-lines*
                                          :total-bytes @output-bytes*
                                          :at (str (java.time.Instant/now))}))
                        (when-let [evt (parse-json-line line)]
                          (handle-event! evt)))))
        stderr-fut (future
                     (consume-lines!
                      (.getErrorStream proc)
                      (fn [line]
                        (append-line! err-buf line)
                        (let [bytes (count (.getBytes (str line "\n") java.nio.charset.StandardCharsets/UTF_8))]
                          (swap! output-lines* inc)
                          (swap! output-bytes* + bytes)
                          (emit-runtime! {:kind :output
                                          :pid (.pid proc)
                                          :stream :stderr
                                          :bytes bytes
                                          :total-lines @output-lines*
                                          :total-bytes @output-bytes*
                                          :at (str (java.time.Instant/now))}))
                        (when-let [evt (parse-json-line line)]
                          (handle-event! evt)))))]
    (with-open [w (io/writer (.getOutputStream proc))]
      (.write w (str prompt-str "\n")))
    (let [finished? (if (and (number? timeout-ms) (pos? (long timeout-ms)))
                      (.waitFor proc (long timeout-ms) java.util.concurrent.TimeUnit/MILLISECONDS)
                      (do
                        (.waitFor proc)
                        true))]
      (when-not finished?
        (.destroyForcibly proc)
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      @stdout-fut
      @stderr-fut
      (let [exit (.exitValue proc)
            stderr (str/trim (str err-buf))
            raw-output (str/trim
                        (str (str out-buf)
                             (when (and (pos? (.length out-buf))
                                        (pos? (.length err-buf)))
                               "\n")
                             (str err-buf)))]
        (emit-runtime! {:kind :process-exit
                        :pid (.pid proc)
                        :exit exit
                        :timed-out? (not finished?)
                        :total-lines @output-lines*
                        :total-bytes @output-bytes*
                        :at (str (java.time.Instant/now))})
        (when on-process-exit
          (try
            (on-process-exit proc {:exit exit
                                   :timed-out? (not finished?)})
            (catch Throwable _)))
        {:exit exit
         :timed-out? (not finished?)
         :session-id @sid*
         :text @text*
         :error-text @error*
         :execution (summarize-execution @tool-events* @command-events*)
         :stderr stderr
         :raw-output raw-output}))))

(defn make-invoke-fn
  "Create a serialized invoke-fn backed by `codex exec --json`.

   opts:
   - :codex-bin (default \"codex\")
   - :profile (optional Codex config profile passed as `codex -p <profile> exec`)
   - :model (optional, default \"gpt-5-codex\")
   - :sandbox (default \"danger-full-access\")
   - :approval-policy (default \"never\")
   - :reasoning-effort (optional, e.g. low|medium|high)
   - :timeout-ms hard process timeout in milliseconds (default 1800000)
   - :cwd (optional working directory)
   - :on-event (optional fn called with each parsed stream event)"
  [{:keys [codex-bin profile model sandbox approval-policy reasoning-effort timeout-ms cwd
           on-event on-runtime-event on-process-started on-process-exit]
    :or {codex-bin "codex"
         model "gpt-5-codex"
         sandbox "danger-full-access"
         approval-policy "never"
         timeout-ms 1800000}}]
  (let [!lock (Object.)]
    (fn [prompt session-id]
      (locking !lock
        (try
          (let [prompt-str (coerce-prompt prompt)
            cmd (build-exec-args {:codex-bin codex-bin
                                  :profile profile
                                  :model model
                                  :sandbox sandbox
                                  :approval-policy approval-policy
                                      :reasoning-effort reasoning-effort
                                      :session-id session-id})
                {:keys [exit timed-out? text error-text stderr raw-output execution]
                 :as stream-result}
                (run-codex-stream! cmd prompt-str {:timeout-ms timeout-ms
                                                   :cwd cwd
                                                   :on-event on-event
                                                   :on-runtime-event on-runtime-event
                                                   :on-process-started on-process-started
                                                   :on-process-exit on-process-exit})
                stream-sid (:session-id stream-result)
                parsed (parse-output raw-output (or stream-sid session-id))
                final-sid (or stream-sid (:session-id parsed))
                final-text (or (meaningful-agent-text text)
                               (meaningful-agent-text (:text parsed)))
                final-error (or (some-> error-text str/trim not-empty)
                                (when-not (zero? exit)
                                  (some-> (:text parsed) str/trim not-empty))
                                (some-> stderr str/trim not-empty))
                ;; Retry on stale session (action.type error)
                retry? (and (string? session-id)
                            (not (str/blank? session-id))
                            (not timed-out?)
                            (not (zero? exit))
                            (stale-action-type-error? final-error))]
            (if retry?
              ;; Fresh session retry
          (let [cmd2 (build-exec-args {:codex-bin codex-bin
                                       :profile profile
                                       :model model
                                       :sandbox sandbox
                                       :approval-policy approval-policy
                                           :reasoning-effort reasoning-effort
                                           :session-id nil})
                    r2 (run-codex-stream! cmd2 prompt-str {:timeout-ms timeout-ms
                                                           :cwd cwd
                                                           :on-event on-event
                                                           :on-runtime-event on-runtime-event
                                                           :on-process-started on-process-started
                                                           :on-process-exit on-process-exit})
                    p2 (parse-output (:raw-output r2) (:session-id r2))
                    sid2 (or (:session-id r2) (:session-id p2))
                    txt2 (or (some-> (:text r2) str/trim not-empty)
                             (some-> (:text p2) str/trim not-empty))
                    exec2 (or (:execution r2) (summarize-execution 0 0))]
                (cond
                  (:timed-out? r2)
                  {:result nil :session-id sid2 :execution exec2
                   :error (str "codex timed out after " timeout-ms "ms")}
                  (zero? (:exit r2))
                  {:result (or txt2 "[Codex produced no text response]")
                   :session-id sid2 :execution exec2}
                  :else
                  {:result nil :session-id sid2 :execution exec2
                   :error (str "Exit " (:exit r2) ": "
                               (or (:error-text r2) "codex invocation failed")
                               " (after stale-session reset)")}))
              ;; Normal path
              (let [exec (or execution (summarize-execution 0 0))]
                (cond
                  timed-out?
                  {:result nil :session-id final-sid :execution exec
                   :error (str "codex timed out after " timeout-ms "ms")}
                  (zero? exit)
                  {:result (or final-text "[Codex produced no text response]")
                   :session-id final-sid :execution exec}
                  :else
                  {:result nil :session-id final-sid :execution exec
                   :error (str "Exit " exit ": "
                               (or final-error "codex invocation failed"))}))))
          (catch Exception e
            {:result nil
             :session-id session-id
             :error (str "codex invocation error: " (.getMessage e))}))))))
