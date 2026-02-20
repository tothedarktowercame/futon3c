(ns futon3c.agents.codex-cli
  "Codex CLI invoke adapter for runtime agent registration.

   Produces invoke-fn values compatible with the registry contract:
   (fn [prompt session-id] -> {:result string|nil :session-id string|nil :error string?})."
  (:require [cheshire.core :as json]
            [clojure.java.shell :as shell]
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

(defn parse-output
  "Parse `codex exec --json` output into {:session-id :text}.
   Falls back to PRIOR-SESSION-ID when no thread.start event appears."
  [raw-output prior-session-id]
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

(defn build-exec-args
  "Build argv for codex execution.
   When SESSION-ID is present, uses `codex exec ... resume <sid> -`."
  [{:keys [codex-bin model sandbox approval-policy session-id]
    :or {codex-bin "codex"
         sandbox "workspace-write"
         approval-policy "never"}}]
  (let [exec-opts (cond-> ["--json"
                           "--skip-git-repo-check"
                           "--sandbox" sandbox
                           "-c" (format "approval_policy=\"%s\"" approval-policy)]
                    (and (string? model) (not (str/blank? model)))
                    (into ["--model" model]))]
    (if (and (string? session-id) (not (str/blank? session-id)))
      (into [codex-bin "exec"] (concat exec-opts ["resume" session-id "-"]))
      (into [codex-bin "exec"] (concat exec-opts ["-"])))))

(defn make-invoke-fn
  "Create a serialized invoke-fn backed by `codex exec --json`.

   opts:
   - :codex-bin (default \"codex\")
   - :model (optional, default \"gpt-5-codex\")
   - :sandbox (default \"workspace-write\")
   - :approval-policy (default \"never\")
   - :cwd (optional shell working directory)"
  [{:keys [codex-bin model sandbox approval-policy cwd]
    :or {codex-bin "codex"
         model "gpt-5-codex"
         sandbox "workspace-write"
         approval-policy "never"}}]
  (let [!lock (Object.)]
    (fn [prompt session-id]
      (locking !lock
        (try
          (let [prompt-str (coerce-prompt prompt)
                cmd (build-exec-args {:codex-bin codex-bin
                                      :model model
                                      :sandbox sandbox
                                      :approval-policy approval-policy
                                      :session-id session-id})
                shell-args (cond-> (concat cmd [:in (str prompt-str "\n")])
                             (some? cwd) (concat [:dir cwd]))
                {:keys [exit out err]} (apply shell/sh shell-args)
                parsed (parse-output (str (or out "") (or err "")) session-id)
                final-sid (:session-id parsed)
                text (some-> (:text parsed) str/trim not-empty)]
            (if (zero? exit)
              {:result (or text "[Codex produced no text response]")
               :session-id final-sid}
              {:result nil
               :session-id final-sid
               :error (str "Exit " exit ": " (or text "codex invocation failed"))}))
          (catch Exception e
            {:result nil
             :session-id session-id
             :error (str "codex invocation error: " (.getMessage e))}))))))
