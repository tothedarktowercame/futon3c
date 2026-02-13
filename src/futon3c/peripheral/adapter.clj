(ns futon3c.peripheral.adapter
  "Agent ↔ Peripheral translation layer (Claude + Codex).

   Pure functions that bridge agent tool systems and the
   PeripheralRunner protocol. No Claude invocation — this is data
   transformation and prompt construction.

   Three concerns:
   1. Tool mapping: agent tool names ↔ peripheral tool keywords
   2. Prompt generation: express peripheral constraints as system prompt text
   3. Exit detection: parse agent output for hop/exit signals"
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [futon3c.peripheral.runner :as runner]))

;; =============================================================================
;; Tool mapping — Claude Code ↔ Peripheral tool keywords
;; =============================================================================

(def ^:private claude-to-peripheral
  "Reverse mapping: Claude tool name → candidate peripheral tool keywords.
   Order matters — first match in the peripheral's tool set wins."
  {"Read"     [:read]
   "Glob"     [:glob]
   "Grep"     [:grep]
   "Edit"     [:edit]
   "Write"    [:write]
   "Bash"     [:bash :bash-readonly :bash-test :bash-git :bash-deploy]
   "WebFetch" [:web-fetch]})

(def ^:private codex-to-peripheral
  "Reverse mapping: Codex tool name → candidate peripheral tool keywords.
   Includes both CLI event names and normalized aliases."
  {"Read"              [:read]
   "read"              [:read]
   "read_file"         [:read]
   "Glob"              [:glob]
   "glob"              [:glob]
   "Grep"              [:grep]
   "grep"              [:grep]
   "Edit"              [:edit]
   "edit"              [:edit]
   "edit_file"         [:edit]
   "Write"             [:write]
   "write"             [:write]
   "write_file"        [:write]
   "Bash"              [:bash :bash-readonly :bash-test :bash-git :bash-deploy]
   "bash"              [:bash :bash-readonly :bash-test :bash-git :bash-deploy]
   "run_shell_command" [:bash :bash-readonly :bash-test :bash-git :bash-deploy]
   "WebFetch"          [:web-fetch]
   "web_fetch"         [:web-fetch]
   "musn_log"          [:musn-log]})

(defn- mapping-for
  [catalog peripheral-spec]
  (let [tools (:peripheral/tools peripheral-spec)]
    (->> catalog
         (keep (fn [[agent-tool-name candidates]]
                 (when-let [match (first (filter tools candidates))]
                   [agent-tool-name match])))
         (into {}))))

(defn tool-mapping
  "For a peripheral spec, return {\"Claude-tool-name\" :peripheral-tool-keyword}.
   When multiple peripheral tools map to the same Claude tool (e.g. Bash variants),
   picks the first matching tool from the peripheral's tool set."
  [peripheral-spec]
  (mapping-for claude-to-peripheral peripheral-spec))

(defn claude-tools
  "Return the set of Claude Code tool names available for this peripheral."
  [peripheral-spec]
  (set (keys (tool-mapping peripheral-spec))))

(defn codex-tool-mapping
  "For a peripheral spec, return {\"Codex-tool-name\" :peripheral-tool-keyword}."
  [peripheral-spec]
  (mapping-for codex-to-peripheral peripheral-spec))

(defn codex-tools
  "Return the set of Codex tool names available for this peripheral."
  [peripheral-spec]
  (set (keys (codex-tool-mapping peripheral-spec))))

;; =============================================================================
;; Tool call translation — Claude tool-use → PeripheralRunner action
;; =============================================================================

(defn- parse-json-map
  [s]
  (if (string? s)
    (try
      (json/read-str s :key-fn keyword)
      (catch Exception _ nil))
    nil))

(defn- call-name
  [tool-call]
  (or (:name tool-call)
      (:tool-name tool-call)
      (:tool_name tool-call)
      (:tool/name tool-call)))

(defn- call-input
  [tool-call]
  (let [input (or (:input tool-call)
                  (:arguments tool-call)
                  (:tool-params tool-call)
                  (:tool_params tool-call)
                  (:tool/params tool-call))]
    (cond
      (map? input) input
      (string? input) (or (parse-json-map input) {})
      :else {})))

(defn- first-val
  [m ks]
  (first (keep #(get m %) ks)))

(defn- args-for-tool
  [peripheral-tool input]
  (let [file-path (first-val input [:file_path :file-path :path :file])
        pattern (first-val input [:pattern :query :regex])
        target (first-val input [:target :path :file_path :file-path :file])
        command (first-val input [:command :cmd :shell])
        url (first-val input [:url])]
    (vec
     (remove nil?
             (case peripheral-tool
               :read [file-path]
               :glob [pattern]
               :grep [pattern target]
               :edit [file-path]
               :write [file-path]
               (:bash :bash-readonly :bash-test :bash-git :bash-deploy) [command]
               :web-fetch [url]
               :musn-log [file-path]
               [])))))

(defn tool-call->action
  "Translate a Claude Code tool-use result into a PeripheralRunner action.

   tool-call: {:name \"Read\" :input {:file_path \"src/a.clj\"}}
   Returns {:tool :read :args [\"src/a.clj\"]}
  or SocialError if the tool is not available in this peripheral."
  [peripheral-spec tool-call]
  (let [mapping (tool-mapping peripheral-spec)
        claude-name (call-name tool-call)
        peripheral-tool (get mapping claude-name)]
    (if-not peripheral-tool
      (runner/runner-error (:peripheral/id peripheral-spec) :unmapped-tool
                           (str "Claude tool " claude-name " not available in this peripheral")
                           :claude-tool claude-name
                           :peripheral (:peripheral/id peripheral-spec))
      (let [args (args-for-tool peripheral-tool (call-input tool-call))]
        {:tool peripheral-tool
         :args args}))))

(defn codex-tool-call->action
  "Translate a Codex tool-call event into a PeripheralRunner action.

   Accepts several Codex event shapes:
   - {:name \"edit_file\" :input {:path \"src/a.clj\"}}
   - {:tool-name \"Bash\" :tool-params {:command \"clojure -X:test\"}}
   - {:name \"function_call\" :arguments \"{...json...}\"}"
  [peripheral-spec tool-call]
  (let [mapping (codex-tool-mapping peripheral-spec)
        codex-name (call-name tool-call)
        peripheral-tool (get mapping codex-name)]
    (if-not peripheral-tool
      (runner/runner-error (:peripheral/id peripheral-spec) :unmapped-tool
                           (str "Codex tool " codex-name " not available in this peripheral")
                           :codex-tool codex-name
                           :peripheral (:peripheral/id peripheral-spec))
      (let [args (args-for-tool peripheral-tool (call-input tool-call))]
        {:tool peripheral-tool
         :args args}))))

;; =============================================================================
;; Constraint description — structured + prompt text
;; =============================================================================

(def ^:private tool-descriptions
  {:read         "Read file contents"
   :glob         "Find files by pattern"
   :grep         "Search file contents by regex"
   :edit         "Edit files (exact string replacement)"
   :write        "Create or overwrite files"
   :bash         "Execute shell commands"
   :bash-readonly "Execute read-only shell commands (no writes)"
   :bash-test    "Execute test commands"
   :bash-git     "Execute git commands (commit, log, etc.)"
   :bash-deploy  "Execute deployment commands (push, deploy)"
   :web-fetch    "Fetch web content"
   :musn-log     "Access MUSN session logs"})

(def ^:private all-peripheral-tools
  (set (keys tool-descriptions)))

(def ^:private scope-descriptions
  {:full-codebase      "Full codebase — no path restrictions"
   :test-commands-only "Test commands only — no file modifications"
   :git-push-only      "Git operations only — commit and push"
   :session-log-only   "Session log access only — read and analyze"})

(defn describe-constraints
  "Structured representation of a peripheral's constraints.
   Returns {:allowed-tools [...] :forbidden-tools [...] :scope \"...\" :exit-conditions [...]}."
  [peripheral-spec]
  (let [tools (:peripheral/tools peripheral-spec)
        scope (:peripheral/scope peripheral-spec)
        exit (:peripheral/exit peripheral-spec)]
    {:allowed-tools (vec (sort (map name tools)))
     :forbidden-tools (vec (sort (map name (remove tools all-peripheral-tools))))
     :scope (cond
              (keyword? scope) (get scope-descriptions scope (name scope))
              (and (map? scope) (:paths scope))
              (str "Scoped to paths: " (str/join ", " (:paths scope)))
              :else "unrestricted")
     :exit-conditions (vec (sort (map name exit)))}))

(defn peripheral-prompt-section
  "Generate a system prompt section expressing the peripheral's constraints.
   This is the prompt engineering that makes Claude operate within the
   peripheral's envelope — pure data transformation, no Claude invocation."
  [peripheral-spec context]
  (let [{:keys [allowed-tools forbidden-tools scope exit-conditions]}
        (describe-constraints peripheral-spec)
        pid (name (:peripheral/id peripheral-spec))
        sid (:session-id context)]
    (str "## Peripheral Mode: " (str/upper-case pid) "\n\n"
         "You are operating in **" pid "** mode.\n\n"
         "### Available Tools\n"
         (str/join "\n" (map (fn [t]
                               (str "- " t ": "
                                    (get tool-descriptions (keyword t) t)))
                             allowed-tools))
         "\n\n"
         "### Scope\n"
         scope "\n\n"
         "### Constraints\n"
         "You CANNOT use: " (str/join ", " forbidden-tools) "\n"
         "Attempts to use forbidden tools will be structurally rejected.\n\n"
         "### Exit Conditions\n"
         "Signal one of these when your goal is complete:\n"
         (str/join "\n" (map #(str "- " %) exit-conditions))
         (when sid (str "\n\n### Session\n" "session-id: " sid))
         "\n")))

(defn codex-instruction-section
  "Generate a Codex-friendly instruction section for peripheral constraints."
  [peripheral-spec context]
  (str "## Codex Peripheral Constraints\n\n"
       (peripheral-prompt-section peripheral-spec context)))

;; =============================================================================
;; Exit detection — parse Claude's output for hop/exit signals
;; =============================================================================

(def ^:private exit-patterns
  "Maps regex patterns to exit condition keywords.
   Ordered from most specific to least specific."
  [[#"(?i)found\s+(?:the\s+)?target" :found-target]
   [#"(?i)ready\s+to\s+edit"         :ready-to-edit]
   [#"(?i)ready\s+to\s+commit"       :ready-to-commit]
   [#"(?i)tests?\s+pass"             :tests-pass]
   [#"(?i)par\s+generated"           :par-generated]
   [#"(?i)session\s+close"           :session-close]
   [#"(?i)\bblocked\b"               :blocked]
   [#"(?i)hop\s+(?:to\s+)?reflect"   :hop-reflect]
   [#"(?i)hop\s+(?:to\s+)?deploy"    :hop-deploy]
   [#"(?i)hop\s+(?:to\s+)?test"      :hop-test]
   [#"(?i)hop\s+(?:to\s+)?edit"      :hop-edit]
   [#"(?i)user\s+request"            :user-request]])

(defn detect-exit
  "Analyze text for exit condition signals.
   Only matches against the peripheral's own exit conditions.
   Returns nil if no exit detected, or {:exit-condition :keyword :reason \"matched text\"}."
  [peripheral-spec text]
  (when (and (string? text) (seq text))
    (let [exit-set (:peripheral/exit peripheral-spec)]
      (->> exit-patterns
           (keep (fn [[pattern condition]]
                   (when (and (contains? exit-set condition)
                              (re-find pattern text))
                     {:exit-condition condition
                      :reason (str (re-find pattern text))})))
           first))))

(defn codex-detect-exit
  "Alias of detect-exit for Codex output processing."
  [peripheral-spec text]
  (detect-exit peripheral-spec text))
