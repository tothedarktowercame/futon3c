(ns futon3c.peripheral.adapter
  "Claude ↔ Peripheral translation layer.

   Pure functions that bridge between Claude Code's tool system and the
   PeripheralRunner protocol. No Claude invocation — this is data
   transformation and prompt construction.

   Three concerns:
   1. Tool mapping: Claude tool names ↔ peripheral tool keywords
   2. Prompt generation: express peripheral constraints as system prompt text
   3. Exit detection: parse Claude's output for hop/exit signals"
  (:require [clojure.string :as str]
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

(def ^:private peripheral-to-claude
  "Forward mapping: peripheral tool keyword → Claude tool name."
  {:read         "Read"
   :glob         "Glob"
   :grep         "Grep"
   :edit         "Edit"
   :write        "Write"
   :bash         "Bash"
   :bash-readonly "Bash"
   :bash-test    "Bash"
   :bash-git     "Bash"
   :bash-deploy  "Bash"
   :web-fetch    "WebFetch"
   :musn-log     nil})

(defn tool-mapping
  "For a peripheral spec, return {\"Claude-tool-name\" :peripheral-tool-keyword}.
   When multiple peripheral tools map to the same Claude tool (e.g. Bash variants),
   picks the first matching tool from the peripheral's tool set."
  [peripheral-spec]
  (let [tools (:peripheral/tools peripheral-spec)]
    (->> claude-to-peripheral
         (keep (fn [[claude-name candidates]]
                 (when-let [match (first (filter tools candidates))]
                   [claude-name match])))
         (into {}))))

(defn claude-tools
  "Return the set of Claude Code tool names available for this peripheral."
  [peripheral-spec]
  (set (keys (tool-mapping peripheral-spec))))

;; =============================================================================
;; Tool call translation — Claude tool-use → PeripheralRunner action
;; =============================================================================

(def ^:private arg-extractors
  "Extract the primary argument(s) from a Claude tool call's input map."
  {"Read"     (fn [input] [(:file_path input)])
   "Glob"     (fn [input] [(:pattern input)])
   "Grep"     (fn [input] [(:pattern input)])
   "Edit"     (fn [input] [(:file_path input)])
   "Write"    (fn [input] [(:file_path input)])
   "Bash"     (fn [input] [(:command input)])
   "WebFetch" (fn [input] [(:url input)])})

(defn tool-call->action
  "Translate a Claude Code tool-use result into a PeripheralRunner action.

   tool-call: {:name \"Read\" :input {:file_path \"src/a.clj\"}}
   Returns {:tool :read :args [\"src/a.clj\"]}
   or SocialError if the tool is not available in this peripheral."
  [peripheral-spec tool-call]
  (let [mapping (tool-mapping peripheral-spec)
        claude-name (:name tool-call)
        peripheral-tool (get mapping claude-name)]
    (if-not peripheral-tool
      (runner/runner-error (:peripheral/id peripheral-spec) :unmapped-tool
                           (str "Claude tool " claude-name " not available in this peripheral")
                           :claude-tool claude-name
                           :peripheral (:peripheral/id peripheral-spec))
      (let [extractor (get arg-extractors claude-name)
            args (if (and extractor (:input tool-call))
                   (vec (remove nil? (extractor (:input tool-call))))
                   [])]
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
