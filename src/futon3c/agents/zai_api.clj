(ns futon3c.agents.zai-api
  "Small Z.AI API-backed coding harness.

   Z.AI supplies the model and OpenAI-style tool calls; this namespace supplies
   the local agent loop and delegates real work to futon3c.peripheral.real-backend."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agents.zaif-controller :as zaif]
            [futon3c.agents.zaif-inputs :as zaif-inputs]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-backend :as memory-backend]
            [futon3c.peripheral.real-backend :as real-backend]
            [futon3c.peripheral.tools :as tools])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.util UUID]))

(def ^:private default-base-url
  "https://api.z.ai/api/coding/paas/v4")

(def ^:private default-model "glm-5.2")

(defn- getenv [k]
  (some-> (System/getenv k) str/trim not-empty))

(defn- read-key-file [path]
  (try
    (let [f (io/file (str/replace-first path #"^~" (System/getProperty "user.home")))]
      (when (.exists f)
        (some-> f slurp str/trim not-empty)))
    (catch Throwable _ nil)))

(defn resolve-api-key
  []
  (or (getenv "ZAI_API_KEY")
      (read-key-file "~/.zaikey")
      (read-key-file "~/.zai-key")))

(defn- chat-url [base-url]
  (str (str/replace (or base-url default-base-url) #"/+$" "")
       "/chat/completions"))

(defn- json-schema
  [props required]
  {:type "object"
   :properties props
   :required required
   :additionalProperties false})

(def ^:private tool-specs
  [{:name "read_file"
    :description "Read a local file relative to the agent cwd unless an absolute path is supplied."
    :parameters (json-schema
                 {:path {:type "string"}
                  :offset {:type "integer" :description "Optional zero-based line offset."}
                  :limit {:type "integer" :description "Optional maximum number of lines."}}
                 ["path"])}
   {:name "list_files"
    :description "List files matching a glob pattern. Prefer this before broad shell commands."
    :parameters (json-schema
                 {:pattern {:type "string" :description "Glob, for example **/*.clj."}
                  :base_dir {:type "string" :description "Optional base directory."}}
                 ["pattern"])}
   {:name "search"
    :description "Search files with a regular expression."
    :parameters (json-schema
                 {:pattern {:type "string"}
                  :path {:type "string" :description "File or directory to search; defaults to cwd."}
                  :case_insensitive {:type "boolean"}
                  :max_matches {:type "integer"}}
                 ["pattern"])}
   {:name "edit_file"
    :description "Replace the first exact occurrence of old_string in a file."
    :parameters (json-schema
                 {:path {:type "string"}
                  :old_string {:type "string"}
                  :new_string {:type "string"}}
                 ["path" "old_string" "new_string"])}
   {:name "write_file"
    :description "Write a complete file. Use only when creating a file or intentionally replacing all content."
    :parameters (json-schema
                 {:path {:type "string"}
                  :content {:type "string"}}
                 ["path" "content"])}
   {:name "run_shell"
    :description "Run a shell command in cwd. Use for tests, builds, git diff, or focused inspection."
    :parameters (json-schema
                 {:command {:type "string"}
                  :timeout_ms {:type "integer"}}
                 ["command"])}
   {:name "run_readonly"
    :description "Run a read-only shell command in cwd. Destructive-looking commands are rejected."
    :parameters (json-schema
                 {:command {:type "string"}
                  :timeout_ms {:type "integer"}}
                 ["command"])}
   {:name "reflect_namespaces"
    :description "List loaded Clojure namespaces in the JVM."
    :parameters (json-schema
                 {:pattern {:type "string"}}
                 [])}
   {:name "reflect_ns"
    :description "Reflect public vars in a loaded Clojure namespace."
    :parameters (json-schema
                 {:namespace {:type "string"}}
                 ["namespace"])}
   {:name "reflect_var"
    :description "Reflect one Clojure var, either as ns/var or namespace plus var."
    :parameters (json-schema
                 {:name {:type "string"}
                  :namespace {:type "string"}
                  :var {:type "string"}}
                 [])}
   {:name "reflect_deps"
    :description "Return the dependency graph for a loaded Clojure namespace."
    :parameters (json-schema
                 {:namespace {:type "string"}}
                 ["namespace"])}
   {:name "reflect_java_class"
    :description "Reflect a Java class visible to the JVM."
    :parameters (json-schema
                 {:class {:type "string"}}
                 ["class"])}
   {:name "irc_recent"
    :description "Read recent IRC messages from the local Futon IRC bridge log."
    :parameters (json-schema
                 {:limit {:type "integer"}}
                 [])}
   {:name "irc_send"
    :description "Send one message to IRC. Use channel #futon unless the user names another channel."
    :parameters (json-schema
                 {:channel {:type "string"}
                  :from {:type "string"}
                  :text {:type "string"}}
                 ["text"])}
   {:name "boot_context"
    :description "Report this agent's current situation: identity, session, cwd, clock target, git branch and dirty files, AGENTS.md presence. Snapshot at call time."
    :parameters (json-schema {} [])}
   {:name "repo_contract"
    :description "Read the repo's AGENTS.md contract verbatim, with path, size, and mtime."
    :parameters (json-schema
                 {:repo {:type "string" :description "Optional directory; defaults to the agent cwd."}}
                 [])}
   {:name "psr_search"
    :description "Search the futon pattern library for patterns relevant to a task. Returns scored candidates plus bounded hooks for reviewed attached memories; cite the ids you rely on."
    :parameters (json-schema
                 {:query {:type "string"}
                  :top_k {:type "integer"}
                  :include_details {:type "boolean"}}
                 ["query"])}
   {:name "psr_select"
    :description "Record a Pattern Selection Record (PSR) before applying a library pattern. Returns full bodies of reviewed memories attached to the selected pattern."
    :parameters (json-schema
                 {:pattern_id {:type "string"}
                  :rationale {:type "string"}
                  :task_id {:type "string"}
                  :candidates {:type "array" :items {:type "string"}}}
                 ["pattern_id"])}
   {:name "pur_update"
    :description "Record a Pattern Use Record (PUR) after applying a selected pattern. Cite used memory ids, explain deliberate rejections, and optionally attach a separately recorded independent outcome id."
    :parameters (json-schema
                 {:pattern_id {:type "string"}
                  :outcome {:type "string" :description "success | partial | failure"}
                  :prediction_error {:type "string"}
                  :memory_ids {:type "array" :items {:type "string"}
                               :description "Ids returned by psr_select that materially informed the work."}
                  :memory_rejections
                  {:type "array"
                   :items {:type "object"
                           :properties
                           {:memory_id {:type "string"}
                            :reason {:type "string"}}
                           :required ["memory_id" "reason"]}
                   :description "Surfaced memories deliberately rejected as irrelevant or misleading."}
                  :outcome_id
                  {:type "string"
                   :description "Existing separately recorded independently witnessed outcome evidence id."}}
                 ["pattern_id" "outcome"])}
   {:name "par_punctuate"
    :description "Punctuate the session with a PAR: what worked, what didn't, prediction errors, suggestions. Lands as a proof path in the evidence store."
    :parameters (json-schema
                 {:what_worked {:type "string"}
                  :what_didnt {:type "string"}
                  :prediction_errors {:type "array" :items {:type "object"}
                                      :description "Structured entries, e.g. {\"expected\": ..., \"actual\": ...} or {\"description\": ...}. The PAR shape requires maps, not strings."}
                  :suggestions {:type "array" :items {:type "string"}}}
                 ["what_worked"])}
   {:name "memory_record"
    :description (str "Record one deliberate assert memory as evidence plus a typed hyperedge. "
                      "Kind rule: derived-from-a-failure-with-a-why → feedback; documented "
                      "contract/scope fact → reference. Identity is server-stamped.")
    :parameters (json-schema
                 {:name {:type "string" :description "Kebab-case identity hint."}
                  :hook {:type "string" :description "One-line retrieval hook, at most 80 characters."}
                  :kind {:type "string" :enum ["feedback" "reference" "project" "user"]}
                  :body {:type "string" :description "Self-contained memory fact."}
                  :why {:type "string"}
                  :how_to_apply {:type "string" :description "Retrieval predicate or application condition."}
                  :subjects {:type "array"
                             :minItems 1
                             :description "At least one; first is the primary subject."
                             :items {:type "object"
                                     :properties {:ref/type {:type "string"}
                                                  :ref/id {:type "string"}}
                                     :required ["ref/type" "ref/id"]
                                     :additionalProperties false}}
                  :distills {:type "array" :items {:type "string"}
                             :description "Evidence ids or @current-round."}
                  :facets {:type "array" :items {:type "string"}}
                  :volatile {:type "boolean" :description "True when valid-until-changed."}}
                 ["name" "hook" "kind" "body" "subjects"])}
   {:name "memory_search"
    :description "Search the evidence store by filters (type, claim-type, author, since, tags). Returns the memory envelope {:frame :query :items}. Read-only."
    :parameters (json-schema
                 {:subject {:type "object" :description "ArtifactRef {:ref/type :ref/id} to scope the search to one subject."}
                  :type {:type "string" :description "EvidenceType keyword, e.g. observation|claim|pattern-outcome."}
                  :claim_type {:type "string" :description "ClaimType keyword, e.g. goal|conjecture|conclusion."}
                  :author {:type "string"}
                  :since {:type "string" :description "ISO-8601 timestamp lower bound (inclusive)."}
                  :tags {:type "array" :items {:type "string"} :description "Tag keywords to filter by."}
                  :limit {:type "integer" :description "Max items (default 20, max 100)."}
                  :include_ephemeral {:type "boolean"}}
                 [])}
   {:name "tool_history"
    :description "Read the clock-store session state for this agent: current clock target, edit activity, last auto-clock witness. Returns the memory envelope. Read-only."
    :parameters (json-schema {} [])}
   {:name "evidence_graph"
    :description "Project evidence into graphs. Modes: thread (needs subject-ref), reply-chain (needs evidence-id), forks (needs evidence-id), neighborhood (needs end-id). Returns the memory envelope. Read-only."
    :parameters (json-schema
                 {:mode {:type "string" :description "thread | reply-chain | forks | neighborhood. Default thread."}
                  :subject_ref {:type "object" :description "{:ref/type :ref/id} ArtifactRef for thread mode."}
                  :evidence_id {:type "string" :description "EvidenceEntry id for reply-chain/forks modes."}
                  :end_id {:type "string" :description "Endpoint id (string or UUID) for neighborhood mode."}
                  :limit {:type "integer" :description "Max items (default 20, max 100)."}}
                 [])}
   {:name "pattern_memory"
    :description "Query the evidence store for pattern-family tags (PSR/PUR/PAR/proof-path). Returns the memory envelope. Read-only."
    :parameters (json-schema
                 {:tags {:type "array" :items {:type "string"} :description "Override default tags [:psr :pur :par :proof-path]."}
                  :limit {:type "integer" :description "Max items (default 20, max 100)."}}
                 [])}
   {:name "recent_coordination"
    :description "Read recent coordination activity: invoke jobs (bells/whistles) and mesh edges. Returns the memory envelope. Read-only."
    :parameters (json-schema
                 {:limit {:type "integer" :description "Max items (default 20, max 100)."}
                  :scope {:type "string" :description "jobs | edges | both. Default both."}}
                 [])}
   {:name "mission_context"
    :description "Compose mission orientation: mission markdown (status banner + last checkpoint), obligations, and related evidence. Defaults to the clocked mission. Returns the memory envelope. Read-only."
    :parameters (json-schema
                 {:target {:type "string" :description "Optional mission id, e.g. M-custom-harness. Defaults to the clocked mission."}
                  :limit {:type "integer" :description "Max items (default 20, max 100)."}}
                 [])}])

(def ^:private memory-family-tool-names
  "The memory write/read tools plus the two orientation tools; removable per
   :memory-mode for the M-custom-harness §8.4 comparison conditions."
  #{"memory_record" "memory_search" "tool_history" "evidence_graph" "pattern_memory"
    "recent_coordination" "mission_context"})

(def ^:private orientation-tool-names
  #{"boot_context" "repo_contract"})

(defn- specs-for-mode
  "Tool specs for a §8.4 condition. :full (default) — everything;
   :files — no memory family (orientation + files remain: condition b);
   :none — no memory family, no orientation tools (condition a)."
  [memory-mode]
  (case (or memory-mode :full)
    :none (remove #(or (memory-family-tool-names (:name %))
                       (orientation-tool-names (:name %)))
                  tool-specs)
    :files (remove #(memory-family-tool-names (:name %)) tool-specs)
    tool-specs))

(defn- openai-tools
  ([] (openai-tools :full))
  ([memory-mode]
   (mapv (fn [{:keys [name description parameters]}]
           {:type "function"
            :function {:name name
                       :description description
                       :parameters parameters}})
         (specs-for-mode memory-mode))))

(defn- parse-arguments [s]
  (cond
    (map? s) s
    (str/blank? (str s)) {}
    :else (try
            (json/parse-string (str s) true)
            (catch Throwable _
              ;; Malformed arguments are almost always the model's tool-call
              ;; JSON truncated at max_tokens. Mapping this to {} made it look
              ;; like the model omitted its arguments, and the generic error
              ;; sent it into an identical-retry loop (zai-10's write_file
              ;; "I keep forgetting to pass arguments", 2026-07-04). Surface a
              ;; sentinel so execute-tool can report what actually happened.
              (let [raw (str s)]
                {:__unparseable_arguments
                 (str (count raw) " chars of invalid JSON"
                      (when (>= (count raw) 40)
                        (str ", ends: …" (subs raw (- (count raw) 40)))))})))))

(defn- result-string [x]
  (let [s (if (string? x) x (json/generate-string x))]
    (if (> (count s) 12000)
      (str (subs s 0 12000) "\n...[truncated " (- (count s) 12000) " chars]")
      s)))

(defn- detect-stuck!
  "stuck-means-signal, mechanical (mistakes-ledger §11): track consecutive
   identical (tool, args, result) triples in !state {:s sig :n count}.
   At 3 repeats inject a change-approach warning into the tool result; at
   5+ inject a stop-and-bell-your-reviewer instruction. Returns the
   (possibly annotated) executed map unchanged otherwise."
  [!state tc executed]
  (let [sig [(get-in tc [:function :name])
             (str (get-in tc [:function :arguments]))
             (str (get-in executed [:message :content]))]
        {:keys [n]} (swap! !state
                           (fn [{:keys [s n]}]
                             (if (= s sig)
                               {:s s :n (inc (long (or n 0)))}
                               {:s sig :n 1})))]
    (if (>= (long n) 3)
      (update-in executed [:message :content] str
                 "\n[STUCK-DETECTOR] identical call + identical result, x" n
                 " (process-coherence/stuck-means-signal). "
                 (if (>= (long n) 5)
                   (str "STOP repeating NOW. State what you were trying and "
                        "what stayed unchanged, then bell your reviewer for "
                        "help (run_shell: python3 /home/joe/code/futon3c/"
                        "scripts/agency_send.py --from <your-id> --to "
                        "<reviewer> --kind bell). Do not issue this call again.")
                   "Change approach: different arguments, different tool, or chunk the operation."))
      executed)))

(defn- tool-call-detail [tool-call args]
  {:id (:id tool-call)
   :name (get-in tool-call [:function :name])
   :input args})

(declare emit-bug-records!)

(defn- current-mission-id
  [agent-id session-id]
  (try
    (when-let [current-clock (requiring-resolve
                              'futon3c.agency.clock-store/current-clock)]
      (:mission-id (current-clock agent-id session-id)))
    (catch Throwable _ nil)))

(defn- execute-tool
  [backend {:keys [irc-send-fn irc-recent-fn agent-id cwd session-id-atom
                   evidence-store turn-id round profile session-id-fallback]
            :as ctx}
   tool-call]
  (let [name (get-in tool-call [:function :name])
        args (parse-arguments (get-in tool-call [:function :arguments]))
        fail (fn [msg] {:ok false :error msg})
        result
        (cond
          (:__unparseable_arguments args)
          (fail (str "TOOL-CALL ARGUMENTS CORRUPTED IN TRANSIT — you did NOT "
                     "omit them. Received " (:__unparseable_arguments args)
                     ". This usually means your arguments JSON was cut off at "
                     "the output-token limit. Do NOT retry the identical call: "
                     "send a SMALLER call — write_file with a short first chunk "
                     "of the content, then edit_file to append the rest piece "
                     "by piece."))

          ;; Empty arguments on a tool that requires them is the same transport
          ;; artifact (truncation before/at the arguments block), not the model
          ;; "forgetting" — the generic missing-path error sent zai-10 into an
          ;; identical-retry loop (2026-07-04).
          (and (empty? args)
               (contains? #{"read_file" "list_files" "search" "edit_file"
                            "write_file" "run_shell" "run_readonly"
                            "reflect_ns" "reflect_var" "reflect_deps"
                            "reflect_java_class" "psr_search" "psr_select"
                            "pur_update" "memory_record" "irc_send"}
                          name))
          (fail (str "TOOL-CALL ARGUMENTS ARRIVED EMPTY at the harness for "
                     name " — a transport/truncation artifact, not you "
                     "forgetting them. Do NOT retry the identical call. If you "
                     "were writing a long file, the arguments likely exceeded "
                     "the output-token limit: write_file a short first chunk, "
                     "then edit_file to append the rest piece by piece."))

          :else
          (case name
          "read_file"
          (tools/execute-tool backend :read
                              [(:path args)
                               (cond-> {}
                                 (:offset args) (assoc :offset (:offset args))
                                 (:limit args) (assoc :limit (:limit args)))])

          "list_files"
          (tools/execute-tool backend :glob
                              (cond-> [(:pattern args)]
                                (:base_dir args) (conj (:base_dir args))))

          "search"
          (tools/execute-tool backend :grep
                              [(:pattern args)
                               (or (:path args) ".")
                               (cond-> {}
                                 (:case_insensitive args) (assoc :case-insensitive true)
                                 (:max_matches args) (assoc :max-matches (:max_matches args)))])

          "edit_file"
          (tools/execute-tool backend :edit
                              [(:path args) (:old_string args) (:new_string args)])

          "write_file"
          (tools/execute-tool backend :write
                              [(:path args) (:content args)])

          "run_shell"
          (tools/execute-tool backend :bash
                              [(:command args)
                               (cond-> {}
                                 (:timeout_ms args) (assoc :timeout-ms (:timeout_ms args)))])

          "run_readonly"
          (tools/execute-tool backend :bash-readonly
                              [(:command args)
                               (cond-> {}
                                 (:timeout_ms args) (assoc :timeout-ms (:timeout_ms args)))])

          "reflect_namespaces"
          (tools/execute-tool backend :reflect-namespaces
                              (cond-> []
                                (:pattern args) (conj (:pattern args))))

          "reflect_ns"
          (tools/execute-tool backend :reflect-ns [(:namespace args)])

          "reflect_var"
          (tools/execute-tool backend :reflect-var
                              (if (:name args)
                                [(:name args)]
                                [(:namespace args) (:var args)]))

          "reflect_deps"
          (tools/execute-tool backend :reflect-deps [(:namespace args)])

          "reflect_java_class"
          (tools/execute-tool backend :reflect-java-class [(:class args)])

          "irc_recent"
          (if irc-recent-fn
            (try {:ok true :result (irc-recent-fn (or (:limit args) 30))}
                 (catch Throwable t (fail (.getMessage t))))
            (fail "IRC recent-message reader is not configured"))

          "boot_context"
          (memory-backend/boot-context {:agent-id agent-id
                                        :session-id (some-> session-id-atom deref)
                                        :cwd cwd})

          "repo_contract"
          (memory-backend/repo-contract {:cwd cwd} {:repo (:repo args)})

          "memory_search"
          (memory-backend/memory-search
           {:agent-id agent-id :session-id (some-> session-id-atom deref) :cwd cwd}
           (cond-> {}
             (:subject args) (assoc :subject (:subject args))
             (:type args) (assoc :type (:type args))
             (:claim_type args) (assoc :claim-type (:claim_type args))
             (:author args) (assoc :author (:author args))
             (:since args) (assoc :since (:since args))
             (seq (:tags args)) (assoc :tags (vec (:tags args)))
             (:limit args) (assoc :limit (:limit args))
             (:include_ephemeral args) (assoc :include-ephemeral? true)))

          "tool_history"
          (memory-backend/tool-history
           {:agent-id agent-id :session-id (some-> session-id-atom deref)}
           nil)

          "evidence_graph"
          (memory-backend/evidence-graph
           {:agent-id agent-id :cwd cwd}
           (cond-> {}
             (:mode args) (assoc :mode (:mode args))
             (:subject_ref args) (assoc :subject-ref (:subject_ref args))
             (:evidence_id args) (assoc :evidence-id (:evidence_id args))
             (:end_id args) (assoc :end-id (:end_id args))
             (:limit args) (assoc :limit (:limit args))))

          "mission_context"
          (memory-backend/mission-context
           {:agent-id agent-id
            :session-id (some-> session-id-atom deref)
            :cwd cwd}
           (cond-> {}
             (:target args) (assoc :target (:target args))
             (:limit args) (assoc :limit (:limit args))))

          "pattern_memory"
          (memory-backend/pattern-memory
           {:agent-id agent-id :cwd cwd}
           (cond-> {}
             (seq (:tags args)) (assoc :tags (vec (:tags args)))
             (:limit args) (assoc :limit (:limit args))))

          "recent_coordination"
          (memory-backend/recent-coordination
           {:agent-id agent-id :cwd cwd}
           (cond-> {}
             (:limit args) (assoc :limit (:limit args))
             (:scope args) (assoc :scope (:scope args))))

          "psr_search"
          (tools/execute-tool backend :psr-search
                              [(:query args)
                               (cond-> {}
                                 (:top_k args) (assoc :top-k (:top_k args))
                                 (:include_details args) (assoc :include-details true))])

          "psr_select"
          (tools/execute-tool backend :psr-select
                              [(:pattern_id args)
                               (cond-> {}
                                 (:rationale args) (assoc :rationale (:rationale args))
                                 (:task_id args) (assoc :task-id (:task_id args))
                                 (seq (:candidates args)) (assoc :candidates (vec (:candidates args))))])

          "memory_record"
          (let [session-id (or (some-> session-id-atom deref) session-id-fallback)
                payload (cond-> args
                          (:how_to_apply args)
                          (assoc :how-to-apply (:how_to_apply args))
                          (contains? args :volatile)
                          (assoc :volatile? (:volatile args)))]
            (tools/execute-tool
             backend :memory-record
             [{:agent-id agent-id
               :session-id session-id
               :turn-id turn-id
               :round round
               :mission-id (current-mission-id agent-id session-id)
               :domain (or (:memory-domain ctx) :zaif-work)
               :evidence-store evidence-store}
              (dissoc payload :how_to_apply :volatile)]))

          "pur_update"
          (tools/execute-tool backend :pur-update
                              [(:pattern_id args)
                               (cond-> {:outcome (:outcome args)}
                                 (:prediction_error args)
                                 (assoc :prediction-error (:prediction_error args))
                                 (seq (:memory_ids args))
                                 (assoc :memory-ids (vec (:memory_ids args)))
                                 (seq (:memory_rejections args))
                                 (assoc :memory-rejections
                                        (vec (:memory_rejections args)))
                                 (:outcome_id args)
                                 (assoc :outcome-id (:outcome_id args)))])

          "par_punctuate"
          (let [par-result (tools/execute-tool backend :par-punctuate
                              [(cond-> {:session-ref (some-> session-id-atom deref)}
                                 (:what_worked args) (assoc :what-worked (:what_worked args))
                                 (:what_didnt args) (assoc :what-didnt (:what_didnt args))
                                 ;; PAR shape wants [:vector map?] — coerce stray strings.
                                 (seq (:prediction_errors args))
                                 (assoc :prediction-errors
                                        (mapv #(if (map? %) % {:description (str %)})
                                              (:prediction_errors args)))
                                 (seq (:suggestions args)) (assoc :suggestions (vec (:suggestions args))))])]
            ;; ZU-4: session-end sweep — emit :bug/* records for tool failures.
            (emit-bug-records! {:agent-id agent-id
                                :sid (some-> session-id-atom deref str)
                                :turn-id turn-id
                                :profile profile
                                :evidence-store evidence-store})
            par-result)

          "irc_send"
          (if irc-send-fn
            (try
              (let [channel (or (:channel args) "#futon")
                    from (or (:from args) agent-id "zai")
                    text (:text args)]
                (irc-send-fn channel from text)
                {:ok true :result {:channel channel :from from :text text}})
              (catch Throwable t (fail (.getMessage t))))
            (fail "IRC sender is not configured"))

          (fail (str "Unknown tool: " name))))]

    {:detail (tool-call-detail tool-call args)
     :message {:role "tool"
               :tool_call_id (:id tool-call)
               :name name
               :content (result-string result)}
     :result result
     :error? (and (map? result) (false? (:ok result)))}))

(defn- assistant-text [message]
  (let [content (:content message)
        reasoning (:reasoning_content message)]
    (cond
      (and (string? content) (not (str/blank? content))) content
      (and (string? reasoning) (not (str/blank? reasoning))) reasoning
      :else "")))

(defn- chat!
  [client {:keys [api-key base-url model max-tokens temperature timeout-ms memory-mode]} messages]
  (let [body (json/generate-string
              (cond-> {:model (or model default-model)
                       :messages messages
                       :tools (openai-tools memory-mode)
                       :tool_choice "auto"
                       ;; 8192: 4096 truncated large tool-call arguments in
                       ;; transit (zai-10's write_file loop, claude-18's
                       ;; diagnosis 2026-07-04) — big file writes need headroom.
                       :max_tokens (or max-tokens 8192)
                       :temperature (or temperature 0.2)
                       :thinking {:type "disabled"}
                       :reasoning_effort "none"}
                (getenv "ZAI_THINKING_TYPE")
                (assoc-in [:thinking :type] (getenv "ZAI_THINKING_TYPE"))
                (getenv "ZAI_REASONING_EFFORT")
                (assoc :reasoning_effort (getenv "ZAI_REASONING_EFFORT"))))
        req (-> (HttpRequest/newBuilder (URI/create (chat-url base-url)))
                (.timeout (Duration/ofMillis (long (or timeout-ms 300000))))
                (.header "Content-Type" "application/json")
                (.header "Authorization" (str "Bearer " api-key))
                (.POST (HttpRequest$BodyPublishers/ofString body))
                .build)
        resp (.send client req (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)
        raw (.body resp)
        parsed (try (json/parse-string raw true)
                    (catch Throwable t
                      {:error {:message (str "Could not parse response: " (.getMessage t))
                               :raw raw}}))]
    (if (<= 200 status 299)
      parsed
      {:error {:message (str "HTTP " status)
               :body parsed}})))

(defn- sink! [agent-id event]
  (when (find-ns 'futon3c.agency.registry)
    (when-let [get-sink (ns-resolve 'futon3c.agency.registry 'get-invoke-event-sink)]
      (when-let [sink (get-sink (str agent-id))]
        (try (sink event) (catch Throwable _))))))

;; --- U1: transcript persistence (M-zaif-harness) --------------------------
;; sink! above feeds the invoke-jobs ring buffer: display-grade, in-memory,
;; gone on JVM restart — which left an agent's claims about its own past
;; tool calls unadjudicable (first live zaif demo, 2026-07-11). Persist each
;; tool ROUND as a typed evidence entry instead: what the agent did becomes
;; part of the record (R9 — narration is not evidence). These are semantic
;; act records (tool + args + result digest), never raw transport envelopes
;; (policy: transport/http.clj emit-invoke-evidence!). Volume is bounded by
;; the round budget (~1-25 entries per turn); large tool RESULTS are stored
;; as digest + preview only. The store is resolved dynamically so a
;; namespace reload picks this up in already-registered invoke closures,
;; and so a swapped backend (futon3c.dev/!evidence-store) is honoured.

(def ^:private transcript-text-cap 4096)
(def ^:private transcript-args-cap 2048)
(def ^:private transcript-preview-cap 240)

(defn- transcript-truncate [s cap]
  (let [s (str s)]
    (if (> (count s) cap)
      (str (subs s 0 cap) "…[+" (- (count s) cap) " chars]")
      s)))

(defn- transcript-digest
  "Digest a tool result for the record: enough to adjudicate against
   (identity + size + head), without storing bulk content twice."
  [content]
  (let [s (str content)
        md (java.security.MessageDigest/getInstance "SHA-256")
        hex (apply str (map #(format "%02x" %) (take 8 (.digest md (.getBytes s "UTF-8")))))]
    {:sha256-16 hex
     :chars (count s)
     :preview (transcript-truncate s transcript-preview-cap)}))

(defonce ^:private !transcript-persistence-status
  (atom {:ok-count 0 :failure-count 0 :last-error nil :last-evidence-id nil}))

(defn transcript-persistence-status
  "Return loss-accounting counters for ZAI/ZAIF transcript writes."
  []
  @!transcript-persistence-status)

(defn- persist-transcript-entry!
  "Synchronously append ENTRY and fail the turn if durable evidence is lost."
  [evidence-store entry]
  (let [evidence-id (:evidence/id entry)]
    (try
      (when-not evidence-store
        (throw (ex-info "ZAI transcript evidence store is unavailable"
                        {:evidence-id evidence-id})))
      (let [receipt (boundary/append! evidence-store entry)]
        (when-not (:ok receipt)
          (throw (ex-info "ZAI transcript persistence was rejected"
                          {:evidence-id evidence-id :receipt receipt})))
        (swap! !transcript-persistence-status
               (fn [status]
                 (-> status
                     (update :ok-count (fnil inc 0))
                     (assoc :last-error nil :last-evidence-id evidence-id))))
        receipt)
      (catch Throwable t
        (swap! !transcript-persistence-status
               (fn [status]
                 (-> status
                     (update :failure-count (fnil inc 0))
                     (assoc :last-error (.getMessage t)
                            :last-evidence-id evidence-id))))
        (binding [*out* *err*]
          (println (str "[zai-transcript] FATAL " evidence-id ": " (.getMessage t)))
          (flush))
        (throw t)))))

(defn- persist-transcript-safely!
  "Turn-safe transcript persistence. The loss is already counted in
   !transcript-persistence-status and logged FATAL by persist-transcript-entry!;
   here it is additionally surfaced to follow-mode and then swallowed —
   instrumentation must not kill a live turn (2026-07-22: store outages
   aborted operator turns through this path, losing the turn AND the
   transcript; the ledger + visible line preserve loss-accounting instead)."
  [agent-id evidence-store entry]
  (try
    (persist-transcript-entry! evidence-store entry)
    (catch Throwable t
      (try
        (sink! agent-id {:type "text"
                         :text (str "[zai ✗ transcript not persisted: "
                                    (.getMessage t) "]")})
        (catch Throwable _ nil))
      nil)))

(defn- transcript-entry
  [{:keys [agent-id sid turn-id profile event body]}]
  {:evidence/id (str "e-" (UUID/randomUUID))
   :evidence/subject {:ref/type :agent :ref/id (str agent-id)}
   :evidence/type :coordination
   :evidence/claim-type :step
   :evidence/author (str agent-id)
   :evidence/session-id (str sid)
   :evidence/at (str (java.time.Instant/now))
   :evidence/tags [:transcript event profile]
   :evidence/body (merge {:event event
                          :turn-id (str turn-id)
                          :profile profile}
                         body)})

(defn- persist-turn-start!
  "Persist the exact model-facing PROMPT before a ZAI/ZAIF turn begins."
  [{:keys [evidence-store agent-id sid turn-id profile prompt]}]
  (persist-transcript-safely!
   agent-id evidence-store
   (transcript-entry
    {:agent-id agent-id :sid sid :turn-id turn-id :profile profile
     :event :turn-start
     :body {:prompt (str prompt)
            :prompt-chars (count (str prompt))}})))

(defn- persist-round!
  "Append one durable, turn-addressable round record.
CALLS contains maps of tool name, arguments, and result digest."
  [{:keys [evidence-store agent-id sid turn-id profile round text calls final?]}]
  (persist-transcript-safely!
   agent-id evidence-store
   (transcript-entry
    {:agent-id agent-id :sid sid :turn-id turn-id :profile profile
     :event :turn-round
     :body {:round round
            :final (boolean final?)
            :text (transcript-truncate text transcript-text-cap)
            :calls (vec calls)}})))

(defn- sha256-8 [s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (take 8 (.digest md (.getBytes s "UTF-8")))))))

(defn- emit-bug-records!
  "ZU-4 CI-in-the-loop: sweep the session's transcript for tool-call failures
   and emit each as a typed :bug/* event into the evidence store. Called from
   the par_punctuate path (session-end sweep). Failures are deduplicated by
   (tool, args-sha) with a count. Writes use the same fail-closed boundary as
   the transcript they summarize."
  [{:keys [agent-id sid turn-id profile evidence-store]}]
  (let [results (estore/query* evidence-store {:query/author (str agent-id)
                                                :query/session-id (str sid)
                                                :query/tags [:transcript]
                                                :query/limit 100})
        entries (if (map? results) (:items results) results)
        failed-calls (for [e entries
                           :let [calls (get-in e [:evidence/body :calls])]
                           call calls
                           :when (:error? call)]
                       call)
        grouped (reduce
                 (fn [m call]
                   (let [k [(:tool call) (sha256-8 (:args call))]]
                     (update m k
                             (fn [v]
                               (-> (or v call)
                                   (assoc :count (inc (long (:count v 0)))))))))
                 {} failed-calls)]
    (doseq [[[_tool _sha] bug] grouped]
      (persist-transcript-safely!
       agent-id evidence-store
       {:evidence/id (str "bug-" (UUID/randomUUID))
        :evidence/subject {:ref/type :agent :ref/id (str agent-id)}
        :evidence/type :coordination
        :evidence/claim-type :step
        :evidence/author (str agent-id)
        :evidence/session-id (str sid)
        :evidence/at (str (java.time.Instant/now))
        :evidence/tags [:bug :tool-failure profile]
        :evidence/body {:event :bug
                        :turn-id (some-> turn-id str)
                        :profile profile
                        :tool (:tool bug)
                        :args-sha (sha256-8 (:args bug))
                        :error (:error-text bug "unknown")
                        :count (:count bug 1)
                        :session-id (str sid)}}))))
(defn- transcript-calls
  "Zip tool-call details with their executed results into the persisted
   call records: full tool name + (truncated) args, digest of the result.
   ZU-4: failed calls carry :error? true and the verbatim error text."
  [details executed]
  (mapv (fn [detail ex]
          (let [error? (:error? ex)
                result-map (:result ex)]
            (cond-> {:tool (:name detail)
                     :args (transcript-truncate (pr-str (:input detail)) transcript-args-cap)
                     :result (transcript-digest (get-in ex [:message :content]))}
              error? (assoc :error? true
                            :error-text (transcript-truncate
                                         (str (or (:error result-map) "unknown error"))
                                         transcript-preview-cap)))))
        details executed))

(def ^:private tool-round-budget
  ;; 24 rounds: the original 8 demonstrably binds - the first real handoff
  ;; (M-custom-harness slice 2, 2026-07-04) exhausted all 8 on spec/source
  ;; reads and was cut off before writing anything.
  24)

(def ^:private default-auto-continue-max 8)

(defn- parse-nonnegative-int
  [x fallback]
  (try
    (let [n (cond
              (number? x) (long x)
              (some? x) (Long/parseLong (str/trim (str x)))
              :else fallback)]
      (max 0 n))
    (catch Throwable _
      fallback)))

(defn- configured-auto-continue-max
  [x]
  (parse-nonnegative-int
   (or x (getenv "FUTON3C_ZAI_AUTO_CONTINUE_MAX"))
   default-auto-continue-max))

(defn- auto-continue-message
  [n cap]
  (str "[harness auto-continue " n "/" cap
       ": round budget exhausted mid-task. Continue working toward the task set "
       "at the start of this turn. When the task is actually complete, reply "
       "with a final summary and make no tool calls.]"))

(defn- max-tool-rounds-result
  [sid final-text]
  {:result (if (str/blank? final-text)
             "[z.ai stopped after maximum tool rounds]"
             (str final-text "\n[z.ai stopped after maximum tool rounds]"))
   :session-id sid
   :error "max-tool-rounds"})

(defn- resolve-profile
  [profile]
  (zaif/env-profile (or profile (getenv "FUTON3C_ZAI_PROFILE"))))

(defn- default-zaif-inputs
  [{:keys [mission gamma observations task-belief c-belief]}]
  {:task-belief (or task-belief {})
   :c-belief (or c-belief {})
   :gamma (or gamma {})
   :mission mission
   :observations (or observations {})})

(defn- zaif-pairing-key
  "Build a deterministic pairing key for Z3a dual-constant entries.
   Shared across both entries from the same round so the scorer can
   mechanically pair them by querying for the key."
  [turn-id round]
  (str turn-id ":r" (or round 0)))

(defn- maybe-zaif-decision!
  "Compute and persist ZAIF arm decisions for this round.

   When hydrated inputs are available, records BOTH Z3a constants'
   decisions (shipped 0.65 + sweep 0.15) from the same inputs, mechanically
   paired via :pairing-key. Returns the shipped (primary) decision. The
   _decision binding in run-tool-rounds! stays unused — NO actuation.

   Any hydration error degrades to the empty-map default (never hurts a
   turn); persistence errors propagate (the loss-accounting contract)."
  [{:keys [profile zaif-inputs-fn agent-id sid evidence-store] :as ctx}]
  (when (= :zaif (resolve-profile profile))
    (let [inputs (try
                   (if zaif-inputs-fn
                     (zaif-inputs-fn ctx)
                     ;; D-1 hydrator is the default: real beliefs from the B1
                     ;; γ artifact + context text. Without it every live
                     ;; decide() sees empty maps and degenerates to :act.
                     (zaif-inputs/hydrate-inputs ctx))
                   (catch Throwable _
                     (default-zaif-inputs ctx)))
          pairing-key (zaif-pairing-key (:turn-id ctx) (:round ctx))
          dual-results (zaif/dual-decide inputs)]
      ;; Shadow instrumentation must never kill a live turn: a persistence
      ;; rejection (e.g. a store brown-out) is counted in persistence-status
      ;; by persist-decision! and surfaced in follow-mode, then swallowed.
      ;; The scorer only counts complete pairs, so a partial round drops out
      ;; of the cohort and shows up in the failure ledger — visible loss,
      ;; not a dead turn (incident 2026-07-22: futon1b memory-pressure
      ;; brown-out aborted an operator turn through this path).
      (try
        (doseq [{:keys [label operator-attention-cost decision]} dual-results]
          (zaif/persist-decision! {:agent-id agent-id
                                   :sid sid
                                   :turn-id (:turn-id ctx)
                                   :round (:round ctx)
                                   :evidence-store evidence-store
                                   :decision decision
                                   :inputs inputs
                                   :constant operator-attention-cost
                                   :constant-label label
                                   :pairing-key pairing-key}))
        (catch Throwable t
          (try
            (sink! agent-id {:type "text"
                             :text (str "[zaif ✗ decision not persisted: "
                                        (.getMessage t) "]")})
            (catch Throwable _ nil))))
      ;; Return the shipped (primary) decision for any callers that read it.
      (:decision (first dual-results)))))

(defn run-tool-rounds!
  "Run one logical Z.AI turn. Kept as a top-level var so a namespace reload can
   update already-registered invoke closures."
  [{:keys [client opts api-key !messages backend tool-opts agent-id sid
           !repeats auto-continue-max] :as ctx}]
  (let [auto-continue-max (configured-auto-continue-max auto-continue-max)]
    (loop [remaining tool-round-budget
           final-text ""
           auto-continues 0
           mid-work? false
           round-n 1]
      (if (zero? remaining)
        (if (and mid-work? (< auto-continues auto-continue-max))
          (let [n (inc auto-continues)
                text (auto-continue-message n auto-continue-max)]
            (swap! !messages conj {:role "user" :content text})
            (sink! agent-id {:type "text" :text (str "[auto-continue " n "/" auto-continue-max "]")})
            (recur tool-round-budget final-text n false round-n))
          (max-tool-rounds-result sid final-text))
        (let [_decision (maybe-zaif-decision!
                          (assoc ctx
                                 :round round-n
                                 :context (some->> @!messages
                                                   (filter #(= (:role %) "user"))
                                                   last
                                                   :content)))
              resp (chat! client (assoc opts :api-key api-key) @!messages)
              err (:error resp)]
          (if err
            {:result nil
             :session-id sid
             :error (result-string err)}
            (let [message (get-in resp [:choices 0 :message])
                  text (assistant-text message)
                  tool-calls (seq (:tool_calls message))]
              (swap! !messages conj message)
              (when-not (str/blank? text)
                (sink! agent-id {:type "text" :text text}))
              (if tool-calls
                ;; A tool exception must NEVER kill the turn: feed the error
                ;; back as the tool result so the model can correct (found live
                ;; 2026-07-04: a nil :path arg NPE'd through resolve-path and
                ;; destroyed a 37-event turn mid-flight).
                (let [executed (mapv (fn [tc]
                                       (detect-stuck!
                                        !repeats tc
                                        (try (execute-tool backend
                                                           (assoc tool-opts
                                                                  :turn-id (:turn-id ctx)
                                                                  :round round-n
                                                                  :profile (:profile ctx)
                                                                  ;; the live session id; tool-opts'
                                                                  ;; session-id-atom is nil on this
                                                                  ;; path (found live 2026-07-22:
                                                                  ;; nil killed every memory_record)
                                                                  :session-id-fallback sid)
                                                           tc)
                                             (catch Throwable t
                                               (let [d (tool-call-detail
                                                        tc
                                                        (parse-arguments
                                                         (get-in tc [:function :arguments])))]
                                                 {:detail d
                                                  :error? true
                                                  :result {:ok false :error (.getMessage t)}
                                                  :message {:role "tool"
                                                            :tool_call_id (:id tc)
                                                            :name (get-in tc [:function :name])
                                                            :content (str "TOOL ERROR (turn continues): "
                                                                          (.getName (class t)) ": "
                                                                          (or (.getMessage t) "no message")
                                                                          " - check argument names/values and retry")}})))))
                                     tool-calls)
                      details (mapv :detail executed)
                      results (mapv (fn [{:keys [detail message]}]
                                      {:tool_use_id (:id detail)
                                       :content (:content message)})
                                    executed)]
                  (sink! agent-id {:type "tool_use"
                                   :tools (mapv :name details)
                                   :tool_details details})
                  (sink! agent-id {:type "tool_result"
                                   :results results})
                  ;; ZU-4: surface tool errors verbatim in follow-mode display
                  (doseq [{:keys [detail error? result]} executed
                          :when error?]
                    (sink! agent-id {:type "text"
                                     :text (str "[" (:name detail) " \u2717 "
                                                (transcript-truncate
                                                 (str (or (:error result) "unknown error"))
                                                 transcript-preview-cap)
                                                "]")}))
                  (persist-round! {:evidence-store (:evidence-store ctx)
                                   :agent-id agent-id :sid sid
                                   :turn-id (:turn-id ctx) :profile (:profile ctx)
                                   :round round-n
                                   :text text
                                   :calls (transcript-calls details executed)})
                  (swap! !messages into (mapv :message executed))
                  (recur (dec remaining) (str final-text text) auto-continues true (inc round-n)))
                (do
                  (persist-round! {:evidence-store (:evidence-store ctx)
                                   :agent-id agent-id :sid sid
                                   :turn-id (:turn-id ctx) :profile (:profile ctx)
                                   :round round-n
                                   :text (if (str/blank? text) final-text text)
                                   :calls [] :final? true})
                  {:result (if (str/blank? text) final-text text)
                   :session-id sid})))))))))

(defn make-invoke-fn
  "Return an Agency invoke-fn backed by Z.AI tool calling."
  [{:keys [agent-id session-file session-id-atom initial-session-id cwd evidence-store
           api-key base-url model timeout-ms max-tokens temperature irc-send-fn irc-recent-fn
           memory-mode memory-domain auto-continue-max profile zaif-inputs-fn]
    :or {agent-id "zai" timeout-ms 300000 memory-mode :full}}]
  (when-not evidence-store
    (throw (ex-info "ZAI/ZAIF requires a durable evidence store"
                    {:agent-id agent-id})))
  (let [client (HttpClient/newHttpClient)
        key (or api-key (resolve-api-key))
        cwd* (or cwd (System/getProperty "user.dir"))
        sid0 (or initial-session-id
                 (when (and session-file (.exists (io/file session-file)))
                   (some-> session-file slurp str/trim not-empty))
                 (str "zai-" (UUID/randomUUID)))
        !session-id (or session-id-atom (atom sid0))
        backend (real-backend/make-real-backend
                 {:cwd cwd*
                  :timeout-ms 30000
                  :evidence-store evidence-store
                  :memory-domain (or memory-domain :zaif-work)
                  :memory-recall-limit 3
                  :agent-id agent-id
                  :session-id-fn #(some-> @!session-id str)})
        !messages (atom [{:role "system"
                          :content (str "You are " agent-id ", an agentic coding assistant running inside Joe's Futon Agency. "
                                        "Use tools to inspect files, edit files, run commands, inspect the JVM, and talk on IRC when asked. "
                                        "Do not claim to have inspected or changed anything unless you used tools or the user supplied the content. "
                                        "Prefer small, focused tool calls and summarize concrete results. "
                                        "Your boot context (appended below at first use) is a snapshot from session start; refresh with boot_context when you need current state. "
                                        "Tool results that quote the record — pattern candidates, PSR/PUR/PAR proofs — are recorded, not necessarily current: when you rely on one, cite its id. "
                                        "For live state (what is running, what is dirty, who is active) use live tools, never remembered results. "
                                        "Never present a single remembered entry as a standing operator preference; standing preferences arrive explicitly tagged. "
                                        "When you adopt a library pattern for non-trivial work, record it: psr_search then psr_select before applying, pur_update after, and par_punctuate when a work session ends. "
                                        "When you learn something durable and reusable — an approach failed for a stateable reason, a replacement worked, a tool or API behaves contrary to expectation — record it with memory_record at the moment of the realization, not at session end. "
                                        "To message another agent in the Agency, send a bell via run_shell: "
                                        "python3 /home/joe/code/futon3c/scripts/agency_send.py --from " agent-id " --to <agent> --kind bell "
                                        "with the message on stdin (a quoted heredoc is safest). Always pass --from " agent-id " so the reply can route back to you. "
                                        "When you finish work another agent belled you about, bell them back with a summary — do not rely on the automatic completion bell alone. "
                                        "Send required bells IMMEDIATELY when the checkpoint is reached — the bell is part of the checkpoint, not an epilogue. "
                                        "Never end a turn with a bell announced but unsent: your turn ends when you stop calling tools, so stated intent does not execute. "
                                        "'Announced' is not 'sent'. "
                                        "Symmetrically: instructions that arrive in the reply/auto-bellback "
                                        "to YOUR OWN bell are operative dispatches — act on them in that turn; "
                                        "never park 'holding for a bell' that the reply already was. "
                                        "Self-marking (M-points-de-fuite): when you genuinely reverse course — "
                                        "an approach abandoned, a wrong assumption caught — put ✘ in your narration "
                                        "at the point of reversal; long form (✘ :ref <target> \"why\") when the "
                                        "referent matters. When you mint an idea worth exploring later but out of "
                                        "scope now, mark it 💡. These glyphs are parsed from your narration into "
                                        "the evidence record. Mark real events only, at most a few per turn: an "
                                        "unmarked correction is better than a marked non-correction, and marks "
                                        "emitted to look thorough poison the record.")}])
        !booted (atom false)
        opts {:base-url (or base-url (getenv "ZAI_BASE_URL") default-base-url)
              :model (or model (getenv "ZAI_MODEL") default-model)
              :max-tokens max-tokens
              :temperature temperature
              :timeout-ms timeout-ms
              :memory-mode memory-mode}
        profile* (resolve-profile profile)
        tool-opts {:irc-send-fn irc-send-fn
                   :irc-recent-fn irc-recent-fn
                   :agent-id agent-id
                   :cwd cwd*
                   :evidence-store evidence-store
                   :memory-domain (or memory-domain :zaif-work)
                   :session-id-atom !session-id}]
    (fn [prompt incoming-session-id]
      (let [key* (or key (resolve-api-key))
            sid (or incoming-session-id @!session-id sid0)
            turn-id (str "zai-turn-" (UUID/randomUUID))
            ;; stuck-means-signal detector (mistakes-ledger §11): consecutive
            ;; identical tool calls with identical results get a warning
            ;; injected at 3 and a stop-and-bell instruction at 5. Fresh per
            ;; turn — repetition across turns is the reviewer's watch.
            !repeats (atom {:s nil :n 0})]
        (if-not key*
          {:result nil
           :session-id sid
           :error "Z.AI API key missing; set ZAI_API_KEY or create ~/.zaikey or ~/.zai-key"}
          (do
        (reset! !session-id sid)
        (when session-file (spit session-file sid))
        (when (compare-and-set! !booted false true)
          ;; §8.4 condition gating: :none skips the boot packet (condition
          ;; a); :files and :full get it (conditions b, c). Rehydration is
          ;; ledger memory, so :full only.
          (when-not (= :none memory-mode)
            (try
              (let [packet (memory-backend/boot-packet-string
                            {:agent-id agent-id :session-id sid :cwd cwd*})]
                (when-not (str/blank? packet)
                  (swap! !messages update-in [0 :content] str "\n\n" packet)))
              (catch Throwable _)))
          ;; D-7: if this session id has prior turns in the ledger (resumed
          ;; identity, e.g. post-crash), inject the condensed record.
          (when (= :full memory-mode)
            (try
              (let [rehydration (memory-backend/rehydration-string
                                 {:agent-id agent-id :session-id sid :limit 10})]
                (when-not (str/blank? rehydration)
                  (swap! !messages update-in [0 :content] str "\n\n" rehydration)))
              (catch Throwable _))))
        (persist-turn-start! {:evidence-store evidence-store
                              :agent-id agent-id
                              :sid sid
                              :turn-id turn-id
                              :profile profile*
                              :prompt prompt})
        (swap! !messages conj {:role "user" :content (str prompt)})
        (run-tool-rounds! {:client client
                           :opts opts
                           :api-key key*
                           :!messages !messages
                           :backend backend
                           :tool-opts tool-opts
                           :agent-id agent-id
                           :sid sid
                           :turn-id turn-id
                           :evidence-store evidence-store
                           :!repeats !repeats
                           :profile profile*
                           :zaif-inputs-fn zaif-inputs-fn
                           :auto-continue-max auto-continue-max})))))))
