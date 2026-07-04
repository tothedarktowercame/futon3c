(ns futon3c.agents.zai-api
  "Small Z.AI API-backed coding harness.

   Z.AI supplies the model and OpenAI-style tool calls; this namespace supplies
   the local agent loop and delegates real work to futon3c.peripheral.real-backend."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
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
                 ["text"])}])

(defn- openai-tools []
  (mapv (fn [{:keys [name description parameters]}]
          {:type "function"
           :function {:name name
                      :description description
                      :parameters parameters}})
        tool-specs))

(defn- parse-arguments [s]
  (cond
    (map? s) s
    (str/blank? (str s)) {}
    :else (try
            (json/parse-string (str s) true)
            (catch Throwable _
              {}))))

(defn- result-string [x]
  (let [s (if (string? x) x (json/generate-string x))]
    (if (> (count s) 12000)
      (str (subs s 0 12000) "\n...[truncated " (- (count s) 12000) " chars]")
      s)))

(defn- tool-call-detail [tool-call args]
  {:id (:id tool-call)
   :name (get-in tool-call [:function :name])
   :input args})

(defn- execute-tool
  [backend {:keys [irc-send-fn irc-recent-fn agent-id]} tool-call]
  (let [name (get-in tool-call [:function :name])
        args (parse-arguments (get-in tool-call [:function :arguments]))
        fail (fn [msg] {:ok false :error msg})
        result
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

          (fail (str "Unknown tool: " name)))]
    {:detail (tool-call-detail tool-call args)
     :message {:role "tool"
               :tool_call_id (:id tool-call)
               :name name
               :content (result-string result)}
     :result result}))

(defn- assistant-text [message]
  (let [content (:content message)
        reasoning (:reasoning_content message)]
    (cond
      (and (string? content) (not (str/blank? content))) content
      (and (string? reasoning) (not (str/blank? reasoning))) reasoning
      :else "")))

(defn- chat!
  [client {:keys [api-key base-url model max-tokens temperature timeout-ms]} messages]
  (let [body (json/generate-string
              (cond-> {:model (or model default-model)
                       :messages messages
                       :tools (openai-tools)
                       :tool_choice "auto"
                       :max_tokens (or max-tokens 4096)
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

(defn make-invoke-fn
  "Return an Agency invoke-fn backed by Z.AI tool calling."
  [{:keys [agent-id session-file session-id-atom initial-session-id cwd evidence-store
           api-key base-url model timeout-ms max-tokens temperature irc-send-fn irc-recent-fn]
    :or {agent-id "zai" timeout-ms 300000}}]
  (let [client (HttpClient/newHttpClient)
        key (or api-key (resolve-api-key))
        backend (real-backend/make-real-backend
                 {:cwd (or cwd (System/getProperty "user.dir"))
                  :timeout-ms 30000
                  :evidence-store evidence-store})
        sid0 (or initial-session-id
                 (when (and session-file (.exists (io/file session-file)))
                   (some-> session-file slurp str/trim not-empty))
                 (str "zai-" (UUID/randomUUID)))
        !session-id (or session-id-atom (atom sid0))
        !messages (atom [{:role "system"
                          :content (str "You are " agent-id ", an agentic coding assistant running inside Joe's Futon Agency. "
                                        "Use tools to inspect files, edit files, run commands, inspect the JVM, and talk on IRC when asked. "
                                        "Do not claim to have inspected or changed anything unless you used tools or the user supplied the content. "
                                        "Prefer small, focused tool calls and summarize concrete results.")}])
        opts {:base-url (or base-url (getenv "ZAI_BASE_URL") default-base-url)
              :model (or model (getenv "ZAI_MODEL") default-model)
              :max-tokens max-tokens
              :temperature temperature
              :timeout-ms timeout-ms}
        tool-opts {:irc-send-fn irc-send-fn
                   :irc-recent-fn irc-recent-fn
                   :agent-id agent-id}]
    (fn [prompt incoming-session-id]
      (let [key* (or key (resolve-api-key))
            sid (or incoming-session-id @!session-id sid0)]
        (if-not key*
          {:result nil
           :session-id sid
           :error "Z.AI API key missing; set ZAI_API_KEY or create ~/.zaikey or ~/.zai-key"}
          (do
        (reset! !session-id sid)
        (when session-file (spit session-file sid))
        (swap! !messages conj {:role "user" :content (str prompt)})
        (loop [remaining 8
               final-text ""]
          (if (zero? remaining)
            {:result (if (str/blank? final-text)
                       "[z.ai stopped after maximum tool rounds]"
                       final-text)
             :session-id sid
             :error (when (str/blank? final-text) "max-tool-rounds")}
            (let [resp (chat! client (assoc opts :api-key key*) @!messages)
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
                    (let [executed (mapv #(execute-tool backend tool-opts %) tool-calls)
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
                      (swap! !messages into (mapv :message executed))
                      (recur (dec remaining) (str final-text text)))
                    {:result (if (str/blank? text) final-text text)
                     :session-id sid}))))))))))))
