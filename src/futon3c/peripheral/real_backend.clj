(ns futon3c.peripheral.real-backend
  "Real tool backend — actual file I/O, search, and command execution.

   This is the keystone piece that makes peripherals alive. Each tool-id
   maps to real operations on the filesystem and system. The peripheral
   spec enforces which tools are available and what scope they operate in;
   the backend just executes faithfully.

   No subprocess spawning of Claude or Codex — the point of peripherals
   is that the same agent continues across mode changes. Context stays,
   capabilities change.

   Config:
     :cwd        — working directory for relative paths (default: user.dir)
     :timeout-ms — default command timeout in milliseconds (default: 30000)
     :evidence-store — atom or EvidenceBackend for :musn-log tool (optional)"
  (:require [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as str]
            [futon.notions :as notions]
            [futon3.gate.shapes :as gate-shapes]
            [futon3b.query.relations :as relations]
            [futon2.aif.memory-contract :as memory-contract]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-recall :as memory-recall]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.peripheral.tools :as tools]
            [futon3c.reflection.core :as reflection])
  (:import [java.io File]
           [java.nio.file FileSystems Path]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent TimeUnit]))

;; =============================================================================
;; File tools
;; =============================================================================

(defn- resolve-path
  "Resolve a path relative to cwd. Absolute paths pass through.
   nil path throws a clear error (io/file nil silently returns nil and the
   NPE surfaces far away — found live 2026-07-04, a nil :path tool arg)."
  ^File [cwd path]
  (when (nil? path)
    (throw (ex-info "path argument required (got nil) — check the tool's argument names" {})))
  (let [f (io/file path)]
    (if (.isAbsolute f)
      f
      (io/file cwd path))))

(defn- tool-read
  "Read file contents. Args: [file-path] or [file-path {:offset N :limit N}]."
  [cwd args]
  (let [file-path (first args)
        opts (when (map? (second args)) (second args))
        f (resolve-path cwd file-path)]
    (if-not (.exists f)
      {:ok false :error (str "File not found: " file-path)}
      (if-not (.isFile f)
        {:ok false :error (str "Not a file: " file-path)}
        (let [lines (str/split-lines (slurp f))
              offset (or (:offset opts) 0)
              limit (or (:limit opts) (count lines))
              selected (->> lines (drop offset) (take limit) (str/join "\n"))]
          {:ok true :result selected})))))

(defn- tool-glob
  "Find files by glob pattern. Args: [pattern] or [pattern base-dir]."
  [cwd args]
  (let [pattern (first args)
        base (if (second args)
               (resolve-path cwd (second args))
               (io/file cwd))
        base-path (.toPath base)
        matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern))
        results (atom [])]
    (when (.exists base)
      (doseq [^File f (file-seq base)
              :when (.isFile f)]
        (let [^Path rel (try (.relativize base-path (.toPath f))
                              (catch Exception _ nil))]
          (when (and rel (.matches matcher rel))
            (swap! results conj (.getPath ^File f))))))
    {:ok true :result @results}))

(def ^:private grep-skip-dirs
  "Directory names never descended into by tool-grep (unless explicitly the
   search base). Found live 2026-07-04: a whole-workspace search slurped a
   279 MB CUDA .so from .venv and RocksDB .sst store files for ~50 minutes,
   wedging the agent's turn drainer."
  #{"node_modules" "storage" "target" "out" "dist" ".venv"})

(def ^:private grep-max-file-bytes (* 2 1024 1024))
(def ^:private grep-time-budget-ms 30000)

(defn- tool-grep
  "Search file contents by regex. Args: [pattern] or [pattern path] or [pattern path opts].
   opts: {:case-insensitive bool, :max-matches N}.
   Bounded: prunes dot/vendor/store directories, skips files > 2 MB, and
   stops at a 30 s budget — truncation is reported as a sentinel match, not
   silently."
  [cwd args]
  (let [pattern-str (first args)
        search-path (or (second args) (str cwd))
        opts (when (map? (nth args 2 nil)) (nth args 2))
        flags (if (:case-insensitive opts)
                java.util.regex.Pattern/CASE_INSENSITIVE
                0)
        re (java.util.regex.Pattern/compile pattern-str flags)
        max-matches (or (:max-matches opts) 500)
        base (resolve-path cwd search-path)
        deadline (+ (System/currentTimeMillis) grep-time-budget-ms)
        skip-dir? (fn [^File d]
                    (let [n (.getName d)]
                      (or (str/starts-with? n ".")
                          (contains? grep-skip-dirs n))))
        results (atom [])
        done? (atom false)
        timed-out? (atom false)
        over? (fn []
                (or @done?
                    (.isInterrupted (Thread/currentThread))
                    (when (> (System/currentTimeMillis) deadline)
                      (reset! timed-out? true)
                      true)))]
    (when (.exists base)
      (let [files (if (.isFile base)
                    [base]
                    (->> (tree-seq
                          (fn [^File f]
                            (and (.isDirectory f)
                                 (or (= f base) (not (skip-dir? f)))))
                          (fn [^File f] (seq (.listFiles f)))
                          base)
                         (filter #(.isFile ^File %))
                         (remove #(str/starts-with? (.getName ^File %) "."))
                         (remove #(> (.length ^File %) grep-max-file-bytes))))]
        (doseq [^File f files
                :while (not (over?))]
          (try
            (let [content (slurp f)
                  lines (str/split-lines content)]
              (doseq [[idx line] (map-indexed vector lines)
                      :while (not (over?))]
                (when (.find (.matcher re line))
                  (swap! results conj {:file (.getPath f)
                                       :line (inc idx)
                                       :content (subs line 0 (min (count line) 500))})
                  (when (>= (count @results) max-matches)
                    (reset! done? true)))))
            (catch Exception _)))))
    {:ok true
     :result (if @timed-out?
               (conj @results
                     {:file "[search-truncated]" :line 0
                      :content (str "search stopped at the " (/ grep-time-budget-ms 1000)
                                    "s budget — results may be incomplete; "
                                    "narrow the path or pattern")})
               @results)}))

(defn- tool-edit
  "Edit file contents (find/replace). Args: [file-path old-string new-string]."
  [cwd args]
  (let [file-path (first args)
        old-str (second args)
        new-str (nth args 2)
        f (resolve-path cwd file-path)]
    (cond
      (not (.exists f))
      {:ok false :error (str "File not found: " file-path)}

      (not (string? old-str))
      {:ok false :error "old-string must be a string"}

      (not (string? new-str))
      {:ok false :error "new-string must be a string"}

      :else
      (let [content (slurp f)]
        (if-not (str/includes? content old-str)
          {:ok false :error (str "old-string not found in " file-path)}
          (do
            (spit f (str/replace-first content old-str new-str))
            {:ok true :result {:file (.getPath f)
                               :replacements 1}}))))))

(defn- tool-write
  "Write file contents. Args: [file-path content]."
  [cwd args]
  (let [file-path (first args)
        content (second args)
        f (resolve-path cwd file-path)]
    (when-let [parent (.getParentFile f)]
      (.mkdirs parent))
    (spit f content)
    {:ok true :result {:file (.getPath f)
                       :bytes (count content)}}))

;; =============================================================================
;; Command tools
;; =============================================================================

(defn- run-command
  "Execute a shell command and return {:exit N :out string :err string}."
  [cwd command timeout-ms]
  (let [pb (ProcessBuilder. ^java.util.List ["bash" "-c" command])
        _ (.directory pb (io/file cwd))
        _ (.redirectErrorStream pb false)
        proc (.start pb)
        out-future (future (slurp (.getInputStream proc)))
        err-future (future (slurp (.getErrorStream proc)))
        finished? (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)]
    (if finished?
      (let [exit (.exitValue proc)
            out @out-future
            err @err-future]
        {:exit exit :out out :err err})
      (do
        ;; Kill the WHOLE process tree, descendants first. destroyForcibly
        ;; on the bash child alone orphans grandchildren — found live
        ;; 2026-07-04: timed-out `clojure -M:node` runs left full XTDB/Arrow
        ;; JVMs running invisibly (default heap = 1/4 RAM each), building
        ;; the memory pressure implicated in both serving-JVM deaths.
        (doseq [h (iterator-seq (.iterator (.descendants (.toHandle proc))))]
          (try (.destroyForcibly ^java.lang.ProcessHandle h)
               (catch Throwable _)))
        (.destroyForcibly proc)
        {:exit -1 :out ""
         :err (str "Command timed out after " timeout-ms "ms"
                   " (process tree killed; pass {\"timeout_ms\": N} for long runs)")}))))

(defn- tool-bash
  "Execute a bash command. Args: [command] or [command {:timeout-ms N}].
   Used by :bash, :bash-test, :bash-git, :bash-deploy."
  [cwd default-timeout args]
  (let [command (first args)
        opts (when (map? (second args)) (second args))
        timeout (or (:timeout-ms opts) default-timeout)]
    (if (str/blank? command)
      {:ok false :error "Command must be a non-blank string"}
      (let [result (run-command cwd command timeout)]
        (if (zero? (:exit result))
          {:ok true :result result}
          {:ok true :result result})))))

(defn- tool-bash-readonly
  "Execute a read-only bash command. Rejects obviously destructive commands.
   Args: [command] or [command {:timeout-ms N}]."
  [cwd default-timeout args]
  (let [command (str (first args))
        destructive-patterns [#"(?:^|\s|;|&&|\|\|)\s*rm\s"
                              #"(?:^|\s|;|&&|\|\|)\s*mv\s"
                              #"(?:^|\s|;|&&|\|\|)\s*cp\s.*>\s"
                              #">\s*/"
                              #"(?:^|\s|;|&&|\|\|)\s*dd\s"
                              #"(?:^|\s|;|&&|\|\|)\s*chmod\s"
                              #"(?:^|\s|;|&&|\|\|)\s*chown\s"
                              #"(?:^|\s|;|&&|\|\|)\s*kill\s"]]
    (if (some #(re-find % command) destructive-patterns)
      {:ok false :error "Command rejected: appears destructive (readonly peripheral)"}
      (tool-bash cwd default-timeout args))))

;; =============================================================================
;; HTTP tool
;; =============================================================================

(defn- tool-web-fetch
  "Fetch a URL. Args: [url] or [url {:method :get}]."
  [args]
  (let [url-str (first args)]
    (if (str/blank? url-str)
      {:ok false :error "URL must be non-blank"}
      (try
        (let [url (java.net.URI. url-str)
              conn (.openConnection (.toURL url))]
          (when (instance? java.net.HttpURLConnection conn)
            (.setRequestMethod ^java.net.HttpURLConnection conn "GET")
            (.setConnectTimeout ^java.net.HttpURLConnection conn 10000)
            (.setReadTimeout ^java.net.HttpURLConnection conn 10000))
          (let [content (slurp (.getInputStream conn))
                status (when (instance? java.net.HttpURLConnection conn)
                         (.getResponseCode ^java.net.HttpURLConnection conn))]
            {:ok true :result {:status (or status 200)
                               :body content}}))
        (catch Exception e
          {:ok false :error (str "Fetch failed: " (.getMessage e))})))))

;; =============================================================================
;; Evidence tool
;; =============================================================================

(defn- tool-musn-log
  "Read from the evidence store. Args: [session-id] or [{:query ...}].
   Requires :evidence-store in backend config."
  [evidence-store args]
  (if (nil? evidence-store)
    {:ok false :error "No evidence store configured for musn-log"}
    (let [query-arg (first args)
          all (estore/query* evidence-store {:query/include-ephemeral? true})
          entries (if (string? query-arg)
                    (->> all
                         (filter #(= query-arg (:evidence/session-id %)))
                         (sort-by :evidence/at)
                         vec)
                    (->> all
                         (sort-by :evidence/at)
                         vec))]
      {:ok true :result entries})))

;; =============================================================================
;; Reflection tools (Clojure runtime introspection)
;; =============================================================================

(defn- tool-reflect-namespaces
  "List loaded namespaces. Args: [] or [pattern-string]."
  [args]
  (try
    (let [result (if-let [pattern (first args)]
                   (reflection/list-namespaces pattern)
                   (reflection/list-namespaces))]
      {:ok true :result result})
    (catch Exception e
      {:ok false :error (str "reflect-namespaces failed: " (.getMessage e))})))

(defn- tool-reflect-ns
  "Public vars in a namespace. Args: [ns-sym-or-string]."
  [args]
  (let [ns-sym (symbol (str (first args)))]
    (let [result (reflection/reflect-ns ns-sym)]
      (if (:error result)
        {:ok false :error (:error result)}
        {:ok true :result result}))))

(defn- tool-reflect-var
  "Full metadata for a var. Args: [ns-sym var-sym] or [\"ns/var\"]."
  [args]
  (let [[ns-sym var-sym] (if (= 1 (count args))
                           (let [fqn (str (first args))
                                 idx (.lastIndexOf fqn "/")]
                             (if (pos? idx)
                               [(symbol (subs fqn 0 idx))
                                (symbol (subs fqn (inc idx)))]
                               [nil nil]))
                           [(symbol (str (first args)))
                            (symbol (str (second args)))])]
    (if (and ns-sym var-sym)
      (let [result (reflection/reflect-var ns-sym var-sym)]
        (if (:error result)
          {:ok false :error (:error result)}
          {:ok true :result result}))
      {:ok false :error "Expected [ns var] or [\"ns/var\"]"})))

(defn- tool-reflect-deps
  "Namespace dependency graph. Args: [ns-sym-or-string]."
  [args]
  (let [ns-sym (symbol (str (first args)))]
    (let [result (reflection/reflect-deps ns-sym)]
      (if (:error result)
        {:ok false :error (:error result)}
        {:ok true :result result}))))

(defn- tool-reflect-java-class
  "Reflect on a Java class. Args: [class-name-string]."
  [args]
  (let [class-name (str (first args))]
    (let [result (reflection/reflect-java-class class-name)]
      (if (:error result)
        {:ok false :error (:error result)}
        {:ok true :result result}))))

;; =============================================================================
;; Discipline tools (futon3a + futon3b adapters)
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(defn- gen-id [prefix]
  (str prefix "-" (UUID/randomUUID)))

(defn- parse-int
  [x default]
  (try
    (let [n (Integer/parseInt (str x))]
      (if (pos? n) n default))
    (catch Exception _ default)))

(defn- normalize-pattern-id
  "Normalize input to canonical string pattern-id (no leading colon)."
  [x]
  (cond
    (keyword? x) (subs (str x) 1)
    (string? x)
    (let [s (str/trim x)
          s (if (str/starts-with? s ":") (subs s 1) s)]
      (when-not (str/blank? s) s))
    :else nil))

(defn- tokenize-query
  [text]
  (->> (str/split (str/lower-case (or text "")) #"[^a-z0-9._-]+")
       (remove str/blank?)
       (remove #(< (count %) 3))
       set))

(defn- score-index-entry
  [tokens entry]
  (let [hotwords (->> (:hotwords entry)
                      (map str/lower-case)
                      set)
        rationale-tokens (tokenize-query (:rationale entry))
        overlap (count (cset/intersection tokens hotwords))
        rationale-overlap (count (cset/intersection tokens rationale-tokens))]
    (+ (* 2.0 overlap) (* 0.5 rationale-overlap))))

(defn- default-notions-index-path
  [cwd]
  (let [candidates [(str (io/file cwd "../futon3a/resources/notions/patterns-index.tsv"))
                    (str (io/file cwd "futon3a/resources/notions/patterns-index.tsv"))
                    (str (io/file (System/getProperty "user.home")
                                  "code/futon3a/resources/notions/patterns-index.tsv"))]]
    (some (fn [p]
            (let [f (io/file p)]
              (when (.exists f) (.getPath f))))
          candidates)))

(defn- notions-index-path
  [cwd config]
  (let [configured (or (:notions-index-path config)
                       (System/getenv "FUTON3A_NOTIONS_INDEX"))]
    (or (when (and configured (.exists (io/file configured)))
          configured)
        (default-notions-index-path cwd))))

(defn- normalize-pur-outcome
  [outcome-or-status]
  (let [v (if (keyword? outcome-or-status)
            (name outcome-or-status)
            (str/lower-case (str (or outcome-or-status ""))))]
    (if (contains? #{"pass" "ok" "success" "true" "1"} v)
      :pass
      :fail)))

(defn- normalize-prediction-error
  [prediction-error]
  (cond
    (map? prediction-error) prediction-error
    (and (string? prediction-error)
         (not (str/blank? prediction-error)))
    {:description prediction-error}
    :else nil))

(defn- persist-proof-path!
  "Persist a proof-path to EDN and, when an evidence-store is available,
   also append a synthetic evidence entry so the proof-path is queryable
   from the evidence landscape (Seam 3: futon3b proof-paths → futon3c evidence)."
  [events evidence evidence-store]
  (let [proof-path {:path/id (gen-id "path")
                    :events (vec events)}
        persisted (relations/append-proof-path! {:proof-path proof-path
                                                 :evidence evidence})
        result {:proof-path/id (:path/id proof-path)
                :proof-path/file (:file persisted)}]
    ;; Bridge: append synthetic evidence entry to the evidence landscape
    (when evidence-store
      ;; Punctuator identity (M-custom-harness §13.5c): the bridged body
      ;; strips gate records to ids, which made agents unable to recognize
      ;; their own PARs in the shared record. Carry the session-ref and the
      ;; discipline kind explicitly.
      (let [session-ref (or (get-in evidence [:par :par/session-ref])
                            (get-in evidence [:psr :psr/session-ref]))
            kind (cond
                   (:par evidence) :par
                   (:pur evidence) :pur
                   (:psr evidence) :psr
                   :else :proof-path)
            entry (cond-> {:evidence/id (str "e-pp-" (:path/id proof-path))
                           :evidence/subject {:ref/type :proof-path
                                              :ref/id (:path/id proof-path)}
                           :evidence/type :coordination
                           :evidence/claim-type :observation
                           :evidence/author "gate-pipeline"
                           :evidence/at (str (Instant/now))
                           :evidence/body (cond-> {:event :proof-path-persisted
                                                   :path/id (:path/id proof-path)
                                                   :path/file (:file persisted)
                                                   :gate-events (mapv :gate/id events)
                                                   :discipline-kind kind}
                                            session-ref (assoc :session-ref session-ref))
                           :evidence/tags [:proof-path :gate-traversal]}
                    session-ref (assoc :evidence/session-id session-ref))
            append-result (boundary/append! evidence-store entry)]
        (when (and (map? append-result) (contains? append-result :error/code))
          (println "[WARN] proof-path evidence append failed:" (:error/message append-result)))))
    result))

(defn- tool-psr-search
  [cwd config args]
  (let [query (some-> (first args) str str/trim)
        opts (if (map? (second args)) (second args) {})
        top-k (parse-int (:top-k opts) 5)
        include-details? (true? (:include-details opts))
        idx-path (notions-index-path cwd config)]
    (cond
      (str/blank? query)
      {:ok false :error "psr-search requires a non-blank query"}

      (nil? idx-path)
      {:ok false :error "Could not locate futon3a notions index (set FUTON3A_NOTIONS_INDEX)"}

      :else
      (try
        (let [tokens (tokenize-query query)
              index (notions/load-pattern-index idx-path)
              candidate->result
              (fn [entry]
                (let [score (score-index-entry tokens entry)
                      pid (:id entry)]
                  (cond-> {:pattern-id pid
                           :score score
                           :rationale (:rationale entry)
                           :sigil (:sigil entry)
                           :hotwords (vec (:hotwords entry))
                           :exists-in-library? (relations/pattern-exists? pid)
                           :source :futon3a/notions-index}
                    include-details?
                    (assoc :details (notions/get-pattern-details pid)))))
              scored-by-id
              (into {} (map (fn [entry]
                              [(:id entry) (candidate->result entry)]))
                    index)
              lexical-candidates
              (->> (vals scored-by-id)
                   (filter #(pos? (:score %))))
              proposal-fn
              (or (:memory-proposal-fn config)
                  memory-recall/propose-patterns-by-query)
              proposal
              (if-let [domain (:memory-domain config)]
                (try
                  (proposal-fn
                   {:domain domain}
                   query
                   {:limit (or (:memory-proposal-limit config) 10)})
                  (catch Throwable t
                    {:ok false
                     :candidates []
                     :error {:error/code :memory-proposal-unavailable
                             :error/message (or (.getMessage t)
                                                "Memory proposal lane failed")}}))
                {:ok false :candidates []})
              proposed-by-id
              (into {}
                    (keep
                     (fn [{:keys [pattern-id memory-support source]}]
                       (when-let [base (get scored-by-id pattern-id)]
                         [pattern-id
                          (assoc base
                                 :memory-proposal-source source
                                 :memory-proposal-support memory-support)])))
                    (:candidates proposal))
              candidates
              (->> (concat lexical-candidates (vals proposed-by-id))
                   (reduce
                    (fn [by-id candidate]
                      (update by-id (:pattern-id candidate)
                              #(merge % candidate)))
                    {})
                   vals
                   (sort-by (juxt #(count (:memory-proposal-support %))
                                  :score)
                            #(compare %2 %1))
                   (take top-k)
                   vec)
              candidates
              (mapv (fn [candidate]
                      (let [pattern-id (:pattern-id candidate)
                            recall-fn
                            (or (:memory-recall-fn config)
                                memory-recall/recall-by-endpoint)
                            recall
                            (if-let [domain (:memory-domain config)]
                              (recall-fn
                               {:domain domain
                                :evidence-store (:evidence-store config)}
                               pattern-id
                               {:limit (or (:memory-recall-limit config) 3)})
                              {:ok false
                               :endpoint pattern-id
                               :memories []
                               :error {:error/code :memory-domain-not-configured
                                       :error/message
                                       "Pattern recall requires an explicit memory domain"}})
                            hooks
                            (mapv (fn [memory]
                                    {:memory-id (:memory/id memory)
                                     :hook (:memory/hook memory)
                                     :role (if (or (= :challenged
                                                     (:memory/state memory))
                                                   (= :challenged
                                                      (:memory/witness-status memory)))
                                             :challenging
                                             :supporting)
                                     :state (:memory/state memory)
                                     :witness-status
                                     (:memory/witness-status memory)
                                     :volatile? (:memory/volatile? memory)})
                                  (:memories recall))]
                        (cond-> (assoc candidate
                                       :memory-hooks hooks
                                       :memory-recall
                                       (dissoc recall :memories))
                          (empty? hooks)
                          (assoc :memory-hole
                                 {:kind (if (:ok recall)
                                          :no-reviewed-attachment
                                          :recall-unavailable)
                                  :pattern-id pattern-id
                                  :error (:error recall)}))))
                    candidates)]
          {:ok true
           :result {:query query
                    :top-k top-k
                    :candidate-construction
                    {:lexical-index :futon3a/notions-index
                     :reviewed-memory-proposal
                     (dissoc proposal :candidates)}
                    :candidates candidates}})
        (catch Exception e
          {:ok false :error (str "psr-search failed: " (.getMessage e))})))))

(defn- tool-psr-select
  [discipline-state config args]
  (let [pattern-id (normalize-pattern-id (first args))
        opts (if (map? (second args)) (second args) {})
        task-id (or (:task-id opts) (gen-id "task"))
        rationale (or (:rationale opts)
                      (some-> (nth args 2 nil) str))
        recall-fn (or (:memory-recall-fn config)
                      memory-recall/recall-by-endpoint)]
    (cond
      (str/blank? pattern-id)
      {:ok false :error "psr-select requires a pattern-id"}

      (not (relations/pattern-exists? pattern-id))
      {:ok false :error (str "Pattern not found in futon3b library: " pattern-id)}

      :else
      (try
        (let [psr {:psr/id (gen-id "psr")
                   :psr/task-id task-id
                   :psr/type :selection
                   :psr/pattern-ref pattern-id
                   :psr/candidates (vec (or (:candidates opts) []))
                   :psr/rationale rationale}
              _ (gate-shapes/validate! gate-shapes/PSR psr)
              recall
              (if-let [domain (:memory-domain config)]
                (recall-fn
                 {:domain domain
                  :evidence-store (:evidence-store config)}
                 pattern-id
                 {:limit (or (:memory-recall-limit config) 3)
                  :include-bodies? true})
                {:ok false
                 :endpoint pattern-id
                 :memories []
                 :error {:error/code :memory-domain-not-configured
                         :error/message
                         "Pattern recall requires an explicit memory domain"}})
              memories (vec (:memories recall))
              surfaced-ids (mapv :memory/id memories)
              inclusion-reasons
              (into {}
                    (map (fn [memory]
                           [(:memory/id memory)
                            (str "reviewed attachment to selected pattern "
                                 pattern-id)])
                         memories))]
          (swap! discipline-state assoc-in [:psr/by-pattern pattern-id] psr)
          (swap! discipline-state assoc-in
                 [:memory/by-psr (:psr/id psr)]
                 {:pattern-id pattern-id
                  :domain (:memory-domain config)
                  :surfaced-at (now-str)
                  :surfaced-memory-ids surfaced-ids
                  :inclusion-reasons inclusion-reasons
                  :recall-audit (dissoc recall :memories)})
          {:ok true
           :result {:pattern-id pattern-id
                    :psr psr
                    :selected? true
                    :attached-memories memories
                    :memory-recall (dissoc recall :memories)
                    :memory-hole
                    (when (empty? memories)
                      {:kind (if (:ok recall)
                               :no-reviewed-attachment
                               :recall-unavailable)
                       :pattern-id pattern-id
                       :error (:error recall)})}})
        (catch Exception e
          {:ok false :error (str "psr-select failed: " (.getMessage e))})))))

(defn- normalize-memory-rejections
  [xs]
  (reduce
   (fn [acc item]
     (let [memory-id (or (:memory-id item) (:memory_id item))
           reason (:reason item)]
       (if (and (string? memory-id) (not (str/blank? memory-id))
                (string? reason) (not (str/blank? reason)))
         (assoc acc memory-id reason)
         (throw (ex-info "memory rejections require memory_id and reason"
                         {:rejection item})))))
   {}
   (or xs [])))

(defn- verify-independent-outcome
  [evidence-store agent-id outcome-id]
  (when outcome-id
    (let [entry (estore/get-entry* evidence-store outcome-id)]
      (cond
        (nil? entry)
        (throw (ex-info "independent outcome evidence was not found"
                        {:outcome-id outcome-id}))

        (= (str agent-id) (:evidence/author entry))
        (throw (ex-info "memory user cannot witness its own outcome"
                        {:outcome-id outcome-id :author agent-id}))

        (not= :independently-witnessed
              (get-in entry [:evidence/body
                             :memory-outcome/witness-status]))
        (throw (ex-info "outcome evidence is not independently witnessed"
                        {:outcome-id outcome-id}))

        :else entry))))

(defn- persist-memory-use!
  [evidence-store config pattern-id receipt]
  (let [entry
        {:evidence/subject {:ref/type :pattern :ref/id pattern-id}
         :evidence/type :pattern-outcome
         :evidence/claim-type :observation
         :evidence/author (str (:agent-id config))
         :evidence/session-id ((:session-id-fn config))
         :evidence/body {:event :memory-use
                         :memory-use receipt}
         :evidence/tags [:memory :memory-use]}
        persisted (boundary/append! evidence-store entry)]
    (when-not (:ok persisted)
      (throw (ex-info "memory-use receipt persistence failed"
                      {:receipt persisted})))
    persisted))

(defn- pur-request-key
  [pattern-id psr-ref outcome opts]
  {:pattern-id pattern-id
   :psr-ref psr-ref
   :outcome outcome
   :outcome-id (:outcome-id opts)
   :criteria-eval (:criteria-eval opts)
   :prediction-error (normalize-prediction-error
                      (:prediction-error opts))
   :memory-ids (vec (or (:memory-ids opts) []))
   :memory-rejections
   (normalize-memory-rejections (:memory-rejections opts))})

(defn- tool-pur-update
  [discipline-state evidence-store config args]
  (let [pattern-id (normalize-pattern-id (first args))
        second-arg (second args)
        third-arg (nth args 2 nil)
        opts (cond
               (map? second-arg) second-arg
               (map? third-arg) third-arg
               :else {})
        status (if (map? second-arg) (:status second-arg) second-arg)
        outcome (normalize-pur-outcome (or (:outcome opts) status))
        psr (get-in @discipline-state [:psr/by-pattern pattern-id])
        psr-ref (or (:psr/id psr) (:psr-ref opts))
        memory-context (get-in @discipline-state [:memory/by-psr psr-ref])
        request-key-result
        (try
          {:key (pur-request-key pattern-id psr-ref outcome opts)}
          (catch Exception e
            {:error (.getMessage e)}))
        request-key (:key request-key-result)
        prior (some #(when (= request-key (:request-key %)) %)
                    (get @discipline-state :pur/history))]
    (cond
      (str/blank? pattern-id)
      {:ok false :error "pur-update requires a pattern-id"}

      (str/blank? psr-ref)
      {:ok false :error "pur-update requires prior psr-select (missing psr-ref)"}

      (:error request-key-result)
      {:ok false
       :error (str "pur-update failed: " (:error request-key-result))}

      prior
      {:ok true
       :result (assoc (:result prior) :idempotent-replay? true)}

      :else
      (try
        (let [criteria-eval (or (:criteria-eval opts)
                                {:status status
                                 :pattern-id pattern-id})
              pur (cond-> {:pur/id (gen-id "pur")
                           :pur/psr-ref psr-ref
                           :pur/outcome outcome
                           :pur/criteria-eval criteria-eval}
                    (some? (normalize-prediction-error
                            (:prediction-error opts)))
                    (assoc :pur/prediction-error
                           (normalize-prediction-error
                            (:prediction-error opts))))
              _ (gate-shapes/validate! gate-shapes/PUR pur)
              events (cond-> []
                       psr (conj {:gate/id :g3 :gate/record psr :gate/at (now-str)})
                       true (conj {:gate/id :g1 :gate/record pur :gate/at (now-str)}))
              memory-receipt
              (when (:memory-domain config)
                (let [surfaced (vec (or (:surfaced-memory-ids memory-context)
                                        []))
                      used (vec (or (:memory-ids opts) []))
                      rejection-reasons
                      (normalize-memory-rejections (:memory-rejections opts))
                      outcome-id (:outcome-id opts)
                      recorded-at (now-str)]
                  (verify-independent-outcome
                   evidence-store (:agent-id config) outcome-id)
                  (memory-contract/use-receipt
                   {:decision-id psr-ref
                    :session-id ((:session-id-fn config))
                    :domain (:memory-domain config)
                    :surfaced-memory-ids surfaced
                    :used-memory-ids used
                    :rejected-memory-ids (vec (keys rejection-reasons))
                    :inclusion-reasons
                    (or (:inclusion-reasons memory-context) {})
                    :rejection-reasons rejection-reasons
                    :pattern-id pattern-id
                    :outcome-id outcome-id
                    :surfaced-at (:surfaced-at memory-context)
                    :recorded-at recorded-at})))
              persisted (persist-proof-path! events {:pattern-id pattern-id
                                                     :psr psr
                                                     :pur pur}
                                                   evidence-store)
              memory-use
              (when memory-receipt
                (let [persisted-receipt
                      (persist-memory-use! evidence-store config
                                           pattern-id memory-receipt)]
                  {:receipt memory-receipt
                   :evidence-id (:evidence/id persisted-receipt)}))
              result {:pattern-id pattern-id
                      :outcome outcome
                      :pur pur
                      :proof persisted
                      :memory-use memory-use}]
          (swap! discipline-state update :pur/history
                 (fnil conj []) {:pattern-id pattern-id
                                 :request-key request-key
                                 :pur pur
                                 :proof persisted
                                 :memory-use memory-use
                                 :result result})
          {:ok true :result result})
        (catch Exception e
          {:ok false :error (str "pur-update failed: " (.getMessage e))})))))

(defn- tool-pur-mark-pivot
  [discipline-state evidence-store args]
  (let [pattern-id (normalize-pattern-id (first args))
        second-arg (second args)
        opts (if (map? second-arg) second-arg {})
        rationale (or (:rationale opts)
                      (when (string? second-arg) second-arg)
                      "pivot")
        task-id (or (:task-id opts) (gen-id "task"))
        gap-psr {:psr/id (gen-id "psr")
                 :psr/task-id task-id
                 :psr/type :gap
                 :psr/rationale rationale
                 :psr/candidates (cond-> []
                                   pattern-id (conj {:pattern-id pattern-id :event :pivot}))}]
    (try
      (gate-shapes/validate! gate-shapes/PSR gap-psr)
      (let [persisted (persist-proof-path!
                        [{:gate/id :g3 :gate/record gap-psr :gate/at (now-str)}]
                        {:pattern-id pattern-id :gap-psr gap-psr}
                        evidence-store)]
        (swap! discipline-state update :pivot/history
               (fnil conj []) {:pattern-id pattern-id :psr gap-psr :proof persisted})
        {:ok true
         :result {:pattern-id pattern-id
                  :pivoted? true
                  :psr gap-psr
                  :proof persisted}})
      (catch Exception e
        {:ok false :error (str "pur-mark-pivot failed: " (.getMessage e))}))))

(defn- tool-par-punctuate
  [discipline-state evidence-store args]
  (let [first-arg (first args)
        opts (cond
               (map? first-arg) first-arg
               (map? (second args)) (second args)
               :else {})
        par {:par/id (gen-id "par")
             :par/session-ref (or (:session-ref opts)
                                  (:session-id opts)
                                  (gen-id "discipline-session"))
             :par/what-worked (or (:what-worked opts)
                                  (when (string? first-arg) first-arg))
             :par/what-didnt (:what-didnt opts)
             :par/prediction-errors (vec (or (:prediction-errors opts) []))
             :par/suggestions (vec (map str (or (:suggestions opts) [])))}]
    (try
      (gate-shapes/validate! gate-shapes/PAR par)
      (let [persisted (persist-proof-path!
                        [{:gate/id :g0 :gate/record par :gate/at (now-str)}]
                        {:par par}
                        evidence-store)]
        (swap! discipline-state update :par/history (fnil conj []) {:par par :proof persisted})
        {:ok true
         :result {:par par
                  :proof persisted}})
      (catch Exception e
        {:ok false :error (str "par-punctuate failed: " (.getMessage e))}))))

;; =============================================================================
;; RealBackend
;; =============================================================================

(defrecord RealBackend [config discipline-state]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (let [cwd (or (:cwd config) (System/getProperty "user.dir"))
          timeout (or (:timeout-ms config) 30000)
          evidence-store (:evidence-store config)]
      (case tool-id
        ;; File tools
        :read           (tool-read cwd args)
        :glob           (tool-glob cwd args)
        :grep           (tool-grep cwd args)
        :edit           (tool-edit cwd args)
        :write          (tool-write cwd args)

        ;; Command tools
        :bash           (tool-bash cwd timeout args)
        :bash-readonly  (tool-bash-readonly cwd timeout args)
        :bash-test      (tool-bash cwd timeout args)
        :bash-git       (tool-bash cwd timeout args)
        :bash-deploy    (tool-bash cwd timeout args)

        ;; Network tools
        :web-fetch      (tool-web-fetch args)

        ;; Evidence tools
        :musn-log       (tool-musn-log evidence-store args)

        ;; Reflection tools
        :reflect-namespaces  (tool-reflect-namespaces args)
        :reflect-ns          (tool-reflect-ns args)
        :reflect-var         (tool-reflect-var args)
        :reflect-deps        (tool-reflect-deps args)
        :reflect-java-class  (tool-reflect-java-class args)

        ;; Discipline-domain tools
        :psr-search     (tool-psr-search cwd config args)
        :psr-select     (tool-psr-select discipline-state config args)
        :pur-update     (tool-pur-update discipline-state evidence-store config args)
        :pur-mark-pivot (tool-pur-mark-pivot discipline-state evidence-store args)
        :par-punctuate  (tool-par-punctuate discipline-state evidence-store args)
        :memory-record  (let [[ctx payload] args]
                          (memory-write/record-memory!
                           (assoc (or ctx {}) :evidence-store evidence-store)
                           payload))

        ;; Unknown
        {:ok false :error (str "Unknown tool: " tool-id)}))))

(defn make-real-backend
  "Create a real backend that executes tools against the filesystem and system.

  config:
     :cwd            — working directory (default: user.dir)
     :timeout-ms     — default command timeout (default: 30000)
     :evidence-store — atom for :musn-log tool (optional)
     :notions-index-path — futon3a patterns index path for :psr-search (optional)
     :memory-domain — explicit shared-memory domain; enables PSR recall
     :memory-recall-limit — per-pattern bound (default 3)
     :memory-recall-fn — injectable recall function for tests
     :agent-id / :session-id-fn — controller-stamped receipt identity
     :discipline-state — atom for PSR/PUR/PAR continuity (optional)"
  ([]
   (make-real-backend {}))
  ([config]
   (->RealBackend config (or (:discipline-state config)
                             (atom {:psr/by-pattern {}
                                    :pur/history []
                                    :pivot/history []
                                    :par/history []})))))
