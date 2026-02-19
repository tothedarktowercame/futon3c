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
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.tools :as tools])
  (:import [java.io File]
           [java.nio.file FileSystems Path]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent TimeUnit]))

;; =============================================================================
;; File tools
;; =============================================================================

(defn- resolve-path
  "Resolve a path relative to cwd. Absolute paths pass through."
  ^File [cwd path]
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

(defn- tool-grep
  "Search file contents by regex. Args: [pattern] or [pattern path] or [pattern path opts].
   opts: {:case-insensitive bool, :max-matches N}."
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
        results (atom [])
        done? (atom false)]
    (when (.exists base)
      (let [files (if (.isFile base)
                    [base]
                    (->> (file-seq base)
                         (filter #(.isFile ^File %))
                         (remove #(str/starts-with? (.getName ^File %) "."))))]
        (doseq [^File f files
                :while (not @done?)]
          (try
            (let [content (slurp f)
                  lines (str/split-lines content)]
              (doseq [[idx line] (map-indexed vector lines)
                      :while (not @done?)]
                (when (.find (.matcher re line))
                  (swap! results conj {:file (.getPath f)
                                       :line (inc idx)
                                       :content (subs line 0 (min (count line) 500))})
                  (when (>= (count @results) max-matches)
                    (reset! done? true)))))
            (catch Exception _)))))
    {:ok true :result @results}))

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
        (.destroyForcibly proc)
        {:exit -1 :out "" :err (str "Command timed out after " timeout-ms "ms")}))))

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
      (let [entry {:evidence/id (str "e-pp-" (:path/id proof-path))
                   :evidence/subject {:ref/type :proof-path
                                      :ref/id (:path/id proof-path)}
                   :evidence/type :coordination
                   :evidence/claim-type :observation
                   :evidence/author "gate-pipeline"
                   :evidence/at (str (Instant/now))
                   :evidence/body {:event :proof-path-persisted
                                   :path/id (:path/id proof-path)
                                   :path/file (:file persisted)
                                   :gate-events (mapv :gate/id events)}
                   :evidence/tags [:proof-path :gate-traversal]}]
        (let [append-result (estore/append* evidence-store entry)]
          (when (and (map? append-result) (contains? append-result :error/code))
            (println "[WARN] proof-path evidence append failed:" (:error/message append-result))))))
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
              candidates (->> index
                              (map candidate->result)
                              (filter #(pos? (:score %)))
                              (sort-by :score >)
                              (take top-k)
                              vec)]
          {:ok true
           :result {:query query
                    :top-k top-k
                    :candidates candidates}})
        (catch Exception e
          {:ok false :error (str "psr-search failed: " (.getMessage e))})))))

(defn- tool-psr-select
  [discipline-state args]
  (let [pattern-id (normalize-pattern-id (first args))
        opts (if (map? (second args)) (second args) {})
        task-id (or (:task-id opts) (gen-id "task"))
        rationale (or (:rationale opts)
                      (some-> (nth args 2 nil) str))]
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
              _ (gate-shapes/validate! gate-shapes/PSR psr)]
          (swap! discipline-state assoc-in [:psr/by-pattern pattern-id] psr)
          {:ok true
           :result {:pattern-id pattern-id
                    :psr psr
                    :selected? true}})
        (catch Exception e
          {:ok false :error (str "psr-select failed: " (.getMessage e))})))))

(defn- tool-pur-update
  [discipline-state evidence-store args]
  (let [pattern-id (normalize-pattern-id (first args))
        second-arg (second args)
        third-arg (nth args 2 nil)
        opts (cond
               (map? second-arg) second-arg
               (map? third-arg) third-arg
               :else {})
        status (if (map? second-arg) (:status second-arg) second-arg)
        outcome (or (:outcome opts) (normalize-pur-outcome status))
        psr (get-in @discipline-state [:psr/by-pattern pattern-id])
        psr-ref (or (:psr/id psr) (:psr-ref opts))]
    (cond
      (str/blank? pattern-id)
      {:ok false :error "pur-update requires a pattern-id"}

      (str/blank? psr-ref)
      {:ok false :error "pur-update requires prior psr-select (missing psr-ref)"}

      :else
      (try
        (let [criteria-eval (or (:criteria-eval opts)
                                {:status status
                                 :pattern-id pattern-id})
              pur (cond-> {:pur/id (gen-id "pur")
                           :pur/psr-ref psr-ref
                           :pur/outcome outcome
                           :pur/criteria-eval criteria-eval}
                    (contains? opts :prediction-error)
                    (assoc :pur/prediction-error (:prediction-error opts)))
              _ (gate-shapes/validate! gate-shapes/PUR pur)
              events (cond-> []
                       psr (conj {:gate/id :g3 :gate/record psr :gate/at (now-str)})
                       true (conj {:gate/id :g1 :gate/record pur :gate/at (now-str)}))
              persisted (persist-proof-path! events {:pattern-id pattern-id
                                                     :psr psr
                                                     :pur pur}
                                                   evidence-store)]
          (swap! discipline-state update :pur/history
                 (fnil conj []) {:pattern-id pattern-id :pur pur :proof persisted})
          {:ok true
           :result {:pattern-id pattern-id
                    :outcome outcome
                    :pur pur
                    :proof persisted}})
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

        ;; Discipline-domain tools
        :psr-search     (tool-psr-search cwd config args)
        :psr-select     (tool-psr-select discipline-state args)
        :pur-update     (tool-pur-update discipline-state evidence-store args)
        :pur-mark-pivot (tool-pur-mark-pivot discipline-state evidence-store args)
        :par-punctuate  (tool-par-punctuate discipline-state evidence-store args)

        ;; Unknown
        {:ok false :error (str "Unknown tool: " tool-id)}))))

(defn make-real-backend
  "Create a real backend that executes tools against the filesystem and system.

  config:
     :cwd            — working directory (default: user.dir)
     :timeout-ms     — default command timeout (default: 30000)
     :evidence-store — atom for :musn-log tool (optional)
     :notions-index-path — futon3a patterns index path for :psr-search (optional)
     :discipline-state — atom for PSR/PUR/PAR continuity (optional)"
  ([]
   (make-real-backend {}))
  ([config]
   (->RealBackend config (or (:discipline-state config)
                             (atom {:psr/by-pattern {}
                                    :pur/history []
                                    :pivot/history []
                                    :par/history []})))))
