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
     :evidence-store — atom for :musn-log tool (optional)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.peripheral.tools :as tools])
  (:import [java.io File]
           [java.nio.file FileSystems Files Path]
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
          entries (if (string? query-arg)
                    ;; Query by session-id
                    (->> (:entries @evidence-store)
                         vals
                         (filter #(= query-arg (:evidence/session-id %)))
                         (sort-by :evidence/at)
                         vec)
                    ;; Query by map
                    (->> (:entries @evidence-store)
                         vals
                         (sort-by :evidence/at)
                         vec))]
      {:ok true :result entries})))

;; =============================================================================
;; RealBackend
;; =============================================================================

(defrecord RealBackend [config]
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

        ;; Unknown
        {:ok false :error (str "Unknown tool: " tool-id)}))))

(defn make-real-backend
  "Create a real backend that executes tools against the filesystem and system.

   config:
     :cwd            — working directory (default: user.dir)
     :timeout-ms     — default command timeout (default: 30000)
     :evidence-store — atom for :musn-log tool (optional)"
  ([]
   (make-real-backend {}))
  ([config]
   (->RealBackend config)))
