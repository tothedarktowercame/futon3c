(ns futon3c.test-registry
  "Test warrants in the existing evidence store. Each run is an intent -> result
  -> review hash chain, not a second ledger. SHA integrity is not authentication
  or a proof of test adequacy. Missing warrants never prohibit running tests.

  A WARRANT PINS ITS LOG BY CONTENT, NOT BY PATH. The log goes into the
  write-only ledger (`futon3c.test-registry.ledger`) under its own sha256, and
  a check resolves it there first, falling back to the recorded path only for
  records written before the ledger existed. So `:artifact-dir` is now just
  where the run writes as it goes: losing it, moving it or tampering with it
  no longer touches the warrant.

  That replaces the rule this docstring carried earlier today — choose a
  permanent artifact directory and never move it — which asked every caller to
  get something right that the store can simply guarantee. What survives of it
  is the reason: on 2026-09-17 three warrants pinned logs under /tmp, and a
  warrant whose log is gone is not stale but unverifiable, with no way back
  except re-running. All 45 run records to that date were backfilled into the
  ledger, so that failure is now unreachable for them too."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.agency.warrant :as warrant]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.evidence.store :as store]
            [futon3c.test-registry.ledger :as ledger])
  (:import [java.net URL]
           [java.nio.file Files]
           [java.security MessageDigest]
           [java.time Instant]
           [java.util UUID]))

(def default-spot-check-count 1)
(def max-dependency-directory-bytes (* 256 1024 1024))
(defn none [reason] {:status :none :reason reason})
(defn refusal [reason details]
  {:record/type :test-registry/refusal :warrant? false :reason reason :details details})
(defn- fail! [reason details] (throw (ex-info (name reason) (refusal reason details))))
(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn canonical [x]
  (cond (map? x) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [[k v]] [k (canonical v)])) x)
        (set? x) (vec (sort-by pr-str (map canonical x)))
        (sequential? x) (mapv canonical x)
        :else x))
(defn sha [x] (digest/sha256 (pr-str (canonical x))))
(defn file-sha [file]
  (let [md (MessageDigest/getInstance "SHA-256") buffer (byte-array 65536)]
    (with-open [in (io/input-stream file)]
      (loop [] (let [n (.read in buffer)]
                 (when (pos? n) (.update md buffer 0 n) (recur)))))
    (format "%064x" (BigInteger. 1 (.digest md)))))
(def ^:dynamic *test-environment* {})
(defn test-environment [options]
  (let [env (:test-environment options {})]
    (when-not (and (map? env) (every? #{"LC_ALL" "LANG" "TZ"} (keys env))
                   (every? nonblank? (vals env)))
      (fail! :invalid-test-environment {:allowed-keys ["LC_ALL" "LANG" "TZ"]}))
    env))
(defn effective-environment [] (merge (into {} (System/getenv)) *test-environment*))

(defn command! [root argv]
  (let [r (apply shell/sh (concat argv [:dir root :env (effective-environment)]))]
    (when-not (zero? (:exit r)) (fail! :command-failed {:command argv :result r}))
    (str/trim (:out r))))

(defn- files-under [file]
  (when-not (.exists (io/file file)) (fail! :missing-path {:path (str file)}))
  (let [files (tree-seq #(and (.isDirectory %) (not (Files/isSymbolicLink (.toPath %))))
                        #(seq (.listFiles %)) (io/file file))]
    (doseq [f files]
      (when (Files/isSymbolicLink (.toPath f))
        (fail! :symlink-scope-unsupported {:path (str f)})))
    (filter #(.isFile %) files)))

(defn manifest [root paths]
  (when-not (and (vector? paths) (seq paths) (every? nonblank? paths))
    (fail! :scope-required {:paths paths}))
  (let [base (.toPath (.getCanonicalFile (io/file root)))]
    (into (sorted-map)
          (for [path paths
                :let [f (io/file root path)]
                _ [(when (or (.isAbsolute (io/file path))
                             (not (.startsWith (.toPath (.getCanonicalFile f)) base)))
                     (fail! :path-outside-repository {:path path}))]
                file (files-under f)]
            [(str (.relativize base (.toPath (.getCanonicalFile file)))) (file-sha file)]))))

(defn capture-code [{:keys [repo-root code-paths test-paths]}]
  (let [code (manifest repo-root code-paths) tests (manifest repo-root test-paths)]
    {:code-sha (sha code) :test-sha (sha tests) :code-files code :test-files tests
     :git-head (command! repo-root ["git" "rev-parse" "HEAD"])}))

(defn directory-sha [directory]
  (let [files (vec (files-under directory))
        bytes (reduce + (map #(.length %) files))]
    (when (> bytes max-dependency-directory-bytes)
      (fail! :dependency-directory-too-large
             {:path (str directory) :bytes bytes :limit max-dependency-directory-bytes
              :next-action :declare-a-bounded-test-classpath}))
    (sha (into (sorted-map)
               (for [child files]
                 [(str (.relativize (.toPath directory) (.toPath child))) (file-sha child)])))))

(defn- fingerprint*
  "Resolve the test command's actual Clojure classpath and hash JAR bytes and
  local dependencies. Files under classpath DIRECTORIES are pinned by the LOAD
  CLOSURE instead (loaded sources plus resources the run opened), not by
  walking the directory: a classpath may include the repo root itself, and
  hashing it would stale every warrant on any commit.
  Other runners need an explicit adapter; they are not silently fingerprinted."
  [{:keys [repo-root command]}]
  (when-not (and (= "clojure" (first command))
                 (re-matches #"-M(?::[A-Za-z0-9_+.-]+)+" (second command)))
    (fail! :unsupported-runner {:command command}))
  (let [alias (second command)
        cp (command! repo-root ["clojure" "-Spath" alias])
        paths (str/split cp (re-pattern (java.util.regex.Pattern/quote java.io.File/pathSeparator)))
        deps (mapv (fn [p]
                     (let [f (.getCanonicalFile (io/file (if (.isAbsolute (io/file p)) p (str repo-root "/" p))))]
                       (cond
                         ;; A classpath entry that does not exist loads nothing;
                         ;; if it appears later the fingerprint differs.
                         (not (.exists f))
                         {:path (str f) :sha256 (none :absent)}

                         (.isDirectory f)
                         ;; Directory classpath entries are pinned by the LOAD
                         ;; CLOSURE (see compute-closure), not by walking them.
                         {:path (str f) :sha256 :load-closure-only}

                         :else
                         {:path (str f) :sha256 (file-sha f)}))) paths)
        executable (command! repo-root ["which" "clojure"])
        probe-form "(prn (select-keys (into {} (System/getProperties)) [\"java.home\" \"java.runtime.version\" \"java.vendor\" \"os.name\" \"os.arch\" \"os.version\" \"file.encoding\" \"user.language\" \"user.country\"]))"
        probe-config (pr-str {:aliases {:futon3c.test-registry/jvm-probe {:main-opts ["-e" probe-form]}}})
        probe-alias (str alias ":futon3c.test-registry/jvm-probe")
        probe-cp (command! repo-root ["clojure" "-Sdeps" probe-config "-Spath" probe-alias])
        _ (when-not (= cp probe-cp) (fail! :probe-classpath-mismatch {:command command}))
        jvm-properties (edn/read-string (command! repo-root ["clojure" "-Sdeps" probe-config probe-alias]))
        java (str (get jvm-properties "java.home") "/bin/java")
        description (command! repo-root ["clojure" "-Sdescribe"])
        config-files (:config-files (edn/read-string description))
        parts {:toolchain {:clojure description :alias alias
                           :config-hashes (into (sorted-map) (for [p config-files] [p (file-sha (if (.isAbsolute (io/file p)) p (io/file repo-root p)))]))
                           :launcher-sha (file-sha executable)}
               :jvm {:version (get jvm-properties "java.runtime.version")
                     :vendor (get jvm-properties "java.vendor")
                     :os (select-keys jvm-properties ["os.name" "os.arch" "os.version" "file.encoding" "user.language" "user.country"])
                     :java-sha (file-sha java)
                     :modules-sha (file-sha (str (get jvm-properties "java.home") "/lib/modules"))}
               :dependencies deps
               :environment (into (sorted-map)
                                  (for [key ["JAVA_HOME" "JAVA_TOOL_OPTIONS" "JDK_JAVA_OPTIONS" "CLJ_CONFIG" "CLJ_JVM_OPTS" "JAVA_OPTS" "LANG" "LC_ALL" "TZ"]]
                                    [key (if-let [value (get (effective-environment) key)] (sha value) (none :unset))]))}]
    (assoc parts :sha256 (sha parts))))

(defn- lean-fingerprint*
  "Lean build fingerprint: lean-toolchain, lake-manifest.json and lakefile.*
  content shas; `lake --version` and `lake env lean --version` (the lean lake resolves for this
  repo; bare `lean` need not be on PATH);
  the declared environment keys as today. Never hashes .lake/ build outputs."
  [{:keys [repo-root]}]
  (let [config-files (into ["lean-toolchain" "lake-manifest.json" "lakefile.lean" "lakefile.toml"]
                           (filter #(re-find #"lakefile\." %)
                                   (map #(.getName %)
                                        (filter #(.isFile %)
                                                (or (seq (.listFiles (io/file repo-root))) [])))))
        existing (distinct (filter #(.exists (io/file repo-root %)) config-files))
        parts {:toolchain (into (sorted-map) (for [f existing] [f (file-sha (io/file repo-root f))]))
               :lake-version (command! repo-root ["lake" "--version"])
               :lean-version (command! repo-root ["lake" "env" "lean" "--version"])
               :environment (into (sorted-map)
                                  (for [key ["JAVA_HOME" "JAVA_TOOL_OPTIONS" "JDK_JAVA_OPTIONS"
                                             "CLJ_CONFIG" "CLJ_JVM_OPTS" "JAVA_OPTS" "LANG" "LC_ALL" "TZ"]]
                                    [key (if-let [value (get (effective-environment) key)]
                                           (sha value) (none :unset))]))}]
    (assoc parts :sha256 (sha parts))))

(defn fingerprint [options]
  (binding [*test-environment* (test-environment options)]
    (if (= "lake" (first (:command options)))
      (lean-fingerprint* options)
      (fingerprint* options))))

(defn test-namespace-of
  "The single declared test namespace (validate-command! enforces exactly one -n)."
  [command]
  (some (fn [[flag value]] (when (contains? #{"-n" "--namespace"} flag) value))
        (partition 2 (drop 2 command))))

(defn- entry->closure
  [{:keys [ns url]} base-path]
  ;; The classpath-visible path, normalized but NOT canonicalized: a symlinked
  ;; resource is recorded at its link path and hashed through the link, so
  ;; retargeting the link or editing its target both change the recorded sha.
  (let [file (io/file (URL. url))
        visible (.normalize (.toAbsolutePath (.toPath file)))]
    {:ns ns
     :path (if (.startsWith visible base-path)
             (str (.relativize base-path visible))
             (str visible))
     :sha256 (file-sha file)}))

(defn closure-from-entries
  "Runner out-file entries -> the closure vector: repo-relative paths inside
  repo-root, absolute paths for sibling local roots, per-file shas. A
  namespace source also looked up as a resource collapses to one entry per
  path."
  [entries repo-root]
  (when-not (and (vector? entries)
                 (every? #(and (map? %) (nonblank? (:ns %)) (nonblank? (:url %))) entries))
    (fail! :closure-unparseable {:entries (pr-str (take 3 entries))}))
  (let [base-path (.toPath (.getCanonicalFile (io/file repo-root)))]
    (->> entries
         (map #(entry->closure % base-path))
         (group-by :path)
         vals
         (map (fn [rows] (first (sort-by #(str/starts-with? (:ns %) "resource:") rows))))
         (sort-by :path)
         vec)))

(defn- strip-lean-comments
  "Remove `--` line comments and nested `/- … -/` block comments (including
  doc comments) from Lean source text."
  [^String text]
  (let [n (count text) sb (StringBuilder.)]
    (loop [i 0 depth 0]
      (cond
        (>= i n) (str sb)
        (and (< (inc i) n) (= \/ (.charAt text i)) (= \- (.charAt text (inc i))))
        (recur (+ i 2) (inc depth))
        (and (pos? depth) (< (inc i) n) (= \- (.charAt text i)) (= \/ (.charAt text (inc i))))
        (do (when (= 1 depth) (.append sb \space)) (recur (+ i 2) (dec depth)))
        (pos? depth) (recur (inc i) depth)
        (and (< (inc i) n) (= \- (.charAt text i)) (= \- (.charAt text (inc i))))
        (let [eol (str/index-of text "\n" i)] (recur (if eol eol n) 0))
        :else (do (.append sb (.charAt text i)) (recur (inc i) 0))))))

(defn lean-header-imports
  "Pure: the modules a Lean file's header imports, in order. The header is an
  optional `module`, an optional `prelude`, then import commands with optional
  `public`/`private`/`meta` modifiers and an optional `all`. The header ends at
  the first other token. Checked against `lean --src-deps` in the tests."
  [text]
  (loop [[tok & more] (str/split (str/trim (strip-lean-comments text)) #"\s+")
         imports []]
    (cond
      (contains? #{"module" "prelude" "public" "private" "meta"} tok) (recur more imports)
      (= "import" tok) (let [[m & rest-toks] (if (= "all" (first more)) (rest more) more)]
                         (if (nonblank? m) (recur rest-toks (conj imports m)) imports))
      :else imports)))

(defn lean-module-file
  "Dotted module -> its source path relative to the project root."
  [module]
  (str (str/replace module \. \/) ".lean"))

(defn lean-closure
  "The transitive IMPORT closure of a Lean module, restricted to .lean sources
  inside repo-root (including the module itself), with per-file shas. Imports
  that do not resolve to a file in repo-root (Init/Lean/Std in the toolchain,
  .lake/packages) are pinned by the toolchain and lake-manifest fingerprint
  instead. `lean --deps`/`--src-deps` list DIRECT imports only, and one lean
  process per module is too slow for Mathlib-sized closures, so the header is
  parsed here and cross-checked against the toolchain in the tests."
  [{:keys [repo-root]} module]
  (let [base (.getCanonicalFile (io/file repo-root))
        source (fn [m] (let [f (io/file base (lean-module-file m))] (when (.isFile f) f)))]
    (when-not (source module)
      (fail! :lean-module-source-missing {:module module :path (lean-module-file module)}))
    (loop [queue [module] seen #{} closure []]
      (if-let [m (first queue)]
        (if (contains? seen m)
          (recur (subvec queue 1) seen closure)
          (let [f (source m)
                imports (when f (lean-header-imports (slurp f)))]
            (recur (into (subvec queue 1) (remove seen imports))
                   (conj seen m)
                   (cond-> closure f (conj {:ns m :path (lean-module-file m) :sha256 (file-sha f)})))))
        (vec (sort-by :path closure))))))

(defn lean-command? [command] (= "lake" (first command)))

(defn compute-closure
  "Clojure: the runner's out-file, written in the run JVM after the tests.
  Lean: the import closure of the built module."
  [{:keys [repo-root command]} out-file]
  (if (lean-command? command)
    (lean-closure {:repo-root repo-root} (last command))
    (closure-from-entries (edn/read-string (slurp out-file)) repo-root)))

(defn closure-shas
  "Pure: closure entries -> {path sha256}. Testable without shelling out."
  [closure]
  (into {} (map (juxt :path :sha256)) closure))

(defn current-closure-shas
  "Re-hash exactly the RECORDED closure files (no test or build process):
  {path sha256}, omitting files that no longer exist. Relative paths resolve
  against repo-root; sibling local roots were recorded absolute. Sound for
  Clojure because the closure was written after the tests in the run JVM, so
  loading any new source or opening a new resource requires a change to a
  recorded file."
  [repo-root closure]
  (into {} (for [{:keys [path]} closure
                 :let [f (if (.isAbsolute (io/file path)) (io/file path) (io/file repo-root path))]
                 :when (.isFile f)]
             [path (file-sha f)])))

(defn closure-diff
  "Pure: the paths whose sha differs between a recorded closure and an
  observed one (changed, added or removed), sorted. Empty means identical."
  [recorded observed]
  (sort (set/union
         (set (for [[p s] recorded :when (not= s (get observed p))] p))
         (set (for [[p s] observed :when (not= s (get recorded p))] p)))))

(defn- decode [entry]
  (let [body (:evidence/body entry) text (:payload-edn body)]
    (when-not (and (string? text) (= (:sha256 body) (digest/sha256 text))
                   (= (:evidence/id entry) (str "test-registry-" (:sha256 body))))
      (fail! :record-digest-mismatch {:evidence/id (:evidence/id entry)}))
    (let [payload (edn/read-string text)]
      (when-not (and (= "test-registry/v1" (:schema payload))
                     (= text (pr-str (canonical payload)))
                     (= (:author payload) (:evidence/author entry)))
        (fail! :invalid-record-envelope {:evidence/id (:evidence/id entry)}))
      payload)))

(defn read-chain! [backend id]
  (loop [id id seen #{} entries []]
    (when (contains? seen id) (fail! :chain-cycle {:evidence/id id}))
    (let [entry (store/get-entry* backend id)]
      (when-not entry (fail! :missing-entry {:evidence/id id}))
      (let [payload (decode entry) previous (:previous payload)
            row {:evidence/id id :sha256 (get-in entry [:evidence/body :sha256]) :payload payload}]
        (if (= previous (none :genesis))
          (do (when (:evidence/in-reply-to entry) (fail! :chain-link-mismatch {:evidence/id id}))
              (vec (reverse (conj entries row))))
          (let [parent (store/get-entry* backend (:evidence/id previous))]
            (when-not (and parent (= (:evidence/id previous) (:evidence/in-reply-to entry))
                           (= (:sha256 previous) (get-in parent [:evidence/body :sha256])))
              (fail! :chain-link-mismatch {:evidence/id id}))
            (recur (:evidence/id previous) (conj seen id) (conj entries row))))))))

(defn append-record!
  "Append through the single evidence boundary and verify the returned chain.
  Explicit predecessor avoids a mutable global registry head across federations."
  [backend payload parent-id]
  (when-not (and backend (nonblank? (:author payload)) (nonblank? (:run/id payload)))
    (fail! :explicit-backend-and-identity-required {}))
  (let [parent (when parent-id (last (read-chain! backend parent-id)))
        payload (assoc payload :schema "test-registry/v1"
                               :previous (if parent (select-keys parent [:evidence/id :sha256]) (none :genesis)))
        text (pr-str (canonical payload)) hash (digest/sha256 text)
        id (str "test-registry-" hash)
        entry (cond-> {:evidence/id id :evidence/subject {:ref/type :session :ref/id (:run/id payload)}
                       :evidence/type :coordination :evidence/claim-type :observation
                       :evidence/author (:author payload) :evidence/at (str (Instant/now))
                       :evidence/tags [:test-registry]
                       :evidence/body {:payload-edn text :sha256 hash}}
                parent-id (assoc :evidence/in-reply-to parent-id))
        receipt (boundary/append! backend entry)]
    (when-not (:ok receipt) (fail! :registration-failed {:receipt receipt :evidence/id id}))
    (last (read-chain! backend id))))

(defn parse-results [exit log duration-ms]
  (let [counts (re-seq #"Ran (\d+) tests containing (\d+) assertions\.\s+(\d+) failures, (\d+) errors\." log)]
    (if (= 1 (count counts))
      (let [[tests assertions failures errors] (mapv parse-long (rest (first counts)))]
        {:exit exit :tests tests :assertions assertions :failures failures :errors errors :duration-ms duration-ms})
      {:exit exit :tests (none :unparsed) :assertions (none :unparsed)
       :failures (none :unparsed) :errors (none :unparsed) :duration-ms duration-ms})))

(defn successful? [results]
  (and (every? #(and (integer? %) (not (neg? %)))
               ((juxt :exit :tests :assertions :failures :errors :duration-ms) results))
       (pos? (:tests results)) (zero? (:exit results))
       (zero? (:failures results)) (zero? (:errors results))))

(def ^:private lean-sorry-line
  ;; Lean 4.31 writes: warning: Path/File.lean:12:4: declaration uses `sorry`
  ;; (older toolchains used straight quotes). Lake replays these for cached jobs.
  #"^warning: (.+?\.lean):\d+:\d+: declaration uses [`'\u2018]sorry[`'\u2019]")

(defn parse-lean-results
  "Lake build results: exit; jobs from the completion line; :sorry-files, the
  count of `declaration uses sorry` warnings per source file, with their total
  as :sorry-count; :error-count, lines starting `error:`. Unparsed jobs take
  the (none :unparsed) route like Clojure counts."
  [exit log duration-ms]
  (let [lines (str/split-lines log)
        jobs (some-> (re-find #"Build completed successfully \((\d+) jobs?\)\." log)
                     second parse-long)
        sorry-files (into (sorted-map) (frequencies (keep #(second (re-find lean-sorry-line %)) lines)))]
    {:exit exit
     :jobs (if (integer? jobs) jobs (none :unparsed))
     :sorry-files sorry-files
     :sorry-count (reduce + 0 (vals sorry-files))
     :error-count (count (filter #(str/starts-with? % "error:") lines))
     :duration-ms duration-ms}))

(defn lean-successful?
  "A Lean build warrant attests that the module BUILT: exit 0, jobs parsed,
  no errors. Sorries do not block it; they are recorded per file
  (:sorry-files) because a closure can legitimately contain declared holes
  (DarkTower/WarMachine/Holes.lean). Consumers decide which files may carry
  a sorry."
  [results]
  (and (every? #(and (integer? %) (not (neg? %)))
               ((juxt :exit :jobs :sorry-count :error-count :duration-ms) results))
       (map? (:sorry-files results))
       (zero? (:exit results)) (pos? (:jobs results))
       (zero? (:error-count results))))

(defn command-successful? [command results]
  (if (lean-command? command) (lean-successful? results) (successful? results)))

(def runner-alias :futon3c.test-registry/runner)

(defn validate-command!
  "Two exact shapes, nothing broader.
  Clojure: [\"clojure\" \"-M<aliases>\" \"-n\" <ns>], optionally followed by
           [\"-v\" <ns/var>] for a spot-check. Namespace-bound: never the whole
           suite. The registry executes it through futon3c.test-registry.runner
           (see execution-command); records keep this logical command.
  Lean:    [\"lake\" \"build\" <Dotted.Module>], one module target."
  [command]
  (cond
    (and (vector? command) (every? nonblank? command) (lean-command? command))
    (let [[_ build module & more] command]
      (when-not (and (= "build" build) (nil? more) (nonblank? module)
                     (re-matches #"[A-Za-z_][A-Za-z0-9_']*(\.[A-Za-z_][A-Za-z0-9_']*)*" module))
        (fail! :invalid-lean-module {:command command})))

    (not (and (vector? command) (every? nonblank? command)
              (= "clojure" (first command))
              (re-matches #"-M(?::[A-Za-z0-9_+.-]+)+" (str (second command)))
              (contains? #{4 6} (count command))
              (= "-n" (nth command 2))
              (not (str/starts-with? (nth command 3) "-"))
              (or (= 4 (count command))
                  (and (= "-v" (nth command 4)) (str/includes? (nth command 5) "/")))))
    (fail! :explicit-namespace-required {:command command})))

(defn runner-root
  "The runner library (futon3c/test-registry-runner), located from this
  namespace's own source file so it works from any caller's working directory."
  []
  (let [src (io/resource "futon3c/test_registry.clj")]
    (when-not (= "file" (some-> src .getProtocol))
      (fail! :runner-library-unavailable {:source (str src)}))
    (-> (io/file src) .getCanonicalFile .getParentFile .getParentFile .getParentFile
        (io/file "test-registry-runner") .getCanonicalPath)))

(defn execution-command
  "The argv actually executed. Clojure runs the declared alias's classpath with
  the registry runner's alias appended: it adds the runner library as a local
  dependency and its :main-opts replace the declared runner. The closure
  out-file is the last argument. Lean runs as declared."
  [command closure-out]
  (if (lean-command? command)
    command
    (let [config (pr-str {:aliases {runner-alias
                                    {:extra-deps {'futon3c/test-registry-runner {:local/root (runner-root)}}
                                     :main-opts ["-m" "futon3c.test-registry.runner"]}}})]
      (-> ["clojure" "-Sdeps" config (str (second command) ":" (namespace runner-alias) "/" (name runner-alias))]
          (into (drop 2 command))
          (conj (str closure-out))))))

(defn parse-command-results [command exit log duration-ms]
  (if (lean-command? command)
    (parse-lean-results exit log duration-ms)
    (parse-results exit log duration-ms)))

(defn closure-out-file
  "Where the runner writes a run's load closure: beside its log."
  [log-file]
  (io/file (str/replace (str log-file) #"\.log$" ".closure.edn")))

(defn run-process!
  "Execute the LOGICAL command (see execution-command) with its log beside the
  closure out-file, and parse results by command kind."
  [root command log-file]
  (let [builder (ProcessBuilder. ^java.util.List (execution-command command (closure-out-file log-file)))
        _ (.directory builder (io/file root))
        _ (.putAll (.environment builder) *test-environment*)
        _ (.redirectErrorStream builder true)
        _ (.redirectOutput builder (io/file log-file))
        start (System/nanoTime) process (.start builder) exit (.waitFor process)]
    (parse-command-results command exit (slurp log-file) (long (/ (- (System/nanoTime) start) 1000000)))))

(defn register-run!
  "Run and register mechanically. Intent survives interrupted runs; a failed
  append leaves no warrant. Logs are artifacts, not a separate ledger."
  [backend {:keys [repo-root command author artifact-dir] :as options}]
  (validate-command! command)
  (when-not (and (nonblank? author) (nonblank? artifact-dir))
    (fail! :author-and-artifact-directory-required {}))
  (let [wall-start (System/nanoTime)
        id (str (UUID/randomUUID)) start (str (Instant/now))
        code (capture-code options) env (fingerprint options)
        common (merge code {:run/id id :author author :ran-at start :repo/root repo-root
                            :scope (select-keys options [:code-paths :test-paths :test-environment])
                            :command command :env-fingerprint env
                            :origin (:origin options "agency-local")})
        intent (append-record! backend (assoc common :kind :intent) (:previous-id options))
        _ (.mkdirs (io/file artifact-dir))
        log-file (io/file artifact-dir (str id ".log"))
        _ (when-not (.createNewFile log-file) (fail! :artifact-already-exists {:path (str log-file)}))
        results (try (binding [*test-environment* (test-environment options)]
                       (run-process! repo-root command log-file))
                     (catch Exception e {:exit (none :process-failed) :tests (none :process-failed)
                                         :assertions (none :process-failed) :failures (none :process-failed)
                                         :errors (none :process-failed) :duration-ms (none :process-failed)
                                         :error (.getMessage e)}))
        ;; The load closure pins what the run actually loaded: for Clojure,
        ;; written by the runner in the run JVM after the tests (dynamic
        ;; requires included); for Lean, the module's import closure. No
        ;; closure means no warrant; the run record is still appended.
        closure (try (compute-closure options (closure-out-file log-file))
                     (catch Exception e (refusal :closure-unavailable {:error (.getMessage e)})))
        post (try {:code (capture-code options) :env (fingerprint options)}
                  (catch Exception e {:error (.getMessage e)}))
        stable? (and (= code (:code post)) (= env (:env post)))
        record (merge common {:kind :run :finished-at (str (Instant/now)) :results results
                              :load-closure closure
                              :log-artifact (ledger/artifact
                                             (:ledger-root options ledger/default-root)
                                             log-file)
                              :execution/stable? stable?
                              :cost {:total-before-result-append-ms (long (/ (- (System/nanoTime) wall-start) 1000000))
                                     :execution-ms (:duration-ms results)}
                              :postcheck (if stable? {:status :matched} (refusal :inputs-changed-during-run post))
                              :warrant? (and stable? (vector? closure) (command-successful? command results))})]
    (append-record! backend record (:evidence/id intent))))

(defn check-record!
  "Cheap relative to execution, but hashes actual scope, resolved dependencies
  and log bytes. Caller supplies the review diff; no author-chosen diff waiver."
  [backend {:keys [entry-id repo-root changed-paths]}]
  (try
    (let [chain (read-chain! backend entry-id) run (:payload (last chain))
          _ (when-not (= :run (:kind run)) (fail! :not-a-run-record {}))
          intent (when (>= (count chain) 2) (:payload (nth chain (- (count chain) 2))))
          _ (when-not (and (= :intent (:kind intent))
                           (= (select-keys intent [:run/id :author :ran-at :code-sha :test-sha :command :env-fingerprint :scope])
                              (select-keys run [:run/id :author :ran-at :code-sha :test-sha :command :env-fingerprint :scope])))
              (fail! :missing-or-mismatched-run-intent {}))
          _ (when-not (and (nonblank? (:author run))
                           (not (.isBefore (Instant/parse (:finished-at run)) (Instant/parse (:ran-at run)))))
              (fail! :invalid-run-time {}))
          _ (when-not (and (vector? changed-paths) (every? nonblank? changed-paths))
              (fail! :review-diff-required {}))
          capture-options (merge (:scope run) {:repo-root repo-root :command (:command run)})
          current (capture-code capture-options)
          covered (set (concat (keys (:code-files run)) (keys (:test-files run))
                               (map :path (:load-closure run))))
          outside-closure (vec (sort (remove covered changed-paths)))
          env (fingerprint capture-options)
          ;; Closure check WITHOUT rerunning tests or builds: re-hash the
          ;; RECORDED closure files. Sound because the closure was recorded
          ;; in the run JVM after the tests (dynamic requires included), so
          ;; any newly-loaded source implies a change to an already-recorded
          ;; closure file. A changed closure file refuses naming the FILES; a
          ;; changed path outside manifests and closure is :outside-closure.
          closure-changed (closure-diff (closure-shas (:load-closure run))
                                        (current-closure-shas repo-root (:load-closure run)))
          log (:log-artifact run)
          ;; The ledger holds the object under its own sha, so it cannot have
          ;; moved; the recorded path is the fallback for pre-ledger records.
          log-file (ledger/locate log)]
      (when-not (and (true? (:warrant? run)) (true? (:execution/stable? run))
                     (vector? (:load-closure run)) (command-successful? (:command run) (:results run)))
        (fail! :unsupported-results {:results (:results run)}))
      (when-not (every? #(= (get run %) (get current %)) [:code-sha :test-sha :code-files :test-files])
        (fail! :stale-sha {:current current}))
      (when (seq closure-changed)
        (fail! :environment-mismatch {:changed-files closure-changed
                                      :next-action :rerun-the-declared-namespace}))
      (when-not (= env (:env-fingerprint run))
        (let [[expected observed] (data/diff (:env-fingerprint run) env)]
          (fail! :environment-mismatch {:expected-only expected :observed-only observed
                                        :next-action :reconcile-test-environment})))
      (when-not (and (some? log-file) (= (:sha256 log) (file-sha log-file)))
        (fail! :log-mismatch {:looked-in (if (:ledger log) [:ledger :path] [:path])}))
      (when-not (= (:results run) (parse-command-results (:command run) (get-in run [:results :exit]) (slurp log-file)
                                                       (get-in run [:results :duration-ms])))
        (fail! :results-log-mismatch {}))
      {:warrant? true :record run :chain-length (count chain) :entry-id entry-id
       :checked-at (str (Instant/now)) :diff-paths changed-paths
       :outside-closure outside-closure})
    (catch Exception e (if (:record/type (ex-data e)) (ex-data e)
                          (refusal :record-unavailable {:error (.getMessage e)})))))

(defn execution-policy
  "Decision authority for the review's execution lane. Delegates to
  futon3c.agency.warrant/reviewer-lane so there is ONE lane rule: any
  disagreement between the two would be a bug, not a policy. Note the resolved
  disagreement: the old inline policy treated a nil first-run? as \"not first
  run\" (spot-check eligible); reviewer-lane treats nil as UNDECLARED and
  routes to full scope — the stricter reading, per
  rerun-when-the-warrant-fails (\"tests changed left undeclared\" is a listed
  violation)."
  [check lane first-run?]
  (let [synthetic-warrant {:handoff/warrant-status (if (:warrant? check) :warranted :unwarranted)
                           :warrants (when lane [{:lane lane}])}
        verdict (warrant/reviewer-lane synthetic-warrant check first-run?)]
    (if (= :full-rerun (:lane verdict))
      {:mode :full-scope :reason (:reason verdict)}
      {:mode :spot-check :count default-spot-check-count :reason (:reason verdict)})))

(defn lane!
  "One command for the reviewer: check the record, get the lane. Takes the
  same EDN as check plus :lane (routine|pre-push|invariant) and
  :tests-changed? (true/false; absent means nil — undeclared, which routes to
  full scope)."
  [backend {:keys [lane tests-changed?] :as options}]
  (when-not (contains? #{:routine :pre-push :invariant nil} lane)
    (fail! :invalid-lane {:lane lane}))
  (when-not (or (nil? tests-changed?) (boolean? tests-changed?))
    (fail! :invalid-tests-changed {:tests-changed? tests-changed?}))
  (let [check (check-record! backend options)
        verdict (warrant/reviewer-lane
                 {:handoff/warrant-status (if (true? (:warrant? check)) :warranted :unwarranted)
                  :warrants (when lane [{:lane lane}])}
                 check
                 tests-changed?)]
    {:check (dissoc check :record)
     :reviewer-lane verdict}))

(defn review!
  "Record-check + independent adequacy note + priced execution. Full-scope means
  the declared namespace command, never the repo's unbounded entire suite."
  [backend {:keys [entry-id reviewer lane first-run? spot-test adequacy artifact-dir repo-root] :as options}]
  (let [started (System/nanoTime) check (check-record! backend options)
        check-ms (long (/ (- (System/nanoTime) started) 1000000))
        run (:record check) policy (execution-policy check lane first-run?)]
    (when-not (:warrant? check) (fail! :rerun-required {:check check :policy policy}))
    (when-not (and (nonblank? reviewer) (not= reviewer (:author run)) (nonblank? adequacy)
                   (contains? #{:routine :pre-push :invariant} lane) (boolean? first-run?))
      (fail! :independent-review-required {}))
    (when (and (lean-command? (:command run)) (= :spot-check (:mode policy)))
      (fail! :lean-spot-check-is-the-record-check
             {:next-action :use-lane-or-full-rebuild :policy policy}))
    (when (some #{"-v" "--var"} (:command run))
      (fail! :full-scope-command-required {:command (:command run)}))
    (when (and (= :spot-check (:mode policy))
               (not (and (nonblank? spot-test) (str/includes? spot-test "/"))))
      (fail! :spot-test-required {}))
    (let [sample-start (System/nanoTime)
          command (cond-> (:command run) (= :spot-check (:mode policy)) (into ["-v" spot-test]))
          sample (register-run! backend (merge (:scope run)
                                              {:repo-root repo-root :command command :author reviewer
                                               :artifact-dir artifact-dir :previous-id entry-id
                                               :origin (:origin run)}))
          sample-results (get-in sample [:payload :results])
          sufficient? (and (get-in sample [:payload :warrant?])
                           (or (= :full-scope (:mode policy)) (= 1 (:tests sample-results))))
          record {:run/id (:run/id run) :kind :review :author reviewer :ran-at (str (Instant/now))
                  :reviewed-entry entry-id :record-check (dissoc check :record)
                  :policy policy :lane lane :first-run? first-run?
                  :spot-test (if (= :spot-check (:mode policy)) spot-test (none :full-scope))
                  :adequacy adequacy :sample-entry (:evidence/id sample)
                  :warrant? sufficient? :outcome (if sufficient? :reviewed :selective-execution-failed)
                  :cost {:record-check-ms check-ms :author-execution-ms (get-in run [:results :duration-ms])
                         :review-execution-ms (:duration-ms sample-results)
                         :sample-total-ms (long (/ (- (System/nanoTime) sample-start) 1000000))
                         :review-total-before-append-ms (long (/ (- (System/nanoTime) started) 1000000))
                         :comparison :measured-execution-times-not-an-adequacy-proof}}]
      (append-record! backend record (:evidence/id sample)))))

(defn- emit-result
  "Print RESULT as EDN, or as one line of JSON when the config says
  :output :json (for non-Clojure callers such as emit-machine-contracts.py;
  keywords become strings, namespaced keys keep their namespace)."
  [options result]
  (if (= :json (:output options))
    (println (json/generate-string result))
    (prn result)))

(defn -main [operation config-path]
  (try
    (let [options (edn/read-string (slurp config-path))
          backend (http-backend/make-http-backend (:agency-url options "http://localhost:7070"))
          result (case operation
                   "run" (register-run! backend options)
                   "check" (check-record! backend options)
                   "review" (review! backend options)
                   "lane" (lane! backend options)
                   (fail! :unknown-operation {:operation operation}))]
      (emit-result options result)
      (shutdown-agents)
      (when (or (= false (:warrant? result)) (= false (get-in result [:payload :warrant?])))
        (System/exit 1)))
    (catch Exception e
      (emit-result (try (edn/read-string (slurp config-path)) (catch Exception _ {}))
                   (or (ex-data e) (refusal :registry-failed {:error (.getMessage e)})))
      (shutdown-agents)
      (System/exit 1))))
