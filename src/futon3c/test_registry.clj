(ns futon3c.test-registry
  "Test warrants in the existing evidence store. Each run is an intent -> result
  -> review hash chain, not a second ledger. SHA integrity is not authentication
  or a proof of test adequacy. Missing warrants never prohibit running tests."
  (:require [clojure.edn :as edn]
            [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.agency.warrant :as warrant]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.evidence.store :as store])
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
  directory contents, including local dependencies. No version-name-only pins.
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
                       (if (.isDirectory f)
                         ;; Directory classpath entries are pinned by the LOAD
                         ;; CLOSURE (see load-closure), not whole-directory
                         ;; hashing: an unrelated commit to the same directory
                         ;; must not stale every warrant on a busy repo.
                         {:path (str f) :sha256 :load-closure-only}
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

(defn fingerprint [options]
  (binding [*test-environment* (test-environment options)] (fingerprint* options)))

(defn test-namespace-of
  "The single declared test namespace (validate-command! enforces exactly one -n)."
  [command]
  (some (fn [[flag value]] (when (contains? #{"-n" "--namespace"} flag) value))
        (partition 2 (drop 2 command))))

(defn- closure-probe-form
  "A probe form run under the test command's own resolved classpath: require
  the declared namespace, then report every loaded namespace whose source
  resolves to a file: resource (repo/checkout sources; jar sources are pinned
  by the jar fingerprint and excluded here)."
  [test-ns]
  (pr-str
   `(do (require 'clojure.string 'clojure.java.io '~(symbol test-ns))
        (let [res# (fn [nm#]
                     (let [b# (clojure.string/replace nm# \. \/)]
                       (or (clojure.java.io/resource (str b# ".clj"))
                           (clojure.java.io/resource (str b# ".cljc")))))]
          (println (pr-str
                    (vec (for [n# (all-ns)
                               :let [nm# (str (ns-name n#))
                                     u# (some-> (res# nm#) str)]
                               :when (and u# (clojure.string/starts-with? u# "file:"))]
                           {:ns nm# :url u#}))))))))

(defn load-closure
  "The LOAD CLOSURE of a test run: which source files the declared namespace
  actually loads, with per-file shas. Directory classpath entries are pinned
  by this closure instead of whole-directory hashing, so an unrelated commit
  to the same directory does not stale the warrant — only a change to a file
  the run actually loads does. JARs, JVM, CLI config and the declared test
  environment are fingerprinted separately and unchanged."
  [{:keys [repo-root command] :as options}]
  (binding [*test-environment* (test-environment options)]
    (let [alias (second command)
          config (pr-str {:aliases {:futon3c.test-registry/closure-probe
                                    {:main-opts ["-e" (closure-probe-form
                                                       (test-namespace-of command))]}}})
          probe-alias (str alias ":futon3c.test-registry/closure-probe")
          out (command! repo-root ["clojure" "-Sdeps" config probe-alias])
          entries (try (edn/read-string out)
                       (catch Exception e
                         (fail! :closure-probe-unparseable
                                {:error (.getMessage e)
                                 :output (subs out 0 (min 256 (count out)))})))
          _ (when-not (and (vector? entries)
                           (every? #(and (map? %) (nonblank? (:ns %)) (nonblank? (:url %))) entries))
              (fail! :closure-probe-unparseable {:output (subs out 0 (min 256 (count out)))}))
          base-path (.toPath (.getCanonicalFile (io/file repo-root)))]
      (vec
       (sort-by :ns
                (for [{:keys [ns url]} entries
                      :let [file (io/file (URL. url))
                            canon (.toPath (.getCanonicalFile file))]]
                  {:ns ns
                   :path (if (.startsWith canon base-path)
                           (str (.relativize base-path canon))
                           (str canon))
                   :sha256 (file-sha file)}))))))

(defn closure-shas
  "Pure: closure entries -> {path sha256}. Testable without shelling out."
  [closure]
  (into {} (map (juxt :path :sha256)) closure))

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

(defn run-process! [root command log-file]
  (let [builder (ProcessBuilder. ^java.util.List command)
        _ (.directory builder (io/file root))
        _ (.putAll (.environment builder) *test-environment*)
        _ (.redirectErrorStream builder true)
        _ (.redirectOutput builder (io/file log-file))
        start (System/nanoTime) process (.start builder) exit (.waitFor process)]
    (parse-results exit (slurp log-file) (long (/ (- (System/nanoTime) start) 1000000)))))

(defn validate-command! [command]
  ;; Namespace bound: never expand a registry invocation to the whole suite.
  (when-not (and (vector? command) (every? nonblank? command)
                 (= "clojure" (first command))
                 (= 1 (count (filter #{"-n" "--namespace"} command)))
                 (even? (count (drop 2 command)))
                 (every? (fn [[flag value]]
                           (and (contains? #{"-n" "--namespace" "-v" "--var" "-i" "--include" "-e" "--exclude" "-d" "--dir"} flag)
                                (not (str/starts-with? value "-"))))
                         (partition 2 (drop 2 command))))
    (fail! :explicit-namespace-required {:command command})))

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
        ;; The load closure pins exactly what the run actually loads; directory
        ;; classpath entries carry :load-closure-only in the fingerprint.
        closure (load-closure options)
        post (try {:code (capture-code options) :env (fingerprint options)}
                  (catch Exception e {:error (.getMessage e)}))
        stable? (and (= code (:code post)) (= env (:env post)))
        record (merge common {:kind :run :finished-at (str (Instant/now)) :results results
                              :load-closure closure
                              :log-artifact {:path (.getCanonicalPath log-file) :sha256 (file-sha log-file)}
                              :execution/stable? stable?
                              :cost {:total-before-result-append-ms (long (/ (- (System/nanoTime) wall-start) 1000000))
                                     :execution-ms (:duration-ms results)}
                              :postcheck (if stable? {:status :matched} (refusal :inputs-changed-during-run post))
                              :warrant? (and stable? (successful? results))})]
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
          ;; Closure check: exactly the files the run actually loaded. A changed
          ;; closure file refuses naming the FILES; a changed path outside both
          ;; the manifests and the closure is irrelevant to this warrant.
          observed-closure (try (load-closure capture-options)
                                (catch Exception e
                                  (fail! :closure-probe-failed {:error (.getMessage e)})))
          closure-changed (closure-diff (closure-shas (:load-closure run))
                                        (closure-shas observed-closure))
          log (:log-artifact run)]
      (when-not (and (true? (:warrant? run)) (true? (:execution/stable? run)) (successful? (:results run)))
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
      (when-not (and (nonblank? (:path log)) (= (:sha256 log) (file-sha (:path log))))
        (fail! :log-mismatch {}))
      (when-not (= (:results run) (parse-results (get-in run [:results :exit]) (slurp (:path log))
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
      (prn result)
      (shutdown-agents)
      (when (or (= false (:warrant? result)) (= false (get-in result [:payload :warrant?])))
        (System/exit 1)))
    (catch Exception e
      (prn (or (ex-data e) (refusal :registry-failed {:error (.getMessage e)})))
      (shutdown-agents)
      (System/exit 1))))
