(ns futon3c.logic.locus
  "Single-locus check-fn factories under family `atomic-inspectable-units`.

   Shape: for identity I and attribute A, the system carries exactly one
   record `(I, A) → R`. Each artifact-class is a sibling invariant under
   namespace `single-locus/<artifact-class>`. See
   futon3/library/invariant-coherence/single-locus.flexiarg for the full
   pattern (vocabulary, cardinality test, exemplar table, anti-patterns).

   Siblings implemented here:

     - `single-locus/mission-home` — for each active mission file,
       extract its `Home-repo:` annotation if any; cardinality-1 test
       across annotations within a single file.
     - `single-locus/agent-routing` — for each agent-id, ensure the
       registry plus any in-flight registration buffer exposes at most
       one simultaneously-active routing record.
     - `single-locus/artifact-live-copy` — for each artifact basename
       under `library/` or `scripts/`, ensure multi-repo copies carry an
       explicit `canonical-repo:` marker on one matching file.

   Library references:
     - futon3/library/invariant-coherence/single-locus.flexiarg
     - futon3/library/invariant-coherence/protocol-family-naming.flexiarg
     - futon3/library/invariant-coherence/shape-first-identify.flexiarg

   Mission: M-single-locus (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.logic.probe :as probe])
  (:import [java.time Instant]))

(def I-single-locus
  "Canonical statement of the single-locus shape. Grep-verifiable."
  (str "I-single-locus: for identity I and attribute A, the system "
       "carries at most one simultaneously-active record (I, A) -> R. "
       "Conflicting records (R1 != R2) for the same (I, A) are "
       "violations enumerated by the per-class check-fn. Default values "
       "for missing records are part of the cardinality-1 set."))

;; ---------------------------------------------------------------------------
;; single-locus/mission-home
;; ---------------------------------------------------------------------------

(def ^:private home-repo-line-re
  "Regex matching a `Home-repo:` annotation line (case-insensitive).
   Tolerates leading markdown emphasis (`**`, `*`, `_`) and trailing
   markdown emphasis around the tag. Captures the repo identifier
   (alphanumeric + dashes/underscores). Trailing characters past the
   value are tolerated (markdown closers, period, comma)."
  #"(?i)^\s*[*_]{0,2}\s*home-repo\s*[*_]{0,2}\s*:\s*[*_]{0,2}\s*([A-Za-z0-9_\-]+)\b")

(def home-repo-scan-lines
  "Number of leading lines of a mission file scanned for Home-repo annotations."
  10)

(def canonical-repo-scan-lines
  "Number of leading lines of an artifact file scanned for `canonical-repo:`."
  5)

(def default-artifact-globs
  "Default artifact scan globs for the artifact-live-copy sibling."
  ["library/**/*.flexiarg" "scripts/**"])

(def ^:private canonical-repo-line-re
  "Regex matching a `canonical-repo:` marker line (case-insensitive).
   Tolerates common line-comment prefixes so scripts can carry the
   marker without breaking syntax."
  #"(?i)^\s*(?:(?:;+|#+|//+|<!--)\s*)?[*_]{0,2}\s*canonical-repo\s*[*_]{0,2}\s*:\s*[*_]{0,2}\s*([A-Za-z0-9_\-]+)\b")

(defn- read-leading-lines
  [path n]
  (with-open [r (io/reader path)]
    (->> (line-seq r)
         (take n)
         (vec))))

(defn parse-home-repo-annotations
  "Read MISSION-FILE-PATH; return a vector of every `Home-repo:` value
  found in the first `home-repo-scan-lines` lines (lower-cased,
  case-insensitive match). Empty vector when no annotation; vector of
  two-or-more values when the file has a contradictory pair."
  [mission-file-path]
  (try
    (let [lines (read-leading-lines mission-file-path home-repo-scan-lines)]
      (->> lines
           (keep (fn [ln]
                   (when-let [m (re-find home-repo-line-re ln)]
                     (str/lower-case (second m)))))
           (vec)))
    (catch Throwable _ [])))

(defn- list-mission-files
  "Return [{:repo string :path string :basename string} ...] for every
   `holes/missions/M-*.md` file under REPO-PATH."
  [repo-path]
  (let [missions-dir (io/file repo-path "holes" "missions")]
    (if (.exists missions-dir)
      (->> (file-seq missions-dir)
           (filter #(.isFile ^java.io.File %))
           (filter #(re-matches #"M-.*\.md" (.getName ^java.io.File %)))
           (mapv (fn [^java.io.File f]
                   {:repo (.getName (io/file repo-path))
                    :path (.getPath f)
                    :basename (.getName f)})))
      [])))

(defn- repo-basename [repo-path]
  (.getName (io/file repo-path)))

(defn- relativize-path
  [repo-path file-path]
  (-> (.relativize (.toPath (io/file repo-path))
                   (.toPath (io/file file-path)))
      (str)))

(defn parse-canonical-repo-marker
  "Read ARTIFACT-PATH and return the first `canonical-repo:` marker in
   its first `canonical-repo-scan-lines` lines, lower-cased. Returns nil
   when absent or unreadable."
  [artifact-path]
  (try
    (some->> (read-leading-lines artifact-path canonical-repo-scan-lines)
             (keep (fn [ln]
                     (when-let [m (re-find canonical-repo-line-re ln)]
                       (str/lower-case (second m)))))
             (first))
    (catch Throwable _ nil)))

(defn- artifact-glob-match?
  [relative-path glob]
  (case glob
    "library/**/*.flexiarg"
    (and (str/starts-with? relative-path "library/")
         (str/ends-with? relative-path ".flexiarg"))

    "scripts/**"
    (str/starts-with? relative-path "scripts/")

    false))

(defn- artifact-file-interesting?
  [relative-path]
  (and (not (str/includes? relative-path "/__pycache__/"))
       (not (str/ends-with? relative-path ".pyc"))
       (not (str/starts-with? (last (str/split relative-path #"/")) "."))))

(defn- list-artifact-files
  "Return artifact records for files under REPO-PATH matching GLOBS."
  [repo-path globs]
  (let [repo-name (str/lower-case (repo-basename repo-path))]
    (->> (file-seq (io/file repo-path))
         (filter #(.isFile ^java.io.File %))
         (map (fn [^java.io.File f]
                (let [path (.getPath f)
                      relative-path (relativize-path repo-path path)]
                  {:repo repo-name
                   :path path
                   :relative-path relative-path
                   :basename (.getName f)
                   :canonical-repo (parse-canonical-repo-marker path)})))
         (filter (fn [{:keys [relative-path]}]
                   (and (artifact-file-interesting? relative-path)
                        (some #(artifact-glob-match? relative-path %) globs))))
         (vec))))

(defn- normalize-agent-id
  [x]
  (cond
    (map? x) (some-> (:id/value x) str)
    (string? x) x
    (keyword? x) (name x)
    (some? x) (str x)
    :else nil))

(defn- resolve-state-source
  [state-source]
  (cond
    (instance? clojure.lang.IAtom state-source) @state-source
    (fn? state-source) (state-source)
    :else state-source))

(def ^:private routing-buffer-keys
  [:registration-buffer
   :in-flight-registration-buffer
   :in-flight-registrations
   :pending-registrations
   :routing-buffer])

(defn- extract-registry-routing-records
  [registry]
  (if (map? registry)
    (->> registry
         (map (fn [[registry-key agent-record]]
                (let [agent-id (normalize-agent-id
                                (or (:agent/id agent-record) registry-key))]
                  {:identity agent-id
                   :tier :registry
                   :registry-key (normalize-agent-id registry-key)
                   :transport (or (:agent/transport agent-record)
                                  (get-in agent-record [:agent/metadata :transport])
                                  (get-in agent-record [:agent/metadata "transport"]))
                   :session-id (:agent/session-id agent-record)
                   :handle (:agent/handle agent-record)
                   :registered-at (:agent/registered-at agent-record)})))
         (filter :identity)
         (vec))
    []))

(defn- buffer-entries
  [buffer]
  (cond
    (nil? buffer) []
    (sequential? buffer) (vec buffer)
    (map? buffer) (vec (vals buffer))
    :else [buffer]))

(defn- extract-buffer-routing-records
  [tier-key buffer]
  (->> (buffer-entries buffer)
       (map-indexed
        (fn [idx entry]
          (let [agent-record (or (:agent-record entry) (:record entry) entry)
                agent-id (normalize-agent-id
                          (or (:agent/id agent-record)
                              (:agent-id agent-record)
                              (:agent-id entry)
                              (:id entry)))]
            {:identity agent-id
             :tier tier-key
             :buffer-index idx
             :transport (or (:transport entry)
                            (:agent/transport agent-record)
                            (get-in agent-record [:agent/metadata :transport])
                            (get-in agent-record [:agent/metadata "transport"]))
             :session-id (or (:session-id entry)
                             (:agent/session-id agent-record))
             :handle (or (:handle entry)
                         (:agent/handle agent-record))
             :registered-at (or (:registered-at entry)
                                (:agent/registered-at agent-record))})))
       (filter :identity)
       (vec)))

(defn- extract-agent-routing-records
  [state]
  (let [registry (if (and (map? state) (contains? state :registry))
                   (:registry state)
                   state)
        registry-records (extract-registry-routing-records registry)
        buffer-records (mapcat (fn [tier-key]
                                 (extract-buffer-routing-records
                                  tier-key
                                  (get state tier-key)))
                               routing-buffer-keys)]
    (vec (concat registry-records buffer-records))))

(defn check-mission-home-locus
  "Probe check-fn for `single-locus/mission-home`.

   For each mission file in REPO-PATHS' `holes/missions/M-*.md`,
   extract `Home-repo:` annotations from the first 10 lines.
   Cardinality test:
     0 annotations: implicit home = mission file's own repo (OK).
     1 annotation:  explicit home = the annotated repo (OK).
     2+ annotations all matching: explicit home with redundancy (OK).
     2+ annotations with at least two distinct values: VIOLATION
                    (the mission claims contradictory homes).

   The probe-result :detail enumerates per-mission stats and the
   contradiction list."
  [repo-paths]
  (fn [_evidence-store]
    (try
      (let [per-mission
            (vec
             (for [repo repo-paths
                   :when (.exists (io/file repo ".git"))
                   m (list-mission-files repo)]
               (let [annotations (parse-home-repo-annotations (:path m))
                     distinct-vals (set annotations)
                     resolved-home (cond
                                     (empty? annotations) (str/lower-case
                                                            (repo-basename repo))
                                     (= 1 (count distinct-vals)) (first distinct-vals)
                                     :else nil)
                     contradiction? (and (>= (count annotations) 2)
                                         (> (count distinct-vals) 1))]
                 {:mission-path (:path m)
                  :basename (:basename m)
                  :file-repo (str/lower-case (repo-basename repo))
                  :annotations annotations
                  :resolved-home resolved-home
                  :contradiction? contradiction?})))
            violations (vec (filter :contradiction? per-mission))
            with-explicit (count (filter #(seq (:annotations %)) per-mission))
            with-implicit (- (count per-mission) with-explicit)]
        (if (empty? violations)
          {:outcome :ok
           :detail {:scanned-repos (count (filter
                                            #(.exists (io/file % ".git"))
                                            repo-paths))
                    :total-missions (count per-mission)
                    :explicit-home-count with-explicit
                    :implicit-home-count with-implicit
                    :invariant I-single-locus}}
          {:outcome :violation
           :detail {:scanned-repos (count repo-paths)
                    :total-missions (count per-mission)
                    :violations (mapv (fn [v]
                                        (select-keys v [:mission-path
                                                        :basename
                                                        :file-repo
                                                        :annotations]))
                                      violations)
                    :invariant I-single-locus}}))
      (catch Throwable t
        {:outcome :violation
         :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                  :invariant I-single-locus}}))))

;; ---------------------------------------------------------------------------
;; single-locus/agent-routing
;; ---------------------------------------------------------------------------

(defn check-agent-routing-locus
  "Probe check-fn for `single-locus/agent-routing`.

   STATE-SOURCE may be:
   - the registry atom itself (`futon3c.agency.registry/!registry`)
   - a zero-arg fn returning either the registry map or a state map
     carrying `:registry` plus optional buffer keys in
     `routing-buffer-keys`
   - a literal registry/state map (tests)

   Cardinality test: for each agent-id, the union of the registry and
   any in-flight registration buffer yields at most one routing record."
  [state-source]
  (fn [_evidence-store]
    (try
      (let [state (resolve-state-source state-source)
            records (extract-agent-routing-records state)
            grouped (group-by :identity records)
            violations (->> grouped
                            (keep (fn [[identity rs]]
                                    (when (> (count rs) 1)
                                      {:identity identity
                                       :records (mapv #(select-keys %
                                                                    [:tier
                                                                     :registry-key
                                                                     :buffer-index
                                                                     :transport
                                                                     :session-id
                                                                     :handle
                                                                     :registered-at])
                                                      rs)})))
                            (sort-by :identity)
                            (vec))]
        (if (empty? violations)
          {:outcome :ok
           :detail {:scanned-identities (count grouped)
                    :total-agent-records (count records)
                    :buffer-record-count (count (remove #(= :registry (:tier %))
                                                        records))
                    :invariant I-single-locus}}
          {:outcome :violation
           :detail {:scanned-identities (count grouped)
                    :total-agent-records (count records)
                    :violations violations
                    :invariant I-single-locus}}))
      (catch Throwable t
        {:outcome :violation
         :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                  :invariant I-single-locus}}))))

;; ---------------------------------------------------------------------------
;; single-locus/artifact-live-copy
;; ---------------------------------------------------------------------------

(defn check-artifact-live-copy-locus
  "Probe check-fn for `single-locus/artifact-live-copy`.

   Scans REPO-PATHS for artifact files matching GLOBS (default:
   `default-artifact-globs`). For each basename appearing in two or more
   repos, require exactly one valid `canonical-repo:` marker across the
   group; otherwise surface a contradiction."
  ([repo-paths]
   (check-artifact-live-copy-locus repo-paths {}))
  ([repo-paths {:keys [globs] :or {globs default-artifact-globs}}]
   (fn [_evidence-store]
     (try
       (let [artifact-files
             (->> repo-paths
                  (filter #(.exists (io/file % ".git")))
                  (mapcat #(list-artifact-files % globs))
                  (vec))
             grouped (group-by :basename artifact-files)
             multi-repo-groups
             (->> grouped
                  (keep (fn [[basename records]]
                          (let [repos (set (map :repo records))]
                            (when (>= (count repos) 2)
                              [basename (vec records)]))))
                  (into {}))
             violations
             (->> multi-repo-groups
                  (keep (fn [[basename records]]
                          (let [repo-set (set (map :repo records))
                                canonical-repos (->> records
                                                     (keep :canonical-repo)
                                                     (set))
                                canonical-ok? (and (= 1 (count canonical-repos))
                                                   (contains? repo-set
                                                              (first canonical-repos)))]
                            (when-not canonical-ok?
                              {:identity basename
                               :canonical-repos (vec (sort canonical-repos))
                               :records (mapv #(select-keys %
                                                            [:repo
                                                             :relative-path
                                                             :canonical-repo])
                                              records)}))))
                  (sort-by :identity)
                  (vec))]
         (if (empty? violations)
           {:outcome :ok
            :detail {:scanned-repos (count (filter #(.exists (io/file % ".git"))
                                                   repo-paths))
                     :total-artifacts (count artifact-files)
                     :multi-repo-basename-count (count multi-repo-groups)
                     :globs globs
                     :invariant I-single-locus}}
           {:outcome :violation
            :detail {:scanned-repos (count repo-paths)
                     :total-artifacts (count artifact-files)
                     :violations violations
                     :globs globs
                     :invariant I-single-locus}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                   :invariant I-single-locus}})))))

;; ---------------------------------------------------------------------------
;; Load-time check (mirrors archaeology.clj load-time pattern)
;; ---------------------------------------------------------------------------

(def default-repo-paths
  "Default futon-stack repo paths the mission-home check scans."
  ["/home/joe/code/futon0"
   "/home/joe/code/futon1a"
   "/home/joe/code/futon3"
   "/home/joe/code/futon3a"
   "/home/joe/code/futon3b"
   "/home/joe/code/futon3c"
   "/home/joe/code/futon4"
   "/home/joe/code/futon5"
   "/home/joe/code/futon5a"])

(defn- emit-load-time-fired!
  [evidence-store family-id author {:keys [outcome detail]}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id (str "structural-law-family/" (name family-id))}
    :type :coordination
    :claim-type :observation
    :author author
    :body {:event :family-fired
           :family-id family-id
           :outcome outcome
           :detail (or detail {})
           :invariant I-single-locus
           :at (str (Instant/now))}
    :tags [:invariant-queue :family-canary :family-fired
           :load-time family-id outcome]}))

(defn- load-time-ok-summary
  [family-id detail]
  (case family-id
    :single-locus/mission-home
    (str (:total-missions detail) " mission(s); "
         (:explicit-home-count detail) " explicit, "
         (:implicit-home-count detail) " implicit")

    :single-locus/agent-routing
    (str (:total-agent-records detail) " routing record(s); "
         (:scanned-identities detail) " agent-id(s)")

    :single-locus/artifact-live-copy
    (str (:total-artifacts detail) " artifact file(s); "
         (:multi-repo-basename-count detail) " multi-repo basename group(s)")

    (pr-str detail)))

(defn- load-time-violation-lines
  [family-id detail]
  (let [violations (:violations detail)]
    (case family-id
      :single-locus/mission-home
      (map (fn [v]
             (str (:basename v) " in " (:file-repo v)
                  ": " (pr-str (:annotations v))))
           violations)

      :single-locus/agent-routing
      (map (fn [v]
             (str (:identity v) ": "
                  (pr-str (mapv #(select-keys % [:tier :registry-key :buffer-index])
                                (:records v)))))
           violations)

      :single-locus/artifact-live-copy
      (map (fn [v]
             (str (:identity v) ": "
                  (pr-str (mapv #(select-keys % [:repo :relative-path :canonical-repo])
                                (:records v)))))
           violations)

      [(pr-str detail)])))

(defn- run-load-time-check!
  [evidence-store family-id author result {:keys [emit? print?]
                                           :or {emit? true print? true}}]
  (let [outcome (or (:outcome result) :violation)
        detail (:detail result)
        receipt (when emit?
                  (try
                    (emit-load-time-fired!
                     evidence-store family-id author
                     {:outcome outcome :detail detail})
                    (catch Throwable t
                      {:ok false
                       :error/code :emit-failed
                       :error/exception (str (.getName (class t)) ": "
                                             (.getMessage t))})))]
    (when print?
      (case outcome
        :ok
        (println (str "[locus] " (name family-id) " load-time check: OK ("
                      (load-time-ok-summary family-id detail) ")"))
        :inactive
        (println (str "[locus] " (name family-id)
                      " load-time check: INACTIVE"))
        (do
          (println "================================================================")
          (println (str "[locus] " (name family-id) " LOAD-TIME CHECK FAILED"))
          (println (str "        violations: " (count (:violations detail))))
          (doseq [line (load-time-violation-lines family-id detail)]
            (println (str "        " line)))
          (when-let [exception (:exception detail)]
            (println (str "        exception: " exception)))
          (println "        Boot continues; the violation is recorded as evidence.")
          (println "================================================================"))))
    (cond-> (assoc result :outcome outcome)
      receipt (assoc :emit-receipt receipt))))

(defn check-mission-home-locus-on-load!
  "Run the mission-home single-locus check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-mission-home-locus-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths}}]
   (run-load-time-check!
    evidence-store
    :single-locus/mission-home
    "locus/check-mission-home-locus-on-load!"
    ((check-mission-home-locus repo-paths) evidence-store)
    {:emit? emit? :print? print?})))

(defn check-agent-routing-locus-on-load!
  "Run the agent-routing single-locus check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store]
   (check-agent-routing-locus-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? state-source]
                    :or {emit? true
                         print? true
                         state-source reg/!registry}}]
   (run-load-time-check!
    evidence-store
    :single-locus/agent-routing
    "locus/check-agent-routing-locus-on-load!"
    ((check-agent-routing-locus state-source) evidence-store)
    {:emit? emit? :print? print?})))

(defn check-artifact-live-copy-locus-on-load!
  "Run the artifact-live-copy single-locus check at JVM load time and
   emit a `:family-fired` evidence entry for the outcome."
  ([evidence-store]
   (check-artifact-live-copy-locus-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths globs]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths
                         globs default-artifact-globs}}]
   (run-load-time-check!
    evidence-store
    :single-locus/artifact-live-copy
    "locus/check-artifact-live-copy-locus-on-load!"
    ((check-artifact-live-copy-locus repo-paths {:globs globs}) evidence-store)
    {:emit? emit? :print? print?})))

;; ---------------------------------------------------------------------------
;; Convenience: register-locus-taps!
;; ---------------------------------------------------------------------------

(defn register-locus-taps!
  "Register the single-locus/* check-fns with `futon3c.logic.probe`.
   Operator-driven; intended to be called once at activation time
   alongside `register-default-taps!` and `register-archaeology-control-taps!`.

   Options:
     :repo-paths — override the default futon-stack repo list for the
                   mission-home and artifact-live-copy checks.
     :state-source — override the agent-routing state source.
     :globs — override artifact-live-copy globs.

   Returns the set of registered family-ids."
  ([] (register-locus-taps! {}))
  ([{:keys [repo-paths state-source globs]
     :or {repo-paths default-repo-paths
          state-source reg/!registry
          globs default-artifact-globs}}]
   (probe/register-family-check!
    :single-locus/mission-home
    (check-mission-home-locus repo-paths))
   (probe/register-family-check!
    :single-locus/agent-routing
    (check-agent-routing-locus state-source))
   (probe/register-family-check!
    :single-locus/artifact-live-copy
    (check-artifact-live-copy-locus repo-paths {:globs globs}))
   #{:single-locus/mission-home
     :single-locus/agent-routing
     :single-locus/artifact-live-copy}))
