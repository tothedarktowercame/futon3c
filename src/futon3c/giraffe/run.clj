(ns futon3c.giraffe.run
  "M-giraffe stage 1, packet 3: write one Giraffe run to disk.

  §DERIVE \\\"Data flow\\\": one directory per run, keyed by run id,
  holding the pass input, the rejects, the pass output (each with its
  sha256 in a manifest) and a readable report.  §ARGUE \\\"Order of
  build\\\" stage 1: the report shows Giraffe's order beside the
  source's own order (the tickets index's order of open items).

  No new computation here: (write-run!) takes the adapter's result and
  the pass output and writes; -main composes adapter + pass + write
  over holes/tickets for a given edges file.  Agent edge reading is a
  later packet.

  Determinism: run-id is a UTC timestamp plus the first 8 hex of the
  sha256 of input.edn, so two writes of the same input share the hash
  part and produce byte-identical pass.edn (maps are normalized to
  sorted maps before printing)."
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [futon3c.giraffe.missions :as gm]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.pass :as gp]
            [futon3c.giraffe.tickets :as gt]))

(def default-root "/home/joe/code/storage/giraffe/")
(def default-tickets-dir "holes/tickets")
(def default-index-file "holes/tickets/tickets-index.edn")

;; ---------------------------------------------------------------- sha256

(defn sha256
  "Hex sha256 of a file's bytes, or of a string."
  [x]
  (let [bs (if (bytes? x)
             x
             (if (instance? java.io.File x)
               (with-open [in (io/input-stream x)]
                 (let [buf (java.io.ByteArrayOutputStream.)]
                   (io/copy in buf)
                   (.toByteArray buf)))
               (.getBytes (str x) "UTF-8")))
        digest (doto (java.security.MessageDigest/getInstance "SHA-256")
                 (.update bs))]
    (apply str (map #(format "%02x" %) (.digest digest)))))

;; ---------------------------------------------------------- printing

(defn- sorted
  "Recursively turn every map into a sorted map so printed output is
  byte-stable for the same value."
  [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [(sorted k) (sorted v)])) x)
    (sequential? x) (mapv sorted x)
    (set? x) (into (sorted-set) (map sorted) x)
    :else x))

(defn- print-edn [value]
  (with-out-str (pp/pprint (sorted value))))

;; ------------------------------------------------------------- run id

(defn- timestamp []
  (-> (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC)
      (.format (java.time.format.DateTimeFormatter/ofPattern
                "yyyyMMdd'T'HHmmss'.'SSS'Z'"))))

(defn- run-id-for [input-bytes]
  (str (timestamp) "-" (subs (sha256 input-bytes) 0 8)))

;; ------------------------------------------------------------ report

(defn- edge-line [e]
  ;; quotes span wrapped lines in the ticket; collapse them so one edge stays
  ;; one markdown line.  External targets show the raw text, not the key.
  (format "- %s needs %s (%s) -- quote: \"%s\" -- %s:%s"
          (name (:from e)) (or (:external-target e) (name (:to e)))
          (name (or (:class e) :closure))
          (str/join " " (str/split (str/trim (or (:quote e) "")) #"\s+"))
          (or (:file e) "?") (or (:line e) "?")))

(defn- order-row [entry]
  (let [{:keys [id status reason cone unblocks]} entry]
    (format "| %s | %s | %s | %s | %s |"
            (name id) (name status) (name reason) (str cone) (str unblocks))))

(defn- report
  "report.md: header counts, the two orders side by side, and the
  flags (blocker-cancelled, needs-breakdown cycles, done-with-open-needs
  anomalies) with their edges and quotes."
  [{:keys [input adapter-rejected pass]}]
  (let [{:keys [as-of items edges]} input
        {:keys [counts readiness cycles anomalies order]} pass
        open-items (->> items
                        (remove #(contains? #{:completed :cancelled}
                                            (:state-group %)))
                        (sort-by :source-order))
        src-entries (map (fn [it]
                           (let [id (:id it)
                                 r (get readiness id)]
                             {:id id
                              :status (:status r)
                              :reason (:reason r)
                              :cone (when-not (:graph-coverage pass) (count (get-in pass [:cones id])))
                              :unblocks (when-not (:graph-coverage pass) (count (get-in pass [:unblocks id])))}))
                         open-items)
        edge-sources (frequencies (map #(or (:source %) :unrecorded) edges))
        reject-reasons (frequencies (map :reject adapter-rejected))
        blockers (sort-by :id (for [[id r] readiness
                             :when (= :blocker-cancelled (:flag r))]
                         {:id id :blockers (:blockers r)
                          :evidence (:evidence r)}))]
    (letfn [(section [title lines]
              (when (seq lines)
                (concat ["" (str "## Flag: " title) ""] lines)))
            (flag-lines [header body]
              (concat [header] body [""]))]
      (str/join
       "\n"
       (concat
        ["# Giraffe run"
         ""
         (str "As of: " as-of)
         (str "Open items: " (:open counts))
         (str "Ready: " (:ready counts))
         (str "Ready ignoring externals: "
              (:ready-ignoring-externals counts))
         ""
         "Edge sources and counts:"
         (str/join "\n" (for [[s n] (sort-by str edge-sources)]
                          (str "- " (name s) ": " n)))
         ""
         "Rejects by reason:"
         (str/join "\n" (if (seq reject-reasons)
                          (for [[r n] (sort-by str reject-reasons)]
                            (str "- " (name r) ": " n))
                          ["- none"]))
         ""
         "## Order of work"
         ""
         "The source's own order (tickets index order of open items) beside"
         "Giraffe's order (ready first by unblocks, cone, source order)."
         ""
         "| Source order | status | reason | cone | unblocks | Giraffe order | status | reason | cone | unblocks |"
         "|---|---|---|---|---|---|---|---|---|---|"]
        (map (fn [src giraffe]
               (let [l (order-row src) r (order-row giraffe)]
                 ;; splice: drop l's trailing | and r's leading "| "
                 (str (subs l 0 (dec (count l))) " | " (subs r 2))))
             src-entries
             (concat order (repeat {:id :none-listed :status :none
                                    :reason :none :cone 0 :unblocks 0})))
        [""]
        (let [waiting (for [{:keys [id]} order
                            :let [r (get readiness id)]
                            :when (not= :ready (:status r))]
                        [id r])]
          (when (seq waiting)
            (concat ["## Waiting on" ""
                     "What each not-ready item still needs, with the sentence the need was read from."
                     ""]
                    (mapcat (fn [[id r]]
                              (concat [(format "### %s (%s)" (name id)
                                               (name (or (:reason r) :unknown)))]
                                      (if (every? map? (:evidence r))
                                        (map edge-line (:evidence r))
                                        [(str "- " (pr-str (:evidence r)))])
                                      [""]))
                            waiting))))
        (section "blocker-cancelled"
                 (mapcat (fn [b]
                           (flag-lines
                            (format "### %s blocked by cancelled %s"
                                    (name (:id b))
                                    (str/join ", " (map name (:blockers b))))
                            (map edge-line (:evidence b))))
                         blockers))
        (section "needs-breakdown cycles"
                 (map (fn [c]
                        (str "- " (str/join " <-> " (map name c))
                             " (cycle members are withheld from readiness)"))
                      cycles))
        (section "done-with-open-needs anomalies"
                 (if (seq anomalies)
                   (mapcat (fn [a]
                             (flag-lines
                              (format "### %s closed with open needs"
                                      (name (:item a)))
                              (map edge-line (:edges a))))
                           anomalies)
                   ["- none"])))))))  ; end section, concat, join, letfn, let, defn

;; ------------------------------------------------------------- writing

(defn- git-head-sha []
  (try
    (str/trim
     (:out (shell/sh "git" "rev-parse" "HEAD"
                                  :dir (System/getProperty "user.dir"))))
    (catch Exception _ nil)))

(defn write-run!
  "Write one run directory under :root (default the canonical storage
  root, never /tmp for real runs).  :run-id defaults to a UTC timestamp
  plus a short hash of the input.  Returns {:run-id :dir :manifest}.
  The manifest carries the sha256 of every file written, the futon3c
  HEAD git sha, and the sha256 of the tickets index and edges file the
  adapter read (when supplied in the adapter result)."
  [{:keys [root run-id adapter-result pass-output git-sha index-file
           edges-file]
    :or {root default-root}}]
  {:pre [(:input adapter-result) pass-output]}
  (let [input (:input adapter-result)
        input-bytes (.getBytes ^String (print-edn input) "UTF-8")
        run-id (or run-id (run-id-for input-bytes))
        dir (io/file root run-id)
        rejected (:rejected adapter-result)
        _ (.mkdirs dir)
        write! (fn [name content]
                 (let [f (io/file dir name)]
                   (spit f content)
                   {:file name :sha256 (sha256 f)}))
        input-entry (assoc (write! "input.edn" (print-edn input))
                           :bytes input-bytes)
        rejected-entry (write! "rejected.edn" (print-edn rejected))
        excluded-entry (when (contains? adapter-result :excluded)
                         (write! "excluded.edn" (print-edn (:excluded adapter-result))))
        pass-entry (write! "pass.edn" (print-edn pass-output))
        clusters-entry (write! "work-clusters.json"
                               (json/generate-string
                                {:run_id run-id
                                 :clusters (:work-clusters pass-output)
                                 :by_item (:work-cluster-by-item pass-output)
                                 :interpretation
                                 (:work-cluster-interpretation pass-output)
                                 :ticket_provenance
                                 (:ticket-provenance pass-output)}
                                {:pretty true}))
        report-entry (write! "report.md"
                             (report {:input input
                                      :adapter-rejected rejected
                                      :pass pass-output}))
        manifest {:run-id run-id
                  :git-sha (or git-sha (git-head-sha))
                  :files (cond-> [(dissoc input-entry :bytes)
                                  rejected-entry pass-entry clusters-entry report-entry]
                           excluded-entry (conj excluded-entry))
                  :sources (cond-> {}
                             index-file (assoc :tickets-index
                                               {:path (str index-file)
                                                :sha256 (sha256 (io/file index-file))})
                             edges-file (assoc :edges
                                               {:path (str edges-file)
                                                :sha256 (sha256 (io/file edges-file))}))}]
    (write! "manifest.edn" (print-edn manifest))
    {:run-id run-id :dir (str dir) :manifest manifest}))

(def ^:private run-files
  ["input.edn" "rejected.edn" "pass.edn" "work-clusters.json" "report.md"])

(def ^:private legacy-run-files
  "Baseline for run directories written before work-clusters.json joined
  the sealed file set.  The first code that wrote work-clusters.json is
  commit e90fe14bb; run manifests record the git sha of the code that
  wrote them, so the manifest's own :git-sha is the format witness."
  ["input.edn" "rejected.edn" "pass.edn" "report.md"])

(def ^:private work-clusters-sha
  "e90fe14bb3c181964f8d8b239e5bff16225dff37")

(defn- git-descends-from-work-clusters?
  "Decide a run manifest's format from its recorded :git-sha by asking
  the repository at REPO-DIR.  Returns true (sha resolves and
  work-clusters-sha is an ancestor: current format), false (sha
  resolves, not an ancestor: legacy format) or nil (sha missing,
  malformed, unresolvable, or git unavailable: format undecidable —
  callers fail closed)."
  [repo-dir sha]
  (when (and (string? sha) (re-matches #"[0-9a-f]{40}" sha))
    (try
      (let [resolved (shell/sh "git" "rev-parse" "--verify" "--quiet"
                               (str sha "^{commit}") :dir repo-dir)]
        (when (zero? (:exit resolved))
          (let [anc (shell/sh "git" "merge-base" "--is-ancestor"
                              work-clusters-sha sha :dir repo-dir)]
            (cond (zero? (:exit anc)) true
                  (= 1 (:exit anc)) false
                  :else nil))))
      (catch Exception _ nil))))

(defn verify-run
  "Recompute the sha256 of every file the manifest recorded in this run
  directory; {:ok? true :mismatches []} or the list of files whose
  bytes no longer match.  A file planted or edited after the write is
  reported, never silently re-sealed.

  Format rule: the manifest's own :git-sha decides which baseline file
  set the manifest must list.  A run whose git-sha resolves in the repo
  and does NOT descend from e90fe14bb predates work-clusters.json and
  verifies its ORIGINAL sealed list (legacy baseline); a run at or after
  e90fe14bb must list work-clusters.json (current baseline).  A missing
  or unresolvable git-sha is undecidable and fails closed to the current
  baseline — a stripped current manifest is never accepted as legacy on
  the mere absence of the artifact.  Whatever the format, every file the
  manifest lists is sha-verified, so a legacy run that happens to list
  extra files still has them verified."
  [dir]
  (let [manifest (with-open [r (java.io.PushbackReader.
                                (io/reader (io/file dir "manifest.edn")))]
                   (edn/read r))
        listed (set (map :file (:files manifest)))
        format (git-descends-from-work-clusters?
                (System/getProperty "user.dir") (:git-sha manifest))
        legacy? (false? format)
        required (if legacy? legacy-run-files run-files)
        unlisted (vec (for [f required :when (not (listed f))]
                        (cond-> {:file f :missing-from-manifest true}
                          (nil? format) (assoc :format-undecidable true))))
        mismatches (into unlisted
                         (for [{:keys [file] :as entry} (:files manifest)
                              :let [f (io/file dir file)
                                    actual (when (.isFile f) (sha256 f))]
                              :when (not= actual (:sha256 entry))]
                          {:file file
                           :manifest-sha256 (:sha256 entry)
                           :actual-sha256 actual}))]
    {:ok? (empty? mismatches) :mismatches (vec mismatches)}))

;; -------------------------------------------------- freshness (no write)

(defn- field-run?
  "Does this run's manifest describe the missions Giraffe field (not a
  tickets-only run)?  Mirrors the consumer-side filter
  futon2.aif.full-loop-runtime/missions-giraffe-manifest?: a manifest
  whose :sources carries :tickets-index fed the tickets adapter and is
  not the field a click consumes."
  [manifest]
  (not (contains? (:sources manifest) :tickets-index)))

(defn newest-field-run
  "The newest verified run under ROOT whose manifest describes the
  missions field (runs are ordered by directory name, the same
  newest-first order futon2's find-giraffe-run uses).  Returns
  {:run-id ... :input-sha256 ...} for the first run that verifies
  under verify-run, or nil when none does.  Read-only."
  [root]
  (->> (io/file root)
       (file-seq)
       (filter #(.isDirectory ^java.io.File %))
       (remove #(= (io/file root) %))
       (sort-by #(.getName ^java.io.File %) #(compare %2 %1))
       (some (fn [dir]
               (try
                 (let [manifest (edn/read-string
                                 (slurp (io/file dir "manifest.edn")))]
                   (when (and (field-run? manifest)
                              (:ok? (verify-run dir)))
                     {:run-id (.getName ^java.io.File dir)
                      :input-sha256 (sha256 (io/file dir "input.edn"))}))
                 (catch Exception _ nil))))))

(defn check-freshness
  "Compare the adapter input freshly computed from the same sources the
  field is minted from with the input of the newest verified field run
  under ROOT.  Prints one machine-readable block and writes NOTHING --
  this is the staleness probe the WM click preparation gates its mint
  on (deficits row Q1-GIRAFFE-FIELD-STALENESS)."
  [{:keys [root adapter-result]}]
  (let [adapter-sha (sha256 (.getBytes ^String (print-edn (:input adapter-result))
                                       "UTF-8"))
        newest (newest-field-run root)
        newest-sha (:input-sha256 newest)
        fresh? (boolean (and newest-sha (= newest-sha adapter-sha)))]
    (println "giraffe-check-fresh")
    (println "adapter-input-sha256:" adapter-sha)
    (println "newest-verified-run-id:" (or (:run-id newest) "-"))
    (println "newest-verified-input-sha256:" (or newest-sha "-"))
    (println "fresh:" (if fresh? "true" "false"))
    {:fresh? fresh? :newest newest :adapter-input-sha256 adapter-sha}))

;; ---------------------------------------------------------------- main

(defn- merge-args [args]
  (into {}
        (map (fn [[k v]] [(subs k 2) v]))
        (partition 2 args)))

(defn -main
  "clojure -M -m futon3c.giraffe.run --edges <file> [--only <ids-file>]
  [--root <dir>] [--run-id <id>] [--source tickets|missions|missions-excursions]
  [--repos <comma-separated repo roots, missions source only>]
  [--readings-root <checked-reading-directory>]

  Runs the chosen source adapter (default tickets) plus the edges
  file, the Giraffe pass, and writes one run directory.  Prints the
  run dir and manifest path.  With --check-fresh computes the adapter
  input, compares it with the newest verified field run under --root,
  prints the freshness block and writes NOTHING (staleness probe)."
  [& args]
  (let [check-fresh? (some #{ "--check-fresh" } args)
        {:strs [edges only root run-id source repos readings-root]}
        (merge-args (remove #{"--check-fresh"} args))
        source (or source "tickets")
        _ (when (str/blank? (str edges))
            (throw (ex-info "--edges <file> is required"
                            {:kind :giraffe/edges-required})))
        _ (when-not (contains? #{"tickets" "missions" "missions-excursions"} source)
            (throw (ex-info "--source must be tickets or missions"
                            {:kind :giraffe/unknown-source
                             :source source})))
        only-ids (when only
                   (vec (remove str/blank? (str/split (slurp only) #"\s+"))))
        combined? (= "missions-excursions" source)
        missions? (or combined? (= "missions" source))
        repo-roots (when missions?
                     (or (when repos
                           (vec (remove str/blank?
                                        (map str/trim (str/split repos #",")))))
                         gm/default-repos))
        reading-batches (when readings-root
                          (->> (file-seq (io/file readings-root))
                               (filter #(.isFile %))
                               (filter #(re-matches #"batch-.*\.jsonl" (.getName %)))
                               (sort-by #(.getName %))
                               (mapv #(.getAbsolutePath %))))
        _ (when (and readings-root (not combined?))
            (throw (ex-info "--readings-root requires --source missions-excursions"
                            {:kind :giraffe/readings-source-mismatch})))
        _ (when (and readings-root (empty? reading-batches))
            (throw (ex-info "--readings-root contains no batch-*.jsonl files"
                            {:kind :giraffe/readings-empty :root readings-root})))
        adapter-result (if missions?
                         ((if combined? excursions/missions-and-excursions->input gm/missions->input)
                          {:repos repo-roots
                           :edges-file edges
                           :only only-ids
                           :include-tickets (boolean readings-root)
                           :reading-checker (when readings-root
                                              (str (io/file readings-root "check_edges.py")))
                           :members-file (when readings-root
                                           (str (io/file readings-root "members.json")))
                           :reading-batches reading-batches
                           :reading-scope (when readings-root
                                            :population/missions-excursions-tickets)})
                         (gt/tickets->input
                          {:tickets-dir default-tickets-dir
                           :index-file default-index-file
                           :edges-file edges
                           :only only-ids}))
        pass-output ((if combined? excursions/combined-pass gp/pass) (:input adapter-result))
        _ (when check-fresh?
            (check-freshness {:root (or root default-root)
                              :adapter-result adapter-result})
            (System/exit 0))
        result (write-run! {:root (or root default-root)
                            :run-id run-id
                            :adapter-result adapter-result
                            :pass-output pass-output
                            :git-sha (git-head-sha)
                            :index-file (when-not missions? default-index-file)
                            :edges-file edges})]
    (println "run dir:" (:dir result))
    (println "run id:" (:run-id result))
    (println "open:" (get-in pass-output [:counts :open])
             "ready:" (get-in pass-output [:counts :ready])
             "ready ignoring externals:"
             (get-in pass-output [:counts :ready-ignoring-externals]))
    (System/exit 0)))
