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
        report-entry (write! "report.md"
                             (report {:input input
                                      :adapter-rejected rejected
                                      :pass pass-output}))
        manifest {:run-id run-id
                  :git-sha (or git-sha (git-head-sha))
                  :files (cond-> [(dissoc input-entry :bytes)
                                  rejected-entry pass-entry report-entry]
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
  ["input.edn" "rejected.edn" "pass.edn" "report.md"])

(defn verify-run
  "Recompute the sha256 of every file the manifest recorded in this run
  directory; {:ok? true :mismatches []} or the list of files whose
  bytes no longer match.  A file planted or edited after the write is
  reported, never silently re-sealed."
  [dir]
  (let [manifest (with-open [r (java.io.PushbackReader.
                                (io/reader (io/file dir "manifest.edn")))]
                   (edn/read r))
        listed (set (map :file (:files manifest)))
        unlisted (vec (for [f run-files :when (not (listed f))]
                        {:file f :missing-from-manifest true}))
        mismatches (into unlisted
                         (for [{:keys [file] :as entry} (:files manifest)
                              :let [f (io/file dir file)
                                    actual (when (.isFile f) (sha256 f))]
                              :when (not= actual (:sha256 entry))]
                          {:file file
                           :manifest-sha256 (:sha256 entry)
                           :actual-sha256 actual}))]
    {:ok? (empty? mismatches) :mismatches (vec mismatches)}))

;; ---------------------------------------------------------------- main

(defn- merge-args [args]
  (into {}
        (map (fn [[k v]] [(subs k 2) v]))
        (partition 2 args)))

(defn -main
  "clojure -M -m futon3c.giraffe.run --edges <file> [--only <ids-file>]
  [--root <dir>] [--run-id <id>] [--source tickets|missions|missions-excursions]
  [--repos <comma-separated repo roots, missions source only>]

  Runs the chosen source adapter (default tickets) plus the edges
  file, the Giraffe pass, and writes one run directory.  Prints the
  run dir and manifest path."
  [& args]
  (let [{:strs [edges only root run-id source repos]} (merge-args args)
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
        adapter-result (if missions?
                         ((if combined? excursions/missions-and-excursions->input gm/missions->input)
                          {:repos repo-roots
                                              :edges-file edges
                                              :only only-ids})
                         (gt/tickets->input
                          {:tickets-dir default-tickets-dir
                           :index-file default-index-file
                           :edges-file edges
                           :only only-ids}))
        pass-output ((if combined? excursions/combined-pass gp/pass) (:input adapter-result))
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
