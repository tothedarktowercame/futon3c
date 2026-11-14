(ns futon3c.logic.mission-doc-invariants
  "M-repl-circus: two read-only invariants over the watched mission /
  excursion / ticket / campaign documents (Joe, 2026-11-14: \"Can you please
  deduplicate and give everything a clear status? Both would be good
  invariants to enforce going forward.\").

  Invariant 1 — ONE DOCUMENT PER ID:
  - no two mission/excursion/ticket/campaign files share a file name
    (external id) across the watcher roots (futon3c.watcher.roots/watch-roots,
    honouring :only/:exclude; nested holes/ copies excluded via
    futon3c.watcher.file-ingest/nested-holes-copy?) and the extra ledgers
    (default: ~/code/giraffe-reread-requisitions);
  - in futon1b no external id has more than one :mission/doc |
    :excursion/doc | :ticket/doc entity (sidecar
    ~/code/futon1b/migration-store-21/fts5-evidence.db, ent_node, read-only
    via JDBC with mode=ro).

  Invariant 2 — A READABLE STATUS: every such document's status, as the
  watcher's parser classifies it
  (futon3c.peripheral.mission-control-backend/parse-mission-md for M-,
  parse-excursion-md for E- — including the Requisition fallback — and
  futon3c.watcher.file-ingest/parse-ticket-md for T-), is neither :unknown
  nor nil. C- campaign files have no watcher parser yet (Phase 2b note in
  file_ingest.clj); their Status header is classified with
  classify-status directly. Violations list the path and the raw status
  line so a human can fix the document.

  Pure functions over an explicit doc list; nothing here writes. The
  violations map is invariant-runner shaped ({:domain-key [violations]}),
  so a runner domain spec can use {:check futon3c.logic.mission-doc-invariants/check}
  with {:input-fn scan-defaults}. Read-only by construction: file reads,
  one read-only SQLite connection."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.watcher.file-ingest :as fi]
            [futon3c.watcher.roots :as roots])
  (:import [java.sql DriverManager]))

;; ---------------------------------------------------------------------------
;; Document discovery
;; ---------------------------------------------------------------------------

(def default-ledger-paths
  "Extra ledgers scanned in addition to watch-roots. The giraffe reread
  requisition ledger keeps one E-kimi-task-*.md per requisition under
  holes/excursions/, so the same path predicates apply to it."
  ["/home/joe/code/giraffe-reread-requisitions"])

(def default-db-path
  "futon1b sidecar store, read read-only for entity-level duplicate
  external ids. Named explicitly so tests can point it at a fixture."
  "/home/joe/code/futon1b/migration-store-21/fts5-evidence.db")

(def ^:private skip-dirs
  "Directories never walked. The watcher itself never ingests documents
  from VCS/build trees, and .git in particular is huge."
  #{".git" "node_modules" "target" ".shadow-cljs" ".cpcache"})

(defn- canonical [x]
  (-> (io/file (str x)) .getCanonicalPath))

(defn- under-any? [path prefixes]
  (some (fn [prefix]
          (or (= path prefix)
              (str/starts-with? path (str prefix "/"))))
        prefixes))

(defn- excluded? [path excludes]
  (under-any? path excludes))

(defn- walk-files
  "All regular files under DIR (recursive), skipping skip-dirs, paths
  outside :only prefixes and paths inside :exclude prefixes."
  [{:keys [path only exclude]}]
  (let [root (canonical path)
        only (mapv canonical (or only [root]))
        exclude (mapv canonical (or exclude []))
        out (atom [])]
    (letfn [(walk [^java.io.File dir]
              (doseq [^java.io.File f (.listFiles dir)]
                (let [p (.getCanonicalPath f)]
                  (cond
                    (.isDirectory f)
                    (when (and (not (skip-dirs (.getName f)))
                               (not (excluded? p exclude))
                               (or (under-any? p only) (= p root)))
                      (walk f))

                    (.isFile f)
                    (when (and (or (under-any? p only) (= p root))
                               (not (excluded? p exclude)))
                      (swap! out conj p))))))]
      (when (.isDirectory (io/file root))
        (walk (io/file root)))
      @out)))

(defn classify-doc-path
  "Which kind of watched document PATH is, or nil when it is not one.
  Delegates to the watcher's own path predicates (including the
  nested-holes-copy exclusion), so this namespace can never disagree
  with what the watcher ingests."
  [path]
  (cond
    (fi/mission-doc-path? path)    :mission
    (fi/excursion-doc-path? path)  :excursion
    (fi/ticket-doc-path? path)     :ticket
    (fi/campaign-doc-path? path)   :campaign
    :else nil))

(defn scan-root
  "All watched documents under one root spec ({:path :label :only :exclude}).
  Returns {:path :kind :external-id :label}. The external id is the file
  name — the value the watcher stamps on the futon1b entity
  (external-id \"M-…\"/\"E-…\"/\"T-…\") — so file-name equality is id
  equality for invariant 1."
  [{:keys [label] :as root}]
  (into []
        (comp (map (fn [p]
                     (when-let [kind (classify-doc-path p)]
                       {:path p
                        :kind kind
                        :external-id (.getName (io/file p))
                        :label label})))
              (remove nil?))
        (walk-files root)))

(defn scan-docs
  "Documents across WATCH-ROOTS (default roots/watch-roots) plus LEDGER-PATHS
  (default default-ledger-paths, scanned as bare roots with the same
  predicates)."
  ([] (scan-docs roots/watch-roots default-ledger-paths))
  ([watch-roots ledger-paths]
   (into []
         (mapcat scan-root)
         (concat (map #(update % :path canonical) watch-roots)
                 (map (fn [p] {:path p :label "ledger"}) ledger-paths)))))

;; ---------------------------------------------------------------------------
;; Invariant 1 — one document per id
;; ---------------------------------------------------------------------------

(defn duplicate-file-name-violations
  "Invariant 1, file half: external ids (file names) claimed by more than
  one document across the scanned roots."
  [docs]
  (->> docs
       (group-by :external-id)
       (filter (fn [[_ ds]] (> (count ds) 1)))
       (map (fn [[ext-id ds]]
              {:invariant :one-document-per-id
               :scope :files
               :external-id ext-id
               :paths (mapv :path (sort-by :path ds))}))
       (sort-by :external-id)
       vec))

(defn- sqlite-query
  "Rows from SQLITE db at PATH, opened read-only (uri mode=ro). Returns []
  when the db file is absent (futon1b may not be checked out) — absent
  input is not a violation."
  [path sql]
  (if-not (.exists (io/file (str path)))
    []
    (with-open [conn (DriverManager/getConnection
                      (str "jdbc:sqlite:file:" path "?mode=ro"))]
      (with-open [stmt (.createStatement conn)
                  rs (.executeQuery stmt sql)]
        (let [cols (-> rs .getMetaData .getColumnCount)
              rows (atom [])]
          (while (.next rs)
            (swap! rows conj (mapv #(.getObject rs (inc %)) (range cols))))
          @rows)))))

(def entity-doc-types
  "The futon1b ent_node types that hold one entity per document."
  [":mission/doc" ":excursion/doc" ":ticket/doc"])

(defn duplicate-entity-violations
  "Invariant 1, entity half: futon1b external ids carried by more than one
  :mission/doc | :excursion/doc | :ticket/doc entity."
  ([] (duplicate-entity-violations default-db-path))
  ([db-path]
   (let [types (str/join "," (map #(str "'" % "'") entity-doc-types))
         sql (str "select external_id, count(*) as n, group_concat(id, '|') "
                  "from ent_node where type in (" types ") "
                  "and external_id is not null "
                  "group by external_id having count(*) > 1")]
     (->> (sqlite-query db-path sql)
          (map (fn [[ext-id n ids]]
                 {:invariant :one-document-per-id
                  :scope :futon1b-entities
                  :external-id (str ext-id)
                  :entity-count (long n)
                  :entity-ids (str/split (str ids) #"\|")}))
          (sort-by :external-id)
          vec))))

;; ---------------------------------------------------------------------------
;; Invariant 2 — a readable status
;; ---------------------------------------------------------------------------

(defn- campaign-raw-status
  "Campaign documents have no watcher parser yet (file_ingest Phase 2b
  comment), so pull the Status header the same way the mission parser
  does: bold-key, bold-key-outer-colon, plain or ## heading, first 80
  lines."
  [path]
  (let [header (->> (str/split-lines (slurp path))
                    (take 80)
                    (str/join "\n"))]
    (or (some-> (re-find #"(?mi)^\*\*Status:\*\*\s*(.+)$" header) second str/trim)
        (some-> (re-find #"(?mi)^\*\*Status\*\*:\s*(.+)$" header) second str/trim)
        (some-> (re-find #"(?mi)^(?:#{1,3}\s+)?Status:\s*(.+)$" header) second str/trim))))

(defn doc-status
  "The document's status as the watcher classifies it, plus the raw status
  line. Returns {:status keyword-or-nil :raw-status string-or-nil}."
  [{:keys [path kind label]}]
  (case kind
    :mission
    (let [m (mcb/parse-mission-md path (keyword (or label "unknown")))]
      {:status (:mission/status m) :raw-status (:mission/raw-status m)})
    :excursion
    (let [m (mcb/parse-excursion-md path (keyword (or label "unknown")))]
      {:status (:excursion/status m) :raw-status (:excursion/raw-status m)})
    :ticket
    (let [t (fi/parse-ticket-md path)]
      {:status (:ticket/status t) :raw-status (:ticket/raw-status t)})
    :campaign
    (let [raw (campaign-raw-status path)]
      {:status (mcb/classify-status raw) :raw-status raw})))

(defn unreadable-status-violations
  "Invariant 2: documents whose classified status is :unknown or nil.
  Each violation names the path and the raw status line (nil means no
  status line was found at all)."
  [docs]
  (->> docs
       (keep (fn [doc]
               (let [{:keys [status raw-status]} (doc-status doc)]
                 (when (or (nil? status) (= :unknown status))
                   {:invariant :readable-status
                    :path (:path doc)
                    :kind (:kind doc)
                    :status status
                    :raw-status raw-status}))))
       (sort-by :path)
       vec))

;; ---------------------------------------------------------------------------
;; Aggregate
;; ---------------------------------------------------------------------------

(defn violations
  "Both invariants over an explicit input map:
  {:docs scanned documents
   :db-path futon1b sidecar path (nil skips the entity half)}
  Returns {:one-document-per-id {:files [...] :futon1b-entities [...]}
           :readable-status [...]} — invariant-runner check shaped."
  [{:keys [docs db-path]}]
  {:one-document-per-id
   {:files (duplicate-file-name-violations docs)
    :futon1b-entities (if db-path
                        (duplicate-entity-violations db-path)
                        [])}
   :readable-status (unreadable-status-violations docs)})

(defn check
  "invariant-runner domain check fn: input ({:docs :db-path}) -> violations
  map. Pair with {:input-fn scan-defaults} in a domain spec."
  [input]
  (violations input))

(defn scan-defaults
  "Input for `check` from the real files: watch-roots + ledgers + the
  futon1b sidecar."
  []
  {:docs (scan-docs)
   :db-path default-db-path})

(defn summarize
  "One line per invariant half, for humans."
  [v]
  [(str "one-document-per-id/files: "
        (count (get-in v [:one-document-per-id :files])) " duplicate external ids")
   (str "one-document-per-id/futon1b-entities: "
        (count (get-in v [:one-document-per-id :futon1b-entities])) " duplicate external ids")
   (str "readable-status: "
        (count (:readable-status v)) " documents with unknown/absent status")])
