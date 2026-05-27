(ns futon3c.watcher.multi
  "Substrate-2 phase-4.5 multi-repo watcher — in-JVM service edition.

   Polls each watched root on a fixed-delay schedule, dispatches per-file
   ingest for changed files, and emits per-cycle heartbeats and
   commit-vertex catch-up.

   Service shape (matches futon3c.portfolio_inference.scheduler):
     start!  — start the watcher loop with the given options
     stop!   — stop and clear state
     status  — current state snapshot
     tick!   — fire one cycle on demand (manual)

   Ported from /home/joe/code/futon3/scripts/multi_watcher.clj
   (bb script + clojure.main wrapper). bb-isms replaced; the per-file
   ingest call is now a direct function call into
   futon3c.watcher.file-ingest (no subprocess); commit-vertex live
   ingestion uses futon3c.watcher.commit-ingest. WatchService mode is
   not ported in v0 — poll-mode only. See E-live-means-live for full
   migration plan."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.cyder :as cyder]
            [futon3c.watcher.commit-ingest :as commit-ingest]
            [futon3c.watcher.file-ingest :as file-ingest])
  (:import [java.time Instant]
           [java.util.concurrent
            Executors ScheduledExecutorService TimeUnit]))

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(def WATCHED-EXTS #{"clj" "cljs" "cljc" "el" "py" "flexiarg" "md"})
(def NOISE-PATTERN
  #"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo|pytest_cache|venv|state)/|/node_modules/|/target/|/out/|/__pycache__/")

(def VERTEX-TYPES ["code/v05/namespace" "code/v05/var" "code/v05/test"])
(def RENAMED-LINK-TYPE "edge/renamed-to")

(def ^:private mission-doc-pattern
  #"/holes/missions/M-[^/]+\.md$")

(declare !state)

;; ---------- HTTP write surface ----------

(defn directed-endpoints [hx-type endpoints]
  (if (and (#{"code/v05/watcher-event" RENAMED-LINK-TYPE} hx-type)
           (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

(defn post-hyperedge!
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))]
    (try
      (let [resp (http/post (str FUTON1A "/api/alpha/hyperedge")
                            {:headers {"Content-Type" "application/json"
                                       "X-Penholder" PENHOLDER}
                             :body (json/generate-string payload)
                             :throw false})]
        {:ok? (= 200 (:status resp))})
      (catch Exception _ {:ok? false}))))

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER}
                            :throw false
                            :timeout 5000})]
    (if (= 200 (:status resp))
      (edn/read-string (:body resp))
      (throw (ex-info "HTTP non-200"
                      {:url url :status (:status resp) :body (:body resp)})))))

(defn- type-str [x]
  (cond
    (keyword? x) (if (namespace x)
                   (str (namespace x) "/" (name x))
                   (name x))
    (string? x) x
    :else (str x)))

(defn- prop-get [h k]
  (let [props (:hx/props h)
        ks (cond
             (keyword? k) [k (name k)]
             (string? k) [k (keyword k)]
             :else [k])]
    (some #(when (contains? props %) (get props %)) ks)))

(defn- real-endpoints [h]
  (vec (remove #(str/starts-with? % "dir:") (:hx/endpoints h))))

(defn fetch-of-type
  ([hx-type label] (fetch-of-type hx-type label nil))
  ([hx-type label source-file]
   (let [query (cond-> (str FUTON1A "/api/alpha/hyperedges?type="
                            (java.net.URLEncoder/encode hx-type "UTF-8")
                            "&repo="
                            (java.net.URLEncoder/encode label "UTF-8"))
                 source-file
                 (str "&source-file="
                      (java.net.URLEncoder/encode source-file "UTF-8")))
         r (http-get-edn query)]
     (vec (or (:hyperedges r) [])))))

(defn source-file-vertices [label path]
  (->> VERTEX-TYPES
       (mapcat #(fetch-of-type % label path))
       vec))

(defn- primary-endpoint [h] (first (real-endpoints h)))

(defn- local-name [qname]
  (when (string? qname)
    (let [i (str/last-index-of qname "/")]
      (if (neg? i) qname (subs qname (inc i))))))

(defn- rename-pairing-key [h]
  (let [t (type-str (:hx/type h))]
    (case t
      "code/v05/var"       [t (local-name (prop-get h :var/qname))]
      "code/v05/test"      [t (local-name (prop-get h :test/qname))]
      "code/v05/namespace" [t :namespace]
      [t (primary-endpoint h)])))

(defn deterministic-rename-pairs [old-vertices new-vertices]
  (let [survivor-eps (clojure.set/intersection
                      (set (map primary-endpoint old-vertices))
                      (set (map primary-endpoint new-vertices)))
        old-left (remove #(survivor-eps (primary-endpoint %)) old-vertices)
        new-left (remove #(survivor-eps (primary-endpoint %)) new-vertices)
        old-by-key (group-by rename-pairing-key old-left)
        new-by-key (group-by rename-pairing-key new-left)]
    (vec
     (for [[k olds] old-by-key
           :let [news (get new-by-key k)]
           :when (and (= 1 (count olds)) (= 1 (count news)))]
       {:from (first olds) :to (first news)}))))

(defn reupsert-hyperedge! [h merged-props]
  (post-hyperedge! (type-str (:hx/type h))
                   (real-endpoints h)
                   (:hx/labels h)
                   merged-props))

(defn mark-vertices-stale! [vertices reason extra-props]
  (let [ts (System/currentTimeMillis)]
    (reduce (fn [acc h]
              (let [merged (merge (or (:hx/props h) {})
                                  {"edge/witness-stale" true
                                   "edge/witness-stale-ts" ts
                                   "edge/witness-stale-reason" reason}
                                  extra-props)
                    ok? (:ok? (reupsert-hyperedge! h merged))]
                (update acc (if ok? :written :failed) inc)))
            {:written 0 :failed 0}
            vertices)))

(defn emit-renamed-link!
  [{:keys [from to label from-path to-path hash from-label to-label]}]
  (post-hyperedge!
   RENAMED-LINK-TYPE
   [(primary-endpoint from) (primary-endpoint to)]
   (cond-> ["phase-4.6" label "renamed-to"]
     (and from-label to-label) (conj "cross-root"))
   (cond-> {"repo" label "phase" 4.6
            "from-file" from-path "to-file" to-path
            "hash" hash "ts" (System/currentTimeMillis)}
     from-label (assoc "from-repo" from-label)
     to-label (assoc "to-repo" to-label))))

;; ---------- file walk ----------

(defn- file-ext [^String path]
  (let [i (str/last-index-of path ".")]
    (when (and i (> i 0)) (subs path (inc i)))))

(defn watched? [path]
  (let [norm (str/replace (str path) "\\" "/")
        ext (file-ext (str path))
        mission-doc? (boolean (re-find mission-doc-pattern norm))
        sorry-registry? (file-ingest/sorry-registry-path? norm)]
    (and (or (and ext (WATCHED-EXTS ext)) sorry-registry?)
         (not (re-find NOISE-PATTERN norm))
         (or sorry-registry?
             (not= ext "md")
             mission-doc?))))

(defn- mark-subtask!
  [subtask]
  (let [progress-at (Instant/now)]
    (swap! !state
           (fn [s]
             (if (map? s)
               (assoc s
                      :last-subtask subtask
                      :last-progress-at progress-at)
               s)))
    (cyder/touch! "multi-watcher")))

(defn noise-dir? [dir]
  (let [norm (str/replace (str dir) "\\" "/")]
    (boolean (re-find NOISE-PATTERN (str norm "/")))))

(defn file-fingerprint [^String path]
  (let [f (java.io.File. path)]
    {:mtime (.lastModified f) :size (.length f)}))

(defn walk-root [root]
  (->> (file-seq (java.io.File. ^String root))
       (filter #(.isFile ^java.io.File %))
       (map #(.getPath ^java.io.File %))
       (filter watched?)
       (map (fn [p] [p (file-fingerprint p)]))
       (into {})))

;; ---------- B-3 v0: deletion + rename detection ----------

(defn sha-256 [^String path]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bs (try (.readAllBytes (io/input-stream path))
                (catch Exception _ (byte-array 0)))]
    (.update md bs)
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn enriched-snapshot [root]
  (->> (walk-root root)
       (map (fn [[p meta]] [p (assoc meta :hash (sha-256 p))]))
       (into {})))

(defn changed-fingerprint? [old-meta new-meta]
  (or (nil? old-meta)
      (not= (select-keys old-meta [:mtime :size])
            (select-keys new-meta [:mtime :size]))))

(defn incremental-snapshot [root cache]
  (let [fingerprints (walk-root root)]
    (reduce-kv
     (fn [acc path meta]
       (let [old-meta (get cache path)
             hash (if (changed-fingerprint? old-meta meta)
                    (sha-256 path)
                    (or (:hash old-meta) (sha-256 path)))]
         (assoc acc path (assoc meta :hash hash))))
     {}
     fingerprints)))

(defn detect-moves-and-deletes [cache snapshot]
  (let [vacated  (sort (clojure.set/difference (set (keys cache))
                                                (set (keys snapshot))))
        appeared (sort (clojure.set/difference (set (keys snapshot))
                                                (set (keys cache))))
        appeared-by-hash (group-by #(get-in snapshot [% :hash]) appeared)
        vacated-by-hash  (group-by #(get-in cache    [% :hash]) vacated)
        rename-pairs (for [[h v-paths] vacated-by-hash
                           :let [a-paths (get appeared-by-hash h)]
                           :when (and h (seq a-paths))
                           :let [pairs (map vector v-paths a-paths)]
                           [v a] pairs]
                       {:from v :to a :hash h})
        renamed-from (set (map :from rename-pairs))
        renamed-to   (set (map :to rename-pairs))
        deleted (vec (remove renamed-from vacated))
        added   (vec (remove renamed-to appeared))]
    {:vacated (vec vacated) :appeared (vec appeared)
     :renamed (vec rename-pairs) :deleted deleted :added added}))

;; ---------- ingest dispatch (in-JVM, no subprocess) ----------

(defn ingest-event!
  "Run per-file ingest for `path` under (root, label) by direct
   function call into file-ingest/dispatch!. Emits the watcher-event
   evidence hyperedge on completion. Returns the dispatch result."
  [{:keys [path root label run-id event-n source]}]
  (let [t-start (System/currentTimeMillis)
        result (try
                 (file-ingest/dispatch! {:path path :root root :label label})
                 (catch Exception e
                   {:status :error :error (.getMessage e)}))
        dur (- (System/currentTimeMillis) t-start)
        evidence-id (str root "/run-" run-id "/event-" event-n)
        ingest-exit (case (:status result)
                      :ingested 0
                      :mission-doc 0
                      :unhandled 0
                      :error 1
                      0)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.5" label "watcher-event"]
     {"repo" label "phase" 4.5 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis) "file" path
      "ingest-exit" ingest-exit "duration-ms" dur "source" source})
    (println (format "[%s] %s status=%s %dms"
                     source path (name (or (:status result) :error)) dur))
    (when (= :error (:status result))
      (binding [*out* *err*]
        (println "  error:" (:error result))))
    result))

;; ---------- heartbeat / event hyperedges ----------

(defn heartbeat!
  [{:keys [root label run-id cycle-n files-seen files-changed
           n-deleted n-renamed n-added n-cross-root-moves]}]
  (let [evidence-id (str root "/run-" run-id "/heartbeat-" cycle-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str cycle-n)]
     ["v05" "phase-4.5" label "heartbeat"]
     {"repo" label "phase" 4.5 "run-id" run-id "cycle" cycle-n
      "ts" (System/currentTimeMillis) "files-seen" files-seen
      "files-changed" files-changed
      "n-deleted" (or n-deleted 0)
      "n-renamed" (or n-renamed 0)
      "n-added" (or n-added 0)
      "n-cross-root-moves" (or n-cross-root-moves 0)
      "source" "heartbeat"})))

(defn deletion-event!
  [{:keys [path root label run-id event-n hash]}]
  (let [evidence-id (str root "/run-" run-id "/deletion-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.5" label "deletion-event"]
     {"repo" label "phase" 4.5 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis)
      "file" path "last-known-hash" hash "source" "deletion"})
    (println (format "[deletion] %s" path))))

(defn rename-event!
  [{:keys [from to root label run-id event-n hash]}]
  (let [evidence-id (str root "/run-" run-id "/rename-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.5" label "rename-event"]
     {"repo" label "phase" 4.5 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis)
      "from" from "to" to "hash" hash "source" "rename"})
    (println (format "[rename] %s → %s" from to))))

(defn cross-root-move-event!
  [{:keys [from to from-root to-root from-label to-label run-id event-n hash]}]
  (let [evidence-id (str from-root "/run-" run-id "/cross-root-move-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.6" from-label to-label "cross-root-move-event"]
     {"repo" from-label "from-repo" from-label "to-repo" to-label
      "phase" 4.6 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis)
      "from" from "to" to
      "from-root" from-root "to-root" to-root
      "hash" hash "source" "cross-root-move"})
    (println (format "[cross-root-move] %s → %s" from to))))

(defn handle-deletion!
  [{:keys [path root label run-id event-n hash]}]
  (let [victims (source-file-vertices label path)
        stale (mark-vertices-stale! victims "deletion"
                                    {"edge/witness-stale-source-file" path
                                     "edge/witness-stale-last-known-hash" hash})]
    (deletion-event! {:path path :root root :label label
                      :run-id run-id :event-n event-n :hash hash})
    (println (format "[deletion-stale] %s vertices=%d failed=%d"
                     path (:written stale) (:failed stale)))))

(defn handle-rename!
  [{:keys [from to root label run-id event-n hash]}]
  (let [old-vertices (source-file-vertices label from)]
    (ingest-event! {:path to :root root :label label
                    :run-id run-id :event-n event-n
                    :source "rename-ingest"})
    (let [new-vertices (source-file-vertices label to)
          survivor-eps (clojure.set/intersection
                        (set (map primary-endpoint old-vertices))
                        (set (map primary-endpoint new-vertices)))
          stale-old (vec (remove #(survivor-eps (primary-endpoint %)) old-vertices))
          stale (mark-vertices-stale! stale-old "rename"
                                      {"edge/witness-stale-source-file" from
                                       "edge/witness-stale-renamed-to" to
                                       "edge/witness-stale-last-known-hash" hash})
          pairs (deterministic-rename-pairs old-vertices new-vertices)
          link-stats (reduce (fn [acc {:keys [from to]}]
                               (let [ok? (:ok? (emit-renamed-link!
                                                {:from from :to to :label label
                                                 :from-path from :to-path to
                                                 :hash hash}))]
                                 (update acc (if ok? :written :failed) inc)))
                             {:written 0 :failed 0}
                             pairs)]
      (rename-event! {:from from :to to :hash hash
                      :root root :label label
                      :run-id run-id :event-n event-n})
      (println (format "[rename-cascade] %s → %s stale=%d stale-failed=%d links=%d link-failed=%d"
                       from to
                       (:written stale) (:failed stale)
                       (:written link-stats) (:failed link-stats))))))

(defn handle-cross-root-move!
  [{:keys [from to from-root to-root from-label to-label run-id event-n hash]}]
  (let [old-vertices (source-file-vertices from-label from)]
    (ingest-event! {:path to :root to-root :label to-label
                    :run-id run-id :event-n event-n
                    :source "cross-root-move-ingest"})
    (let [new-vertices (source-file-vertices to-label to)
          stale (mark-vertices-stale! old-vertices "cross-root-move"
                                      {"edge/witness-stale-source-file" from
                                       "edge/witness-stale-renamed-to" to
                                       "edge/witness-stale-last-known-hash" hash
                                       "edge/witness-stale-to-repo" to-label})
          pairs (deterministic-rename-pairs old-vertices new-vertices)
          link-stats (reduce (fn [acc {:keys [from to]}]
                               (let [ok? (:ok? (emit-renamed-link!
                                                {:from from :to to :label from-label
                                                 :from-path from :to-path to
                                                 :hash hash
                                                 :from-label from-label
                                                 :to-label to-label}))]
                                 (update acc (if ok? :written :failed) inc)))
                             {:written 0 :failed 0}
                             pairs)]
      (cross-root-move-event! {:from from :to to
                               :from-root from-root :to-root to-root
                               :from-label from-label :to-label to-label
                               :run-id run-id :event-n event-n :hash hash})
      (println (format "[cross-root-cascade] %s → %s stale=%d stale-failed=%d links=%d link-failed=%d"
                       from to
                       (:written stale) (:failed stale)
                       (:written link-stats) (:failed link-stats))))))

(defn detect-cross-root-moves [plans]
  (let [deleteds (for [{:keys [root label cache moves]} plans
                       path (:deleted moves)
                       :let [hash (get-in cache [path :hash])]
                       :when hash]
                   {:from path :from-root root :from-label label :hash hash})
        addeds (for [{:keys [root label snapshot moves]} plans
                     path (:added moves)
                     :let [hash (get-in snapshot [path :hash])]
                     :when hash]
                 {:to path :to-root root :to-label label :hash hash})
        deleted-by-hash (group-by :hash deleteds)
        added-by-hash (group-by :hash addeds)]
    (vec
     (for [[hash ds] deleted-by-hash
           :let [as (get added-by-hash hash)]
           :when (and (= 1 (count ds)) (= 1 (count as))
                      (not= (:from-root (first ds)) (:to-root (first as))))]
       (merge (first ds) (first as))))))

;; ---------- cycle driver ----------

(defn build-plan
  [{:keys [path label]} per-root-cache]
  (mark-subtask! {:phase :build-plan :repo label})
  (let [cache (get @per-root-cache path)
        first-cycle? (empty? cache)
        snapshot (if (seq cache)
                   (incremental-snapshot path cache)
                   (enriched-snapshot path))
        changed (filter (fn [[p {:keys [hash]}]]
                          (not= hash (get-in cache [p :hash])))
                        snapshot)
        changed-paths (sort (map first changed))
        moves (detect-moves-and-deletes cache snapshot)
        renamed-to (set (map :to (:renamed moves)))]
    {:root path
     :label label
     :snapshot snapshot
     :cache cache
     :first-cycle? first-cycle?
     :moves moves
     :ingest-paths (vec (remove renamed-to changed-paths))}))

;; ---------- commit-vertex live ingestion ----------

(defn query-repo-vars-by-file
  "Queries substrate-2 for all var/test vertices in repo-label. Returns
   {rel-path → [unprefixed-qnames…]} for use as the file->vars
   parameter to commit-ingest's :edits resolution.

  Cost: one HTTP call to /api/alpha/hyperedges per vertex-type per
   repo per cycle. {} on any HTTP failure (caller skips :edits)."
  [repo-label]
  (mark-subtask! {:phase :query-repo-vars-by-file :repo repo-label})
  (try
    (reduce
     (fn [acc t]
       (let [resp (http-get-edn
                   (str FUTON1A "/api/alpha/hyperedges?type=" t
                        "&repo=" repo-label))
             edges (:hyperedges resp)]
         (reduce (fn [m e]
                   (let [src-file (some-> e :hx/props :source-file)
                         qname (first (:hx/endpoints e))]
                     (cond-> m
                       (and (string? src-file) (string? qname))
                       (update src-file (fnil conj []) qname))))
                 acc edges)))
     {}
     ["code/v05/var" "code/v05/test"])
    (catch Exception _ {})))

(defn ingest-new-commits-for-root!
  "Per-cycle commit-vertex catch-up. Failures are isolated; do not break
   file-watch ingestion."
  [{:keys [root label cycle-n]}]
  (mark-subtask! {:phase :commit-ingest :repo label :root root :cycle-n cycle-n})
  (try
    (let [vars-by-file (query-repo-vars-by-file label)
          file->vars (fn [path] (get vars-by-file path))
          report (commit-ingest/ingest-new-commits!
                  {:repo-root root
                   :repo-label label
                   :file->vars file->vars})]
      (when (pos? (:n-ingested report))
        (println (format "[cycle %d] %s: ingested %d new commit(s); latest=%s%s"
                         cycle-n label
                         (:n-ingested report)
                         (apply str (take 7 (or (:latest-sha report) "")))
                         (if (pos? (:n-failed report))
                           (format " (FAILED=%d)" (:n-failed report))
                           "")))))
    (catch Exception e
      (binding [*out* *err*]
        (println (format "[cycle %d] %s: commit-ingest error: %s"
                         cycle-n label (.getMessage e)))))))

(declare stop-requested? status)

(defn run-cycle!
  [{:keys [roots per-root-cache run-id event-n cycle-n cold-scan? commit-ingest?]
    :or {commit-ingest? true}}]
  (let [n (swap! cycle-n inc)
        plans (vec (map #(build-plan % per-root-cache) roots))
        cross-root-moves (detect-cross-root-moves plans)
        cross-root-froms (set (map :from cross-root-moves))
        cross-root-tos (set (map :to cross-root-moves))]
    (doseq [{:keys [root label snapshot cache moves ingest-paths first-cycle?]} plans]
      (let [{:keys [renamed deleted added]} moves
            ingest-paths (vec (remove cross-root-tos ingest-paths))
            deleted (vec (remove cross-root-froms deleted))
            cross-root-count (count (filter #(or (= root (:from-root %))
                                                 (= root (:to-root %)))
                                            cross-root-moves))
            ;; Suppress per-file dispatch on the first cycle when
            ;; cold-scan? is false — matches the bb watcher's
            ;; --no-cold-scan behaviour. Subsequent cycles dispatch
            ;; only files whose hash actually moved.
            dispatch-paths (if (and first-cycle? (not cold-scan?))
                             []
                             ingest-paths)]
        (when (seq dispatch-paths)
          (mark-subtask! {:phase :file-ingest-batch :repo label :count (count dispatch-paths)})
          (println (format "[cycle %d] %s: %d/%d files changed"
                           n label (count dispatch-paths) (count snapshot)))
          (doseq [p dispatch-paths]
            (mark-subtask! {:phase :file-ingest :repo label :path p})
            (let [ev-n (swap! event-n inc)]
              (ingest-event! {:path p :root root :label label
                              :run-id run-id :event-n ev-n
                              :source (if (and first-cycle? cold-scan?)
                                        "cold-scan"
                                        "fs-watch")}))))
        (doseq [p deleted]
          (mark-subtask! {:phase :deletion :repo label :path p})
          (let [ev-n (swap! event-n inc)]
            (handle-deletion! {:path p :root root :label label
                               :run-id run-id :event-n ev-n
                               :hash (get-in cache [p :hash])})))
        (doseq [{:keys [from to hash]} renamed]
          (mark-subtask! {:phase :rename :repo label :from from :to to})
          (let [ev-n (swap! event-n inc)]
            (handle-rename! {:from from :to to :hash hash
                             :root root :label label
                             :run-id run-id :event-n ev-n})))
        (when-not (stop-requested?)
          (heartbeat! {:root root :label label :run-id run-id
                       :cycle-n n
                       :files-seen (count snapshot)
                       :files-changed (count ingest-paths)
                       :n-deleted (count deleted)
                       :n-renamed (count renamed)
                       :n-added (count added)
                       :n-cross-root-moves cross-root-count}))
        (when (and commit-ingest? (not (stop-requested?)))
          (ingest-new-commits-for-root!
           {:root root :label label :cycle-n n}))
        (swap! per-root-cache assoc root snapshot)))
    (doseq [{:keys [from to from-root to-root from-label to-label hash]} cross-root-moves]
      (mark-subtask! {:phase :cross-root-move
                      :from-repo from-label
                      :to-repo to-label
                      :from from
                      :to to})
      (let [ev-n (swap! event-n inc)]
        (handle-cross-root-move! {:from from :to to
                                  :from-root from-root :to-root to-root
                                  :from-label from-label :to-label to-label
                                  :run-id run-id :event-n ev-n :hash hash})))
    (mark-subtask! {:phase :idle :cycle-n n})))

;; ---------- service ----------

(defonce ^{:doc "Watcher service state. nil when stopped; otherwise:
                 {:executor <ScheduledExecutorService>
                  :run-id <epoch-millis>
                  :event-n <atom int>
                 :cycle-n <atom int>
                 :per-root-cache <atom {root → snapshot}>
                 :roots <vec of {:path :label}>
                 :interval-ms <int>
                  :commit-ingest? <boolean>
                  :last-cycle-started-at <Instant|nil>
                  :last-cycle-finished-at <Instant|nil>
                  :last-progress-at <Instant|nil>
                  :last-error <string|nil>
                  :last-subtask <map|nil>
                  :stopping? <boolean>}"}
  !state (atom nil))

(defn stop-requested?
  "Return non-nil when the watcher should stop before more per-root work."
  []
  (or (Thread/interrupted)
      (true? (:stopping? @!state))
      (nil? @!state)))

(defn- safe-cycle! [state]
  (when-not (stop-requested?)
    (let [started-at (Instant/now)]
      (swap! !state
             (fn [s]
               (if (map? s)
                 (assoc s
                        :last-cycle-started-at started-at
                        :last-progress-at started-at
                        :last-error nil
                        :last-subtask {:phase :cycle-start})
                 s))))
    (try
      (run-cycle! state)
      (swap! !state
             (fn [s]
               (if (map? s)
                 (assoc s
                        :last-cycle-finished-at (Instant/now)
                        :last-error nil)
                 s)))
      (cyder/touch! "multi-watcher")
      (catch Throwable t
        (swap! !state
               (fn [s]
                 (if (map? s)
                   (assoc s
                          :last-cycle-finished-at (Instant/now)
                          :last-error (.getMessage t)
                          :last-subtask {:phase :cycle-error})
                   s)))
        (cyder/touch! "multi-watcher")
        (binding [*out* *err*]
          (println "[multi.run-cycle!] uncaught:" (.getMessage t)))))))

(defn start!
  "Start the watcher loop with the given options. Idempotent — if
   already running, returns the existing status without restarting.

   Opts:
     :roots         — vec of {:path \"/abs/path\" :label \"futonX-d\"}
     :interval-ms   — poll cadence (default 5000)
     :cold-scan?    — emit per-file ingest for every file on first cycle
                      (default false; matches bb watcher's --no-cold-scan).
                      Set true on a fresh substrate-2 to force initial
                      population.
     :commit-ingest? — run the per-cycle commit-vertex catch-up
                       (default true). Set false when live file-event
                       ingestion is wanted without the slower commit sidecar.

   Returns the same shape as `status`."
  [{:keys [roots interval-ms cold-scan? commit-ingest?]
    :or {interval-ms 5000 cold-scan? false commit-ingest? true}}]
  (when-let [s @!state]
    (when (:executor s)
      (throw (ex-info "watcher already running; call stop! first or use status"
                      {:running true}))))
  (let [run-id (System/currentTimeMillis)
        event-n (atom 0)
        cycle-n (atom 0)
        per-root-cache (atom (zipmap (map :path roots) (repeat {})))
        executor (Executors/newSingleThreadScheduledExecutor
                  (reify java.util.concurrent.ThreadFactory
                    (newThread [_ r]
                      (doto (Thread. r "futon3c-watcher")
                        (.setDaemon true)))))
        ;; Indirect through vars (#'safe-cycle!) so reloads of run-cycle!
        ;; / safe-cycle! are picked up on next tick — reload-safety per
        ;; feedback_reload_safety.md.
        cycle-state {:roots roots
                     :per-root-cache per-root-cache
                     :run-id run-id
                     :event-n event-n
                     :cycle-n cycle-n
                     :cold-scan? cold-scan?
                     :commit-ingest? commit-ingest?}
        task #(#'safe-cycle! cycle-state)]
    (.scheduleWithFixedDelay executor task 0 interval-ms TimeUnit/MILLISECONDS)
    (reset! !state {:executor executor
                    :run-id run-id
                    :event-n event-n
                    :cycle-n cycle-n
                    :per-root-cache per-root-cache
                    :roots roots
                    :interval-ms interval-ms
                    :cold-scan? cold-scan?
                    :commit-ingest? commit-ingest?
                    :last-cycle-started-at nil
                    :last-cycle-finished-at nil
                    :last-progress-at nil
                    :last-error nil
                    :last-subtask {:phase :boot}
                    :stopping? false})
    (println (format "[futon3c.watcher.multi] started run-id=%d roots=%d interval-ms=%d"
                     run-id (count roots) interval-ms))
    (doseq [{:keys [path label]} roots]
      (println (format "  watch %s  →  label=%s" path label)))
    (status nil)))

(defn stop!
  "Stop the watcher loop. Idempotent — no-op if not running."
  []
  (when-let [s @!state]
    (swap! !state assoc :stopping? true)
    (when-let [^ScheduledExecutorService ex (:executor s)]
      (.shutdownNow ex)
      (.awaitTermination ex 2 TimeUnit/SECONDS))
    (reset! !state nil)
    (println "[futon3c.watcher.multi] stopped"))
  nil)

(defn status
  "Current watcher state — safe to call whether running or stopped."
  [_]
  (when-let [s @!state]
    (-> s
        (dissoc :executor :event-n :cycle-n :per-root-cache)
        (assoc :running? (some? (:executor s))
               :event-n @(:event-n s)
               :cycle-n @(:cycle-n s)
               :last-cycle-started-at (some-> (:last-cycle-started-at s) str)
               :last-cycle-finished-at (some-> (:last-cycle-finished-at s) str)
               :last-progress-at (some-> (:last-progress-at s) str)
               :n-roots (count (:roots s))))))

(defn tick!
  "Manually fire one cycle on demand. Useful for tests + interactive
   debugging via Drawbridge. Errors propagate."
  []
  (let [s @!state]
    (when-not (and s (:executor s))
      (throw (ex-info "watcher not running; call start! first" {})))
    (run-cycle! {:roots (:roots s)
                 :per-root-cache (:per-root-cache s)
                 :run-id (:run-id s)
                 :event-n (:event-n s)
                 :cycle-n (:cycle-n s)})
    (status nil)))
