(ns futon3c.watcher.file-ingest
  "Substrate-2 phase-4.5 per-file ingest. Called by the in-JVM
   watcher (`futon3c.watcher.multi`) on each fs event with the
   file path + repo root + label, plus a cached `root-ctx` (by-ns
   resolution map for the repo).

   Dispatch by file extension / path shape:
     .clj/.cljs/.cljc → vendored Clojure projector (this file)
     .el              → futon3c.watcher.projections.elisp/collect-file
     .py              → futon3c.watcher.projections.python/collect-file
     .flexiarg        → futon3c.watcher.projections.flexiarg/collect-file
     futonN/essays/<slug>/<slug>.md
                       → futon3c.watcher.projections.essay/collect-file
     holes/missions/M-*.md → mission sync push into futon3c + futon1a
     else            → :unhandled (no-op, returns OK)

   Ported from /home/joe/code/futon3/scripts/ingest_one_file.clj
   (bb script). babashka.fs replaced with java.io.File equivalents;
   load-file replaced with proper namespace requires."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.watcher.projections.elisp :as elisp]
            [futon3c.watcher.projections.python :as python]
            [futon3c.watcher.projections.flexiarg :as flexiarg]
            [futon3c.watcher.projections.essay :as essay]))

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def FUTON3C   (or (System/getenv "FUTON3C_URL") "http://localhost:7070"))
(def ^:private home (System/getProperty "user.home"))

(def ^:private mission-doc-pattern
  #"/holes/missions/M-[^/]+\.md$")

(def ^:private excursion-doc-pattern
  #"/holes/missions/E-[^/]+\.md$")

(def ^:private sorry-registry-pattern
  ;; R-A.1 (M-war-machine-first-outing): sorrys.edn relocated data/ → resources/
  ;; (tracked, git=ledger). Matches BOTH so the move is transition-safe.
  #"/futon2/(?:data|resources)/sorrys\.edn$")

(def ^:private mission-cross-ref-type
  "code/v05/mission-cross-ref")

(def ^:private file-to-mission-type
  "code/v05/file→mission")

(def ^:private excursion-to-parent-type
  "code/v05/excursion→parent-mission")

(def directed-types
  #{"code/v05/calls" "code/v05/coverage" "code/v05/vocabulary-use"
    "code/v05/term-defines" "code/v05/contains"
    "code/v05/related-mission"
    "code/v05/file→mission"
    "code/v05/excursion→parent-mission"})

(defn directed-endpoints [hx-type endpoints]
  (if (and (directed-types hx-type) (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

(defn post-hyperedge!
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))
        resp (try
               (http/post (str FUTON1A "/api/alpha/hyperedge")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (and (= 200 (:status resp))
               (or (:hyperedge body) (:hx/id body)))}))

(defn post-hyperedge-doc!
  [{:keys [id hx-type endpoints labels props]}]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  id (assoc "hx/id" id)
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))
        resp (try
               (http/post (str FUTON1A "/api/alpha/hyperedge")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (and (= 200 (:status resp))
               (or (:hyperedge body) (:hx/id body)))}))

;; ---------- mission cross-ref edge emission (T-9d) ----------

(def ^:private mission-id-cache (atom {:t 0 :map {}}))

(defn- fetch-mission-id->vertex-id
  "Query substrate-2 for every current mission-doc hyperedge and return
   a map of mission-id (without the `M-` prefix) → vertex-id (the
   canonical endpoint string). Used to resolve `:mission/cross-refs`
   targets without having to know each cross-ref's source repo."
  []
  (try
    (let [url (str FUTON1A "/api/alpha/hyperedges?type=code/v05/mission-doc&limit=500")
          resp (http/get url {:headers {"Accept" "application/json"}
                              :throw false
                              :timeout 5000})]
      (when (= 200 (:status resp))
        (let [parsed (json/parse-string (:body resp) true)
              hxes (:hyperedges parsed)]
          (into {}
                (keep (fn [hx]
                        (let [props (:hx/props hx)
                              id (or (:mission/id props) (get props "mission/id"))
                              endpoints (:hx/endpoints hx)]
                          (when (and id (seq endpoints))
                            [id (first endpoints)]))))
                hxes))))
    (catch Exception _ {})))

(defn- mission-id->vertex-id-map
  "Cached lookup; refreshes every 30 seconds to amortise the substrate-2
   query across batch ingest passes."
  []
  (let [now (System/currentTimeMillis)
        c @mission-id-cache]
    (if (< (- now (:t c)) 30000)
      (:map c)
      (let [m (fetch-mission-id->vertex-id)]
        (reset! mission-id-cache {:t now :map m})
        m))))

(defn invalidate-mission-id-cache!
  "Force the next cross-ref emission to re-fetch from substrate-2.
   Call this between backfill passes if recent ingests need to be
   visible to subsequent edge-emission calls in the same wall-clock
   second."
  []
  (reset! mission-id-cache {:t 0 :map {}}))

(defn- cross-ref-edge-id
  "Deterministic hyperedge id for a cross-ref edge from SOURCE→TARGET
   vertex ids. Same (source, target) always produces the same id, so
   substrate-2 dedupes on re-ingest instead of accumulating duplicates."
  [source target]
  (str "hx:" mission-cross-ref-type ":" source ":" target))

(defn emit-cross-ref-edges!
  "For each cross-ref name in CROSS-REFS, resolve to a target vertex
   via the substrate-2 mission index and emit a directed cross-ref
   hyperedge from SOURCE-VERTEX-ID. Skips self-references and
   unresolvable names. Returns {:emitted N :unresolved N :failed N}."
  [source-vertex-id source-mission-id cross-refs]
  (let [resolver (mission-id->vertex-id-map)
        results (atom {:emitted 0 :unresolved 0 :failed 0})]
    (doseq [cref cross-refs
            :let [target-id (str/replace-first (str cref) "M-" "")
                  target-vertex (get resolver target-id)]]
      (cond
        (nil? target-vertex)
        (swap! results update :unresolved inc)

        (= target-vertex source-vertex-id)
        (swap! results update :unresolved inc)

        :else
        (let [ok? (:ok? (post-hyperedge-doc!
                         {:id (cross-ref-edge-id source-vertex-id target-vertex)
                          :hx-type mission-cross-ref-type
                          :endpoints [source-vertex-id target-vertex]
                          :labels ["v05" "phase-4.5" "mission-cross-ref"]
                          :props {"mission/source" source-mission-id
                                  "mission/target" target-id}}))]
          (if ok?
            (swap! results update :emitted inc)
            (swap! results update :failed inc)))))
    @results))

;; ---------- end T-9d ----------

(defn post-entity!
  [{:keys [id name type props source external-id]}]
  (let [payload (cond-> {"name" name "type" type}
                  id (assoc "id" id)
                  props (assoc "props" props)
                  source (assoc "source" source)
                  external-id (assoc "external-id" external-id))
        resp (try
               (http/post (str FUTON1A "/api/alpha/entity")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (and (= 200 (:status resp))
               (or (:entity body) (:id body)))}))

;; ---------- file-extension helpers (replaces babashka.fs/extension) ----------

(defn- file-ext [path]
  (let [n (str path)
        i (str/last-index-of n ".")]
    (when (and i (> i 0)) (subs n (inc i)))))

(defn- file-exists? [path]
  (.exists (java.io.File. (str path))))

;; ---------- Clojure projection (vendored from v0) ----------

(def src-exts #{"clj" "cljs" "cljc"})
(def def-forms #{'defn 'defn- 'def 'defmulti 'defmethod 'defprotocol 'defrecord 'deftype})

(defn read-forms [^java.io.File f]
  (with-open [pbr (java.io.PushbackReader. (io/reader f))]
    (binding [*default-data-reader-fn* (fn [_t v] v)]
      (loop [acc []]
        (let [form (try (read {:read-cond :allow :features #{:clj :cljs} :eof ::eof} pbr)
                        (catch Exception _ ::eof))]
          (if (= form ::eof) acc (recur (conj acc form))))))))

(defn ns-form [forms] (some #(when (and (seq? %) (= 'ns (first %))) %) forms))

(defn parse-requires [ns-form]
  (let [reqs (some (fn [c] (when (and (seq? c) (= :require (first c))) (rest c))) ns-form)
        flat (mapcat (fn [r]
                       (cond
                         (symbol? r) [[r r]]
                         (vector? r)
                         (let [n (first r) opts (apply hash-map (rest r))]
                           (if-let [a (:as opts)] [[a n]] [[n n]]))
                         :else [])) reqs)]
    (into {} flat)))

(defn defn-body [form] (drop 2 form))

(defn collect-symbols [body]
  (let [syms (atom #{})]
    (walk/postwalk (fn [x] (when (symbol? x) (swap! syms conj x)) x) body)
    @syms))

(defn test-file? [path]
  (or (str/includes? path "/test/")
      (str/includes? path "/tests/")
      (str/ends-with? path "_test.clj")
      (str/ends-with? path "_test.cljs")
      (str/ends-with? path "_test.cljc")))

(defn collect-clj-file [path]
  (let [forms (read-forms (io/file path))
        nf (ns-form forms)
        nsym (when nf (second nf))]
    (when nsym
      (let [aliases (parse-requires nf)
            test? (test-file? path)
            vars (atom []) tests (atom [])]
        (doseq [f forms :when (and (seq? f) (symbol? (first f)))]
          (let [head (first f)]
            (cond
              (= head 'deftest)
              (when-let [tname (second f)]
                (when (symbol? tname)
                  (swap! tests conj
                         {:test/ns (str nsym) :test/name (str tname)
                          :test/qname (str nsym "/" tname)
                          :test/syms (collect-symbols (defn-body f))})))
              (def-forms head)
              (when-let [vname (second f)]
                (when (symbol? vname)
                  (let [docstr (when (string? (nth f 2 nil)) (nth f 2 nil))]
                    (swap! vars conj
                           {:var/ns (str nsym) :var/name (str vname)
                            :var/qname (str nsym "/" vname)
                            :var/kind (str head)
                            :var/has-doc (some? docstr)
                            :var/syms (collect-symbols (defn-body f))})))))))
        {:ns (str nsym) :aliases aliases
         :vars (if test? [] @vars)
         :tests (if test? @tests [])}))))

;; ---------- collect-file dispatch ----------

(defn collect-file [path]
  (let [ext (file-ext path)]
    (cond
      (src-exts ext) (collect-clj-file path)
      ((:src-exts (meta #'elisp/src-exts) elisp/src-exts) ext) (elisp/collect-file path)
      ((:src-exts (meta #'python/src-exts) python/src-exts) ext) (python/collect-file path)
      ((:src-exts (meta #'flexiarg/src-exts) flexiarg/src-exts) ext) (flexiarg/collect-file path)
      :else nil)))

(defn essay-home-path?
  [path]
  (essay/essay-home-md? path))

;; ---------- mission doc path detection + sync ----------

(defn mission-doc-path? [path]
  (boolean
   (re-find mission-doc-pattern
            (str/replace (str path) "\\" "/"))))

(defn excursion-doc-path?
  [path]
  (boolean
   (re-find excursion-doc-pattern
            (str/replace (str path) "\\" "/"))))

(defn sorry-registry-path? [path]
  (boolean
   (re-find sorry-registry-pattern
            (str/replace (str path) "\\" "/"))))

(defn current-mission-resolver
  "Public wrapper around the cached mission-doc lookup. Used by the sorry
   registry ingest path and by non-live bootstrap tooling that wants the same
   normalization convention when live mission-doc vertices already exist."
  []
  (mission-id->vertex-id-map))

(defn- normalize-mission-id
  [mission-name]
  (str/replace-first (str mission-name) #"^M-" ""))

(defn- normalize-path
  [path]
  (str/replace (str path) "\\" "/"))

(defn- split-repo-and-relative
  [rest-path]
  (when (and (string? rest-path) (not (str/blank? rest-path)))
    (let [[repo rel] (str/split rest-path #"/" 2)]
      (when (and repo rel (not (str/blank? repo)) (not (str/blank? rel)))
        [repo rel]))))

(defn normalize-file-endpoint
  "Normalize an absolute file path onto the substrate-2 file endpoint
   convention `<repo-label>/file/<relative-path>`."
  [path]
  (let [norm (normalize-path path)
        code-prefix (str home "/code/")
        home-prefix (str home "/")]
    (cond
      (str/starts-with? norm code-prefix)
      (when-let [[repo rel] (split-repo-and-relative (subs norm (count code-prefix)))]
        (str repo "-d/file/" rel))

      (str/starts-with? norm home-prefix)
      (when-let [[repo rel] (split-repo-and-relative (subs norm (count home-prefix)))]
        (str repo "-d/file/" rel))

      :else nil)))

(defn- local-name-from-registry-id
  [registry-id]
  (let [raw (cond
              (keyword? registry-id) (name registry-id)
              :else (str registry-id))]
    (if-let [i (str/index-of raw "/")]
      (subs raw (inc i))
      raw)))

(defn normalize-sorry-endpoint
  [label registry-id]
  (str label "/sorry/" (local-name-from-registry-id registry-id)))

(defn normalize-excursion-endpoint
  [label excursion-id]
  (str label "/excursion/" (local-name-from-registry-id excursion-id)))

(defn sorry-t
  [status]
  (if (= status :open) 1 0))

(defn- json-safe-value
  [v]
  (cond
    (keyword? v) (str v)
    (symbol? v) (str v)
    (map? v) (into {}
                   (map (fn [[k inner]]
                          [(if (keyword? k)
                             (if-let [ns-part (namespace k)]
                               (str ns-part "/" (name k))
                               (name k))
                             (str k))
                           (json-safe-value inner)]))
                   v)
    (vector? v) (mapv json-safe-value v)
    (seq? v) (mapv json-safe-value v)
    (set? v) (mapv json-safe-value (sort-by pr-str v))
    :else v))

(defn- sorry-source-props
  [sorry]
  (into {}
        (map (fn [[k v]]
               [(str "sorry/" (name k)) (json-safe-value v)]))
        (dissoc sorry :id)))

(defn- related-mission-edge-doc
  [path label source-endpoint registry-id mission-name target-endpoint]
  {:hx-type "code/v05/related-mission"
   :endpoints [source-endpoint target-endpoint]
   :labels ["v05" "phase-4.5" label "related-mission"]
   :props {"repo" label
           "phase" 4.5
           "source-file" path
           "sorry/source-endpoint" source-endpoint
           "sorry/source-registry-id" (str registry-id)
           "mission/target-id" (normalize-mission-id mission-name)
           "mission/target-external-id" (str mission-name)
           "relation/source-field" "related-missions"}})

(defn- file-to-mission-edge-id
  [source-endpoint target-endpoint]
  (str "hx:" file-to-mission-type ":" source-endpoint ":" target-endpoint))

(defn- file-to-mission-edge-doc
  [path mission-repo-label mission-id source-path source-endpoint target-endpoint]
  {:id (file-to-mission-edge-id source-endpoint target-endpoint)
   :hx-type file-to-mission-type
   :endpoints [source-endpoint target-endpoint]
   :labels ["v05" "phase-4.5"
            (or (some-> source-endpoint
                        (str/split #"/file/" 2)
                        first)
                mission-repo-label)
            "file→mission"
            "mission/mentions-file"
            "mentions/stated"]
   :props {"repo" (or (some-> source-endpoint
                              (str/split #"/file/" 2)
                              first)
                      mission-repo-label)
           "phase" 4.5
           "source-file" path
           "file/source-path" source-path
           "file/source-endpoint" source-endpoint
           "mission/target-id" mission-id
           "mission/target-endpoint" target-endpoint
           "relation/source-field" "mission/code-paths"
           ;; Ratified 2026-06-02: keep code/v05/file→mission as
           ;; compatibility storage while exposing the logical mission→file
           ;; mention semantics and endpoint roles.
           "relation/semantics" "mission/mentions-file"
           "relation/subtype" "mentions/stated"
           "relation/compat-alias" file-to-mission-type
           "relation/logical-direction" "mission→file"
           "relation/logical-source-endpoint" target-endpoint
           "relation/logical-source-role" "mission"
           "relation/logical-target-endpoint" source-endpoint
           "relation/logical-target-role" "file"
           "relation/storage-direction" "file→mission"
           "relation/storage-source-role" "file"
           "relation/storage-target-role" "mission"
           "relation/feeds-mu?" true
           "relation/feeds-A?" false}})

(defn build-file-to-mission-edge-docs
  "Pure projection from `:mission/code-paths` to substrate-2
   `code/v05/file→mission` edge docs."
  [{:keys [path label mission-id mission-endpoint mission-code-paths]}]
  (reduce (fn [acc source-path]
            (if-let [source-endpoint (normalize-file-endpoint source-path)]
              (update acc :edge-docs conj
                      (file-to-mission-edge-doc path
                                                label
                                                mission-id
                                                source-path
                                                source-endpoint
                                                mission-endpoint))
              (update acc :unresolved-code-paths conj
                      {"mission/target-id" mission-id
                       "mission/target-endpoint" mission-endpoint
                       "file/source-path" source-path})))
          {:edge-docs [] :unresolved-code-paths []}
          (vec (or mission-code-paths []))))

(defn- excursion-parent-edge-id
  [source-endpoint target-endpoint]
  (str "hx:" excursion-to-parent-type ":" source-endpoint ":" target-endpoint))

(defn- excursion-parent-edge-doc
  [path label source-endpoint target-endpoint]
  {:id (excursion-parent-edge-id source-endpoint target-endpoint)
   :hx-type excursion-to-parent-type
   :endpoints [source-endpoint target-endpoint]
   :labels ["v05" "phase-4.5" label "excursion→parent-mission"]
   :props {"repo" label
           "phase" 4.5
           "source-file" path
           "excursion/source-endpoint" source-endpoint
           "mission/target-endpoint" target-endpoint
           "relation/source-field" "excursion/parent-mission"}})

(defn build-excursion-docs
  "Pure projection from a parsed excursion entry to substrate-2 vertex docs
   and optional parent-mission edge docs."
  [{:keys [path label excursion]}]
  (let [excursion-id (:excursion/id excursion)
        endpoint (normalize-excursion-endpoint label excursion-id)
        parent-endpoint (:excursion/parent-mission excursion)
        vertex-doc {:hx-type "code/v05/excursion-doc"
                    :endpoints [endpoint]
                    :labels ["v05" "phase-4.5" label "excursion-doc"]
                    :props {"repo" label
                            "phase" 4.5
                            "source-file" path
                            "excursion/id" excursion-id
                            "excursion/endpoint" endpoint
                            "excursion/title" (:excursion/title excursion)
                            "excursion/status" (when-let [s (:excursion/status excursion)]
                                                 (name s))
                            "excursion/raw-status" (:excursion/raw-status excursion)
                            "excursion/date" (:excursion/date excursion)
                            "excursion/owner" (:excursion/owner excursion)
                            "excursion/parent-mission" parent-endpoint
                            "excursion/parent-mission-raw" (:excursion/parent-mission-raw excursion)
                            "excursion/summary" (:excursion/summary excursion)
                            "excursion/cross-refs" (or (:excursion/cross-refs excursion) [])
                            "excursion/code-paths" (or (:excursion/code-paths excursion) [])
                            "excursion/mtime" (:excursion/mtime excursion)}}]
    {:vertex-doc vertex-doc
     :edge-docs (cond-> []
                  parent-endpoint
                  (conj (excursion-parent-edge-doc path label endpoint parent-endpoint)))
     :skipped-parent-edge? (nil? parent-endpoint)}))

(defn build-sorry-registry-docs
  "Pure projection from the canonical sorry registry to substrate-2 hyperedge
   docs. Returns vertex docs, v0 typed related-mission edge docs, and any
   unresolved mission references that could not be normalized through the
   current mission-doc surface."
  [{:keys [path label registry mission-resolver]
    :or {mission-resolver {}}}]
  (let [sorrys (vec (or (:sorrys registry) []))]
    (reduce (fn [acc sorry]
              (let [registry-id (:id sorry)
                    endpoint (normalize-sorry-endpoint label registry-id)
                    vertex-doc {:hx-type "code/v05/sorry"
                                :endpoints [endpoint]
                                :labels ["v05" "phase-4.5" label "sorry-registry"]
                                :props (merge {"repo" label
                                               "phase" 4.5
                                               "source-file" path
                                               "sorry/endpoint" endpoint
                                               "sorry/registry-id" (str registry-id)
                                               "sorry/t" (sorry-t (:status sorry))}
                                              (sorry-source-props sorry))}
                    related (vec (or (:related-missions sorry) []))
                    {:keys [edge-docs unresolved]}
                    (reduce (fn [edge-acc mission-name]
                              (let [target-id (normalize-mission-id mission-name)
                                    target-endpoint (get mission-resolver target-id)]
                                (if target-endpoint
                                  (update edge-acc :edge-docs conj
                                          (related-mission-edge-doc path
                                                                    label
                                                                    endpoint
                                                                    registry-id
                                                                    mission-name
                                                                    target-endpoint))
                                  (update edge-acc :unresolved conj
                                          {"sorry/registry-id" (str registry-id)
                                           "sorry/endpoint" endpoint
                                           "mission/target-external-id" (str mission-name)
                                           "mission/target-id" target-id}))))
                            {:edge-docs [] :unresolved []}
                            related)]
                (-> acc
                    (update :vertex-docs conj vertex-doc)
                    (update :edge-docs into edge-docs)
                    (update :unresolved-related into unresolved))))
            {:vertex-docs [] :edge-docs [] :unresolved-related []}
            sorrys)))

(defn fixture-sorry-roundtrip
  "Non-live T-A1 fixture for the sorry registry projection. Exercises three
   cases against the pure projection layer: one :open sorry, one non-open
   sorry with closure metadata, and one :n-a-by-design sorry. Returns a report
   map suitable for dry-run validation; does not touch futon1a."
  [{:keys [path label]
    :or {label "futon2"}}]
  (let [registry (edn/read-string (slurp path))
        sorrys (vec (:sorrys registry))
        open-sorry (first (filter #(= :open (:status %)) sorrys))
        closed-sorry (first (filter #(and (not= :open (:status %))
                                          (or (:resolved-at %)
                                              (:addressed-at %)
                                              (:resolution %)
                                              (:partial-closure-notes %)))
                                    sorrys))
        na-sorry (first (filter #(= :n-a-by-design (:status %)) sorrys))
        cases (vec (remove nil? [open-sorry closed-sorry na-sorry]))
        selected-registry {:schema-version (:schema-version registry)
                           :sorrys cases}
        mission-resolver (into {}
                              (map (fn [mission-name]
                                     [(normalize-mission-id mission-name)
                                      (str "fixture/mission/"
                                           (normalize-mission-id mission-name))]))
                              (distinct (mapcat :related-missions cases)))
        plan-a (build-sorry-registry-docs {:path path
                                           :label label
                                           :registry selected-registry
                                           :mission-resolver mission-resolver})
        plan-b (build-sorry-registry-docs {:path path
                                           :label label
                                           :registry selected-registry
                                           :mission-resolver mission-resolver})
        by-endpoint (into {}
                          (map (fn [doc]
                                 [(first (:endpoints doc)) doc]))
                          (:vertex-docs plan-a))
        case-results
        (mapv (fn [sorry]
                (let [endpoint (normalize-sorry-endpoint label (:id sorry))
                      doc (get by-endpoint endpoint)
                      props (:props doc)
                      expected-t (sorry-t (:status sorry))]
                  {:registry-id (str (:id sorry))
                   :endpoint endpoint
                   :status (str (:status sorry))
                   :passed? (and (= "code/v05/sorry" (:hx-type doc))
                                 (= [endpoint] (:endpoints doc))
                                 (= endpoint (get props "sorry/endpoint"))
                                 (= (str (:id sorry)) (get props "sorry/registry-id"))
                                 (= expected-t (get props "sorry/t"))
                                 (= (str (:status sorry)) (get props "sorry/status"))
                                 (= (:title sorry) (get props "sorry/title"))
                                 (= (:raised-at sorry) (get props "sorry/raised-at")))}))
              cases)
        lossless? (= plan-a plan-b)]
    {:pass? (and (= 3 (count cases))
                 (every? :passed? case-results)
                 lossless?)
     :case-count (count cases)
     :cases case-results
     :lossless-roundtrip? lossless?
     :planned-vertices (count (:vertex-docs plan-a))
     :planned-edges (count (:edge-docs plan-a))
     :unresolved-related (count (:unresolved-related plan-a))}))

(defn sync-mission!
  "Push a single mission markdown file into futon3c's evidence layer
   via HTTP. (We're inside the futon3c JVM here, but the HTTP route
   handles the existing path; routing through it preserves the
   write-pipeline discipline rather than bypassing it.)"
  [path root]
  (let [repo-name (.getName (io/file root))
        payload {:path (str path) :repo repo-name}
        resp (try
               (http/post (str FUTON3C "/api/alpha/mc/sync-mission")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (= 200 (:status resp))
     :status (:status resp)
     :body body}))

(defn ingest-mission-doc!
  [{:keys [path label root]}]
  (let [sync (sync-mission! path root)
        mission (:mission (:body sync))
        mission-id (or (:mission/id mission) (get mission "mission/id"))
        mission-title (or (:mission/title mission) (get mission "mission/title"))
        mission-status (or (:mission/status mission) (get mission "mission/status"))
        mission-repo (or (:mission/repo mission) (get mission "mission/repo"))
        mission-date (or (:mission/date mission) (get mission "mission/date"))
        mission-owner (or (:mission/owner mission) (get mission "mission/owner"))
        ;; T-9b: content-enrichment props now sourced from the canonical
        ;; parser. Removes the need for downstream consumers (futon3a, etc.)
        ;; to re-parse the source markdown.
        mission-summary (or (:mission/summary mission) (get mission "mission/summary"))
        mission-cross-refs (or (:mission/cross-refs mission) (get mission "mission/cross-refs"))
        mission-code-paths (or (:mission/code-paths mission) (get mission "mission/code-paths"))
        mission-phase (or (:mission/phase mission) (get mission "mission/phase"))
        mission-mtime (or (:mission/mtime mission) (get mission "mission/mtime"))
        mission-psrs (or (:mission/psrs mission) (get mission "mission/psrs") [])
        mission-purs (or (:mission/purs mission) (get mission "mission/purs") [])
        vertex-id (str label "/mission/" mission-id)
        labels ["v05" "phase-4.5" label "mission-doc"]
        props {"repo" label
               "phase" 4.5
               "source-file" path
               "mission/id" mission-id
               "mission/title" mission-title
               "mission/status" (cond
                                  (keyword? mission-status) (name mission-status)
                                  :else mission-status)
               "mission/repo" mission-repo
               "mission/date" mission-date
               "mission/owner" mission-owner
               "mission/raw-status" (:mission/raw-status mission)
               "mission/blocked-by" (:mission/blocked-by mission)
               "mission/summary" mission-summary
               "mission/cross-refs" (or mission-cross-refs [])
               "mission/code-paths" (or mission-code-paths [])
               "mission/phase" (cond
                                 (keyword? mission-phase) (name mission-phase)
                                 :else mission-phase)
               "mission/mtime" mission-mtime
               "mission/psrs" mission-psrs
               "mission/purs" mission-purs
               "mission/sync-status" (:status sync)
               "mission/sync-created" (boolean (or (get-in sync [:body :created])
                                                    (get-in sync [:body "created"])))
               "mission/sync-skipped" (boolean (or (get-in sync [:body :skipped])
                                                    (get-in sync [:body "skipped"])))}
        hx-ok? (and mission-id
                    (:ok? sync)
                    (:ok? (post-hyperedge! "code/v05/mission-doc" [vertex-id] labels props)))
        ;; T-9e: also create a mission-doc entity whose :name matches the
        ;; hyperedge endpoint string, so the WebArxana Graph view can
        ;; focus on it and cross-link to the existing hyperedges via
        ;; futon1a's hyperedges-by-end (UUID → name smart-resolve in
        ;; routes.clj). See README-conventions.md §3.
        _entity-ok? (when hx-ok?
                      (:ok? (post-entity!
                             {:name vertex-id
                              :type "mission/doc"
                              :source "mission-doc-watcher"
                              :external-id (str "M-" mission-id)
                              :props {"mission/id" mission-id
                                      "mission/title" mission-title
                                      "mission/status" (cond
                                                         (keyword? mission-status) (name mission-status)
                                                         :else mission-status)
                                      "mission/repo" mission-repo
                                      "mission/phase" (cond
                                                        (keyword? mission-phase) (name mission-phase)
                                                        :else mission-phase)}})))
        {:keys [edge-docs unresolved-code-paths]}
        (build-file-to-mission-edge-docs {:path path
                                          :label label
                                          :mission-id mission-id
                                          :mission-endpoint vertex-id
                                          :mission-code-paths mission-code-paths})
        edge-stats (if (and hx-ok? (seq mission-cross-refs))
                     (emit-cross-ref-edges! vertex-id mission-id mission-cross-refs)
                     {:emitted 0 :unresolved 0 :failed 0})
        file-edge-stats (if hx-ok?
                          (reduce (fn [acc doc]
                                    (let [resp (post-hyperedge-doc! doc)]
                                      (update acc (if (:ok? resp) :emitted :failed) inc)))
                                  {:emitted 0 :failed 0}
                                  edge-docs)
                          {:emitted 0 :failed 0})]
    {:vertices (if hx-ok? 1 0)
     :edges (+ (:emitted edge-stats) (:emitted file-edge-stats))
     :unresolved-edges (+ (:unresolved edge-stats) (count unresolved-code-paths))
     :failed (+ (if hx-ok? 0 1) (:failed edge-stats) (:failed file-edge-stats))
     :sync sync}))

(defn- label->repo-keyword
  [label]
  (-> (str label)
      (str/replace #"-d$" "")
      keyword))

(defn ingest-excursion-doc!
  [{:keys [path label]}]
  (let [excursion (mcb/parse-excursion-md path (label->repo-keyword label))
        {:keys [vertex-doc edge-docs skipped-parent-edge?]}
        (build-excursion-docs {:path path
                               :label label
                               :excursion excursion})
        hx-ok? (:ok? (post-hyperedge! (:hx-type vertex-doc)
                                      (:endpoints vertex-doc)
                                      (:labels vertex-doc)
                                      (:props vertex-doc)))
        _entity-ok? (when hx-ok?
                      (:ok? (post-entity!
                             {:name (first (:endpoints vertex-doc))
                              :type "excursion/doc"
                              :source "excursion-doc-watcher"
                              :external-id (str "E-" (:excursion/id excursion))
                              :props {"excursion/id" (:excursion/id excursion)
                                      "excursion/title" (:excursion/title excursion)
                                      "excursion/status" (when-let [s (:excursion/status excursion)]
                                                           (name s))
                                      "excursion/parent-mission" (:excursion/parent-mission excursion)}})))
        edge-stats (if hx-ok?
                     (reduce (fn [acc doc]
                               (let [resp (post-hyperedge-doc! doc)]
                                 (update acc (if (:ok? resp) :emitted :failed) inc)))
                             {:emitted 0 :failed 0}
                             edge-docs)
                     {:emitted 0 :failed 0})]
    {:vertices (if hx-ok? 1 0)
     :edges (:emitted edge-stats)
     :unresolved-edges (if skipped-parent-edge? 1 0)
     :failed (+ (if hx-ok? 0 1) (:failed edge-stats))
     :excursion excursion}))

(defn ingest-sorry-registry!
  [{:keys [path label mission-resolver]}]
  (let [registry (edn/read-string (slurp path))
        {:keys [vertex-docs edge-docs unresolved-related]}
        (build-sorry-registry-docs {:path path
                                    :label label
                                    :registry registry
                                    :mission-resolver (or mission-resolver
                                                          (current-mission-resolver))})
        vertex-stats (reduce (fn [acc doc]
                               (let [resp (post-hyperedge! (:hx-type doc)
                                                           (:endpoints doc)
                                                           (:labels doc)
                                                           (:props doc))]
                                 (update acc (if (:ok? resp) :ok :failed) inc)))
                             {:ok 0 :failed 0}
                             vertex-docs)
        edge-stats (reduce (fn [acc doc]
                             (let [resp (post-hyperedge! (:hx-type doc)
                                                         (:endpoints doc)
                                                         (:labels doc)
                                                         (:props doc))]
                               (update acc (if (:ok? resp) :ok :failed) inc)))
                           {:ok 0 :failed 0}
                           edge-docs)]
    {:vertices (:ok vertex-stats)
     :edges (:ok edge-stats)
     :unresolved-edges (count unresolved-related)
     :failed (+ (:failed vertex-stats) (:failed edge-stats))}))

(defn ingest-essay!
  [{:keys [path label]}]
  (let [{:keys [essay sections annotations]} (essay/collect-file path)
        labels ["v05" "phase-4.5" label "essay-home"]
        base-props {"repo" label
                    "phase" 4.5
                    "source-file" path
                    "essay/id" (:id essay)}
        stats (atom {:vertices 0 :edges 0 :failed 0})]
    (doseq [entity (into [essay] sections)]
      (let [resp (post-entity! {:id (:id entity)
                                :name (:name entity)
                                :type (:type entity)
                                :props (:props entity)})]
        (swap! stats update (if (:ok? resp) :vertices :failed) inc)))
    (doseq [annotation annotations]
      (let [resp (post-hyperedge-doc! {:id (:id annotation)
                                       :hx-type (:hx-type annotation)
                                       :endpoints (:endpoints annotation)
                                       :labels labels
                                       :props (merge base-props (:props annotation))})]
        (swap! stats update (if (:ok? resp) :edges :failed) inc)))
    @stats))

;; ---------- repo-wide by-ns rebuild for resolution ----------

(def ^:private byns-ttl-ms (* 60 1000))

(defn- byns-cache-path [root]
  (let [h (java.security.MessageDigest/getInstance "SHA-256")
        bs (.getBytes ^String (.getCanonicalPath (java.io.File. (str root))))]
    (.update h bs)
    (str "/tmp/substrate2-byns-"
         (subs (apply str (map #(format "%02x" %) (.digest h))) 0 16)
         ".edn")))

(defn- read-fresh-byns [root]
  (let [p (byns-cache-path root)]
    (when (and (file-exists? p)
               (let [age (- (System/currentTimeMillis)
                            (.lastModified (java.io.File. ^String p)))]
                 (< age byns-ttl-ms)))
      (try (edn/read-string (slurp p))
           (catch Exception _ nil)))))

(defn- write-byns! [root data]
  (try (spit (byns-cache-path root)
             (binding [*print-length* nil
                       *print-level*  nil]
               (pr-str data)))
       (catch Exception _ nil)))

(defn- supported-ext? [ext]
  (or (src-exts ext)
      (elisp/src-exts ext)
      (python/src-exts ext)
      (flexiarg/src-exts ext)))

(defn- collect-repo* [root]
  (let [files (->> (file-seq (io/file root))
                   (filter #(.isFile ^java.io.File %))
                   (filter #(supported-ext? (file-ext (.getPath ^java.io.File %))))
                   (remove #(re-find #"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo)/|/node_modules/|/target/"
                                     (.getPath ^java.io.File %))))
        out (atom {:vars [] :tests [] :ns-set #{} :ns→aliases {}})]
    (doseq [f files]
      (when-let [{:keys [ns aliases vars tests]}
                 (try (collect-file (.getPath ^java.io.File f))
                      (catch Exception _ nil))]
        (swap! out update :ns-set conj ns)
        (swap! out update :vars into vars)
        (swap! out update :tests into tests)
        (swap! out assoc-in [:ns→aliases ns] aliases)))
    (let [{:keys [vars]} @out]
      (assoc @out :by-ns
             (reduce (fn [acc v]
                       (assoc-in acc [(:var/ns v) (:var/name v)] (:var/qname v)))
                     {} vars)))))

(defn collect-repo
  "Walk repo, return {:by-ns ..., :all-vars ..., :ns-set ..., :ns→aliases ...}.
   Cached under /tmp with a 60s TTL so back-to-back per-file dispatches
   amortise the walk cost. Cache is filesystem-derived (not substrate-2-
   derived) so it doesn't violate the 'don't cache substrate-2 state'
   discipline — it's just a CPU-saving optimization."
  [root]
  (or (when-let [cached (read-fresh-byns root)] cached)
      (let [data (collect-repo* root)]
        (write-byns! root data)
        data)))

(defn resolve-symbol [s cur-ns aliases by-ns]
  (let [nm (name s) nsp (namespace s)]
    (cond
      (and (nil? nsp) (get-in by-ns [cur-ns nm])) (get-in by-ns [cur-ns nm])
      (and nsp aliases)
      (let [a-sym (symbol nsp)
            target-ns (some-> (get aliases a-sym) str)]
        (when (and target-ns (get-in by-ns [target-ns nm]))
          (get-in by-ns [target-ns nm])))
      (and nsp (get-in by-ns [nsp nm])) (get-in by-ns [nsp nm]))))

;; ---------- per-file ingest ----------

(def ^:private flexiarg-var-prop-keys
  [:pattern/id
   :pattern/title
   :pattern/source-path
   :pattern/conclusion
   :pattern/projection-version
   :pattern/references
   :pattern/keywords
   :pattern/sigils-raw
   :pattern/sigils-canonical
   :pattern/sigil-pending
   :pattern/audience
   :pattern/tone
   :pattern/style
   :pattern/factor
   :pattern/energy
   :pattern/pattern-ref
   :pattern/slots])

(defn- prop-key
  [k]
  (if-let [ns-part (namespace k)]
    (str ns-part "/" (name k))
    (name k)))

(defn- slot->props
  [slot]
  {"slot/index" (:slot/index slot)
   "slot/name" (:slot/name slot)
   "slot/name-key" (:slot/name-key slot)
   "slot/slug" (:slot/slug slot)
   "slot/text" (:slot/text slot)})

(defn- slot-text-props
  [slots]
  (into {}
        (keep (fn [{:slot/keys [name-key text]}]
                (when (and name-key text)
                  [(str "pattern/" name-key) text])))
        slots))

(defn- flexiarg-var-props
  [v]
  (let [pattern-props
        (reduce (fn [acc k]
                  (if (contains? v k)
                    (let [value (get v k)]
                      (assoc acc
                             (prop-key k)
                             (if (= k :pattern/slots)
                               (mapv slot->props value)
                               value)))
                    acc))
                {}
                flexiarg-var-prop-keys)]
    (merge {"var/ns" (:var/ns v)
            "var/qname" (:var/qname v)
            "var/kind" (:var/kind v)
            "var/has-doc" (:var/has-doc v)}
           pattern-props
           (slot-text-props (:pattern/slots v)))))

(defn- pattern-slot-edge-id
  [pattern-qname {:slot/keys [index name-key]}]
  (str "hx:code/v05/pattern-slot:" pattern-qname ":" index ":" name-key))

(defn- pattern-slot-edge-doc
  [pf labels base-props v slot]
  {:id (pattern-slot-edge-id (:var/qname v) slot)
   :hx-type "code/v05/pattern-slot"
   :endpoints [(pf (:var/qname v)) (str "slot/" (:slot/name-key slot))]
   :labels labels
   :props (merge base-props
                 {"pattern/id" (:pattern/id v)
                  "pattern/qname" (:var/qname v)}
                 (slot->props slot))})

(defn ingest-one-file!
  "Parse `path`, POST its vertices and edges to futon1a. Returns stats.
   B-2 v0: per-repo prefix applied to per-repo qname endpoints."
  [{:keys [path label root-ctx]}]
  (let [{:keys [ns vars tests aliases]} (or (collect-file path) {})
        labels ["v05" "phase-4.5" label "per-file"]
        base-props {"repo" label "phase" 4.5 "source-file" path}
        pf (fn [q] (str label "/" q))
        post! (fn [t eps & [extra-props]]
                (post-hyperedge! t eps labels (merge base-props extra-props)))
        stats (atom {:vertices 0 :edges 0 :failed 0})]
    (when ns
      (post! "code/v05/namespace" [(pf ns)] {"namespace" ns})
      (swap! stats update :vertices inc)
      (doseq [v vars]
        (let [r (post! "code/v05/var" [(pf (:var/qname v))]
                       (flexiarg-var-props v))]
          (swap! stats update (if (:ok? r) :vertices :failed) inc)
          (doseq [slot (:pattern/slots v)]
            (let [slot-r (post-hyperedge-doc!
                          (pattern-slot-edge-doc pf labels base-props v slot))]
              (swap! stats update (if (:ok? slot-r) :edges :failed) inc)))))
      (doseq [t tests]
        (let [r (post! "code/v05/test" [(pf (:test/qname t))]
                       {"test/ns" (:test/ns t) "test/qname" (:test/qname t)})]
          (swap! stats update (if (:ok? r) :vertices :failed) inc)))
      (let [{:keys [by-ns]} root-ctx]
        (doseq [v vars
                s (:var/syms v)
                :let [qn (resolve-symbol s ns aliases by-ns)]
                :when (and qn (not= qn (:var/qname v)))]
          (let [r (post! "code/v05/calls" [(pf (:var/qname v)) (pf qn)])]
            (swap! stats update (if (:ok? r) :edges :failed) inc)))
        (doseq [t tests
                s (:test/syms t)
                :let [qn (resolve-symbol s ns aliases by-ns)]
                :when (and qn (not= qn (:test/qname t)))]
          (let [r (post! "code/v05/coverage" [(pf (:test/qname t)) (pf qn)])]
            (swap! stats update (if (:ok? r) :edges :failed) inc)))
        (doseq [v vars]
          (let [r (post! "code/v05/contains" [(pf ns) (pf (:var/qname v))])]
            (swap! stats update (if (:ok? r) :edges :failed) inc)))))
    @stats))

(defn dispatch!
  "Top-level entry point used by the watcher's per-cycle loop. Takes
   {:path :root :label} (no root-ctx — this function fetches it via
   collect-repo). Dispatches to mission-sync or per-file ingest as
   appropriate, or returns :unhandled for unsupported types."
  [{:keys [path root label]}]
  (when-not (file-exists? path)
    (throw (ex-info "file does not exist" {:path path})))
  (let [ext (file-ext path)
        mission-doc? (mission-doc-path? path)
        excursion-doc? (excursion-doc-path? path)
        sorry-registry? (sorry-registry-path? path)
        essay-home? (essay-home-path? path)
        handled? (or mission-doc? excursion-doc? sorry-registry? essay-home? (supported-ext? ext))]
    (cond
      (not handled?) {:status :unhandled :path path :ext ext}

      mission-doc?
      (let [t-start (System/currentTimeMillis)
            stats (ingest-mission-doc! {:path path :label label :root root})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :mission-doc :duration-ms dur :path path))

      excursion-doc?
      (let [t-start (System/currentTimeMillis)
            stats (ingest-excursion-doc! {:path path :label label})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :excursion-doc :duration-ms dur :path path))

      sorry-registry?
      (let [t-start (System/currentTimeMillis)
            stats (ingest-sorry-registry! {:path path :label label})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :sorry-registry :duration-ms dur :path path))

      essay-home?
      (let [t-start (System/currentTimeMillis)
            stats (ingest-essay! {:path path :label label})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :essay :duration-ms dur :path path))

      :else
      (let [t-start (System/currentTimeMillis)
            root-ctx (collect-repo root)
            stats (ingest-one-file! {:path path :label label :root-ctx root-ctx})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :ingested :duration-ms dur :path path)))))
