(ns futon3c.watcher.file-ingest
  "Substrate-2 phase-4.5 per-file ingest. Called by the in-JVM
   watcher (`futon3c.watcher.multi`) on each fs event with the
   file path + repo root + label, plus a cached `root-ctx` (by-ns
   resolution map for the repo).

   Dispatch by file extension:
     .clj/.cljs/.cljc → vendored Clojure projector (this file)
     .el              → futon3c.watcher.projections.elisp/collect-file
     .py              → futon3c.watcher.projections.python/collect-file
     .flexiarg        → futon3c.watcher.projections.flexiarg/collect-file
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
            [futon3c.watcher.projections.elisp :as elisp]
            [futon3c.watcher.projections.python :as python]
            [futon3c.watcher.projections.flexiarg :as flexiarg]))

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def FUTON3C   (or (System/getenv "FUTON3C_URL") "http://localhost:7070"))

(def directed-types
  #{"code/v05/calls" "code/v05/coverage" "code/v05/vocabulary-use"
    "code/v05/term-defines" "code/v05/contains"})

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

;; ---------- mission doc path detection + sync ----------

(defn mission-doc-path? [path]
  (boolean
   (re-find #"/holes/missions/M-[^/]+\.md$"
            (str/replace (str path) "\\" "/"))))

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
               "mission/sync-status" (:status sync)
               "mission/sync-created" (boolean (or (get-in sync [:body :created])
                                                    (get-in sync [:body "created"])))
               "mission/sync-skipped" (boolean (or (get-in sync [:body :skipped])
                                                    (get-in sync [:body "skipped"])))}
        hx-ok? (and mission-id
                    (:ok? sync)
                    (:ok? (post-hyperedge! "code/v05/mission-doc" [vertex-id] labels props)))]
    {:vertices (if hx-ok? 1 0)
     :edges 0
     :failed (if hx-ok? 0 1)
     :sync sync}))

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
                       {"var/ns" (:var/ns v) "var/qname" (:var/qname v)
                        "var/kind" (:var/kind v)
                        "var/has-doc" (:var/has-doc v)})]
          (swap! stats update (if (:ok? r) :vertices :failed) inc)))
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
        handled? (or mission-doc? (supported-ext? ext))]
    (cond
      (not handled?) {:status :unhandled :path path :ext ext}

      mission-doc?
      (let [t-start (System/currentTimeMillis)
            stats (ingest-mission-doc! {:path path :label label :root root})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :mission-doc :duration-ms dur :path path))

      :else
      (let [t-start (System/currentTimeMillis)
            root-ctx (collect-repo root)
            stats (ingest-one-file! {:path path :label label :root-ctx root-ctx})
            dur (- (System/currentTimeMillis) t-start)]
        (assoc stats :status :ingested :duration-ms dur :path path)))))
