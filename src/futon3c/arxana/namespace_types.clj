(ns futon3c.arxana.namespace-types
  "Literal source evidence, not a call graph or proof of runtime execution.
  Coverage is recomputed from current indexed files; published edges alone are
  not a coverage certificate (removed call sites may still have old edges)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.type-registry :as registry]
            [org.httpkit.client :as http]
            [rewrite-clj.zip :as z])
  (:import [java.net URLEncoder]))

(def default-url "http://localhost:7073")
(defn- sexpr [loc] (try (z/sexpr loc) (catch Exception _ ::unreadable)))
(defn- locations [loc]
  (lazy-seq
   (when loc
     (let [form (sexpr loc)]
       (when-not (or (#{:quote :syntax-quote :uneval} (z/tag loc))
                     (and (seq? form) (#{'quote 'comment} (first form))))
         (cons loc (mapcat locations (take-while some? (iterate z/right (z/down loc))))))))))
(defn- aliases [forms]
  (into {} (mapcat
            (fn [spec]
              (let [lib (str (first spec)) opts (apply hash-map (rest spec))]
                (concat (when (:as opts) [[(str (:as opts)) lib]])
                        (for [v (:refer opts) :when (symbol? v)]
                          [(str v) (str lib "/" v)]))))
            (for [f forms :when (and (seq? f) (= 'ns (first f)))
                  clause (drop 2 f) :when (and (seq? clause) (= :require (first clause)))
                  spec (rest clause) :when (vector? spec)] spec))))
(defn- resolve-call [aliases s]
  (when (symbol? s)
    (if (namespace s)
      (str (get aliases (namespace s) (namespace s)) "/" (name s))
      (get aliases (str s) (str s)))))
(defn- literal-type? [x]
  (and (vector? x) (= 2 (count x)) (keyword? (first x))
       (integer? (second x)) (pos? (second x))))

(defn extract
  "Static literals in recognised Arxana builders/query calls only. Quoted and
  discarded forms are excluded. Computed maps/types are :dynamic. No eval,
  interprocedural inference, local binding resolution or runtime-use claim.
  :publication-sites reports write sinks separately from constructions."
  [source]
  (try
    (let [root (z/of-string source {:track-position? true})
          locs (vec (mapcat locations (take-while some? (iterate z/right root))))
          as (aliases (map sexpr locs))
          calls (for [loc locs :let [form (sexpr loc)] :when (seq? form)
                      :let [call (resolve-call as (first form))] :when call]
                  {:call call :args (rest form) :line (first (z/position loc))})
          sites (filterv #(re-find #"futon3c\.arxana\.(store-http/append-batch!|store/conditional-write!|write/execute)$" (:call %)) calls)
          uses (keep
                (fn [{:keys [call args line]}]
                  (let [write? (boolean (re-find #"^futon3c\.arxana\.records/make-" call))
                        read? (boolean (re-find #"^futon3c\.arxana\.(store/bounded-query!?|queries/(records-by-type|records-by-field|records-by-field-range|records-by-span-overlap)|dependence/reverse-records)$" call))]
                    (when (or write? read?)
                      (let [m (first (filter map? args))
                            t (if (str/ends-with? call "dependence/reverse-records") (second args)
                                  (or (:type-revision m) (:edge-type m)))
                            ;; Builders with an implicit kind have no literal type
                            ;; revision: keep that gap visible rather than guess.
                            row {:direction (if write? :writes :reads) :call call :line line}]
                        (if (literal-type? t) (assoc row :type-revision t)
                            (assoc row :status :dynamic :expression (pr-str (or t args)))))))) calls)]
      {:status :ok :uses (vec (concat uses
                                      (mapcat (fn [{:keys [args call line]}]
                                                (let [maps (filter #(and (map? %) (contains? % :type-revision))
                                                                   (mapcat #(tree-seq (fn [x] (or (vector? x) (map? x))) seq %) args))]
                                                  (if (seq maps)
                                                    (map (fn [m]
                                                           (merge {:direction :writes :call call :line line}
                                                                  (if (literal-type? (:type-revision m))
                                                                    {:type-revision (:type-revision m)}
                                                                    {:status :dynamic :expression (pr-str (:type-revision m))}))) maps)
                                                    [{:direction :writes :call call :line line :status :dynamic
                                                      :expression (pr-str args)}]))) sites))) :publication-sites (mapv #(dissoc % :args) sites)})
    (catch Exception e {:status :unknown :reason :unreadable-source :message (.getMessage e)})))

(defn- prop [r k] (or (get r (keyword "prop" (name k))) (get-in r [:hx/props k])))
(defn- canonical [p] (.getCanonicalPath (io/file p)))
(defn inspect
  "Inspect one index record. Only .clj files are in this packet's coverage."
  [r]
  (let [file (prop r :source-file)]
    (if-not (and (string? file) (str/ends-with? file ".clj"))
      {:status :unknown :reason :outside-clojure-scope :source-file file}
      (try (let [source (slurp file)]
             (assoc (extract source) :source-file file :source-sha256
                    (digest/sha256-hex (.getBytes source "UTF-8")) :namespace-id (:hx/id r) :repo (prop r :repo)))
           (catch Exception e {:status :unknown :reason :unreadable-source
                               :source-file file :message (.getMessage e)})))))

(defn edges [inspection]
  (mapv (fn [{:keys [direction type-revision line]}]
          (let [[t rev] type-revision
                target (str "arxana-type:" (subs (str t) 1) "@" rev)
                kind (if (= :writes direction) (keyword "code/v05/writes-record-type") (keyword "code/v05/reads-record-type"))
                props (select-keys inspection [:repo :source-file :source-sha256])]
            {:hx/id (str "hx:" (subs (str kind) 1) ":"
                         (digest/canonical-digest [(:namespace-id inspection) target line]))
             :hx/type kind :hx/endpoints [(:namespace-id inspection) target]
             :hx/props (assoc props :line line :extractor/version 1 :basis :literal-source)}))
        (filter :type-revision (:uses inspection))))

(defn- request! [url method path body]
  (let [r @(http/request {:url (str url path) :method method :timeout 30000 :as :text
                         :headers {"Content-Type" "application/edn" "X-Penholder" (or (System/getenv "FUTON1A_PENHOLDER") "api")}
                         :body (when body (pr-str body))})]
    (when (or (:error r) (not= 200 (:status r)))
      (throw (ex-info "namespace index transport refused" {:type :namespace-types/transport :response r})))
    (edn/read-string (:body r))))
(defn indexed-namespaces
  "Exhaust the cursor; never mistake one bounded page for complete coverage."
  ([] (indexed-namespaces default-url))
  ([url]
   (loop [cursor nil seen #{} rows []]
     (let [r (request! url :get (str "/api/alpha/hyperedges?type=code/v05/namespace&limit=1000"
                                    (when cursor (str "&after=" (URLEncoder/encode cursor "UTF-8")))) nil)
           next (:next-cursor r)]
       (when-not (vector? (:hyperedges r))
         (throw (ex-info "namespace index refused" {:type :namespace-types/index :response r})))
       (when (and next (seen next)) (throw (ex-info "repeated index cursor" {:type :namespace-types/cursor})))
       (if next (recur next (conj seen next) (into rows (:hyperedges r)))
           (into rows (:hyperedges r)))))))

(defn built-on?
  "YES means a literal construction/query site, not observed execution.
  Unknown coverage takes precedence over positive sites. Dynamic sites cannot
  justify NOT-YET. Source is rescanned so stale published edges are not facts.
  The target must match a registered revision and have an indexed literal writer.
  The 3-arity accepts an index snapshot for hermetic checks."
  ([subject-files type-prefix]
   (try (built-on? (indexed-namespaces) subject-files type-prefix)
        (catch Exception e {:answer :unknown :evidence [] :coverage []
                            :reason :index-unavailable :message (.getMessage e)})))
  ([index subject-files type-prefix]
   (let [inspect-once (memoize inspect)
         by-file (group-by #(some-> (prop % :source-file) canonical) index)
         scans (mapcat (fn [file]
                         (if-let [rs (get by-file (canonical file))]
                           (map inspect-once rs)
                           [{:status :unknown :reason :unindexed :source-file file}])) subject-files)
         unknown (filter #(not= :ok (:status %)) scans)
         dynamic (mapcat #(filter (comp #{:dynamic} :status) (:uses %)) scans)
         ;; :arxana = any type in Arxana's registry ("built on Arxana" is not
         ;; a name prefix); otherwise a name prefix such as "ma/".
         prefix (if (#{:arxana "arxana"} type-prefix) "" (str/replace (str type-prefix) #"^:" ""))
         registered (set (filter #(str/starts-with? (subs (str (first %)) 1) prefix)
                                 (keys registry/registry-v1)))
         writer (when (seq registered)
                  (some (fn [r]
                          (let [scan (inspect-once r)]
                            (when-let [use (some #(when (and (= :writes (:direction %))
                                                           (registered (:type-revision %))) %) (:uses scan))]
                              (merge (select-keys scan [:namespace-id :source-file :source-sha256]) use)))) index))
         target-reason (cond (empty? registered) :unregistered-target
                             (nil? writer) :no-indexed-target-writer)
         evidence (vec (for [scan scans use (:uses scan) :when (:type-revision use)
                            :when (registered (:type-revision use))]
                        (merge (select-keys scan [:namespace-id :source-file :source-sha256]) use)))]
     {:answer (cond (or target-reason (empty? subject-files) (seq unknown)) :unknown
                    (seq evidence) :yes (seq dynamic) :unknown :else :not-yet)
      :evidence evidence :coverage (vec scans)
      :target-coverage {:type-revisions (vec (sort-by pr-str registered))
                        :writer writer :reason target-reason}})))

(defn backfill!
  "Default dry-run. Only indexed futon3c .clj files; positive edges in batches.
  Old edges are not removed: use built-on? for current coverage, not edge absence."
  [{:keys [url write?] :or {url default-url write? false}}]
  (let [index (indexed-namespaces url)
        scans (mapv inspect (filter #(and (str/starts-with? (or (prop % :repo) "") "futon3c")
                                          (str/ends-with? (or (prop % :source-file) "") ".clj")) index))
        rows (vec (distinct (mapcat edges scans)))]
    (when write?
      (doseq [batch (partition-all 100 rows)]
        (let [r (request! url :post "/api/alpha/hyperedges/batch" {:hyperedges (vec batch)})]
          (when-not (:ok r) (throw (ex-info "namespace type write refused" {:type :namespace-types/write :response r}))))))
    {:mode (if write? :write :dry-run) :namespaces (count scans) :edges rows
     :coverage scans}))
(defn -main [& args]
  (when-not (every? #{"--dry-run" "--write"} args)
    (throw (ex-info "expected --dry-run or --write" {:args args})))
  (when (= #{"--dry-run" "--write"} (set args))
    (throw (ex-info "conflicting modes" {:args args})))
  (prn (backfill! {:write? (boolean (some #{"--write"} args))})))
