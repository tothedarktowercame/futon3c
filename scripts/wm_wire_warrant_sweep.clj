(ns wm-wire-warrant-sweep
  "Run from futon3c root:
   clojure -Sdeps '{:aliases {:sweep {:extra-paths [\"scripts\"]}}}' -M:sweep -m wm-wire-warrant-sweep [--only NS] [--register]
   Default: read-only closure audit; never loads wire namespaces or runs tests.
   --register runs stale/nonpassing/missing namespaces serially at one HEAD."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [futon3c.evidence.http-backend :as http]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.ledger :as artifacts]))

(def classes [:current :current-by-form :stale-closure :not-passing :no-warrant :read-failed])
(defn wire-namespaces [file]
  (with-open [r (java.io.PushbackReader. (io/reader file))]
    (loop []
      (let [f (read {:eof ::eof} r)]
        (cond (= ::eof f) (throw (ex-info "wire-test-nses not found" {:file file}))
              (and (seq? f) (= 'def (first f)) (= 'wire-test-nses (second f)))
              (let [q (nth f 2)]
                (when-not (and (= 'quote (first q)) (vector? (second q)))
                  (throw (ex-info "Expected literal quoted namespace vector" {:form f})))
                (mapv str (distinct (second q))))
              :else (recur))))))

(defn source-file [root path]
  (.getCanonicalFile (if (.isAbsolute (io/file path)) (io/file path) (io/file root path))))

(defn text-sha [text]
  (format "%064x" (java.math.BigInteger. 1 (.digest (java.security.MessageDigest/getInstance "SHA-256")
                                                   (.getBytes text "UTF-8")))))

(defn history-content [file sha]
  (let [parent (str (.getParentFile (io/file file)))
        top (shell/sh "timeout" "30s" "git" "rev-parse" "--show-toplevel" :dir parent)]
    (when (zero? (:exit top))
      (let [root (str/trim (:out top))
            path (str (.relativize (.toPath (io/file root)) (.toPath (io/file file))))
            log (shell/sh "timeout" "30s" "git" "log" "--format=%H" "--" path :dir root)]
        (when (zero? (:exit log))
          (some (fn [rev]
                  (let [r (shell/sh "timeout" "30s" "git" "show" (str rev ":" path) :dir root)]
                    (when (and (zero? (:exit r)) (= sha (text-sha (:out r)))) (:out r))))
                (str/split-lines (:out log))))))))

(def ^:dynamic *history-content* history-content)
(def ^:dynamic *parse-cache* nil)

(defn symbol-names [form]
  (set (keep #(when (symbol? %) (symbol (name %)))
             (tree-seq #(or (coll? %) (reader-conditional? %) (seq (meta %)))
                       #(concat (when (coll? %) (seq %))
                                (when (reader-conditional? %) [(:form %)])
                                (when (meta %) [(meta %)])) form))))

(defn parse-source [text]
  ;; No evaluation, namespace loading or alias mutation. Unknown reader aliases
  ;; and tags refuse the audit; source text avoids generated fn-symbol drift.
  (if-let [hit (get (when *parse-cache* @*parse-cache*) text)] hit
    (let [result
          (try
            (binding [reader/*read-eval* false]
              (let [r (readers/source-logging-push-back-reader text)]
                (loop [forms []]
                  (let [[f raw] (reader/read+string {:eof ::eof :read-cond :preserve} r)]
                    (if (= ::eof f)
                      {:forms forms :symbols (apply set/union #{} (map (comp symbol-names :form) forms))}
                      (recur (conj forms {:form f :text raw})))))))
            (catch Exception _ {:reason :unreadable}))]
      (when *parse-cache* (swap! *parse-cache* assoc text result)) result)))

(defn definition-name [{:keys [form]}]
  (when (and (seq? form) (symbol? (first form))
             (str/starts-with? (name (first form)) "def")
             (not= "defmethod" (name (first form))) (symbol? (second form)))
    (symbol (name (second form)))))

(defn form-delta [old new]
  (let [a (parse-source old) b (parse-source new)
        ns? #(and (seq? (:form %)) (= 'ns (first (:form %))))
        unnamed #(remove (fn [f] (or (ns? f) (definition-name f))) (:forms %))
        defs #(group-by definition-name (filter definition-name (:forms %)))]
    (cond
      (or (:reason a) (:reason b)) {:reason :unreadable}
      (not= (map :text (filter ns? (:forms a))) (map :text (filter ns? (:forms b))))
      {:reason :ns-form-changed}
      (not= (map :text (unnamed a)) (map :text (unnamed b))) {:reason :unnamed-form-changed}
      :else
      (let [ad (defs a) bd (defs b)
            changed (set (filter #(not= (map :text (get ad %)) (map :text (get bd %)))
                                 (set/union (set (keys ad)) (set (keys bd)))))
            reached (loop [d changed]
                      (let [next-d (into d (for [[n fs] bd
                                                :when (some #(seq (set/intersection d (symbol-names (:form %)))) fs)] n))]
                        (if (= d next-d) d (recur next-d))))]
        {:changed-names (vec (sort changed)) :reachable-names (vec (sort reached)) :reason :cleared}))))

(defn check-path [root closure path]
  (let [base {:path path :changed-names [] :reachable-names []}]
    (try
      (let [file (source-file root path)
            sha (get (registry/closure-shas closure) path)]
        (if-not (re-find #"\.(clj|cljc|bb)$" path)
          (assoc base :reason :non-clojure-source)
          (if-let [old (*history-content* (str file) sha)]
            (let [delta (form-delta old (slurp file)) result (merge base delta)]
              (if (not= :cleared (:reason delta)) result
                (let [names (set (:reachable-names delta))
                      hits (for [{other :path} closure
                                 :let [other-file (source-file root other)]
                                 ;; A changed test must itself count as a consumer.
                                 :when (or (not= file other-file) (str/includes? (str file) "/test/"))
                                 :let [parsed (parse-source (slurp other-file))
                                       used (set/intersection names (:symbols parsed))]
                                 :when (or (:reason parsed) (seq used))]
                             {:file other :names (vec (sort used)) :reason (or (:reason parsed) :changed-definition-reachable)})]
                  (if-let [hit (first hits)]
                    (assoc result :reason (:reason hit) :consumer hit)
                    result))))
            (assoc base :reason :old-content-unavailable))))
      (catch Exception _ (assoc base :reason :unreadable)))))

(defn form-classification [root closure changed]
  (let [checks (mapv #(check-path root closure %) changed)
        failure (first (remove #(= :cleared (:reason %)) checks))]
    {:class (if failure :stale-closure :current-by-form)
     :reason (if failure (:reason failure) :changed-definitions-unreachable)
     :form-check checks}))

(defn classify [root payload]
  (cond
    (nil? payload) {:class :no-warrant}
    (or (= :registry-read-failed (:reason payload)) (= :read-failed (:error/code payload)))
    {:class :read-failed :reason :registry-read-failed :detail payload}
    (not (and (= 0 (get-in payload [:results :failures]))
              (= 0 (get-in payload [:results :errors])))) {:class :not-passing}
    (not (true? (:warrant? payload))) {:class :no-warrant}
    (not (seq (:load-closure payload))) {:class :no-warrant :reason :missing-load-closure}
    :else
    (let [changed (registry/closure-diff
                   (registry/closure-shas (:load-closure payload))
                   (registry/current-closure-shas root (:load-closure payload)))]
      (if (seq changed) (merge {:changed-paths (vec (take 5 changed)) :changed-count (count changed)}
                               (form-classification root (:load-closure payload) changed))
          {:class :current}))))

(defn assess [root namespace hit read-payload]
  (try
    (merge {:namespace namespace :entry-id (:entry-id hit)}
           (if hit
             (let [payload (read-payload (:entry-id hit))]
               (if payload (classify root payload)
                   {:class :read-failed :reason :registry-read-failed :detail :indexed-entry-missing}))
             {:class :no-warrant}))
    (catch Exception e
      {:namespace namespace :entry-id (:entry-id hit) :class :read-failed
       :reason :registry-read-failed :detail (or (ex-data e) (ex-message e))})))

(defn scan [root namespaces read-payload]
  (try
    (let [index (:namespaces (registry/namespace-ledger (str root "/data/test-registry/namespace-ledger.edn")))]
      (binding [*history-content* (memoize history-content) *parse-cache* (atom {})]
        (mapv #(assess root % (get index %) read-payload) namespaces)))
    (catch Exception e
      (mapv #(hash-map :namespace % :class :read-failed :reason :registry-read-failed
                      :detail (or (ex-data e) (ex-message e))) namespaces))))

(defn summary [rows]
  {:counts (merge (zipmap classes (repeat 0)) (frequencies (map :class rows)))
   :non-current (filterv #(not= :current (:class %)) rows)})

(defn failure-snippet [result]
  (let [text (str (:out result) "\n" (:err result))
        row (try (edn/read-string (last (str/split-lines (:out result)))) (catch Exception _ nil))
        artifact (get-in row [:payload :log-artifact])
        file (when (:sha256 artifact) (artifacts/resolve-file (:sha256 artifact)))
        log (if file (slurp file) text)
        at (re-find #"(?m)^(?:FAIL|ERROR) in .*" log)
        excerpt (if at (subs log (str/index-of log at)) text)]
    (subs excerpt 0 (min 300 (count excerpt)))))

(defn register-one! [root revision namespace]
  (let [script (.getCanonicalPath (io/file root "../futon2/scripts/wm/register-warrant.sh"))
        result (shell/sh "timeout" "600s" script "--pinned" revision namespace
                         :dir root :env (assoc (into {} (System/getenv))
                                              "AUTHOR" (or (System/getenv "AUTHOR") "wire-warrant-sweep")
                                              "CODE_PATHS" "test/futon3c/diagramprover/wm_wire.clj"))]
    (cond-> {:namespace namespace :registration-exit (:exit result)}
      (not= 0 (:exit result)) (assoc :failure (failure-snippet result)))))

(defn -main [& args]
  (let [register? (boolean (some #{"--register"} args))
        only (second (drop-while #(not= "--only" %) args))
        _ (when (or (and (some #{"--only"} args) (nil? only))
                    (seq (remove #{"--register" "--only" only} args)))
            (throw (ex-info "Usage: [--register] [--only NS]" {:args args})))
        root (.getCanonicalPath (io/file "."))
        all (wire-namespaces "test/futon3c/diagramprover/wm_wire_ledger_test.clj")
        _ (when (and only (not (some #{only} all))) (throw (ex-info "Unknown wire namespace" {:namespace only})))
        namespaces (if only [only] all)
        backend (http/make-http-backend (or (System/getenv "AGENCY_URL") "http://localhost:7070"))
        read-payload #(-> (registry/read-chain! backend %) last :payload)
        rows (scan root namespaces read-payload)]
    (prn (assoc (summary rows) :mode (if register? :before-register :dry-run)))
    (when register?
      (let [revision (shell/sh "git" "rev-parse" "HEAD" :dir root)]
        (when-not (zero? (:exit revision)) (throw (ex-info "Cannot pin HEAD" revision)))
        (doseq [row rows :when (not (#{:current :current-by-form :read-failed} (:class row)))]
          (prn {:namespace (:namespace row) :action :register :previous-class (:class row)}) (flush)
          (prn (register-one! root (str/trim (:out revision)) (:namespace row))) (flush))
      (prn (assoc (summary (scan root namespaces read-payload)) :mode :after-register))))
    (shutdown-agents)))
