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
;; One scan reads each file once and resolves each (namespace, file) pair once.
;; Files are taken not to change while a scan runs.
(def ^:dynamic *memo* nil)
(defn memo [k f]
  (if-not *memo* (f)
    (let [hit (get @*memo* k ::none)]
      (if (not= ::none hit) hit
        (let [v (f)] (swap! *memo* assoc k v) v)))))

(defn symbol-names [form]
  (set (keep #(when (symbol? %) (symbol (name %)))
             (tree-seq #(or (coll? %) (reader-conditional? %) (seq (meta %)))
                       #(concat (when (coll? %) (seq %))
                                (when (reader-conditional? %) [(:form %)])
                                (when (meta %) [(meta %)])) form))))

(defn parse-source [text]
  ;; No evaluation, namespace loading or alias mutation; source text avoids
  ;; generated fn-symbol drift.
  (if-let [hit (get (when *parse-cache* @*parse-cache*) text)] hit
    (let [result
          (try
            ;; ::alias/key and #tag are read without resolving them: an alias
            ;; stands for itself and a tagged value keeps its tag.
            (binding [reader/*read-eval* false
                      reader/*alias-map* (fn [a] a)
                      reader/*default-data-reader-fn* tagged-literal]
              (let [r (readers/source-logging-push-back-reader text)]
                (loop [forms []]
                  (let [[f raw] (reader/read+string {:eof ::eof :read-cond :preserve} r)]
                    (if (= ::eof f)
                      {:forms forms :symbols (apply set/union #{} (map :symbols forms))}
                      (recur (conj forms {:form f :text raw :symbols (symbol-names f)})))))))
            (catch Exception _ {:reason :unreadable}))]
      (when *parse-cache* (swap! *parse-cache* assoc text result)) result)))

(defn definition-name [{:keys [form]}]
  (when (and (seq? form) (symbol? (first form))
             (str/starts-with? (name (first form)) "def")
             ;; these are used under names other than their own (methods,
             ;; ->Constructors), so they are never followed by name
             (not (#{"defmethod" "defrecord" "deftype" "defprotocol" "definterface"}
                   (name (first form))))
             (symbol? (second form)))
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
                                                :when (some #(seq (set/intersection d (:symbols %))) fs)] n))]
                        (if (= d next-d) d (recur next-d))))]
        {:changed-names (vec (sort changed)) :reachable-names (vec (sort reached)) :reason :cleared}))))

(defn ns-form [parsed]
  (first (filter #(and (seq? (:form %)) (= 'ns (first (:form %)))) (:forms parsed))))

(defn source-ns [parsed]
  (let [n (second (:form (ns-form parsed)))] (when (symbol? n) n)))

(defn libspecs
  "Every [lib & opts] in an ns form, prefix lists expanded. Read only."
  [form]
  (let [clauses (filter #(and (seq? %) (#{:require :use} (first %))) form)]
    (for [clause clauses
          spec (rest clause)
          [lib opts] (cond
                       (symbol? spec) [[spec []]]
                       (and (sequential? spec) (symbol? (first spec))
                            (some sequential? (rest spec)) (not (some keyword? (rest spec))))
                       (for [s (rest spec)]
                         (if (symbol? s) [(symbol (str (first spec) "." s)) []]
                             [(symbol (str (first spec) "." (first s))) (rest s)]))
                       (and (sequential? spec) (symbol? (first spec))) [[(first spec) (rest spec)]]
                       :else [])]
      {:lib lib :use? (= :use (first clause)) :opts (apply hash-map (if (even? (count opts)) opts []))
       :odd? (odd? (count opts))})))

(defn raw-symbols [form]
  (filter symbol?
          (tree-seq #(or (coll? %) (reader-conditional? %) (seq (meta %)))
                    #(concat (when (coll? %) (seq %))
                             (when (reader-conditional? %) [(:form %)])
                             (when (meta %) [(meta %)])) form)))

(defn without-docstring
  "A def-shaped form with its docstring removed: prose about a function is
   not a call to it. (def name \"text\") keeps its string, which is the value."
  [form]
  (if (and (seq? form) (symbol? (first form)) (str/starts-with? (name (first form)) "def")
           (symbol? (second form)) (string? (nth form 2 nil)) (> (count form) 3))
    (concat (take 2 form) (drop 3 form))
    form))

(defn code-strings [form]
  (filter string?
          (tree-seq #(or (coll? %) (reader-conditional? %))
                    #(concat (when (coll? %) (seq (without-docstring %)))
                             (when (reader-conditional? %) [(:form %)])) form)))

(defn mention-fn
  "For one consumer file, a function from one of its forms to the short names
   through which that form can refer to definitions of namespace `target`.
   A file that defines its own var of the same short name does not mention the
   target's. Anything not resolved by reading the ns form falls back to every
   short name in the form and every sought name found in its text (the
   over-matching rule). The returned function takes a form and the names sought."
  [target parsed]
  (let [nsf (ns-form parsed)
        specs (when nsf (libspecs (:form nsf)))
        body (remove #(identical? % nsf) (:forms parsed))
        tname (str target)
        alias-of #(some-> (or (get-in % [:opts :as]) (get-in % [:opts :as-alias])) str)
        mine (filter #(= target (:lib %)) specs)
        aliases (set (keep alias-of mine))
        other-aliases (set (keep alias-of (remove #(= target (:lib %)) specs)))
        refer-all? (some #(or (= :all (get-in % [:opts :refer]))
                              (and (:use? %) (not (get-in % [:opts :only])))) mine)
        referred (set (map #(symbol (name %))
                           (mapcat #(concat (let [r (get-in % [:opts :refer])] (when (sequential? r) r))
                                            (get-in % [:opts :only])) mine)))
        ;; the namespace named outside the ns form, as a bare symbol or inside a
        ;; string that is not a docstring: a require or resolve made at run time
        outside? (or (some #(and (nil? (namespace %)) (= tname (name %)))
                           (mapcat #(raw-symbols (:form %)) body))
                     (some #(str/includes? % tname) (mapcat #(code-strings (:form %)) body)))
        fallback? (or (nil? nsf) (some :odd? specs) refer-all? outside?
                      (some #(and (sequential? (get-in % [:opts :refer]))
                                  (not (every? symbol? (get-in % [:opts :refer])))) mine))]
    (if fallback?
      (fn [{:keys [text symbols]} names]
        (into (set symbols) (filter #(str/includes? text (str %)) names)))
      ;; resolved once per form; the answer does not depend on the names sought
      (let [resolved
            (into {}
                  (for [{:keys [form] :as f} body
                        :let [syms (raw-symbols form)]]
                    [(:text f)
                     (set (concat
                           (for [s syms :let [q (namespace s)]
                                 :when (and q (or (= q tname) (aliases q)
                                                  ;; an alias this ns form does not declare: undecided, so counted
                                                  (and (not (other-aliases q)) (not (str/includes? q ".")))))]
                             (symbol (name s)))
                           (for [s syms :when (and (nil? (namespace s)) (referred s))] s)))]))]
        (fn [f _names] (get resolved (:text f) #{}))))))

;; The namespace whose warrant is being read. A deftest in any other file of the
;; closure is loaded but not run by that warrant. nil: every deftest counts.
(def ^:dynamic *warranted-ns* nil)

(defn test-form? [{:keys [form]}]
  (and (seq? form) (symbol? (first form)) (= "deftest" (name (first form)))))

(defn reach
  "Follow the changed definitions through the closure, file to file, by reading.
   `start` is {namespace #{short names}}. A definition that mentions a reached
   name is itself reached. Returns the first place where a reached name arrives
   at something that runs without being called — a deftest, or a top-level form
   that is not a named definition — or nil when no such place exists."
  [files start]
  (loop [reached start]
    (let [step
          (reduce
           (fn [acc {:keys [path parsed]}]
             (if (:reason parsed)
               (reduced {:hit {:file path :names [] :reason (:reason parsed)}})
               (let [own (or (source-ns parsed) (symbol path))
                     nsf (ns-form parsed)
                     fns (into {} (for [[target names] (:reached acc) :when (and (seq names) (not= target own))]
                                    [target (memo [:mention target path] #(mention-fn target parsed))]))
                     found
                     (for [f (:forms parsed) :when (not (identical? f nsf))
                           :let [used (into (set/intersection (get-in acc [:reached own] #{}) (:symbols f))
                                            (mapcat (fn [[target m]]
                                                      (let [names (get-in acc [:reached target])]
                                                        (set/intersection names (m f names))))
                                                    fns))
                                 n (definition-name f)
                                 used (disj used n)]
                           :when (seq used)]
                       {:name n :used used
                        :test? (and (test-form? f)
                                    (or (nil? *warranted-ns*) (= (str own) (str *warranted-ns*))))})
                     hit (first (filter #(or (:test? %) (nil? (:name %))) found))]
                 (if hit
                   (reduced {:hit {:file path :names (vec (sort (:used hit)))
                                   :reason :changed-definition-reachable}})
                   (if (seq found)
                     (update-in acc [:reached own] (fnil into #{}) (map :name found))
                     acc)))))
           {:reached reached} files)]
      (cond (:hit step) (:hit step)
            (= reached (:reached step)) nil
            :else (recur (:reached step))))))

(defn check-path [root closure path]
  (let [base {:path path :changed-names [] :reachable-names []}]
    (try
      (let [file (source-file root path)
            sha (get (registry/closure-shas closure) path)]
        (if-not (re-find #"\.(clj|cljc|bb)$" path)
          (assoc base :reason :non-clojure-source)
          (if-let [old (*history-content* (str file) sha)]
            (let [new-text (slurp file)
                  delta (form-delta old new-text) result (merge base delta)]
              (if (not= :cleared (:reason delta)) result
                (let [names (set (:reachable-names delta))
                      parsed (parse-source new-text)
                      target (or (source-ns parsed) (symbol path))
                      ;; a changed or removed deftest in the changed file itself
                      own-test (seq (filter #(and (test-form? %) (names (definition-name %))) (:forms parsed)))
                      files (for [{other :path} closure
                                  :when (re-find #"\.(clj|cljc|cljs|bb)$" other)
                                  :let [other-file (source-file root other)]
                                  :when (not= file other-file)]
                              {:path other
                               :parsed (memo [:file (str other-file)] #(parse-source (slurp other-file)))})
                      hit (if own-test
                            {:file path :names (vec (sort (map definition-name own-test)))
                             :reason :changed-definition-reachable}
                            (memo [:reach path names *warranted-ns* (mapv :path files)]
                                  #(reach (vec files) {target names})))]
                  (if hit
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
    (let [tests (:test-files payload)
          closure (vals (merge (into {} (map (juxt :path identity) (:load-closure payload)))
                               (into {} (for [[path sha] tests]
                                          [path {:path path :sha256 sha :warrant-test? true}]))))
          changed (registry/closure-diff (registry/closure-shas closure)
                                         (registry/current-closure-shas root closure))]
      (if (seq changed) (merge {:changed-paths (vec (take 5 changed)) :changed-count (count changed)}
                               (form-classification root closure changed))
          {:class :current}))))

(defn assess [root namespace hit read-payload]
  (try
    (merge {:namespace namespace :entry-id (:entry-id hit)}
           (if hit
             (let [payload (read-payload (:entry-id hit))]
               (if payload (binding [*warranted-ns* namespace] (classify root payload))
                   {:class :read-failed :reason :registry-read-failed :detail :indexed-entry-missing}))
             {:class :no-warrant}))
    (catch Exception e
      {:namespace namespace :entry-id (:entry-id hit) :class :read-failed
       :reason :registry-read-failed :detail (or (ex-data e) (ex-message e))})))

(defn scan [root namespaces read-payload]
  (try
    (let [index (:namespaces (registry/namespace-ledger (str root "/data/test-registry/namespace-ledger.edn")))]
      (binding [*history-content* (memoize history-content) *parse-cache* (atom {})
                *memo* (atom {})]
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
