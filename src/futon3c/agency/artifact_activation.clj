(ns futon3c.agency.artifact-activation
  "Weak semantic activations recorded beside durable work artifacts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.pattern-search :as pattern-search]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as evidence-store])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util UUID]
           [java.util.concurrent Executors ThreadFactory]))

(def ^:private max-text-bytes (* 64 1024))
(def ^:private top-k 8)
(def ^:private model-name "sentence-transformers/all-MiniLM-L6-v2")

(defn- futon3a-root []
  (or (System/getenv "FUTON3A_ROOT")
      (str (System/getProperty "user.home") "/code/futon3a")))

(defn- script-file [] (io/file (futon3a-root) "scripts/notions_search.py"))
(defn- index-file []
  (io/file (futon3a-root) "resources/notions/minilm_pattern_embeddings.json"))

(defonce ^:private !digest-cache (atom {}))

(defn- hex [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bytes)))

(defn- sha256-bytes [^bytes bytes]
  (hex (.digest (MessageDigest/getInstance "SHA-256") bytes)))

(defn- sha256-text [text]
  (sha256-bytes (.getBytes (str text) StandardCharsets/UTF_8)))

(defn markdown-sections
  "Split Markdown into headed sections. Each section owns its heading and the
   following lines up to the next heading of any level, so body text belongs
   to exactly one section. ATX headings of levels 1--4 are recognised;
   heading-like lines inside fenced code blocks are ordinary body text."
  [text]
  (let [lines (vec (str/split-lines (str text)))
        fence-re #"^\s*(```+|~~~+)"
        heading-re #"^\s*(#{1,4})\s+(.+?)\s*#*\s*$"
        headings (loop [i 0 fence nil found []]
                   (if (= i (count lines))
                     found
                     (let [line (nth lines i)
                           fence-mark (some-> (re-find fence-re line) second)
                           closes? (and fence fence-mark
                                        (= (first fence) (first fence-mark)))
                           opens? (and (nil? fence) fence-mark)
                           match (when-not fence (re-matches heading-re line))]
                       (recur (inc i)
                              (cond closes? nil opens? fence-mark :else fence)
                              (cond-> found match
                                (conj {:index i :heading (str/trim (nth match 2))}))))))]
    (mapv (fn [n {:keys [index heading]}]
            (let [end-index (dec (or (:index (nth headings (inc n) nil))
                                     (count lines)))]
              {:heading heading
               :start-line (inc index)
               :end-line (inc end-index)
               :text (str/join "\n" (subvec lines index (inc end-index)))}))
          (range (count headings)) headings)))

(defn changed-sections
  "Return NEW-TEXT sections that are new or changed. Identity for comparison
   is heading plus text digest, deliberately excluding line numbers so a pure
   line shift does not create an activation."
  [old-text new-text]
  (let [old-signatures (->> (markdown-sections old-text)
                            (map (juxt :heading (comp sha256-text :text)))
                            set)]
    (->> (markdown-sections new-text)
         (remove #(contains? old-signatures
                             [(:heading %) (sha256-text (:text %))]))
         vec)))

(defn- file-sha256 [file]
  (let [f (io/file file)
        key [(.getCanonicalPath f) (.lastModified f) (.length f)]]
    (or (get @!digest-cache key)
        (let [digest (with-open [in (io/input-stream f)]
                       (let [md (MessageDigest/getInstance "SHA-256")
                             buffer (byte-array 65536)]
                         (loop []
                           (let [n (.read in buffer)]
                             (when (pos? n)
                               (.update md buffer 0 n)
                               (recur))))
                         (hex (.digest md))))]
          (swap! !digest-cache
                 (fn [cache]
                   (->> cache
                        (remove (fn [[[path]]]
                                  (= path (.getCanonicalPath f))))
                        (into {})
                        (#(assoc % key digest)))))
          digest))))

(defn retrieval-descriptor []
  {:name :futon3a/notions-search
   :model model-name
   :script-sha256 (file-sha256 (script-file))
   :index-sha256 (file-sha256 (index-file))})

(defn cited-ids
  "Return canonical KNOWN-IDS cited exactly in TEXT. A leading `~`, backticks,
   and Markdown link syntax are presentation only; bare leaf names do not match."
  [text known-ids]
  (let [text (str text)]
    (->> known-ids
         (filter string?)
         (filter (fn [id]
                   (let [quoted (java.util.regex.Pattern/quote id)
                         pattern (re-pattern
                                  (str "(?:^|[^\\p{L}\\p{N}_/\\-])~?`?" quoted
                                       "`?(?![\\p{L}\\p{N}_/\\-])"))]
                     (boolean (re-find pattern text)))))
         sort
         vec)))

(defn- truncate-utf8 [text]
  (let [bytes (.getBytes (str text) StandardCharsets/UTF_8)]
    (if (<= (alength bytes) max-text-bytes)
      [(str text) false]
      (let [prefix (String. bytes 0 max-text-bytes StandardCharsets/UTF_8)]
        ;; A cut inside a multibyte sequence becomes U+FFFD. Drop it so the
        ;; stored text is valid and remains within the byte bound.
        [(str/replace prefix #"\uFFFD$" "") true]))))

(defn- record-id [artifact-id text-sha retrieval]
  (str "artifact-activation:"
       (UUID/nameUUIDFromBytes
        (.getBytes (pr-str [artifact-id text-sha
                            (select-keys retrieval
                                         [:name :model :script-sha256 :index-sha256])])
                   StandardCharsets/UTF_8))))

(defn activation-record
  "Build the closed weak-activation evidence entry. ARTIFACT requires :kind,
   :id and :observed-at. RETRIEVAL is the pinned descriptor; HITS is a ranked
   result sequence or {:error {:reason keyword :message string}}."
  [artifact text retrieval hits]
  (let [text (str text)
        text-sha (sha256-text text)
        [stored-text truncated?] (truncate-utf8 text)
        error (:error hits)
        ranked (if error []
                   (->> hits
                        (take top-k)
                        (map-indexed
                         (fn [i hit]
                           {:rank (or (:rank hit) (inc i))
                            :id (:id hit)
                            :title (:title hit)
                            :score (:score hit)}))
                        vec))
        cited (cited-ids text (map :id ranked))
        cited-set (set cited)
        weak (->> ranked (map :id) (remove cited-set) vec)
        body (cond-> {:artifact {:kind (:kind artifact) :id (:id artifact)}
                      :text stored-text
                      :text-truncated? truncated?
                      :text-sha256 text-sha
                      :retrieval retrieval
                      :library-commit nil
                      :library-commit-basis :unattested
                      :hits ranked
                      :cited cited
                      :weak weak
                      :observed-at (:observed-at artifact)}
               error (assoc :error (cond-> {:reason (:reason error)
                                            :message (str (:message error))}
                                     (contains? error :count)
                                     (assoc :count (:count error)))))]
    {:evidence/id (record-id (:id artifact) text-sha retrieval)
     :evidence/subject {:ref/type (:kind artifact) :ref/id (:id artifact)}
     :evidence/type :artifact/weak-activation
     :evidence/claim-type :observation
     :evidence/author "futon3c/artifact-activation"
     :evidence/at (:observed-at artifact)
     :evidence/body body
     :evidence/tags [:artifact-retrieval :weak-activation]}))

(def ^:dynamic *search-fn* pattern-search/search)
(def ^:dynamic *append-fn* boundary/append!)
(def ^:dynamic *get-fn* evidence-store/get-entry*)
(def ^:dynamic *descriptor-fn* retrieval-descriptor)

(defn- comparable-entry [entry]
  (select-keys entry [:evidence/id :evidence/subject :evidence/type
                      :evidence/claim-type :evidence/author :evidence/at
                      :evidence/body :evidence/tags]))

(defn record!
  "Retrieve and durably append one activation record. Exact replay is
   :existing; an existing deterministic id with different content conflicts."
  ([artifact text] (record! nil artifact text))
  ([store artifact text]
   (let [descriptor (*descriptor-fn*)
         hits (try
                (let [r (*search-fn* text top-k)]
                  ;; The one-shot fallback returns nil on timeout; an index of
                  ;; ~1,600 patterns never yields zero hits, so empty is failure.
                  (if (seq r) r
                      {:error {:reason :search-returned-nothing
                               :message "search returned no hits"}}))
                (catch Throwable t
                  {:error {:reason (or (:error/code (ex-data t)) :search-failed)
                           :message (.getMessage t)}}))
         entry (activation-record artifact text descriptor hits)
         existing (*get-fn* store (:evidence/id entry))]
     (cond
       (= (comparable-entry existing) (comparable-entry entry))
       {:status :existing :entry existing :evidence/id (:evidence/id entry)}

       existing
       (throw (ex-info "Weak-activation deterministic id conflict"
                       {:reason :activation-conflict
                        :evidence/id (:evidence/id entry)}))

       :else
       (let [result (*append-fn* store entry)]
         (if (:ok result)
           {:status :recorded :entry (:entry result)
            :evidence/id (:evidence/id entry)}
           (throw (ex-info "Weak-activation append failed"
                           {:reason :activation-append-failed :result result}))))))))

(defn record-error!
  "Append a typed activation failure without running retrieval. Used when the
   artifact producer itself cannot submit the full retrieval population."
  ([artifact text error] (record-error! nil artifact text error))
  ([store artifact text error]
   (let [entry (activation-record artifact text (*descriptor-fn*) {:error error})
         existing (*get-fn* store (:evidence/id entry))]
     (cond
       (= (comparable-entry existing) (comparable-entry entry))
       {:status :existing :entry existing :evidence/id (:evidence/id entry)}

       existing
       (throw (ex-info "Weak-activation deterministic id conflict"
                       {:reason :activation-conflict
                        :evidence/id (:evidence/id entry)}))

       :else
       (let [result (*append-fn* store entry)]
         (if (:ok result)
           {:status :recorded :entry (:entry result)
            :evidence/id (:evidence/id entry)}
           (throw (ex-info "Weak-activation append failed"
                           {:reason :activation-append-failed :result result}))))))))

(defonce ^:private executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. runnable "artifact-activation")
         (.setDaemon true))))))

(def ^:dynamic *submit-fn*
  (fn [task] (.submit executor ^Runnable task)))

(defn submit-task!
  "Run TASK on the activation executor and return immediately."
  [task]
  (*submit-fn* (bound-fn [] (task)))
  :submitted)

(defn submit-work!
  "Queue a work packet after its invoke job is durable. Returns immediately."
  [store artifact text]
  (submit-task!
   (fn []
     (try (record! store artifact text)
          (catch Throwable t
            (binding [*out* *err*]
              (println "[artifact-activation] record failed"
                       (pr-str {:artifact (:id artifact)
                                :message (.getMessage t)}))))))))

(defn submit-error!
  "Queue a typed producer-side failure record and return immediately."
  [store artifact text error]
  (submit-task!
   #(try (record-error! store artifact text error)
         (catch Throwable t
           (binding [*out* *err*]
             (println "[artifact-activation] error record failed"
                      (pr-str {:artifact (:id artifact)
                               :message (.getMessage t)})))))))

(defn missing-activations
  "Accepted work JOB-IDS having neither a success nor typed failure record."
  [job-ids records]
  (let [covered (->> records
                     (filter #(= :artifact/weak-activation (:evidence/type %)))
                     (map #(get-in % [:evidence/subject :ref/id]))
                     set)]
    (->> job-ids
         (remove covered)
         (mapv (fn [job-id]
                 {:job-id job-id :reason :activation-missing})))))
