(ns futon3c.xiang.ma-occurrence
  "馬's per-pattern verdict, read as a chain-wide occurrence.

  For one operator turn record, each pattern in its :proposed_patterns is
  mapped part by part (IF=I1, HOWEVER=I2, BECAUSE=G, THEN=B,
  CASCADE-SPEC-v3 §2) by one bounded whistle to 馬-sonnet, in two windows:
  the proposal turn alone, and the chain of turns (this record plus up to
  n-1 earlier operator records of the session).  A map stands only when its
  quote occurs verbatim in the named record's source_text (who=operator) or
  reply_text (who=reply), the record id is inside the window (the turn
  window is the record itself), and the quote has at least 5 words.  A map
  that fails is turned into a void naming the failed check; nothing is
  dropped silently and no bad map fails the run.

  The pattern parts are read from the futon3 library the way
  scripts/occurrence_square.py reads them (read_pattern ported to Clojure,
  not shelled out: the JVM reading path cannot take a python dependency),
  and the pattern file's sha256 is kept on the verdict.

  Design copied from futon3c.xiang.ma-reading: injectable :ask, a verbatim
  quote check, and the write goes through the turn store's update-record!,
  so it cannot race the JVM's other record writers.  Registered by start!
  on :aligned (after the alignment reading) and wired live from
  futon3c.xiang.ma-reading-live."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.pattern-occurrences :as occurrences]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.turn-mention-writer :as mention-writer]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts])
  (:import [java.nio.file Files]
           [java.net SocketTimeoutException]
           [java.util.concurrent ExecutorService Executors ThreadFactory]
           [java.security MessageDigest]))

(def defaults
  "The occurrence reading's knobs, as ma-reading's. :seat names the seat to
  whistle; :ask is the bounded effect ((fn [{:keys [seat prompt timeout-ms]}]
  -> reply text or {:failure_kind ...})); nil :ask means no reader wired."
  {:seat "馬-sonnet"
   :ask nil
   :timeout-ms 120000
   :window-n 8})

(def pattern-library
  "Root of the .flexiarg pattern library (scripts/occurrence_square.py's LIB)."
  "/home/joe/code/futon3/library")

(def model-failure-kinds
  #{"model/quota" "model/timeout" "model/http" "model/parse"
    "model/error" "model/no-reader" "model/unparsed-reply"})

(def retryable-model-failure-kinds
  #{"model/quota" "model/timeout"})

(def ^:dynamic *whistle-url* "http://127.0.0.1:7070/api/alpha/whistle")
(def ^:dynamic *whistle-read-slack-ms* 15000)

(defn- model-failure
  ([kind]
   {:failure_kind (if (contains? model-failure-kinds kind) kind "model/error")})
  ([kind http-status error]
   {:failure_kind (if (contains? model-failure-kinds kind) kind "model/error")
    :http_status http-status :error error}))

(def ^:private role-names ["I1" "I2" "G" "B"])
(def ^:private min-quote-words
  "A quote of fewer words than this matches almost any turn."
  5)

;; ------------------------------------------------------- window over the store

(defn chain-window
  "The record plus up to N-1 earlier operator-origin records of the same
  session, oldest first, ending at the record: [{:id :record}].  Records
  of any other origin (session-end \"system\", bell \"agent\") are excluded,
  as are records of other sessions.  Pure over the store.  Default N is 8."
  ([store record-id] (chain-window store record-id (:window-n defaults)))
  ([store record-id n]
   (when-let [record (and (ts/record-id? record-id) (ts/read-record store record-id))]
     (let [session (:session_id record)
           at (str (:created_at record))
           all (->> (ts/list-records store :session-id session :limit 500)
                    (filterv (fn [{r :record}] (= "operator" (:origin r))))
                    (sort-by #(str (get-in % [:record :created_at])))
                    (filterv (fn [{id :id r :record}]
                               (or (= id record-id)
                                   (<= (compare (str (:created_at r)) at) 0))))
                    vec
                    ((fn [w] (if (some (fn [e] (= record-id (:id e))) w)
                               w
                               (conj w {:id record-id :record record})))))
           ;; the window is the n entries ENDING at the record: the
           ;; record plus the n-1 operator turns immediately before it.
           cut (max 0 (- (count all) (long n)))]
       (subvec all cut)))))

;; ------------------------------------------------------------- pattern parts

(defn- parse-pattern-file
  "Port of occurrence_square.py read_pattern's part splitter: a two-space
  '+ name:' line starts a top-level part; an unindented non-empty line
  ends it.  Only IF/HOWEVER/THEN/BECAUSE are kept."
  [^String text]
  (let [lines (str/split text #"\n" -1)]
    (loop [[line & rest*] lines cur nil buf [] parts {}]
      (if-not line
        (let [parts (if cur (assoc parts cur (str/trim (str/join "\n" buf))) parts)]
          {:parts (select-keys parts ["IF" "HOWEVER" "THEN" "BECAUSE"])})
        (let [m (re-matches #"  \+ ([A-Za-z-]+):(.*)" line)]
          (cond
            m (recur rest* (second m)
                     (if (str/blank? (nth m 2)) [] [(str/trim (nth m 2))])
                     (if cur (assoc parts cur (str/trim (str/join "\n" buf))) parts))
            (nil? cur) (recur rest* cur buf parts)
            ;; an unindented non-empty line leaves the part's body
            (and (seq line)
                 (not (str/starts-with? line "    "))
                 (not (str/starts-with? line "  +"))
                 (seq (str/trim line)))
            (recur rest* nil [] (assoc parts cur (str/trim (str/join "\n" buf))))
            :else (recur rest* cur (conj buf line) parts)))))))

(defn- sha256-hex ^String [^bytes data]
  (apply str (map #(format "%02x" %)
                  (.digest (doto (MessageDigest/getInstance "SHA-256")
                             (.update data))))))

(defn read-pattern
  "The pattern's four parts and the file's sha256, as
  occurrence_square.py reads them, from ROOT (default the futon3 library).
  A pattern whose file is absent reads as :missing with no parts, so its
  maps void (why pattern-file-missing) instead of failing the run."
  ([pid] (read-pattern pattern-library pid))
  ([root pid]
   (let [f (io/file (str root) (str pid ".flexiarg"))]
     (if-not (.isFile f)
       {:id pid :path nil :sha256 nil :missing true :parts {}}
       (let [data (Files/readAllBytes (.toPath f))
             {:keys [parts]} (parse-pattern-file (String. data "UTF-8"))]
         {:id pid
          :path (str "futon3/library/" pid ".flexiarg")
          :sha256 (sha256-hex data)
          :parts parts})))))

(defn- ranked-ids
  "The ranked entries' ids, ~ stripped, blanks dropped, first-seen order."
  [coll]
  (into [] (comp (keep #(some-> (:id %) str))
                 (map #(str/replace-first % #"^~" ""))
                 (remove str/blank?))
        coll))

(def per-source-cap
  "Packet 2b: each source contributes at most its top 3 ids to 馬's verdict,
  so the union across embedding / 驴/m40 / 驴 is capped at 9 patterns."
  3)

(defn patterns-of
  "The pattern ids proposed to RECORD for 馬's verdict, each as
  {:id .. :sources [..] :shown bool}: the union of the :proposed_patterns
  ids, the :embedding_ranked ids, the :m40_ranked ids and the :lu_ranked ids
  (leading ~ stripped), deduplicated in that source order, each source
  capped at its top 3 (so at most 9). :sources is the subset of
  \"embedding\", \"驴/m40\" and \"驴\" that proposed the id; :shown is true
  for the ids of a shown segment (a render's :ranked).  A render that
  carries :ranked contributes all of its ranked ids; an older render
  contributes its :patterns."
  [record]
  (let [proposed (into []
                       (comp (mapcat (fn [entry]
                                       (if-let [ranked (seq (:ranked entry))]
                                         (keep #(some-> (:id %) str) ranked)
                                         (:patterns entry))))
                             (map #(str/replace-first % #"^~" ""))
                             (remove str/blank?))
                       (:proposed_patterns record))
        shown-ids (into #{} (comp (mapcat #(ranked-ids (:ranked %)))
                                  (distinct))
                        (:proposed_patterns record))
        groups [["embedding" (distinct (concat proposed
                                               (ranked-ids (:embedding_ranked record))))]
                ["驴/m40" (ranked-ids (:ranked (:m40_ranked record)))]
                ["驴" (:lu_proposal_ids record)]]
        entries (mapcat (fn [[source ids]]
                          (for [i (take per-source-cap ids)]
                            {:id i :source source}))
                        groups)]
    (->>
      (reduce (fn [acc {:keys [id source]}]
                (if (some #(when (= (:id %) id) %) acc)
                  (mapv #(if (= (:id %) id)
                           (update % :sources (fn [s] (vec (distinct (conj (vec s) source)))))
                           %)
                        acc)
                  (conj acc {:id id :sources [source]})))
              []
              entries)
      (mapv #(if (contains? shown-ids (:id %))
               (assoc % :shown true)
               %)))))

;; ------------------------------------------------------------- the model pass

(defn- extract-json-array
  "The model's reply, cut to its outermost [ … ]; nil when none parses."
  [text]
  (try
    (let [^String s (str text)
          lo (.indexOf s "[")
          hi (.lastIndexOf s "]")]
      (when (and (not (neg? lo)) (> hi lo))
        (json/parse-string (subs s lo (inc hi)) true)))
    (catch Exception _ nil)))

(defn- window-texts
  "The verbatim-quote grounds: {record-id {:operator .. :reply ..}}."
  [window]
  (into {} (map (fn [{id :id r :record}]
                  [id {:operator (str (or (:source_text r) ""))
                       :reply (str (or (:reply_text r) ""))}]))
        window))

(defn- void-map
  "The stored form of a failed map: void, with why, and the attempt kept."
  [m why]
  {:element nil :why why :attempt m})

(defn check-map
  "One model map against the quote grounds TEXTS and the ids ALLOWED-IDS
  (the turn window allows the proposal record only).  Returns the stored
  map (element kept) or a void naming the failed check.  Pure."
  [texts allowed-ids m]
  (let [element (:element m)
        turn (str (:turn m))
        quote (str/trim (str (or (:quote m) "")))
        who (:who m)]
    (cond
      (or (nil? element) (= "null" element)) (void-map m "void")
      (not (contains? #{"operator" "reply"} who))
      (void-map m "who-not-operator-or-reply")
      (not (contains? allowed-ids turn))
      (void-map m "record-outside-window")
      (< (count (re-seq #"\S+" quote)) min-quote-words)
      (void-map m "quote-too-short")
      :else (let [text (get-in texts [turn (keyword who)])]
              (if (and (string? text) (str/includes? text quote))
                {:element element :turn turn :quote quote :who who}
                (void-map m "quote-not-found"))))))

(defn- normalize-window
  "The model's raw {role map} for one window, every map checked.  A missing
  window or map voids with why map-missing; never dropped."
  [raw texts allowed-ids]
  (into {}
        (map (fn [role]
               [role (let [m (or (get raw role) (get raw (keyword role)))]
                       (if (map? m)
                         (check-map texts allowed-ids (update m :turn str))
                         (void-map m "map-missing")))]))
        role-names))

(defn- stored-count [window-maps]
  (count (filter #(seq (str (:element %))) (vals window-maps))))

(defn- status-of [chain-mapped]
  (cond (= 4 chain-mapped) "occurrence"
        (pos? chain-mapped) "partial"
        :else "void"))

(defn- prompt-for
  "The whistle prompt: the packet wording of the 2026-10-24 experiment over
  the computed window and patterns."
  [window record-id patterns]
  (str
   "From claude-11 to 馬-sonnet. A bounded reading task. You write one JSON reply; nothing else.\n\n"
   "Rule (CASCADE-SPEC-v3 §2): an occurrence of a pattern maps each of its four parts "
   "(IF -> I1, HOWEVER -> I2, THEN -> B, BECAUSE -> G) to one situation element, meaning "
   "something the turns actually contain that the part's text names. A part with no such "
   "element is void. The test is strict: the element must be what the part describes, not "
   "merely something on the same topic.\n\n"
   "The turns below are the chain window, oldest first; the proposal turn is \""
   record-id "\".\n\n"
   "Turns:\n"
   (str/join "\n\n"
             (for [{id :id r :record} window]
               (format "--- turn %s (%s)\noperator:\n%s\n\nreply:\n%s"
                       id (str (or (:created_at r) ""))
                       (str (or (:source_text r) ""))
                       (str (or (:reply_text r) "")))))
   "\n\nProposed patterns, with each pattern's four parts:\n"
   (str/join "\n\n"
             (for [p patterns]
               (format "--- pattern %s\nIF: %s\nHOWEVER: %s\nTHEN: %s\nBECAUSE: %s"
                       (:id p)
                       (str/replace (or (get (:parts p) "IF") "MISSING") #"\s+" " ")
                       (str/replace (or (get (:parts p) "HOWEVER") "MISSING") #"\s+" " ")
                       (str/replace (or (get (:parts p) "THEN") "MISSING") #"\s+" " ")
                       (str/replace (or (get (:parts p) "BECAUSE") "MISSING") #"\s+" " "))))
   "\n\nFor every pattern above, produce two readings:\n"
   "- \"turn\": map each part using only the proposal turn (its operator text and its reply).\n"
   "- \"chain\": map each part using any turn in the window, earlier or later.\n\n"
   "For every map that is not void, give:\n"
   "- \"element\": a short name you choose for the situation element;\n"
   "- \"turn\": the turn id where it is found (the proposal turn's id for the turn window);\n"
   "- \"quote\": a verbatim excerpt from that turn's operator or reply text, at least 5 words, "
   "copied exactly (it will be checked by string match);\n"
   "- \"who\": \"operator\" or \"reply\".\n\n"
   "For a void, give \"element\": null and a one-line \"why\".\n\n"
   "Reply with strict JSON only, an array with one object per pattern:\n"
   "[{\"pattern\": \"family/name\",\n"
   "  \"turn\":  {\"I1\": {..}, \"I2\": {..}, \"G\": {..}, \"B\": {..}},\n"
   "  \"chain\": {\"I1\": {..}, \"I2\": {..}, \"G\": {..}, \"B\": {..}}}]\n"
   "Do not summarise or judge the work in the turns. Only map the parts."))

(defn- no-reader-pattern [sources shown p why]
  (let [void {:element nil :why why}]
    {:pattern (:id p) :sha256 (:sha256 p)
     :sources (vec (or (get sources (:id p)) []))
     :shown (boolean (get shown (:id p)))
     :turn {:I1 void :I2 void :G void :B void}
     :chain {:I1 void :I2 void :G void :B void}
     :turn_mapped 0 :chain_mapped 0 :status "void"}))

(defn- verdict-map
  "The version-1 :pattern_verdicts map for RECORD under OPTS.  Pure apart
  from the (bounded, injected) ask."
  [{:keys [seat ask timeout-ms library window-n] :as opts
    :or {seat (:seat defaults)
         timeout-ms (:timeout-ms defaults)
         window-n (:window-n defaults)}}
   store record-id]
  (let [record (ts/read-record store record-id)
        lu ((or (:proposal-reader opts) proposals/read-ranked)
            (or (:proposal-config opts) (mention-writer/default-config))
            (assoc record :id record-id))
        record (assoc record :lu_proposal_ids (:ids lu))
        window (chain-window store record-id window-n)
        window-ids (into #{} (map :id) window)
        texts (window-texts window)
        proposed (patterns-of record)
        sources (into {} (map (juxt :id :sources)) proposed)
        shown (into {} (map (juxt :id :shown)) proposed)
        patterns (mapv #(read-pattern (or library pattern-library) (:id %))
                       proposed)
        reply (cond
                (not ask) (model-failure "model/no-reader")
                (seq patterns)
                (try (let [result (ask {:seat seat
                                        :prompt (prompt-for window record-id patterns)
                                        :timeout-ms timeout-ms})]
                       (cond
                         (string? result) result
                         (map? result) (model-failure (:failure_kind result)
                                                      (:http_status result) (:error result))
                         :else (model-failure "model/error" nil "ask returned nil")))
                     (catch Exception e
                       (model-failure "model/error" nil (.getMessage e))))
                :else (model-failure "model/no-reader"))
        failure (when (map? reply) reply)
        rows (if failure
               ;; typed absence: no reader (not wired, off, or failed) is
               ;; recorded, not defaulted to success.
               (mapv #(no-reader-pattern sources shown %
                                         (if (= "model/no-reader" (:failure_kind failure))
                                           "no reader" (:failure_kind failure)))
                     patterns)
               (let [parsed (extract-json-array reply)
                     by-pattern (into {}
                                      (comp (filter map?)
                                            (keep (fn [row]
                                                    (when-let [p (:pattern row)]
                                                      [(str/replace-first (str p) #"^~" "")
                                                       row]))))
                                      parsed)]
                 (mapv (fn [p]
                         (let [row (get by-pattern (:id p) {})
                               turn-m (normalize-window (:turn row) texts #{record-id})
                               chain-m (normalize-window (:chain row) texts window-ids)
                               chain-n (stored-count chain-m)]
                           {:pattern (:id p) :sha256 (:sha256 p)
                            :sources (vec (or (get sources (:id p)) []))
                            :shown (boolean (get shown (:id p)))
                            :turn turn-m :chain chain-m
                            :turn_mapped (stored-count turn-m)
                            :chain_mapped chain-n
                            :status (status-of chain-n)}))
                       patterns)))
        usable-reply? (and (string? reply)
                           (seq (filter :pattern (or (extract-json-array reply) []))))
        model-outcome (cond
                        failure failure
                        usable-reply? {:failure_kind nil}
                        :else {:failure_kind "model/unparsed-reply"})]
    {:version 1
     :at (str (java.time.Instant/now))
     :window (mapv :id window)
     :reader (if (string? reply) (str (or seat "unknown")) "none")
     :model_outcome model-outcome
     :patterns (vec rows)}))

(defn- verdict-pattern-ids
  "The set of pattern ids a stored verdict covers."
  [v]
  (into #{} (map :pattern) (:patterns v)))

(defn- retryable-verdict? [verdict]
  (contains? retryable-model-failure-kinds
             (get-in verdict [:model_outcome :failure_kind])))

(declare occurrence-writer-executor)

(defn verdict!
  "Read one operator turn record's per-pattern occurrence verdict and store
  :pattern_verdicts on it (the field scripts/xiangma_port_export.py measures
  for the patternVerdict port).  Nil (nothing stored) when the record is
  absent, is not an operator turn, or proposed no patterns.  Never throws.

  Idempotent over the pattern set except for a retryable model failure. A
  later :aligned event may replace quota/timeout up to three attempts; success
  and every non-retryable result remain final. A changed pattern set is read."
  [{:keys [store notify!] :as opts} id]
  (try
    (let [record (and store (ts/read-record store id))
          lu (when (and record (= "operator" (:origin record)))
               ((or (:proposal-reader opts) proposals/read-ranked)
                (or (:proposal-config opts) (mention-writer/default-config))
                (assoc record :id id)))
          record (when record (assoc record :lu_proposal_ids (:ids lu)))
          proposed (some-> record patterns-of)]
      (if (and (= "operator" (:origin record)) (:pending lu))
        (let [pending {:version 1 :at (str (java.time.Instant/now))
                       :reader "none" :state :lu-proposals-pending
                       :why "lu proposals pending" :patterns []}]
          ;; Never clobber a real verdict (a re-fired :aligned on a turn
          ;; read before CL-3); only an absent or pending one is replaced.
          (ts/update-record! store id
                             #(let [v (:pattern_verdicts %)]
                                (if (or (nil? v) (= "lu-proposals-pending" (some-> v :state name)))
                                  (assoc % :pattern_verdicts pending)
                                  %)))
          pending)
        (when (and record
                 (= "operator" (:origin record))
                 (seq proposed))
        (let [existing (:pattern_verdicts record)]
          (if (and (map? existing)
                   (= (set (map :id proposed)) (verdict-pattern-ids existing))
                   (or (not (retryable-verdict? existing))
                       (>= (long (or (:model_attempts existing) 1)) 3)))
            existing
            (let [attempt (if (and (map? existing) (retryable-verdict? existing))
                            (inc (long (or (:model_attempts existing) 1)))
                            1)
                  read-verdict (verdict-map opts store id)
                  verdicts (cond-> (assoc read-verdict :model_attempts attempt)
                             (and (retryable-verdict? read-verdict)
                                  (>= attempt 3))
                             (assoc :model_retry_exhausted true))]
              (ts/update-record! store id #(assoc % :pattern_verdicts verdicts))
              (if-let [writer (:occurrence-writer! opts)]
                ((or (:occurrence-async! opts)
                     #(.submit occurrence-writer-executor ^Runnable %))
                 (fn []
                   (let [current (assoc (ts/read-record store id) :id id)
                         result (try
                                  (writer (or (:occurrence-config opts)
                                              (mention-writer/default-config))
                                          current verdicts)
                                  (catch Throwable e
                                    {:failed :occurrence-write-exception
                                     :detail (.getMessage e)}))]
                     (ts/update-record!
                      store id
                      #(if (:written (:pattern_occurrences %)) %
                           (assoc % :pattern_occurrences result)))
                     ;; GCC consumers see the event only after the write outcome
                     ;; is durable. lu-memory remains a field consumer.
                     (when notify! (notify! {:event :pattern-verdicts :id id})))))
                (when notify! (notify! {:event :pattern-verdicts :id id})))
              verdicts))))))
    (catch Exception _ nil)))

;; ------------------------------------------------------------ live wiring

(defonce ^:private ^ExecutorService executor
  ;; One daemon thread, as ma-reading's: the model pass can take up to
  ;; :timeout-ms, and the turn service's scheduler is shared with every
  ;; other turn listener.
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "ma-occurrence") (.setDaemon true))))))

(defonce ^:private ^ExecutorService occurrence-writer-executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "ma-occurrence-gcc") (.setDaemon true))))))

(def ^:private retry-window
  "How many of the most recent turn records an aligned event re-checks."
  20)

(defn- retry-candidates
  "Ids among the RETRY-WINDOW newest records, other than ID, whose verdict
  is a retryable model failure under the attempt cap, or pending proposals."
  [store id]
  (->> (try (ts/list-records store :limit retry-window) (catch Exception _ nil))
       (keep (fn [{rid :id record :record}]
               (let [v (:pattern_verdicts record)]
                 (when (and (not= rid id)
                            (map? v)
                            (or (= "lu-proposals-pending" (some-> v :state name))
                                (and (retryable-verdict? v)
                                     (not (:model_retry_exhausted v)))))
                   rid))))
       vec))

(defn handle-event
  "One turn-service event for the occurrence reading under OPTS: run the
  verdict only for an :aligned event with a record id (the alignment
  reading raises :aligned after storing the alignment; the occurrence
  reading needs the reply and the computed ports too, so a fair verdict
  runs after it, not on :ported)."
  [{:keys [store notify!] :as opts} {:keys [event id]}]
  (when (and (= :aligned event) id)
    (let [store (if (fn? store) (store) store)]
      (when store
        (let [opts (assoc opts :store store :notify! notify!)
              result (verdict! opts id)]
          ;; A turn raises :aligned about once, so waiting for the SAME id's
          ;; next event never retries it. Each aligned turn is the retry
          ;; trigger for the recent turns still waiting: a retryable model
          ;; failure (bounded by its own attempt cap) or pending proposals.
          (doseq [other (retry-candidates store id)]
            (verdict! opts other))
          result)))))

(defn start!
  "Register the occurrence listener on turn-service events.  OPTS as per
  defaults plus :store (turn-store, or a 0-arg fn returning it, resolved at
  event time) and :notify! (a 1-arg fn, usually the running service's
  notify).  The work runs on this namespace's own thread, so a slow whistle
  never holds the turn service's scheduler.  Re-registering replaces the
  listener."
  [opts]
  (turn-service/add-turn-listener!
   ::ma-occurrence
   (fn [event]
     (.submit executor ^Runnable #((resolve `handle-event) opts event))))
  nil)

(defn whistle-ask
  "The production :ask: a bounded whistle to SEAT through Agency's HTTP API
  (synchronous, so the request and its answer cannot cross). Returns the
  seat's text or one typed model failure map."
  [{:keys [seat prompt timeout-ms]}]
  (try
    (let [conn (doto ^java.net.HttpURLConnection
                (.openConnection (java.net.URL. *whistle-url*))
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 5000)
                 (.setReadTimeout (int (+ (or timeout-ms 60000) *whistle-read-slack-ms*)))
                 (.setRequestProperty "Content-Type" "application/json"))]
      (with-open [w (io/writer (.getOutputStream conn))]
        (.write w (json/generate-string {:agent-id seat :prompt prompt
                                         :timeout-ms (or timeout-ms 60000)
                                         :caller "ma-occurrence"})))
      (let [status (.getResponseCode conn)
            stream (if (<= 200 status 299) (.getInputStream conn) (.getErrorStream conn))
            raw (if stream (slurp stream) "")
            body (try (json/parse-string raw true) (catch Throwable _ ::invalid-json))]
        (cond
          (= status 429)
          (model-failure "model/quota" status
                         (when-not (= ::invalid-json body)
                           (some-> (or (:error body) (:message body)) str)))

          (and (= ::invalid-json body) (not (<= 200 status 299)))
          (model-failure "model/http" status "non-JSON HTTP failure")

          (= ::invalid-json body)
          (model-failure "model/parse" status "invalid JSON response")

          (and (<= 200 status 299) (:ok body) (some? (:response body)))
          (str (:response body))

          :else
          (let [typed (some-> (or (:failure-kind body) (:failure_kind body)) str
                              (str/replace-first #"^:" ""))
                error (some-> (or (:error body) (:message body)) str)
                kind (cond
                       (= status 429) "model/quota"
                       (= typed "provider/quota") "model/quota"
                       (contains? #{"timeout" "provider/timeout"} typed) "model/timeout"
                       (= "timeout" error) "model/timeout"
                       ;; The current whistle failure body exposes only :error,
                       ;; not invoke-job :failure-kind (transport/http.clj
                       ;; handle-whistle). Keep this one ingestion fallback.
                       (re-find #"(?i)quota|rate[ ._-]?limit|HTTP 429" (or error "")) "model/quota"
                       :else "model/http")]
            (model-failure kind status error)))))
    (catch SocketTimeoutException e
      (model-failure "model/timeout" nil (.getMessage e)))
    (catch Throwable e
      (model-failure "model/error" nil (.getMessage e)))))

(defn- live-service []
  (try (some-> (requiring-resolve 'futon3c.transport.http/xiang-bell-service) deref (apply []))
       (catch Throwable _ nil)))

(defn start-live!
  "Wire the listener to the running service: its store and notify are found
  at event time, so this survives reloads of either namespace."
  []
  (start! {:ask whistle-ask
           :occurrence-writer! occurrences/write!
           :store (fn [] (some-> (live-service) :config :store))
           :notify! (fn [ev] (when-let [svc (live-service)]
                               (@#'turn-service/notify-turn! svc ev)))}))
