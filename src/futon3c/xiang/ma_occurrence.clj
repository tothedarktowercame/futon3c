(ns futon3c.xiang.ma-occurrence
  "馬's per-pattern verdict, read as a chain-wide occurrence.

  For one operator turn record, each pattern in its :proposed_patterns is
  mapped part by part (IF=I1, HOWEVER=I2, BECAUSE=G, THEN=B,
  CASCADE-SPEC-v3 §2) by bounded batches of at most three patterns to
  馬-sonnet, in two windows:
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
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [futon3c.arxana.ma-strand :as ma-strand]
            [futon3c.arxana.pattern-occurrences :as occurrences]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.turn-mention-writer :as mention-writer]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts]
            [futon3c.xiang.whistle-ask :as wa])
  (:import [java.nio.file Files]
           [java.util.concurrent ExecutorService Executors ThreadFactory]
           [java.security MessageDigest]))

(def defaults
  "The occurrence reading's knobs, as ma-reading's. :seat names the seat to
  whistle; :ask is the bounded effect ((fn [{:keys [seat prompt timeout-ms]}]
  -> reply text or {:failure_kind ...})); nil :ask means no reader wired."
  {:seat "馬-sonnet"
   :ask nil
   :timeout-ms 120000
   :window-n 4})

(def batch-size
  "Maximum patterns in one occurrence-reader call."
  3)

(def pattern-library
  "Root of the .flexiarg pattern library (scripts/occurrence_square.py's LIB)."
  "/home/joe/code/futon3/library")

(def model-failure-kinds wa/failure-kinds)

(declare sha256-hex retryable-verdict?)

(def retryable-model-failure-kinds
  #{"model/quota" "model/timeout"})

(defn- model-failure
  ([kind]
   (wa/model-failure kind))
  ([kind http-status error]
   (wa/model-failure kind http-status error)))

(def ^:private role-names ["I1" "I2" "G" "B"])
(def ^:private min-quote-words
  "A quote of fewer words than this matches almost any turn."
  5)

;; ------------------------------------------------------- window over the store

(defn chain-window
  "The record plus up to N-1 earlier operator-origin records of the same
  session, oldest first, ending at the record: [{:id :record}].  Records
  of any other origin (session-end \"system\", bell \"agent\") are excluded,
  as are records of other sessions.  Pure over the store.  Default N is 4."
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

;; ------------------------------------------------------- turn strand receipt

(defn- sorted-value [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [k (sorted-value v)])) x)
    (set? x) (into (sorted-set) (map sorted-value) x)
    (sequential? x) (mapv sorted-value x)
    :else x))

(defn verdict-digest
  "Canonical SHA-256 of the durable 馬 payload. The predecessor connection and
  write timestamp are excluded: they are receipt metadata, not the occurrence
  reading, and excluding the connection keeps each strand edge small."
  [verdict]
  (sha256-hex (.getBytes (with-out-str
                           (pp/pprint (sorted-value
                                       (dissoc verdict :connection :at))))
                         "UTF-8")))

(defn- operator-turns
  "All durable operator turns in RECORD's session, oldest first. Equal
  timestamps are ordered by record id, so predecessor selection survives a
  restart and does not depend on filesystem enumeration order."
  [store record]
  (let [session (:session_id record)]
    (if (nil? session)
      []
      (->> (ts/list-records store :session-id session :limit Long/MAX_VALUE)
           (filter #(= "operator" (get-in % [:record :origin])))
           (sort-by (juxt #(str (get-in % [:record :created_at])) :id))
           vec))))

(defn- adjacent-turn-by-scan
  "Legacy path for records written before the turn store kept strand links."
  [store id record direction]
  (let [turns (operator-turns store record)
        index (first (keep-indexed #(when (= id (:id %2)) %1) turns))
        target (when (some? index) (+ index direction))]
    ;; Re-read the neighbour: list-records serves a cached copy keyed on
    ;; mtime+length, which misses a same-length rewrite in the same tick.
    (when (and target (<= 0 target) (< target (count turns)))
      (let [tid (:id (nth turns target))]
        (when-let [r (ts/read-record store tid)] {:id tid :record r})))))

(defn- adjacent-turn
  "ID's operator-turn neighbour in its session (DIRECTION -1 or +1), from the
  turn store's strand links (turn_store write-record!). Records written
  before the links existed fall back to a session scan."
  [store id direction]
  (when-let [record (ts/read-record store id)]
    (let [linked? (contains? record :prev_operator_turn_id)
          k (if (neg? direction) :prev_operator_turn_id :next_operator_turn_id)
          target (get record k)]
      (cond
        target (when-let [r (ts/read-record store target)] {:id target :record r})
        (or linked? (and (pos? direction) (contains? record k))) nil
        :else (adjacent-turn-by-scan store id record direction)))))

(defn- absent-reason [record]
  (cond
    (or (:reply_absence record) (not (string? (:reply_text record)))) :no-reply
    (contains? #{"interrupted" "cancelled" "aborted"}
               (some-> (:analysis_status record) str/lower-case)) :interrupted
    :else nil))

(defonce ^:private ^java.util.concurrent.ConcurrentHashMap session-locks
  (java.util.concurrent.ConcurrentHashMap.))

(defn- session-lock [store id]
  (let [record (ts/read-record store id)
        key (str (:dir store) "\u0000" (or (:session_id record) id))]
    (.computeIfAbsent session-locks key
                      (reify java.util.function.Function
                        (apply [_ _] (Object.))))))

(defn- with-session-lock [store id f]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (session-lock store id) (f)))

(defn- final-verdict
  "PREV's verdict when it will not be replaced, else nil. The lu-pending
  placeholder and a retryable model failure with attempts left are both
  rewritten later; linking to them would pin a digest that no longer exists."
  [prev]
  (let [v (:pattern_verdicts prev)]
    (when (and (map? v)
               (not= "lu-proposals-pending" (some-> (:state v) name))
               (not (and (retryable-verdict? v)
                         (< (long (or (:model_attempts v) 1)) 3))))
      v)))

(defn connection-for
  "Compute ID's connection from durable same-session records. Never skips an
  immediate operator predecessor. A durable void verdict is still linked."
  [store id]
  (if-let [{prev-id :id prev :record} (adjacent-turn store id -1)]
    (if-let [reason (absent-reason prev)]
      {:status :prev-absent :turn/id prev-id :reason reason}
      (if-let [verdict (final-verdict prev)]
        {:status :linked :turn/id prev-id :sha256 (verdict-digest verdict)}
        {:status :prev-pending :turn/id prev-id}))
    {:status :session-start}))

(defn repair-connection!
  "CAS-like repair of ID's pending receipt. Only the exact pending receipt for
  its immediate predecessor may become linked; linked and absence receipts are
  immutable. Returns the resulting verdict, or nil when ID has none."
  [store id]
  (with-session-lock
    store id
    #(when-let [{prev-id :id prev :record} (adjacent-turn store id -1)]
       (when-let [previous-verdict (when-not (absent-reason prev)
                                    (final-verdict prev))]
         (let [linked {:status :linked :turn/id prev-id
                       :sha256 (verdict-digest previous-verdict)}]
           (:pattern_verdicts
            (ts/update-record!
             store id
             (fn [record]
               (let [receipt (get-in record [:pattern_verdicts :connection])]
                 (if (and (= :prev-pending (some-> (:status receipt) keyword))
                          (= prev-id (:turn/id receipt)))
                   (assoc-in record [:pattern_verdicts :connection] linked)
                   record))))))))))

(defn- repair-successor!
  [store id]
  (when-let [{successor-id :id} (adjacent-turn store id 1)]
    (repair-connection! store successor-id)))

(declare occurrence-writer-executor)

(defn- publish-connection-async!
  [opts store id]
  (when-let [writer (:strand-writer! opts)]
    ((or (:occurrence-async! opts)
         #(.submit occurrence-writer-executor ^Runnable %))
     (fn []
       (let [turn (some-> (ts/read-record store id) (assoc :id id))
             result (try
                      (writer (or (:strand-config opts)
                                  (mention-writer/default-config)) turn)
                      (catch Throwable e
                        {:failed :ma-strand-write-exception
                         :detail (.getMessage e)}))]
         (ts/update-record! store id #(assoc % :ma_strand_write result)))))))

(defn- ensure-connection!
  ([store id] (ensure-connection! {} store id))
  ([opts store id]
  (with-session-lock
    store id
    #(let [connection (connection-for store id)
           successor-id (:id (adjacent-turn store id 1))
           successor-before (when successor-id
                              (get-in (ts/read-record store successor-id)
                                      [:pattern_verdicts :connection]))]
       (ts/update-record!
        store id
        (fn [record]
          (if (and (map? (:pattern_verdicts record))
                   (nil? (get-in record [:pattern_verdicts :connection])))
            (assoc-in record [:pattern_verdicts :connection] connection)
            record)))
       (repair-connection! store id)
       (repair-successor! store id)
       (publish-connection-async! opts store id)
       (when (and successor-id
                  (not= successor-before
                        (get-in (ts/read-record store successor-id)
                                [:pattern_verdicts :connection])))
         (publish-connection-async! opts store successor-id))
       (:pattern_verdicts (ts/read-record store id))))))

(defn backfill-connections!
  "Give every stored 馬 verdict without a :connection one, from the strand
  links (run turn-store/backfill-strand! first). WRITE? false counts only."
  [store & {:keys [write?]}]
  (let [ids (->> (ts/list-records store :limit Long/MAX_VALUE)
                 (filter #(and (map? (get-in % [:record :pattern_verdicts]))
                               (nil? (get-in % [:record :pattern_verdicts :connection]))))
                 (map :id))
        by-status (frequencies
                   (for [id ids]
                     (let [connection (connection-for store id)]
                       (when write?
                         (with-session-lock
                           store id
                           #(ts/update-record!
                             store id
                             (fn [record]
                               (if (and (map? (:pattern_verdicts record))
                                        (nil? (get-in record [:pattern_verdicts :connection])))
                                 (assoc-in record [:pattern_verdicts :connection] connection)
                                 record)))))
                       (:status connection))))]
    {:mode (if write? :write :dry-run) :verdicts (count ids) :by-status by-status}))

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
               [(keyword role)
                (let [m (or (get raw role) (get raw (keyword role)))]
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
  "Compact whistle prompt. Turn texts remain byte-for-byte present because
  returned evidence quotes are checked against those exact strings."
  [window record-id patterns]
  (str
   "JSON only. Map IF=I1, HOWEVER=I2, BECAUSE=G, THEN=B to elements actually "
   "named by the turns; topic similarity is insufficient. proposal=" record-id "\nTURNS\n"
   (str/join "\n"
             (for [{id :id r :record} window]
               (format "[%s %s]\nO:%s\nR:%s"
                       id (str (or (:created_at r) ""))
                       (str (or (:source_text r) ""))
                       (str (or (:reply_text r) "")))))
   "\nPATTERNS\n"
   (str/join "\n"
             (for [p patterns]
               (format "[%s]\nI1:%s\nI2:%s\nB:%s\nG:%s"
                       (:id p)
                       (str/replace (or (get (:parts p) "IF") "MISSING") #"\s+" " ")
                       (str/replace (or (get (:parts p) "HOWEVER") "MISSING") #"\s+" " ")
                       (str/replace (or (get (:parts p) "THEN") "MISSING") #"\s+" " ")
                       (str/replace (or (get (:parts p) "BECAUSE") "MISSING") #"\s+" " "))))
   "\nFor each pattern return turn (proposal only) and chain (any listed turn). "
   "Each non-null role needs element, turn, an exact >=5-word quote, and who "
   "(operator|reply). Null roles need element:null and why. Schema:\n"
   "[{\"pattern\": \"family/name\",\n"
   "  \"turn\":  {\"I1\": {..}, \"I2\": {..}, \"G\": {..}, \"B\": {..}},\n"
   "  \"chain\": {\"I1\": {..}, \"I2\": {..}, \"G\": {..}, \"B\": {..}}}]"))

(defn- no-reader-pattern [sources shown p why]
  (let [void {:element nil :why why}]
    {:pattern (:id p) :sha256 (:sha256 p)
     :sources (vec (or (get sources (:id p)) []))
     :shown (boolean (get shown (:id p)))
     :turn {:I1 void :I2 void :G void :B void}
     :chain {:I1 void :I2 void :G void :B void}
     :turn_mapped 0 :chain_mapped 0 :status "void"}))

(defn- reply-result
  "Parse one batch reply, distinguishing a cut-off JSON array from other
  malformed output."
  [reply]
  (let [s (str reply)
        lo (.indexOf ^String s "[")
        hi (.lastIndexOf ^String s "]")]
    (cond
      (and (not (neg? lo)) (< hi lo))
      {:failure_kind "model/truncated-reply"}

      :else
      (let [parsed (extract-json-array s)]
        (if (seq (filter :pattern (or parsed [])))
          {:failure_kind nil :parsed parsed}
          {:failure_kind "model/unparsed-reply"})))))

(defn- pattern-row
  [p row sources shown texts record-id window-ids]
  (let [turn-m (normalize-window (:turn row) texts #{record-id})
        chain-m (normalize-window (:chain row) texts window-ids)
        chain-n (stored-count chain-m)]
    {:pattern (:id p) :sha256 (:sha256 p)
     :sources (vec (or (get sources (:id p)) []))
     :shown (boolean (get shown (:id p)))
     :turn turn-m :chain chain-m
     :turn_mapped (stored-count turn-m)
     :chain_mapped chain-n
     :status (status-of chain-n)}))

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
        patterns (mapv #(read-pattern (or library pattern-library) (:id %)) proposed)
        batches
        (mapv
         (fn [batch-index batch]
           (let [reply (cond
                         (not ask) (model-failure "model/no-reader")
                         (seq batch)
                         (try
                           (let [result (ask {:seat seat
                                              :prompt (prompt-for window record-id batch)
                                              :timeout-ms timeout-ms})]
                             (cond
                               (string? result) result
                               (and (map? result) (:failure_kind result)) result
                               :else (model-failure "model/error" nil "ask returned nil")))
                           (catch Exception e
                             (model-failure "model/error" nil (.getMessage e))))
                         :else (model-failure "model/no-reader"))
                 result (if (string? reply) (reply-result reply) reply)
                 failure-kind (:failure_kind result)
                 outcome (-> (dissoc result :parsed)
                             (assoc :batch batch-index
                                    :patterns (mapv :id batch)))
                 rows (if failure-kind
                        (mapv #(no-reader-pattern
                                sources shown %
                                (if (= "model/no-reader" failure-kind)
                                  "no reader" failure-kind))
                              batch)
                        (let [by-pattern
                              (into {}
                                    (comp (filter map?)
                                          (keep (fn [row]
                                                  (when-let [p (:pattern row)]
                                                    [(str/replace-first (str p) #"^~" "")
                                                     row]))))
                                    (:parsed result))]
                          (mapv #(pattern-row % (get by-pattern (:id %) {})
                                              sources shown texts record-id window-ids)
                                batch)))]
             {:outcome outcome :rows rows}))
         (range)
         (partition-all batch-size patterns))
        outcomes (mapv :outcome batches)
        rows (into [] (mapcat :rows) batches)
        first-failure (some #(when (:failure_kind %) %) outcomes)
        model-outcome (assoc (or first-failure {:failure_kind nil})
                             :batches outcomes)]
    {:version 1
     :at (str (java.time.Instant/now))
     :window (mapv :id window)
     :reader (if ask (str (or seat "unknown")) "none")
     :model_outcome model-outcome
     :patterns (vec rows)}))

(defn- verdict-pattern-ids
  "The set of pattern ids a stored verdict covers."
  [v]
  (into #{} (map :pattern) (:patterns v)))

(defn- retryable-verdict? [verdict]
  (contains? retryable-model-failure-kinds
             (get-in verdict [:model_outcome :failure_kind])))

(def ^:private max-listener-attempts 3)

(defn- record-listener-failure!
  [store id e]
  (try
    (ts/update-record!
     store id
     (fn [record]
       (if (map? (:pattern_verdicts record))
         record
         (assoc record :ma_listener_failure
                {:at (str (java.time.Instant/now))
                 :error (.getMessage e)
                 :class (.getName (class e))
                 :attempts (inc (long (or (get-in record [:ma_listener_failure :attempts])
                                          0)))}))))
    (catch Exception receipt-error
      (binding [*out* *err*]
        (println "[ma-occurrence] listener failure receipt write failed"
                 id (.getMessage receipt-error))))))

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
                       :why "lu proposals pending" :patterns []
                       :connection (connection-for store id)}]
          ;; Never clobber a real verdict (a re-fired :aligned on a turn
          ;; read before CL-3); only an absent or pending one is replaced.
          (ts/update-record! store id
                             #(let [v (:pattern_verdicts %)]
                                (if (or (nil? v) (= "lu-proposals-pending" (some-> v :state name)))
                                  (-> %
                                      (assoc :pattern_verdicts pending)
                                      (dissoc :ma_listener_failure))
                                  %)))
          (ensure-connection! opts store id))
        (if (and record (= "operator" (:origin record)) (empty? proposed))
          ;; Joe 2026-11-18: every Circus animal writes into one connected
          ;; component. A turn with nothing to read still gets a typed 馬
          ;; record, so its successor links to it instead of waiting forever.
          (do (ts/update-record! store id
                                 #(let [v (:pattern_verdicts %)]
                                    (if (or (nil? v) (= "lu-proposals-pending" (some-> v :state name)))
                                      (assoc % :pattern_verdicts
                                             {:version 1 :at (str (java.time.Instant/now))
                                              :reader "none" :state :no-proposed-patterns
                                              :why "no proposed patterns" :patterns []
                                              :connection (connection-for store id)})
                                      %)))
              (ensure-connection! opts store id))
        (when (and record
                 (= "operator" (:origin record))
                 (seq proposed))
        (let [existing (:pattern_verdicts record)]
          (if (and (map? existing)
                   (= (set (map :id proposed)) (verdict-pattern-ids existing))
                   (or (not (retryable-verdict? existing))
                       (>= (long (or (:model_attempts existing) 1)) 3)))
            (ensure-connection! opts store id)
            (let [attempt (if (and (map? existing) (retryable-verdict? existing))
                            (inc (long (or (:model_attempts existing) 1)))
                            1)
                  read-verdict (verdict-map opts store id)
                  verdicts (cond-> (assoc read-verdict
                                          :model_attempts attempt
                                          :connection (connection-for store id))
                             (and (retryable-verdict? read-verdict)
                                  (>= attempt 3))
                             (assoc :model_retry_exhausted true))]
              (ts/update-record! store id #(-> %
                                               (assoc :pattern_verdicts verdicts)
                                               (dissoc :ma_listener_failure)))
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
              (ensure-connection! opts store id))))))))
    (catch Exception e
      (record-listener-failure! store id e)
      nil)))

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
  "How many earlier operator turns of the same session an aligned event
  re-checks, walking the strand links back from it."
  20)

(defn- retry-candidates
  "Ids among the RETRY-WINDOW operator turns before ID in its session (strand
  links, no store listing) whose verdict is a retryable model failure,
  pending proposals, or whose listener failure receipt remains under the
  attempt cap."
  [store id]
  (->> (iterate (fn [rid] (when rid (:id (adjacent-turn store rid -1)))) id)
       (drop 1)
       (take-while some?)
       (take retry-window)
       (keep (fn [rid]
               (let [record (ts/read-record store rid)
                     v (:pattern_verdicts record)
                     listener-failure (:ma_listener_failure record)]
                 (when (or (and (map? v)
                                (or (= "lu-proposals-pending" (some-> v :state name))
                                    (and (retryable-verdict? v)
                                         (not (:model_retry_exhausted v)))))
                           (and (not (map? v))
                                (map? listener-failure)
                                (< (long (or (:attempts listener-failure) 0))
                                   max-listener-attempts)))
                   rid))))
       vec))

(defn handle-event
  "One turn-service event for the occurrence reading under OPTS: run the
  verdict only for an :aligned event with a record id (the alignment
  reading raises :aligned after storing the alignment; the occurrence
  reading needs the reply and the computed ports too, so a fair verdict
  runs after it, not on :ported)."
  [{:keys [store notify!] :as opts} {:keys [event id]}]
  (when (and (= :lu-proposals event) id)
    ;; 驴's proposals landed after 馬 read the turn: replace the placeholder.
    (let [store (if (fn? store) (store) store)]
      (when (and store (= "lu-proposals-pending"
                          (some-> (ts/read-record store id) :pattern_verdicts :state name)))
        (verdict! (assoc opts :store store :notify! notify!) id))))
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
  [request]
  (wa/ask (assoc request :caller "ma-occurrence")))

(defn- live-service []
  (try (some-> (requiring-resolve 'futon3c.transport.http/xiang-bell-service) deref (apply []))
       (catch Throwable _ nil)))

(defn start-live!
  "Wire the listener to the running service: its store and notify are found
  at event time, so this survives reloads of either namespace."
  []
  (start! {:ask whistle-ask
           :occurrence-writer! occurrences/write!
           :strand-writer! ma-strand/write!
           :store (fn [] (some-> (live-service) :config :store))
           :notify! (fn [ev] (when-let [svc (live-service)]
                               (@#'turn-service/notify-turn! svc ev)))}))
