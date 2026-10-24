(ns futon3c.xiang.lu-rerank
  "驴, the retrieval reranker: re-pick the patterns shown to a turn.

  The embedding retrieval picks the top 3 patterns for a turn; the offline
  experiment (LAB-2026-10-24-lu-rerank-offline, 1badc90c8) showed a Haiku
  reranker over a classical candidate pool picks patterns that actually occur
  twice as often (chain_mapped >= 2: 12 vs 6 over 24 turns). This namespace
  is that reranker as a library: a candidate pool (embedding top 20 plus the
  giant-graph neighbours of the session's already-occurrent patterns), one
  bounded whistle to the 驴 seat with a verbatim-quote ground for every pick,
  and an embedding fallback so a timeout never blocks a render.

  Injectable :ask like futon3c.xiang.ma-reading / ma-occurrence; nothing here
  writes to any turn record."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.pattern-search :as pattern-search]
            [futon3c.xiang.ma-occurrence :as ma]
            [futon3c.xiang.turn-store :as ts])
  (:import [java.net HttpURLConnection URL]))

(def defaults
  {:seat "驴-haiku"
   :ask nil
   :timeout-ms 30000
   :keep 3
   :embedding-top 20
   :neighbour-cap 15})

(def publication-edn
  "The published-graph pointer; the newest published graph is located through
  it. Configurable per call with :graph-path."
  "/home/joe/notes/experiments/2026-10-24-session-cascade/mined-pattern-graph.json.publication.edn")

(def edge-kind-order
  "Neighbours are ranked by edge kind, best first."
  ["why" "how" "co-cited" "rejected-beside" "next-in-session"])

(def min-quote-words 5)

;; ------------------------------------------------------------- the graph leg

(defn graph-path
  "The newest published graph's path, read from the publication pointer (an
  EDN map with :graph-path). Falls back to the pointer's own directory's
  graph.json when the pointer is unreadable."
  ([]
   (graph-path publication-edn))
  ([pointer-path]
   (or (try (-> (edn/read-string (slurp pointer-path))
                :graph-path
                str
                not-empty)
            (catch Throwable _ nil))
       (str (io/file (.getParentFile (io/file pointer-path)) "graph.json")))))

(defn load-graph
  "The graph as {:edges [{:a .. :b .. :kind ..}]}; nil when absent/unreadable."
  [path]
  (try
    (let [parsed (json/parse-string (slurp path) true)]
      {:edges (vec (:edges parsed))})
    (catch Throwable _ nil)))

(defn seed-patterns
  "The distinct pattern ids with status occurrence or partial in the
  :pattern_verdicts of EARLIER operator records of the record's session
  (strictly earlier by created_at), ~-stripped, first-seen order."
  [store record-id]
  (when-let [record (ts/read-record store record-id)]
    (let [session (:session_id record)
          at (str (:created_at record))]
      (into [] (comp (filter #(= session (:session_id (:record %))))
                     (filter #(= "operator" (:origin (:record %))))
                     (filter #(< (compare (str (:created_at (:record %))) at) 0))
                     (mapcat #(get-in (:record %) [:pattern_verdicts :patterns]))
                     (filter #(contains? #{"occurrence" "partial"} (:status %)))
                     (map #(str/replace-first (str (:pattern %)) #"^~" ""))
                     (remove str/blank?)
                     (distinct))
            (ts/list-records store 500)))))

(defn graph-neighbours
  "The neighbours of SEEDS in GRAPH, at most CAP, ordered by best edge kind
  (why, how, co-cited, rejected-beside, next-in-session), then by id."
  ([graph seeds] (graph-neighbours graph seeds (:neighbour-cap defaults)))
  ([graph seeds cap]
   (let [prio (into {} (map-indexed (fn [i k] [k i]) edge-kind-order))
         seeds (set seeds)
         best (reduce (fn [acc {:keys [a b kind]}]
                        (if (or (contains? seeds a) (contains? seeds b))
                          (let [other (if (contains? seeds a) b a)
                                rank (get prio kind 99)]
                            (if (or (contains? seeds other)
                                    (not (string? other)))
                              acc
                              (update acc other
                                      (fn [cur best-rank]
                                        (if cur (min cur best-rank) best-rank))
                                      rank)))
                          acc))
                      {} (:edges graph))]
     (->> (sort-by (fn [[id rank]] [rank (str id)]) (seq best))
          (mapv first)
          (take (long cap))))))

;; --------------------------------------------------------- the embedding leg

(defn embedding-top
  "The embedding top N ids for TEXT, the same search the pattern-card
  provider feeds from (futon3c.agency.pattern-search/search — the resident
  futon3a notions_search over minilm_pattern_embeddings.json), ~-stripped.
  Nil when the search yields nothing."
  ([text] (embedding-top text (:embedding-top defaults)))
  ([text n]
   (->> (pattern-search/search (str text) (long n))
        (keep #(some-> (:id %) str))
        (map #(str/replace-first % #"^~" ""))
        (remove str/blank?)
        vec
        not-empty)))

(defn candidate-pool
  "The 驴 candidate pool for RECORD-ID: (a) the embedding top 20 for the
  record's source_text, then (b) the graph neighbours (at most 15, ranked by
  edge kind) of the session's earlier occurrence/partial patterns. Deduped,
  (a) first. GRAPH may be nil (pool (a) alone)."
  ([store record-id] (candidate-pool store record-id (load-graph (graph-path))))
  ([store record-id graph]
   (when-let [record (ts/read-record store record-id)]
     (let [a (or (embedding-top (str (or (:source_text record) "")))
                 [])
           b (graph-neighbours graph (seed-patterns store record-id))]
       (vec (distinct (concat a b)))))))

;; ---------------------------------------------------------------- the rerank

(defn- candidate-block
  "One candidate's four parts for the prompt (ma-occurrence's read-pattern
  reads the parts the same way occurrence_square.py does, sha256 kept)."
  [pid]
  (let [{:keys [parts]} (ma/read-pattern pid)
        flat #(str/replace (or (get parts %) "MISSING") #"\s+" " ")]
    (format "--- pattern %s\nIF: %s\nHOWEVER: %s\nTHEN: %s\nBECAUSE: %s"
            pid (flat "IF") (flat "HOWEVER") (flat "THEN") (flat "BECAUSE"))))

(defn prompt-for
  "The 驴 whistle: rank the candidates whose IF names a situation element this
  operator turn actually contains, each pick with a verbatim quote from the
  operator text (string-checked afterwards). Ported from the offline script
  lu_rerank_offline.py."
  [source-text pool]
  (str
   "A reranking task. You write one JSON reply; nothing else.\n\n"
   "Below are candidate patterns and one operator turn. Rank the SIX "
   "patterns whose IF best names a situation element this operator turn "
   "actually contains. For each pick, give a verbatim quote of at least "
   "5 words, copied exactly from the operator text, that the pattern's "
   "IF names (it will be checked by string match).\n\n"
   "Operator turn:\n" (str (or source-text "")) "\n\n"
   (str/join "\n\n" (mapv candidate-block pool))
   "\n\nReply with strict JSON only, an array of at most 6 objects in ranked order:\n"
   "[{\"pattern\": \"family/name\", \"quote\": \"verbatim >=5 words from the operator text\"}]"))

(defn- norm [s] (str/replace (str (or s "")) #"\s+" " "))

(defn check-picks
  "The 驴 quote ground: a pick stands only if its quote, at least 5 words,
  occurs verbatim (whitespace-normalised) in SOURCE-TEXT and names a pooled
  pattern. Failed picks are dropped; the first KEEP survivors are taken.
  Rows are the parsed reply objects in ranked order."
  ([source-text pool rows] (check-picks source-text pool rows (:keep defaults)))
  ([source-text pool rows keep]
   (let [pool (set pool)
         text (norm source-text)]
     (reduce (fn [out row]
               (if (>= (count out) keep)
                 (reduced out)
                 (let [pid (-> row (get "pattern") str (str/replace-first #"^~" ""))
                       quote (-> row (get "quote") str norm str/trim)]
                   (cond
                     (not (contains? pool pid)) out
                     (or (not (string? quote)) (< (count (str/split quote #"\s+")) min-quote-words)) out
                     (not (str/includes? text quote)) out
                     :else (conj out {:id pid :rank (inc (count out)) :quote quote})))))
             [] rows))))

(defn- extract-array [reply]
  (try
    (let [^String s (str reply)
          lo (.indexOf s "[")
          hi (.lastIndexOf s "]")]
      (when (and (not (neg? lo)) (> hi lo))
        (json/parse-string (subs s lo (inc hi)))))
    (catch Throwable _ nil)))

(defn rerank
  "Re-pick the patterns for RECORD-ID: the 驴 seat reranks the candidate pool
  and the first KEEP verbatim-grounded picks are kept. Returns
  {:ranked [{:id :rank :quote}] :source \"驴\" | \"embedding-fallback\"
   :ms n :reason ...}. On a timeout, an error, no reader wired, an empty pool
  or zero surviving picks, the embedding top 3 is returned with
  :source \"embedding-fallback\" and the reason. Never throws. Nothing is
  written to any record."
  [opts store record-id]
  (try
    (let [opts (merge defaults opts)
          record (and store (ts/read-record store record-id))
          source-text (str (or (:source_text record) ""))
          pool (candidate-pool store record-id (:graph opts))
          t0 (System/nanoTime)
          reply (when (and (:ask opts) (seq pool))
                  (try ((:ask opts) {:seat (:seat opts)
                                     :prompt (prompt-for source-text pool)
                                     :timeout-ms (:timeout-ms opts)})
                       (catch Throwable t
                         (throw (ex-info "ask-failed"
                                         {:reason (.getMessage t)})))))
          ms (quot (- (System/nanoTime) t0) 1000000)]
      (if (and (seq pool) (string? reply))
        (let [rows (filter map? (or (extract-array reply) []))
              picks (check-picks source-text pool rows (:keep opts))]
          (if (seq picks)
            {:ranked picks :source "驴" :ms ms
             :pool-n (count pool)}
            {:ranked (vec (map-indexed (fn [i id] {:id id :rank (inc i)})
                                       (embedding-top source-text (:keep opts))))
             :source "embedding-fallback" :ms ms
             :reason "zero-surviving-picks" :pool-n (count pool)}))
        {:ranked (vec (map-indexed (fn [i id] {:id id :rank (inc i)})
                                   (embedding-top source-text (:keep opts))))
         :source "embedding-fallback" :ms ms
         :reason (cond
                   (not (seq pool)) "empty-pool"
                   (nil? (:ask opts)) "no-reader"
                   :else "timeout-or-empty-reply")}))
    (catch Throwable t
      (try
        (let [record (and store (ts/read-record store record-id))
              source-text (str (or (:source_text record) ""))]
          {:ranked (vec (map-indexed (fn [i id] {:id id :rank (inc i)})
                                     (embedding-top source-text (:keep (merge defaults opts)))))
           :source "embedding-fallback"
           :ms (:ms (ex-data t) 0)
           :reason (str "ask-error: " (or (.getMessage t) (class t)))})
        (catch Throwable _ {:ranked [] :source "embedding-fallback" :ms 0
                            :reason "error"})))))

;; ------------------------------------------------------------ live wiring

(defn whistle-ask
  "The production :ask: a bounded whistle to SEAT through Agency's HTTP API,
  synchronous, as ma-reading's. Returns the seat's text, or nil on any
  failure or timeout."
  [{:keys [seat prompt timeout-ms]}]
  (try
    (let [conn (doto ^HttpURLConnection
                (.openConnection (URL. "http://127.0.0.1:7070/api/alpha/whistle"))
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 5000)
                 (.setReadTimeout (int (+ (or timeout-ms 60000) 15000)))
                 (.setRequestProperty "Content-Type" "application/json"))]
      (with-open [w (io/writer (.getOutputStream conn))]
        (.write w (json/generate-string {:agent-id seat :prompt prompt
                                         :timeout-ms (or timeout-ms 60000)
                                         :caller "lu-rerank"})))
      (let [body (json/parse-string (slurp (.getInputStream conn)) true)]
        (when (:ok body) (some-> (:response body) str))))
    (catch Throwable _ nil)))
