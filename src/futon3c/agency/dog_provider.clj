(ns futon3c.agency.dog-provider
  "狗 pre-hook (M-repl-circus, \"狗: recall before the agent's turn, use
   checked after it\"): recall relevant minus40 memories for an agent turn
   that answers an operator turn, before the turn runs.

   Relevance is seeded from 驴's patterns for the turn (the top patterns'
   ids and titles, read from the record's .patterns.json at arrival time;
   11-11: \"it could either read the operator text patterns or wait for
   the 驴 patterns\") AND from the operator turn's own text keywords —
   both queries run and their items take the slots alternately (驴,
   keyword, 驴; see merge-items). The read is the same one m40_find makes:
   GET <minus40-store :7093>/api/alpha/evidence/text-search?q=... (the
   store's FTS5 sidecar; hits whose body is a claude-code memory are
   grouped by project/name, one item per memory). Dated directives
   (\"stop dispatches on 2026-10-23\") are dropped — a lapsed order is
   not a current fact.

   Measured 2026-11-11 over 10 real operator turns of session ed69fb38:
   p50 75 ms, max 101 ms — the max does not fit the default provider
   budget (100 ms), so the recall runs on a background thread when the
   象 turn record arrives (a turn-service :recorded listener, as 驴's
   rerank does), and the provider only READS the cache. A render that
   finds no cached result says \"memory: not ready\" rather than waiting
   or raising the budget.

   The recalled items ride the segment as :segment/dog-recall so
   prompt-line can write them onto the 象 turn record as :dog_recall
   (the post-hook packet reads that field). Absence is always typed:
   {:status \"not-ready\"|\"failed\"|\"none\"}.

   Analysis seats get nothing (as for 驴). The off-switch file
   ~/code/data/minus40/dog-off (style of lu-off) disables the whole
   hook. minus40 is read-only from here: this namespace never writes
   to the store.

   Tests inject :request-fn (fn [url-string] -> parsed JSON map) so no
   test touches a real store."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as turn-store])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration Instant]
           [java.util.concurrent ExecutorService Executors ThreadFactory]))

(def minus40-base
  "The minus40 store (user unit minus40-store on :7093). Never the live
   :7073."
  "http://127.0.0.1:7093")

(def off-switch-path
  "The 狗 off-switch, in the style of 驴's lu-off: when this file exists
   the listener and the provider do nothing."
  "/home/joe/code/data/minus40/dog-off")

(def max-items 3)
(def ^:private keyword-count 6)
(def ^:private search-limit 50)
(def ^:private query-timeout-ms 5000)
(def ^:private lu-pattern-count 5)
(def ^:private lu-term-count 8)

(def ^:private stopwords
  #{"the" "a" "an" "and" "or" "of" "to" "in" "is" "it" "for" "on" "with"
    "that" "this" "are" "was" "be" "as" "at" "by" "from" "have" "has"
    "not" "but" "so" "we" "you" "i" "they" "them" "their" "our" "your"
    "he" "she" "his" "her" "its" "my" "me" "us" "do" "does" "did" "will"
    "would" "could" "should" "can" "may" "might" "if" "then" "than" "when"
    "what" "which" "who" "how" "all" "any" "each" "about" "into" "over"
    "such" "no" "yes" "up" "out" "just" "also" "now" "here" "there"})

(def defaults
  {:base minus40-base
   :off-switch off-switch-path
   :request-fn nil
   :store nil})

(defonce ^:private !recalls (atom {}))
(defonce ^:private !in-flight (atom #{}))

(defn reset-cache!
  "Testing seam: drop every cached recall and in-flight marker."
  []
  (reset! !recalls {})
  (reset! !in-flight #{}))

(defn cached-recall [record-id]
  (get @!recalls (str record-id)))

(def ^:dynamic *off-switch-path*
  "Seam over the off-switch file path so tests never touch the real one."
  nil)

(defn- off? [opts]
  (try (.isFile (io/file (str (or (:off-switch opts) *off-switch-path* off-switch-path))))
       (catch Throwable _ false)))

;; ------------------------------------------------------------- the m40 read

(def ^:dynamic *request-fn*
  "Seam over the HTTP read (fn [url-string] -> parsed JSON map), so tests
   and the provider's own background kick never touch a real store. Nil
   means the production GET against :7093."
  nil)

(defn- default-request-fn
  "One read-only JSON GET against the minus40 store's FTS5 text-search."
  [^String url]
  (let [req (-> (HttpRequest/newBuilder (URI/create url))
                (.timeout (Duration/ofMillis query-timeout-ms))
                (.header "Accept" "application/json")
                (.method "GET" (HttpRequest$BodyPublishers/noBody))
                .build)
        resp (.send (HttpClient/newHttpClient) req
                    (HttpResponse$BodyHandlers/ofString))]
    (when (<= 200 (.statusCode resp) 299)
      (json/parse-string (.body resp) true))))

(defn- effective-request-fn [opts]
  (or (:request-fn opts) *request-fn* default-request-fn))

(defn- content-terms
  "FTS5-safe content terms of S, in order: stopwords dropped, words of 3
   letters or fewer dropped, duplicates dropped. Hyphens are kept (the
   store's FTS5 index tokenises them as the operator text did)."
  [s]
  (->> (str/split (str/replace (str s) #"[^0-9A-Za-z\-]+" " ") #"\s+")
       (filter #(and (> (count %) 3)
                     (not (contains? stopwords (str/lower-case %)))))
       distinct))

(defn keywords
  "The fast relevance query from the operator turn's text: the store's
   FTS5 sidecar treats a space-separated query as AND, so a whole turn
   matches only its own verbatim echoes (lu situations). Extract up to
   KEYWORD-COUNT distinctive content words (longest first, stopwords
   dropped, FTS5-significant characters stripped) and OR them."
  [text]
  (->> (content-terms text)
       (sort-by count >)
       (take keyword-count)
       (str/join " OR ")))

(defn lu-terms
  "驴's terms for RECORD-ID, the better-aimed seed: the top
   LU-PATTERN-COUNT patterns (by score) of the record's .patterns.json,
   their id name-parts and titles mined for content terms, up to
   LU-TERM-COUNT. When no patterns file exists, falls back to the
   record's own :lu_ranked :ranked ids. Nil when neither exists — the
   caller then falls back to the operator text's keywords."
  [store record-id]
  (let [record-id (str record-id)]
    (when (turn-store/record-id? record-id)
      (let [f (io/file (turn-store/pattern-candidates-path store record-id))]
        (if (.isFile f)
          (some->> (try (json/parse-string (slurp f) true)
                        (catch Throwable _ nil))
                   vals (apply concat)
                   (sort-by #(or (:score %) 0) >)
                   (take lu-pattern-count)
                   (mapcat (fn [p] (content-terms
                                    (str/lower-case
                                     ;; the id's name-part, not its family
                                     ;; ("operator" drags junk), plus title
                                     (str (peek (str/split (str (:id p)) #"/"))
                                          " " (:title p))))))
                   distinct (take lu-term-count) seq)
          (some->> (when store (turn-store/read-record store record-id))
                   :lu_ranked :ranked
                   (map :id)
                   (mapcat #(content-terms (str/lower-case (peek (str/split (str %) #"/")))))
                   distinct (take lu-term-count) seq))))))

(defn readable-project
  "A path-mangled project slug as a readable name: `-home-joe-code-voxterm`
   becomes `voxterm`. A plain project stays itself. The raw :memory
   handle is unchanged, so the item stays resolvable through m40_find in
   one call."
  [project]
  (let [p (str project)]
    (if (str/starts-with? p "-")
      (or (last (remove str/blank? (str/split p #"-"))) p)
      p)))

(defn- dated-directive?
  "A dated instruction is not a current fact: S (a memory's slug plus
   hook) names a directive verb bound to an ISO date ('stop dispatches
   on 2026-10-23', 'hold until <date>'). Such memories are DROPPED rather
   than tagged: offered at all, the prompt line presents them as current
   context, which is exactly the observed failure (a lapsed order
   recalled as a standing fact on a dispatch turn). An agent that needs
   the history can still find it through m40."
  [s]
  (let [s (str s)]
    (boolean (and (re-find #"\d{4}-\d{2}-\d{2}" s)
                  (re-find #"(?i)\b(stop|hold|until|pause|freeze|resume|defer|cancel)\b" s)))))

(defn- memory-item
  "One text-search hit whose body is a claude-code memory, as
   {:memory \"<project>/<slug>\" :display \"<readable>/<slug>\" :score :why}.
   The why is the memory's hook trimmed to 8 words. Nil for non-memory
   hits (turn records and lu situations share the index) and for dated
   directives (see dated-directive?)."
  [hit]
  (let [body (get-in hit [:entry :evidence/body])
        body (cond
               (map? body) body
               (string? body) (try (json/parse-string body true)
                                   (catch Throwable _ nil))
               :else nil)]
    (when (and (map? body) (= "claude-code" (:source body)))
      (let [project (some-> (:project body) str)
            slug (some-> (:name body) str)
            hook (str (or (:hook body) ""))
            why (let [words (->> (str/split (str/trim hook) #"\s+")
                                 (remove str/blank?)
                                 (take 8))]
                  (if (seq words) (str/join " " words) "keyword match"))]
        (when-not (or (str/blank? project) (str/blank? slug)
                      (dated-directive? (str slug " " hook)))
          {:memory (str project "/" slug)
           :display (str (readable-project project) "/" slug)
           :score (:score hit)
           :why why})))))

(defn- recall-from-response
  "Group one text-search response by memory, best score first."
  [response]
  (->> (:results response)
       (keep memory-item)
       (group-by :memory)
       (map (fn [[_ items]] (first (sort-by #(or (:score %) Double/NEGATIVE_INFINITY) > items))))
       (sort-by #(or (:score %) Double/NEGATIVE_INFINITY) >)
       (take max-items)
       vec))

(defn- search-memories
  "One text-search for QUERY, memory-grouped items."
  [request-fn base query]
  (let [url (str base
                 "/api/alpha/evidence/text-search?q="
                 (URLEncoder/encode query "UTF-8")
                 "&limit=" search-limit)]
    (recall-from-response (or (request-fn url) {}))))

(defn- merge-items
  "Merge 驴-seeded and keyword items into the MAX-ITEMS slots, ALTERNATING
   with 驴 first (驴, keyword, 驴, ...). An item whose :memory already has
   a slot is skipped, so a duplicate never takes two slots; when one
   source has no unseen items left the other fills the rest. 11-11 merge
   pass: 驴-fills-every-slot-first was the one regression cause (UkLbCu:
   three weak 驴 hits took all slots and the keyword query never ran;
   vsC2tD: a 驴 item displaced a relevant keyword hit), while ni2vBM
   showed the 驴 seed itself is worth a slot."
  [lu-items kw-items]
  (let [first-unseen (fn [items seen] (first (remove #(contains? seen (:memory %)) items)))]
    (loop [seen #{} out [] prefer-lu? true]
      (let [lu-item (first-unseen lu-items seen)
            kw-item (first-unseen kw-items seen)
            pick (cond (and prefer-lu? lu-item) lu-item
                       (and (not prefer-lu?) kw-item) kw-item
                       ;; one side exhausted (or all duplicates): the
                       ;; other fills the remaining slots
                       lu-item lu-item
                       :else kw-item)]
        (if (or (nil? pick) (= max-items (count out)))
          (vec out)
          (recur (conj seen (:memory pick)) (conj out pick) (not prefer-lu?)))))))

(defn recall
  "One 狗 recall for the operator turn TEXT. BOTH queries run: the
   驴-seeded one (OPTS :lu-terms, see lu-terms) and the operator text's
   keyword query; their items are merged into the slots alternately
   (驴, keyword, 驴 — see merge-items), so neither source can starve
   the other. Each item records which source produced it (:source
   \"lu\" or \"keywords\"). Returns {:at :items [{:memory :display :score
   :why :source}...] :query-ms :query :query-source}, with :status
   \"none\" when no memory matched and \"failed\" on any error. Never
   throws. OPTS as per defaults; :request-fn receives the query URL and
   returns the parsed JSON response."
  ([text] (recall text {}))
  ([text opts]
   (let [opts (merge defaults opts)
         t0 (System/nanoTime)
         elapsed #(quot (- (System/nanoTime) t0) 1000000)
         request-fn (effective-request-fn opts)
         base (or (:base opts) minus40-base)
         lu-query (some->> (seq (:lu-terms opts)) (str/join " OR "))
         kw-query (keywords text)]
     (try
       (if (and (str/blank? (or lu-query "")) (str/blank? kw-query))
         {:status "none" :at (str (Instant/now)) :query-ms (elapsed) :query ""
          :query-source "none" :items []}
         (let [;; Always run both queries; merge alternates the slots.
               lu-items (when-not (str/blank? (or lu-query ""))
                          (mapv #(assoc % :source "lu")
                                (search-memories request-fn base lu-query)))
               kw-items (when-not (str/blank? kw-query)
                          (mapv #(assoc % :source "keywords")
                                (search-memories request-fn base kw-query)))
               items (merge-items lu-items kw-items)]
           (if (seq items)
             {:at (str (Instant/now)) :query-ms (elapsed)
              :query (str/join " || " (remove str/blank? [lu-query kw-query]))
              :query-source (let [sources (set (map :source items))]
                              (cond (= sources #{"lu"}) "lu"
                                    (= sources #{"keywords"}) "keywords"
                                    :else "mixed"))
              :items items}
             {:status "none" :at (str (Instant/now)) :query-ms (elapsed)
              :query (str/join " || " (remove str/blank? [lu-query kw-query]))
              :query-source (cond (and lu-query (not (str/blank? kw-query))) "mixed"
                                  lu-query "lu"
                                  :else "keywords")
              :items []})))
       (catch Throwable t
         {:status "failed" :reason (str "recall-error: " (.getMessage t))
          :at (str (Instant/now)) :query-ms (elapsed)
          :query (str (or lu-query kw-query)) :items []})))))

;; ------------------------------------------------- arrival-time compute

(defonce ^:private ^ExecutorService executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "dog-recall") (.setDaemon true))))))

(defn- publish!
  "Cache RECALL for RECORD-ID. A typed absence is cached too, so the
   provider can record it; only a non-map is dropped."
  [record-id recall]
  (when (and (turn-store/record-id? (str record-id)) (map? recall))
    (swap! !recalls assoc (str record-id) recall))
  recall)

(defn prepare-async!
  "Kick the background recall for RECORD-ID's :source_text when it is
   not already cached or in flight. Returns immediately."
  [opts record-id]
  (let [record-id (str record-id)]
    (when (and (turn-store/record-id? record-id)
               (not (off? opts))
               (not (contains? @!recalls record-id))
               (not (contains? @!in-flight record-id)))
      (swap! !in-flight conj record-id)
      (.submit executor
               ^Runnable
               ;; bound-fn conveys the caller's dynamic bindings
               ;; (*request-fn*, prompt-line/*turn-record-store*) onto the
               ;; executor thread, as prompt-line's submit-provider does.
               (bound-fn []
                 (try
                   (let [store (let [s (:store opts)] (if (fn? s) (s) s))
                         record (when store (turn-store/read-record store record-id))]
                     (if (and (map? record)
                              (not (str/blank? (str (:source_text record)))))
                       (publish! record-id
                                 (recall (:source_text record)
                                         (assoc opts :lu-terms
                                                (try (lu-terms store record-id)
                                                     (catch Throwable _ nil)))))
                       ;; No record or no operator text: a typed absence, so
                       ;; the render records it instead of re-kicking forever.
                       (publish! record-id
                                 {:status "none" :reason "no-source-text"
                                  :at (str (Instant/now)) :query-ms 0 :items []})))
                   (catch Throwable t
                     (publish! record-id
                               {:status "failed" :reason (str "prepare-error: " (.getMessage t))
                                :at (str (Instant/now)) :query-ms 0 :items []}))
                   (finally
                     (swap! !in-flight disj record-id)))))))
  nil)

(defn handle-event
  "One turn-service event: on :recorded, prepare the recall for the new
   record's source text on this namespace's own daemon thread. Skips
   non-operator records and everything when the off-switch exists.
   Never throws."
  [opts {:keys [event id]}]
  (when (and (= :recorded event) id (not (off? opts)))
    (try
      (let [store (let [s (:store opts)] (if (fn? s) (s) s))]
        (when-let [record (and store
                               (turn-store/record-id? (str id))
                               (turn-store/read-record store (str id)))]
          (when (and (= "operator" (or (:origin record) "operator"))
                     (not (str/blank? (str (:source_text record)))))
            (prepare-async! opts (str id)))))
      (catch Throwable _ nil))))

(defn start!
  "Register the 狗 recall listener on turn-service events. OPTS as per
   defaults plus :store (turn-store, or a 0-arg fn resolved at event
   time). Re-registering replaces the listener."
  [opts]
  (turn-service/add-turn-listener!
   ::dog-recall
   (fn [event]
     (.submit executor ^Runnable #((resolve `handle-event) opts event))))
  nil)

(defn- live-service []
  (try (some-> (requiring-resolve 'futon3c.transport.http/xiang-bell-service) deref (apply []))
       (catch Throwable _ nil)))

(defn start-live!
  "Wire the listener to the running service."
  []
  (start! {:store (fn [] (some-> (live-service) :config :store))}))

;; ---------------------------------------------------------------- provider

(defn prompt-fact
  "The Prompt-line fact for one 狗 :memory segment: `memory <header>` on
   a hit, `memory: not ready` while the background recall is pending,
   and NIL — the segment is omitted from the line — when the turn has
   no memory items (status \"none\"/\"failed\", or a payload-less
   segment). Omitting is the deliberate choice (claude-13's 11-11 merge
   requisition offered omit or `memory: none`): the typed absence still
   lands on the 象 turn record as :dog_recall, so the line stays clean
   and nothing is lost. When items exist but the header is missing —
   the shape that printed a bare `memory` on Joe's E-futon-questions
   turn (turn-RgkbTi, 2026-11-11: the live-loaded draft segment carried
   items yet the line ended in a bare label) — the label is rebuilt
   from the items rather than printed bare."
  [{:segment/keys [header dog-recall]}]
  (let [items (:items dog-recall)]
    (cond
      (not (str/blank? header)) (str "memory " header)
      (seq items) (str "memory "
                       (str/join ", " (map #(str "~" (or (:display %) (:memory %))) items)))
      (= "not-ready" (:status dog-recall)) "memory: not ready"
      :else nil)))

(defn- dog-segment
  "The :memory segment for one cached recall. Items (up to MAX-ITEMS)
   render as `memory ~<project>/<slug> (<why>)` via :segment/header; the
   whole recall rides :segment/dog-recall so prompt-line can record it
   as :dog_recall on the 象 turn record. A typed absence carries no
   header: \"not-ready\" renders as `memory: not ready`, \"none\" and
   \"failed\" stay off the line."
  [{:keys [agent-id session-id]} record-id recall]
  (let [items (:items recall)
        status (:status recall)
        header (when (seq items)
                 (let [shown (mapv #(str "~" (or (:display %) (:memory %)) " (" (:why %) ")") items)]
                   (str (first shown)
                        (when (< 1 (count shown))
                          (str "; also " (str/join ", " (rest shown)))))))]
    (cond-> {:segment/id :memory
             :segment/provider "futon3c.agency.dog-provider/provider"
             :segment/observed-at (str (or (:at recall) (Instant/now)))
             :segment/basis {:scope {:agent-id (str agent-id)
                                     :session-id (str session-id)
                                     :turn-record-id (str record-id)
                                     :basis-status (if (seq items) :dog-recall :dog-absence)}}
             :segment/dog-recall (select-keys recall [:at :items :query-ms :query-source :status :reason])}
      (seq header) (assoc :segment/header header)
      (= "not-ready" status) (assoc :segment/not-ready true))))

(defn provider
  "狗 pre-hook prompt-line provider. Reads only the recall cache (the
   recall itself was computed at the record's arrival; if it was not —
   no listener, a missed event — this kicks the background compute now
   and reports not-ready). Analysis seats and the off-switch get nil."
  [{:keys [agent-id turn-record-id] :as ctx}]
  (when-not (prompt-line/analysis-seat? agent-id)
    (when (and (not (off? nil))
               (turn-store/record-id? (some-> turn-record-id str)))
      (let [record-id (str turn-record-id)]
        (if-let [recall (cached-recall record-id)]
          (dog-segment ctx record-id recall)
          (do (prepare-async!
               {:store (fn [] (or prompt-line/*turn-record-store* (turn-store/store)))}
               record-id)
              (dog-segment ctx record-id
                           {:status "not-ready" :at (str (Instant/now))
                            :query-ms 0 :items []})))))))

(defn register! []
  (prompt-line/register-provider!
   {:segment/id :memory
    :provider "futon3c.agency.dog-provider/provider"
    :fn provider
    ;; Cache-only: never blocks on m40, so the default budget is plenty.
    :budget-ms 25}))

(register!)
