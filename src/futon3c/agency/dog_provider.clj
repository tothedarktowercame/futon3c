(ns futon3c.agency.dog-provider
  "狗 pre-hook (M-repl-circus, \"狗: recall before the agent's turn, use
   checked after it\"): recall relevant minus40 memories for an agent turn
   that answers an operator turn, before the turn runs.

   Relevance is the operator turn's own text (the fast path; 11-11: \"it
   could either read the operator text patterns or wait for the 驴
   patterns\"). The read is the same one m40_find makes:
   GET <minus40-store :7093>/api/alpha/evidence/text-search?q=... (the
   store's FTS5 sidecar; hits whose body is a claude-code memory are
   grouped by project/name, one item per memory).

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

(defn keywords
  "The fast relevance query from the operator turn's text: the store's
   FTS5 sidecar treats a space-separated query as AND, so a whole turn
   matches only its own verbatim echoes (lu situations). Extract up to
   KEYWORD-COUNT distinctive content words (longest first, stopwords
   dropped, FTS5-significant characters stripped) and OR them."
  [text]
  (let [tokens (-> (str text)
                   (str/replace #"[^0-9A-Za-z\-]+" " ")
                   (str/split #"\s+"))
        tokens (->> tokens
                    (filter #(and (> (count %) 3)
                                  (not (contains? stopwords (str/lower-case %)))))
                    distinct
                    (sort-by count >)
                    (take keyword-count))]
    (str/join " OR " tokens)))

(defn- memory-item
  "One text-search hit whose body is a claude-code memory, as
   {:memory \"<project>/<slug>\" :score :why}. The why is the memory's
   hook trimmed to 8 words. Nil for non-memory hits (turn records and
   lu situations share the index)."
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
            why (let [words (->> (str/split (str/trim (str (or (:hook body) ""))) #"\s+")
                                 (remove str/blank?)
                                 (take 8))]
                  (if (seq words) (str/join " " words) "keyword match"))]
        (when-not (or (str/blank? project) (str/blank? slug))
          {:memory (str project "/" slug)
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

(defn recall
  "One 狗 recall for the operator turn TEXT. Returns
   {:at :items [{:memory :score :why}...] :query-ms :query}, with :status
   \"none\" when no memory matched and \"failed\" on any error. Never
   throws. OPTS as per defaults; :request-fn receives the query URL and
   returns the parsed JSON response."
  ([text] (recall text {}))
  ([text opts]
   (let [opts (merge defaults opts)
         t0 (System/nanoTime)
         elapsed #(quot (- (System/nanoTime) t0) 1000000)
         request-fn (effective-request-fn opts)
         query (keywords text)]
     (try
       (if (str/blank? query)
         {:status "none" :at (str (Instant/now)) :query-ms (elapsed) :query "" :items []}
         (let [url (str (or (:base opts) minus40-base)
                        "/api/alpha/evidence/text-search?q="
                        (URLEncoder/encode query "UTF-8")
                        "&limit=" search-limit)
               response (request-fn url)
               items (recall-from-response (or response {}))]
           (if (seq items)
             {:at (str (Instant/now)) :query-ms (elapsed) :query query :items items}
             {:status "none" :at (str (Instant/now)) :query-ms (elapsed) :query query
              :items []})))
       (catch Throwable t
         {:status "failed" :reason (str "recall-error: " (.getMessage t))
          :at (str (Instant/now)) :query-ms (elapsed) :query query :items []})))))

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
                       (publish! record-id (recall (:source_text record) opts))
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
                 (let [shown (mapv #(str "~" (:memory %) " (" (:why %) ")") items)]
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
             :segment/dog-recall (select-keys recall [:at :items :query-ms :status :reason])}
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
