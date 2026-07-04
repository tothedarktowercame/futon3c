(ns futon3c.evidence.xtdb-backend
  "XTDB-backed evidence persistence via futon1a.

   Wraps an XTDB node (from futon1a's XtdbStore) to provide durable
   storage for the evidence landscape. Evidence entries are stored as
   XTDB documents with :xt/id = :evidence/id.

   Constructor: (make-xtdb-backend xtdb-store-or-node)
   Accepts either a futon1a XtdbStore record or a raw XTDB node."
  (:require [xtdb.api :as xtdb]
            [futon3c.evidence.backend :as backend])
  (:import [java.time Duration Instant]
           [java.util Date]))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-store
           :error/code code
           :error/message message
           :error/at (str (java.time.Instant/now))}
    (seq context) (assoc :error/context context)))

(defn- strip-xt-id
  "Remove :xt/id from an XTDB document to return a clean EvidenceEntry."
  [doc]
  (when doc
    (dissoc doc :xt/id)))

(defn- db
  "Get a consistent db snapshot from the node."
  [node]
  (xtdb/db node))

(defn- entity
  "Read an entity by id, stripping :xt/id."
  [node id]
  (strip-xt-id (xtdb/entity (db node) id)))

(defn- entity-exists?
  "Check if an entity exists by id."
  [node id]
  (some? (xtdb/entity (db node) id)))

(defn- parse-valid-time
  "Parse :evidence/at to a java.util.Date for XTDB valid-time.
   Returns nil if unparseable (XTDB will use transaction time as fallback)."
  [doc]
  (when-let [at (:evidence/at doc)]
    (try
      (Date/from (Instant/parse at))
      (catch Exception _ nil))))

(def ^:private ^Duration put-timeout
  "Maximum time to wait for XTDB to index a put transaction.
   Defense-in-depth: prevents HTTP handler threads from blocking indefinitely
   when XTDB indexing is stalled or interrupted."
  (Duration/ofSeconds 10))

(defn- put-and-sync!
  "Submit a put transaction and wait for it to be indexed (with timeout).
   Sets XTDB valid-time to :evidence/at so entries appear at their true
   chronological position, not when they were physically written (important
   for replicated entries that arrive after a delay)."
  [node doc]
  (let [valid-time (parse-valid-time doc)
        tx-op (if valid-time
                [:xtdb.api/put doc valid-time]
                [:xtdb.api/put doc])
        tx (xtdb/submit-tx node [tx-op])]
    (xtdb/await-tx node tx put-timeout)))

(defn- delete-and-sync!
  "Submit delete transactions for a set of ids and wait for indexing."
  [node ids]
  (let [tx-ops (mapv (fn [id] [:xtdb.api/delete id]) ids)
        tx (xtdb/submit-tx node tx-ops)]
    (xtdb/await-tx node tx put-timeout)))

(def ^:private query-timeout-ms
  "Hard ceiling for evidence XTDB queries, in ms.
   Defense-in-depth around the remaining unbounded paths (-all and broad
   id scans): callers degrade rather than wedging handler threads if XTDB
   stalls under load."
  (long (or (some-> (System/getProperty "futon3c.evidence.query-timeout-ms")
                    Long/parseLong)
            15000)))

(defn- query-all-entries
  "Datalog query returning all evidence entries.
   Used by -all only; -query uses query-entries so filters/order/limit are
   pushed into XTDB before entity pulls."
  [node]
  (->> (xtdb/q (db node)
               {:find '[(pull e [*])]
                :where '[[e :evidence/id _]]
                :timeout query-timeout-ms})
       (map first)
       (map strip-xt-id)))

(defn- add-eq-filter
  [query-state attr value]
  (let [v (symbol (str "v" (count (:args query-state))))]
    (-> query-state
        (update :where conj ['e attr v])
        (update :in conj v)
        (update :args conj value))))

(defn- add-tag-filter
  [query-state tag]
  (let [v (symbol (str "v" (count (:args query-state))))]
    (-> query-state
        (update :where conj ['e :evidence/tags v])
        (update :in conj v)
        (update :args conj tag))))

(defn- query-state
  "Build the bounded XTDB query for -query.

   Pushed into datalog: subject, type, claim-type, author, tag membership,
   order, positive limit, and a conservative
   `:evidence/at` lower bound (1s below `since`) so a `:query/since` no longer
   full-scans the whole store. Precise `since` filtering — and the malformed-
   `since` all-entries fallback — stays at the application level
   (`backend/filter-and-sort-entries`); the datalog bound can only OVER-include
   near the boundary, never under-return. HTTP-only filters (session-id and pattern-id) are also
   kept outside this backend; the HTTP handler already withholds :query/limit
   from the backend and applies its limit after those filters, preventing the
   classic push-limit-before-app-filter under-return."
  [{:query/keys [subject type claim-type author tags limit since before]}]
  (let [base {;; NB: lead with :evidence/at (not :evidence/id) — the latter forces
              ;; a full ~64k scan that defeats the `since` range-seek below.
              ;; :evidence/at is present on every entry, so e is still fully bound.
              ;; Ephemeral exclusion is NOT pushed here either: a `(not ...)`
              ;; negation also forces a full scan (measured ~20s vs ~0.1s without
              ;; it), and backend/filter-and-sort-entries already removes
              ;; ephemeral entries application-side.
              :where '[[e :evidence/at t]]
              :in []
              :args []}
        base (cond-> base
               subject (add-eq-filter :evidence/subject subject)
               type (add-eq-filter :evidence/type type)
               claim-type (add-eq-filter :evidence/claim-type claim-type)
               author (add-eq-filter :evidence/author author))
        base (reduce add-tag-filter base (seq tags))
        ;; Conservative :evidence/at lower bound (1s below `since`) pushed into
        ;; datalog to prune the scan — fixes the full-store-scan timeout (the
        ;; 60k+-entry degrade-to-empty). Only for a parseable `since`; a
        ;; malformed `since` keeps the app-level all-entries fallback. The bound
        ;; is 1s low so it can only over-include near the boundary (the precise
        ;; app filter removes those), never drop a wanted entry.
        since-lower (when since
                      (try (.toString (.minusSeconds ^Instant (backend/parse-instant since) 1))
                           (catch Exception _ nil)))
        base (if since-lower
               (-> base
                   (update :where conj '[(<= since-lb t)])
                   (update :in conj 'since-lb)
                   (update :args conj since-lower))
               base)
        before-upper (when before
                       (try (.toString ^Instant (backend/parse-instant before))
                            (catch Exception _ nil)))
        base (if before-upper
               (-> base
                   (update :where conj '[(< t before-ub)])
                   (update :in conj 'before-ub)
                   (update :args conj before-upper))
               base)
        q (cond-> {:find '[e t]
                   :where (:where base)
                   :order-by '[[t :desc]]
                   :timeout query-timeout-ms}
            (seq (:in base)) (assoc :in (:in base))
            (and (int? limit) (pos? limit)) (assoc :limit limit))]
    {:query q :args (:args base)}))

(defn- query-entries
  "Return query entries newest-first while pulling only surviving ids."
  [node params]
  (let [{:keys [query args]} (query-state params)
        ids (map first (apply xtdb/q (db node) query args))]
    (keep #(entity node %) ids)))

(def ^:private recency-window-hours
  "Widening lookback ladder (hours) for the limit-without-since fast path:
   1h, 6h, 1d, 4d, 16d, 64d, ~2y. Each rung is an indexed :evidence/at
   range-seek (~10ms on the 85k live store); the first rung that yields a
   full page wins. If no rung fills, return the broadest-window result instead
   of falling back to the unbounded scan (which XTDB's :order-by fully realizes
   — 15-20s live)."
  [1 6 24 96 384 1536 17520])

(defn- query-recent
  "Fast path for a limit query with NO :query/since: probe widening
   recency windows instead of realizing the whole store.

   Correctness: a window is exact when it yields a FULL page after
   `backend/filter-and-sort-entries` (ephemeral exclusion and all app-level
   filters applied). Entries outside the window are strictly older than
   everything in it, so a full page from a window IS the global newest-`limit`
   result. An under-full window widens; if the ladder never fills but the
   broadest window found any entries, return those rather than falling into the
   pathological unbounded scan. A completely empty broadest window returns nil
   so ancient fixtures and explicit archival stores keep the old exact path.

   The probe intentionally does NOT push :query/limit into datalog:
   the window bounds the row count, and a datalog limit applied before
   the app-side ephemeral filter could under-fill a window that
   actually has a full page (forcing a needless widen).

   The returned vector carries {:evidence/fast-path :recency-window
   :evidence/window-hours h} metadata — response-invisible, used by the
   witness test."
  [node params]
  (let [limit (:query/limit params)
        now (Instant/now)]
    (loop [[hours & more] recency-window-hours
           broadest []]
      (if hours
        (let [since (.toString (.minus now (Duration/ofHours hours)))
              probe (-> params
                        (dissoc :query/limit)
                        (assoc :query/since since))
              rows (query-entries node probe)
              entries (backend/filter-and-sort-entries
                       rows (assoc params :query/since since))
              result (with-meta entries {:evidence/fast-path :recency-window
                                         :evidence/window-hours hours})]
          (if (= limit (count entries))
            result
            (recur more result)))
        (when (seq broadest) broadest)))))

(defonce ^:private count-cache
  ;; Broad counts (params with no selective filter) are O(store) and blow the
  ;; query-timeout under load — and they RECUR (context-retrieval / self-watch fire
  ;; them every agent turn/attach, per futon1a#5). Cache by params with a short TTL
  ;; so at most one scan per TTL happens instead of one per call, and cache the
  ;; degraded -1 too so a slow store degrades ONCE per TTL rather than hanging every
  ;; attach for 15s. (claude-10, 2026-07-04.)
  (atom {}))

(def ^:private count-cache-ttl-ms 30000)

(defn- count-entries*
  "Uncached count of matching evidence entries (original logic)."
  [node params]
  (let [{:keys [query args]} (query-state (dissoc params :query/limit))
        count-query (cond-> (-> query
                                (assoc :find '[(count e)])
                                (dissoc :order-by :limit))
                      (not (true? (:query/include-ephemeral? params)))
                      (update :where conj '(not [e :evidence/ephemeral? true])))
        result (ffirst (apply xtdb/q (db node) count-query args))]
    (long (or result 0))))

(defn- count-entries
  "Count matching evidence entries without pulling documents.
   TTL-cached (count-cache) so repeated broad counts don't each re-scan the store;
   a query timeout degrades to -1 and is cached, so a slow store hangs once per TTL
   instead of once per call/attach. Never throws — always returns a long."
  [node params]
  (let [now (System/currentTimeMillis)
        cached (get @count-cache params)]
    (if (and cached (< (- now (long (:at cached))) count-cache-ttl-ms))
      (:v cached)
      (let [v (try
                (count-entries* node params)
                (catch java.util.concurrent.TimeoutException _
                  (binding [*out* *err*]
                    (println (str "[evidence] WARN XTDB evidence count timed out after "
                                  query-timeout-ms "ms — degraded to -1, cached "
                                  count-cache-ttl-ms "ms. params=" (pr-str params))))
                  -1)
                (catch Throwable t
                  (binding [*out* *err*]
                    (println (str "[evidence] WARN XTDB evidence count failed: "
                                  (.getMessage t) " — degraded to -1. params=" (pr-str params))))
                  -1))]
        (swap! count-cache assoc params {:v v :at now})
        v))))

(defrecord XtdbBackend [node]
  backend/EvidenceBackend

  (-append [_ validated]
    (let [eid (:evidence/id validated)]
      (cond
        (entity-exists? node eid)
        (social-error :duplicate-id "Evidence id already exists" :evidence-id eid)

        (and (:evidence/in-reply-to validated)
             (not (entity-exists? node (:evidence/in-reply-to validated))))
        (social-error :reply-not-found
                      "in-reply-to references missing entry"
                      :in-reply-to (:evidence/in-reply-to validated)
                      :evidence-id eid)

        (and (:evidence/fork-of validated)
             (not (entity-exists? node (:evidence/fork-of validated))))
        (social-error :fork-not-found
                      "fork-of references missing entry"
                      :fork-of (:evidence/fork-of validated)
                      :evidence-id eid)

        :else
        (do
          (put-and-sync! node (assoc validated :xt/id eid))
          {:ok true :entry validated}))))

  (-get [_ id]
    (entity node id))

  (-exists? [_ id]
    (entity-exists? node id))

  (-query [_ params]
    (try
      (or (when (and (int? (:query/limit params))
                     (pos? (:query/limit params))
                     (nil? (:query/since params)))
            (query-recent node params))
          (let [entries (query-entries node params)]
            ;; Final pass preserves shared AtomBackend semantics for app-level
            ;; filters such as :query/since and malformed timestamp fallback.
            (backend/filter-and-sort-entries entries params)))
      (catch java.util.concurrent.TimeoutException _
        (binding [*out* *err*]
          (println (str "[evidence] WARN XTDB evidence query timed out after "
                        query-timeout-ms "ms — degraded to empty result. "
                        "params=" (pr-str params))))
        [])))

  (-count [_ params]
    (try
      (count-entries node params)
      (catch java.util.concurrent.TimeoutException _
        (binding [*out* *err*]
          (println (str "[evidence] WARN XTDB evidence count timed out after "
                        query-timeout-ms "ms — degraded to -1. "
                        "params=" (pr-str params))))
        -1)))

  (-forks-of [_ evidence-id]
    (let [results (->> (xtdb/q (db node)
                               '{:find [(pull e [*])]
                                 :where [[e :evidence/fork-of eid]]
                                 :in [eid]}
                               evidence-id)
                       (map first)
                       (map strip-xt-id))]
      (->> results (sort-by backend/entry-at) vec)))

  (-delete! [_ ids]
    (let [id-set (set ids)]
      (when (seq id-set)
        (delete-and-sync! node id-set))
      {:compacted (count id-set)}))

  (-all [_]
    (query-all-entries node)))

(defn make-xtdb-backend
  "Create an XtdbBackend from either a futon1a XtdbStore or a raw XTDB node.
   XtdbStore has a :node field; raw nodes are used directly."
  [store-or-node]
  (let [node (if-let [n (:node store-or-node)]
               n
               store-or-node)]
    (->XtdbBackend node)))
