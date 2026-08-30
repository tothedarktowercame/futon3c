(ns futon3c.evidence.futon1b-backend
  "EvidenceBackend over HTTP/EDN against the futon1b server (XTDB 2 store
   JVM) — the B1 slice of E-futon1b-operational-switchover.

   Why not http_backend: that one speaks JSON to a futon3c Agency API
   (keyword/namespace fidelity lost in translation). futon1b is EDN
   end-to-end, so entries round-trip byte-faithfully.

   Correctness stance: every -query/-count re-applies
   backend/filter-and-sort-entries LOCALLY with the full params — protocol
   semantics are the shared implementation's. Pushdown scope depends on the
   query: when every membership-deciding filter is one the server applies
   IDENTICALLY (type/claim-type/author/session-id/since/before/fork-of +
   the explicit ephemeral flag), `limit` passes through too and the server
   windows (the unlimited author=joe fetch hydrated 8,882 docs/10MB for a
   5-item recall and, cold, silently timed out zai-1's first live demo,
   2026-07-11). When client-only filters are present (tags/subject/
   pattern-id), Futon1b's keyset-window implementation applies the same
   predicates before completing the requested window. The client still
   re-applies the shared filter as a parity check, but no longer disables the
   limit and accidentally requests the entire corpus.

   -append preserves the EvidenceBackend semantics: duplicate-id /
   reply-not-found / fork-not-found come back as SocialError maps (the
   server's 409 maps to :duplicate-id; reply/fork existence is checked
   client-side — the append-only server doesn't enforce those).
   -delete! is a logged no-op: futon1b evidence is append-only and no
   live path compacts (verified 2026-07-10).

   Reads throw on transport errors (R4 loud failure); -append returns a
   SocialError so the invoke path can surface it as data."
  (:require [futon3c.evidence.backend :as backend]
            [futon3c.evidence.subject :as subject]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def default-url
  ;; 127.0.0.1, NOT localhost: the futon1b JVM runs preferIPv4Stack (XTDB
  ;; pgwire needs it), so its HttpServer binds IPv4 only — a client JVM
  ;; resolving localhost to ::1 gets "unreachable" (bit us at Gate 1).
  "http://127.0.0.1:7074")

(defn- penholder []
  (or (System/getenv "FUTON1B_PENHOLDER")
      (System/getenv "FUTON1A_PENHOLDER")
      "api"))

(def ^:private timeout-ms
  ;; 120s, not 30s: a server-decidable limit=1000 scan takes ~19s on lucy
  ;; (4G box, both JVMs, swap-pressured) and crossed 30s whenever the
  ;; ingest daemons overlapped — every read surfaced as "futon1b
  ;; unreachable" (2026-07-13).
  (or (some-> (System/getenv "FUTON1B_TIMEOUT_MS") parse-long)
      120000))

(def ^:private append-timeout-ms
  ;; Append is retried by stable id. It must return control to the compatibility
  ;; route well before the general 120s analytical-read budget so abandoned
  ;; callers cannot accumulate indefinitely under store contention.
  (or (some-> (System/getenv "FUTON1B_APPEND_TIMEOUT_MS") parse-long)
      30000))

(def append-retry-max-backoff-ms
  ;; Ceiling on ONE backoff sleep. Uncapped 100*2^n doubling leaves a ~39s
  ;; stretch at the end of a 90s window where the store is never probed, so a
  ;; restart completing inside that gap still loses the write (measured: a
  ;; Dionysus restart took 84s, inside the gap).
  (or (some-> (System/getenv "FUTON1B_APPEND_RETRY_MAX_BACKOFF_MS") parse-long)
      5000))

(def append-retry-ms
  ;; Connection refusal proves that the store received nothing, so retry that
  ;; transport failure through a bounded restart window. A request timeout is
  ;; deliberately excluded below because the write may have landed.
  ;;
  ;; 300s, not 90s: the window is only useful if it outlasts a real restart,
  ;; and measured stop->healthy times are 42s and 61s on Zone but 84s, 85s and
  ;; 93s on the laptop. A 90s default lost the write on the 93s run -- the
  ;; retry behaved correctly and the evidence was gone anyway. Size this above
  ;; the SLOWEST restart you expect, not the typical one.
  ;;
  ;; The cost is that a caller blocks for up to this long during a genuine
  ;; outage, which is the argument for the durable-spool follow-on: the spool
  ;; is what lets the window be short again.
  (or (some-> (System/getenv "FUTON1B_APPEND_RETRY_MS") parse-long)
      300000))

(def query-cache-ttl-ms
  ;; The War Machine and AIF stack are read-mostly projections. Their HTTP
  ;; handlers can ask the same bounded question several times per scheduler
  ;; tick, so a short cache prevents each surface from independently occupying
  ;; a futon1b expensive-read permit. Successful writes through this backend
  ;; invalidate immediately; external writers are visible after this bounded
  ;; interval.
  (or (some-> (System/getenv "FUTON1B_QUERY_CACHE_TTL_MS") parse-long)
      60000))

(defonce ^:private !query-cache
  (atom {:generation 0 :entries {}}))

(defonce ^:private !query-locks (atom {}))

(defn- invalidate-query-cache! []
  (swap! !query-cache
         (fn [{:keys [generation]}]
           {:generation (inc (long (or generation 0)))
            :entries {}})))

(defn- api-url [base-url path]
  (str (str/replace base-url #"/$" "") path))

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))

(defn- read-edn
  "Tolerant EDN read: unknown tagged literals pass through as their value
   (backfilled docs can carry tags this client doesn't register readers for)."
  [s]
  (let [s (if (string? s) s (some-> s slurp))]
    (when (seq (str s))
      (let [value (edn/read-string {:default (fn [_tag v] v)} s)]
        ;; Successful append responses contain singular :entry. Invalidate
        ;; here, rather than only in the defrecord method, so a Drawbridge
        ;; reload also updates already-constructed serving backend instances.
        (when (and (map? value) (contains? value :entry))
          (invalidate-query-cache!))
        value))))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-store
           :error/code code
           :error/message message
           :error/at (str (Instant/now))}
    (seq context) (assoc :error/context context)))

(defn- append-trace-id
  "Stable, non-secret correlation id shared by the producer and Futon1b logs."
  [evidence-id]
  (str "evidence-append:" evidence-id))

(defn- edn-readable?
  [value]
  (try
    (edn/read-string (pr-str value))
    true
    (catch Exception _
      false)))

(defn- path-segment
  "Keep diagnostic paths structural. Map keys that cannot themselves be
   represented as EDN are replaced with their entry index."
  [k idx]
  (if (and (or (keyword? k) (string? k) (integer? k))
           (edn-readable? k))
    k
    [:map-entry idx]))

(defn- invalid-edn-leaves
  "Locate the deepest values that make a payload unreadable by
   clojure.edn/read-string. Values are not copied into diagnostics; only a
   structural path, JVM type, and malformed keyword/symbol token are retained."
  ([value] (invalid-edn-leaves [] value))
  ([path value]
   (let [children
         (cond
           (map? value)
           (mapcat (fn [[idx [k v]]]
                     (let [segment (path-segment k idx)]
                       (concat
                        (invalid-edn-leaves (conj path segment :map-key) k)
                        (invalid-edn-leaves (conj path segment) v))))
                   (map-indexed vector value))

           (or (vector? value) (list? value) (seq? value))
           (mapcat (fn [[idx v]]
                     (invalid-edn-leaves (conj path idx) v))
                   (map-indexed vector value))

           (set? value)
           (mapcat (fn [[idx v]]
                     (invalid-edn-leaves (conj path [:set-member idx]) v))
                   (map-indexed vector value))

           :else
           [])]
     (if (seq children)
       children
       (when-not (edn-readable? value)
         [{:path path
           :value-type (.getName (class value))
           :token (when (or (keyword? value) (symbol? value))
                    (pr-str value))}])))))

(defn- serialize-append
  "Serialize exactly once and prove that Futon1b's EDN reader can consume the
   resulting wire representation before opening a connection."
  [validated]
  (try
    (let [body (pr-str validated)]
      (edn/read-string body)
      {:body body})
    (catch Exception e
      {:error e
       :invalid-edn (vec (invalid-edn-leaves validated))})))

(defn- timeout-error?
  [error]
  (loop [t error]
    (when t
      (let [throwable? (instance? Throwable t)
            description (str (.getName (class t)) " "
                             (if throwable? (.getMessage ^Throwable t) t))]
        (or (re-find #"(?i)timeout|timed out" description)
            (when throwable? (recur (.getCause ^Throwable t))))))))

(defn- get-edn
  "GET url, EDN-parse the body. Returns {:status n :body v}.
   Throws on transport-level failure (connection refused etc.)."
  ([url] (get-edn url timeout-ms))
  ([url request-timeout-ms]
   ;; http-kit's :timeout can expire after headers while its response promise
   ;; remains blocked on a stalled body.  Bound the promise dereference itself
   ;; so evidence reads cannot monopolize a regulator tick indefinitely.
   (let [pending (future @(http/get url {:timeout request-timeout-ms :as :text}))
         timed-out (Object.)
         response (deref pending request-timeout-ms timed-out)]
     (when (identical? timed-out response)
       (future-cancel pending)
       (throw (ex-info "futon1b read timed out"
                       {:url url :timeout-ms request-timeout-ms
                        :error/component :transport
                        :error/code :futon1b-read-timeout})))
     (let [{:keys [status body error]} response]
       (when error
         (throw (ex-info "futon1b unreachable" {:url url} error)))
       {:status status :body (read-edn body)}))))

(defn- query-string
  "Pushdown params. See ns docstring for the two regimes."
  [{:query/keys [type claim-type author session-id since before fork-of
                 tags subject pattern-id limit include-ephemeral?
                 cursor-at cursor-id]}]
  (let [pairs (cond-> [["include-ephemeral" (str (boolean include-ephemeral?))]]
                (and (int? limit) (pos? limit))
                (conj ["limit" (str limit)])
                type (conj ["type" (name type)])
                claim-type (conj ["claim-type" (name claim-type)])
                author (conj ["author" (str author)])
                session-id (conj ["session-id" (str session-id)])
                since (conj ["since" (str since)])
                before (conj ["before" (str before)])
                fork-of (conj ["fork-of" (str fork-of)])
                (and cursor-at cursor-id)
                (conj ["cursor-at" (str cursor-at)]
                      ["cursor-id" (str cursor-id)])
                (seq tags) (conj ["tags" (str/join "," (map name tags))])
                subject (conj ["subject-type" (name (:ref/type subject))]
                              ["subject-id" (str (:ref/id subject))])
                pattern-id (conj ["pattern-id" (name pattern-id)]))]
    (str/join "&" (map (fn [[k v]] (str k "=" (enc v))) pairs))))

(def ^:private server-page-size 1000)
(def ^:private admission-retries 7)
(def evidence-request-budget 20)

(defn partial-result?
  "True when an evidence cursor walk exhausted its client request budget."
  [entries]
  (true? (:partial? (meta entries))))

(defn- fetch-page
  [base-url params]
  (let [url (str (api-url base-url "/api/alpha/evidence") "?"
                 (query-string params))]
    (loop [attempt 0]
      (let [{:keys [status body]} (get-edn url)]
        (cond
          (= 200 status) body

          (and (= 503 status)
               (= :expensive-read-busy (:error body))
               (< attempt admission-retries))
          (do (Thread/sleep (* 100 (bit-shift-left 1 attempt)))
              (recur (inc attempt)))

          :else
          (throw (ex-info "futon1b evidence query failed"
                          {:status status :body body :url url})))))))

(defn- fetch-entries-uncached
  "Fetch exact query semantics through futon1b's bounded cursor protocol.
  A caller limit may span multiple server pages; protocol operations such as
  -all page until exhaustion without asking the store JVM for an unbounded
  response."
  [base-url params]
  (let [requested (get params :query/limit)
        target (when (and (int? requested) (pos? requested)) requested)
        budget (long (or (:query/request-budget params)
                         evidence-request-budget))]
    (loop [cursor nil
           requests 0
           entries []]
      (if (>= requests budget)
        (with-meta (vec entries)
          {:partial? true
           :next-cursor cursor
           :requests requests
           :request-budget budget})
        (let [remaining (when target (- target (count entries)))
              page-limit (long (min server-page-size (or remaining server-page-size)))
              page-params (cond-> (assoc params :query/limit page-limit)
                            cursor (assoc :query/cursor-at (:at cursor)
                                          :query/cursor-id (:id cursor)))
              body (fetch-page base-url page-params)
              entries' (into entries (:entries body))
              next-cursor (:next-cursor body)
              ;; The cursor is the server's authoritative continuation signal.
              ;; In particular, :incomplete may accompany a short/empty page
              ;; when post-filtering consumed the 20k-row scan allowance.
              continue? (boolean next-cursor)
              result (if target (vec (take target entries')) (vec entries'))]
          (cond
            (and target (>= (count entries') target))
            (cond-> result
              next-cursor
              (with-meta {:partial? true
                          :next-cursor next-cursor
                          :requests (inc requests)
                          :request-budget budget}))

            continue?
            (recur next-cursor (inc requests) entries')

            (:incomplete body)
            (with-meta result
              {:partial? true
               :reason :incomplete-page-without-cursor
               :requests (inc requests)
               :request-budget budget})

            :else result))))))

(defn- fresh-cache-entry
  [cache-state key now-ms]
  (let [entry (get-in cache-state [:entries key])]
    (when (< now-ms (long (or (:expires-at-ms entry) 0)))
      entry)))

(defn- fetch-entries
  "Cache one exact bounded evidence query for a short interval.

  A per-query lock supplies single-flight behaviour for concurrent copies of the same
  surface request. A generation check prevents a read begun before a successful
  append from repopulating stale data after that append invalidates the cache."
  [base-url params]
  (let [key [base-url params]
        now-ms (System/currentTimeMillis)]
    (if-let [entry (fresh-cache-entry @!query-cache key now-ms)]
      (:value entry)
      (let [lock-object (get (swap! !query-locks
                                    #(if (contains? % key)
                                       %
                                       (assoc % key (Object.))))
                             key)]
        (try
          (locking lock-object
            (let [now-ms (System/currentTimeMillis)]
              (if-let [entry (fresh-cache-entry @!query-cache key now-ms)]
                (:value entry)
                (let [generation (:generation @!query-cache)
                      value (fetch-entries-uncached base-url params)
                      expires-at-ms (+ (System/currentTimeMillis)
                                       query-cache-ttl-ms)]
                  (swap! !query-cache
                         (fn [state]
                           (if (= generation (:generation state))
                             (assoc state :entries
                                    (assoc
                                     (into {}
                                           (keep (fn [[cached-key cached-entry]]
                                                   (when (< (System/currentTimeMillis)
                                                            (:expires-at-ms cached-entry))
                                                     [cached-key cached-entry])))
                                           (:entries state))
                                     key {:value value
                                          :expires-at-ms expires-at-ms}))
                             state)))
                  value))))
          (finally
            (swap! !query-locks dissoc key)))))))

(defn get-entry-bounded
  "Read one evidence entry with CALLER-BOUND-MS as the complete HTTP bound.

  Snapshot publication uses this narrower operational bound instead of the
  store's general analytical-read timeout, so a visibility wave has an honest
  calculable deadline."
  [backend evidence-id caller-bound-ms]
  (let [{:keys [status body]}
        (get-edn (str (api-url (:base-url backend) "/api/alpha/evidence/")
                      (enc evidence-id))
                 caller-bound-ms)]
    (when (= 200 status) body)))

(defrecord Futon1bBackend [base-url]
  backend/EvidenceBackend

  (-append [_ validated]
    (let [eid (:evidence/id validated)
          trace-id (append-trace-id eid)
          serialized (serialize-append validated)]
      (if-let [serialization-error (:error serialized)]
        (social-error :store-serialization
                      "evidence payload is not EDN-wire-readable"
                      :evidence-id eid
                      :trace-id trace-id
                      :detail (.getMessage ^Exception serialization-error)
                      :invalid-edn (:invalid-edn serialized))
        (let [url (api-url base-url "/api/alpha/evidence")
              options {:timeout append-timeout-ms
                       :as :text
                       :headers {"content-type" "application/edn"
                                 "x-penholder" (penholder)
                                 "x-trace-id" trace-id}
                       :body (:body serialized)}
              started-ns (System/nanoTime)
              elapsed-ms (fn []
                           (quot (- (System/nanoTime) started-ns) 1000000))
              unreachable-error
              (fn [error attempts elapsed]
                (social-error :store-unreachable
                              "futon1b server unreachable"
                              :evidence-id eid :trace-id trace-id
                              :detail (str error)
                              :attempts attempts :elapsed-ms elapsed))]
          (loop [attempt 0]
            (let [{:keys [status body error]} @(http/post url options)
                  parsed (read-edn body)
                  attempts (inc attempt)]
              (cond
                (and error (timeout-error? error))
                (social-error :store-timeout
                              "futon1b persistence timed out"
                              :evidence-id eid :trace-id trace-id
                              :detail (str error))

                error
                (let [elapsed (elapsed-ms)
                      remaining (- (max 0 (long append-retry-ms)) elapsed)]
                  (if (pos? remaining)
                    ;; Sleep to the window edge at most, then ALWAYS attempt
                    ;; again: giving up part-way through a sleep discards
                    ;; window that is still usable, and the previous shape
                    ;; could sleep through the store coming back.
                    (let [backoff (min (* 100 (bit-shift-left 1 (min attempt 16)))
                                       (long append-retry-max-backoff-ms))
                          sleep-ms (min backoff remaining)]
                      (Thread/sleep sleep-ms)
                      (recur (inc attempt)))
                    (unreachable-error error attempts elapsed)))

                (= 201 status)
                {:ok true
                 :entry (or (:entry parsed) validated)
                 :trace-id trace-id}

                (and (= 409 status) (= :reply-not-found (:error parsed)))
                (social-error :reply-not-found
                              "in-reply-to references missing entry"
                              :in-reply-to (:evidence/in-reply-to validated)
                              :evidence-id eid :trace-id trace-id)

                (and (= 409 status) (= :fork-not-found (:error parsed)))
                (social-error :fork-not-found "fork-of references missing entry"
                              :fork-of (:evidence/fork-of validated)
                              :evidence-id eid :trace-id trace-id)

                ;; A bare 409 on a RETRY is our own earlier attempt having
                ;; landed. Evidence ids are client-minted, so a duplicate of
                ;; this exact payload can only be us; reporting failure here
                ;; would mask a write that actually succeeded -- the same
                ;; class of lie as losing it silently, with the sign flipped.
                (and (= 409 status) (pos? attempt))
                {:ok true
                 :entry (or (:entry parsed) validated)
                 :trace-id trace-id
                 :recovered-after-retry true}

                (= 409 status)
                (social-error :duplicate-id "Evidence id already exists"
                              :evidence-id eid :trace-id trace-id)

                :else
                (social-error :store-rejected "futon1b rejected the append"
                              :evidence-id eid :trace-id trace-id
                              :status status :body parsed))))))))

  (-get [_ evidence-id]
    (let [{:keys [status body]}
          (get-edn (str (api-url base-url "/api/alpha/evidence/") (enc evidence-id))
                   append-timeout-ms)]
      (when (= 200 status) body)))

  (-exists? [this evidence-id]
    (some? (backend/-get this evidence-id)))

  (-query [_ params]
    (let [subject-ref (:query/subject params)
          ref-types (if subject-ref
                      (subject/readable-ref-types (:ref/type subject-ref))
                      [nil])
          pages (mapv
                 (fn [ref-type]
                   (fetch-entries
                    base-url
                    (cond-> params
                      ref-type (assoc-in [:query/subject :ref/type]
                                         ref-type))))
                 ref-types)
          entries (mapcat identity pages)
          result (backend/filter-and-sort-entries entries params)
          partial-pages (filterv partial-result? pages)]
      (cond-> result
        (seq partial-pages)
        (with-meta {:partial? true
                    :partial-pages (mapv meta partial-pages)}))))

  (-count [_ params]
    ;; Futon1b's projected count path implements every supported filter. Never
    ;; fetch and hydrate the corpus merely to count a locally filtered subset.
    (let [params (dissoc params :query/limit)
          subject-ref (:query/subject params)
          ref-types (if subject-ref
                      (subject/readable-ref-types (:ref/type subject-ref))
                      [nil])]
      (reduce
       (fn [total ref-type]
         (let [params (cond-> params
                        ref-type (assoc-in [:query/subject :ref/type] ref-type))
               url (str (api-url base-url "/api/alpha/evidence/count")
                        "?" (query-string params))
               {:keys [status body]} (get-edn url)]
           (if (= 200 status)
             (+ total (long (or (:count body) 0)))
             (throw (ex-info "futon1b count failed"
                             {:status status :body body})))))
       0 ref-types)))

  (-forks-of [_ evidence-id]
    ;; include-ephemeral? true: -forks-of does not filter ephemeral (protocol)
    (->> (fetch-entries base-url {:query/fork-of evidence-id
                                  :query/include-ephemeral? true})
         (sort-by backend/entry-at)
         vec))

  (-delete! [_ ids]
    (println (str "[futon1b-backend] -delete! is a no-op (append-only store); "
                  "requested " (count ids) " ids"))
    {:compacted 0})

  (-all [_]
    (fetch-entries base-url {:query/include-ephemeral? true})))

(defn health
  "GET /health on the futon1b server. Returns the parsed body or nil when
   unreachable — used by the I-evidence-per-turn boot check."
  [base-url]
  (try
    (let [{:keys [status body]} (get-edn (api-url base-url "/health"))]
      (when (= 200 status) body))
    (catch Exception _ nil)))

(defn make-futon1b-backend
  "Construct the backend. base-url default: FUTON1B_URL env, then
   http://localhost:7074 (lucy's port — nginx owns :7073 there)."
  ([] (make-futon1b-backend (or (System/getenv "FUTON1B_URL") default-url)))
  ([base-url] (->Futon1bBackend base-url)))
