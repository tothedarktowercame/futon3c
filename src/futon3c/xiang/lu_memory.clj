(ns futon3c.xiang.lu-memory
  "驴 as a Minus40-native agent, write half: each scored pattern verdict row
  of an operator turn becomes one evidenced situation→pattern record in the
  Minus40 futon1b store (the user unit minus40-store on :7093, never the
  live :7073, never ~/.claude memory files).

  For each row of a record's :pattern_verdicts:
    - one evidence entity for the situation: the text is 驴's IF quote from
      :lu_ranked for that pattern when there is one, else the operator's
      whole :source_text; the record id, agent, session and created_at ride
      in the body and fields;
    - one hyperedge :lu/occurrence over [situation-evidence-id pattern-id]
      with :status, :turn_mapped, :chain_mapped, :sources, the verdict's
      :at and the pattern's :sha256, :hx/valid-time = the record's
      created_at.

  Void rows are written too (they are the negative cases).  Writing is
  idempotent: evidence ids and hyperedge ids are stable digests of
  (record-id, pattern-id, verdict :at), the hyperedge is only written when
  its id is not already stored, and a 409 duplicate evidence id counts as
  :unchanged.

  Registered by start! as a turn listener on :pattern-verdicts (raised by
  ma-occurrence after storing the verdicts), on this namespace's own
  daemon thread.  The file ~/code/data/minus40/lu-off stops it.  The HTTP
  transport is injectable as :request (fn [method path body] -> {:status
  .. :body ..}), so tests never touch a real store."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts])
  (:import [java.net URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.security MessageDigest]
           [java.util.concurrent ExecutorService Executors ThreadFactory]))

(def defaults
  "The write half's knobs. :base is the minus40 store (the user unit
  minus40-store); :request is the EDN transport, injectable for tests."
  {:base "http://127.0.0.1:7093"
   :request nil})

(def author "lu-memory")

(declare append-situation!) ; defined with the fast read half, used by the listener

(def off-switch-path
  "The 驴 off-switch (shared with lu-rerank): when this file exists the
  listener does nothing. An opts key (:off-switch) so tests never touch
  the real file."
  "/home/joe/code/data/minus40/lu-off")

;; ------------------------------------------------------------- the transport

(defn- parse [text]
  (try (edn/read-string text) (catch Exception _ text)))

(defn http-request
  "The production :request: one EDN request against BASE, status-tolerant
  (a 409 duplicate and a 404 lookup are outcomes, not exceptions)."
  [{:keys [base method path body]}]
  (let [req (-> (HttpRequest/newBuilder (java.net.URI/create (str base path)))
                (.timeout (java.time.Duration/ofSeconds 60))
                (.header "Accept" "application/edn")
                (.header "Content-Type" "application/edn")
                (.header "X-Penholder" "api")
                (.method method (if body
                                  (HttpRequest$BodyPublishers/ofString (pr-str body))
                                  (HttpRequest$BodyPublishers/noBody)))
                .build)
        resp (.send (HttpClient/newHttpClient) req
                    (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode resp) :body (parse (.body resp))}))

(defn- request [opts method path body]
  ((or (:request opts) (partial http-request))
   {:base (or (:base opts) (:base defaults)) :method method :path path :body body}))

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))

;; --------------------------------------------------------------- the records

(defn- sha256-hex [^String s]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) d))))

(defn situation-id
  "Stable evidence id for one (record, pattern, verdict-at) situation."
  [record-id pattern-id at]
  (str "e-lu-" (subs (sha256-hex (str record-id "\n" pattern-id "\n" at)) 0 20)))

(defn occurrence-id
  "Stable hyperedge id for one (record, pattern, verdict-at) occurrence."
  [record-id pattern-id at]
  (str "hx:lu.occurrence." (subs (sha256-hex (str record-id "\n" pattern-id "\n" at)) 0 20)))

(defn lu-quote
  "驴's IF quote for PATTERN-ID from the record's :lu_ranked, if any."
  [record pattern-id]
  (some (fn [entry]
          (when (= pattern-id (str (:id entry)))
            (str/trim (str (or (:quote entry) "")))))
        (:ranked (:lu_ranked record))))

(defn situation-evidence
  "The evidence entity for one verdict ROW of RECORD (id RECORD-ID): the
  situation text is 驴's quote when there is one, else the operator's whole
  :source_text."
  [record-id record row]
  (let [pattern-id (str (:pattern row))
        at (str (or (:at record) (:created_at record) ""))]
    {:evidence/id (situation-id record-id pattern-id at)
     :evidence/type :lu-situation
     :evidence/claim-type :observation
     :evidence/author author
     :evidence/at (str (:created_at record))
     :evidence/session-id (:session_id record)
     :evidence/subject {:ref/type :pattern :ref/id pattern-id}
     :evidence/body {:text (or (lu-quote record pattern-id)
                               (str (or (:source_text record) "")))
                     :text-source (if (lu-quote record pattern-id) "lu-quote" "operator-source-text")
                     :record (:turn_id record)
                     :agent (:agent_id record)
                     :session (:session_id record)
                     :created-at (:created_at record)
                     :pattern pattern-id}
     :evidence/tags [:lu :lu/occurrence :xiang]}))

(defn occurrence-hyperedge
  "The :lu/occurrence hyperedge for one verdict ROW of RECORD (id RECORD-ID)."
  [record-id record row]
  (let [pattern-id (str (:pattern row))
        at (str (or (:at record) (:created_at record) ""))
        eid (situation-id record-id pattern-id at)]
    {:hx/id (occurrence-id record-id pattern-id at)
     :hx/type :lu/occurrence
     :hx/endpoints [eid pattern-id]
     :hx/props {:roles {:situation eid :pattern pattern-id}
                :status (str (:status row))
                :turn_mapped (long (or (:turn_mapped row) 0))
                :chain_mapped (long (or (:chain_mapped row) 0))
                :sources (vec (or (:sources row) []))
                :at (str (get-in record [:pattern_verdicts :at]))
                :sha256 (:sha256 row)}
     :hx/valid-time (str (:created_at record))}))

(defn- outcome
  [{:keys [e-status h-status h-body error]}]
  (cond
    error error
    (and (#{200 201} e-status) (#{200 201} h-status)) :written
    (or (= 409 e-status)
        (and (#{200 404} e-status) (not (#{200 201} h-status))))
    {:evidence e-status :hyperedge h-status :body h-body}
    (#{200 201} h-status) :written
    :else {:evidence e-status :hyperedge h-status :body h-body}))

(defn- written-row
  "The append payload for one written row: situation id, text and props for
  the sidecar."
  [record-id record row]
  (let [ev (situation-evidence record-id record row)]
    {:id (:evidence/id ev)
     :text (get-in ev [:evidence/body :text])
     :props {:record record-id
             :session (:session_id record)
             :pattern (str (:pattern row))
             :status (str (:status row))
             :chain_mapped (long (or (:chain_mapped row) 0))
             :sources (vec (or (:sources row) []))}}))

(defn write-record!
  "Write every verdict row of RECORD-ID as a situation evidence + :lu/occurrence
  hyperedge pair into the minus40 store under OPTS. Returns a tally
  {:rows n :written n :unchanged n :failed [...]}. Idempotent per (record,
  pattern, verdict :at): the hyperedge id is stable and a pair whose
  hyperedge is already stored (GET 200) is not written again. Never throws."
  [opts record-id]
  (try
    (let [record (ts/read-record (:store opts) record-id)
          rows (or (-> record :pattern_verdicts :patterns) [])]
      (if (empty? rows)
        {:rows 0 :written 0 :unchanged 0 :failed [] :written-rows []}
        (let [results
              (for [row rows
                    :let [hx (occurrence-hyperedge record-id record row)
                          existing (:status (request opts "GET"
                                                      (str "/api/alpha/hyperedge/"
                                                           (enc (:hx/id hx)))
                                                      nil))]
                    :let [result
                          (if (= 200 existing)
                            :unchanged
                            (let [e (:status (request opts "POST" "/api/alpha/evidence"
                                                     (situation-evidence record-id record row)))
                                  h (request opts "POST" "/api/alpha/hyperedge" hx)]
                              (outcome {:e-status e :h-status (:status h)
                                        :h-body (:body h)})))]]
                result)]
          {:rows (count rows)
           :written (count (filter #(= :written %) results))
           :unchanged (count (filter #(= :unchanged %) results))
           :written-rows (vec (keep #(when (= :written (:result %))
                                       (written-row record-id record (:row %)))
                                    (map (fn [row result] {:row row :result result})
                                         rows results)))
           :failed (vec (keep #(when (map? %) %) results))})))
    (catch Throwable t
      {:rows 0 :written 0 :unchanged 0 :failed [{:error (str (.getMessage t))}]})))

;; ---------------------------------------------------------------- the query

(defn situations-of
  "PATTERN-ID's situations, read back by endpoint through the futon1b
  hyperedge query (as pattern-card-provider's default-card-query): each
  {:situation <evidence id> :status .. :chain_mapped .. :turn_mapped ..
   :sources .. :at ..}. Nil when the query fails."
  [opts pattern-id]
  (try
    (let [{:keys [status body]}
          (request opts "GET"
                   (str "/api/alpha/hyperedges?type=" (enc "lu/occurrence")
                        "&end=" (enc pattern-id) "&limit=1000&include-total=false")
                   nil)]
      (when (= 200 status)
        (vec (for [hx (:hyperedges body)]
               {:situation (first (:hx/endpoints hx))
                :status (get-in hx [:hx/props :status])
                :turn_mapped (get-in hx [:hx/props :turn_mapped])
                :chain_mapped (get-in hx [:hx/props :chain_mapped])
                :sources (get-in hx [:hx/props :sources])
                :at (get-in hx [:hx/props :at])}))))
    (catch Throwable _ nil)))

;; ------------------------------------------------------- the record-time leg

(defn- off? [opts]
  (try (.isFile (io/file (str (or (:off-switch opts) off-switch-path))))
       (catch Throwable _ false)))

(defn handle-event
  "One turn-service event: on :pattern-verdicts with a record id, write the
  verdict's hyperedges. Does nothing when the off-switch file exists.
  Never throws."
  [{:keys [store] :as opts} {:keys [event id]}]
  (when (and (= :pattern-verdicts event) id (not (off? opts)))
    (let [store (if (fn? store) (store) store)]
      (when store
        (let [tally (write-record! (assoc opts :store store) (str id))]
          ;; keep the sidecar current: append each newly written situation's
          ;; vector. A failure is logged inside append-situation!, never
          ;; thrown, and never blocks the write half.
          (doseq [{sid :id text :text props :props} (:written-rows tally)]
            (append-situation! sid text props)))))))

(defonce ^:private ^ExecutorService executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "lu-memory") (.setDaemon true))))))

(defn start!
  "Register the write-half listener on turn-service events. OPTS as per
  defaults plus :store (turn-store, or a 0-arg fn resolved at event time).
  Runs on this namespace's own daemon thread. Re-registering replaces the
  listener."
  [opts]
  (turn-service/add-turn-listener!
   ::lu-memory
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

;; ------------------------------------------------------------------- the seed

(defn seed!
  "One-off backfill: write every record that already carries
  :pattern_verdicts, using the same write function. Returns the tally plus
  counts by status, distinct patterns and distinct situations."
  ([opts] (seed! opts nil))
  ([opts store]
   (let [store (or store (:store opts))
         with-verdicts (->> (ts/list-records store :limit 100000)
                            (filter #(seq (-> (:record %) :pattern_verdicts :patterns))))]
     (loop [[{id :id r :record} & rest*] with-verdicts
            acc {:records 0 :rows 0 :written 0 :unchanged 0 :failed []
                 :by-status {} :patterns #{} :situations #{}}]
       (if-not id
         (-> acc
             (assoc :patterns (count (:patterns acc))
                    :situations (count (:situations acc))))
         (let [tally (write-record! (assoc opts :store store) id)
               rows (-> r :pattern_verdicts :patterns)
               at (str (or (:at r) (:created_at r) ""))]
           (recur rest*
                  (-> acc
                      (update :records inc)
                      (update :rows + (:rows tally))
                      (update :written + (:written tally))
                      (update :unchanged + (:unchanged tally))
                      (update :failed into (:failed tally))
                      (update :by-status (fn [m]
                                           (reduce (fn [m row]
                                                     (assoc m (:status row)
                                                            (inc (get m (:status row) 0))))
                                                   (or m {}) rows)))
                      (update :patterns into (set (map #(str (:pattern %)) rows)))
                      (update :situations
                              into (set (map #(situation-id id (str (:pattern %)) at)
                                             rows)))))))))))

;; -------------------------------------------------- the fast read half (2a)

(def read-defaults
  "The fast half's knobs. The rule is FIXED (claude-11, packet 2a; do not
  tune it on the data): k=10 nearest situations with cosine >= 0.35, a
  pattern scores sum(cosine * w(status)) with w occurrence 1.0 partial 0.5
  void -0.5, patterns with score > 0, top 3."
  {:k 10
   :min-cos 0.35
   :top 3
   :model "sentence-transformers/all-MiniLM-L6-v2"
   :script "/home/joe/code/futon3c/scripts/lu_situations.py"
   :query-timeout-ms 5000
   :startup-timeout-ms 60000})

(def status-weights {"occurrence" 1.0 "partial" 0.5 "void" -0.5})

(defn score-patterns
  "The fixed scoring rule over NEIGHBOURS ({:pattern :status :cos :record}):
  each pattern scores sum(cosine * w(status)); keep score > 0, top 3 by
  score, each {:id :score :rank :evidence [the neighbour records]}. Pure."
  ([neighbours] (score-patterns neighbours 3))
  ([neighbours top]
   (->> neighbours
        (group-by :pattern)
        (map (fn [[pid ns]]
               {:id pid
                :score (double (reduce + (map #(-> (get status-weights (:status %) 0.0)
                                                   (* (double (or (:cos %) 0.0))))
                                              ns)))
                :evidence (vec (distinct (keep :record ns)))}))
        (filter #(pos? (:score %)))
        (sort-by (juxt (comp - :score) :id))
        (take (long top))
        (map-indexed (fn [i p] (assoc p :rank (inc i))))
        vec)))

;; The resident process: pattern-search keeps the futon3a notions search
;; resident as one owned python process speaking one JSON line per query
;; (handshake {"resident" "ready"}, single owner lock, restart on death,
;; bounded timeouts). lu-situations.py speaks the same protocol and also
;; embeds appends, so the same mechanism is reused here with a second
;; resident owned by this namespace — no second model load per call.

(defonce ^:private read-owner-lock (Object.))
(defonce ^:private !read-resident (atom nil))

(defn- futon3a-python []
  (or (System/getenv "FUTON3A_PYTHON")
      (str (System/getProperty "user.home") "/code/futon3a/.venv/bin/python3")))

(defn- read-resident-command [opts]
  (or (:resident-command opts)
      [(futon3a-python) "-u" (:script (merge read-defaults opts))
       "--resident" "--model" (:model (merge read-defaults opts))]))

(defn- destroy-read-resident! [{:keys [^java.lang.Process process
                                       ^java.io.BufferedReader reader
                                       ^java.io.BufferedWriter writer]}]
  (when writer (try (.close writer) (catch Throwable _)))
  (when process
    (try
      (when (.isAlive process)
        (.destroyForcibly process)
        (.waitFor process 500 java.util.concurrent.TimeUnit/MILLISECONDS))
      (catch Throwable _)))
  (when reader (try (.close reader) (catch Throwable _))))

(defn stop-read-resident! []
  (locking read-owner-lock
    (destroy-read-resident! @!read-resident)
    (reset! !read-resident nil)))

(defn- start-read-resident! [opts]
  (let [builder (doto (java.lang.ProcessBuilder.
                       ^java.util.List (vec (read-resident-command opts)))
                  (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))
        env (.environment builder)
        _ (.put env "HF_HUB_OFFLINE" "1")
        _ (.put env "TRANSFORMERS_OFFLINE" "1")
        process (.start builder)
        resident {:process process
                  :reader (java.io.BufferedReader.
                           (java.io.InputStreamReader. (.getInputStream process)))
                  :writer (java.io.BufferedWriter.
                           (java.io.OutputStreamWriter. (.getOutputStream process)))}
        pending (future (.readLine ^java.io.BufferedReader (:reader resident)))
        token (Object.)
        line (deref pending (:startup-timeout-ms (merge read-defaults opts)) token)]
    (if (or (identical? token line) (nil? line))
      (do (future-cancel pending)
          (destroy-read-resident! resident)
          (throw (ex-info "lu situation resident failed startup handshake"
                          {:error/code :resident-startup :line (str line)})))
      (do (future-cancel pending)
          (reset! !read-resident resident)
          resident))))

(defn read-resident-op!
  "One op against the resident situation reader: writes one JSON line,
  reads one JSON reply. Throws on timeout/exit/malformed (the caller
  decides whether that is a fallback)."
  ([op] (read-resident-op! op {}))
  ([op opts]
   (locking read-owner-lock
     (let [resident (or (when-let [r @!read-resident]
                          (when (.isAlive ^java.lang.Process (:process r)) r))
                        (start-read-resident! opts))
           ^java.io.BufferedWriter writer (:writer resident)
           ^java.io.BufferedReader reader (:reader resident)]
       (try
         (.write writer (json/generate-string op))
         (.newLine writer)
         (.flush writer)
         (let [pending (future (.readLine reader))
               token (Object.)
               line (deref pending (:query-timeout-ms (merge read-defaults opts)) token)]
           (cond
             (identical? token line)
             (throw (ex-info "lu resident op timed out" {:error/code :resident-timeout}))
             (nil? line)
             (throw (ex-info "lu resident exited" {:error/code :resident-exited}))
             :else (let [reply (json/parse-string line true)]
                     (when-not (and (map? reply) (true? (:ok reply)))
                       (throw (ex-info "lu resident op refused"
                                       {:error/code :resident-refused
                                        :reply reply})))
                     reply)))
         (catch Throwable t
           (destroy-read-resident! resident)
           (reset! !read-resident nil)
           (throw t)))))))

(defn read-near
  "The fast read half: the patterns that occurred in the stored situations
  most like TEXT. Opts: :record-id (exclude that record's situations, so a
  turn never retrieves itself), plus read-defaults. Returns
  {:ranked [{:id :score :rank :evidence [record ids]}] :neighbours n
   :ms n :source \"驴/m40\" | nil :reason ...}. Fewer than 3 neighbours
  above the threshold, or no positive pattern, gives :source nil with the
  reason (the caller's cue to fall back to the embedding). Never throws."
  ([text] (read-near text {}))
  ([text opts]
   (let [opts (merge read-defaults opts)
         t0 (System/nanoTime)]
     (try
       (let [reply (read-resident-op!
                    {:op "near" :text (str text)
                     :k (long (:k opts)) :min-cos (double (:min-cos opts))
                     :exclude-records (vec (keep identity [(:record-id opts)]))}
                    opts)
             neighbours (vec (:neighbours reply))
             ms (quot (- (System/nanoTime) t0) 1000000)
             ranked (score-patterns neighbours (:top opts))]
         (cond
           (< (count neighbours) 3)
           {:ranked [] :neighbours (count neighbours) :ms ms :source nil
            :reason "fewer-than-3-neighbours"}
           (empty? ranked)
           {:ranked [] :neighbours (count neighbours) :ms ms :source nil
            :reason "no-positive-pattern"}
           :else {:ranked ranked :neighbours (count neighbours) :ms ms
                  :source "驴/m40"}))
       (catch Throwable t
         {:ranked [] :neighbours 0
          :ms (quot (- (System/nanoTime) t0) 1000000) :source nil
          :reason (str "resident-error: " (or (.getMessage t) (class t)))})))))

(defn append-situation!
  "Keep the sidecar current: embed one situation (id, text, props) and
  append its vector through the resident. Returns true when appended,
  false when unchanged, and nil (logged, never thrown) on failure — a
  failure here must never block the write half."
  ([id text props] (append-situation! id text props {}))
  ([id text props opts]
   (try
     (let [reply (read-resident-op! (assoc props :op "add" :id (str id)
                                          :text (str text))
                                    (merge read-defaults opts))]
       (not (true? (:unchanged reply))))
     (catch Throwable t
       (println (str "[lu-memory] situation append failed for " id ": "
                     (.getMessage t)))
       nil))))
