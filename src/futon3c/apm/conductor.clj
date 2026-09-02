(ns futon3c.apm.conductor
  "Callable orchestration for one APM problem frame.

   This namespace owns sequence, checkpointing, and a compact operation log.
   The problem peripheral remains the sole owner of cycle state and invariants."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [babashka.http-client :as http-client]
            [cheshire.core :as json]
            [futon3c.agency.registry :as agency]
            [futon3c.apm.conductor-binding :as binding]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.substrate.client :as substrate])
  (:import [java.util.concurrent CompletableFuture ExecutionException
            TimeUnit TimeoutException]))

(def ^:private default-memory-cascade-cap 100)
(def ^:dynamic *cascade-request-timeout-ms* 60000)
(def ^:dynamic *cascade-connect-timeout-ms* 5000)
(def ^:dynamic *cascade-read-parallelism* 8)
(def ^:dynamic *cascade-admission-retries* 3)
(def ^:dynamic *cascade-retry-sleep!* #(Thread/sleep %))

(def default-memory-cascade-operation-budget-ms (* 30 60 1000))

(declare expand-memory-cascade)

(defn run-observed-memory-cascade
  "Run one cascade expansion while durably publishing its bounded operation.

   PERSIST-FN is called before substrate work and once at termination. A
   persistence refusal fails closed: an unobservable long-running expansion
   must not be mistaken for a conformant one by the projection watchdog."
  [seed-memory-ids options
   {:keys [persist-fn now-ms-fn now-fn budget-ms authority expand-fn]
    :or {now-ms-fn #(System/currentTimeMillis)
         now-fn #(str (java.time.Instant/now))
         budget-ms default-memory-cascade-operation-budget-ms
         expand-fn expand-memory-cascade}}]
  (when-not (and (fn? persist-fn) (pos-int? budget-ms))
    (throw (ex-info "memory cascade operation authority invalid"
                    {:error/component :apparatus
                     :error/code :memory-cascade-operation-authority-invalid
                     :budget-ms budget-ms})))
  (let [started-ms (long (now-ms-fn))
        operation-id (str (java.util.UUID/randomUUID))
        base (merge {:state/type :memory-cascade-operation
                     :operation/id operation-id
                     :operation :memory-cascade-expansion
                     :status :running
                     :started-at (now-fn)
                     :started-at-ms started-ms
                     :budget-ms budget-ms
                     :deadline-at-ms (+ started-ms budget-ms)
                     :attempt 1
                     :progress {:stage :expanding
                                :seed-count (count seed-memory-ids)}}
                    authority)
        persisted (persist-fn base)]
    (when-not (:ok persisted)
      (throw (ex-info "memory cascade operation authority persistence failed"
                      {:error/component :apparatus
                       :error/code :memory-cascade-operation-persistence-failed
                       :finding persisted})))
    (try
      (let [result (expand-fn seed-memory-ids options)
            finished-ms (long (now-ms-fn))
            terminal (assoc base
                            :status :succeeded
                            :finished-at (now-fn)
                            :finished-at-ms finished-ms
                            :result {:outcome :ok
                                     :elapsed-ms (- finished-ms started-ms)
                                     :expanded-count (:expanded-count result)
                                     :expanded-available
                                     (:expanded-available result)
                                     :truncated? (:truncated? result)})
            saved (persist-fn terminal)]
        (when-not (:ok saved)
          (throw (ex-info "memory cascade terminal authority persistence failed"
                          {:error/component :apparatus
                           :error/code
                           :memory-cascade-operation-persistence-failed
                           :finding saved})))
        result)
      (catch Throwable t
        (let [data (ex-data t)
              status (:status data)
              finished-ms (long (now-ms-fn))
              outcome (if (= 503 status) :failed-503 :failed)
              terminal (assoc base
                              :status :failed
                              :finished-at (now-fn)
                              :finished-at-ms finished-ms
                              :result (cond->
                                      {:outcome outcome
                                       :elapsed-ms (- finished-ms started-ms)
                                       :error/code (or (:error/code data)
                                                       :memory-cascade-failed)}
                                status (assoc :http/status status)))]
          ;; Preserve the original failure. If the terminal write also fails,
          ;; its absence remains fail-closed to the watchdog at the deadline.
          (try (persist-fn terminal) (catch Throwable _ nil))
          (throw t))))))

(defn- bounded-parallel-map
  "Apply F concurrently in small batches while retaining input order.

   Cascade endpoints are independent, but unbounded `pmap` can overload the
   substrate.  Batch boundaries provide a fixed concurrency ceiling and force
   every future before starting the next batch."
  [f xs]
  (->> xs
       (partition-all *cascade-read-parallelism*)
       (mapcat (fn [batch]
                 (let [pending (mapv #(future (f %)) batch)]
                   (mapv deref pending))))))

(defn- default-close-hook
  [{:keys [agency-base analyst-seat caller prompt]}]
  (let [response (http-client/post
                  (str agency-base "/api/alpha/bell")
                  {:headers {"Content-Type" "application/json"}
                   :body (json/generate-string
                          {:agent-id analyst-seat
                           :caller caller
                           :surface "bell"
                           :mode "brief"
                           :prompt prompt})
                   :throw false
                   :timeout 10000})]
    (if (= 202 (:status response))
      {:status :sent :http-status 202}
      {:status :failed :reason :bell-refused
       :http-status (:status response)})))

(defn- analyst-wake!
  [closed]
  (let [config (:config closed)
        seat (some-> (:analyst-seat config) str str/trim not-empty)
        problem-id (:problem-id config)
        cycle-id (:cycle-id closed)
        envelope (:envelope closed)
        payload {:event :apm/frame-closed
                 :problem-id problem-id
                 :cycle-id cycle-id
                 :launchable? (:launchable? envelope)
                 :failure-count (count (:failures envelope))}]
    (cond
      (nil? seat)
      {:status :skipped :reason :analyst-seat-not-configured
       :payload payload}

      (nil? (agency/get-agent seat))
      {:status :skipped :reason :analyst-seat-unregistered
       :analyst-seat seat :payload payload}

      :else
      (try
        (let [hook (or (:close-hook config) default-close-hook)
              result (hook {:agency-base
                            (or (:agency-base config)
                                (get-in config [:conductor :park-base])
                                "http://localhost:7070")
                            :analyst-seat seat
                            :caller (or (get-in config [:conductor :agent])
                                        "apm-conductor")
                            :payload payload
                            :prompt (str "APM frame closed; run Analyst close checks and "
                                         "append the series entry.\n" (pr-str payload))})]
          (merge {:analyst-seat seat :payload payload} result))
        (catch Throwable t
          {:status :failed :reason :close-hook-threw
           :analyst-seat seat :payload payload
           :error/message (.getMessage t)})))))

(defn- report-analyst-wake! [wake]
  (when-not (= :sent (:status wake))
    (binding [*out* *err*]
      (println (str "[apm.conductor] Analyst wake "
                    (name (or (:status wake) :unknown))
                    (when-let [reason (:reason wake)]
                      (str ": " (name reason)))
                    (when-let [seat (:analyst-seat wake)]
                      (str " seat=" seat))))))
  wake)

(defn- safe-analyst-wake! [closed]
  (try
    (report-analyst-wake! (analyst-wake! closed))
    (catch Throwable t
      (report-analyst-wake!
       {:status :failed :reason :wake-boundary-threw
        :error/message (.getMessage t)}))))

(defn- failure [handle code message & [details]]
  (cond-> (assoc handle :ok false
                 :error {:error/component :apm-conductor
                         :error/code code
                         :error/message message})
    details (assoc-in [:error :error/context] details)))

(defn- logged [handle tool result]
  (update handle :log (fnil conj [])
          (cond-> {:tool tool :ok (true? (:ok result))}
            (:result result) (assoc :result (:result result))
            (:error/code result) (assoc :error/code (:error/code result))
            (:error/message result) (assoc :error/message (:error/message result)))))

(defn- raw-step [handle tool args]
  (if (false? (:ok handle))
    {:handle handle}
    (try
      (let [result (runner/step (:peripheral handle) (:state handle)
                                {:tool tool :args (vec args)})
            next-handle (logged handle tool result)]
        (if (:ok result)
          {:handle (assoc next-handle :state (:state result))
           :result (:result result)}
          {:handle (failure next-handle
                            (or (:error/code result) :tool-refused)
                            (or (:error/message result)
                                (str tool " refused"))
                            (:error/context result))}))
      (catch Throwable t
        {:handle (failure (logged handle tool {:ok false})
                          :tool-threw
                          (str tool " threw: " (.getMessage t)))}))))

(defn- checkpoint [handle]
  (if (false? (:ok handle))
    {:handle handle}
    (raw-step handle :problem-save [])))

(def ^:private refusal-diagnostic-keys
  #{:artifact-id :lane :memory-id :offer-id :outcome :pattern-id :reviewer})

(defn- bounded-string [x]
  (let [s (str x)]
    (if (> (count s) 160) (str (subs s 0 160) "…") s)))

(defn- refusal-diagnostic [arg]
  (if (map? arg)
    {:arg/type :map
     :arg/keys (->> (keys arg) (map str) sort vec)
     :arg/diagnostic
     (into {}
           (keep (fn [[k v]]
                   (when (and (contains? refusal-diagnostic-keys k)
                              (or (string? v) (keyword? v)
                                  (boolean? v) (number? v)))
                     [k (if (string? v) (bounded-string v) v)])))
           arg)}
    {:arg/type (cond
                 (string? arg) :string
                 (sequential? arg) :collection
                 (nil? arg) :nil
                 :else :scalar)}))

(defn record-action-refusal!
  "Checkpoint a sanitized refusal on the last authoritative, non-failed handle."
  [handle {:keys [action-id operation args]} failed-handle]
  (let [error (:error failed-handle)
        receipt {:refusal/action-id action-id
                 :refusal/tool operation
                 :refusal/args (mapv refusal-diagnostic (or args []))
                 :refusal/error (select-keys error
                                             [:error/component :error/code
                                              :error/message])
                 :refusal/step-index (count (get-in handle [:state :steps]))}
        recorded (update-in handle [:state :cycle/action-refusals]
                            (fnil conj []) receipt)]
    (:handle (checkpoint recorded))))

(defn- saved-step [handle tool args]
  (let [{h :handle result :result} (raw-step handle tool args)]
    (if (false? (:ok h))
      {:handle h}
      (let [{saved :handle} (checkpoint h)]
        {:handle saved :result result}))))

(defn- advance [handle payload]
  (saved-step handle problem/advance
              ["apm-conductor" (:problem-id (:config handle)) payload]))

(defn- decoded-response-body [response]
  (let [body (:body response)]
    (if (string? body)
      (try (edn/read-string body) (catch Throwable _ body))
      body)))

(defn- expensive-read-busy? [response]
  (and (= 503 (:status response))
       (= :expensive-read-busy (:error (decoded-response-body response)))))

(defn- response-edn [response context]
  (if (= 200 (:status response))
    (decoded-response-body response)
    (throw (ex-info "memory cascade substrate read failed"
                    (cond-> (assoc context :status (:status response)
                                   :body (:body response))
                      (expensive-read-busy? response)
                      (assoc :error/component :transport
                             :error/code :memory-cascade-unreachable))))))

(defn- cascade-read-edn
  "Run one bounded cascade request, honoring futon1b's explicit busy signal.

   Only 503 :expensive-read-busy is retried. The retry count is separate from
   transport timeouts and the server's retry-after is used when present."
  [request-fn context]
  (loop [attempt 0]
    (let [response (request-fn)]
      (if (and (expensive-read-busy? response)
               (< attempt *cascade-admission-retries*))
        (let [body (decoded-response-body response)
              retry-after (get body :retry-after-seconds)
              delay-ms (if (and (number? retry-after) (pos? retry-after))
                         (* 1000 (long retry-after))
                         (* 100 (bit-shift-left 1 attempt)))]
          (*cascade-retry-sleep!* delay-ms)
          (recur (inc attempt)))
        (response-edn response context)))))

(defn- bounded-cascade-get
  "GET and decode the complete response body inside one wall-clock bound.

   babashka.http-client 0.4.23 maps :timeout to HttpRequest.timeout, but uses
   BodyHandlers/ofInputStream and decodes that stream afterwards. The JDK
   timeout ends once response headers arrive, so a stalled body can otherwise
   block forever. The outer CompletableFuture deadline covers interception and
   body decoding as well."
  [url options context]
  (let [timeout-ms *cascade-request-timeout-ms*
        client (http-client/client
                {:connect-timeout *cascade-connect-timeout-ms*})
        future ^CompletableFuture
        (http-client/get url (assoc options :client client :async true
                                    :timeout timeout-ms))]
    (try
      ;; Enforce the deadline in the coordinator thread itself. Relying on
      ;; CompletableFuture.orTimeout delegates expiry to the JVM-global
      ;; Delayer, which can be starved while the serving JVM is under load.
      (.get future timeout-ms TimeUnit/MILLISECONDS)
      (catch TimeoutException error
        (.cancel future true)
        (throw (ex-info "memory cascade substrate transport timed out"
                        (assoc context
                               :error/component :transport
                               :error/code :memory-cascade-unreachable
                               :timeout-ms timeout-ms
                               :error/message (.getMessage error))
                        error)))
      (catch ExecutionException error
        (let [cause (or (.getCause error) error)]
          (throw (ex-info "memory cascade substrate transport failed"
                          (assoc context
                                 :error/component :transport
                                 :error/code :memory-cascade-unreachable
                                 :timeout-ms timeout-ms
                                 :error/message (.getMessage cause))
                          cause)))))))

(defn- cascade-get [base path query-params]
  (let [context {:path path :query-params query-params}]
    (cascade-read-edn
     #(bounded-cascade-get (str (str/replace base #"/+$" "") path)
                           {:query-params query-params :throw false}
                           context)
     context)))

(defn- cascade-pattern [base pattern-id]
  (let [path (str "/api/alpha/entity/"
                  (java.net.URLEncoder/encode (str pattern-id) "UTF-8"))
        context {:path path :pattern-id pattern-id}]
    (cascade-read-edn
     #(bounded-cascade-get (str (str/replace base #"/+$" "") path)
                           {:headers {"Accept" "application/edn"}
                            :throw false}
                           context)
     context)))

(defn- cascade-memory [base memory-id]
  (let [path (str "/api/alpha/evidence/"
                  (java.net.URLEncoder/encode (str memory-id) "UTF-8"))
        context {:path path :memory-id memory-id}]
    (cascade-read-edn
     #(bounded-cascade-get (str (str/replace base #"/+$" "") path)
                           {:headers {"Accept" "application/edn"}
                            :throw false}
                           context)
     context)))

(defn- qualified-name [x]
  (if (keyword? x)
    (if-let [ns (namespace x)] (str ns "/" (name x)) (name x))
    (str x)))

(defn- reviewed-attachment? [edge]
  ;; A superseded edge version keeps its :reviewed status; only the :current
  ;; version is an attachment. Without this the hub counted 41 where the store
  ;; held 40 (H5b, 2026-08-26). Mirrors memory_snapshot/candidate-visible?.
  (and (= "memory/assert" (qualified-name (:hx/type edge)))
       (= "reviewed"
          (qualified-name
           (or (get-in edge [:hx/props :attachment-status])
               (:prop/attachment-status edge))))
       (= "current"
          (qualified-name
           (or (get-in edge [:hx/props :state])
               (:prop/state edge))))))

(defn- attachment-memory-id [edge]
  (or (get-in edge [:hx/props :roles :entry])
      (get-in edge [:prop/roles :entry])))

(defn- attachment-patterns [edge]
  (vec (or (get-in edge [:hx/props :roles :patterns])
           (get-in edge [:prop/roles :patterns]) [])))

(defn- attachment-problems [edge]
  ;; APM problem ids are the only machine-identified problem endpoints in the
  ;; historical attachment shape; the remaining :subjects are hooks, missions,
  ;; and pattern ids. Keep this deliberately narrow rather than treating every
  ;; subject as a co-incidence bridge.
  (->> (or (get-in edge [:hx/props :roles :subjects])
           (get-in edge [:prop/roles :subjects]) [])
       (filter #(and (string? %) (re-matches #"[A-Za-z]\d{2}[A-Z]\d{2}" %)))
       vec))

;; The substrate refuses any hyperedge window above 1000 with a layer-4
;; :invalid-limit (futon1b 4cd17bc, parse-hyperedge-limit, 2026-08-23). This
;; reader asked for 5000, which worked until that day — the round-1 frames
;; f9/f10/f13/f15 (2026-08-18/20) expanded through it and persisted routed
;; offers (D1-round1-cascade-offers-2026-08-26.md) — and threw from then on
;; (D0, 2026-08-26). The `end=` form ignores `after`, so there is no cursor to
;; page with; a full page is therefore refused rather than silently truncated.
(def ^:private cascade-hyperedge-page-limit 1000)

(defn- complete-page
  "ROWS as returned for a window of LIMIT. A full window cannot be shown to be
   exhaustive on an endpoint form without a cursor, so it is an error, not a
   result."
  [rows limit context]
  (if (>= (count rows) limit)
    (throw (ex-info "memory cascade attachment window overflow"
                    (assoc context :limit limit :count (count rows))))
    rows))

(defn- live-cascade-readers [config]
  (let [base (or (:evidence-store-url config) (substrate/configured-url))
        attachments
        (memoize
         (fn [endpoint]
           (-> (:hyperedges
                (cascade-get base "/api/alpha/hyperedges"
                             {:end endpoint :type "memory/assert"
                              :limit cascade-hyperedge-page-limit}))
               (complete-page cascade-hyperedge-page-limit {:endpoint endpoint})
               (->> (filter reviewed-attachment?) vec))))
        why-targets
        (memoize
         (fn [pattern-id]
           (->> (:relations
                 (cascade-get base "/api/alpha/relations"
                              {:from pattern-id :limit 100}))
                (keep (fn [relation]
                        (when (= "pattern/has-semantic-why"
                                 (qualified-name (:relation/type relation)))
                          (or (:relation/to relation) (:relation/dst relation)))))
                distinct vec)))
        pattern
        (memoize
         (fn [pattern-id]
           (try
             (cascade-pattern base pattern-id)
             (catch clojure.lang.ExceptionInfo error
               (if (= :transport (:error/component (ex-data error)))
                 (throw error)
                 nil)))))
        memory (memoize #(cascade-memory base %))]
    {:attachments-fn
     attachments
     :why-targets-fn why-targets
     ;; Pattern content is additive. A missing legacy entity must not hide
     ;; the named pattern offer or break an otherwise valid memory cascade.
     :pattern-fn pattern
     :memory-fn memory}))

(defn domain-general-pattern-id?
  "True when PATTERN-ID's pre-slash family has no uppercase subject suffix."
  [pattern-id]
  (let [family (first (str/split (str pattern-id) #"/" 2))]
    (not (boolean (re-find #"-[A-Z]{2,}$" family)))))

(defn- pattern-surface-content [surface]
  (let [entity (or (:entity surface) surface)
        props (or (:entity/props entity) (:props entity) entity)
        hook (or (:hook props) (:pattern/hook props) (:source entity))
        body (or (:body props) (:pattern/body props))
        content (when (and (map? props) (seq props)) props)]
    (cond-> {}
      (some? hook) (assoc :offer/pattern-hook hook)
      (some? body) (assoc :offer/pattern-body body)
      content (assoc :offer/pattern-content content))))

(defn- memory-surface-content [surface]
  (let [entry (or (:entry surface) surface)
        raw-body (:evidence/body entry)
        body (if (string? raw-body)
               (try (edn/read-string raw-body) (catch Throwable _ nil))
               raw-body)
        name (:name body)
        hook (:hook body)]
    (when (and (some? name) (some? hook))
      {:offer/name name :offer/hook hook})))

(defn expand-memory-cascade
  "Expand surfaced memories through reviewed pattern attachments.

   `attachments-fn` returns memory/assert edges for an endpoint;
   `why-targets-fn` returns authored @why targets for a pattern. The result is
   bounded after cheapest-route deduplication. `:expanded-count` excludes the
   leaf memories already surfaced by retrieval."
  [seed-memory-ids {:keys [attachments-fn why-targets-fn pattern-fn memory-fn
                           cap routes exclude]
                    :or {cap default-memory-cascade-cap}}]
  (let [attachments-fn (memoize attachments-fn)
        routes-enabled (or routes #{:why-hop :co-incidence})
        seed-memory-ids (vec (distinct seed-memory-ids))
        seed-memory-set (set seed-memory-ids)
        ;; `:exclude` — memory ids that must not be offered by ANY route,
        ;; whatever the store says. Every structural route reads candidates
        ;; from the store (attachments-fn), not from the shelf, so an id the
        ;; caller withheld from the seeds (the attempt-1 same-problem holdout,
        ;; prereg amendment 8) would otherwise come straight back as a
        ;; sibling: it is attached to the very patterns the seeds sit on.
        excluded (set exclude)
        seed-edges (mapcat identity
                           (bounded-parallel-map attachments-fn seed-memory-ids))
        seed-patterns (vec (distinct (mapcat attachment-patterns seed-edges)))
        ;; Authored why edges form a directed graph. Record the shortest
        ;; distance from any seed pattern.
        why-patterns
        (if (contains? routes-enabled :why-hop)
          (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                             (map #(vector % 0) seed-patterns))
                 seen (zipmap seed-patterns (repeat 0))]
            (if (empty? queue)
              (dissoc seen nil)
              (let [[pattern hops] (peek queue)
                    queue (pop queue)
                    next-hop (inc hops)
                    targets (remove #(contains? seen %) (why-targets-fn pattern))]
                (recur (into queue (map #(vector % next-hop) targets))
                       (reduce #(assoc %1 %2 next-hop) seen targets)))))
          {})
        why-patterns (apply dissoc why-patterns seed-patterns)
        ;; Co-incidence is exactly pattern -> problem -> pattern. Only the
        ;; original seed patterns initiate it; it does not recursively flood.
        seed-pattern-edges
        (mapcat identity (bounded-parallel-map attachments-fn seed-patterns))
        seed-problems (vec (distinct (mapcat attachment-problems
                                             seed-pattern-edges)))
        coincident-patterns
        (if (contains? routes-enabled :co-incidence)
          (->> seed-problems
               (bounded-parallel-map attachments-fn)
               (mapcat identity)
               (mapcat attachment-patterns)
               (remove (set seed-patterns))
               distinct
               (map #(vector % 2))
               (into {}))
          {})
        ;; Warm every independent attachment endpoint with bounded concurrency.
        ;; `live-cascade-readers` memoizes these calls for the comprehensions
        ;; below, so result ordering and cheapest-route selection stay exact.
        _ (dorun (bounded-parallel-map attachments-fn
                                       (distinct (concat seed-patterns
                                                         (keys why-patterns)
                                                         (keys coincident-patterns)))))
        route-rank {:sibling 0 :why-hop 1 :co-incidence 2}
        route-key (fn [{:keys [route hops pattern]}]
                    [hops (get route-rank route 3) (str pattern)])
        structural-unbounded
        (concat
         (when (contains? routes-enabled :sibling)
           (for [pattern seed-patterns
                 edge (attachments-fn pattern)
                 :let [memory-id (attachment-memory-id edge)]
                 :when (and memory-id (not (seed-memory-set memory-id)))]
             [memory-id {:route :sibling :hops 1 :pattern pattern}]))
         (for [[pattern hops] why-patterns
               edge (attachments-fn pattern)
               :let [memory-id (attachment-memory-id edge)]
               :when (and memory-id (not (seed-memory-set memory-id)))]
           [memory-id {:route :why-hop :hops hops :pattern pattern}])
         (for [[pattern hops] coincident-patterns
               edge (attachments-fn pattern)
               :let [memory-id (attachment-memory-id edge)]
               :when (and memory-id (not (seed-memory-set memory-id)))]
           [memory-id {:route :co-incidence :hops hops :pattern pattern}]))
        excluded-offers (->> structural-unbounded
                             (map first)
                             (filter excluded)
                             distinct
                             count)
        structural (remove #(excluded (first %)) structural-unbounded)
        cheapest
        (reduce (fn [by-memory [memory-id route]]
                  (update by-memory memory-id
                          #(if (or (nil? %)
                                   (neg? (compare (route-key route)
                                                  (route-key %))))
                             route %)))
                {} structural)
        ordered (->> cheapest
                     (sort-by (fn [[memory-id route]]
                                (conj (route-key route) memory-id)))
                     vec)
        selected (vec (take cap ordered))
        sibling-selected (if memory-fn
                           (filterv #(= :sibling (get-in % [1 :route])) selected)
                           [])
        enrichment-results
        (bounded-parallel-map
         (fn [[memory-id _]]
           (try
             [memory-id (some-> (memory-fn memory-id)
                                memory-surface-content)]
             (catch Throwable _ [memory-id nil])))
         sibling-selected)
        enrichment-by-memory (into {} (keep (fn [[memory-id content]]
                                               (when content [memory-id content])))
                                         enrichment-results)
        selected (mapv (fn [[memory-id route]]
                         [memory-id (merge route
                                           (get enrichment-by-memory memory-id))])
                       selected)
        enrichment {:attempted (count sibling-selected)
                    :enriched (count enrichment-by-memory)
                    :failed (- (count sibling-selected)
                               (count enrichment-by-memory))}
        offered-pattern-ids
        (->> selected
             (keep (comp :pattern second))
             (filter domain-general-pattern-id?)
             distinct
             vec)
        pattern-surfaces
        (if pattern-fn
          (into {}
                (keep (fn [pattern-id]
                        (when-let [surface (pattern-fn pattern-id)]
                          [pattern-id surface])))
                offered-pattern-ids)
          {})]
    {:routes (into (mapv #(vector % {:route :leaf :hops 0}) seed-memory-ids)
                   selected)
     :routes-enabled routes-enabled
     :cascade/enrichment enrichment
     :pattern-surfaces pattern-surfaces
     :seed-patterns seed-patterns
     :patterns-per-problem (count seed-patterns)
     :expanded-count (count selected)
     :expanded-available (count ordered)
     :exclude-count (count excluded)
     :excluded-offers excluded-offers
     :cap cap
     :truncated? (> (count ordered) cap)}))

(defn cascade-receipt-offers
  "Turn one dispatch receipt into route-labelled, optionally expanded offers."
  [receipt config]
  (let [body (:body receipt)
        job-id (:job-id body)
        memory-ids (vec (get-in body [:memory-use :memory-use/surfaced-ids]))
        enabled? (true? (:memory-cascade-enabled? config))
        expansion (when enabled?
                    (expand-memory-cascade
                     memory-ids
                     (cond->
                      (merge (live-cascade-readers config)
                             {:cap (or (:memory-cascade-cap config)
                                       default-memory-cascade-cap)})
                       (contains? config :memory-cascade-routes)
                       (assoc :routes (:memory-cascade-routes config)))))
        routes (or (:routes expansion)
                   (mapv #(vector % {:route :leaf :hops 0}) memory-ids))
        routed-counts (frequencies (keep (comp :pattern second) routes))
        leaves (filterv #(= :leaf (get-in % [1 :route])) routes)
        expanded (remove #(= :leaf (get-in % [1 :route])) routes)
        pattern-and-memory-items
        (:items
         (reduce
          (fn [{:keys [seen items]} [_ route :as memory-route]]
            (let [pattern-id (:pattern route)
                  emit-pattern? (and pattern-id
                                     (domain-general-pattern-id? pattern-id)
                                     (not (contains? seen pattern-id)))
                  pattern-item
                  [nil (merge {:route :pattern
                               :hops 1
                               :pattern-id pattern-id
                               :routed-count (get routed-counts pattern-id)}
                              (pattern-surface-content
                               (get-in expansion
                                       [:pattern-surfaces pattern-id])))]]
              {:seen (cond-> seen emit-pattern? (conj pattern-id))
               :items (cond-> items
                        emit-pattern? (conj pattern-item)
                        true (conj memory-route))}))
          {:seen #{} :items []}
          expanded))
        ordered-items (into leaves pattern-and-memory-items)]
    (map-indexed
     (fn [index [memory-id route]]
       (cond-> {:offer/id (str "offer/" job-id "/" index)
                :offer/route (:route route)
                :offer/hops (:hops route)}
         memory-id
         (assoc :offer/memory-id memory-id)
         (:pattern-id route)
         (assoc :offer/pattern-id (:pattern-id route)
                :offer/routed-count (:routed-count route))
         (:offer/pattern-hook route)
         (assoc :offer/pattern-hook (:offer/pattern-hook route))
         (:offer/pattern-body route)
         (assoc :offer/pattern-body (:offer/pattern-body route))
         (:offer/pattern-content route)
         (assoc :offer/pattern-content (:offer/pattern-content route))
         (:offer/name route)
         (assoc :offer/name (:offer/name route))
         (:offer/hook route)
         (assoc :offer/hook (:offer/hook route))
         enabled?
         (assoc :offer/patterns-per-problem (:patterns-per-problem expansion)
                :offer/cascade-cap (:cap expansion)
                :offer/cascade-truncated? (:truncated? expansion)
                :offer/cascade-expanded-available
                (:expanded-available expansion))
         (and enabled? (:pattern route))
         (assoc :offer/via-pattern (:pattern route))))
     ordered-items)))

(defn- memory-offers [state config]
  (->> (:steps state)
       (keep (fn [{:keys [tool result]}]
               (when (#{:dispatch-solver :dispatch-student-fresh} tool)
                 (:memory-offers result))))
       (mapcat identity)
       (mapcat #(cascade-receipt-offers % config))
       vec))

(defn- require-mission [handle opts]
  (if (and (string? (:mission opts)) (not (str/blank? (:mission opts))))
    nil
    (failure handle :mission-absent
             "dispatch requires a non-blank :mission")))

(defn- initial-handle [config]
  (let [evidence-store (or (:evidence-store config)
                           (f1b/make-futon1b-backend
                            (:evidence-store-url config)))
        peripheral (or (:peripheral config) (problem/make-problem))
        mode (:mode config)
        context {:session-id (:session-id config)
                 :problem-id (:problem-id config)
                 :cycle/mode mode
                 :cycle/deposit-state (or (:deposit-state config) :n/a)
                 :evidence-store evidence-store
                 :harness-repo (:harness-repo config)
                 :lean-repo (:lean-repo config)
                 :agency-endpoint (:agency-endpoint config)
                 :authorization-revision (:authorization-revision config)
                 :authorization-output (:authorization-output config)
                 :conductor (:conductor config)
                 :author (get-in config [:conductor :agent])}
        started (runner/start peripheral context)
        handle {:ok true :peripheral peripheral :state (:state started)
                :log [] :deposits [] :config config}]
    (if (:ok started)
      handle
      (failure handle (or (:error/code started) :start-refused)
               (or (:error/message started) "problem peripheral refused start")))))

(defn open-frame!
  "Open and register a frame, returning a handle at :guided-solve.

   Tests may supply :peripheral and :evidence-store in config; production uses
   problem/make-problem and the configured Futon1b backend."
  [config]
  (try
    (let [
          ;; First-production findings (frame-3 mis-open, 2026-08-16): a nil
          ;; mode opened a malformed cycle; :deposit-state and :conductor were
          ;; never threaded, silently disabling deposit-state validity and
          ;; the atomic dispatch+park integration. Validate and thread.
          mode (:mode config)
          _ (when-not (contains? #{:store-mode :harness-mode} mode)
              (throw (ex-info (str "open-frame! requires :mode :store-mode or "
                                   ":harness-mode; got " (pr-str mode))
                              {:error/code :invalid-frame-mode})))
          initial (initial-handle config)]
      (if (false? (:ok initial))
        initial
        (let [{h1 :handle begin :result}
              (saved-step initial :begin-problem-cycle
                          [(:conductor config) (:problem-id config)])
              {h2 :handle registration :result}
              (saved-step h1 :read-registration [(:registration-path config)])
              {h3 :handle validation :result}
              (saved-step h2 :validate-registration [registration])
              {h4 :handle snapshot :result} (saved-step h3 :snapshot-store [])
              {h5 :handle frozen :result} (saved-step h4 :freeze-stratum [])
              checkout (merge (:checkout config) {:problem (:problem-id config)})
              {h6 :handle assignment :result}
              (saved-step h5 :assign-checkouts [checkout])
              solver (get-in assignment [:environment-checkouts :solver])
              register-payload
              {:registration (or (:registration validation) registration)
               :store-snapshot snapshot
               :stratum-frozen-at (:cycle/stratum-frozen-at frozen)
               :environment-revision (:base-revision solver)
               :harness-revision (:harness-revision begin)
               :environment-checkouts (:environment-checkouts assignment)}
              {h7 :handle} (advance h6 register-payload)
              frame (:frame config)
              {h8 :handle}
              (saved-step h7 :emit-frame
                          [{:scaffold-path (str (:scaffold-path frame))
                            :closing-path (str (:closing-path frame))
                            :containment-witness-path
                            (some-> (:witness-path frame) str)
                            :containment-claimed? (some? (:witness-path frame))}])
              {h9 :handle} (advance h8 {})
              opened (assoc h9 :cycle-id (get-in h9 [:state :current-cycle-id]))
              conductor (:conductor config)
              agent-id (when (map? conductor) (:agent conductor))
              session-id (when (map? conductor) (:session conductor))]
          (if (and agent-id session-id (not (false? (:ok opened))))
            (let [installed (binding/install! agent-id session-id opened)]
              (if (:ok installed)
                opened
                (failure opened (:error/code installed)
                         "problem conductor binding refused" installed)))
            opened))))
    (catch Throwable t
      (failure {:ok false :peripheral nil :state nil :log [] :deposits []
                :config config}
               :open-frame-threw (.getMessage t)))))

(defn- dispatch! [handle tool opts packet]
  (if-let [refusal (require-mission handle opts)]
    refusal
    (let [parked-version (inc (binding/handle-version handle))
          opts (assoc (or opts {})
                      :conductor/cycle-id (:cycle-id handle)
                      :conductor/version parked-version)]
      (:handle (saved-step handle tool [opts packet])))))

(defn dispatch-solver! [handle opts packet]
  (dispatch! handle :dispatch-solver opts packet))

(defn guide-solver!
  ([handle _opts _packet]
   (failure handle :guidance-type-absent
            "guide-solver requires a typed-bell performative"))
  ([handle bell-type opts packet]
   (if (contains? prereg/guidance-bell-types bell-type)
     (dispatch! handle :guide-solver (assoc (or opts {}) :bell-type bell-type)
                packet)
     (failure handle :guidance-type-invalid
              "guide-solver requires a valid typed-bell performative"
              {:bell-type bell-type}))))

(defn- recorded-results [state tool]
  (->> (:steps state)
       (keep (fn [step]
               (when (= tool (:tool step)) (:result step))))
       vec))

(defn dispatch-student! [handle opts packet]
  (let [handle (if (= :promote-solver (get-in handle [:state :current-phase]))
                 (:handle
                  (advance handle
                           {:promotion-result
                            (recorded-results (:state handle)
                                              :promote-artifact)}))
                 handle)
        student-seat (get-in handle
                             [:state :cycle/outputs :registration
                              :reg/student-seat])]
    ;; Machine-owned registration wins over a caller-supplied recipient.
    (dispatch! handle :dispatch-student-fresh
               (assoc (or opts {}) :to student-seat) packet)))

(defn- resolve-scribe-card-path
  [pinned-blob]
  (when (and (string? pinned-blob) (re-matches #"[0-9a-f]{40}" pinned-blob))
    (let [{root-exit :exit root-out :out}
          (shell/sh "git" "rev-parse" "--show-toplevel")
          root (some-> root-out str/trim not-empty)]
      (when (and (zero? root-exit) root)
        (let [{tree-exit :exit tree-out :out}
              (shell/sh "git" "-C" root "ls-tree" "-r" "HEAD")
              matches (when (zero? tree-exit)
                        (->> (str/split-lines tree-out)
                             (keep (fn [line]
                                     (let [[metadata path] (str/split line #"\t" 2)
                                           [_ kind blob] (str/split metadata #"\s+")]
                                       (when (and (= "blob" kind)
                                                  (= pinned-blob blob)
                                                  (string? path)
                                                  (str/includes? path "/role-cards/"))
                                         path))))
                             vec))]
          (when (= 1 (count matches))
            (let [card (io/file root (first matches))]
              (when (.isFile card) (.getCanonicalPath card)))))))))

(defn- recorded-job-ids [state tool]
  (->> (:steps state)
       (keep (fn [step]
               (when (= tool (:tool step))
                 (get-in step [:result :job-id]))))
       vec))

(defn dispatch-scribe!
  "Dispatch the registered scribe with machine-owned cycle references."
  [handle opts packet]
  (let [state (:state handle)
        pinned-blob (get-in state [:cycle/outputs :registration
                                   :reg/role-cards :scribe])
        scribe-card-path (resolve-scribe-card-path pinned-blob)]
    (if-not scribe-card-path
      (failure handle :scribe-card-unresolved
               "registered scribe role card could not be uniquely resolved"
               {:pinned-blob pinned-blob})
      (let [context {:problem-id (get-in handle [:config :problem-id])
                     :cycle-id (:cycle-id handle)
                     :solver-job-ids (recorded-job-ids state :dispatch-solver)
                     :student-job-ids (recorded-job-ids state
                                                        :dispatch-student-fresh)
                     :scribe-card-path scribe-card-path}]
        (dispatch! handle :dispatch-scribe (merge (or opts {}) context) packet)))))

(defn promote-artifact!
  "Record one promotion through the phase-gated problem tool."
  [handle opts]
  (:handle (saved-step handle :promote-artifact [(or opts {})])))

(defn record-scribe-lanes!
  "Record one scribe lane report through the phase-gated problem tool."
  [handle opts]
  (:handle (saved-step handle :record-scribe-lanes [(or opts {})])))

(defn record-solver-attempt! [handle attempt extra-outputs]
  (try
    (:handle
     (advance handle
              (merge {:solver-attempt attempt
                      :memory-offers (memory-offers (:state handle)
                                                    (:config handle))}
                     extra-outputs)))
    (catch Throwable t
      (failure handle :record-solver-threw (.getMessage t)))))

(defn deposit! [handle payload]
  (try
    (let [{h1 :handle receipt :result}
          (saved-step handle :write-substrate [payload])]
      (if (false? (:ok h1))
        h1
        (let [memory-id (:memory-id receipt)
              h1 (update h1 :deposits (fnil conj []) memory-id)
              {h2 :handle}
              (advance h1 {:intervention {:kind :store-write
                                           :memory-id memory-id}})]
          h2)))
    (catch Throwable t
      (failure handle :deposit-threw (.getMessage t)))))

(defn record-students! [handle attempts uses]
  (try
    (:handle (advance handle {:student-attempts (vec attempts)
                              :memory-uses (vec uses)}))
    (catch Throwable t
      (failure handle :record-students-threw (.getMessage t)))))

(defn write-uses!
  "Disposition every memory offer recorded by this cycle."
  [handle]
  (try
    (reduce (fn [h offer-id]
              (if (false? (:ok h))
                (reduced h)
                (:handle (saved-step h :write-use [{:offer-id offer-id}]))))
            handle
            (->> (get-in handle [:state :cycle/outputs :memory-offers])
                 (keep :offer/id)
                 distinct
                 vec))
    (catch Throwable t
      (failure handle :write-uses-threw (.getMessage t)))))

(defn adjudicate! [handle disposition]
  (try
    (let [promotions (vec (or (:promotion-result disposition) []))
          disposition (dissoc disposition :promotion-result)
          {h1 :handle} (saved-step handle :write-disposition [disposition])
          {h2 :handle} (advance h1 {})
          h3 (reduce (fn [h promotion]
                       (if (false? (:ok h))
                         (reduced h)
                         (:handle (saved-step h :promote-artifact [promotion]))))
                     h2 promotions)]
      ;; :promote is an active work phase. Leave it reachable so the guide can
      ;; dispatch the post-adjudication scribe and record its outputs.
      h3)
    (catch Throwable t
      (failure handle :adjudicate-threw (.getMessage t)))))

(defn close! [handle]
  (try
    (let [promotions (recorded-results (:state handle) :promote-artifact)
          h0 (if (= :promote (get-in handle [:state :current-phase]))
               (:handle (advance handle {:promotion-result promotions}))
               handle)
          {h1 :handle measurement :result}
          (saved-step h0 :record-measurement [])
          {h2 :handle} (saved-step h1 :emit-capability-probes [])
          {h3 :handle trace-envelope :result} (saved-step h2 :emit-trace [])
          {h4 :handle validation :result} (saved-step h3 :validate-trace [])
          {h5 :handle} (saved-step h4 :write-authorization [])
          ;; The terminal advance clears the cycle id, so checkpoint immediately
          ;; before it rather than attempting an impossible post-sentinel save.
          {h6 :handle} (checkpoint h5)
          {h7 :handle} (raw-step h6 problem/advance
                                ["apm-conductor"
                                 (:problem-id (:config h6)) {}])
          failures (vec (concat (:producer-failures trace-envelope)
                                (:failures validation)))
          closed (assoc h7
                        :envelope {:measurement measurement
                                   :failures failures
                                   :launchable? (true? (:launchable? validation))}
                        :cycle-id (or (:cycle-id handle)
                                      (get-in handle [:state :current-cycle-id])))]
      (if (and (not (false? (:ok closed)))
               (nil? (get-in closed [:state :current-phase])))
        (assoc closed :analyst-wake
               (safe-analyst-wake! closed))
        (assoc closed :analyst-wake
               {:status :skipped :reason :close-incomplete})))
    (catch Throwable t
      (failure handle :close-threw (.getMessage t)))))

(defn resume
  "Restore a saved cycle version through the peripheral's public load tool.
   A handle is required because it carries the peripheral construction and
   runtime evidence backend; nil is refused rather than guessing either."
  [handle cycle-id version]
  (if-not handle
    (failure {:ok false :peripheral nil :state nil :log [] :deposits []}
             :resume-handle-required
             "resume requires a conductor handle with its peripheral runtime")
    (try
      (let [{loaded :handle restored :result}
            (raw-step handle :problem-load [cycle-id version])]
        (if (false? (:ok loaded))
          loaded
          ;; For a load, the tool result IS the restored state. Retaining the
          ;; runner's wrapper state would embed that state again as the load
          ;; step's result; its runtime evidence-store then points back through
          ;; the emitted load evidence, making the next EDN checkpoint cyclic.
          ;; Install the validated public-tool result, while the conductor log
          ;; remains the takeover record, then checkpoint the restored state.
          (:handle (checkpoint (assoc loaded :state restored)))))
      (catch Throwable t
        (failure handle :resume-threw (.getMessage t))))))

(defn resume-fresh
  "Rebuild the normal peripheral runtime, then load a named saved cycle."
  ([source-handle cycle-id version]
   (resume-fresh source-handle cycle-id version nil))
  ([source-handle cycle-id version conductor-identity]
   (let [config (cond-> (:config source-handle)
                  conductor-identity
                  (update :conductor merge conductor-identity))
         fresh (initial-handle config)]
     (if (false? (:ok fresh)) fresh (resume fresh cycle-id version)))))
