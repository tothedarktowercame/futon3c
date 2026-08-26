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
            [futon3c.substrate.client :as substrate]))

(def ^:private default-memory-cascade-cap 100)

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

(defn- response-edn [response context]
  (if (= 200 (:status response))
    (edn/read-string (:body response))
    (throw (ex-info "memory cascade substrate read failed"
                    (assoc context :status (:status response)
                           :body (:body response))))))

(defn- cascade-get [base path query-params]
  (response-edn
   (http-client/get (str (str/replace base #"/+$" "") path)
                    {:query-params query-params
                     :throw false :timeout 60000})
   {:path path :query-params query-params}))

(defn- cascade-pattern [base pattern-id]
  (let [path (str "/api/alpha/entity/"
                  (java.net.URLEncoder/encode (str pattern-id) "UTF-8"))]
    (response-edn
     (http-client/get (str (str/replace base #"/+$" "") path)
                      {:headers {"Accept" "application/edn"}
                       :throw false :timeout 60000})
     {:path path :pattern-id pattern-id})))

(defn- qualified-name [x]
  (if (keyword? x)
    (if-let [ns (namespace x)] (str ns "/" (name x)) (name x))
    (str x)))

(defn- reviewed-attachment? [edge]
  (and (= "memory/assert" (qualified-name (:hx/type edge)))
       (= "reviewed"
          (qualified-name
           (or (get-in edge [:hx/props :attachment-status])
               (:prop/attachment-status edge))))))

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
;; :invalid-limit (futon1b 999af15, 2026-07-22). This reader asked for 5000
;; from the day it was written, so every live expansion since then — including
;; the nine round-1 registrations that set :reg/memory-cascade-enabled? true —
;; would have thrown before reading a single attachment (D0, 2026-08-26).
;; The `end=` form also ignores `after`, so there is no cursor to page with;
;; a full page is therefore refused rather than silently truncated.
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
  (let [base (or (:evidence-store-url config) (substrate/configured-url))]
    {:attachments-fn
     (fn [endpoint]
       (-> (:hyperedges
            (cascade-get base "/api/alpha/hyperedges"
                         {:end endpoint :type "memory/assert"
                          :limit cascade-hyperedge-page-limit}))
           (complete-page cascade-hyperedge-page-limit {:endpoint endpoint})
           (->> (filter reviewed-attachment?)
                vec)))
     :why-targets-fn
     (fn [pattern-id]
       (->> (:relations
             (cascade-get base "/api/alpha/relations"
                          {:from pattern-id :limit 100}))
            (keep (fn [relation]
                    (when (= "pattern/has-semantic-why"
                             (qualified-name (:relation/type relation)))
                      (or (:relation/to relation) (:relation/dst relation)))))
            distinct
            vec))
     :pattern-fn
     (fn [pattern-id]
       ;; Pattern content is additive. A missing legacy entity must not hide
       ;; the named pattern offer or break an otherwise valid memory cascade.
       (try
         (cascade-pattern base pattern-id)
         (catch Throwable _ nil)))}))

(defn domain-general-pattern-id?
  "True when PATTERN-ID's pre-slash family has no uppercase subject suffix."
  [pattern-id]
  (let [family (first (str/split (str pattern-id) #"/" 2))]
    (not (boolean (re-find #"-[A-Z]{2,}$" family)))))

(defn- pattern-surface-content [surface]
  (let [entity (or (:entity surface) surface)
        props (or (:entity/props entity) (:props entity) entity)
        hook (or (:hook props) (:pattern/hook props))
        body (or (:body props) (:pattern/body props))
        content (when (and (map? props) (seq props)) props)]
    (cond-> {}
      (some? hook) (assoc :offer/pattern-hook hook)
      (some? body) (assoc :offer/pattern-body body)
      content (assoc :offer/pattern-content content))))

(defn expand-memory-cascade
  "Expand surfaced memories through reviewed pattern attachments.

   `attachments-fn` returns memory/assert edges for an endpoint;
   `why-targets-fn` returns authored @why targets for a pattern. The result is
   bounded after cheapest-route deduplication. `:expanded-count` excludes the
   leaf memories already surfaced by retrieval."
  [seed-memory-ids {:keys [attachments-fn why-targets-fn pattern-fn cap]
                    :or {cap default-memory-cascade-cap}}]
  (let [seed-memory-ids (vec (distinct seed-memory-ids))
        seed-memory-set (set seed-memory-ids)
        seed-edges (mapcat attachments-fn seed-memory-ids)
        seed-patterns (vec (distinct (mapcat attachment-patterns seed-edges)))
        ;; Authored why edges form a directed graph. Record the shortest
        ;; distance from any seed pattern.
        why-patterns
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
        why-patterns (apply dissoc why-patterns seed-patterns)
        ;; Co-incidence is exactly pattern -> problem -> pattern. Only the
        ;; original seed patterns initiate it; it does not recursively flood.
        seed-pattern-edges (mapcat attachments-fn seed-patterns)
        seed-problems (vec (distinct (mapcat attachment-problems
                                             seed-pattern-edges)))
        coincident-patterns
        (->> seed-problems
             (mapcat attachments-fn)
             (mapcat attachment-patterns)
             (remove (set seed-patterns))
             distinct
             (map #(vector % 2))
             (into {}))
        pattern-routes
        ;; On equal cost retain the authored why route (the left map). The
        ;; receipt then distinguishes authored structure from an incidental
        ;; co-incidence without claiming a cheaper path.
        (merge-with (fn [a b] (if (<= (:hops a) (:hops b)) a b))
                    (into {} (map (fn [[pattern hops]]
                                    [pattern {:route :why-hop :hops hops
                                              :pattern pattern}])
                                  why-patterns))
                    (into {} (map (fn [[pattern hops]]
                                    [pattern {:route :co-incidence :hops hops
                                              :pattern pattern}])
                                  coincident-patterns)))
        structural
        (for [[pattern route] pattern-routes
              edge (attachments-fn pattern)
              :let [memory-id (attachment-memory-id edge)]
              :when (and memory-id (not (seed-memory-set memory-id)))]
          [memory-id route])
        cheapest
        (reduce (fn [by-memory [memory-id route]]
                  (update by-memory memory-id
                          #(if (or (nil? %) (< (:hops route) (:hops %)))
                             route %)))
                {} structural)
        ordered (->> cheapest
                     (sort-by (fn [[memory-id {:keys [route hops]}]]
                                [hops ({:why-hop 0 :co-incidence 1} route 2)
                                 memory-id]))
                     vec)
        selected (vec (take cap ordered))
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
     :pattern-surfaces pattern-surfaces
     :seed-patterns seed-patterns
     :patterns-per-problem (count seed-patterns)
     :expanded-count (count selected)
     :expanded-available (count ordered)
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
                     (merge (live-cascade-readers config)
                            {:cap (or (:memory-cascade-cap config)
                                      default-memory-cascade-cap)})))
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
