(ns futon3c.agency.logic
  "Relational invariant layer for the Agency subsystem.

   Snapshots the agent registry, peripheral topology, and federation state
   into a core.logic fact database. Expresses structural properties as
   goals and queries for violations.

   This namespace does not implement agency behavior — it checks whether
   the running state satisfies the structural law of the agency subsystem.
   The invariants here are properties of *any* correct agency implementation,
   not assumptions about a specific one.

   Three domains:

   1. Registration integrity — uniqueness, typing, proxy discipline
   2. Invoke routing — consistency between declared route and actual readiness
   3. Hop topology — entry/exit symmetry, reachability, dead-end detection

   Pattern follows portfolio/logic.clj and tickle_logic.clj:
   snapshot → build-db → goals → query-violations."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as registry]))

;; =============================================================================
;; Relations (fact schema)
;; =============================================================================

;; --- Registration facts ---
(pldb/db-rel agento agent-id)
(pldb/db-rel agent-typeo agent-id agent-type)
(pldb/db-rel capabilityo agent-id capability)
(pldb/db-rel typed-ido agent-id id-type)          ; :continuity | :transport | :protocol
(pldb/db-rel proxyo agent-id origin-url)           ; present iff proxy agent
(pldb/db-rel origin-urlo agent-id origin-url)
(pldb/db-rel sessiono agent-id session-id)         ; current session-id (nil omitted)
(pldb/db-rel live-writero agent-id session-id inhabitant-id)
(pldb/db-rel home-pointo agent-id federation-point)
(pldb/db-rel local-pointo federation-point)
(pldb/db-rel ttlo agent-id ttl-ms)                 ; bounded lifecycle

;; --- Federation map facts ---
(pldb/db-rel siteo site-id)
(pldb/db-rel self-urlo site-id url)
(pldb/db-rel peero site-id peer-site peer-url)
(pldb/db-rel roster-agento site-id agent-id)
(pldb/db-rel local-registrationo agent-id site-id)

;; --- Invoke routing facts ---
(pldb/db-rel invoke-routeo agent-id route)         ; :local | :ws | :none
(pldb/db-rel invoke-readyo agent-id)               ; agent is invocable
(pldb/db-rel invoke-diagnostico agent-id diagnostic)
(pldb/db-rel invoke-statuso agent-id status)       ; :idle | :invoking
(pldb/db-rel has-invoke-fno agent-id)              ; has a local invoke-fn

;; --- Federation/liveness probe facts ---
(pldb/db-rel connection-stateo connection-id declared-state channel-state)
(pldb/db-rel remote-healtho agent-id registered-ready? current-ready?)
(pldb/db-rel session-capacityo agent-id session-id capacity-state)
(pldb/db-rel server-accepto server-id listening? accepting? backlog backlog-limit accept-latency-ms latency-bound-ms)

;; --- Peripheral topology facts ---
(pldb/db-rel peripheralo peripheral-id)
(pldb/db-rel peripheral-toolo peripheral-id tool)
(pldb/db-rel entry-seto peripheral-id entry-cond)
(pldb/db-rel exit-seto peripheral-id exit-cond)

;; --- Observed hop facts (from evidence/sessions) ---
(pldb/db-rel observed-hopo from-pid to-pid session-id)

;; =============================================================================
;; Database construction
;; =============================================================================

(defn- add-agent-facts
  "Add facts from a registry snapshot.
   registry-snapshot: {agent-id-value -> agent-record}"
  [db registry-snapshot]
  (reduce-kv
    (fn [db' aid-val agent]
      (let [aid aid-val
            typed-id (:agent/id agent)
            id-type (or (:id/type typed-id) :unknown)
            agent-type (or (:agent/type agent) :unknown)
            capabilities (or (:agent/capabilities agent) [])
            session (:agent/session-id agent)
            ttl (:agent/ttl-ms agent)
            status (or (:agent/status agent) :idle)
            has-fn? (fn? (:agent/invoke-fn agent))
            metadata (or (:agent/metadata agent) {})
            proxy? (:proxy? metadata)
            origin (:origin-url metadata)
            home-point (or (:home-point metadata)
                           (:home-site metadata)
                           (:site metadata)
                           (get metadata "home-point")
                           (get metadata "home-site")
                           (get metadata "site"))]
        (cond-> (-> db'
                    (pldb/db-fact agento aid)
                    (pldb/db-fact agent-typeo aid agent-type)
                    (pldb/db-fact typed-ido aid id-type)
                    (pldb/db-fact invoke-statuso aid status))
          ;; capabilities
          (seq capabilities)
          (as-> db'' (reduce #(pldb/db-fact %1 capabilityo aid %2) db'' capabilities))
          ;; session
          session
          (pldb/db-fact sessiono aid session)
          ;; TTL
          ttl
          (pldb/db-fact ttlo aid ttl)
          ;; home federation point
          home-point
          (pldb/db-fact home-pointo aid home-point)
          ;; invoke-fn
          has-fn?
          (pldb/db-fact has-invoke-fno aid)
          ;; proxy
          proxy?
          (pldb/db-fact proxyo aid (or origin "unknown"))
          ;; origin-url is a separate fact because AG-2 distinguishes a
          ;; federated proxy from a locally-minted remote phantom.
          origin
          (pldb/db-fact origin-urlo aid origin))))
    db
    registry-snapshot))

(defn- add-local-point-facts
  [db local-point]
  (if local-point
    (-> db
        (pldb/db-fact local-pointo local-point)
        (pldb/db-fact siteo local-point))
    db))

(defn- peer-site
  [peer]
  (cond
    (map? peer) (or (:site peer) (:peer-site peer) (:id peer) (:url peer))
    :else peer))

(defn- peer-url
  [peer]
  (cond
    (map? peer) (or (:url peer) (:peer-url peer) (:site peer) (:id peer))
    :else peer))

(defn- local-agent-record?
  [agent]
  (not (true? (get-in agent [:agent/metadata :proxy?]))))

(defn- add-federation-facts
  "Add federation topology and roster facts.

   peers: [{:site :lon :url \"http://...\"} ...] or peer-url strings.
   peer-rosters: {site-id #{agent-id ...}} or {site-id {:agents [...]}}.
   The current registry is also modelled as the local site's roster."
  [db {:keys [registry local-point site self-url peers peer-rosters]}]
  (let [site-id (or site local-point)]
    (cond-> db
      site-id
      (pldb/db-fact siteo site-id)

      (and site-id self-url)
      (pldb/db-fact self-urlo site-id self-url)

      (and site-id (seq peers))
      (as-> db' (reduce (fn [acc peer]
                          (let [psite (peer-site peer)
                                purl (peer-url peer)]
                            (if (and psite purl)
                              (-> acc
                                  (pldb/db-fact siteo psite)
                                  (pldb/db-fact peero site-id psite purl))
                              acc)))
                        db'
                        peers))

      (and site-id registry)
      (as-> db' (reduce-kv (fn [acc aid agent]
                             (cond-> (pldb/db-fact acc roster-agento site-id aid)
                               (local-agent-record? agent)
                               (pldb/db-fact local-registrationo aid site-id)))
                           db'
                           registry))

      (seq peer-rosters)
      (as-> db' (reduce-kv (fn [acc peer-site-id roster]
                             (let [agents (cond
                                            (map? roster) (or (:agents roster)
                                                              (:agent-ids roster)
                                                              (:roster roster)
                                                              [])
                                            :else roster)]
                               (reduce (fn [acc' aid]
                                         (-> acc'
                                             (pldb/db-fact siteo peer-site-id)
                                             (pldb/db-fact roster-agento peer-site-id aid)))
                                       acc
                                       agents)))
                           db'
                           peer-rosters)))))

(defn- add-routing-facts
  "Add invoke routing facts.
   routing-info: {agent-id-value -> {:invoke-route :invoke-ready? ...}}"
  [db routing-info]
  (reduce-kv
    (fn [db' aid-val info]
      (let [route (or (:invoke-route info) :none)
            diagnostic (or (:invoke-diagnostic info)
                           (:diagnostic info))]
        (cond-> (pldb/db-fact db' invoke-routeo aid-val route)
          (:invoke-ready? info)
          (pldb/db-fact invoke-readyo aid-val)
          diagnostic
          (pldb/db-fact invoke-diagnostico aid-val diagnostic))))
    db
    routing-info))

(defn- add-connection-facts
  "Add connection state facts.
   connections: [{:connection-id id :declared-state :connected :channel-state :live} ...]"
  [db connections]
  (reduce
    (fn [db' {:keys [connection-id declared-state channel-state]}]
      (if (and connection-id declared-state channel-state)
        (pldb/db-fact db' connection-stateo connection-id declared-state channel-state)
        db'))
    db
    connections))

(defn- add-remote-health-facts
  "Add remote health facts.
   remote-health: [{:agent-id aid :registered-ready? bool :current-ready? bool} ...]"
  [db remote-health]
  (reduce
    (fn [db' {:keys [agent-id registered-ready? current-ready?]}]
      (if agent-id
        (pldb/db-fact db' remote-healtho agent-id (boolean registered-ready?) (boolean current-ready?))
        db'))
    db
    remote-health))

(defn- add-session-capacity-facts
  "Add session capacity facts.
   session-capacity: [{:agent-id aid :session-id sid :capacity-state :ok|:context-exhausted} ...]"
  [db session-capacity]
  (reduce
    (fn [db' {:keys [agent-id session-id capacity-state]}]
      (if (and agent-id session-id capacity-state)
        (pldb/db-fact db' session-capacityo agent-id session-id capacity-state)
        db'))
    db
    session-capacity))

(defn- add-server-accept-facts
  "Add HTTP/server accept-loop facts.
   server-accept: [{:server-id id :listening? bool :accepting? bool
                    :backlog n :backlog-limit n :accept-latency-ms n
                    :latency-bound-ms n} ...]"
  [db server-accept]
  (reduce
    (fn [db' {:keys [server-id listening? accepting? backlog backlog-limit
                     accept-latency-ms latency-bound-ms]}]
      (if server-id
        (pldb/db-fact db' server-accepto server-id (boolean listening?) (boolean accepting?)
                      (long (or backlog 0))
                      (long (or backlog-limit Long/MAX_VALUE))
                      (long (or accept-latency-ms 0))
                      (long (or latency-bound-ms Long/MAX_VALUE)))
        db'))
    db
    server-accept))

(defn- add-peripheral-facts
  "Add peripheral topology from peripherals.edn spec.
   peripheral-specs: {:peripherals {pid -> PeripheralSpec}}"
  [db peripheral-specs]
  (let [specs (or (:peripherals peripheral-specs) peripheral-specs)]
    (reduce-kv
      (fn [db' pid spec]
        (let [tools (or (:peripheral/tools spec) #{})
              entry (or (:peripheral/entry spec) #{})
              exit (or (:peripheral/exit spec) #{})]
          (as-> (pldb/db-fact db' peripheralo pid) db''
            (reduce #(pldb/db-fact %1 peripheral-toolo pid %2) db'' tools)
            (reduce #(pldb/db-fact %1 entry-seto pid %2) db'' entry)
            (reduce #(pldb/db-fact %1 exit-seto pid %2) db'' exit))))
      db
      specs)))

(defn- add-hop-facts
  "Add observed hop facts from session evidence.
   hops: [{:from pid :to pid :session-id str} ...]"
  [db hops]
  (reduce
    (fn [db' {:keys [from to session-id]}]
      (pldb/db-fact db' observed-hopo from to (or session-id "unknown")))
    db
    hops))

(defn- add-live-writer-facts
  "Add live session writer facts.
   live-writers: [{:agent-id aid :session-id sid :inhabitant-id pid-or-token} ...]"
  [db live-writers]
  (reduce
    (fn [db' {:keys [agent-id session-id inhabitant-id]}]
      (if (and agent-id session-id inhabitant-id)
        (pldb/db-fact db' live-writero agent-id session-id inhabitant-id)
        db'))
    db
    live-writers))

(defn build-db
  "Build a logic database from agency state snapshots.

   Takes a map with:
     :registry       — {agent-id-value -> agent-record} (from @reg/!registry)
     :routing        — {agent-id-value -> routing-info} (optional)
     :local-point    — current federation point/site id (optional)
     :site           — current federation site id; defaults to :local-point (optional)
     :self-url       — current site's URL (optional)
     :peers          — peer site/url entries (optional)
     :peer-rosters   — {site-id -> roster} for remote peer snapshots (optional)
     :live-writers   — [{:agent-id :session-id :inhabitant-id}] (optional)
     :connections    — [{:connection-id :declared-state :channel-state}] (optional)
     :remote-health  — [{:agent-id :registered-ready? :current-ready?}] (optional)
     :session-capacity — [{:agent-id :session-id :capacity-state}] (optional)
     :server-accept  — [{:server-id :listening? :accepting? :backlog ...}] (optional)
     :peripherals    — peripheral specs from peripherals.edn (optional)
     :observed-hops  — [{:from :to :session-id}] (optional)"
  [{:keys [registry routing local-point live-writers connections remote-health
           session-capacity server-accept peripherals observed-hops]
    :as snapshot}]
  (cond-> (pldb/db)
    registry       (add-agent-facts registry)
    routing        (add-routing-facts routing)
    local-point    (add-local-point-facts local-point)
    (or (:site snapshot) (:self-url snapshot) (:peers snapshot) (:peer-rosters snapshot))
    (add-federation-facts snapshot)
    live-writers   (add-live-writer-facts live-writers)
    connections    (add-connection-facts connections)
    remote-health  (add-remote-health-facts remote-health)
    session-capacity (add-session-capacity-facts session-capacity)
    server-accept  (add-server-accept-facts server-accept)
    peripherals    (add-peripheral-facts peripherals)
    observed-hops  (add-hop-facts observed-hops)))

;; =============================================================================
;; Goals — structural properties
;; =============================================================================

;; --- Registration ---

(defn agent-has-typed-ido
  "Agent has a valid id-type (not :unknown)."
  [aid]
  (l/fresh [id-type]
    (l/all
      (typed-ido aid id-type)
      (l/!= id-type :unknown))))

(defn agent-is-proxyo
  "Agent is a proxy (federation mirror)."
  [aid]
  (l/fresh [origin]
    (proxyo aid origin)))

(defn agent-is-localo
  "Agent is not a proxy."
  [aid]
  (l/all
    (agento aid)
    (l/project [aid]
      (l/nafc agent-is-proxyo aid))))

(defn remote-homed-local-phantomo
  "A remote-homed agent is being run as a non-proxy local agent."
  [aid]
  (l/fresh [home local]
    (l/all
      (agento aid)
      (agent-is-localo aid)
      (home-pointo aid home)
      (local-pointo local)
      (l/!= home local)
      (invoke-routeo aid :local))))

(defn ag-1-session-collisiono
  "AG-1: two distinct agents must not share a session-id."
  [a1 a2 sid]
  (l/all
    (sessiono a1 sid)
    (sessiono a2 sid)
    (l/!= a1 a2)))

(defn ag-1-live-writer-collisiono
  "AG-1: a session-id must not have two live writer inhabitants."
  [w1 w2 sid]
  (l/fresh [a1 a2]
    (l/all
      (live-writero a1 sid w1)
      (live-writero a2 sid w2)
      (l/!= w1 w2))))

(defn ag-2-phantomo
  "AG-2: remote-home/local-route phantom violation."
  [aid]
  (remote-homed-local-phantomo aid))

(defn missing-from-peer-rostero
  "A box-local agent is absent from a configured peer's roster."
  [aid site peer-site]
  (l/all
    (local-registrationo aid site)
    (l/fresh [peer-url]
      (peero site peer-site peer-url))
    (l/project [peer-site aid]
      (l/nafc roster-agento peer-site aid))))

(defn ag-3-4-unreachable-connectedo
  "AG-3/AG-4: consolidated liveness honesty violation.

   AG-3 covers declared connected channels with dead underlying transport.
   AG-4 covers registration-frozen remote health. Both are one DERIVE-level
   liveness invariant: modeled reachability must reflect current reachability."
  [kind id]
  (l/conde
    [(l/fresh [channel-state]
       (l/all
         (connection-stateo id :connected channel-state)
         (l/!= channel-state :live)
         (l/== kind :dead-channel)))]
    [(l/all
       (remote-healtho id true false)
       (l/== kind :stale-remote-health))]))

(defn ag-5-invoke-readiness-gapo
  "AG-5: route :none requires a diagnostic explaining non-readiness."
  [aid]
  (l/all
    (invoke-routeo aid :none)
    (l/project [aid]
      (l/nafc (fn [a] (l/fresh [diagnostic] (invoke-diagnostico a diagnostic))) aid))))

(defn ag-6-exhausted-sessiono
  "AG-6: context-exhausted sessions must be reset to fresh continuity."
  [aid sid]
  (session-capacityo aid sid :context-exhausted))

(defn ag-7-unaccepting-servero
  "AG-7 relational part: a listening server fact says it is not accepting."
  [server-id]
  (l/fresh [backlog backlog-limit latency bound]
    (server-accepto server-id true false backlog backlog-limit latency bound)))

;; --- Invoke routing ---

(defn route-consistento
  "Agent's invoke route is consistent with its invoke-fn presence.
   If agent has a local invoke-fn, route should be :local.
   If route is :local, agent should have invoke-fn."
  [aid]
  (l/conde
    [(has-invoke-fno aid) (invoke-routeo aid :local)]
    [(l/nafc has-invoke-fno aid) (invoke-routeo aid :ws)]
    [(l/nafc has-invoke-fno aid) (invoke-routeo aid :none)]))

(defn invoking-has-timestampo
  "Agent in :invoking status should be detectable.
   (Timestamp check is structural — we just verify the status is known.)"
  [aid]
  (invoke-statuso aid :invoking))

;; --- Hop topology ---

(defn entry-allows-fromo
  "Target peripheral allows entry from source peripheral.
   True if target has :from-any OR :from-<source> in its entry set."
  [target-pid source-pid]
  (l/conde
    [(entry-seto target-pid :from-any)]
    [(l/project [source-pid]
       (entry-seto target-pid (keyword (str "from-" (name source-pid)))))]))

(defn hop-topology-valido
  "An observed hop respects the declared topology."
  [from-pid to-pid]
  (l/all
    (peripheralo from-pid)
    (peripheralo to-pid)
    (entry-allows-fromo to-pid from-pid)))

;; =============================================================================
;; Queries — detect violations
;; =============================================================================

;; --- Registration integrity ---

(defn query-untyped-agents
  "Agents whose id-type is :unknown (missing TypedAgentId)."
  [db]
  (pldb/with-db db
    (l/run* [aid]
      (agento aid)
      (typed-ido aid :unknown))))

(defn query-duplicate-sessions
  "Pairs of agents sharing the same session-id.
   Returns [[aid1 aid2 session-id] ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [a1 a2 sid]
        (ag-1-session-collisiono a1 a2 sid)
        ;; canonical ordering to avoid [a,b] + [b,a] duplicates
        (l/project [a1 a2]
          (l/== (neg? (compare (str a1) (str a2))) true))
        (l/== q [a1 a2 sid])))))

(defn query-duplicate-live-writers
  "Pairs of live inhabitants writing the same session-id.
   Returns [[inhabitant1 inhabitant2 session-id] ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [a1 a2 sid w1 w2]
        (ag-1-live-writer-collisiono w1 w2 sid)
        (live-writero a1 sid w1)
        (live-writero a2 sid w2)
        (l/project [w1 w2]
          (l/== (neg? (compare (str w1) (str w2))) true))
        (l/== q [w1 w2 sid])))))

(defn query-remote-local-phantoms
  "Remote-homed agents that are registered as non-proxy local invocations.
   Returns [{:agent-id aid :home-point home :local-point local} ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [aid home local]
        (remote-homed-local-phantomo aid)
        (home-pointo aid home)
        (local-pointo local)
        (l/== q {:agent-id aid :home-point home :local-point local})))))

(defn query-unpropagated-agents
  "Box-local agents absent from configured peer rosters.
   Returns [{:agent-id aid :site site :missing-peer peer-site :peer-url url} ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [aid site peer-site peer-url]
        (missing-from-peer-rostero aid site peer-site)
        (peero site peer-site peer-url)
        (l/== q {:agent-id aid
                 :site site
                 :missing-peer peer-site
                 :peer-url peer-url})))))

(defn query-proxy-shadowing-local
  "Proxy agents that share an agent-id with a local agent.
   This should never happen — federation non-resurrection (I-I).
   Returns [[proxy-aid origin-url] ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [aid origin]
        ;; This checks for an agent-id that appears as both proxy and local.
        ;; Since our fact schema uses agent-id-value as the key, a single ID
        ;; can't be both proxy and non-proxy simultaneously in the registry.
        ;; But federation bugs could create stale proxy entries. Check for
        ;; proxies whose origin is unreachable or self-referential.
        (proxyo aid origin)
        (l/== q [aid origin])))))

;; --- Invoke routing ---

(defn query-route-inconsistencies
  "Agents whose invoke route doesn't match their invoke-fn presence.
   Returns [{:agent-id :has-fn? :route} ...]."
  [db]
  (let [;; Agents with invoke-fn but not routed :local
        fn-not-local
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [aid route]
              (has-invoke-fno aid)
              (invoke-routeo aid route)
              (l/!= route :local)
              (l/== q [aid :has-fn route]))))
        ;; Agents routed :local but no invoke-fn
        local-no-fn
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [aid]
              (invoke-routeo aid :local)
              (l/nafc has-invoke-fno aid)
              (l/== q [aid :no-fn :local]))))]
    (mapv (fn [[aid fn-status route]]
            {:agent-id aid :has-fn? (= fn-status :has-fn) :route route})
          (concat fn-not-local local-no-fn))))

(defn query-dead-connected-channels
  "AG-3: connections declared :connected whose underlying channel is not :live.
   Returns [{:connection-id id :declared-state :connected :channel-state state} ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [cid channel-state]
        (ag-3-4-unreachable-connectedo :dead-channel cid)
        (connection-stateo cid :connected channel-state)
        (l/== q {:connection-id cid
                 :declared-state :connected
                 :channel-state channel-state})))))

(defn query-stale-remote-health
  "AG-4: remote target was registered as reachable but is currently not reachable."
  [db]
  (pldb/with-db db
    (l/run* [aid]
      (ag-3-4-unreachable-connectedo :stale-remote-health aid))))

(defn query-invoke-readiness-gaps
  "AG-5: route :none must carry a diagnostic explaining why invoke is not ready."
  [db]
  (pldb/with-db db
    (l/run* [aid]
      (ag-5-invoke-readiness-gapo aid))))

(defn query-exhausted-sessions
  "AG-6: context-exhausted sessions must be reset to fresh continuity."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [aid sid]
        (ag-6-exhausted-sessiono aid sid)
        (l/== q {:agent-id aid :session-id sid :capacity-state :context-exhausted})))))

(defn query-unaccepting-servers
  "AG-7: a listening server must be accepting within bounds and not saturating backlog."
  [db]
  (let [rows (pldb/with-db db
               (l/run* [q]
                 (l/fresh [sid listening? accepting? backlog backlog-limit latency bound]
                   (server-accepto sid listening? accepting? backlog backlog-limit latency bound)
                   (l/== q {:server-id sid
                            :listening? listening?
                            :accepting? accepting?
                            :backlog backlog
                            :backlog-limit backlog-limit
                            :accept-latency-ms latency
                            :latency-bound-ms bound}))))]
    (->> rows
         (filter (fn [{:keys [listening? accepting? backlog backlog-limit
                              accept-latency-ms latency-bound-ms]}]
                   (and listening?
                        (or (not accepting?)
                            (>= backlog backlog-limit)
                            (> accept-latency-ms latency-bound-ms)))))
         vec)))

(defn query-invoking-without-idle-path
  "Agents stuck in :invoking status (informational — needs timestamp
   comparison to determine if truly stuck, which is outside logic scope)."
  [db]
  (pldb/with-db db
    (l/run* [aid]
      (invoke-statuso aid :invoking))))

;; --- Hop topology ---

(defn query-dead-end-peripherals
  "Peripherals with no exit conditions — agent would be stuck."
  [db]
  (pldb/with-db db
    (l/run* [pid]
      (peripheralo pid)
      (l/nafc (fn [p] (l/fresh [ec] (exit-seto p ec))) pid))))

(defn query-entry-exit-asymmetry
  "Peripheral pairs where A lists B-related exit but B doesn't allow entry from A.
   Checks: if A has exit :hop-B, does B have :from-A or :from-any in entry?
   Returns [[from-pid to-pid] ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [from-pid to-pid exit-cond]
        (peripheralo from-pid)
        (exit-seto from-pid exit-cond)
        ;; Only check :hop-X exits (these imply a destination)
        (l/project [exit-cond]
          (let [exit-name (name exit-cond)]
            (if (.startsWith exit-name "hop-")
              (let [target-name (subs exit-name 4)
                    target-kw (keyword target-name)]
                (l/all
                  (peripheralo target-kw)
                  (l/== to-pid target-kw)
                  (l/nafc entry-allows-fromo target-kw from-pid)))
              l/fail)))
        (l/== q [from-pid to-pid])))))

(defn query-invalid-observed-hops
  "Observed hops that violate the declared topology.
   Returns [{:from :to :session-id} ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [from-pid to-pid sid]
        (observed-hopo from-pid to-pid sid)
        (l/nafc hop-topology-valido from-pid to-pid)
        (l/== q {:from from-pid :to to-pid :session-id sid})))))

;; =============================================================================
;; Aggregate violation query
;; =============================================================================

(defn query-violations
  "Run all invariant checks against a logic database.
   Returns a map of violation category → violations.
   Empty vectors mean the invariant holds."
  [db]
  {:untyped-agents         (query-untyped-agents db)
   :duplicate-sessions     (query-duplicate-sessions db)
   :duplicate-live-writers (query-duplicate-live-writers db)
   :remote-local-phantoms  (query-remote-local-phantoms db)
   :unpropagated-agents    (query-unpropagated-agents db)
   :proxy-agents           (query-proxy-shadowing-local db)
   :route-inconsistencies  (query-route-inconsistencies db)
   :dead-connected-channels (query-dead-connected-channels db)
   :stale-remote-health    (query-stale-remote-health db)
   :invoke-readiness-gaps  (query-invoke-readiness-gaps db)
   :exhausted-sessions     (query-exhausted-sessions db)
   :unaccepting-servers    (query-unaccepting-servers db)
   :agents-invoking        (query-invoking-without-idle-path db)
   :dead-end-peripherals   (query-dead-end-peripherals db)
   :entry-exit-asymmetry   (query-entry-exit-asymmetry db)
   :invalid-observed-hops  (query-invalid-observed-hops db)})

(defn find-phantoms
  "CP-A finder: remote-homed agents incorrectly present as local phantoms."
  [db]
  (query-remote-local-phantoms db))

(defn find-unpropagated
  "CP-A finder: box-local agents missing from peer rosters."
  [db]
  (query-unpropagated-agents db))

(defn find-session-collisions
  "CP-A finder: AG-1 session-id and live-writer collisions."
  [db]
  {:duplicate-sessions (query-duplicate-sessions db)
   :duplicate-live-writers (query-duplicate-live-writers db)})

(defn find-unreachable-connected
  "CP-A finder: consolidated AG-3/AG-4 liveness violations."
  [db]
  (vec
    (concat
      (map #(assoc % :kind :dead-channel)
           (query-dead-connected-channels db))
      (map (fn [agent-id]
             {:kind :stale-remote-health
              :agent-id agent-id})
           (query-stale-remote-health db)))))

(def ^:private informational-keys
  "Keys that are informational, not violations.
   proxy-agents lists all proxies; agents-invoking lists currently-invoking agents."
  #{:proxy-agents :agents-invoking})

(defn violations?
  "True if any non-informational invariant has violations."
  [violations]
  (some (fn [[k v]] (and (not (informational-keys k)) (seq v))) violations))

;; =============================================================================
;; Convenience: snapshot from live system
;; =============================================================================

(defn snapshot-registry
  "Take a snapshot of the live registry for invariant checking.
   Returns a map suitable for build-db."
  [registry-atom]
  {:registry @registry-atom})

(defn snapshot->db
  "Build a logic database from a plain data snapshot.
   Tests use this entrypoint to avoid depending on live Agency boxes."
  [snapshot]
  (build-db snapshot))

(defn live-snapshot
  "Snapshot live registry/federation state without mutating the runtime.

   This intentionally mirrors tickle_logic's snapshot discipline: read the
   registry/federation state into plain data, then let build-db do the MAP."
  []
  (let [status (registry/registry-status)
        routing (into {}
                      (map (fn [[aid info]]
                             [aid (select-keys info
                                               [:invoke-route
                                                :invoke-ready?
                                                :invoke-diagnostic
                                                :diagnostic])])
                           (:agents status)))
        self-url (federation/self-url)]
    {:registry @registry/!registry
     :routing routing
     :site (or self-url :local)
     :local-point (or self-url :local)
     :self-url self-url
     :peers (mapv (fn [peer]
                    (if (map? peer)
                      peer
                      {:site peer :url peer}))
                  (federation/peers))}))

(defn build-live-db
  "Build a logic database from the current live registry/federation snapshot."
  []
  (snapshot->db (live-snapshot)))

(defn check-registry
  "One-shot: snapshot registry → build db → query violations."
  [registry-atom]
  (-> (snapshot-registry registry-atom)
      build-db
      query-violations))
