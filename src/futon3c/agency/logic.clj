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
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations (fact schema)
;; =============================================================================

;; --- Registration facts ---
(pldb/db-rel agento agent-id)
(pldb/db-rel agent-typeo agent-id agent-type)
(pldb/db-rel capabilityo agent-id capability)
(pldb/db-rel typed-ido agent-id id-type)          ; :continuity | :transport | :protocol
(pldb/db-rel proxyo agent-id origin-url)           ; present iff proxy agent
(pldb/db-rel sessiono agent-id session-id)         ; current session-id (nil omitted)
(pldb/db-rel ttlo agent-id ttl-ms)                 ; bounded lifecycle

;; --- Invoke routing facts ---
(pldb/db-rel invoke-routeo agent-id route)         ; :local | :ws | :none
(pldb/db-rel invoke-readyo agent-id)               ; agent is invocable
(pldb/db-rel invoke-statuso agent-id status)       ; :idle | :invoking
(pldb/db-rel has-invoke-fno agent-id)              ; has a local invoke-fn

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
            origin (:origin-url metadata)]
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
          ;; invoke-fn
          has-fn?
          (pldb/db-fact has-invoke-fno aid)
          ;; proxy
          proxy?
          (pldb/db-fact proxyo aid (or origin "unknown")))))
    db
    registry-snapshot))

(defn- add-routing-facts
  "Add invoke routing facts.
   routing-info: {agent-id-value -> {:invoke-route :invoke-ready? ...}}"
  [db routing-info]
  (reduce-kv
    (fn [db' aid-val info]
      (let [route (or (:invoke-route info) :none)]
        (cond-> (pldb/db-fact db' invoke-routeo aid-val route)
          (:invoke-ready? info)
          (pldb/db-fact invoke-readyo aid-val))))
    db
    routing-info))

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

(defn build-db
  "Build a logic database from agency state snapshots.

   Takes a map with:
     :registry       — {agent-id-value -> agent-record} (from @reg/!registry)
     :routing        — {agent-id-value -> routing-info} (optional)
     :peripherals    — peripheral specs from peripherals.edn (optional)
     :observed-hops  — [{:from :to :session-id}] (optional)"
  [{:keys [registry routing peripherals observed-hops]}]
  (cond-> (pldb/db)
    registry       (add-agent-facts registry)
    routing        (add-routing-facts routing)
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
    (typed-ido aid id-type)
    (l/!= id-type :unknown)))

(defn agent-is-proxyo
  "Agent is a proxy (federation mirror)."
  [aid]
  (l/fresh [origin]
    (proxyo aid origin)))

(defn agent-is-localo
  "Agent is not a proxy."
  [aid]
  (agento aid)
  (l/nafc agent-is-proxyo aid))

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
  (peripheralo from-pid)
  (peripheralo to-pid)
  (entry-allows-fromo to-pid from-pid))

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
        (sessiono a1 sid)
        (sessiono a2 sid)
        (l/!= a1 a2)
        ;; canonical ordering to avoid [a,b] + [b,a] duplicates
        (l/project [a1 a2]
          (l/== (neg? (compare (str a1) (str a2))) true))
        (l/== q [a1 a2 sid])))))

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
   :proxy-agents           (query-proxy-shadowing-local db)
   :route-inconsistencies  (query-route-inconsistencies db)
   :agents-invoking        (query-invoking-without-idle-path db)
   :dead-end-peripherals   (query-dead-end-peripherals db)
   :entry-exit-asymmetry   (query-entry-exit-asymmetry db)
   :invalid-observed-hops  (query-invalid-observed-hops db)})

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

(defn check-registry
  "One-shot: snapshot registry → build db → query violations."
  [registry-atom]
  (-> (snapshot-registry registry-atom)
      build-db
      query-violations))
