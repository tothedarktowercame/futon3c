(ns futon3c.agents.tickle-logic
  "Relational invariant checks for Tickle/watchdog coordination.

   This namespace is intentionally an invariant layer, not another Tickle
   implementation. The runtime behavior still lives in `futon3c.agents.tickle`
   and the extracted `futon3c.dev.*` orchestration modules. This namespace
   snapshots their outputs into a core.logic fact DB so we can ask structural
   questions about:

   - registry eligibility and routing
   - page/escalation bookkeeping supplied by callers
   - watchdog scan/page/escalation chains derived from evidence
   - umwelt-darkness alignment between stall judgements and public evidence"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore])
  (:import [java.time Duration Instant]))

;; =============================================================================
;; Relations (fact schema)
;; =============================================================================

(pldb/db-rel agento agent-id)
(pldb/db-rel agent-typeo agent-id agent-type)
(pldb/db-rel capabilityo agent-id capability)
(pldb/db-rel statuso agent-id status)
(pldb/db-rel invoke-readyo agent-id ready?)
(pldb/db-rel invoke-routeo agent-id route)
(pldb/db-rel authorityo agent-id authority)
(pldb/db-rel assignableo agent-id obligation-id)
(pldb/db-rel pageo event-id agent-id obligation-id cause)
(pldb/db-rel escalationo event-id agent-id obligation-id cause)
(pldb/db-rel precedeso earlier-event-id later-event-id)

;; Watchdog/evidence-derived relations.
(pldb/db-rel scannedo agent-id scan-at threshold-seconds)
(pldb/db-rel stallo agent-id scan-at threshold-seconds)
(pldb/db-rel pagedo agent-id page-at method)
(pldb/db-rel escalatedo agent-id escalate-at cause)
(pldb/db-rel last-evidenceo agent-id evidence-at)

(def ^:private allowed-authorities
  #{:registry :evidence :manual})

(def ^:private allowed-causes
  #{:tick :post-invoke :manual})

(defn- parse-instant
  [x]
  (cond
    (instance? Instant x) x
    (string? x) (try
                  (Instant/parse x)
                  (catch Exception _ nil))
    :else nil))

(defn- event-tags
  [entry]
  (set (or (:evidence/tags entry) (:tags entry) [])))

(defn- event-body
  [entry]
  (or (:evidence/body entry) (:body entry) {}))

(defn- event-at
  [entry]
  (or (:evidence/at entry)
      (:at entry)
      (:evidence/at (event-body entry))
      (:at (event-body entry))))

(defn- latest-activity-by-author
  [entries]
  (reduce
   (fn [acc entry]
     (let [author (:evidence/author entry)
           at (parse-instant (:evidence/at entry))]
       (if (and (string? author) at)
         (update acc author
                 (fn [prev]
                   (if (or (nil? prev) (.isAfter ^Instant at ^Instant prev))
                     at
                     prev)))
         acc)))
   {}
   entries))

;; =============================================================================
;; Fact database construction
;; =============================================================================

(defn- add-agent-facts
  [db [agent-id agent]]
  (let [db (pldb/db-fact db agento agent-id)
        db (pldb/db-fact db agent-typeo agent-id (:type agent))
        db (pldb/db-fact db statuso agent-id (or (:status agent) :idle))
        db (pldb/db-fact db invoke-readyo agent-id (boolean (:invoke-ready? agent)))
        db (pldb/db-fact db invoke-routeo agent-id (:invoke-route agent))]
    (reduce (fn [db* capability]
              (pldb/db-fact db* capabilityo agent-id capability))
            db
            (or (:capabilities agent) []))))

(defn- add-authority-facts
  [db authority-map]
  (reduce (fn [db* [agent-id authority]]
            (if (allowed-authorities authority)
              (pldb/db-fact db* authorityo agent-id authority)
              db*))
          db
          authority-map))

(defn- add-assignable-facts
  [db assignments]
  (reduce (fn [db* {:keys [agent-id obligation-id]}]
            (if (and agent-id obligation-id)
              (pldb/db-fact db* assignableo agent-id obligation-id)
              db*))
          db
          assignments))

(defn- add-page-facts
  [db pages]
  (reduce (fn [db* {:keys [event-id agent-id obligation-id cause]}]
            (if (and event-id agent-id obligation-id (allowed-causes cause))
              (pldb/db-fact db* pageo event-id agent-id obligation-id cause)
              db*))
          db
          pages))

(defn- add-escalation-facts
  [db escalations]
  (reduce (fn [db* {:keys [event-id agent-id obligation-id cause]}]
            (if (and event-id agent-id obligation-id cause)
              (pldb/db-fact db* escalationo event-id agent-id obligation-id cause)
              db*))
          db
          escalations))

(defn- add-order-facts
  [db precedes]
  (reduce (fn [db* [earlier later]]
            (if (and earlier later)
              (pldb/db-fact db* precedeso earlier later)
              db*))
          db
          precedes))

(defn- add-last-evidence-facts
  [db entries]
  (reduce (fn [db* [agent-id last-at]]
            (pldb/db-fact db* last-evidenceo agent-id (str last-at)))
          db
          (latest-activity-by-author entries)))

(defn- add-derived-watchdog-facts
  [db entries]
  (reduce
   (fn [db* entry]
     (let [tags (event-tags entry)
           body (event-body entry)]
       (cond
         (and (= "tickle-1" (:evidence/author entry))
              (contains? tags :tickle)
              (contains? tags :scan))
         (let [scan-at (or (:cycle-at body) (event-at entry))
               threshold (long (or (:threshold-seconds body) 0))
               activity (or (:activity body) {})
               stalled (set (or (:stalled body) []))
               db* (reduce (fn [db** agent-id]
                             (pldb/db-fact db** scannedo agent-id scan-at threshold))
                           db*
                           (keys activity))]
           (reduce (fn [db** agent-id]
                     (pldb/db-fact db** stallo agent-id scan-at threshold))
                   db*
                   stalled))

         (and (= "tickle-1" (:evidence/author entry))
              (contains? tags :tickle)
              (contains? tags :page))
         (let [page-at (or (:at body) (event-at entry))
               agent-id (:agent-id body)
               method (or (:method body) :unknown)]
           (if (and agent-id page-at)
             (pldb/db-fact db* pagedo agent-id page-at method)
             db*))

         (and (= "tickle-1" (:evidence/author entry))
              (contains? tags :tickle)
              (contains? tags :escalation))
         (let [escalate-at (or (:at body) (event-at entry))
               agent-id (:agent-id body)
               cause (or (:cause body) :unknown)]
           (if (and agent-id escalate-at)
             (pldb/db-fact db* escalatedo agent-id escalate-at cause)
             db*))

         :else
         db*)))
   db
   entries))

(defn build-db
  "Build a fact DB for Tickle/runtime coordination checks.

   Input keys:
   - :registry-status  result of `futon3c.agency.registry/registry-status`
   - :authorities      {agent-id -> :registry | :evidence | :manual}
   - :assignments      [{:agent-id ... :obligation-id ...}]
   - :pages            [{:event-id ... :agent-id ... :obligation-id ...
   -                    :cause :tick|:post-invoke|:manual}]
   - :escalations      [{:event-id ... :agent-id ... :obligation-id ...
   -                    :cause keyword}]
   - :precedes         [[earlier-event-id later-event-id] ...]
   - :evidence-entries EvidenceEntry seq used to derive watchdog scan/page/
   -                    escalation facts and last-public-evidence timestamps."
  [{:keys [registry-status authorities assignments pages escalations precedes evidence-entries]}]
  (let [agents (or (:agents registry-status) {})
        entries (vec (or evidence-entries []))
        base (pldb/db)]
    (-> (reduce add-agent-facts base agents)
        (add-authority-facts (or authorities {}))
        (add-assignable-facts (or assignments []))
        (add-page-facts (or pages []))
        (add-escalation-facts (or escalations []))
        (add-order-facts (or precedes []))
        (add-derived-watchdog-facts entries)
        (add-last-evidence-facts entries))))

(defn build-live-db
  "Convenience wrapper around `reg/registry-status` and the evidence store for
   REPL diagnostics / invariant checks."
  [{:keys [registry-status evidence-store] :as opts}]
  (let [entries (or (:evidence-entries opts)
                    (estore/query* evidence-store {}))]
    (build-db (assoc opts
                     :registry-status (or registry-status (reg/registry-status))
                     :evidence-entries entries))))

;; =============================================================================
;; Logic goals
;; =============================================================================

(defn coordination-capableo
  [agent-id]
  (capabilityo agent-id :coordination/execute))

(defn page-target-valido
  "Page targets must be registered, coordination-capable, and invoke-ready."
  [agent-id]
  (l/all
   (agento agent-id)
   (coordination-capableo agent-id)
   (invoke-readyo agent-id true)))

(defn page-backed-by-assignmento
  [event-id]
  (l/fresh [agent-id obligation-id cause]
    (pageo event-id agent-id obligation-id cause)
    (assignableo agent-id obligation-id)))

(defn escalation-backed-by-pageo
  [escalation-id]
  (l/fresh [agent-id obligation-id cause page-id page-cause]
    (escalationo escalation-id agent-id obligation-id cause)
    (pageo page-id agent-id obligation-id page-cause)
    (precedeso page-id escalation-id)))

(defn page-cause-valido
  [event-id]
  (l/fresh [agent-id obligation-id cause]
    (pageo event-id agent-id obligation-id cause)
    (l/membero cause (seq allowed-causes))))

;; =============================================================================
;; Query helpers
;; =============================================================================

(defn- all-page-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [event-id agent-id obligation-id cause]
        (pageo event-id agent-id obligation-id cause)
        (l/== q {:event-id event-id
                 :agent-id agent-id
                 :obligation-id obligation-id
                 :cause cause})))))

(defn- all-escalation-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [event-id agent-id obligation-id cause]
        (escalationo event-id agent-id obligation-id cause)
        (l/== q {:event-id event-id
                 :agent-id agent-id
                 :obligation-id obligation-id
                 :cause cause})))))

(defn- all-scan-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [agent-id scan-at threshold-seconds]
        (scannedo agent-id scan-at threshold-seconds)
        (l/== q {:agent-id agent-id
                 :scan-at scan-at
                 :threshold-seconds threshold-seconds})))))

(defn- all-stall-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [agent-id scan-at threshold-seconds]
        (stallo agent-id scan-at threshold-seconds)
        (l/== q {:agent-id agent-id
                 :scan-at scan-at
                 :threshold-seconds threshold-seconds})))))

(defn- all-watchdog-page-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [agent-id page-at method]
        (pagedo agent-id page-at method)
        (l/== q {:agent-id agent-id
                 :page-at page-at
                 :method method})))))

(defn- all-watchdog-escalation-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [agent-id escalate-at cause]
        (escalatedo agent-id escalate-at cause)
        (l/== q {:agent-id agent-id
                 :escalate-at escalate-at
                 :cause cause})))))

(defn- registered-agent-ids
  [db]
  (set
   (pldb/with-db db
     (l/run* [agent-id]
       (agento agent-id)))))

(defn- invoke-ready-agent-ids
  [db]
  (set
   (pldb/with-db db
     (l/run* [agent-id]
       (invoke-readyo agent-id true)))))

(defn- coordination-capable-agent-ids
  [db]
  (set
   (pldb/with-db db
     (l/run* [agent-id]
       (coordination-capableo agent-id)))))

(defn- assigned-pairs
  [db]
  (set
   (pldb/with-db db
     (l/run* [q]
       (l/fresh [agent-id obligation-id]
         (assignableo agent-id obligation-id)
         (l/== q [agent-id obligation-id]))))))

(defn- authority-agent-ids
  [db]
  (set
   (pldb/with-db db
     (l/run* [agent-id]
       (l/fresh [authority]
         (authorityo agent-id authority))))))

(defn- precedence-pairs
  [db]
  (set
   (pldb/with-db db
     (l/run* [q]
       (l/fresh [earlier later]
         (precedeso earlier later)
         (l/== q [earlier later]))))))

(defn- last-evidence-map
  [db]
  (into {}
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [agent-id evidence-at]
              (last-evidenceo agent-id evidence-at)
              (l/== q [agent-id evidence-at]))))))

(defn- prior-event?
  [earlier later]
  (let [earlier (parse-instant earlier)
        later (parse-instant later)]
    (and earlier later (.isBefore ^Instant earlier ^Instant later))))

(defn- stale-at-scan?
  [last-evidence-at scan-at threshold-seconds]
  (let [scan-at (parse-instant scan-at)
        last-evidence-at (parse-instant last-evidence-at)]
    (if-not scan-at
      false
      (if-not last-evidence-at
        true
        (> (.getSeconds (Duration/between last-evidence-at scan-at))
           (long threshold-seconds))))))

;; =============================================================================
;; Queries
;; =============================================================================

(defn query-unregistered-pages
  "Return page event IDs whose target agent is absent from the registry facts."
  [db]
  (let [registered (registered-agent-ids db)]
    (->> (all-page-facts db)
         (remove (fn [{:keys [agent-id]}] (contains? registered agent-id)))
         (mapv :event-id))))

(defn query-unroutable-pages
  "Return [event-id agent-id] for pages sent to non-eligible live targets."
  [db]
  (let [ready (invoke-ready-agent-ids db)
        capable (coordination-capable-agent-ids db)]
    (->> (all-page-facts db)
         (remove (fn [{:keys [agent-id]}]
                   (and (contains? ready agent-id)
                        (contains? capable agent-id))))
         (mapv (fn [{:keys [event-id agent-id]}]
                 [event-id agent-id])))))

(defn query-pages-without-assignments
  "Return [event-id agent-id obligation-id] for pages that lack a matching
   assignable obligation fact."
  [db]
  (let [assigned (assigned-pairs db)]
    (->> (all-page-facts db)
         (remove (fn [{:keys [agent-id obligation-id]}]
                   (contains? assigned [agent-id obligation-id])))
         (mapv (fn [{:keys [event-id agent-id obligation-id]}]
                 [event-id agent-id obligation-id])))))

(defn query-pages-without-authority
  "Return [event-id agent-id] when the caller has not declared what liveness
   authority justified the page."
  [db]
  (let [authorized (authority-agent-ids db)]
    (->> (all-page-facts db)
         (remove (fn [{:keys [agent-id]}] (contains? authorized agent-id)))
         (mapv (fn [{:keys [event-id agent-id]}]
                 [event-id agent-id])))))

(defn query-orphan-escalations
  "Return escalation IDs that are not backed by an earlier matching page."
  [db]
  (let [pages (all-page-facts db)
        precedence (precedence-pairs db)]
    (->> (all-escalation-facts db)
         (remove (fn [{:keys [event-id agent-id obligation-id]}]
                   (some (fn [{page-id :event-id
                              page-agent-id :agent-id
                              page-obligation-id :obligation-id}]
                           (and (= page-agent-id agent-id)
                                (= page-obligation-id obligation-id)
                                (contains? precedence [page-id event-id])))
                         pages)))
         (mapv :event-id))))

(defn query-watchdog-pages-without-scans
  "Return agent IDs for watchdog pages that were not preceded by a scan for the
   same agent."
  [db]
  (let [scans (all-scan-facts db)]
    (->> (all-watchdog-page-facts db)
         (remove (fn [{page-agent-id :agent-id
                       :keys [page-at]}]
                   (some (fn [{scan-agent-id :agent-id
                               :keys [scan-at]}]
                           (and (= scan-agent-id page-agent-id)
                                (prior-event? scan-at page-at)))
                         scans)))
         (mapv :agent-id))))

(defn query-watchdog-escalations-without-pages
  "Return agent IDs for watchdog escalations that were not preceded by a page
   for the same agent."
  [db]
  (let [pages (all-watchdog-page-facts db)]
    (->> (all-watchdog-escalation-facts db)
         (remove (fn [{escalate-agent-id :agent-id
                       :keys [escalate-at]}]
                   (some (fn [{page-agent-id :agent-id
                               :keys [page-at]}]
                           (and (= page-agent-id escalate-agent-id)
                                (prior-event? page-at escalate-at)))
                         pages)))
         (mapv :agent-id))))

(defn query-stall-evidence-mismatches
  "Return scan facts where Tickle's stale/not-stale judgement disagrees with
   the latest public evidence timestamp for that agent."
  [db]
  (let [stalls (set (map (juxt :agent-id :scan-at) (all-stall-facts db)))
        last-evidence (last-evidence-map db)]
    (->> (all-scan-facts db)
         (keep (fn [{:keys [agent-id scan-at threshold-seconds]}]
                 (let [expected-stale? (stale-at-scan? (get last-evidence agent-id)
                                                      scan-at
                                                      threshold-seconds)
                       actual-stale? (contains? stalls [agent-id scan-at])]
                   (when (not= expected-stale? actual-stale?)
                     {:agent-id agent-id
                      :scan-at scan-at
                      :threshold-seconds threshold-seconds
                      :expected-stale? expected-stale?
                      :actual-stale? actual-stale?
                      :last-evidence-at (get last-evidence agent-id)}))))
         vec)))

(defn query-violations
  "Aggregate the current checks into a map of violation vectors."
  [db]
  {:unregistered-pages (query-unregistered-pages db)
   :unroutable-pages (query-unroutable-pages db)
   :pages-without-assignments (query-pages-without-assignments db)
   :pages-without-authority (query-pages-without-authority db)
   :orphan-escalations (query-orphan-escalations db)
   :watchdog-pages-without-scans (query-watchdog-pages-without-scans db)
   :watchdog-escalations-without-pages (query-watchdog-escalations-without-pages db)
   :stall-evidence-mismatches (query-stall-evidence-mismatches db)})
