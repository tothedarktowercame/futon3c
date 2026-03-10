(ns futon3c.agents.codex-code-logic
  "core.logic companion for Codex cross-store invariants.

   This namespace intentionally does not model Emacs-local REPL buffer state.
   Marker ordering, pending-process cleanup, and local session-file hygiene stay
   in `codex-repl.el` / `agent-chat-invariants.el`.

   The concern here is the server-side Codex picture that used to leak into
   `codex-repl.el` diagnostics: registry status, invoke jobs, and public
   acceptance events should form a coherent story."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]))

;; =============================================================================
;; Relations (fact schema)
;; =============================================================================

(pldb/db-rel agento agent-id)
(pldb/db-rel registry-statuso agent-id status)
(pldb/db-rel registry-sessiono agent-id session-id)
(pldb/db-rel queued-job-counto agent-id count)
(pldb/db-rel running-job-counto agent-id count)
(pldb/db-rel nonterminal-job-counto agent-id count)
(pldb/db-rel jobo job-id agent-id state)
(pldb/db-rel job-sessiono job-id session-id)
(pldb/db-rel announcemento announce-id job-id agent-id surface)

;; =============================================================================
;; Fact database construction
;; =============================================================================

(defn- nonblank-string
  [x]
  (let [s (some-> x str str/trim)]
    (when (seq s) s)))

(defn- add-agent-facts
  [db [agent-id agent]]
  (let [db (pldb/db-fact db agento agent-id)
        db (pldb/db-fact db registry-statuso agent-id (or (:status agent) :idle))
        db (cond-> db
             (nonblank-string (:session-id agent))
             (pldb/db-fact registry-sessiono agent-id (nonblank-string (:session-id agent))))]
    (-> db
        (pldb/db-fact queued-job-counto agent-id (long (or (:queued-jobs agent) 0)))
        (pldb/db-fact running-job-counto agent-id (long (or (:running-jobs agent) 0)))
        (pldb/db-fact nonterminal-job-counto agent-id (long (or (:nonterminal-jobs agent) 0))))))

(defn- add-job-facts
  [db jobs]
  (reduce (fn [db* {:keys [job-id agent-id state session-id]}]
            (let [job-id (nonblank-string job-id)
                  agent-id (nonblank-string agent-id)
                  state (nonblank-string state)]
              (if (and job-id agent-id state)
                (cond-> (pldb/db-fact db* jobo job-id agent-id state)
                  (nonblank-string session-id)
                  (pldb/db-fact job-sessiono job-id (nonblank-string session-id)))
                db*)))
          db
          jobs))

(defn- add-announcement-facts
  [db announcements]
  (reduce (fn [db* {:keys [announce-id job-id agent-id surface]}]
            (let [announce-id (nonblank-string announce-id)
                  job-id (nonblank-string job-id)
                  agent-id (nonblank-string agent-id)
                  surface (nonblank-string (or surface "irc"))]
              (if (and announce-id job-id agent-id surface)
                (pldb/db-fact db* announcemento announce-id job-id agent-id surface)
                db*)))
          db
          announcements))

(defn build-db
  "Build a fact DB for Codex runtime integrity checks.

   Input keys:
   - :registry-status result of `futon3c.agency.registry/registry-status`
   - :jobs [{:job-id ... :agent-id ... :state ... :session-id ...}]
   - :announcements [{:announce-id ... :job-id ... :agent-id ... :surface ...}]

   `:jobs` and `:announcements` are injected snapshots. This keeps the logic
   layer honest about its sources instead of reaching into private internals."
  [{:keys [registry-status jobs announcements]}]
  (let [agents (or (:agents registry-status) {})
        base (pldb/db)]
    (-> (reduce add-agent-facts base agents)
        (add-job-facts (or jobs []))
        (add-announcement-facts (or announcements [])))))

(defn build-live-db
  "Convenience wrapper for live registry-only diagnostics.

   This is enough for count/status checks. Per-job or public-commitment checks
   still require the caller to supply `:jobs` or `:announcements` snapshots."
  [opts]
  (build-db (assoc opts :registry-status (or (:registry-status opts)
                                             (reg/registry-status)))))

;; =============================================================================
;; Logic goals
;; =============================================================================

(defn running-job-implies-invokingo
  [job-id]
  (l/fresh [agent-id session-id]
    (jobo job-id agent-id "running")
    (registry-statuso agent-id :invoking)))

(defn announcement-backed-by-jobo
  [announce-id]
  (l/fresh [job-id agent-id surface job-agent-id state]
    (announcemento announce-id job-id agent-id surface)
    (jobo job-id job-agent-id state)))

(defn running-session-alignedo
  [job-id]
  (l/fresh [agent-id job-session registry-session]
    (jobo job-id agent-id "running")
    (job-sessiono job-id job-session)
    (registry-sessiono agent-id registry-session)
    (l/== job-session registry-session)))

;; =============================================================================
;; Queries
;; =============================================================================

(defn- all-job-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [job-id agent-id state]
        (jobo job-id agent-id state)
        (l/== q {:job-id job-id
                 :agent-id agent-id
                 :state state})))))

(defn- all-announcement-facts
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [announce-id job-id agent-id surface]
        (announcemento announce-id job-id agent-id surface)
        (l/== q {:announce-id announce-id
                 :job-id job-id
                 :agent-id agent-id
                 :surface surface})))))

(defn- registered-agent-ids
  [db]
  (set
   (pldb/with-db db
     (l/run* [agent-id]
       (agento agent-id)))))

(defn- agent-status-map
  [db]
  (into {}
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [agent-id status]
              (registry-statuso agent-id status)
              (l/== q [agent-id status]))))))

(defn- agent-session-map
  [db]
  (into {}
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [agent-id session-id]
              (registry-sessiono agent-id session-id)
              (l/== q [agent-id session-id]))))))

(defn- running-count-map
  [db]
  (into {}
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [agent-id count]
              (running-job-counto agent-id count)
              (l/== q [agent-id count]))))))

(defn- job-session-map
  [db]
  (into {}
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [job-id session-id]
              (job-sessiono job-id session-id)
              (l/== q [job-id session-id]))))))

(defn- job-agent-map
  [db]
  (into {}
        (map (juxt :job-id identity) (all-job-facts db))))

(defn query-jobs-for-unregistered-agents
  "Return [job-id agent-id] for jobs whose target agent is not registered."
  [db]
  (let [registered (registered-agent-ids db)]
    (->> (all-job-facts db)
         (remove (fn [{:keys [agent-id]}] (contains? registered agent-id)))
         (mapv (fn [{:keys [job-id agent-id]}] [job-id agent-id])))))

(defn query-running-jobs-on-idle-agents
  "Return [job-id agent-id status] when a running job contradicts registry status."
  [db]
  (let [statuses (agent-status-map db)]
    (->> (all-job-facts db)
         (filter (fn [{:keys [state]}] (= "running" state)))
         (remove (fn [{:keys [agent-id]}] (= :invoking (get statuses agent-id))))
         (mapv (fn [{:keys [job-id agent-id]}]
                 [job-id agent-id (get statuses agent-id :missing)])))))

(defn query-running-count-mismatches
  "Return [agent-id running-jobs status] when registry counts imply in-flight work
   but the registry status is not `:invoking`."
  [db]
  (let [statuses (agent-status-map db)]
    (->> (running-count-map db)
         (filter (fn [[_ count]] (pos? (long count))))
         (remove (fn [[agent-id _]] (= :invoking (get statuses agent-id))))
         (mapv (fn [[agent-id count]]
                 [agent-id count (get statuses agent-id :missing)])))))

(defn query-orphan-announcements
  "Return [announce-id job-id] for public acceptances without canonical jobs."
  [db]
  (let [jobs (job-agent-map db)]
    (->> (all-announcement-facts db)
         (remove (fn [{:keys [job-id]}] (contains? jobs job-id)))
         (mapv (fn [{:keys [announce-id job-id]}]
                 [announce-id job-id])))))

(defn query-announcement-agent-mismatches
  "Return [announce-id job-id announced-agent canonical-agent] when public
   acceptance metadata disagrees with the canonical ledger."
  [db]
  (let [jobs (job-agent-map db)]
    (->> (all-announcement-facts db)
         (keep (fn [{:keys [announce-id job-id agent-id]}]
                 (when-let [job (get jobs job-id)]
                   (let [canonical-agent (:agent-id job)]
                     (when (not= canonical-agent agent-id)
                       [announce-id job-id agent-id canonical-agent])))))
         vec)))

(defn query-running-session-mismatches
  "Return [job-id agent-id job-session registry-session] when a running job and
   registry disagree about the current session continuity."
  [db]
  (let [registry-sessions (agent-session-map db)
        job-sessions (job-session-map db)]
    (->> (all-job-facts db)
         (filter (fn [{:keys [state]}] (= "running" state)))
         (keep (fn [{:keys [job-id agent-id]}]
                 (let [job-session (get job-sessions job-id)
                       registry-session (get registry-sessions agent-id)]
                   (when (and job-session registry-session
                              (not= job-session registry-session))
                     [job-id agent-id job-session registry-session]))))
         vec)))

(defn query-violations
  "Aggregate current Codex cross-store integrity violations."
  [db]
  {:jobs-for-unregistered-agents (query-jobs-for-unregistered-agents db)
   :running-jobs-on-idle-agents (query-running-jobs-on-idle-agents db)
   :running-count-mismatches (query-running-count-mismatches db)
   :orphan-announcements (query-orphan-announcements db)
   :announcement-agent-mismatches (query-announcement-agent-mismatches db)
   :running-session-mismatches (query-running-session-mismatches db)})
