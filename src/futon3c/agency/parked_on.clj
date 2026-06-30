(ns futon3c.agency.parked-on
  "Durable continuation records for REPL turns — E-repl-continuations INSTANTIATE.

   An agent ends a turn by PARKING it on a join of dispatched dep-ids (the bell/job
   ids it is waiting on). When every dep reaches a terminal state, exactly ONE
   resume fires for the parked agent, carrying the joined result-summaries. This is
   autobell-back generalised: autobell-back is the n=1, caller-keyed case; a
   `parked-on` record is the n>=1, dep-keyed case, with a durable payload + a
   runaway budget.

   DESIGN NOTES (from E-repl-continuations §3-§5):
   - Persisted DISK-backed (spit/slurp edn, like `transport/http` invoke-jobs-ledger
     and `agency/turn_queue`), NOT substrate-2 — so it survives pouch teardown with
     ZERO :7071 write. A substrate-2 hyperedge projection for WebArxana visibility is
     a later, optional layer; disk is the source of truth.
   - Atomic single-fire (R1): release decisions ride a `swap-vals!` so exactly the
     thread that flips a record's join to complete fires its resume — double-delivery
     of the same dep is additionally a no-op because the dep's index edge is consumed.
   - Release AND budget-exhaustion RETRACT the record (R2) — no zombie awaiting-forever.
   - `rehydrate!` (R3) reconciles parked records against the recovered ledger on boot.
   - `sweep-deadlines!` (R4) enforces `:deadline-ms` (the liveness backstop) + fires
     due no-dep timer parks.

   The ns is dependency-injected: callers pass `resume!` (fn of a record),
   `ledger-lookup` (dep-id -> {:state :result-summary}), and `now-ms`, so the five
   hard cases are unit-testable without the live JVM."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.dev.config :as config])
  (:import [java.util UUID]))

(def ^:private default-store "/tmp/futon3c-parked-on.edn")

(defn- store-path [] (or (config/env "FUTON3C_PARKED_ON_PATH") default-store))

;; state = {:records {rid record} :index {dep-id #{rid}}}, where :index maps each
;; still-awaited dep-id to the records waiting on it (the reverse join index).
(defonce ^:private !parked (atom nil))

(defn- empty-state [] {:records {} :index {}})

;; Non-terminal ledger states (mirrors recover-inflight-jobs' "in-flight" set); any
;; other non-nil state counts as terminal (a dep that has finished, success or fail).
(def ^:private non-terminal-states #{"queued" "running" "accepted"})

(defn- terminal-state? [state]
  (and (some? state) (not (contains? non-terminal-states (str state)))))

(defn- load-state []
  (let [f (io/file (store-path))]
    (if (.exists f)
      (try
        (let [s (edn/read-string (slurp f))]
          (if (map? s) (merge (empty-state) (dissoc s :just-released)) (empty-state)))
        (catch Exception _ (empty-state)))
      (empty-state))))

(defn- persist! [state]
  (try
    (spit (store-path) (pr-str (dissoc state :just-released)))
    (catch Exception e
      (println (str "[parked-on] persist failed: " (.getMessage e)))))
  state)

(defn- ensure! [] (when (nil? @!parked) (reset! !parked (load-state))) @!parked)

(defn clear!
  "Reset the store (tests/dev)."
  []
  (reset! !parked (empty-state))
  (persist! (empty-state)))

(defn snapshot [] (ensure!) (dissoc @!parked :just-released))

;; ---------------------------------------------------------------------------
;; index helpers
;; ---------------------------------------------------------------------------

(defn- index-add [index rid awaiting]
  (reduce (fn [idx dep] (update idx dep (fnil conj #{}) rid)) index awaiting))

(defn- index-disj [index dep rid]
  (let [s (disj (get index dep #{}) rid)]
    (if (empty? s) (dissoc index dep) (assoc index dep s))))

(defn- drop-record
  "Remove record RID and all its remaining index edges (release/expire/exhaust)."
  [state rid]
  (let [rec (get-in state [:records rid])]
    (-> (reduce (fn [st dep] (update st :index index-disj dep rid)) state (:awaiting rec))
        (update :records dissoc rid))))

;; ---------------------------------------------------------------------------
;; completion -> dep-keyed JOIN release (the genuine new logic, §3.3)
;; ---------------------------------------------------------------------------

(defn- release-or-retract
  "RID's join just completed. If budget allows, mark released (it will be fired and
   then dropped); else retract silently. Stashes released recs in :just-released."
  [state rid now-ms]
  (let [rec (get-in state [:records rid])
        left (get-in rec [:budget :resumes-left] 1)]
    (if (pos? left)
      (let [rec* (assoc rec :released? true :released-at-ms now-ms
                        :budget (update (:budget rec) :resumes-left dec))]
        (-> state
            (assoc-in [:records rid] rec*)
            (update :just-released (fnil conj []) rec*)))
      ;; budget exhausted -> retract, do not fire
      (drop-record state rid))))

(defn- apply-completion [state dep-id summary now-ms]
  (let [rids (get-in state [:index dep-id])]
    (reduce
     (fn [st rid]
       (let [rec (get-in st [:records rid])]
         (if (or (nil? rec) (:released? rec))
           st
           (let [rec* (-> rec (update :awaiting disj dep-id)
                          (assoc-in [:arrived dep-id] summary))
                 st (-> st
                        (assoc-in [:records rid] rec*)
                        (update :index index-disj dep-id rid))]
             (if (seq (:awaiting rec*))
               st                                   ; join not complete yet
               (release-or-retract st rid now-ms))))))
     (assoc state :just-released [])
     rids)))

(defn note-completion!
  "Fold dep DEP-ID (terminal, carrying RESULT-SUMMARY) into every record awaiting it.
   Fires RESUME! exactly once for each record whose join just completed (budget
   permitting); budget-exhausted records are retracted silently. Returns
   {:released [rid ...]}."
  [dep-id result-summary {:keys [resume! now-ms] :or {now-ms (System/currentTimeMillis)}}]
  (ensure!)
  (let [[_ new] (swap-vals! !parked #(apply-completion % dep-id result-summary now-ms))
        fired (:just-released new)]
    (when (seq fired)
      (swap! !parked (fn [st] (reduce (fn [s rec] (drop-record s (:id rec))) st fired))))
    (persist! @!parked)
    (doseq [rec fired] (when resume! (resume! rec)))
    {:released (mapv :id fired)}))

;; ---------------------------------------------------------------------------
;; park! — register a continuation (§3.2) + reconcile-on-park (case 1)
;; ---------------------------------------------------------------------------

(defn park!
  "Register a continuation: park AGENT/SESSION's turn until all AWAITING dep-ids are
   terminal, then RESUME! once with the join. Reconciles against already-terminal
   deps via LEDGER-LOOKUP at park time (closes the lost-wakeup race). With no deps:
   resumes immediately unless a :timer-due-ms is set (then it waits for the sweep).
   Returns {:id rid :status :parked|:released|:released-immediately}."
  [{:keys [agent session awaiting payload timer-due-ms deadline-ms budget]}
   {:keys [ledger-lookup resume! now-ms] :or {now-ms (System/currentTimeMillis)}}]
  (ensure!)
  (let [rid (str "park-" (UUID/randomUUID))
        awaiting (set awaiting)
        budget (merge {:resumes-left 1 :max-depth 8} budget)
        rec {:id rid :agent agent :session session :awaiting awaiting :arrived {}
             :payload payload :timer-due-ms timer-due-ms :deadline-ms deadline-ms
             :budget budget :parked-at-ms now-ms :released? false}]
    (cond
      (and (empty? awaiting) (not timer-due-ms))
      (do (when resume! (resume! rec)) {:id rid :status :released-immediately})

      :else
      (do
        (swap! !parked (fn [st] (-> st
                                    (assoc-in [:records rid] rec)
                                    (update :index index-add rid awaiting))))
        (persist! @!parked)
        ;; reconcile: any dep already terminal in the ledger -> fold it in now
        (when ledger-lookup
          (doseq [dep awaiting]
            (let [j (ledger-lookup dep)]
              (when (and j (terminal-state? (:state j)))
                (note-completion! dep (:result-summary j)
                                  {:resume! resume! :now-ms now-ms})))))
        (if (get-in @!parked [:records rid])
          {:id rid :status :parked}
          {:id rid :status :released})))))

;; ---------------------------------------------------------------------------
;; rehydrate! (R3, boot) + sweep-deadlines! (R4, the required timer)
;; ---------------------------------------------------------------------------

(defn rehydrate!
  "On boot: reload persisted records and reconcile each awaited dep against the
   (already recovered) ledger — fold now-terminal deps in (a dep marked failed by
   recover-inflight-jobs counts as arrived, not a hang), releasing any join that
   completed during downtime. Returns {:loaded n :released [...]}."
  [{:keys [ledger-lookup resume! now-ms] :or {now-ms (System/currentTimeMillis)}}]
  (reset! !parked (load-state))
  (let [recs (vals (:records @!parked))
        released (atom [])]
    (doseq [rec recs
            dep (vec (:awaiting rec))]
      (when ledger-lookup
        (let [j (ledger-lookup dep)]
          (when (and j (terminal-state? (:state j)))
            (let [r (note-completion! dep (:result-summary j)
                                      {:resume! resume! :now-ms now-ms})]
              (swap! released into (:released r)))))))
    {:loaded (count recs) :released @released}))

(defn sweep-deadlines!
  "Force-terminate records past :deadline-ms (the liveness backstop) and fire due
   no-dep :timer-due-ms parks. ON-EXPIRE is called per expired record; RESUME! fires
   due timers. Returns {:expired [rid ...] :timer-fired [rid ...]}."
  [{:keys [now-ms resume! on-expire] :or {now-ms (System/currentTimeMillis)}}]
  (ensure!)
  (let [recs (vals (:records @!parked))
        expired (filterv (fn [r] (and (not (:released? r)) (:deadline-ms r)
                                      (>= now-ms (:deadline-ms r)))) recs)
        timers  (filterv (fn [r] (and (not (:released? r)) (empty? (:awaiting r))
                                      (:timer-due-ms r) (>= now-ms (:timer-due-ms r))
                                      (not (some #{(:id r)} (map :id expired))))) recs)]
    (when (or (seq expired) (seq timers))
      (swap! !parked (fn [st]
                       (reduce (fn [s r] (drop-record s (:id r)))
                               st (concat expired timers))))
      (persist! @!parked))
    (doseq [r expired] (when on-expire (on-expire r)))
    (doseq [r timers] (when resume! (resume! r)))
    {:expired (mapv :id expired) :timer-fired (mapv :id timers)}))
