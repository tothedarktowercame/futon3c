(ns futon3c.logic.probe
  "Live-state probe for the structural-law inventory.

   The probe walks the operational-families list and asks each family's
   registered check function: 'are you currently binding?' It records the
   answer as a `:event :family-fired` evidence entry per family per run.
   Aggregating those entries gives the arxana operational-families view
   the data it needs for `last-fire-at` / `last-violation-at` / `inactive-since`
   — converting the inventory from a list of past claims into a live
   dashboard of present binding.

   Three running modes (DERIVE choice, ratified by Joe 2026-04-29):

     (a) **Scheduled** — hourly default; the always-on background loop
         that keeps `last-fire-at` current without per-turn cost.
     (b) **On-demand** — `probe-now!` for debug / CI / manual inspection.
     (c) **Autoshutter** — `with-autoshutter-probe` for tests that
         explicitly need the probe to fire as part of their assertions
         (synchronous; expensive; opt-in).

   Backoff to per-24-hours is a configuration switch (`:cadence-ms`).
   Per-turn synchrony was rejected up front.

   Per-family checks are not built into this namespace. The probe READS
   from a registry (`family-check-fns`) populated by domain code; if a
   family has no registered check, its outcome is `:inactive` (the
   projection apparatus surfaces the fact that nothing is verifying that
   family right now). This matches the projection-not-enumeration framing
   from M-invariant-queue-unstuck IDENTIFY.

   Evidence shape, per family per run:

     {:evidence/type :coordination
      :evidence/claim-type :observation
      :evidence/body
        {:event :family-fired
         :family-id <kw>
         :outcome :ok | :violation | :inactive
         :detail <check-specific data, optional>
         :probe-run-id <uuid>
         :invariant I-family-canary
         :at <iso-string>}
      :evidence/tags [:invariant-queue :family-canary :family-fired
                      :<outcome> :<family-id>]}

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/)."
  (:require [futon3c.evidence.boundary :as boundary]
            [futon3c.logic.inventory :as inventory])
  (:import [java.time Instant]
           [java.util UUID]
           [java.util.concurrent Executors
                                 ScheduledExecutorService
                                 ScheduledFuture
                                 TimeUnit]))

(def I-family-canary
  "Canonical statement of the family-canary invariant. Grep-verifiable."
  (str "I-family-canary: every family with :status :operational has at "
       "least one :family-fired evidence entry within the configured "
       "staleness window (default: 24 hours). Older = :inactive and "
       "surfaces in the operational-families view distinctly."))

;; ---------------------------------------------------------------------------
;; Cadence configuration
;; ---------------------------------------------------------------------------

(def default-cadence-ms
  "Default scheduled-probe cadence: hourly. Configurable per `start-probe-loop!`."
  (* 60 60 1000))

(def backoff-cadence-ms
  "Documented backoff cadence: per-24-hours. Use when hourly proves too noisy."
  (* 24 60 60 1000))

;; ---------------------------------------------------------------------------
;; Per-family check registry
;; ---------------------------------------------------------------------------

(defonce ^{:doc "family-id (keyword) → check-fn `(fn [evidence-store] result)`.

  Result shape: {:outcome :ok | :violation :detail <map>}. The probe
  augments this with :family-id, :probe-run-id, :at, and the standard
  evidence wrapper. A check-fn that throws is treated as :violation
  with :detail {:exception <message>}.

  Domain code populates this registry via `register-family-check!`; the
  probe never modifies it. INSTANTIATE-3 ships the apparatus only —
  population is INSTANTIATE-4's projection-wave work."
           :durable true}
  family-check-fns
  (atom {}))

;; ^:durable metadata (M-reachable-from-boot 2026-05-01): the live probe
;; registry must be repopulated from bootstrap-rooted registrar calls on
;; every JVM start. Direct `(reset! family-check-fns ...)`, `(swap!
;; family-check-fns ...)`, or ad hoc `(probe/register-family-check! ...)`
;; call sites outside the registrar allowlist are refused by
;; `scripts/check-reachable-from-boot-family-check-fns.sh`.

(defn register-family-check!
  "Register a check function for FAMILY-ID. Replaces any prior registration."
  [family-id check-fn]
  (let [registered-fn (if (instance? clojure.lang.IObj check-fn)
                        (vary-meta check-fn assoc :registered-at (str (Instant/now)))
                        check-fn)]
    (swap! family-check-fns assoc family-id registered-fn))
  family-id)

(defn unregister-family-check!
  "Remove FAMILY-ID's check registration (testing helper)."
  [family-id]
  (swap! family-check-fns dissoc family-id)
  family-id)

(defn registered-family-ids
  "Return the set of family-ids with a registered check."
  []
  (set (keys @family-check-fns)))

;; ---------------------------------------------------------------------------
;; Single-family run
;; ---------------------------------------------------------------------------

(defn- run-check-fn
  "Invoke CHECK-FN with EVIDENCE-STORE; normalize result to
   {:outcome :ok | :violation | :inactive :detail <map>}. A throw →
   :violation with exception detail. The :inactive outcome is supported
   so deferred-stub taps (futon3c.logic.probe-taps/make-deferred-tap)
   can explicitly surface 'documented-but-not-yet-wired' rather than
   silently looking like a violation."
  [check-fn evidence-store]
  (try
    (let [r (check-fn evidence-store)]
      (cond
        (= :ok (:outcome r))
        {:outcome :ok :detail (or (:detail r) {})}

        (= :violation (:outcome r))
        {:outcome :violation :detail (or (:detail r) {})}

        (= :inactive (:outcome r))
        {:outcome :inactive :detail (or (:detail r) {})}

        :else
        {:outcome :violation
         :detail {:reason "check-fn returned a non-conforming result"
                  :raw r}}))
    (catch Throwable t
      {:outcome :violation
       :detail {:exception (str (.getName (class t)) ": " (.getMessage t))}})))

(defn run-family-check
  "Run the registered check for FAMILY-ID against EVIDENCE-STORE. Returns
   {:family-id :outcome :detail}. If no check is registered, returns
   :outcome :inactive — the family is in the inventory but no probe is
   verifying it (the canonical projection-apparatus signal that the
   inventory has more claims than the live system can confirm)."
  [evidence-store family-id]
  (if-let [check-fn (get @family-check-fns family-id)]
    (let [{:keys [outcome detail]} (run-check-fn check-fn evidence-store)]
      {:family-id family-id :outcome outcome :detail detail})
    {:family-id family-id
     :outcome :inactive
     :detail {:reason "no check registered for this family"}}))

(defn- emit-family-fired!
  "Emit a :family-fired evidence entry for the given probe result."
  [evidence-store {:keys [family-id outcome detail]} probe-run-id]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id (str "structural-law-family/"
                           (if (keyword? family-id) (name family-id) (str family-id)))}
    :type :coordination
    :claim-type :observation
    :author "probe"
    :body {:event :family-fired
           :family-id family-id
           :outcome outcome
           :detail (or detail {})
           :probe-run-id probe-run-id
           :invariant I-family-canary
           :at (str (Instant/now))}
    :tags (cond-> [:invariant-queue :family-canary :family-fired outcome]
            (keyword? family-id) (conj family-id))}))

;; ---------------------------------------------------------------------------
;; Sweep — one family-by-family run across the inventory
;; ---------------------------------------------------------------------------

(def default-inventory-path
  "Default path to the structural-law inventory, relative to the futon3c
   repo root."
  "docs/structural-law-inventory.sexp")

(defn list-inventory-family-ids
  "Read the inventory at PATH (default `default-inventory-path`) and return
   the seq of family-ids (keywords). Returns empty when the inventory file
   doesn't exist, can't be parsed, or has no families."
  ([] (list-inventory-family-ids default-inventory-path))
  ([path]
   (try
     (let [text (slurp path)
           parsed (inventory/parse-sexp-string text)
           families (inventory/extract-all-families parsed)]
       (->> families
            (keep :id)
            vec))
     (catch Throwable _ []))))

(defn run-probe-sweep!
  "Run the probe across families in the inventory (or the supplied list).

   Two-arity:
     (run-probe-sweep! evidence-store)
       — read family list from `default-inventory-path`.
     (run-probe-sweep! evidence-store family-ids)
       — use the supplied seq of family-ids (testing / per-source taps).

   Returns a summary map:
     {:probe-run-id <uuid>
      :at <iso>
      :results [{:family-id :outcome :detail :evidence/id} ...]
      :counts {:ok N :violation N :inactive N}
      :family-count N
      :invariant <I-family-canary string>}

   Each family's outcome is also emitted as a `:family-fired` evidence
   entry through the boundary (so query-by-tag finds them later)."
  ([evidence-store]
   (run-probe-sweep! evidence-store (list-inventory-family-ids)))
  ([evidence-store family-ids]
   (let [run-id (str "probe-" (UUID/randomUUID))
         at (str (Instant/now))
         results (mapv (fn [fid]
                         (let [r (run-family-check evidence-store fid)
                               emit-result (emit-family-fired! evidence-store r run-id)]
                           (assoc r :evidence/id (:evidence/id emit-result)
                                  :emit-ok? (:ok emit-result))))
                       (vec family-ids))
         counts (->> results
                     (group-by :outcome)
                     (reduce-kv (fn [acc k v] (assoc acc k (count v))) {}))]
     {:probe-run-id run-id
      :at at
      :results results
      :counts (merge {:ok 0 :violation 0 :inactive 0} counts)
      :family-count (count family-ids)
      :invariant I-family-canary})))

;; ---------------------------------------------------------------------------
;; Mode (a) — scheduled background loop
;; ---------------------------------------------------------------------------

(defonce ^{:doc "{:executor <ScheduledExecutorService> :future <ScheduledFuture> ...}"}
  probe-loop-state
  (atom nil))

(defn- make-runnable
  [evidence-store on-error]
  (reify Runnable
    (run [_]
      (try
        (run-probe-sweep! evidence-store)
        (catch Throwable t
          (when on-error (on-error t)))))))

(defn start-probe-loop!
  "Start the scheduled probe loop. Idempotent — calling when already running
   is a no-op (returns the existing state). Returns the loop-state map.

   Options:
     :evidence-store — required, the backend to emit family-fired entries to.
     :cadence-ms     — default `default-cadence-ms` (hourly). Backoff to
                       `backoff-cadence-ms` (24h) is a configuration switch.
     :initial-delay-ms — default 0. Useful for tests that want the first
                       sweep to fire immediately or after a small lag.
     :on-error       — optional callback; called with the throwable when
                       a sweep throws (the loop continues either way).

   Stop the loop with `stop-probe-loop!`."
  [{:keys [evidence-store cadence-ms initial-delay-ms on-error]
    :or {cadence-ms default-cadence-ms
         initial-delay-ms 0}}]
  (or @probe-loop-state
      (let [executor (Executors/newSingleThreadScheduledExecutor)
            runnable (make-runnable evidence-store on-error)
            ^ScheduledFuture fut
            (.scheduleAtFixedRate ^ScheduledExecutorService executor
                                  runnable
                                  (long initial-delay-ms)
                                  (long cadence-ms)
                                  TimeUnit/MILLISECONDS)
            state {:executor executor
                   :future fut
                   :evidence-store evidence-store
                   :cadence-ms cadence-ms
                   :started-at (str (Instant/now))}]
        (reset! probe-loop-state state)
        state)))

(defn stop-probe-loop!
  "Stop the scheduled probe loop and shut down its executor. Returns the
   prior state, or nil if no loop was running."
  []
  (when-let [{:keys [executor future]} @probe-loop-state]
    (try (.cancel ^ScheduledFuture future false) (catch Throwable _ nil))
    (try (.shutdown ^ScheduledExecutorService executor) (catch Throwable _ nil))
    (let [s @probe-loop-state]
      (reset! probe-loop-state nil)
      s)))

(defn probe-loop-running?
  "Return true if the scheduled probe loop is currently active."
  []
  (boolean @probe-loop-state))

;; ---------------------------------------------------------------------------
;; Mode (b) — on-demand
;; ---------------------------------------------------------------------------

(defn probe-now!
  "Run a single probe sweep synchronously. Same return shape as
   `run-probe-sweep!`. Use for debug, CI, or human-driven inspection.
   Independent of the scheduled loop."
  [evidence-store]
  (run-probe-sweep! evidence-store))

;; ---------------------------------------------------------------------------
;; Mode (c) — autoshutter (synchronous test-time fire)
;; ---------------------------------------------------------------------------

(defmacro with-autoshutter-probe
  "Run BODY with a synchronous probe sweep at the start and (optionally)
   end. Used by tests that explicitly want the probe to fire as part of
   their assertions — opt-in, expensive, not for production code paths.

   Form:
     (with-autoshutter-probe evidence-store
       (assert-state-here)
       (do-some-mutation))

   Returns the value of BODY. Probe results are emitted to the evidence
   store as in any other mode."
  [evidence-store & body]
  `(do
     (run-probe-sweep! ~evidence-store)
     (let [result# (do ~@body)]
       (run-probe-sweep! ~evidence-store)
       result#)))
