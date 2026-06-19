(ns futon3c.portfolio-inference.scheduler
  "Recurring AIF tick for the Portfolio Inference peripheral.

   Wires a `ScheduledExecutorService` that calls
   `futon3c.portfolio.core/portfolio-step!` on a fixed cadence so the
   live AIF loop deposits :portfolio/observation, :portfolio/belief,
   :portfolio/policy, and :portfolio/step evidence into the evidence
   store on its own.

   Why this exists:

   - **M-aif-head** describes Portfolio Inference as the slow-timescale
     dual of the Mission Peripheral. Without a recurring tick, the
     dual is instantiable on demand only — not actually live.
   - **M-war-machine** explicitly names the missing piece as
     'continuously available synthesis' rather than 'one-off documents'.
     The recurring tick is what closes that gap on the AIF side.
   - **M-stack-stereolithography** § precision-posture wants
     belief/policy state as ranking input. The scheduler is the
     producer of that state.

   Design notes:

   - Single-thread `ScheduledExecutorService` with `scheduleWithFixedDelay`
     (NOT `scheduleAtFixedRate`) so a slow tick cannot cause a backlog.
   - Each tick is wrapped in try/catch — a single failure must not
     abort the whole schedule.
   - Registers with `futon3c.cyder` so the recurring tick is visible,
     stoppable, and inspectable from the standard process surface.
   - Configurable period; default 1 hour. The first start can take a
     short period for VERIFY purposes, then be reconfigured.
   - Does NOT auto-start on JVM boot. The operator opts in by calling
     `start!` (or via a dev bootstrap line if Joe wants automatic).

   Public surface:

   - `start!`  — begin the recurring tick
   - `stop!`   — cancel the scheduled task and shut down the executor
   - `status`  — return the current state for inspection
   - `tick!`   — fire one tick synchronously (for testing/debugging)
   - `set-period!` — change the period without dropping the schedule"
  (:require [futon3c.cyder :as cyder]
            [futon3c.portfolio.core :as portfolio])
  (:import [java.time Instant]
           [java.util.concurrent Executors ScheduledExecutorService
                                 ScheduledFuture TimeUnit]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def default-period-seconds
  "Default cadence for the recurring AIF tick. 24 hours: portfolio AIF is
   the slow-timescale loop in the futon stack — daily is the right cron
   shape for it. Hourly was the initial draft; live experience showed
   that hourly inflated the evidence store and triggered downstream
   over-polling without producing observable signal change at that rate."
  86400)

(def cyder-process-id "portfolio-inference-scheduler")

;; =============================================================================
;; State
;; =============================================================================

(defonce ^{:doc "Mutable scheduler state.
  Keys:
   :executor          ScheduledExecutorService (or nil when stopped)
   :handle            ScheduledFuture for the recurring task (or nil)
   :period-seconds    current period
   :evidence-store    evidence store passed to portfolio-step!
   :opts              opts map passed to portfolio-step! per tick
   :started-at        Instant when the schedule was started
   :tick-count        successful ticks since start
   :error-count       failed ticks since start
   :last-tick-at      Instant of last attempted tick
   :last-error        most recent tick exception message (or nil)
   :last-result       most recent successful tick summary (or nil)"}
  !state
  (atom {:executor nil
         :handle nil
         :period-seconds default-period-seconds
         :evidence-store nil
         :opts {}
         :started-at nil
         :tick-count 0
         :error-count 0
         :last-tick-at nil
         :last-error nil
         :last-result nil}))

;; =============================================================================
;; Inspection
;; =============================================================================

(defn- compute-next-tick-at
  "Best-effort estimate of when the next tick will fire, given the last
   tick time and current period. Returns Instant or nil. The scheduler
   uses scheduleWithFixedDelay so this is the *expected* next tick
   assuming ticks are short relative to the period."
  [last-tick-at period-seconds]
  (when (and last-tick-at period-seconds)
    (.plusSeconds ^Instant last-tick-at (long period-seconds))))

(defn status
  "Return a snapshot of the scheduler state, safe for JSON/EDN serialization.

   Notable keys consumed by the War Machine UI:
   :period-seconds    — single source of truth for downstream poll cadence
                        and freshness thresholds
   :last-tick-at      — ISO-8601 instant of most recent tick attempt
   :next-tick-at      — ISO-8601 instant of expected next tick (best-effort)
   :last-diagnostics  — `{:mode :urgency :tau :free-energy}` from the most
                        recent successful tick. Read by the WM mode badge
                        (M-war-machine-tuning § TI-3). nil before the first
                        successful tick or when the last tick errored."
  []
  (let [s @!state
        running? (and (some? (:executor s))
                      (not (.isShutdown ^ScheduledExecutorService (:executor s))))
        next-tick (when running?
                    (compute-next-tick-at (:last-tick-at s) (:period-seconds s)))]
    {:running? running?
     :period-seconds (:period-seconds s)
     :started-at (some-> (:started-at s) str)
     :tick-count (:tick-count s)
     :error-count (:error-count s)
     :last-tick-at (some-> (:last-tick-at s) str)
     :next-tick-at (some-> next-tick str)
     :last-error (:last-error s)
     :last-result-keys (when-let [r (:last-result s)] (vec (keys r)))
     :last-diagnostics (when-let [r (:last-result s)] (:diagnostics r))
     :last-action (when-let [r (:last-result s)] (:action r))
     :last-observation (when-let [r (:last-result s)] (:observation r))
     :last-efe-terms (when-let [r (:last-result s)] (:efe-terms r))
     :last-prediction-errors (when-let [r (:last-result s)] (:prediction-errors r))
     :evidence-store-bound? (some? (:evidence-store s))}))

;; =============================================================================
;; The tick
;; =============================================================================

(defn- chosen-policy-terms
  "Pull the :terms map (pragmatic/epistemic/upvote/effort) for the
   chosen action from the policy evaluation list. Returns nil when
   absent."
  [result]
  (let [chosen (:action result)
        policies (get-in result [:policy :policies])]
    (some #(when (= (:action %) chosen) (:terms %)) policies)))

(defn- result-summary
  "Compact summary of a portfolio-step! result. The full result is large;
   we keep what the Cyder process inspector, the AIF stack response, and
   the WM UI need. Specifically:

   - :action       — chosen policy for this tick
   - :diagnostics  — {:mode :urgency :tau :free-energy} (TI-3 surface)
   - :observation  — 16-channel observation map (TI-2 surface)
   - :efe-terms    — {:pragmatic :epistemic :upvote :effort} for the
                     chosen action (TI-4 surface). 4-term EFE per
                     `futon3c.portfolio.policy/expected-free-energy`,
                     not the 2-term G of the strategic surface in
                     war-machine-terminal-vocabulary.edn (see TI-6).
   - :step-count   — monotonic counter
   - :abstain?     — true when τ below confidence threshold
   - :evidence-deposit-count — how many entries this tick wrote"
  [result]
  (when (map? result)
    (let [terms (chosen-policy-terms result)]
      (cond-> {}
        (:action result)         (assoc :action (:action result))
        (:diagnostics result)    (assoc :diagnostics (:diagnostics result))
        (:observation result)    (assoc :observation (:observation result))
        terms                    (assoc :efe-terms terms)
        (get-in result [:perception :errors])
        (assoc :prediction-errors (get-in result [:perception :errors]))
        (get-in result [:state :step-count])
        (assoc :step-count (get-in result [:state :step-count]))
        (get-in result [:policy :abstain?])
        (assoc :abstain? (get-in result [:policy :abstain?]))
        (:evidence result)
        (assoc :evidence-deposit-count
               (count (get-in result [:evidence :entries])))))))

(defn tick!
  "Fire one AIF step synchronously. Used by the schedule loop and
   available for manual invocation. Wraps the call so it can never
   bubble out and abort the executor."
  []
  (let [now (Instant/now)]
    (swap! !state assoc :last-tick-at now)
    (try
      (let [{:keys [evidence-store opts]} @!state
            result (portfolio/portfolio-step! evidence-store opts)
            summary (result-summary result)]
        (swap! !state
               (fn [s]
                 (-> s
                     (update :tick-count inc)
                     (assoc :last-error nil
                            :last-result summary))))
        (cyder/touch! cyder-process-id)
        {:ok true :summary summary})
      (catch Throwable t
        (swap! !state
               (fn [s]
                 (-> s
                     (update :error-count inc)
                     (assoc :last-error (.getMessage t)))))
        (cyder/touch! cyder-process-id)
        {:ok false :error (.getMessage t)}))))

;; =============================================================================
;; Cyder registration
;; =============================================================================

(defn- register-with-cyder! []
  (cyder/deregister! cyder-process-id) ; idempotent — ignore not-registered
  (cyder/register!
   {:id cyder-process-id
    :type :daemon
    :layer :repl
    :stop-fn (fn [] (when (resolve 'futon3c.portfolio-inference.scheduler/stop!)
                      ((resolve 'futon3c.portfolio-inference.scheduler/stop!))))
    :state-fn status
    :step-fn (fn []
               (let [r (tick!)]
                 (if (:ok r)
                   (str "tick: " (pr-str (:summary r)))
                   (str "tick failed: " (:error r)))))
    :metadata {:purpose "Recurring portfolio-inference AIF tick"
               :cross-refs ["M-aif-head" "M-war-machine"
                            "M-stack-stereolithography"]
               :emits #{:portfolio/observation :portfolio/belief
                        :portfolio/policy :portfolio/step}}}))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- new-executor ^ScheduledExecutorService []
  (Executors/newScheduledThreadPool
   1
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. runnable "portfolio-inference-scheduler")
         (.setDaemon true))))))

(defn start!
  "Begin the recurring AIF tick.

   opts:
   - :period-seconds  override default cadence (default 3600)
   - :evidence-store  evidence store passed to portfolio-step!
   - :step-opts       opts map passed to portfolio-step! per tick
   - :run-on-start?   fire the first tick immediately (default true)

   Returns {:ok true :status status-map} or {:ok false :error string}."
  ([] (start! {}))
  ([{:keys [period-seconds evidence-store step-opts run-on-start?]
     :or {period-seconds default-period-seconds
          step-opts {}
          run-on-start? true}}]
   (let [{:keys [executor handle]} @!state]
     (when (and executor (not (.isShutdown ^ScheduledExecutorService executor)))
       (when handle
         (.cancel ^ScheduledFuture handle false))
       (.shutdown ^ScheduledExecutorService executor)))
   (let [executor (new-executor)
         _ (swap! !state assoc
                  :executor executor
                  :period-seconds period-seconds
                  :evidence-store evidence-store
                  :opts step-opts
                  :started-at (Instant/now)
                  :tick-count 0
                  :error-count 0
                  :last-tick-at nil
                  :last-error nil
                  :last-result nil)
         initial-delay (if run-on-start? 0 period-seconds)
         handle (.scheduleWithFixedDelay
                 executor
                 ^Runnable (fn [] (tick!))
                 (long initial-delay)
                 (long period-seconds)
                 TimeUnit/SECONDS)]
     (swap! !state assoc :handle handle)
     (register-with-cyder!)
     {:ok true :status (status)})))

(defn stop!
  "Cancel the scheduled task and shut down the executor. Idempotent."
  []
  (let [{:keys [executor handle]} @!state]
    (when handle
      (.cancel ^ScheduledFuture handle false))
    (when (and executor (not (.isShutdown ^ScheduledExecutorService executor)))
      (.shutdown ^ScheduledExecutorService executor)
      (try
        (when-not (.awaitTermination ^ScheduledExecutorService executor
                                     5 TimeUnit/SECONDS)
          (.shutdownNow ^ScheduledExecutorService executor))
        (catch InterruptedException _
          (.shutdownNow ^ScheduledExecutorService executor))))
    (swap! !state assoc :executor nil :handle nil)
    (cyder/deregister! cyder-process-id)
    {:ok true :status (status)}))

(defn set-period!
  "Change the tick period without dropping accumulated counters.
   Internally cancels the current handle and reschedules with the new
   period; tick-count/error-count are preserved."
  [new-period-seconds]
  (let [{:keys [executor handle evidence-store opts tick-count error-count]} @!state]
    (when (and executor (not (.isShutdown ^ScheduledExecutorService executor)))
      (when handle (.cancel ^ScheduledFuture handle false))
      (let [new-handle (.scheduleWithFixedDelay
                        ^ScheduledExecutorService executor
                        ^Runnable (fn [] (tick!))
                        (long new-period-seconds)
                        (long new-period-seconds)
                        TimeUnit/SECONDS)]
        (swap! !state assoc
               :handle new-handle
               :period-seconds new-period-seconds)
        (swap! !state assoc :tick-count tick-count :error-count error-count)
        {:ok true :status (status)}))))
