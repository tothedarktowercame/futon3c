(ns futon3c.clock.turn-trigger
  "E-arxana-clock car 3: a CLICK-counted time-driver — the every-≈N-turns
   trigger from README-clicks-and-ticks (the `:turn` subclass).

   It POLLS a click source (the invoke-jobs ledger by default) and fires its
   riders every N new clicks — no hot-path wiring (the WM reads click sources
   as-needed; it does not own them). It registers in `futon3c.cyder` so it
   shows on the Arxana Clock WITH a real cadence (\"every N clicks\") rather
   than the \"—\" the other in-JVM processes carry.

   First rider: the serving-JVM C-vector (belly) refresh — E-C-vector-live's
   \"alive in the running system\" (the cron loop already refreshes the cron-JVM
   belly; this keeps the LONG-RUNNING serving-JVM belly fresh between cron runs).

   Disciplines: cheap when not due (one count read); riders are isolated
   (one failing rider can't block the others); stoppable (cyder I-7)."
  (:require [futon3c.cyder :as cyder]))

(def default-threshold
  "Fire every this-many clicks (Joe: \"every ≈100 turns\")."
  100)

(def cyder-id "turn-trigger")

(defn ledger-click-count
  "Cumulative click count = size of the invoke-jobs ledger (best-effort; 0 if
   the ledger isn't resolvable). Each invoke-job is one agent turn / click."
  []
  (try
    (if-let [v (resolve 'futon3c.transport.http/recent-invoke-jobs)]
      (count (@v 1000000))
      0)
    (catch Throwable _ 0)))

(defonce !state
  (atom {:threshold       default-threshold
         :click-count-fn  ledger-click-count
         :last-fired-count nil        ; nil = baseline not yet set (no fire on first check)
         :last-fired-at   nil
         :fire-count      0
         :riders          {}          ; id -> zero-arg fn
         :loop-running?   false}))

;; ---------------------------------------------------------------------------
;; Riders
;; ---------------------------------------------------------------------------

(defn add-rider! [id f] (swap! !state update :riders assoc id f) id)
(defn remove-rider! [id] (swap! !state update :riders dissoc id) id)

(defn- fire-riders! []
  (doseq [[id f] (:riders @!state)]
    (try (f)
         (catch Throwable t
           (binding [*out* *err*]
             (println (str "[turn-trigger] rider " id " failed: " (.getMessage t))))))))

;; ---------------------------------------------------------------------------
;; The check (poll + maybe-fire)
;; ---------------------------------------------------------------------------

(defn check!
  "Read the click count; if >= threshold clicks elapsed since the last fire,
   fire the riders and advance the baseline. The FIRST call only sets the
   baseline (no immediate fire). Cheap + idempotent when not due. Returns a
   status map."
  []
  (let [{:keys [click-count-fn threshold last-fired-count]} @!state
        now (long (click-count-fn))]
    (cond
      (nil? last-fired-count)
      (do (swap! !state assoc :last-fired-count now)
          {:fired? false :clicks now :baseline-set now})

      (>= (- now last-fired-count) threshold)
      (do (fire-riders!)
          (swap! !state (fn [s] (-> s (assoc :last-fired-count now
                                             :last-fired-at (java.time.Instant/now))
                                    (update :fire-count inc))))
          {:fired? true :clicks now :elapsed (- now last-fired-count)})

      :else
      {:fired? false :clicks now
       :until-next (- threshold (- now last-fired-count))})))

(defn state-snapshot
  "Live state for the cyder registry / the clock display."
  []
  (let [s @!state
        now (long ((:click-count-fn s)))]
    {:threshold (:threshold s)
     :clicks now
     :last-fired-count (:last-fired-count s)
     :last-fired-at (some-> (:last-fired-at s) str)
     :fire-count (:fire-count s)
     :riders (vec (keys (:riders s)))
     :loop-running? (:loop-running? s)
     :until-next (when (:last-fired-count s)
                   (max 0 (- (:threshold s) (- now (:last-fired-count s)))))}))

;; ---------------------------------------------------------------------------
;; The belly rider (E-C-vector-live) — dynamic resolve, no compile-time dep
;; ---------------------------------------------------------------------------

(defn belly-refresh-rider
  "Refresh the serving-JVM C-vector if it changed (off-cycle). Uses
   requiring-resolve so futon3c carries no compile-time dep on futon2; a no-op
   if the belly ns isn't on the classpath."
  []
  (when-let [f (requiring-resolve 'futon2.aif.c-vector/maybe-refresh!)]
    (f)))

;; ---------------------------------------------------------------------------
;; Lifecycle (poll loop + cyder registration)
;; ---------------------------------------------------------------------------

(defn start!
  "Start the polling loop (every `:interval-ms`, default 60s) and register the
   trigger in cyder so it appears on the Arxana Clock. Idempotent. Optional
   `:threshold`."
  [& {:keys [threshold]}]
  ;; RETIRED (post-incident 2026-06-26): start! NO LONGER spawns a background
  ;; poll loop and adds NO belly rider. A perpetual `(future (while …))` in the
  ;; shared serving JVM, polling every cycle, was implicated in freezing the
  ;; futon3c evidence store (see E-arxana-clock "Incident & fix"). The belly is
  ;; now kept fresh by `futon2.aif.c-vector/ensure-belly-fresh!` — demand-driven
  ;; + debounced, at score time, reusing the established wm.scheduler tick — NOT
  ;; by this trigger. `check!` survives only as a passive, manually/event-callable
  ;; counter (no thread). It registers on the clock so it stays visible, marked
  ;; passive.
  (when threshold (swap! !state assoc :threshold threshold))
  (check!) ; set baseline (one read, no loop)
  (cyder/register!
   {:id cyder-id :type :daemon :layer :repl
    :stop-fn (fn [] (swap! !state assoc :loop-running? false))
    :state-fn state-snapshot
    :step-fn check!
    :metadata {:cadence (str "every " (:threshold @!state) " clicks (passive — no loop, no rider)")
               :source "invoke-jobs ledger" :driver :click :passive? true}})
  (state-snapshot))

(defn stop! []
  (swap! !state assoc :loop-running? false)
  (cyder/deregister! cyder-id)
  :stopped)
