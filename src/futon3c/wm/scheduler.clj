(ns futon3c.wm.scheduler
  "Recurring War Machine snapshot cache.

   Runs `futon2.report.war-machine/generate-war-machine` on a background
   schedule and stores per-window snapshots in `!wm-snapshot` so HTTP reads
   are O(1) and never block on the filesystem walk / repo scan."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.cyder :as cyder]
            [futon3c.transport.http :as http])
  (:import [java.time Instant]
           [java.util.concurrent Executors ScheduledExecutorService
                                 ScheduledFuture TimeUnit]))

(declare stop!)

(defn- parse-long-safe [s]
  (when (and (string? s) (re-matches #"[0-9]+" s))
    (Long/parseLong s)))

(defn- configured-period-seconds []
  (or (some-> (System/getenv "FUTON_WM_SNAPSHOT_PERIOD_SECONDS") parse-long-safe)
      (some-> (System/getProperty "futon.wm.snapshot.period-seconds") parse-long-safe)
      300))

(defn- configured-days-windows []
  (let [raw (or (System/getenv "FUTON_WM_SNAPSHOT_DAYS_WINDOWS")
                (System/getProperty "futon.wm.snapshot.days-windows")
                "14")
        parsed (->> (str/split raw #",")
                    (map str/trim)
                    (keep parse-long-safe)
                    (mapv int))]
    (if (seq parsed) parsed [14])))

(def default-period-seconds
  (configured-period-seconds))

(def default-days-windows
  (configured-days-windows))

(def cyder-process-id "war-machine-scheduler")

(defonce !wm-snapshot
  (atom nil))

(defonce ^{:doc "Mutable scheduler state for the recurring WM snapshot task."}
  !state
  (atom {:executor nil
         :handle nil
         :period-seconds default-period-seconds
         :days-windows default-days-windows
         :started-at nil
         :tick-count 0
         :error-count 0
         :last-tick-at nil
         :last-success-at nil
         :last-error nil}))

;; Re-entrancy guard for tick! — set to true while a tick is mid-flight,
;; false otherwise.  `tick!` checks-and-sets via compare-and-set! so two
;; concurrent invocations (e.g. one scheduled, one manual via Drawbridge)
;; do NOT both run the 45-second scan in parallel; the second invocation
;; returns immediately with :reason :already-running.  This guard exists
;; because manual `(tick!)` invocations from Drawbridge proxied through
;; `proof-eval.sh` were killing the JVM (claude-1, 2026-05-25): the shell
;; client SIGKILLed at its own timeout, leaving the JVM either OOM'd by
;; concurrent 15MB scans or with a wedged eval thread.  See
;; ~/.claude/projects/-home-joe-code/memory/feedback_no_synchronous_heavy_drawbridge_calls.md
;; for the discipline; this guard is the structural backstop.
(defonce !tick-in-progress?
  (atom false))

(defn- compute-next-tick-at [last-tick-at period-seconds]
  (when (and last-tick-at period-seconds)
    (.plusSeconds ^Instant last-tick-at (long period-seconds))))

(defn status []
  (let [s @!state
        running? (and (some? (:executor s))
                      (not (.isShutdown ^ScheduledExecutorService (:executor s))))
        next-tick-at (when running?
                       (compute-next-tick-at (:last-tick-at s)
                                             (:period-seconds s)))
        snapshot @!wm-snapshot
        by-days (get snapshot :by-days {})]
    {:running? running?
     :period-seconds (:period-seconds s)
     :days-windows (:days-windows s)
     :started-at (some-> (:started-at s) str)
     :tick-count (:tick-count s)
     :error-count (:error-count s)
     :last-tick-at (some-> (:last-tick-at s) str)
     :last-success-at (some-> (:last-success-at s) str)
     :next-tick-at (some-> next-tick-at str)
     :last-error (:last-error s)
     :cached-days
     (into {}
           (map (fn [[days entry]]
                  [days {:as-of (some-> (:as-of entry) str)
                         :duration-ms (:duration-ms entry)
                         :body-bytes (:body-bytes entry)}]))
           by-days)}))

(defn snapshot-for-days [days]
  (get-in @!wm-snapshot [:by-days days]))

(defn- trim-action-predictions
  "Drop the heavy per-action :prediction/:next-belief (a full belief map ~115KB each × ~121
   actions ≈ 14MB, ~96% of the whole WM payload) from the judgement before serialization.
   It is a transient forward-model byproduct retained only to score the action; the WM viewer
   never reads it (core.cljs: 'this panel does not present predictions'). The light prediction
   fields (:next-observation, :predicted-events, :action) are kept. Shrinks /api/alpha/war-machine
   ~15MB → ~0.6MB. (Joe, 2026-06-12.)"
  [judgement]
  (if (seq (:ranked-actions judgement))
    (update judgement :ranked-actions
            (fn [acts]
              (mapv (fn [a]
                      (if (get-in a [:prediction :next-belief])
                        (update a :prediction dissoc :next-belief)
                        a))
                    acts)))
    judgement))

(defn- render-payload-json
  [{:keys [data judgement] :as bundle}]
  (let [{:keys [data judgement]} (http/wm-response-payload bundle)
        payload (http/stringify-wm-response
                 (-> data
                     (assoc :judgement (trim-action-predictions judgement))
                     (assoc :pilot-inhabitations (http/derive-pilot-inhabitations))))]
    {:payload payload
     :body (json/generate-string payload)}))

(defn- refresh-one-window! [generate days]
  (let [started-ns (System/nanoTime)
        started-at (Instant/now)
        bundle (-> (generate days)
                   http/apply-wm-operator-clear
                   ((requiring-resolve 'futon3c.wm.promote/apply-operator-promote)))
        {:keys [payload body]} (render-payload-json bundle)
        finished-at (Instant/now)
        duration-ms (long (/ (- (System/nanoTime) started-ns) 1000000))]
    (swap! !wm-snapshot assoc-in [:by-days days]
           {:days days
            :as-of finished-at
            :started-at started-at
            :duration-ms duration-ms
            :body-bytes (.length ^String body)
            :payload payload
            :body body})
    {:days days
     :as-of finished-at
     :duration-ms duration-ms
     :body-bytes (.length ^String body)}))

(defn tick!
  "Run one WM scheduler tick: regenerate the cached snapshot for each
   configured days-window.  BLOCKING for ~45 seconds (the heavy
   filesystem scan).

   WARNING — DO NOT call synchronously via Drawbridge / proof-eval.sh.
   The 45-second hold exceeds typical shell client timeouts; the resulting
   SIGKILL cascade has killed the futon3c JVM in the past (2026-05-25).
   Use `request-tick!` instead — it queues the tick on the background
   executor and returns immediately.

   Re-entrancy guard: if a tick is already in progress, this fn returns
   `{:ok false :reason :already-running}` without invoking the scan.  This
   prevents concurrent 15MB-scan ↔ JVM OOM scenarios and serialises both
   scheduled and manual ticks safely."
  []
  (if-not (compare-and-set! !tick-in-progress? false true)
    {:ok false :reason :already-running
     :note "tick! is already in progress; concurrent invocation refused"}
    (let [now (Instant/now)]
      (swap! !state assoc :last-tick-at now)
      (try
        (let [generate (requiring-resolve 'futon2.report.war-machine/generate-war-machine)
              days-windows (:days-windows @!state)
              refreshed (mapv #(refresh-one-window! generate %) days-windows)]
          (swap! !state
                 (fn [s]
                   (-> s
                       (update :tick-count inc)
                       (assoc :last-success-at (Instant/now)
                              :last-error nil))))
          (cyder/touch! cyder-process-id)
          {:ok true :refreshed refreshed})
        (catch Throwable t
          (swap! !state
                 (fn [s]
                   (-> s
                       (update :error-count inc)
                       (assoc :last-error (.getMessage t)))))
          (cyder/touch! cyder-process-id)
          {:ok false :error (.getMessage t)})
        (finally
          ;; Always clear the guard, even on Throwable — never leave the
          ;; scheduler permanently locked.
          (reset! !tick-in-progress? false))))))

(defn request-tick!
  "Async-safe replacement for direct `tick!` invocation.  Queues a tick on
   the background executor and returns immediately with the queued status.
   Safe to call from Drawbridge / proof-eval.sh — the caller is not held
   for the 45-second scan.

   Returns `{:ok true :queued? <bool>}`.  If the executor is not running
   or a tick is already in progress, the queued? flag indicates whether
   anything was actually scheduled."
  []
  (let [executor (:executor @!state)]
    (cond
      (nil? executor)
      {:ok false :reason :scheduler-not-started
       :note "Call (start!) first or rely on the recurring scheduled ticks"}

      (.isShutdown ^ScheduledExecutorService executor)
      {:ok false :reason :scheduler-shut-down}

      @!tick-in-progress?
      {:ok true :queued? false :reason :already-running
       :note "A tick is currently in flight; nothing queued"}

      :else
      (do
        (.execute ^ScheduledExecutorService executor ^Runnable (fn [] (tick!)))
        {:ok true :queued? true
         :note "Tick queued on background executor; observe progress via (status)"}))))

(defn- register-with-cyder! []
  (cyder/deregister! cyder-process-id)
  (cyder/register!
   {:id cyder-process-id
    :type :daemon
    :layer :repl
    :stop-fn (fn []
               (when (resolve 'futon3c.wm.scheduler/stop!)
                 ((resolve 'futon3c.wm.scheduler/stop!))))
    :state-fn status
    :step-fn (fn []
               (let [r (tick!)]
                 (if (:ok r)
                   (str "tick: " (pr-str (:refreshed r)))
                   (str "tick failed: " (:error r)))))
    :metadata {:purpose "Recurring War Machine snapshot refresh"
               :days-windows default-days-windows}}))

(defn- new-executor ^ScheduledExecutorService []
  (Executors/newScheduledThreadPool
   1
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. runnable "war-machine-scheduler")
         (.setDaemon true))))))

(defn start!
  ([] (start! {}))
  ([{:keys [period-seconds days-windows run-on-start?]
     :or {period-seconds default-period-seconds
          days-windows default-days-windows
          run-on-start? true}}]
   (let [{:keys [executor handle]} @!state]
     (when (and executor (not (.isShutdown ^ScheduledExecutorService executor)))
       (when handle
         (.cancel ^ScheduledFuture handle false))
       (.shutdown ^ScheduledExecutorService executor)))
   (let [executor (new-executor)
         days-windows (->> days-windows (map int) distinct vec)
         _ (swap! !state assoc
                  :executor executor
                  :period-seconds (long period-seconds)
                  :days-windows days-windows
                  :started-at (Instant/now)
                  :tick-count 0
                  :error-count 0
                  :last-tick-at nil
                  :last-success-at nil
                  :last-error nil)
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

(defn stop! []
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

(defn set-period! [new-period-seconds]
  (let [{:keys [executor handle tick-count error-count]} @!state]
    (when (and executor (not (.isShutdown ^ScheduledExecutorService executor)))
      (when handle
        (.cancel ^ScheduledFuture handle false))
      (let [new-handle (.scheduleWithFixedDelay
                        ^ScheduledExecutorService executor
                        ^Runnable (fn [] (tick!))
                        (long new-period-seconds)
                        (long new-period-seconds)
                        TimeUnit/SECONDS)]
        (swap! !state assoc
               :handle new-handle
               :period-seconds (long new-period-seconds)
               :tick-count tick-count
               :error-count error-count)
        {:ok true :status (status)}))))

(defn request-window!
  "Ensure DAYS is part of the recurring snapshot set and kick an immediate
   refresh on the background executor when it is newly requested."
  [days]
  (let [days (int days)
        before (:days-windows @!state)
        already? (some #{days} before)]
    (when-not already?
      (swap! !state update :days-windows
             (fn [windows]
               (->> (conj (vec windows) days) distinct vec))))
    (when (and (not already?) (:executor @!state))
      (.execute ^ScheduledExecutorService (:executor @!state)
                ^Runnable (fn [] (tick!))))
    {:ok true
     :already-tracked? already?
     :status (status)}))

(defn ensure-started! []
  (let [{:keys [executor]} @!state]
    (if (and executor (not (.isShutdown ^ScheduledExecutorService executor)))
      {:ok true :status (status)}
      (start!))))
