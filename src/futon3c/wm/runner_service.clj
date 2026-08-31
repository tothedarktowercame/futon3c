(ns futon3c.wm.runner-service
  "Single-flight, in-process service for one War Machine durée click.

   The full-loop implementation remains in Futon2. This service owns only
   serving-JVM lifecycle, direct apparatus visibility, and the HTTP-facing
   status projection."
  (:require [clojure.java.io :as io]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]
           [java.util UUID]))

(def war-machine-agent-id "war-machine")

(def initial-status
  {:running? false
   :click-id nil
   :phase nil
   :attempt-id nil
   :started-at nil
   :last-result nil
   :registry-publication nil})

(defonce !status
  (atom initial-status))

(defonce ^:private !completion
  ;; Completion is deliberately separate from the HTTP status projection.
  ;; Tests and callers may reset or sample !status while the worker is still
  ;; unwinding; that must not erase the only join handle for the actual thread.
  (atom nil))

(def ^:dynamic *resolve-var*
  "Resolver seam for tests. Production always delegates to requiring-resolve."
  requiring-resolve)

(defn status
  "Return the current click service status."
  []
  @!status)

(defn- ensure-apparatus!
  [agent-id]
  (or
   (reg/get-agent agent-id)
   (when (= war-machine-agent-id agent-id)
     (when-let [ensure! (*resolve-var*
                         'futon3c.wm.scheduler/ensure-war-machine-agent!)]
       (ensure!)))))

(defn- phase-activity
  [event click-id]
  (str (name (or (:phase event) :unknown))
       " "
       (or (:attempt-id event) click-id)))

(defn- publication-result!
  [click-id stage result]
  (swap! !status
         (fn [current]
           (if (= click-id (:click-id current))
             (assoc current :registry-publication
                    (assoc result :stage stage))
             current))))

(defn- publish-registry!
  "Publish the secondary registry projection after the authoritative service
   transition. Publication failure is loud and recorded, but cannot roll the
   in-process lifecycle backward or replace its terminal result."
  [click-id stage publish!]
  (try
    (publish!)
    (publication-result! click-id stage {:status :published})
    true
    (catch Throwable throwable
      (let [failure {:status :failed
                     :error-class (.getName (class throwable))
                     :cause (or (ex-message throwable)
                                (.getName (class throwable)))}]
        (publication-result! click-id stage failure)
        (binding [*out* *err*]
          (println "[wm-click] registry publication failed"
                   (pr-str (assoc failure
                                  :click-id click-id
                                  :stage stage))))
        false))))

(defn- report-phase!
  [agent-id click-id event]
  ;; The service projection is the lifecycle authority. Cross that boundary
  ;; before attempting the slower, fallible registry publication.
  (swap! !status
         (fn [current]
           (if (= click-id (:click-id current))
             (cond-> (assoc current
                            :phase (:phase event)
                            :registry-publication
                            {:status :pending :stage :phase})
               (:attempt-id event) (assoc :attempt-id (:attempt-id event)))
             current)))
  (let [activity (phase-activity event click-id)]
    (publish-registry!
     click-id :phase
     (fn []
       (ensure-apparatus! agent-id)
       (reg/update-agent!
        agent-id
        :agent/status :invoking
        :agent/invoke-activity activity
        :agent/invoke-started-at
        (or (:agent/invoke-started-at (reg/get-agent agent-id))
            (Instant/now)))))))

(defn- report-idle!
  [agent-id]
  ;; Clear the runner's legacy external-invoke source as well as the direct
  ;; fields. The runner may keep emitting those harmless duplicate reports;
  ;; the service's close boundary is authoritative for in-process lifecycle.
  (reg/mark-agent-idle! agent-id)
  (reg/clear-external-invoke! agent-id "wm-full-loop"))

(defn- configured-runner-opts
  [opts]
  (try
    (if-let [config-fn (*resolve-var*
                        'futon2.aif.full-loop-runner/config)]
      (config-fn opts)
      opts)
    (catch Throwable _
      opts)))

(defn- append-phase!
  [phase-log event]
  (when phase-log
    (io/make-parents phase-log)
    (spit phase-log (str (pr-str event) "\n") :append true)))

(defn- phase-sink
  [agent-id click-id configured]
  (let [delegate (:phase-log-fn configured)
        phase-log (:phase-log configured)]
    (fn [event]
      (report-phase! agent-id click-id event)
      (if delegate
        (delegate event)
        (append-phase! phase-log event)))))

(defn- in-process-selection
  ;; validated-selection, not current-selection: the phase1-4 allow-list is
  ;; the bounded-autonomy boundary (919d975); the in-process path must refuse
  ;; exactly what the HTTP bridge refuses (integration finding, M-omni-wm-runner).
  [request]
  (let [select (*resolve-var*
                'futon3c.peripheral.live-wm-selection/validated-selection)]
    {:ok true
     :selection (select request)}))

(defn- result-summary
  [result fallback-attempt-id]
  {:attempt-id (or (:attempt-id result) fallback-attempt-id)
   :outcome (or (:outcome result) :unknown)})

(defn- close-click!
  [agent-id click-id result]
  (let [fallback-attempt-id (:attempt-id @!status)
        summary (result-summary result fallback-attempt-id)]
    (swap! !status
           (fn [current]
             (if (= click-id (:click-id current))
               (assoc current
                      :running? false
                      :phase nil
                      :attempt-id (:attempt-id summary)
                      :last-result summary
                      :registry-publication {:status :pending :stage :close})
               current)))
    (publish-registry! click-id :close #(report-idle! agent-id))
    (println "[wm-click]" (pr-str (assoc summary :click-id click-id)))
    (flush)))

(defn- fail-click!
  [agent-id click-id throwable]
  (let [attempt-id (:attempt-id @!status)
        summary {:attempt-id attempt-id
                 :outcome :service-failed
                 :error (or (.getMessage ^Throwable throwable)
                            (.getName (class throwable)))}]
    (swap! !status
           (fn [current]
             (if (= click-id (:click-id current))
               (assoc current
                      :running? false
                      :phase nil
                      :last-result summary
                      :registry-publication {:status :pending :stage :failure})
               current)))
    (publish-registry! click-id :failure #(report-idle! agent-id))
    (println "[wm-click]" (pr-str (assoc summary :click-id click-id)))
    (flush)))

(defn- run-click!
  [click-id opts completion]
  (let [agent-id (or (:wm-agent-id opts) war-machine-agent-id)]
    (try
      (let [configured (configured-runner-opts opts)
            run! (*resolve-var*
                  'futon2.aif.full-loop-runner/run-opportunity!)
            runner-opts
            (-> configured
                (dissoc :wm-agent-id)
                (assoc :phase-log-fn
                       (phase-sink agent-id click-id configured)
                       :strategic-selection-invoke-fn
                       in-process-selection))]
        (close-click! agent-id click-id (run! runner-opts)))
      (catch Throwable throwable
        (fail-click! agent-id click-id throwable))
      (finally
        (deliver completion {:status :completed :click-id click-id})))))

(defn await-click!
  "Wait for the worker identified by `click-id`, independently of the mutable
   HTTP status projection. Returns a typed result; it never guesses completion
   from `:running? false`."
  ([click-id]
   (let [{tracked-id :click-id completion :completion} @!completion]
     (if (and completion (= click-id tracked-id))
       @completion
       {:status :not-tracked :click-id click-id})))
  ([click-id timeout-ms]
   (let [{tracked-id :click-id completion :completion thread :thread} @!completion]
     (if (and completion (= click-id tracked-id))
       (deref completion timeout-ms
              {:status :timed-out :click-id click-id :timeout-ms timeout-ms
               :thread-state (some-> ^Thread thread .getState str)
               :thread-at (some-> ^Thread thread .getStackTrace first str)})
       {:status :not-tracked :click-id click-id}))))

(defn click!
  "Start one in-process duration click.

   Returns {:click-id ... :started-at ...} when accepted. While a click is
   running, returns {:rejected :already-running :click-id ...} without
   starting another thread."
  [opts]
  (loop []
    (let [current @!status]
      (if (:running? current)
        {:rejected :already-running
         :click-id (:click-id current)}
        (let [click-id (str "wm-click-" (UUID/randomUUID))
              started-at (str (Instant/now))
              next-status (assoc current
                                 :running? true
                                 :click-id click-id
                                 :phase :starting
                                 :attempt-id nil
                                 :started-at started-at
                                 :registry-publication nil)]
          (if-not (compare-and-set! !status current next-status)
            (recur)
            (let [completion (promise)
                  _ (reset! !completion {:click-id click-id
                                         :completion completion})
                  runnable (bound-fn [] (run-click! click-id opts completion))
                  thread (Thread. ^Runnable runnable "wm-runner-click")]
              (swap! !completion assoc :thread thread)
              (.setDaemon thread true)
              (try
                (.start thread)
                {:click-id click-id :started-at started-at}
                (catch Throwable throwable
                  (try
                    (fail-click! (or (:wm-agent-id opts)
                                     war-machine-agent-id)
                                 click-id throwable)
                    (finally
                      (deliver completion {:status :start-failed
                                           :click-id click-id})))
                  (throw throwable))))))))))
