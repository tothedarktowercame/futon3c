(ns futon3c.wm.runner-service
  "Single-flight, in-process service for one War Machine durée click.

   The full-loop implementation remains in Futon2. This service owns only
   serving-JVM lifecycle, direct apparatus visibility, and the HTTP-facing
   status projection."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]
           [java.util UUID]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file Files StandardCopyOption StandardOpenOption]))

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

(def ^:dynamic *click-run-binding-dir*
  "/home/joe/code/futon3c/data/wm-click-run-bindings")

(def ^:dynamic *resolve-var*
  "Resolver seam for tests. Production always delegates to requiring-resolve."
  requiring-resolve)

(def ^:dynamic *click-run-binding-persist-stage-hook*
  "Fault-injection seam. Stages are :temp-forced before rename and :renamed
   after authoritative replacement but before directory force."
  nil)

(defn status
  "Return the current click service status."
  []
  (assoc @!status :serving-runner-code
         (try
           (if-let [status-fn (*resolve-var* 'futon3c.wm.code-identity/status)]
             (status-fn)
             {:availability :unavailable :reason :identity-status-unresolvable})
           (catch Throwable throwable
             {:availability :unavailable
              :reason :identity-status-failed
              :error (or (ex-message throwable)
                         (.getName (class throwable)))}))))

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

(defn- safe-id [x]
  (str/replace (str x) #"[^A-Za-z0-9._-]" "_"))

(defn- persist-click-run-binding!
  [click-id result]
  (let [run-id (:run/id result)
        run-record-path (:run-record result)
        run-record (when run-record-path
                     (try (edn/read-string (slurp run-record-path))
                          (catch Throwable _ nil)))
        run-record-status
        (cond
          (nil? run-record-path) :absent
          (nil? run-record) :unavailable
          (not= run-id (:run/id run-record)) :identity-mismatch
          (not= click-id (:click/id run-record)) :identity-mismatch
          :else :present)
        record (cond-> {:schema :wm-click-run-binding-v1
                        :click/id click-id
                        :attempt/id (:attempt-id result)
                        :outcome (or (:outcome result) :unknown)
                        :run-record-status run-record-status
                        :recorded-at (str (Instant/now))}
                 run-record-path
                 (assoc :run-record run-record-path)

                 (not= :present run-record-status)
                 (assoc :run-record-absence
                        (case run-record-status
                          :absent (or (:run-record-absence result)
                                      :runner-did-not-return-run-record)
                          :unavailable :run-record-unreadable
                          :identity-mismatch :run-record-identity-mismatch))

                 (and (string? run-id) (not (str/blank? run-id)))
                 (assoc :run/id run-id :run-id-status :present)

                 (or (not (string? run-id)) (str/blank? (str run-id)))
                 (assoc :run-id-status :absent
                        :reason :runner-did-not-return-run-id))
        dir (io/file *click-run-binding-dir*)
        target (io/file dir (str "click-run-binding-" (safe-id click-id) ".edn"))
        tmp (io/file dir (str "." (.getName target) "." (UUID/randomUUID) ".tmp"))
        renamed? (volatile! false)]
    (io/make-parents target)
    (try
      (let [bytes (.getBytes (str (pr-str record) "\n") "UTF-8")]
        (with-open [channel (FileChannel/open
                             (.toPath tmp)
                             (into-array StandardOpenOption
                                         [StandardOpenOption/CREATE_NEW
                                          StandardOpenOption/WRITE
                                          StandardOpenOption/TRUNCATE_EXISTING]))]
          (let [buffer (ByteBuffer/wrap bytes)]
            (while (.hasRemaining buffer)
              (.write channel buffer)))
          (.force channel true)))
      (when *click-run-binding-persist-stage-hook*
        (*click-run-binding-persist-stage-hook*
         :temp-forced {:target target :temporary tmp}))
      (Files/move (.toPath tmp) (.toPath target)
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (vreset! renamed? true)
      (when *click-run-binding-persist-stage-hook*
        (*click-run-binding-persist-stage-hook*
         :renamed {:target target :temporary tmp}))
      (with-open [directory (FileChannel/open
                             (.toPath dir)
                             (into-array StandardOpenOption
                                         [StandardOpenOption/READ]))]
        (.force directory true))
      (assoc record
             :path (.getAbsolutePath target)
             :durability :confirmed)
      (catch Throwable throwable
        (if @renamed?
          ;; Atomic replacement is already authoritative. Report the weaker
          ;; durability guarantee without pretending the binding is absent.
          (assoc record
                 :path (.getAbsolutePath target)
                 :durability :unconfirmed
                 :durability-warning
                 {:error-class (.getName (class throwable))
                  :cause (or (ex-message throwable)
                             (.getName (class throwable)))})
          (throw (ex-info "click-run binding persistence failed"
                          {:path (.getAbsolutePath target)
                           :temporary-path (.getAbsolutePath tmp)
                           :committed? false
                           :durability :not-committed
                           :cause (or (ex-message throwable)
                                      (.getName (class throwable)))}
                          throwable))))
      (finally
        (Files/deleteIfExists (.toPath tmp))))))

(defn- result-summary
  [click-id result fallback-attempt-id]
  (let [binding (persist-click-run-binding! click-id result)]
    (cond-> {:click-id click-id
             :attempt-id (or (:attempt-id result) fallback-attempt-id)
             :outcome (or (:outcome result) :unknown)
             :run-id-status (:run-id-status binding)
             :run-record-status (:run-record-status binding)
             :run-binding (:path binding)
             :binding-durability (:durability binding)}
      (:durability-warning binding)
      (assoc :binding-durability-warning (:durability-warning binding))
      (:run-record binding)
      (assoc :run-record (:run-record binding))

      (:run-record-absence binding)
      (assoc :run-record-absence (:run-record-absence binding))

      (= :present (:run-id-status binding))
      (assoc :run/id (:run/id binding))

      (= :absent (:run-id-status binding))
      (assoc :run-id-absence (:reason binding)))))

(defn- close-click!
  [agent-id click-id result]
  (let [fallback-attempt-id (:attempt-id @!status)
        summary (result-summary click-id result fallback-attempt-id)]
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
                (assoc :click-id click-id
                       :phase-log-fn
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
