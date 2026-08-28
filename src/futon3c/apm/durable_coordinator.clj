(ns futon3c.apm.durable-coordinator
  "Typed, restartable coordinator registration over `live-regulator`.

   Adapters decide pure next actions. Activation is always a later tick: the
   deterministic intent is first stored in regulator state, then handed to the
   adapter's idempotent reconcile function. Registry entries are typed and
   content-addressed; startup never infers coordinators from directories."
  (:require [clojure.edn :as edn]
            [futon3c.apm.live-preflight-runtime :as persistence]
            [futon3c.apm.live-regulator :as regulator]
            [futon3c.apm.semantic-progress-watchdog :as watchdog])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path]
           [java.security MessageDigest]))

(def registry-type :durable-coordinator-registry)
(def registry-version 1)
(def entry-type :durable-coordinator-registration)
(def intent-type :durable-coordinator-intent)
(defonce ^:private adapters (atom {}))
(def ^:dynamic *watchdog-now-fn* #(System/currentTimeMillis))
(def ^:dynamic *enabled-transition-now-fn* #(System/currentTimeMillis))
(def ^:dynamic *quiescence-now-fn* #(str (java.time.Instant/now)))
(def ^:dynamic *intent-recovery-now-fn* #(System/currentTimeMillis))
(def ^:dynamic *watchdog-start-fn* watchdog/start!)
(def ^:dynamic *watchdog-stop-fn* watchdog/stop!)
(def ^:dynamic *watchdog-running-fn* watchdog/running?)
(def watchdog-rearm-limit 3)
(def watchdog-rearm-window-ms 60000)
(declare stop!)

(defn- canonical [value]
  (cond
    (map? value) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [[k v]] [k (canonical v)])) value)
    (vector? value) (mapv canonical value)
    (set? value) (into (sorted-set) (map canonical) value)
    (sequential? value) (mapv canonical value)
    :else value))

(defn- sha256 [value]
  (binding [*print-namespace-maps* false
            *print-meta* false
            *print-length* nil
            *print-level* nil]
    (let [bytes (.getBytes (pr-str (canonical value)) StandardCharsets/UTF_8)
          digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
      (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))))

(defn entry-digest [entry]
  (sha256 (dissoc entry :coordinator/entry-digest)))

(defn enabled-transition-digest [transition]
  (sha256 (dissoc transition :transition/digest)))

(defn intent-digest [intent]
  (sha256 (dissoc intent :intent/digest)))

(defn state-digest [state]
  (sha256 (dissoc state :coordinator/pending-intent
                  :coordinator/pending-pre-state-digest)))

(defn make-intent [coordinator-id state requested]
  (let [intent {:state/type intent-type
                :coordinator/id coordinator-id
                :job-id (:job-id requested)
                :dispatch/id (:dispatch/id requested)
                :dispatch/action (:dispatch/action requested)
                :dispatch/parameters (:dispatch/parameters requested)
                :pre-state/version (:regulator/ticks state)
                :pre-state/digest (state-digest state)
                :expected/postcondition (:expected/postcondition requested)}]
    (assoc intent :intent/digest (intent-digest intent))))

(defn intent-findings [coordinator-id state intent]
  (cond-> []
    (not= intent-type (:state/type intent)) (conj :type)
    (not= coordinator-id (:coordinator/id intent)) (conj :coordinator-id)
    (not (and (string? (:job-id intent)) (not-empty (:job-id intent))))
    (conj :job-id)
    (not (and (string? (:dispatch/id intent))
              (not-empty (:dispatch/id intent))))
    (conj :dispatch-id)
    (not (keyword? (:dispatch/action intent))) (conj :dispatch-action)
    (and (some? (:dispatch/parameters intent))
         (not (map? (:dispatch/parameters intent))))
    (conj :dispatch-parameters)
    (not (nat-int? (:pre-state/version intent))) (conj :pre-state-version)
    (and (nat-int? (:pre-state/version intent))
         (< (:regulator/ticks state) (inc (:pre-state/version intent))))
    (conj :pre-state-version-relationship)
    (not (string? (:pre-state/digest intent))) (conj :pre-state-digest)
    (not= (:pre-state/digest intent)
          (:coordinator/pending-pre-state-digest state))
    (conj :pre-state-binding)
    (not (map? (:expected/postcondition intent))) (conj :expected-postcondition)
    (not= (:intent/digest intent) (intent-digest intent)) (conj :intent-digest)))

(defn valid-intent? [coordinator-id state intent]
  (empty? (intent-findings coordinator-id state intent)))

(defn postcondition-satisfied? [expected result]
  (and
   (if-let [allowed (:status/one-of expected)]
     (contains? (set allowed) (or (get-in result [:queue/result :status])
                                  (:status result)))
     true)
   (if-let [allowed (:ruling/one-of expected)]
     (contains? (set allowed) (or (get-in result [:lane/result :ruling])
                                  (:ruling result)))
     true)))

(defn valid-entry? [entry]
  (and (= entry-type (:state/type entry))
       (string? (:coordinator/id entry))
       (not-empty (:coordinator/id entry))
       (keyword? (:coordinator/adapter entry))
       (map? (:coordinator/config entry))
       (string? (:coordinator/state-path entry))
       (pos-int? (:coordinator/period-ms entry))
       (boolean? (:coordinator/enabled? entry))
       (or (nil? (:coordinator/lifecycle entry))
           (contains? #{:running :draining} (:coordinator/lifecycle entry)))
       (or (nil? (:coordinator/enabled-history entry))
           (and (vector? (:coordinator/enabled-history entry))
                (every?
                 (fn [transition]
                   (and (= :durable-coordinator-enabled-transition
                           (:state/type transition))
                        (= (:coordinator/id entry)
                           (:coordinator/id transition))
                        (or (nil? (:enabled/previous transition))
                            (boolean? (:enabled/previous transition)))
                        (boolean? (:enabled/new transition))
                        (keyword? (:transition/actor transition))
                        (keyword? (:transition/reason transition))
                        (nat-int? (:transition/timestamp-ms transition))
                        (string? (:durable-state/digest transition))
                        (= (:transition/digest transition)
                           (enabled-transition-digest transition))))
                 (:coordinator/enabled-history entry))))
       (or (nil? (:coordinator/problem-id entry))
           (and (string? (:coordinator/problem-id entry))
                (not-empty (:coordinator/problem-id entry))))
       (or (nil? (:retry/count entry)) (nat-int? (:retry/count entry)))
       (or (nil? (:retry/max entry)) (nat-int? (:retry/max entry)))
       (or (nil? (:retry/max entry))
           (<= (or (:retry/count entry) 0) (:retry/max entry)))
       (= (:coordinator/entry-digest entry) (entry-digest entry))))

(defn register-adapter!
  "Register the process-local constructor for a typed adapter key.

   CONSTRUCTOR receives persisted config and returns `:decide-fn` and
   `:reconcile-fn`. Decide is pure. Reconcile may observe/activate the already
   persisted deterministic intent and therefore must be idempotent."
  [adapter-key constructor]
  (if (and (keyword? adapter-key) (fn? constructor))
    (do (swap! adapters assoc adapter-key constructor)
        {:ok true :adapter adapter-key})
    {:ok false :error/code :durable-coordinator-adapter-invalid}))

(defn- read-edn [path]
  (let [p (Path/of (str path) (make-array String 0))]
    (when (Files/isRegularFile p (make-array LinkOption 0))
      (edn/read-string (slurp (str p))))))

(defn read-registry [registry-path]
  (or (read-edn registry-path)
      {:state/type registry-type :registry/version registry-version :entries {}}))

(defn- valid-registry? [registry]
  (and (= registry-type (:state/type registry))
       (= registry-version (:registry/version registry))
       (map? (:entries registry))
       (every? (fn [[id entry]]
                 (and (= id (:coordinator/id entry)) (valid-entry? entry)))
               (:entries registry))))

(defn register!
  "Persist one typed coordinator registration before it can be started."
  [{:keys [registry-path coordinator-id problem-id retry-count retry-max
           adapter config state-path period-ms]
    :or {period-ms regulator/default-period-ms}}]
  (let [registry (read-registry registry-path)
        state-at-registration (read-edn state-path)
        initial-transition
        {:state/type :durable-coordinator-enabled-transition
         :coordinator/id coordinator-id
         :enabled/previous nil
         :enabled/new true
         :transition/actor :durable-coordinator/register!
         :transition/reason :registration
         :transition/timestamp-ms (*enabled-transition-now-fn*)
         :durable-state/digest (state-digest state-at-registration)}
        initial-transition
        (assoc initial-transition :transition/digest
               (enabled-transition-digest initial-transition))
        entry (cond-> {:state/type entry-type
                       :coordinator/id coordinator-id
                       :coordinator/adapter adapter
                       :coordinator/config (or config {})
                       :coordinator/state-path (str state-path)
                       :coordinator/period-ms period-ms
                       :coordinator/enabled? true
                       :coordinator/lifecycle :running
                       :coordinator/enabled-history [initial-transition]}
                problem-id (assoc :coordinator/problem-id problem-id
                                  :retry/count (or retry-count 0)
                                  :retry/max (or retry-max 0))
                true (assoc :coordinator/entry-digest nil))
        entry (assoc entry :coordinator/entry-digest (entry-digest entry))]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (not (valid-entry? entry))
      {:ok false :error/code :durable-coordinator-registration-invalid}
      :else
      (let [existing (get-in registry [:entries coordinator-id])
            other-for-problem
            (when problem-id
              (some (fn [[id candidate]]
                      (when (and (not= id coordinator-id)
                                 (= problem-id
                                    (:coordinator/problem-id candidate)))
                        id))
                    (:entries registry)))]
        (cond
          other-for-problem
          {:ok false :error/code :durable-coordinator-problem-already-registered
           :finding {:problem-id problem-id
                     :coordinator/id other-for-problem}}

          (and existing (not= existing entry))
          {:ok false :error/code :durable-coordinator-registration-conflict
           :finding {:coordinator/id coordinator-id}}

          :else
          (let [saved (persistence/atomic-persist!
                       (Path/of (str registry-path) (make-array String 0))
                       (assoc-in registry [:entries coordinator-id] entry))]
            (if (:ok saved)
              {:ok true :status (if existing :already-registered :registered)
               :entry entry}
              saved)))))))

(defn retry!
  "Advance the bounded retry counter on the one registered coordinator.

   A retry mutates only the content-addressed registry entry; callers then
   resume the same coordinator. It never manufactures a successor identity."
  [registry-path coordinator-id]
  (let [registry (read-registry registry-path)
        entry (get-in registry [:entries coordinator-id])]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (nil? entry)
      {:ok false :error/code :durable-coordinator-not-registered}
      (nil? (:coordinator/problem-id entry))
      {:ok false :error/code :durable-coordinator-problem-identity-missing}
      (>= (:retry/count entry) (:retry/max entry))
      {:ok false :error/code :durable-coordinator-retry-exhausted
       :finding {:problem-id (:coordinator/problem-id entry)
                 :retry/count (:retry/count entry)
                 :retry/max (:retry/max entry)}}
      :else
      (let [updated (-> entry
                        (update :retry/count inc)
                        (assoc :coordinator/entry-digest nil))
            updated (assoc updated :coordinator/entry-digest
                           (entry-digest updated))
            saved (persistence/atomic-persist!
                   (Path/of (str registry-path) (make-array String 0))
                   (assoc-in registry [:entries coordinator-id] updated))]
        (if (:ok saved)
          {:ok true :status :retry-registered :entry updated}
          saved)))))

(defn- coordinator-tick [coordinator-id adapter state]
  (if-let [intent (:coordinator/pending-intent state)]
    (if-not (valid-intent? coordinator-id state intent)
      {:ok false :error/code :durable-coordinator-intent-integrity-invalid
       :findings (intent-findings coordinator-id state intent)}
      (let [result ((:reconcile-fn adapter) intent state)]
        (cond
          (not (:ok result)) result
          (not (postcondition-satisfied?
                (:expected/postcondition intent) result))
          {:ok false :error/code :durable-coordinator-postcondition-violated
           :finding {:expected (:expected/postcondition intent)
                     :result (dissoc result :regulator/state-updates)}}
          :else
          (cond-> (dissoc result :coordinator/clear-intent?)
            (:coordinator/clear-intent? result)
            (update :regulator/state-updates merge
                    {:coordinator/pending-intent nil
                     :coordinator/pending-pre-state-digest nil
                     :coordinator/last-settled-intent intent})))))
    (let [decision ((:decide-fn adapter) state)]
      (cond
        (not (:ok decision)) decision
        (= :activate (:coordinator/action decision))
        (let [intent (make-intent coordinator-id state
                                  (:coordinator/intent decision))]
          (if (and (map? (:coordinator/intent decision))
                   (= (:pre-state/version intent) (:regulator/ticks state))
                   (string? (:job-id intent))
                   (not-empty (:job-id intent))
                   (string? (:dispatch/id intent))
                   (not-empty (:dispatch/id intent))
                   (keyword? (:dispatch/action intent))
                   (map? (:expected/postcondition intent)))
            {:ok true :status :intent-persisted
             :job-id (:job-id intent)
             :regulator/state-updates
             (merge (:regulator/state-updates decision)
                    {:coordinator/pending-intent intent
                     :coordinator/pending-pre-state-digest
                     (:pre-state/digest intent)})}
            {:ok false :error/code :durable-coordinator-intent-invalid}))
        :else decision))))

(defn- watchdog-id [coordinator-id]
  (str "semantic-progress:" coordinator-id))

(defn- watchdog-state-path [entry]
  (Path/of (str (:coordinator/state-path entry) ".watchdog.edn")
           (make-array String 0)))

(defn- watchdog-rearm-state-path [entry]
  (Path/of (str (:coordinator/state-path entry) ".watchdog-rearms.edn")
           (make-array String 0)))

(defn- intent-deadline [intent]
  (or (:dispatch/deadline intent)
      (get-in intent [:dispatch/parameters :deadline])
      (get-in intent [:dispatch/parameters :deadline-ms])))

(defn supersede-expired-intent!
  "Archive an expired pending intent before clearing it. The two durable
   writes run under the coordinator tick lock. A crash after the archive write
   is recoverable: replay recognizes the same intent digest and performs only
   the clearing write. Live intents are never superseded."
  [registry-path coordinator-id]
  (let [entry (get-in (read-registry registry-path) [:entries coordinator-id])]
    (cond
      (nil? entry)
      {:ok false :error/code :durable-coordinator-not-registered}

      :else
      (let [state-path (Path/of (:coordinator/state-path entry)
                                (make-array String 0))
            tick-lock (regulator/with-file-tick-lock
                       (Path/of (str state-path ".tick-claim.lock")
                                (make-array String 0)))]
        (tick-lock
         (fn []
           (let [state (read-edn state-path)
                 intent (:coordinator/pending-intent state)
                 now-ms (long (*intent-recovery-now-fn*))
                 deadline (some-> intent intent-deadline long)
                 grace-ms (long watchdog/external-deadline-grace-ms)
                 disposition-id (:intent/digest intent)
                 archived? (some #(= disposition-id (:intent/digest %))
                                 (:coordinator/superseded-intents state))]
             (cond
               (nil? intent)
               {:ok true :status :no-pending-intent :state state}

               (not (valid-intent? coordinator-id state intent))
               {:ok false
                :error/code :durable-coordinator-intent-integrity-invalid
                :findings (intent-findings coordinator-id state intent)}

               (nil? deadline)
               {:ok false
                :error/code :durable-coordinator-intent-deadline-missing
                :finding {:job-id (:job-id intent)}}

               (<= now-ms (+ deadline grace-ms))
               {:ok false
                :error/code :durable-coordinator-intent-not-expired
                :finding {:job-id (:job-id intent) :deadline-ms deadline
                          :grace-ms grace-ms :observed-at-ms now-ms}}

               :else
               (let [disposition
                     {:state/type :durable-coordinator-intent-disposition
                      :intent/digest disposition-id
                      :intent intent
                      :disposition :expired
                      :deadline-ms deadline
                      :grace-ms grace-ms
                      :disposed-at-ms now-ms}
                     archived-state
                     (if archived?
                       state
                       (update state :coordinator/superseded-intents
                               (fnil conj []) disposition))
                     archived-write
                     (if archived?
                       {:ok true}
                       (persistence/atomic-persist! state-path archived-state))]
                 (if-not (:ok archived-write)
                   {:ok false
                    :error/code :durable-coordinator-intent-archive-failed
                    :finding archived-write}
                   (let [cleared (-> archived-state
                                     (assoc :coordinator/pending-intent nil
                                            :coordinator/pending-pre-state-digest nil
                                            :coordinator/last-superseded-intent
                                            disposition))
                         cleared-write
                         (persistence/atomic-persist! state-path cleared)]
                     (if (:ok cleared-write)
                       {:ok true :status :expired-intent-superseded
                        :disposition disposition :state cleared}
                       {:ok false
                        :error/code :durable-coordinator-intent-clear-failed
                        :finding cleared-write}))))))))))))

(defn watchdog-observation [entry state]
  (let [intent (:coordinator/pending-intent state)
        delayed-retry (:coordinator/delayed-retry state)
        result (:regulator/last-result state)]
    (cond->
     {:cursor {:frame-id (or (:frame-id state)
                             (:frame/id state)
                             (:coordinator/problem-id entry))
               :phase (:phase state)
               :attempt-ordinal (or (:attempt-ordinal state)
                                    (:submission/attempt state))
               :obligation/status (or (:obligation/status state)
                                      (:status result))
               :active-job-id (or (:job-id intent)
                                  (:retry/id delayed-retry))
               :last-committed-event-id
               (or (:last-committed-event-id state)
                   (:event/id state))}
      :coordinator-enabled? (:coordinator/enabled? entry)
      :regulator state
      :tick-claim (:regulator/tick-claim state)
      :reconciliation/status (:regulator/reconciliation state)
      :supervisor/status (when (= :running (:regulator/status state)) :ready)
      :invalid-state? (and (some? state)
                           (not= :live-regulator (:state/type state)))
      :failed-launch-audit?
      (= :live-supervisor-launch-audit-failed (:error/code result))}
      intent (assoc :awaiting-job {:job-id (:job-id intent)
                                   :deadline (intent-deadline intent)})
      (and (nil? intent) delayed-retry)
      (assoc :awaiting-job {:job-id (:retry/id delayed-retry)
                            :deadline (:not-before-ms delayed-retry)}))))

(defn- arm-watchdog! [registry-path entry]
  (let [id (watchdog-id (:coordinator/id entry))
        state-path (Path/of (:coordinator/state-path entry)
                            (make-array String 0))
        watch-path (watchdog-state-path entry)
        watch-fn #(watchdog/check!
                   {:watch-state (persistence/read-state watch-path)
                    :observation (watchdog-observation
                                  entry (persistence/read-state state-path))
                    :now-ms (*watchdog-now-fn*)
                    :registry-path registry-path
                    :coordinator-id (:coordinator/id entry)
                    :persist-fn (fn [state]
                                  (persistence/atomic-persist! watch-path
                                                               state))})]
    (*watchdog-start-fn* {:watchdog-id id :watch-fn watch-fn})))

(defn- claim-watchdog-rearm! [entry]
  (let [path (watchdog-rearm-state-path entry)
        now (*watchdog-now-fn*)
        earliest (- now watchdog-rearm-window-ms)
        prior (or (read-edn path) {})
        attempts (->> (:watchdog/rearm-attempts-ms prior)
                      (filter #(<= earliest % now))
                      vec)]
    (if (>= (count attempts) watchdog-rearm-limit)
      {:ok false
       :error/code :durable-coordinator-watchdog-rearm-limit-exceeded
       :finding {:coordinator/id (:coordinator/id entry)
                 :attempts (count attempts)
                 :limit watchdog-rearm-limit
                 :window-ms watchdog-rearm-window-ms}}
      (let [updated {:state/type :durable-coordinator-watchdog-rearms
                     :coordinator/id (:coordinator/id entry)
                     :watchdog/rearm-attempts-ms (conj attempts now)}
            saved (persistence/atomic-persist! path updated)]
        (if (:ok saved)
          {:ok true :attempt (count (:watchdog/rearm-attempts-ms updated))}
          {:ok false
           :error/code :durable-coordinator-watchdog-rearm-record-failed
           :finding saved})))))

(defn- ensure-watchdog! [registry-path entry]
  (let [id (watchdog-id (:coordinator/id entry))
        rearm-path (watchdog-rearm-state-path entry)
        with-lock (regulator/with-file-tick-lock
                   (Path/of (str rearm-path ".lock") (make-array String 0)))]
    (with-lock
      (fn []
        (if (*watchdog-running-fn* id)
          {:ok true :status :already-running :watchdog-id id}
          (let [claimed (claim-watchdog-rearm! entry)]
            (if-not (:ok claimed)
              claimed
              (let [armed (arm-watchdog! registry-path entry)
                    running? (and (:ok armed) (*watchdog-running-fn* id))]
                (if running?
                  {:ok true :status :rearmed :watchdog-id id
                   :attempt (:attempt claimed) :arming armed}
                  {:ok false
                   :error/code :durable-coordinator-watchdog-rearm-failed
                   :finding {:coordinator/id (:coordinator/id entry)
                             :attempt (:attempt claimed)
                             :arming armed
                             :running-after-arm? false}})))))))))

(defn- halt-for-watchdog-repair! [registry-path entry repair]
  (let [halted (stop! registry-path (:coordinator/id entry))]
    {:ok false
     :error/code :durable-coordinator-watchdog-repair-failed
     :finding {:watchdog/repair repair
               :halted halted}}))

(defn start-entry!
  ([entry]
   {:ok false :error/code :durable-coordinator-watchdog-authority-missing
    :finding {:coordinator/id (:coordinator/id entry)}})
  ([registry-path entry]
  (cond
    (not (valid-entry? entry))
    {:ok false :error/code :durable-coordinator-registration-invalid}
    (not (:coordinator/enabled? entry))
    (let [state (read-edn (:coordinator/state-path entry))
          claim (:regulator/tick-claim state)]
      (*watchdog-stop-fn* (watchdog-id (:coordinator/id entry)))
      (cond-> {:ok true
               :status (if claim :draining :disabled)
               :coordinator/id (:coordinator/id entry)}
        claim (assoc :in-flight-tick claim)
        (:regulator/quiescence-witness state)
        (assoc :quiescence-witness
               (:regulator/quiescence-witness state))))
    (nil? (get @adapters (:coordinator/adapter entry)))
    {:ok false :error/code :durable-coordinator-adapter-unavailable
     :finding {:adapter (:coordinator/adapter entry)}}
    :else
    (let [adapter ((get @adapters (:coordinator/adapter entry))
                   (assoc (:coordinator/config entry)
                          :coordinator/period-ms
                          (:coordinator/period-ms entry)))]
      (if-not (and (map? adapter) (fn? (:decide-fn adapter))
                   (fn? (:reconcile-fn adapter)))
        {:ok false :error/code :durable-coordinator-adapter-provider-invalid}
        (let [state-path (Path/of (:coordinator/state-path entry)
                                  (make-array String 0))
              watchdog-ready (ensure-watchdog! registry-path entry)]
          (if-not (:ok watchdog-ready)
            (halt-for-watchdog-repair! registry-path entry watchdog-ready)
            (let [started (regulator/start!
                           {:regulator-id (:coordinator/id entry)
                            :period-ms (:coordinator/period-ms entry)
                            :read-fn #(persistence/read-state state-path)
                            :persist-fn #(persistence/atomic-persist!
                                          state-path %)
                            :with-tick-lock-fn
                            (regulator/with-file-tick-lock
                             (Path/of (str state-path ".tick-claim.lock")
                                      (make-array String 0)))
                            :claim-allowed-fn
                            #(let [current (get-in (read-registry registry-path)
                                                   [:entries (:coordinator/id entry)])]
                               (and (:coordinator/enabled? current)
                                    (= :running
                                       (or (:coordinator/lifecycle current)
                                           :running))))
                            :tick-state-fn
                            (fn [state]
                              (let [repair (ensure-watchdog! registry-path entry)]
                                (if (:ok repair)
                                  (coordinator-tick (:coordinator/id entry)
                                                    adapter state)
                                  (halt-for-watchdog-repair!
                                   registry-path entry repair))))})
                  post-start-watchdog (ensure-watchdog! registry-path entry)]
              (cond
                (not (:ok started))
                (do (*watchdog-stop-fn* (watchdog-id (:coordinator/id entry)))
                    started)
                (not (:ok post-start-watchdog))
                (halt-for-watchdog-repair!
                 registry-path entry post-start-watchdog)
                :else
                (assoc started :watchdog post-start-watchdog))))))))))

(defn start-registered!
  "Start one coordinator solely from its typed registry entry."
  [registry-path coordinator-id]
  (let [registry (read-registry registry-path)]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (nil? (get-in registry [:entries coordinator-id]))
      {:ok false :error/code :durable-coordinator-not-registered}
      :else (start-entry! registry-path
                          (get-in registry [:entries coordinator-id])))))

(defn recover-all!
  "Start every typed registry entry. No filesystem discovery is performed."
  [registry-path]
  (let [registry (read-registry registry-path)]
    (if-not (valid-registry? registry)
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (let [results (into (sorted-map)
                          (map (fn [[id entry]]
                                 [id (start-entry! registry-path entry)]))
                          (:entries registry))]
        {:ok (every? :ok (vals results)) :results results}))))

(defn status
  ([coordinator-id] (regulator/status coordinator-id))
  ([registry-path coordinator-id]
   (let [entry (get-in (read-registry registry-path) [:entries coordinator-id])]
     (when entry
       (let [durable-state (read-edn (:coordinator/state-path entry))]
         {:registration entry
          :runtime (regulator/status coordinator-id)
          :durable-state durable-state
          :tick-claim (:regulator/tick-claim durable-state)
          :reconciliation/status
          (:regulator/reconciliation durable-state)})))))

(defn- set-enabled! [registry-path coordinator-id enabled? actor reason]
  (let [registry (read-registry registry-path)
        entry (get-in registry [:entries coordinator-id])]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (nil? entry)
      {:ok false :error/code :durable-coordinator-not-registered}
      :else
      (let [durable-state (read-edn (:coordinator/state-path entry))
            transition {:state/type :durable-coordinator-enabled-transition
                        :coordinator/id coordinator-id
                        :enabled/previous (:coordinator/enabled? entry)
                        :enabled/new enabled?
                        :transition/actor actor
                        :transition/reason reason
                        :transition/timestamp-ms (*enabled-transition-now-fn*)
                        :durable-state/digest (state-digest durable-state)}
            transition (assoc transition :transition/digest
                              (enabled-transition-digest transition))
            updated (-> entry
                        (assoc :coordinator/enabled? enabled?)
                        (assoc :coordinator/lifecycle
                               (if enabled? :running :draining))
                        (update :coordinator/enabled-history
                                (fnil conj []) transition))
            updated (assoc updated :coordinator/entry-digest
                           (entry-digest updated))]
        (persistence/atomic-persist!
         (Path/of (str registry-path) (make-array String 0))
         (assoc-in registry [:entries coordinator-id] updated))))))

(defn cancel-scheduler!
  "Process-local scheduler cancellation for test cleanup and internal failure
  handling. It is deliberately not named stop: no durable witness is written."
  [coordinator-id]
  (regulator/cancel-scheduler! coordinator-id))

(defn stop!
  "Durably drain a coordinator. Returns :stopped only when the state file
  contains a quiescence witness and no tick claim; otherwise names the durable
  in-flight tick and leaves the coordinator in :draining."
  [registry-path coordinator-id]
  (let [disabled (set-enabled! registry-path coordinator-id false
                               :durable-coordinator/stop!
                               :stop-requested)]
    (if-not (:ok disabled)
      disabled
      (let [entry (get-in (read-registry registry-path)
                          [:entries coordinator-id])
            state-path (Path/of (:coordinator/state-path entry)
                                (make-array String 0))
            tick-lock (regulator/with-file-tick-lock
                       (Path/of (str state-path ".tick-claim.lock")
                                (make-array String 0)))
            watchdog-stopped (*watchdog-stop-fn* (watchdog-id coordinator-id))
            scheduler (regulator/cancel-scheduler! coordinator-id)
            observed (read-edn state-path)]
        (if-let [claim (:regulator/tick-claim observed)]
          {:ok true :status :draining :coordinator/id coordinator-id
           :durably-disabled? true :in-flight-tick claim
           :scheduler scheduler :watchdog watchdog-stopped}
          (tick-lock
           (fn []
             (let [state (or (read-edn state-path)
                             (regulator/initial-state coordinator-id))]
               (if-let [claim (:regulator/tick-claim state)]
                 {:ok true :status :draining :coordinator/id coordinator-id
                  :durably-disabled? true :in-flight-tick claim
                  :scheduler scheduler :watchdog watchdog-stopped}
                 (let [witness {:state/type :durable-quiescence-witness
                                :coordinator/id coordinator-id
                                :regulator/epoch (:regulator/epoch state)
                                :regulator/ticks (:regulator/ticks state)
                                :tick-claim nil
                                :witnessed-at (*quiescence-now-fn*)}
                       stopped (assoc state
                                      :regulator/status :stopped
                                      :regulator/reconciliation :quiescent
                                      :regulator/quiescence-witness witness
                                      :regulator/updated-at
                                      (:witnessed-at witness))
                       saved (persistence/atomic-persist! state-path stopped)]
                   (if (:ok saved)
                     {:ok true :status :stopped
                      :coordinator/id coordinator-id
                      :durably-disabled? true
                      :quiescence-witness witness
                      :state stopped :scheduler scheduler
                      :watchdog watchdog-stopped}
                     {:ok false
                      :error/code :durable-coordinator-quiescence-write-failed
                      :finding saved})))))))))))

(defn resume!
  "Enable and start a stopped coordinator, or explicitly repair a failed one."
  ([registry-path coordinator-id]
   (resume! registry-path coordinator-id nil))
  ([registry-path coordinator-id repair-reason]
   (let [entry (get-in (read-registry registry-path) [:entries coordinator-id])
         state (when entry (read-edn (:coordinator/state-path entry)))
         intent-recovery
         (when (:coordinator/pending-intent state)
           (supersede-expired-intent! registry-path coordinator-id))
         recovery-ok?
         (or (nil? intent-recovery)
             (:ok intent-recovery)
             (= :durable-coordinator-intent-not-expired
                (:error/code intent-recovery)))
         state (if (= :expired-intent-superseded (:status intent-recovery))
                 (:state intent-recovery)
                 state)
         persist-state! #(persistence/atomic-persist!
                          (Path/of (:coordinator/state-path entry)
                                   (make-array String 0)) %)
         repaired (if-not recovery-ok?
                    intent-recovery
                    (case (:regulator/status state)
                    :failed (regulator/repair-resume!
                             {:state state :reason repair-reason
                              :persist-fn persist-state!})
                    :complete (regulator/continue-complete!
                               {:state state :reason repair-reason
                                :persist-fn persist-state!})
                    :stopped (regulator/resume-stopped!
                              {:state state :persist-fn persist-state!
                               :now-fn *quiescence-now-fn*})
                    {:ok true}))]
     (if-not (:ok repaired)
       repaired
       (let [enabled (set-enabled! registry-path coordinator-id true
                                   :durable-coordinator/resume!
                                   (if repair-reason
                                     :repair-resume-requested
                                     :resume-requested))]
         (if (:ok enabled)
           (start-registered! registry-path coordinator-id)
           enabled))))))
