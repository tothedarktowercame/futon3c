(ns futon3c.apm.durable-coordinator
  "Typed, restartable coordinator registration over `live-regulator`.

   Adapters decide pure next actions. Activation is always a later tick: the
   deterministic intent is first stored in regulator state, then handed to the
   adapter's idempotent reconcile function. Registry entries are typed and
   content-addressed; startup never infers coordinators from directories."
  (:require [clojure.edn :as edn]
            [futon3c.apm.live-preflight-runtime :as persistence]
            [futon3c.apm.live-regulator :as regulator])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path]
           [java.security MessageDigest]))

(def registry-type :durable-coordinator-registry)
(def registry-version 1)
(def entry-type :durable-coordinator-registration)
(def intent-type :durable-coordinator-intent)
(defonce ^:private adapters (atom {}))

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
    (not (nat-int? (:pre-state/version intent))) (conj :pre-state-version)
    (and (nat-int? (:pre-state/version intent))
         (not= (:regulator/ticks state) (inc (:pre-state/version intent))))
    (conj :pre-state-version-relationship)
    (not (string? (:pre-state/digest intent))) (conj :pre-state-digest)
    (not= (:pre-state/digest intent)
          (:coordinator/pending-pre-state-digest state))
    (conj :pre-state-binding)
    (not (map? (:expected/postcondition intent))) (conj :expected-postcondition)
    (not= (:intent/digest intent) (intent-digest intent)) (conj :intent-digest)))

(defn valid-intent? [coordinator-id state intent]
  (empty? (intent-findings coordinator-id state intent)))

(defn valid-entry? [entry]
  (and (= entry-type (:state/type entry))
       (string? (:coordinator/id entry))
       (not-empty (:coordinator/id entry))
       (keyword? (:coordinator/adapter entry))
       (map? (:coordinator/config entry))
       (string? (:coordinator/state-path entry))
       (pos-int? (:coordinator/period-ms entry))
       (boolean? (:coordinator/enabled? entry))
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
  [{:keys [registry-path coordinator-id adapter config state-path period-ms]
    :or {period-ms regulator/default-period-ms}}]
  (let [registry (read-registry registry-path)
        entry (cond-> {:state/type entry-type
                       :coordinator/id coordinator-id
                       :coordinator/adapter adapter
                       :coordinator/config (or config {})
                       :coordinator/state-path (str state-path)
                       :coordinator/period-ms period-ms
                       :coordinator/enabled? true}
                true (assoc :coordinator/entry-digest nil))
        entry (assoc entry :coordinator/entry-digest (entry-digest entry))]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (not (valid-entry? entry))
      {:ok false :error/code :durable-coordinator-registration-invalid}
      :else
      (let [existing (get-in registry [:entries coordinator-id])]
        (if (and existing (not= existing entry))
          {:ok false :error/code :durable-coordinator-registration-conflict
           :finding {:coordinator/id coordinator-id}}
          (let [saved (persistence/atomic-persist!
                       (Path/of (str registry-path) (make-array String 0))
                       (assoc-in registry [:entries coordinator-id] entry))]
            (if (:ok saved)
              {:ok true :status (if existing :already-registered :registered)
               :entry entry}
              saved)))))))

(defn- coordinator-tick [coordinator-id adapter state]
  (if-let [intent (:coordinator/pending-intent state)]
    (if-not (valid-intent? coordinator-id state intent)
      {:ok false :error/code :durable-coordinator-intent-integrity-invalid
       :findings (intent-findings coordinator-id state intent)}
      (let [result ((:reconcile-fn adapter) intent state)]
        (if (:ok result)
          (cond-> (dissoc result :coordinator/clear-intent?)
            (:coordinator/clear-intent? result)
            (assoc :regulator/state-updates
                   {:coordinator/pending-intent nil
                    :coordinator/pending-pre-state-digest nil
                    :coordinator/last-settled-intent intent}))
          result)))
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
             {:coordinator/pending-intent intent
              :coordinator/pending-pre-state-digest
              (:pre-state/digest intent)}}
            {:ok false :error/code :durable-coordinator-intent-invalid}))
        :else decision))))

(defn start-entry! [entry]
  (cond
    (not (valid-entry? entry))
    {:ok false :error/code :durable-coordinator-registration-invalid}
    (not (:coordinator/enabled? entry))
    {:ok true :status :disabled :coordinator/id (:coordinator/id entry)}
    (nil? (get @adapters (:coordinator/adapter entry)))
    {:ok false :error/code :durable-coordinator-adapter-unavailable
     :finding {:adapter (:coordinator/adapter entry)}}
    :else
    (let [adapter ((get @adapters (:coordinator/adapter entry))
                   (:coordinator/config entry))]
      (if-not (and (map? adapter) (fn? (:decide-fn adapter))
                   (fn? (:reconcile-fn adapter)))
        {:ok false :error/code :durable-coordinator-adapter-provider-invalid}
        (let [state-path (Path/of (:coordinator/state-path entry)
                                  (make-array String 0))]
          (regulator/start!
           {:regulator-id (:coordinator/id entry)
            :period-ms (:coordinator/period-ms entry)
            :read-fn #(persistence/read-state state-path)
            :persist-fn #(persistence/atomic-persist! state-path %)
            :tick-state-fn #(coordinator-tick (:coordinator/id entry)
                                               adapter %)}))))))

(defn start-registered!
  "Start one coordinator solely from its typed registry entry."
  [registry-path coordinator-id]
  (let [registry (read-registry registry-path)]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (nil? (get-in registry [:entries coordinator-id]))
      {:ok false :error/code :durable-coordinator-not-registered}
      :else (start-entry! (get-in registry [:entries coordinator-id])))))

(defn recover-all!
  "Start every typed registry entry. No filesystem discovery is performed."
  [registry-path]
  (let [registry (read-registry registry-path)]
    (if-not (valid-registry? registry)
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (let [results (into (sorted-map)
                          (map (fn [[id entry]] [id (start-entry! entry)]))
                          (:entries registry))]
        {:ok (every? :ok (vals results)) :results results}))))

(defn status
  ([coordinator-id] (regulator/status coordinator-id))
  ([registry-path coordinator-id]
   (let [entry (get-in (read-registry registry-path) [:entries coordinator-id])]
     (when entry
       {:registration entry
        :runtime (regulator/status coordinator-id)
        :durable-state (read-edn (:coordinator/state-path entry))}))))

(defn- set-enabled! [registry-path coordinator-id enabled?]
  (let [registry (read-registry registry-path)
        entry (get-in registry [:entries coordinator-id])]
    (cond
      (not (valid-registry? registry))
      {:ok false :error/code :durable-coordinator-registry-invalid}
      (nil? entry)
      {:ok false :error/code :durable-coordinator-not-registered}
      :else
      (let [updated (assoc entry :coordinator/enabled? enabled?)
            updated (assoc updated :coordinator/entry-digest
                           (entry-digest updated))]
        (persistence/atomic-persist!
         (Path/of (str registry-path) (make-array String 0))
         (assoc-in registry [:entries coordinator-id] updated))))))

(defn stop!
  ([coordinator-id] (regulator/stop! coordinator-id))
  ([registry-path coordinator-id]
   (let [disabled (set-enabled! registry-path coordinator-id false)]
     (if (:ok disabled)
       (assoc (regulator/stop! coordinator-id) :durably-disabled? true)
       disabled))))

(defn resume!
  "Enable and start a stopped coordinator, or explicitly repair a failed one."
  ([registry-path coordinator-id]
   (resume! registry-path coordinator-id nil))
  ([registry-path coordinator-id repair-reason]
   (let [entry (get-in (read-registry registry-path) [:entries coordinator-id])
         state (when entry (read-edn (:coordinator/state-path entry)))
         repaired (if (= :failed (:regulator/status state))
                    (regulator/repair-resume!
                     {:state state :reason repair-reason
                      :persist-fn #(persistence/atomic-persist!
                                    (Path/of (:coordinator/state-path entry)
                                             (make-array String 0)) %)})
                    {:ok true})]
     (if-not (:ok repaired)
       repaired
       (let [enabled (set-enabled! registry-path coordinator-id true)]
         (if (:ok enabled)
           (start-registered! registry-path coordinator-id)
           enabled))))))
