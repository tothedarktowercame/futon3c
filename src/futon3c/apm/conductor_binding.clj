(ns futon3c.apm.conductor-binding
  "Server-owned bindings between an Agency session and one live APM conductor."
  (:require [clojure.string :as str]))

(defonce ^:private !bindings (atom {}))

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn handle-version [handle]
  (or (some (fn [entry]
              (when (= :problem-save (:tool entry))
                (get-in entry [:result :version])))
            (reverse (:log handle)))
      (count (filter #(= :problem-save (:tool %)) (:log handle)))))

(defn binding-key [agent-id session-id]
  [agent-id session-id])

(defn install!
  "Install HANDLE as the sole authority for AGENT-ID/SESSION-ID.
   An already-live binding is never replaced implicitly."
  [agent-id session-id handle]
  (let [key (binding-key agent-id session-id)
        cycle-id (:cycle-id handle)]
    (cond
      (not (nonblank? agent-id))
      {:ok false :error/code :conductor-agent-required}

      (not (nonblank? session-id))
      {:ok false :error/code :conductor-session-required}

      (not (nonblank? cycle-id))
      {:ok false :error/code :conductor-cycle-required}

      (nil? (get-in handle [:state :current-phase]))
      {:ok false :error/code :conductor-cycle-completed}

      :else
      (locking !bindings
        (if-let [existing (get @!bindings key)]
          {:ok false :error/code :conductor-binding-exists
           :cycle-id (:cycle-id existing) :version (:version existing)}
          (let [binding {:agent-id agent-id
                         :session-id session-id
                         :cycle-id cycle-id
                         :version (handle-version handle)
                         :handle (atom handle)
                         :receipts (atom {})
                         :lock (Object.)}]
            (swap! !bindings assoc key binding)
            {:ok true :binding binding}))))))

(defn lookup [agent-id session-id]
  (get @!bindings (binding-key agent-id session-id)))

(defn status
  "Read-only status. Absence is an ordinary observable state, not an error."
  [agent-id session-id]
  (if-let [binding (lookup agent-id session-id)]
    {:ok true :bound? true
     :agent-id agent-id :session-id session-id
     :problem-id (get-in @(:handle binding) [:config :problem-id])
     :cycle-id (:cycle-id binding) :version (:version binding)
     :phase (get-in @(:handle binding) [:state :current-phase])}
    {:ok true :bound? false :agent-id agent-id :session-id session-id}))

(defn check-continuation
  "Validate a parked continuation without changing or dispatching its handle."
  [agent-id session-id cycle-id version]
  (if-let [binding (lookup agent-id session-id)]
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking (:lock binding)
      (cond
        (not (identical? binding (lookup agent-id session-id)))
        {:ok false :error/code :conductor-session-unbound}

        (not= cycle-id (:cycle-id binding))
        {:ok false :error/code :conductor-cycle-stale
         :expected (:cycle-id binding) :received cycle-id}

        (not= version (:version binding))
        {:ok false :error/code :conductor-version-stale
         :expected (:version binding) :received version}

        :else
        {:ok true :cycle-id cycle-id :version version
         :phase (get-in @(:handle binding) [:state :current-phase])}))
    {:ok false :error/code :conductor-session-unbound}))

(defn- binding-for-cycle [cycle-id]
  (some (fn [[key binding]]
          (when (= cycle-id (:cycle-id binding)) [key binding]))
        @!bindings))

(defn- transfer-source!
  [source-key source target-key agent-id session-id cycle-id version loader]
  (cond
    (not= version (:version source))
    {:ok false :error/code :conductor-version-stale
     :expected (:version source) :received version}

    :else
    (let [resumed (loader @(:handle source) cycle-id version)]
      (if (false? (:ok resumed))
        {:ok false :error/code :conductor-takeover-load-refused
         :error (:error resumed)}
        (let [next-version (handle-version resumed)
              transferred (assoc source
                                 :agent-id agent-id
                                 :session-id session-id
                                 :version next-version)]
          (reset! (:handle source) resumed)
          (swap! !bindings #(-> %
                                (dissoc source-key)
                                (assoc target-key transferred)))
          {:ok true :cycle-id cycle-id :version next-version
           :phase (get-in resumed [:state :current-phase])})))))

(defn takeover!
  "Transfer a named, saved cycle to a new Agency session.

   LOADER rebuilds the runtime from the old authoritative handle and must load
   exactly CYCLE-ID/VERSION. The registry lock makes the compare, load, and key
   transfer one install-once operation."
  [agent-id session-id cycle-id version loader]
  (let [target-key (binding-key agent-id session-id)]
    (locking !bindings
      (cond
        (get @!bindings target-key)
        {:ok false :error/code :conductor-binding-exists}

        :else
        (if-let [[source-key source] (binding-for-cycle cycle-id)]
          #_{:clj-kondo/ignore [:locking-suspicious-lock]}
          (locking (:lock source)
            (if (identical? source (get @!bindings source-key))
              (transfer-source! source-key source target-key agent-id session-id
                                cycle-id version loader)
              {:ok false :error/code :conductor-session-unbound}))
          {:ok false :error/code :conductor-cycle-unbound
           :cycle-id cycle-id})))))

(defn execute!
  "Atomically execute one typed action against the authoritative live handle.
   EXECUTOR receives [handle operation args] and returns the next handle."
  ([agent-id session-id action executor]
   (execute! agent-id session-id action executor nil))
  ([agent-id session-id
    {:keys [action-id cycle-id version operation args] :as action}
    executor refusal-recorder]
   (if-let [binding (lookup agent-id session-id)]
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking (:lock binding)
      (cond
        (not (identical? binding (lookup agent-id session-id)))
        {:ok false :error/code :conductor-session-unbound
         :agent-id agent-id :session-id session-id}

        (not (nonblank? action-id))
        {:ok false :error/code :conductor-action-id-required}

        (contains? @(:receipts binding) action-id)
        {:ok false :error/code :conductor-action-duplicate
         :action-id action-id}

        (not= cycle-id (:cycle-id binding))
        {:ok false :error/code :conductor-cycle-stale
         :expected (:cycle-id binding) :received cycle-id}

        (not= version (:version binding))
        {:ok false :error/code :conductor-version-stale
         :expected (:version binding) :received version}

        :else
        (let [current @(:handle binding)
              next-handle (executor current operation (vec (or args [])))]
          (if (false? (:ok next-handle))
            (let [recorded (when refusal-recorder
                             (refusal-recorder current action next-handle))]
              (if (and recorded (not (false? (:ok recorded))))
                (let [next-version (handle-version recorded)
                      receipt {:action-id action-id :operation operation
                               :cycle-id (:cycle-id binding)
                               :version next-version :ok false}]
                  (reset! (:handle binding) recorded)
                  (reset! (:receipts binding)
                          (assoc @(:receipts binding) action-id receipt))
                  (swap! !bindings update (binding-key agent-id session-id)
                         assoc :version next-version)
                  {:ok false
                   :error/code (or (get-in next-handle [:error :error/code])
                                   :conductor-action-refused)
                   :error (:error next-handle)
                   :cycle-id (:cycle-id binding)
                   :version next-version})
                {:ok false
                 :error/code (or (get-in next-handle [:error :error/code])
                                 :conductor-action-refused)
                 :error (:error next-handle)
                 :cycle-id (:cycle-id binding)
                 :version (:version binding)}))
            (let [next-version (handle-version next-handle)
                  receipt {:action-id action-id :operation operation
                           :cycle-id (:cycle-id binding)
                           :version next-version}
                  completed? (nil? (get-in next-handle [:state :current-phase]))]
              (reset! (:handle binding) next-handle)
              (reset! (:receipts binding)
                      (assoc @(:receipts binding) action-id receipt))
              (swap! !bindings update (binding-key agent-id session-id)
                     assoc :version next-version)
              (when completed?
                (swap! !bindings dissoc (binding-key agent-id session-id)))
              {:ok true :receipt receipt :completed? completed?
               :phase (get-in next-handle [:state :current-phase])})))))
     {:ok false :error/code :conductor-session-unbound
      :agent-id agent-id :session-id session-id})))

(defn abandon!
  "Governed removal: the caller must name the current cycle and version."
  [agent-id session-id cycle-id version]
  (if-let [binding (lookup agent-id session-id)]
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking (:lock binding)
      (cond
        (not (identical? binding (lookup agent-id session-id)))
        {:ok false :error/code :conductor-session-unbound}

        (and (= cycle-id (:cycle-id binding)) (= version (:version binding)))
        (do (swap! !bindings dissoc (binding-key agent-id session-id))
            {:ok true :abandoned? true})

        :else
        {:ok false :error/code :conductor-abandonment-stale
         :expected {:cycle-id (:cycle-id binding) :version (:version binding)}}))
    {:ok false :error/code :conductor-session-unbound}))

(defn reset-bindings! []
  (reset! !bindings {})
  true)
