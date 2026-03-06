(ns futon3c.cyder
  "CYDER — Cybernetic Development Environment that Rocks.

   Process registry for everything running in futon3c. Agents have their
   own registry (agency.registry); CYDER tracks the operational layer:
   servers, bridges, daemons, peripherals, state machines.

   Every long-running process registers itself (I-6), is stoppable (I-7),
   inspectable (I-8), and visible on a single surface (I-9). REPL-like
   processes are additionally jackable (I-10).

   Two layers:
     :repl  — steppable loops (peripherals, conductors, state machines)
     :infra — non-steppable infrastructure (servers, bridges, listeners)

   See: holes/missions/M-cyder.md"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.io File]))

;; =============================================================================
;; Process Registry
;; =============================================================================

(defonce ^{:doc "Registry of running processes.

   Structure: {process-id-string -> process-record}

   Process record:
   {:process/id        string
    :process/type      :server | :bridge | :daemon | :peripheral | :state-machine
    :process/layer     :repl | :infra
    :process/started-at Instant
    :process/last-active Instant
    :process/stop-fn   (fn [] ...) — stops the process
    :process/state-fn  (fn [] ...) — returns current state as data
    :process/step-fn   (fn [] ...) — single-step (layer :repl only)
    :process/metadata  map}"}
  !processes
  (atom {}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register!
  "Register a running process.

   Required keys:
     :id       - String identifier (unique)
     :type     - :server, :bridge, :daemon, :peripheral, :state-machine
     :stop-fn  - Zero-arg function that stops the process

   Optional keys:
     :layer    - :repl or :infra (default :infra)
     :state-fn - Zero-arg function returning current state as data
     :step-fn  - Zero-arg function for single-stepping (implies :layer :repl)
     :metadata - Arbitrary map

   Returns process record on success.
   Returns {:ok false :error string} if already registered."
  [{:keys [id type stop-fn layer state-fn step-fn metadata]}]
  {:pre [(string? id) (some? type) (fn? stop-fn)]}
  (let [now (Instant/now)
        layer (or layer (if step-fn :repl :infra))
        record (cond-> {:process/id         id
                        :process/type       type
                        :process/layer      layer
                        :process/started-at now
                        :process/last-active now
                        :process/stop-fn    stop-fn
                        :process/metadata   (or metadata {})}
                 state-fn (assoc :process/state-fn state-fn)
                 step-fn  (assoc :process/step-fn step-fn))
        result (atom nil)]
    (swap! !processes
           (fn [m]
             (if (contains? m id)
               (do (reset! result {:ok false
                                   :error (str "Process already registered: " id)})
                   m)
               (do (reset! result record)
                   (assoc m id record)))))
    @result))

(defn deregister!
  "Remove a process from the registry. Does NOT call its stop-fn.

   Returns {:ok true :id string} or {:ok false :error string}."
  [id]
  (let [result (atom nil)]
    (swap! !processes
           (fn [m]
             (if (contains? m id)
               (do (reset! result {:ok true :id id})
                   (dissoc m id))
               (do (reset! result {:ok false
                                   :error (str "Process not registered: " id)})
                   m))))
    @result))

;; =============================================================================
;; Inspection
;; =============================================================================

(defn list-processes
  "List all registered processes. Returns a vector of summary maps
   (no functions — safe for JSON serialization)."
  []
  (->> (vals @!processes)
       (mapv (fn [p]
               (-> (select-keys p [:process/id :process/type :process/layer
                                   :process/started-at :process/last-active
                                   :process/metadata])
                   (assoc :process/steppable? (boolean (:process/step-fn p))
                          :process/inspectable? (boolean (:process/state-fn p))))))
       (sort-by :process/id)))

(defn inspect
  "Inspect a process. Returns summary + live state (if state-fn provided).

   Returns {:ok true :process {...}} or {:ok false :error string}."
  [id]
  (if-let [p (get @!processes id)]
    {:ok true
     :process (cond-> (-> (select-keys p [:process/id :process/type :process/layer
                                          :process/started-at :process/last-active
                                          :process/metadata])
                          (assoc :process/steppable? (boolean (:process/step-fn p))
                                 :process/inspectable? (boolean (:process/state-fn p))))
                (:process/state-fn p)
                (assoc :process/state
                       (try ((:process/state-fn p))
                            (catch Exception e
                              {:error (.getMessage e)}))))}
    {:ok false :error (str "Process not registered: " id)}))

(defn touch!
  "Update :process/last-active to now."
  [id]
  (swap! !processes
         (fn [m]
           (if (contains? m id)
             (update m id assoc :process/last-active (Instant/now))
             m))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn stop!
  "Stop a process: call its stop-fn, then deregister it.

   Returns {:ok true :id string} or {:ok false :error string}."
  [id]
  (if-let [p (get @!processes id)]
    (do (try ((:process/stop-fn p))
             (catch Exception e
               (println "[cyder] Error stopping" id ":" (.getMessage e))))
        (deregister! id))
    {:ok false :error (str "Process not registered: " id)}))

(defn stop-all!
  "Stop all registered processes. Returns count stopped."
  []
  (let [ids (keys @!processes)
        n (count ids)]
    (doseq [id ids]
      (stop! id))
    n))

;; =============================================================================
;; Jack-in (Layer :repl only)
;; =============================================================================

(defn step!
  "Single-step a REPL-like process.

   Returns {:ok true :result <step-result>} or {:ok false :error string}."
  [id]
  (if-let [p (get @!processes id)]
    (if-let [step-fn (:process/step-fn p)]
      (let [result (step-fn)]
        (touch! id)
        {:ok true :result result})
      {:ok false :error (str "Process not steppable: " id)})
    {:ok false :error (str "Process not registered: " id)}))

;; =============================================================================
;; Status (aggregate view, analogous to agency/registry-status)
;; =============================================================================

(defn registry-status
  "Returns aggregate status for the process surface.

   Shape: {:processes {id -> summary} :count n}"
  []
  (let [procs (list-processes)]
    {:processes (into {} (map (juxt :process/id identity)) procs)
     :count (count procs)}))

;; =============================================================================
;; Mission Scanner (Phase 0 — mission dashboard)
;; =============================================================================

(def ^:private derivation-phases
  "Ordered phases of the futonic derivation xenotype."
  ["IDENTIFY" "MAP" "DERIVE" "ARGUE" "VERIFY" "INSTANTIATE"])

(defn- parse-mission-doc
  "Parse a mission markdown file for dashboard-relevant data.
   Returns {:name :phase :file :last-modified} or nil if unreadable."
  [^File f]
  (try
    (let [content (slurp f)
          name (-> (.getName f)
                   (str/replace #"\.md$" ""))
          status-line (->> (str/split-lines content)
                           (some #(when (str/starts-with? % "## Status:")
                                    (str/trim (subs % (count "## Status:"))))))
          phase (or status-line "UNKNOWN")
          ;; Extract open questions (## Open Questions section)
          open-qs (->> (str/split-lines content)
                       (drop-while #(not (str/starts-with? % "## Open Questions")))
                       rest
                       (take-while #(not (str/starts-with? % "## ")))
                       (filter #(re-matches #"\d+\..*" (str/trim %)))
                       (mapv str/trim))
          last-mod (Instant/ofEpochMilli (.lastModified f))]
      {:name name
       :phase phase
       :file (.getPath f)
       :last-modified last-mod
       :open-questions open-qs})
    (catch Exception _ nil)))

(defn scan-missions
  "Scan holes/missions/M-*.md for active missions.
   Returns a vector of mission summaries."
  ([] (scan-missions "holes/missions"))
  ([dir]
   (let [d (io/file dir)]
     (when (.isDirectory d)
       (->> (.listFiles d)
            (filter #(and (.isFile ^File %)
                          (str/starts-with? (.getName ^File %) "M-")
                          (str/ends-with? (.getName ^File %) ".md")))
            (keep parse-mission-doc)
            (sort-by :name)
            vec)))))

(defn register-missions!
  "Scan mission docs and register each active mission as a CYDER process.
   Active = any phase that isn't DONE/CLOSED/empty.
   Returns count of missions registered."
  ([] (register-missions! "holes/missions"))
  ([dir]
   (let [missions (scan-missions dir)
         active (filter #(not (contains? #{"DONE" "CLOSED" "" "UNKNOWN"}
                                         (-> (:phase %) str/trim
                                             (str/replace #"\s.*" "")
                                             str/upper-case)))
                        missions)]
     (doseq [{:keys [name phase file last-modified open-questions]} active]
       (register!
        {:id       name
         :type     :state-machine
         :layer    :repl
         :stop-fn  (fn []) ; missions aren't stoppable processes
         :state-fn (fn []
                     ;; Re-read the doc live each time state is inspected
                     (if-let [m (parse-mission-doc (io/file file))]
                       (select-keys m [:phase :last-modified :open-questions])
                       {:error "Could not read mission doc"}))
         :metadata {:file file
                    :phase phase}}))
     (count active))))
