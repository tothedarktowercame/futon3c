(ns futon3c.agents.tickle-queue
  "Bell-driven task queue for Tickle conductor.

   Two layers:
   1. Tasks — a pool of work items with priority and dependencies.
      Added via add-task!, dispatched to idle agents by pick-task.
   2. Bells — agents signal idle via enqueue!, with success/failure tracking.
      The conductor wires bells to task dispatch.

   The queue is the single source of truth for work dispatch:
     :tasks     — {task-id task-map} — the work pool
     :assigned  — {agent-id task-id} — who's working on what
     :pending   — bells waiting for dispatch
     :completed — bounded history of finished assignments
     :failures  — per-agent failure tracking"
  (:require [futon3c.agency.registry :as reg])
  (:import [java.time Instant]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defonce !task-queue
  (atom {:tasks     {}    ;; {task-id {:id str :label str :priority :normal :source str
                          ;;           :depends-on #{} :status :pending :assignee nil
                          ;;           :created-at Instant :completed-at nil}}
         :pending   []    ;; [{:agent-id str :bell-at Instant :nonce int :ok? bool :error str}]
         :assigned  {}    ;; {agent-id task-id}
         :completed []    ;; bounded ring buffer of completed assignments
         :failures  {}})) ;; {agent-id {:count int :last-error str :last-at Instant :consecutive int}}

(def ^:private max-completed 50)

(def ^:private priority-order {:high 0 :normal 1 :low 2})

;; ---------------------------------------------------------------------------
;; Task operations
;; ---------------------------------------------------------------------------

(defn add-task!
  "Add a task to the pool. Returns the task map.
   Required: :id, :label. Optional: :priority, :source, :depends-on."
  [{:keys [id label priority source depends-on] :as task}]
  {:pre [(string? id) (string? label)]}
  (let [task (merge {:priority   :normal
                     :source     "manual"
                     :depends-on #{}
                     :status     :pending
                     :assignee   nil
                     :created-at (Instant/now)
                     :completed-at nil}
                    task)]
    (swap! !task-queue assoc-in [:tasks id] task)
    task))

(defn remove-task!
  "Remove a task from the pool. No-op if not found."
  [task-id]
  (swap! !task-queue update :tasks dissoc task-id))

(defn task
  "Return a task by ID, or nil."
  [task-id]
  (get-in @!task-queue [:tasks task-id]))

(defn all-tasks
  "Return all tasks as a seq of maps."
  []
  (vals (:tasks @!task-queue)))

(defn- deps-met?
  "True if all dependencies of a task are :completed."
  [task tasks-map]
  (every? (fn [dep-id]
            (= :completed (:status (get tasks-map dep-id))))
          (:depends-on task)))

(defn available-tasks
  "Return tasks that are :pending and have all deps met, sorted by priority."
  []
  (let [tasks-map (:tasks @!task-queue)]
    (->> (vals tasks-map)
         (filter #(= :pending (:status %)))
         (filter #(deps-met? % tasks-map))
         (sort-by #(get priority-order (:priority %) 1))
         vec)))

(defn pick-task!
  "Assign the highest-priority available task to agent-id.
   Returns the task map, or nil if nothing available."
  [agent-id]
  (let [result (atom nil)]
    (swap! !task-queue
           (fn [q]
             (let [tasks-map (:tasks q)
                   candidates (->> (vals tasks-map)
                                   (filter #(= :pending (:status %)))
                                   (filter #(deps-met? % tasks-map))
                                   (sort-by #(get priority-order (:priority %) 1)))]
               (if-let [t (first candidates)]
                 (let [tid (:id t)]
                   (reset! result t)
                   (-> q
                       (assoc-in [:tasks tid :status] :assigned)
                       (assoc-in [:tasks tid :assignee] agent-id)
                       (assoc-in [:assigned agent-id] tid)))
                 q))))
    @result))

(defn complete-task!
  "Mark an agent's current task as :completed. Moves to completed history.
   Returns the completed task, or nil."
  [agent-id]
  (let [result (atom nil)]
    (swap! !task-queue
           (fn [q]
             (if-let [tid (get-in q [:assigned agent-id])]
               (let [t (get-in q [:tasks tid])
                     completed-task (assoc t :status :completed
                                             :completed-at (Instant/now))]
                 (reset! result completed-task)
                 (-> q
                     (assoc-in [:tasks tid] completed-task)
                     (update :assigned dissoc agent-id)
                     (update :completed
                             (fn [c]
                               (vec (take-last max-completed
                                               (conj c (assoc completed-task
                                                              :agent-id agent-id))))))))
               q)))
    @result))

(defn fail-task!
  "Mark an agent's current task as :pending again (re-queue on failure).
   Clears assignee so another agent can pick it up."
  [agent-id]
  (swap! !task-queue
         (fn [q]
           (if-let [tid (get-in q [:assigned agent-id])]
             (-> q
                 (assoc-in [:tasks tid :status] :pending)
                 (assoc-in [:tasks tid :assignee] nil)
                 (update :assigned dissoc agent-id))
             q))))

;; ---------------------------------------------------------------------------
;; Bell operations (unchanged from before)
;; ---------------------------------------------------------------------------

(defn enqueue!
  "Record that an agent went idle. Called from registry on-idle callback.
   outcome is {:ok bool :error str-or-nil :session-id str-or-nil} or nil."
  ([agent-id] (enqueue! agent-id nil))
  ([agent-id outcome]
   (let [ok? (if outcome (:ok outcome) true)
         bell {:agent-id agent-id
               :bell-at  (Instant/now)
               :nonce    (rand-int 1000000)
               :ok?      ok?
               :error    (when-not ok?
                           (let [e (:error outcome)]
                             (if (map? e)
                               (:social-error/message e)
                               (str e))))}]
     (swap! !task-queue
            (fn [q]
              (-> q
                  (update :pending conj bell)
                  ;; Track failure state per agent
                  (cond->
                    (not ok?)
                    (update-in [:failures agent-id]
                               (fn [f]
                                 {:count       (inc (or (:count f) 0))
                                  :consecutive (inc (or (:consecutive f) 0))
                                  :last-error  (:error bell)
                                  :last-at     (Instant/now)}))
                    ok?
                    (update-in [:failures agent-id]
                               (fn [f]
                                 (when f
                                   (assoc f :consecutive 0))))))))
     bell)))

(defn peek-pending
  "Return the next pending bell without removing it, or nil."
  []
  (first (:pending @!task-queue)))

(defn pop-pending!
  "Remove and return the next pending bell, or nil."
  []
  (let [result (atom nil)]
    (swap! !task-queue
           (fn [q]
             (if-let [bell (first (:pending q))]
               (do (reset! result bell)
                   (update q :pending (comp vec rest)))
               q)))
    @result))

(defn drain-pending!
  "Remove and return all pending bells."
  []
  (let [result (atom nil)]
    (swap! !task-queue
           (fn [q]
             (reset! result (:pending q))
             (assoc q :pending [])))
    @result))

(defn pending-count [] (count (:pending @!task-queue)))
(defn assigned-count [] (count (:assigned @!task-queue)))
(defn task-count [] (count (:tasks @!task-queue)))

(defn snapshot
  "Return the current queue state."
  []
  @!task-queue)

(defn clear!
  "Reset the queue. For testing/dev."
  []
  (reset! !task-queue {:tasks {} :pending [] :assigned {} :completed [] :failures {}}))

;; ---------------------------------------------------------------------------
;; Query
;; ---------------------------------------------------------------------------

(defn agent-assignment
  "Return the task-id the agent is currently assigned to, or nil."
  [agent-id]
  (get-in @!task-queue [:assigned agent-id]))

(defn agent-task
  "Return the full task map for the agent's current assignment, or nil."
  [agent-id]
  (when-let [tid (agent-assignment agent-id)]
    (task tid)))

(defn agent-failures
  "Return failure state for an agent, or nil if no failures recorded."
  [agent-id]
  (get-in @!task-queue [:failures agent-id]))

(defn agent-healthy?
  "True if agent has no consecutive failures (or has never failed)."
  [agent-id]
  (let [f (agent-failures agent-id)]
    (or (nil? f) (zero? (or (:consecutive f) 0)))))

(defn failing-agents
  "Return agent IDs with consecutive failures > 0."
  []
  (->> (:failures @!task-queue)
       (filter (fn [[_ f]] (and f (pos? (or (:consecutive f) 0)))))
       (mapv first)))

(defn idle-unassigned
  "Return agent IDs from the given set that are idle AND have no current assignment."
  [agent-ids]
  (let [assigned (:assigned @!task-queue)]
    (->> agent-ids
         (filter (fn [aid]
                   (and (not (contains? assigned aid))
                        (let [a (get @reg/!registry aid)]
                          (and (some? a)
                               (not= :invoking (:agent/status a)))))))
         vec)))

;; ---------------------------------------------------------------------------
;; Formatting (for blackboard projection)
;; ---------------------------------------------------------------------------

(defn format-queue
  "Format the task queue for display. Returns a string."
  []
  (let [{:keys [tasks pending assigned completed failures]} @!task-queue
        now (System/currentTimeMillis)
        fmt-ago (fn [^Instant inst]
                  (let [ago-s (quot (- now (.toEpochMilli inst)) 1000)]
                    (cond
                      (< ago-s 60) (str ago-s "s ago")
                      (< ago-s 3600) (str (quot ago-s 60) "m ago")
                      :else (str (quot ago-s 3600) "h ago"))))
        available (available-tasks)
        failing (->> failures
                     (filter (fn [[_ f]] (and f (pos? (or (:consecutive f) 0))))))]
    (str "── Task Queue ──\n"
         "TASKS: " (count tasks) " total, " (count available) " available\n"
         (when (seq available)
           (apply str
                  (map (fn [{:keys [id label priority]}]
                         (str "  " (when (= :high priority) "★ ") id ": " label "\n"))
                       (take 5 available))))
         "ASSIGNED:\n"
         (if (empty? assigned)
           "  (none)\n"
           (apply str
                  (map (fn [[aid tid]]
                         (let [t (get tasks tid)]
                           (str "  " aid " → " tid
                                (when t (str " (" (:label t) ")"))
                                "\n")))
                       assigned)))
         "COMPLETED (last 5):\n"
         (if (empty? completed)
           "  (none)\n"
           (apply str
                  (map (fn [{:keys [agent-id id completed-at]}]
                         (str "  " agent-id " → " (or id "?")
                              " ✓ " (when completed-at (fmt-ago completed-at)) "\n"))
                       (take-last 5 completed))))
         (when (seq pending)
           (str "BELLS: " (count pending) " pending\n"))
         (when (seq failing)
           (str "FAILING:\n"
                (apply str
                       (map (fn [[aid {:keys [consecutive last-error last-at]}]]
                              (str "  " aid " × " consecutive " consecutive"
                                   (when last-at (str " (last " (fmt-ago last-at) ")"))
                                   (when last-error
                                     (str " — " (subs last-error 0 (min 60 (count last-error)))))
                                   "\n"))
                            failing)))))))
