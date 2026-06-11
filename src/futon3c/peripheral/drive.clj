(ns futon3c.peripheral.drive
  "Drive harness — step a Cyder-registered :repl process through multiple
   cycles, visibly and boundedly.

   The proof-of-concept for E-drive-prior-art.md's convergent design: the
   arc-2 autorunner failed five ways (no-drive, slow metronome,
   session-private, killed-by-reset, state-blind); this harness answers
   each one IN the stack:
   - drive exists: a conductor loop that calls the target's :step-fn;
   - paced: configurable inter-step delay + max-steps bound;
   - inspectable: the conductor REGISTERS ITSELF with Cyder (a driver is a
     process too — I-6..I-9 apply to it), and keeps a step journal in its
     own :state-fn;
   - JVM-resident: lives where Cyder lives, dies only with the JVM;
   - state-aware: reads the target's :state-fn before every step and stops
     when the target reports :done? (or a guard declines the step) — it
     drives the work that remains, not a wall-clock rhythm.

   I-3 note: for agent-inhabited peripherals the :step-fn must DISPATCH an
   inhabited turn (e.g. enqueue via tickle-queue → idle-bell → registry
   invoke), never impersonate the agent. This harness doesn't care — it
   steps whatever honest :step-fn the process registered."
  (:require [futon3c.cyder :as cyder]))

(defonce ^{:doc "drive-id -> {:status :running|:stopped|:done|:halted
                              :steps [..] :target ..}"}
  !drives (atom {}))

(defn drive-status [drive-id] (get @!drives drive-id))

(defn stop-drive!
  "Visible stop control: the next step-check halts the drive."
  [drive-id]
  (swap! !drives assoc-in [drive-id :stop?] true)
  :stopping)

(defn- target-state [process-id]
  (when-let [p (get @cyder/!processes process-id)]
    (when-let [f (:process/state-fn p)]
      (try (f) (catch Throwable t {:state-error (.getMessage t)})))))

(defn- record-step! [drive-id entry]
  (swap! !drives update-in [drive-id :steps] (fnil conj []) entry))

(defn drive!
  "Drive PROCESS-ID (a Cyder :repl process with :step-fn + :state-fn)
   through up to MAX-STEPS cycles on a background thread.

   Stops when: the target's state reports :done? true; an optional GUARD-FN
   (state -> boolean) declines; stop-drive! was called; the target
   deregisters; or max-steps is reached. Every step is journaled in the
   drive's own state. Returns the drive-id immediately.

   Options: :max-steps (default 32) :step-delay-ms (default 0)
            :guard-fn (fn [state] -> proceed?) :drive-id (default derived)."
  [process-id & {:keys [max-steps step-delay-ms guard-fn drive-id]
                 :or {max-steps 32 step-delay-ms 0}}]
  (let [drive-id (or drive-id (str "drive/" process-id))
        _ (swap! !drives assoc drive-id
                 {:status :running :target process-id :stop? false :steps []})
        _ (cyder/register!
           {:id drive-id
            :type :daemon
            :layer :infra
            :stop-fn #(stop-drive! drive-id)
            :state-fn #(drive-status drive-id)
            :metadata {:doc (str "drive harness over " process-id)
                       :max-steps max-steps}})
        finish! (fn [status]
                  (swap! !drives assoc-in [drive-id :status] status)
                  (cyder/deregister! drive-id)
                  status)
        thread
        (Thread.
         (fn []
           (try
             (loop [n 0]
               (let [proc (get @cyder/!processes process-id)
                     state (target-state process-id)]
                 (cond
                   (get-in @!drives [drive-id :stop?]) (finish! :stopped)
                   (nil? proc) (finish! :halted)
                   (nil? (:process/step-fn proc)) (finish! :halted)
                   (:done? state) (finish! :done)
                   (and guard-fn (not (guard-fn state))) (finish! :halted)
                   (>= n max-steps) (finish! :max-steps)
                   :else
                   (let [result (try ((:process/step-fn proc))
                                     (catch Throwable t
                                       {:step-error (.getMessage t)}))]
                     (record-step! drive-id {:n (inc n)
                                             :pre-state state
                                             :result result})
                     (cyder/touch! process-id)
                     (when (:step-error result) (finish! :halted))
                     (when-not (:step-error result)
                       (when (pos? step-delay-ms) (Thread/sleep step-delay-ms))
                       (recur (inc n)))))))
             (catch Throwable t
               (swap! !drives update drive-id assoc
                      :status :halted :error (.getMessage t))
               (cyder/deregister! drive-id))))
         (str "drive-" process-id))]
    (.setDaemon thread true)
    (.start thread)
    drive-id))

(defn await-drive
  "Block (bounded) until DRIVE-ID leaves :running. For tests/demos."
  [drive-id timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [s (:status (drive-status drive-id))]
        (if (or (not= :running s) (> (System/currentTimeMillis) deadline))
          (drive-status drive-id)
          (do (Thread/sleep 25) (recur)))))))

;; ---------------------------------------------------------------------------
;; Toy target — the "simpler example": a five-cycle counter peripheral
;; ---------------------------------------------------------------------------

(defn register-toy-cycles!
  "Register a toy N-cycle process with Cyder: state {:cycle k :done? bool},
   step advances one cycle and journals a work item. Returns the process id."
  [process-id n-cycles]
  (let [!state (atom {:cycle 0 :done? false :work []})]
    (cyder/register!
     {:id process-id
      :type :state-machine
      :layer :repl
      :stop-fn (fn [] (swap! !state assoc :done? true))
      :state-fn (fn [] @!state)
      :step-fn (fn []
                 (let [{:keys [cycle done?]} @!state]
                   (if done?
                     {:noop true}
                     (let [k (inc cycle)]
                       (swap! !state
                              #(-> %
                                   (assoc :cycle k
                                          :done? (>= k n-cycles))
                                   (update :work conj (str "cycle-" k "-work"))))
                       {:cycled k}))))
      :metadata {:doc (str "toy " n-cycles "-cycle peripheral (drive demo)")}})
    process-id))
