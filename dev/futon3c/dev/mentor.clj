(ns futon3c.dev.mentor
  "Mentor peripheral — claude-2 on #math.

   Extracted from futon3c.dev (Phase 2 of TN-dev-clj-decomposition).
   Manages mentor peripheral lifecycle: start, observe, evaluate,
   intervene, and stop."
  (:require [futon3c.peripheral.mentor :as mentor]
            [futon3c.peripheral.tools :as tools]
            [futon3c.peripheral.runner :as runner]
            [futon3c.agency.registry :as reg]
            [futon3c.dev.irc :as dev-irc])
  (:import [java.util UUID]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defonce !mentor (atom {}))

;; ---------------------------------------------------------------------------
;; IRC wiring for #math
;; ---------------------------------------------------------------------------

(defn make-math-irc-read-fn
  "Create an irc-read-fn that pulls #math messages from the IRC log ring buffer.
   Ensures the IRC connection is alive (auto-reconnects if needed).
   Returns a fn that returns all #math messages as [{:nick :text :at}]."
  []
  (fn []
    (dev-irc/ensure-irc-conn! "listener")
    (->> @dev-irc/!irc-log
         (filter #(= "#math" (:channel %)))
         (mapv #(select-keys % [:nick :text :at])))))

(defn make-math-irc-send-fn
  "Create an irc-send-fn that posts to #math via the bridge."
  []
  (let [bridge-send (dev-irc/make-bridge-irc-send-fn)]
    (fn [channel from-nick message]
      (bridge-send channel from-nick message))))

;; ---------------------------------------------------------------------------
;; Mentor lifecycle
;; ---------------------------------------------------------------------------

(defn start-mentor!
  "Start a mentor peripheral with a handle.
   The handle (e.g. 'mentor:FM-001') owns the map — any agent can inhabit it.

   Options:
     :handle     — mentor handle (default \"mentor:FM-001\")
     :problem-id — FM problem to track (default \"FM-001\")
     :channel    — IRC channel to observe (default \"#math\")
     :agent-id   — agent inhabiting this mentor (default \"claude-2\")
     :evidence-store — evidence store for context"
  [& {:keys [handle problem-id channel agent-id evidence-store]
      :or {handle "mentor:FM-001"
           problem-id "FM-001"
           channel "#math"
           agent-id "claude-2"}}]
  (let [backend (tools/make-mock-backend)
        irc-read-fn (make-math-irc-read-fn)
        irc-send-fn (make-math-irc-send-fn)
        peripheral (mentor/make-mentor backend
                     {:irc-read-fn irc-read-fn :irc-send-fn irc-send-fn}
                     handle)
        context {:session-id (str "mentor-" (UUID/randomUUID))
                 :agent-id agent-id
                 :problem-id problem-id
                 :channel channel
                 :evidence-store evidence-store}
        start-result (runner/start peripheral context)]
    (if (:ok start-result)
      (do
        (swap! !mentor assoc handle
               {:peripheral peripheral
                :state (atom (:state start-result))
                :handle handle})
        (reg/update-agent! agent-id
                           :agent/type :claude
                           :agent/capabilities [:mentor/observe :mentor/intervene])
        (println (str "[dev] Mentor started: " agent-id " as " handle
                      " on " channel " tracking " problem-id))
        :ok)
      (do
        (println (str "[dev] Mentor start failed: " start-result))
        start-result))))

;; ---------------------------------------------------------------------------
;; Mentor operations
;; ---------------------------------------------------------------------------

(defn- resolve-mentor
  "Look up a mentor by handle. Defaults to 'mentor:FM-001'."
  ([] (resolve-mentor "mentor:FM-001"))
  ([handle] (get @!mentor handle)))

(defn mentor-observe!
  "Run one observation cycle: pull new messages, enrich the map."
  ([] (mentor-observe! "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (runner/step
                    peripheral @state {:tool :mentor-observe :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-evaluate!
  "Evaluate triggers against the conversation map."
  ([] (mentor-evaluate! "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (runner/step
                    peripheral @state {:tool :mentor-evaluate :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-intervene!
  "Post an intervention when a trigger fires."
  ([trigger-id message] (mentor-intervene! "mentor:FM-001" trigger-id message))
  ([handle trigger-id message]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (runner/step
                    peripheral @state {:tool :mentor-intervene :args [trigger-id message]})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-status
  "Get current mentor map summary."
  ([] (mentor-status "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (runner/step
                    peripheral @state {:tool :mentor-status :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-map
  "Get full conversation map for a mentor handle."
  ([] (mentor-map "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (runner/step
                    peripheral @state {:tool :mentor-map :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-handles
  "List all active mentor handles."
  []
  (vec (keys @!mentor)))

(defn stop-mentor!
  "Stop a mentor peripheral by handle. Stops all if no handle given."
  ([] (doseq [h (mentor-handles)] (stop-mentor! h)))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (runner/stop peripheral @state "session ended")
     (swap! !mentor dissoc handle)
     (println (str "[dev] Mentor stopped: " handle)))))
