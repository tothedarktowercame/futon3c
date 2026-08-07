(ns futon3c.transport.ws.invoke
  "Track active WS connections capable of handling registry invocations.
   Provides helpers for registry/invoke-agent! to fall back to WS when
   no local invoke-fn is available."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.util UUID]))

(def ^{:const true
       :deprecated "Retained for callers that still reference it. This ns no
  longer imposes a default deadline: invoke! with a nil timeout waits
  indefinitely and lets the job supervisor own lifecycle."}
  default-timeout-ms 3600000)
(def ^:const timeout-sentinel ::timeout)

(defonce ^:private !agents
  ;; {agent-id {:send (fn [json])
  ;;            :pending (atom {invoke-id {:promise p :started-at ms
  ;;                                       :timed-out-at ms-or-absent}})
  ;;            :connection <opaque ws channel>}}
  (atom {}))

(defn- warn!
  [& parts]
  (binding [*out* *err*]
    (println (apply str parts))))

(def ^:const disconnected-result
  "Delivered to callers still waiting when their agent's socket goes away."
  {:error :ws-disconnected})

(defn- fail-pending!
  "Deliver disconnected-result to every promise still waiting on ENTRY.

   Load-bearing since invoke! became unbounded: with no deadline, a caller
   blocked on a socket that dies would otherwise wait forever and leave its
   job stuck in 'overrun' with no clock to rescue it. Confirmed transport
   death is one of the few things that legitimately ends a turn
   (README-agency-cap.md), so it has to actually fire."
  [entry reason]
  (when-let [pending (:pending entry)]
    (let [waiting (first (reset-vals! pending {}))]
      (doseq [[invoke-id {:keys [promise timed-out-at]}] waiting]
        (when (and promise (not timed-out-at))
          (try
            (deliver promise disconnected-result)
            (catch Throwable t
              (warn! "[ws-invoke] failed to release pending invoke-id=" invoke-id
                     " (" reason "): " (.getMessage t))))))
      (count waiting))))

(defn- remove-agent!
  "Atomically drop AGENT-ID and return the entry that was removed, if any."
  [agent-id]
  (let [removed (atom nil)]
    (swap! !agents
           (fn [agents]
             (if-let [entry (get agents agent-id)]
               (do (reset! removed entry)
                   (dissoc agents agent-id))
               (do (reset! removed nil)
                   agents))))
    @removed))

(defn- evict!
  [agent-id reason]
  (let [released (fail-pending! (remove-agent! agent-id) reason)]
    (warn! "[ws-invoke] evicting stale sender agent-id=" agent-id
           " reason=" reason
           (when (and released (pos? released))
             (str " released-pending=" released)))))

(defn- accepted-send?
  [agent-id send json-str]
  (try
    (let [result (send json-str)]
      (if (false? result)
        (do
          (evict! agent-id "send-returned-false")
          false)
        true))
    (catch Throwable t
      (evict! agent-id (str "send-threw:" (.getClass t)))
      false)))

(defn register!
  "Register a WS connection for AGENT-ID with SEND-FN (string -> nil).

   OPTS may set {:observer? true} for broadcast-only participants (e.g. the
   emacs-hud connector): they receive broadcast-frame! but are never invoke
   targets (see invoke!/available?/connected-agent-ids)."
  ([agent-id send-fn] (register! agent-id send-fn nil))
  ([agent-id send-fn opts]
   (when (and (string? agent-id) (not (str/blank? agent-id)))
     (swap! !agents assoc agent-id (cond-> {:send send-fn
                                            :pending (atom {})}
                                     (contains? opts :connection)
                                     (assoc :connection (:connection opts))
                                     (:observer? opts) (assoc :observer? true))))))

(defn unregister!
  "Unregister AGENT-ID from the WS invoke registry, releasing any caller still
   waiting on it with disconnected-result."
  [agent-id]
  (fail-pending! (remove-agent! agent-id) "unregistered"))

(defn unregister-current!
  "Unregister AGENT-ID only when CONNECTION is still the current WS entry.
   Late close events from replaced sockets must not evict newer registrations.
   Callers still waiting on the removed entry are released."
  [agent-id connection]
  (let [removed (atom nil)]
    (swap! !agents
           (fn [agents]
             (let [entry (get agents agent-id)]
               (if (identical? (:connection entry) connection)
                 (do
                   (reset! removed entry)
                   (dissoc agents agent-id))
                 (do (reset! removed nil)
                     agents)))))
    (if-let [entry @removed]
      (do (fail-pending! entry "connection-closed") true)
      false)))

(defn available?
  "True when AGENT-ID has an active, INVOCABLE WS bridge.
   Observers (broadcast-only) are not invocable, so return false for them."
  [agent-id]
  (let [{:keys [observer?] :as entry} (get @!agents agent-id)]
    (and (some? entry) (not observer?))))

(defn connected-agent-ids
  "Return a sorted vector of agent-ids with active, INVOCABLE WS bridges.
   Observers are excluded (see connected-observer-ids)."
  []
  (->> @!agents
       (remove (fn [[_ entry]] (:observer? entry)))
       (map key)
       (filter string?)
       sort
       vec))

(defn connected-observer-ids
  "Return a sorted vector of broadcast-only observer ids (e.g. emacs-hud)."
  []
  (->> @!agents
       (filter (fn [[_ entry]] (:observer? entry)))
       (map key)
       (filter string?)
       sort
       vec))

(defn send-frame!
  "Send a best-effort JSON frame to AGENT-ID over its WS bridge.
   Returns true when the frame was accepted by the WS sender."
  [agent-id frame]
  (if-let [{:keys [send]} (get @!agents agent-id)]
    (accepted-send? agent-id send (json/generate-string frame))
    false))

(defn broadcast-frame!
  "Send FRAME to all connected WS agents. Best-effort, fire-and-forget."
  [frame]
  (let [json-str (json/generate-string frame)]
    (doseq [[aid {:keys [send]}] @!agents]
      (accepted-send? aid send json-str))))

(defonce ^:private !late-result-handler (atom nil))

(defn set-late-result-handler!
  "Register (fn [{:agent-id :invoke-id :result :waited-ms}]) to receive results
   that arrive after their caller stopped waiting. Pass nil to clear."
  [f]
  (reset! !late-result-handler f))

(defn- mark-timed-out!
  "Record that the caller of INVOKE-ID stopped waiting.

   The pending entry is deliberately KEPT. Deleting it made a late `resolve!`
   return false and drop the agent's reply on the floor — a real result,
   already computed, discarded because a clock ran out (README-agency-cap.md).
   Keeping it lets resolve! recognise the invocation and route the payload to
   the late-result handler."
  [pending invoke-id]
  (swap! pending update invoke-id
         (fn [entry]
           (when entry
             (assoc entry :timed-out-at (System/currentTimeMillis)))))
  (when-let [p (get-in @pending [invoke-id :promise])]
    (deliver p timeout-sentinel)))

(defn invoke!
  "Send PROMPT to AGENT-ID over WS and block for a result map.

   Optional SESSION-ID is forwarded. TIMEOUT-MS bounds how long THIS CALL
   waits; nil or non-positive waits indefinitely, which is what an async bell
   wants — the durable job supervisor owns turn lifecycle, and this layer must
   not impose a competing deadline (README-agency-cap.md). A timeout here ends
   the wait only: the agent keeps working and a late reply is still harvested
   via the late-result handler."
  [agent-id prompt session-id timeout-ms]
  (if-let [{:keys [send pending observer?]} (get @!agents agent-id)]
    (if observer?
      ;; Observers are broadcast-only, never invoke targets (I-1).
      {:error :ws-observer-not-invocable}
      (let [invoke-id (str "invoke-" (UUID/randomUUID))
            p (promise)
            timeout (when (and timeout-ms (pos? (long timeout-ms)))
                      (long timeout-ms))
            payload (cond-> {"type" "invoke"
                             "invoke_id" invoke-id
                             "prompt" prompt}
                      session-id (assoc "session_id" session-id))]
        (swap! pending assoc invoke-id {:promise p
                                        :started-at (System/currentTimeMillis)})
        (try
          (send (json/generate-string payload))
          (catch Exception e
            (swap! pending dissoc invoke-id)
            (throw e)))
        ;; Close the registration race: if the agent was evicted between the
        ;; lookup above and the swap! (the send itself can evict), fail-pending!
        ;; has already run on the old entry and would never see this one. With
        ;; no timeout that would block the caller forever.
        (when-not (contains? @!agents agent-id)
          (deliver p disconnected-result))
        (if-not timeout
          @p
          (let [result (deref p timeout timeout-sentinel)]
            (when (= result timeout-sentinel)
              (mark-timed-out! pending invoke-id))
            result))))
    {:error :ws-not-connected}))

(defn resolve!
  "Resolve a pending WS invocation for AGENT-ID and INVOKE-ID.

   Returns true when the invocation was known — including when its caller had
   already timed out, in which case the result is handed to the late-result
   handler rather than discarded."
  [agent-id invoke-id result]
  (if-let [{:keys [pending]} (get @!agents agent-id)]
    (if-let [entry (get @pending invoke-id)]
      (let [{:keys [promise timed-out-at started-at]} entry]
        (swap! pending dissoc invoke-id)
        (if timed-out-at
          (when-let [handler @!late-result-handler]
            (try
              (handler {:agent-id agent-id
                        :invoke-id invoke-id
                        :result result
                        :waited-ms (when started-at
                                     (- (System/currentTimeMillis) started-at))})
              (catch Throwable t
                (warn! "[ws-invoke] late-result handler threw for invoke-id="
                       invoke-id " " (.getMessage t)))))
          (deliver promise result))
        true)
      false)
    false))
