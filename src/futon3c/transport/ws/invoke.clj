(ns futon3c.transport.ws.invoke
  "Track active WS connections capable of handling registry invocations.
   Provides helpers for registry/invoke-agent! to fall back to WS when
   no local invoke-fn is available."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.util UUID]))

(def ^:const default-timeout-ms 1800000)
(def ^:const timeout-sentinel ::timeout)

(defonce ^:private !agents
  ;; {agent-id {:send (fn [json]) :pending (atom {invoke-id promise})}}
  (atom {}))

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
                                     (:observer? opts) (assoc :observer? true))))))

(defn unregister!
  "Unregister AGENT-ID from the WS invoke registry."
  [agent-id]
  (swap! !agents dissoc agent-id))

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
    (try
      (send (json/generate-string frame))
      true
      (catch Throwable _
        false))
    false))

(defn broadcast-frame!
  "Send FRAME to all connected WS agents. Best-effort, fire-and-forget."
  [frame]
  (let [json-str (json/generate-string frame)]
    (doseq [[_aid {:keys [send]}] @!agents]
      (try
        (send json-str)
        (catch Throwable _ nil)))))

(defn- deliver-timeout! [pending invoke-id]
  (when-let [p (get @pending invoke-id)]
    (swap! pending dissoc invoke-id)
    (deliver p timeout-sentinel)))

(defn invoke!
  "Send PROMPT to AGENT-ID over WS and block for a result map.
   Optional SESSION-ID is forwarded; override timeout via TIMEOUT-MS."
  [agent-id prompt session-id timeout-ms]
  (if-let [{:keys [send pending observer?]} (get @!agents agent-id)]
    (if observer?
      ;; Observers are broadcast-only, never invoke targets (I-1).
      {:error :ws-observer-not-invocable}
      (let [invoke-id (str "invoke-" (UUID/randomUUID))
            promise (promise)
            timeout (long (max 1 (or timeout-ms default-timeout-ms)))
            payload (cond-> {"type" "invoke"
                             "invoke_id" invoke-id
                             "prompt" prompt}
                      session-id (assoc "session_id" session-id))]
        (swap! pending assoc invoke-id promise)
        (try
          (send (json/generate-string payload))
          (catch Exception e
            (swap! pending dissoc invoke-id)
            (throw e)))
        (let [result (deref promise timeout timeout-sentinel)]
          (when (= result timeout-sentinel)
            (deliver-timeout! pending invoke-id))
          result)))
    {:error :ws-not-connected}))

(defn resolve!
  "Resolve a pending WS invocation for AGENT-ID and INVOKE-ID."
  [agent-id invoke-id result]
  (if-let [{:keys [pending]} (get @!agents agent-id)]
    (if-let [p (get @pending invoke-id)]
      (do
        (swap! pending dissoc invoke-id)
        (deliver p result)
        true)
      false)
    false))
