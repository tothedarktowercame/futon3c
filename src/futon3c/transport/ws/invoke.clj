(ns futon3c.transport.ws.invoke
  "Track active WS connections capable of handling registry invocations.
   Provides helpers for registry/invoke-agent! to fall back to WS when
   no local invoke-fn is available."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.util UUID]))

(def ^:const default-timeout-ms 600000)
(def ^:const timeout-sentinel ::timeout)

(defonce ^:private !agents
  ;; {agent-id {:send (fn [json]) :pending (atom {invoke-id promise})}}
  (atom {}))

(defn register!
  "Register a WS connection for AGENT-ID with SEND-FN (string -> nil)."
  [agent-id send-fn]
  (when (and (string? agent-id) (not (str/blank? agent-id)))
    (swap! !agents assoc agent-id {:send send-fn
                                   :pending (atom {})})))

(defn unregister!
  "Unregister AGENT-ID from the WS invoke registry."
  [agent-id]
  (swap! !agents dissoc agent-id))

(defn available?
  "True when AGENT-ID has an active WS bridge."
  [agent-id]
  (contains? @!agents agent-id))

(defn- deliver-timeout! [pending invoke-id]
  (when-let [p (get @pending invoke-id)]
    (swap! pending dissoc invoke-id)
    (deliver p timeout-sentinel)))

(defn invoke!
  "Send PROMPT to AGENT-ID over WS and block for a result map.
   Optional SESSION-ID is forwarded; override timeout via TIMEOUT-MS."
  [agent-id prompt session-id timeout-ms]
  (if-let [{:keys [send pending]} (get @!agents agent-id)]
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
        result))
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
