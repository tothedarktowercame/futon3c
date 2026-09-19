(ns futon3c.agency.invoke-controls
  "Shared registry of per-agent interrupt controls for local invoke lanes.

   The Claude/Codex CLI lanes historically kept this registry inside
   futon3c.dev (!invoke-controls). ZAI lanes run their agent loop in-JVM
   (futon3c.agents.zai-api) and cannot reach a dev namespace, so the registry
   lives here and both lanes can share it.

   A control is {:interrupt! (fn [] ...)} registered under an agent-id with a
   token; registration and deregistration are token-guarded so an older turn
   cannot deregister a newer turn's control.")

(defonce ^:private !controls (atom {}))

(defn register!
  "Register CONTROL for AGENT-ID under TOKEN. Idempotent per agent-id."
  [agent-id token control]
  (let [aid (str agent-id)]
    (swap! !controls assoc aid {:token (str token)
                                :agent-id aid
                                :registered-at (str (java.time.Instant/now))
                                :control control})
    true))

(defn deregister!
  "Remove the control for AGENT-ID if it still carries TOKEN."
  [agent-id token]
  (let [aid (str agent-id)]
    (swap! !controls
           (fn [m]
             (let [entry (get m aid)]
               (if (= (str token) (:token entry))
                 (dissoc m aid)
                 m)))))
  true)

(defn control-for
  "Return the {:token ... :control ...} entry for AGENT-ID, or nil."
  [agent-id]
  (get @!controls (str agent-id)))

(defn interrupt!
  "Best-effort interrupt for AGENT-ID's registered control.

   Returns the control's result map, or a {:ok false ...} explanation when no
   usable control is registered."
  [agent-id]
  (let [aid (str agent-id)
        entry (control-for aid)
        control (:control entry)]
    (cond
      (nil? entry)
      {:ok false :agent-id aid :action :no-active-control
       :message "no active local invoke control registered"}

      (not (map? control))
      {:ok false :agent-id aid :action :invalid-control
       :message "invalid invoke control entry"}

      :else
      (let [interrupt-fn (:interrupt! control)]
        (if (fn? interrupt-fn)
          (try
            (interrupt-fn)
            (catch Throwable t
              {:ok false :agent-id aid :action :interrupt-error
               :message (.getMessage t)}))
          {:ok false :agent-id aid :action :missing-interrupt
           :message "interrupt function missing"})))))

(defn snapshot
  "Read-only view: agent-id -> registration metadata (controls excluded)."
  []
  (into {}
        (map (fn [[aid entry]]
               [aid (dissoc entry :control)]))
        @!controls))
