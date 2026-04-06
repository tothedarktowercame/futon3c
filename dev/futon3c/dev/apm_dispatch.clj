(ns futon3c.dev.apm-dispatch
  "Unified idle-event dispatcher for parallel APM conductors.

   The agency registry has a single global !on-idle callback. This module
   owns that callback and routes idle events to the correct conductor
   based on agent-id.

   Usage:
     (register-conductor! :apm-v2 \"claude-1\" (fn [agent-id outcome] ...))
     (register-conductor! :apm-v1 \"codex-1\" (fn [agent-id outcome] ...))
     ;; Both conductors now receive idle events for their respective agents.
     (deregister-conductor! :apm-v2)
     ;; Only :apm-v1 remains active."
  (:require [futon3c.agency.registry :as reg]))

;; {conductor-id -> {:agent-id str :callback (fn [agent-id outcome] ...)}}
(defonce !conductors (atom {}))

(defn- wire-idle-callback!
  "Set the global !on-idle callback to route events to all registered conductors."
  []
  (if (empty? @!conductors)
    (reg/set-on-idle! nil)
    (reg/set-on-idle!
      (fn [agent-id outcome]
        (doseq [[_cid {:keys [callback] :as entry}] @!conductors]
          (when (= agent-id (:agent-id entry))
            (callback agent-id outcome)))))))

(defn register-conductor!
  "Register a conductor to receive idle events for a specific agent.
   Throws if another conductor already owns the same agent-id."
  [conductor-id agent-id callback]
  (let [existing @!conductors
        conflict (some (fn [[cid entry]]
                         (when (and (= agent-id (:agent-id entry))
                                    (not= cid conductor-id))
                           cid))
                       existing)]
    (when conflict
      (throw (ex-info (str "Agent " agent-id " already owned by conductor " conflict)
                      {:agent-id agent-id
                       :existing-conductor conflict
                       :new-conductor conductor-id})))
    (swap! !conductors assoc conductor-id {:agent-id agent-id :callback callback})
    (wire-idle-callback!)
    conductor-id))

(defn deregister-conductor!
  "Remove a conductor's registration. Clears the global callback if no conductors remain."
  [conductor-id]
  (swap! !conductors dissoc conductor-id)
  (wire-idle-callback!)
  conductor-id)

(defn active-conductors
  "Return a map of active conductor registrations (for diagnostics)."
  []
  (into {}
    (map (fn [[cid {:keys [agent-id]}]]
           [cid {:agent-id agent-id}])
         @!conductors)))
