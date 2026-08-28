(ns futon3c.apm.job-state
  "Canonical Agency job-state vocabulary consumed by APM pollers.")

(def active-states #{:queued :activating :running :overrun})
(def settling-states #{:delivering})
(def terminal-states #{:done :failed :error :cancelled :timeout})
(def known-states (into #{} (concat active-states settling-states terminal-states)))

(defn classify [state]
  (cond
    (contains? terminal-states state) :terminal
    (contains? settling-states state) :settling
    (contains? active-states state) :active
    :else :unknown))
