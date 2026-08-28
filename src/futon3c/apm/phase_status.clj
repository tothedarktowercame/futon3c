(ns futon3c.apm.phase-status
  "Canonical vocabulary for successful results returned by APM phase drivers.

  Classification is intentionally closed. Adding a producer status requires a
  class here before the live supervisor can observe it as valid.")

(def status-classes
  {:awaiting-terminal :waiting-terminal
   :awaiting-substrate :waiting-substrate
   :transport-retry-scheduled :waiting-transport-retry
   :terminal-collected :terminal-evidence-collected
   :certified :certified})

(def known-statuses (set (keys status-classes)))

(defn classify [status]
  (get status-classes status :unknown))

