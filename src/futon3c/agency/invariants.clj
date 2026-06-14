(ns futon3c.agency.invariants
  "Agency invariant tripwires.

   Invariant A3: durable-queue hardening is active whenever Agency serves.
   Pattern: fail-loud boot integrity gate, mirroring futon1a I2."
  (:require [clojure.string :as str]
            [futon3c.agency.turn-queue :as turn-queue]))

(defn- falsey?
  [s]
  (#{"0" "false" "no" "off"} (str/lower-case (str/trim (str s)))))

(defn repl-through-queue-enabled?
  "True when operator/REPL turns are routed through the durable queue path."
  []
  (let [v (or (System/getProperty "FUTON3C_REPL_THROUGH_QUEUE")
              (System/getenv "FUTON3C_REPL_THROUGH_QUEUE"))]
    (if (some? v)
      (not (falsey? v))
      true)))

(defn queue-hardening-status
  "Return the A3 durable-queue hardening status.

   The hardening trio is:
   - FUTON3C_DURABLE_QUEUE
   - FUTON3C_DRAINER_V2
   - FUTON3C_REPL_THROUGH_QUEUE"
  []
  (let [gates {:durable-queue (boolean (turn-queue/enabled?))
               :drainer-v2 (boolean (turn-queue/drainer-v2-enabled?))
               :repl-through-queue (boolean (repl-through-queue-enabled?))}
        degraded (->> gates
                      (keep (fn [[k on?]]
                              (when-not on? k)))
                      vec)]
    {:ok? (empty? degraded)
     :gates gates
     :degraded degraded}))

(defn queue-hardening-warning
  "Build a glaring operator-facing warning for a degraded A3 status."
  [{:keys [degraded gates]}]
  (str "[agency-invariants] A3 BOOT INTEGRITY WARNING: durable-queue hardening is DEGRADED; "
       "off gates=" (pr-str (vec degraded))
       " gates=" (pr-str gates)
       ". Agency will continue serving, but operator turns may drop silently. "
       "Fix: unset stale false overrides or set FUTON3C_DURABLE_QUEUE=true, "
       "FUTON3C_DRAINER_V2=true, and FUTON3C_REPL_THROUGH_QUEUE=true "
       "(these should default true)."))

(defn warn-queue-hardening!
  "Log A3 state at boot. Does not throw; degraded Agency is loud but still serves."
  ([] (warn-queue-hardening! (queue-hardening-status)))
  ([status]
   (when-not (:ok? status)
     (println (queue-hardening-warning status))
     (flush))
   status))
