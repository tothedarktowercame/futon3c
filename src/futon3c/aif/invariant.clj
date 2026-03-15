(ns futon3c.aif.invariant
  "Self-enforcing invariant: every registered peripheral has an AIF head.

   This namespace provides the enumeration check for completion criterion C9
   of M-aif-head. It queries the peripheral registry and verifies that each
   peripheral has a registered AifHead binding.

   Callable by any AIF head's check-law method to verify the invariant
   across the stack.

   Design decision: V-3 in M-aif-head VERIFY phase."
  (:require [futon3c.peripheral.registry :as registry]))

;; =============================================================================
;; AIF head binding registry
;; =============================================================================

(defonce ^:private !aif-head-bindings
  "Atom mapping peripheral-id → AifHead implementation.
   Populated by register-aif-head! when peripherals are initialized."
  (atom {}))

(defn register-aif-head!
  "Register an AifHead implementation for a peripheral.
   Called during peripheral initialization."
  [peripheral-id aif-head]
  (swap! !aif-head-bindings assoc peripheral-id aif-head)
  aif-head)

(defn unregister-aif-head!
  "Remove an AifHead binding. Used in tests."
  [peripheral-id]
  (swap! !aif-head-bindings dissoc peripheral-id))

(defn get-aif-head
  "Look up the AifHead for a peripheral. Returns nil if not registered."
  [peripheral-id]
  (get @!aif-head-bindings peripheral-id))

;; =============================================================================
;; Invariant check
;; =============================================================================

(defn check-aif-head-coverage
  "Enumerate all registered peripherals and check each has an AifHead binding.

   Returns:
     {:ok true}
   or:
     {:ok false
      :missing [peripheral-ids...]
      :coverage {:total n :bound n :missing n}}"
  []
  (let [all-peripherals registry/peripheral-ids
        bound (set (keys @!aif-head-bindings))
        missing (vec (sort (remove bound all-peripherals)))
        total (count all-peripherals)
        n-bound (count (filter bound all-peripherals))]
    (if (empty? missing)
      {:ok true
       :coverage {:total total :bound n-bound :missing 0}}
      {:ok false
       :missing missing
       :coverage {:total total :bound n-bound :missing (count missing)}})))

(defn check-aif-head-law
  "Structural law check: is the 'every peripheral has an AIF head' invariant
   satisfied?

   Returns the same shape as AifHead.check-law for composability:
     {:ok true}
   or:
     {:ok false
      :error {:code :structural-law-violation
              :law-family :aif-head-coverage
              :law-id :every-peripheral-has-aif-head
              :message string
              :context {:missing [...] :coverage {...}}}}"
  []
  (let [result (check-aif-head-coverage)]
    (if (:ok result)
      {:ok true}
      {:ok false
       :error {:code :structural-law-violation
               :law-family :aif-head-coverage
               :law-id :every-peripheral-has-aif-head
               :message (str "Peripherals without AIF head: "
                             (pr-str (:missing result)))
               :context result}})))
