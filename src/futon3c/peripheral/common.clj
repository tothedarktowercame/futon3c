(ns futon3c.peripheral.common
  "Shared helpers for concrete peripheral implementations."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.runner :as runner]
            [futon3c.social.shapes :as shapes]))

(defonce ^:private specs
  (delay
    (let [res (io/resource "peripherals.edn")]
      (when-not res
        (throw (ex-info "Missing peripherals.edn resource" {})))
      (-> res slurp edn/read-string :peripherals))))

(defn load-spec
  "Load a single peripheral spec from resources/peripherals.edn."
  [peripheral-id]
  (if-let [spec (get @specs peripheral-id)]
    spec
    (throw (ex-info "Peripheral spec not found" {:peripheral-id peripheral-id}))))

(defn social-error?
  [x]
  (shapes/valid? shapes/SocialError x))

(defn resolve-author
  "Choose a stable author string from context."
  [context]
  (let [agent-id (:agent-id context)]
    (cond
      (string? agent-id) agent-id
      (map? agent-id) (or (:id/value agent-id) "unknown-agent")
      (string? (:author context)) (:author context)
      :else "unknown-agent")))

(defn validate-action
  "Validate action shape before dispatch."
  [peripheral-id action]
  (cond
    (not (map? action))
    (runner/runner-error peripheral-id :invalid-action
                         "Action must be a map"
                         :action action)

    (not (keyword? (:tool action)))
    (runner/runner-error peripheral-id :invalid-action
                         "Action must include a keyword :tool"
                         :action action)

    (and (contains? action :args) (not (sequential? (:args action))))
    (runner/runner-error peripheral-id :invalid-action
                         "Action :args must be sequential"
                         :action action)

    :else nil))

(defn normalize-action
  [action]
  {:tool (:tool action)
   :args (vec (or (:args action) []))})

(defn maybe-append-evidence!
  "Append evidence if state contains an atom under :evidence-store.
   Returns nil on success/no-op, or SocialError on append failure."
  [state evidence-entry]
  (let [evidence-store (:evidence-store state)]
    (when (instance? clojure.lang.IAtom evidence-store)
      (let [result (store/append* evidence-store evidence-entry)]
        (when (social-error? result)
          result)))))
