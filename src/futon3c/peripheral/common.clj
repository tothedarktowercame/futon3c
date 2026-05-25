(ns futon3c.peripheral.common
  "Shared helpers for concrete peripheral implementations."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.peripheral.runner :as runner]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]))

(defn- load-specs-from-resource []
  (let [res (io/resource "peripherals.edn")]
    (when-not res
      (throw (ex-info "Missing peripherals.edn resource" {})))
    (-> res slurp edn/read-string :peripherals)))

(defonce ^:private specs (atom nil))

(defn refresh-specs!
  "Re-read peripherals.edn into the specs cache. Use after editing the file
   to pick up new peripheral registrations without a JVM restart."
  []
  (reset! specs (load-specs-from-resource)))

;; Lazy initialisation — only fires when the atom is still empty (so a
;; `load-file` of this ns is idempotent; an explicit refresh-specs! call is
;; the way to bust the cache after editing peripherals.edn).
(when (nil? @specs)
  (refresh-specs!))

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

   Routes through `futon3c.evidence.boundary/append!` so the
   I-single-boundary and I-evidence-per-turn invariants bind here too.
   Preserves the historical return-shape contract: nil on success or
   no-op, SocialError-shaped map on failure (so existing callers that
   destructure the result do not need to change).

   The boundary's structured violation receipt — when present — is
   embedded in the SocialError's :error/context under
   `:invariant/violation`, so callers that want the richer detail can
   access it without losing back-compat."
  [state evidence-entry]
  (let [evidence-store (:evidence-store state)]
    (when (or (instance? clojure.lang.IAtom evidence-store)
              (satisfies? backend/EvidenceBackend evidence-store))
      (let [result (boundary/append! evidence-store evidence-entry)]
        (when-not (:ok result)
          (cond-> {:error/component :E-store
                   :error/code (or (:error/code result) :append-failed)
                   :error/message (or (:error/message result) "append failed")
                   :error/at (str (Instant/now))}
            (:invariant/violation result)
            (assoc :error/context {:invariant/violation (:invariant/violation result)})))))))
