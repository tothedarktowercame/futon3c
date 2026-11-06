(ns futon3c.arxana.service
  "Increment-3 service spine: capabilities handshake and envelope dispatch
  plumbing. No D2 resolution, D4 correspondence, clients, or deployment — this
  is the reversible seam those increments build against."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.schema :as schema]
            [futon3c.arxana.store :as store]))

(defn handshake
  "Capabilities handshake (D1 compat rule 7: mandatory). Reports the store's
  truth table verbatim — evidence, not aspiration — plus registered operation
  schema versions and projection versions."
  [graph-store]
  {:protocol/version env/protocol-version
   :store/capabilities (store/capabilities graph-store)
   :operations (schema/registered-versions)
   :projections {:semantic/scope-resolve 1
                 :error 1}
   :consistency-mode :revision-precondition
   :subscription false})

(defn dispatch
  "Validate an inbound envelope and its body against the schema registry.
  Returns a D3 response envelope. Operations whose spine handlers are not yet
  implemented answer :unsupported rather than pretending success; increment 4+
  replaces those branches with real handlers."
  [graph-store artifact-provider msg]
  (let [{:keys [ok error]} (env/validate-envelope msg)]
    (if error
      {:protocol/version env/protocol-version
       :message/id (:message/id msg)
       :operation (:operation msg)
       :error error}
      (let [op (:operation ok)
            {body :ok berr :error} (schema/validate-body op (:body ok))]
        (cond
          berr (env/response ok berr)

          (= op :capabilities/handshake)
          (env/response ok (handshake graph-store))

          :else
          (env/response
           ok
           (env/error :unsupported :arxana.service/operation-not-implemented
                      {:operation op
                       :increment 3
                       :note "spine only; handler lands in a later increment"}
                      nil)))))))
