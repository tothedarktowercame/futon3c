(ns futon3c.arxana.service
  "Increment-3 service spine: capabilities handshake and envelope dispatch
  plumbing. No D2 resolution, D4 correspondence, clients, or deployment — this
  is the reversible seam those increments build against."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.resolve.promotion :as promotion]
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

(defn register-strategy!
  "Register an executable strategy value in the graph store under its
  content-addressed revision ID (D2 grammar). Returns the revision ID. The
  value is stored verbatim; the revision ID covers finder vector order."
  [graph-store strategy]
  (let [srid (evaluator/strategy-revision-id strategy)]
    (store/put-entity! graph-store
                       {:entity/id srid
                        :type :arxana/strategy-revision
                        :external-id srid
                        :props {:strategy strategy}})
    srid))

(defn- strategy-source
  "Resolve a strategy revision ID to its executable value via the store."
  [graph-store]
  (fn [srid]
    (when-let [e (:ok (store/get-entity graph-store srid))]
      (get-in e [:props :strategy]))))

(defn dispatch
  "Validate an inbound envelope and its body against the schema registry.
  Returns a D3 response envelope. :scope/resolve and :assertion/promote are
  implemented (increment 4); other registered operations whose handlers are
  not yet implemented answer :unsupported rather than pretending success."
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

          (= op :scope/resolve)
          (let [res (evaluator/evaluate
                     {:artifact-provider artifact-provider
                      :strategy-source (strategy-source graph-store)}
                     (assoc body :request/id (:message/id ok)))]
            (env/response ok res))

          (= op :assertion/promote)
          (let [{:keys [ok error]}
                (promotion/promote
                 {:graph-store graph-store
                  :artifact-provider artifact-provider
                  :strategy-source (strategy-source graph-store)}
                 (assoc body :principal/id (:principal/id ok)))]
            (env/response ok (or ok error)))

          :else
          (env/response
           ok
           (env/error :unsupported :arxana.service/operation-not-implemented
                      {:operation op
                       :increment 4
                       :note "resolve and promotion implemented; this handler lands in a later increment"}
                      nil)))))))
