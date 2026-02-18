(ns futon3c.peripheral.registry
  "Peripheral registry and chain orchestration.

   Registry: maps peripheral-id to concrete PeripheralRunner instances via
   factory dispatch. Chain orchestration runs a sequence of peripherals with
   hop validation (via social/peripheral.clj) and context transfer between
   each transition.

   This is the wiring layer — it connects abstract peripheral implementations
   to the hop protocol that governs peripheral transitions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.peripheral.alfworld :as alfworld]
            [futon3c.peripheral.discipline :as discipline]
            [futon3c.peripheral.deploy :as deploy]
            [futon3c.peripheral.edit :as edit]
            [futon3c.peripheral.explore :as explore]
            [futon3c.peripheral.mission :as mission]
            [futon3c.peripheral.mission-control :as mission-control]
            [futon3c.peripheral.proof :as proof]
            [futon3c.peripheral.reflect :as reflect]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.test-runner :as test-runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.peripheral :as hop]
            [futon3c.social.shapes :as shapes]))

(def peripheral-ids
  "Set of all known peripheral IDs."
  #{:explore :edit :test :deploy :reflect :proof :discipline :mission :mission-control :alfworld})

(def ^:private factories
  "Maps peripheral-id to its factory function."
  {:explore    explore/make-explore
   :edit       edit/make-edit
   :test       test-runner/make-test-runner
   :deploy     deploy/make-deploy
   :reflect    reflect/make-reflect
   :proof      proof/make-proof
   :mission    mission/make-mission
   :discipline      discipline/make-discipline
   :mission-control mission-control/make-mission-control
   :alfworld        alfworld/make-alfworld})

(defn make-peripheral
  "Create a PeripheralRunner for the given peripheral-id.
   1-arity: mock backend, spec from peripherals.edn
   2-arity: custom backend, spec from peripherals.edn
   3-arity: custom spec and backend"
  ([peripheral-id]
   (make-peripheral peripheral-id (tools/make-mock-backend)))
  ([peripheral-id backend]
   (if-let [factory (get factories peripheral-id)]
     (factory backend)
     (throw (ex-info "Unknown peripheral" {:peripheral-id peripheral-id}))))
  ([peripheral-id spec backend]
   (if-let [factory (get factories peripheral-id)]
     (factory spec backend)
     (throw (ex-info "Unknown peripheral" {:peripheral-id peripheral-id})))))

(defn load-peripherals
  "Load peripheral specs from peripherals.edn on the classpath.
   Returns {:peripherals {id spec ...}} — the format expected by
   social/peripheral.clj's validate-hop and transfer-context."
  []
  (let [res (io/resource "peripherals.edn")]
    (when-not res (throw (ex-info "peripherals.edn not found on classpath" {})))
    (let [data (edn/read-string (slurp res))]
      {:peripherals (:peripherals data)})))

;; =============================================================================
;; Single-peripheral execution
;; =============================================================================

(defn- social-error? [x]
  (shapes/valid? shapes/SocialError x))

(defn- run-single
  "Run a single peripheral through its lifecycle: start → step* → stop.
   Returns {:ok true :fruit ... :exit-context ... :evidence [...]}
   or SocialError on any lifecycle failure."
  [peripheral context actions stop-reason]
  (let [start-result (runner/start peripheral context)]
    (if (social-error? start-result)
      start-result
      (loop [state (:state start-result)
             remaining actions
             evidence [(:evidence start-result)]]
        (if (empty? remaining)
          (let [stop-result (runner/stop peripheral state stop-reason)]
            (if (social-error? stop-result)
              stop-result
              {:ok true
               :fruit (:fruit stop-result)
               :exit-context (:context stop-result)
               :evidence (conj evidence (:evidence stop-result))}))
          (let [step-result (runner/step peripheral state (first remaining))]
            (if (social-error? step-result)
              step-result
              (recur (:state step-result)
                     (rest remaining)
                     (conj evidence (:evidence step-result))))))))))

;; =============================================================================
;; Chain orchestration — multi-peripheral with hop validation
;; =============================================================================

(defn- validate-transition
  "Validate a hop between peripherals and compute the next context.
   Returns {:ok true :context <next-start-context>} or SocialError."
  [peripherals from-pid exit-context exit-condition stop-reason to-pid session-id]
  (let [hop-request (cond-> {:hop/to to-pid
                              :hop/reason (or stop-reason "")
                              :hop/session-id session-id}
                      exit-condition (assoc :hop/exit-condition exit-condition))
        hop-result (hop/validate-hop peripherals from-pid hop-request)]
    (if (social-error? hop-result)
      hop-result
      (let [target-spec (hop/get-peripheral peripherals to-pid)]
        (if (social-error? target-spec)
          target-spec
          {:ok true
           :context (hop/transfer-context hop-result exit-context target-spec)})))))

(defn run-chain
  "Run a sequence of peripherals with hop validation and context transfer.

   config: {:backend     ToolBackend
            :peripherals loaded peripherals ({:peripherals {id spec ...}})
            :evidence-store atom (optional — peripherals append here)}
   context: initial context map (must include :session-id)
   steps: [{:peripheral-id :keyword
            :actions [{:tool :kw :args [...]} ...]
            :stop-reason string
            :exit-condition :keyword}
           ...]

   Between each step, validates the hop via social/peripheral.clj and
   transfers context. Evidence accumulates across all peripherals.

   Returns {:ok true :evidence [...] :fruits [...] :final-context {...}}
   or {:ok false :error SocialError :evidence [...] :fruits [...]}."
  [config context steps]
  (let [{:keys [backend peripherals evidence-store]} config
        session-id (:session-id context)]
    (loop [remaining steps
           prev nil
           start-ctx (cond-> context
                       evidence-store (assoc :evidence-store evidence-store))
           all-evidence []
           all-fruits []]
      (if (empty? remaining)
        {:ok true
         :evidence all-evidence
         :fruits all-fruits
         :final-context start-ctx}
        (let [{:keys [peripheral-id actions stop-reason exit-condition]} (first remaining)
              ;; Validate hop if transitioning from a previous peripheral
              transition (if prev
                           (validate-transition
                             peripherals
                             (:peripheral-id prev)
                             (:exit-context prev)
                             (:exit-condition prev)
                             (:stop-reason prev)
                             peripheral-id
                             session-id)
                           {:ok true :context start-ctx})]
          (if (social-error? transition)
            {:ok false
             :error transition
             :evidence all-evidence
             :fruits all-fruits}
            (let [next-ctx (cond-> (:context transition)
                             evidence-store (assoc :evidence-store evidence-store))
                  peripheral (make-peripheral peripheral-id backend)
                  result (run-single peripheral next-ctx actions stop-reason)]
              (if (social-error? result)
                {:ok false
                 :error result
                 :evidence all-evidence
                 :fruits all-fruits}
                (recur (rest remaining)
                       {:peripheral-id peripheral-id
                        :exit-context (:exit-context result)
                        :exit-condition exit-condition
                        :stop-reason stop-reason}
                       (:exit-context result)
                       (into all-evidence (:evidence result))
                       (conj all-fruits (:fruit result)))))))))))
