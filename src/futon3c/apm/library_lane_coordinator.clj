(ns futon3c.apm.library-lane-coordinator
  "Durable coordinator adapter for nonblocking library-lane steps."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.authority-port :as authority-port]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-effects :as effects]
            [futon3c.apm.library-lane-launch :as launch]
            [futon3c.apm.library-lane-runner :as runner]
            [futon3c.apm.live-preflight-runtime :as persistence])
  (:import [java.nio.file Path]))

(def adapter-key :apm/library-lane)
(def default-registry-path "data/apm-coordinators/registry.edn")
(def phase-rulings [:awaiting :phase-certified :partial-banked :closed])

(defn run-step!
  [{:keys [agency-base corpus-root frames-root state-root problem-id
           control-root trunk-branch keying-target contract-path phase
           strategy-required?]
    :as options}]
  (let [eff (effects/live-effects {:agency-base agency-base
                                   :corpus-root corpus-root
                                   :frames-root frames-root})
        observed (when-not (false? (:ok eff))
                   ((:observe-problem-fn eff)
                    {:corpus-root corpus-root :problem-id problem-id}))
        resumed (when (:ok observed)
                  (launch/resume-config
                   {:state-root state-root :problem-id problem-id
                    :revision (get-in observed [:problem :revision])
                    :outcome-fn (:outcome-fn eff)}))
        launched (cond
                   (false? (:ok eff)) eff
                   (not (:ok observed)) observed
                   resumed {:ok true :status :resumed :config resumed}
                   :else
                   (launch/launch!
                    (merge eff
                           {:corpus-root corpus-root :problem-id problem-id
                            :control-root control-root
                            :trunk-branch trunk-branch
                            :keying-target keying-target
                            :state-root state-root
                            :agency-base agency-base
                            :solver-assignment-id
                            (:solver-assignment-id options)})))]
    (if-not (:ok launched)
      launched
      (let [config (assoc (:config launched)
                          :control-root control-root
                          :agency-base agency-base
                          :solver-assignment-id
                          (or (:solver-assignment-id options)
                              (:solver-assignment-id (:config launched))))]
        (runner/step-one!
         {:corpus-root corpus-root :problem-id problem-id
          :contract (edn/read-string (slurp contract-path))
          :seat (:seats config)
          :phase-limit phase
          :strategy-required? strategy-required?
          :phase-inputs-fn (adapters/make-phase-inputs-fn config)
          :bank-request-fn (adapters/make-bank-request-fn config)})))))

(defn- next-intent [config state]
  (let [body {:coordinator/id (:coordinator-id config)
              :problem-id (:problem-id config)
              :phase (or (:library/phase state) :preflight)
              :strategy-required? (boolean (:library/strategy-required? state))
              :prior-intent/digest
              (get-in state [:coordinator/last-settled-intent :intent/digest])
              :regulator/ticks (:regulator/ticks state)}]
    {:job-id (str "library-step-" (machine/ledger-digest [body]))
     :dispatch/id (machine/ledger-digest
                   [(assoc body :dispatch/type :library-lane-step)])
     :dispatch/action :library-lane/step
     :dispatch/parameters
     {:phase (or (:library/phase state) :preflight)
      :strategy-required? (boolean (:library/strategy-required? state))}
     :expected/postcondition {:ruling/one-of phase-rulings}}))

(defn adapter-constructor [config]
  {:decide-fn
   (fn [state]
     {:ok true :coordinator/action :activate
      :coordinator/intent (next-intent config state)})
   :reconcile-fn
   (fn [intent state]
     (let [phase (get-in intent [:dispatch/parameters :phase])
           intent-strategy? (boolean (get-in intent [:dispatch/parameters
                                                     :strategy-required?]))
           current-phase (or (:library/phase state) :preflight)
           current-strategy? (boolean (:library/strategy-required? state))
           successor {:preflight :solve :solve :verify :verify :bank}]
       (if (or (not= phase current-phase)
               (not= intent-strategy? current-strategy?))
         {:ok false :error/code :library-lane-phase-intent-drift
          :finding {:intent-phase phase :state-phase current-phase
                    :intent-strategy-required? intent-strategy?
                    :state-strategy-required? current-strategy?}}
         (let [result (run-step! (assoc config :phase phase
                                        :strategy-required?
                                        intent-strategy?))]
           (case (:ruling result)
         :awaiting {:ok true :status :awaiting-job :lane/result result}
         :phase-certified
         {:ok true :status :library-phase-certified
          :coordinator/clear-intent? true :lane/result result
          :regulator/state-updates
          (cond-> {:library/phase (successor phase)}
            (= :solve phase) (assoc :library/strategy-required? false))}
         :partial-banked {:ok true :status :library-increment-banked
                          :coordinator/clear-intent? true :lane/result result
                          :regulator/state-updates
                          {:library/phase :preflight
                           :library/strategy-required? true}}
         :closed {:ok true :status :frame-complete
                  :coordinator/clear-intent? true :lane/result result}
         (if (:ok result)
           {:ok false :error/code :library-lane-ruling-invalid
            :finding result}
             result))))))})

(coordinator/register-adapter! adapter-key adapter-constructor)

(defn start!
  [{:keys [registry-path state-path coordinator-id problem-id period-ms
           retry-max]
    :as options
    :or {registry-path default-registry-path period-ms 500 retry-max 0}}]
  (let [checked (authority-port/require-dispatch-paths
                 {:control-root (:control-root options)}
                 [[:role-card (:path runner/library-card)]
                  [:role-card (:path runner/solver-restrategize-card)]])
        config (dissoc options :registry-path :state-path :period-ms
                       :retry-max)
        registered (coordinator/register!
                    {:registry-path registry-path :coordinator-id coordinator-id
                     :problem-id problem-id :retry-max retry-max
                     :adapter adapter-key :config config :state-path state-path
                     :period-ms period-ms})]
    (cond
      (not (:ok checked)) checked
      (:ok registered)
      (coordinator/start-registered! registry-path coordinator-id)
      :else registered)))

(defn status [registry-path coordinator-id]
  (coordinator/status registry-path coordinator-id))

(defn stop! [registry-path coordinator-id]
  (coordinator/stop! registry-path coordinator-id))

(defn hydrate-control-authority!
  "Migrate one legacy library registration to explicit control authority.

   The caller supplies the root; no directory inference is permitted. Both
   frozen role cards must exist before the content-addressed registry entry is
   replaced. Coordinator state and any pending intent are never rewritten."
  [registry-path coordinator-id control-root reason]
  (let [registry (coordinator/read-registry registry-path)
        entry (get-in registry [:entries coordinator-id])
        authority {:control-root control-root}
        cards [[:role-card (:path runner/library-card)]
               [:role-card (:path runner/solver-restrategize-card)]]
        checked (authority-port/require-dispatch-paths authority cards)]
    (cond
      (nil? entry)
      {:ok false :error/code :durable-coordinator-not-registered}
      (not= adapter-key (:coordinator/adapter entry))
      {:ok false :error/code :library-lane-registration-adapter-invalid}
      (not (and (string? reason) (not-empty reason)))
      {:ok false :error/code :library-lane-authority-migration-reason-required}
      (not (:ok checked)) checked
      (= control-root (get-in entry [:coordinator/config :control-root]))
      {:ok true :status :already-hydrated :entry entry}
      (some? (get-in entry [:coordinator/config :control-root]))
      {:ok false :error/code :library-lane-control-authority-conflict
       :finding {:registered (get-in entry [:coordinator/config :control-root])
                 :requested control-root}}
      :else
      (let [updated (-> entry
                        (assoc-in [:coordinator/config :control-root]
                                  control-root)
                        (update-in [:coordinator/config :authority/migrations]
                                   (fnil conj [])
                                   {:migration/type :control-root-hydration
                                    :reason reason})
                        (assoc :coordinator/entry-digest nil))
            updated (assoc updated :coordinator/entry-digest
                           (coordinator/entry-digest updated))
            saved (persistence/atomic-persist!
                   (Path/of registry-path (make-array String 0))
                   (assoc-in registry [:entries coordinator-id] updated))]
        (if (:ok saved)
          {:ok true :status :control-authority-hydrated :entry updated}
          saved)))))

(defn migrate-solver-assignment!
  "Bind a legacy library coordinator to its current solver identity.

   This changes future library-frame seat projection only. Durable phase state,
   pending intents, workspaces, and receipts are not rewritten."
  [registry-path coordinator-id solver-assignment-id reason]
  (let [registry (coordinator/read-registry registry-path)
        entry (get-in registry [:entries coordinator-id])
        registered (get-in entry [:coordinator/config :solver-assignment-id])]
    (cond
      (nil? entry)
      {:ok false :error/code :durable-coordinator-not-registered}
      (not= adapter-key (:coordinator/adapter entry))
      {:ok false :error/code :library-lane-registration-adapter-invalid}
      (not (and (string? solver-assignment-id)
                (re-matches #"(?:library-[A-Za-z0-9]+|f[0-9]+)-solver"
                            solver-assignment-id)))
      {:ok false :error/code :library-solver-assignment-invalid}
      (not (and (string? reason) (not-empty reason)))
      {:ok false :error/code :library-solver-assignment-reason-required}
      (= registered solver-assignment-id)
      {:ok true :status :already-migrated :entry entry}
      (some? registered)
      {:ok false :error/code :library-solver-assignment-conflict
       :finding {:registered registered :requested solver-assignment-id}}
      :else
      (let [updated (-> entry
                        (assoc-in [:coordinator/config :solver-assignment-id]
                                  solver-assignment-id)
                        (update-in [:coordinator/config :authority/migrations]
                                   (fnil conj [])
                                   {:migration/type :solver-assignment
                                    :solver-assignment-id solver-assignment-id
                                    :reason reason})
                        (assoc :coordinator/entry-digest nil))
            updated (assoc updated :coordinator/entry-digest
                           (coordinator/entry-digest updated))]
        (persistence/atomic-persist!
         (Path/of registry-path (make-array String 0))
         (assoc-in registry [:entries coordinator-id] updated))))))

(defn retire-superseded-preflight-intent!
  "Retire a failed preflight intent only after its certified revision is
   proven to be an ancestor of the coordinator's current trunk head.

   This is a durable reconciliation migration, not a receipt rewrite: the
   certified preflight remains intact and the next tick mints a fresh intent
   against the already-banked trunk while retaining the solver assignment."
  [{:keys [registry-path coordinator-id preflight-state-path reason run-fn]
    :or {run-fn shell/sh}}]
  (let [{:keys [registration durable-state]}
        (coordinator/status registry-path coordinator-id)
        intent (:coordinator/pending-intent durable-state)
        config (:coordinator/config registration)
        certified (when preflight-state-path
                    (edn/read-string (slurp preflight-state-path)))
        old-revision (get-in certified [:request :problem-revision])
        corpus-root (:corpus-root config)
        trunk-branch (:trunk-branch config)
        head-result (when (and corpus-root trunk-branch)
                      (run-fn "git" "-C" corpus-root "rev-parse" trunk-branch))
        current-revision (some-> (:out head-result) str/trim)
        ancestor-result (when (and old-revision current-revision corpus-root)
                          (run-fn "git" "-C" corpus-root "merge-base"
                                  "--is-ancestor" old-revision current-revision))]
    (cond
      (nil? registration) {:ok false :error/code :durable-coordinator-not-registered}
      (not= :failed (:regulator/status durable-state))
      {:ok false :error/code :library-superseded-intent-not-failed}
      (not= :preflight (:library/phase durable-state))
      {:ok false :error/code :library-superseded-intent-phase-invalid}
      (not= :preflight (get-in intent [:dispatch/parameters :phase]))
      {:ok false :error/code :library-superseded-intent-parameter-invalid}
      (not (true? (:library/strategy-required? durable-state)))
      {:ok false :error/code :library-superseded-intent-strategy-boundary-missing}
      (not= :live-job-certified (:state/type certified))
      {:ok false :error/code :library-superseded-preflight-not-certified}
      (not (and (string? reason) (not (str/blank? reason))))
      {:ok false :error/code :library-superseded-intent-reason-required}
      (not (zero? (:exit head-result)))
      {:ok false :error/code :library-trunk-head-unreadable}
      (= old-revision current-revision)
      {:ok false :error/code :library-preflight-not-superseded}
      (not (zero? (:exit ancestor-result)))
      {:ok false :error/code :library-preflight-not-ancestor
       :finding {:preflight/revision old-revision
                 :trunk/revision current-revision}}
      :else
      (let [updated (-> durable-state
                        (dissoc :coordinator/pending-intent
                                :coordinator/pending-pre-state-digest)
                        (update :coordinator/intent-migrations (fnil conj [])
                                {:migration/type :superseded-preflight-retirement
                                 :retired-intent/digest (:intent/digest intent)
                                 :preflight/revision old-revision
                                 :trunk/revision current-revision
                                 :solver-assignment-id
                                 (:solver-assignment-id config)
                                 :reason reason}))]
        (persistence/atomic-persist!
         (Path/of (:coordinator/state-path registration)
                  (make-array String 0))
         updated)))))

(defn migrate-pending-phase-intent!
  "Bind one legacy library intent to its already-persisted phase.

   This migration is deliberately narrow: it accepts only an otherwise valid
   library step intent with no dispatch parameters and preserves its job,
   dispatch, and pre-state identities."
  [registry-path coordinator-id]
  (let [{:keys [registration durable-state]}
        (coordinator/status registry-path coordinator-id)
        intent (:coordinator/pending-intent durable-state)
        phase (or (:library/phase durable-state) :preflight)]
    (cond
      (nil? registration)
      {:ok false :error/code :durable-coordinator-not-registered}
      (nil? intent)
      {:ok true :status :no-pending-intent}
      (not (coordinator/valid-intent? coordinator-id durable-state intent))
      {:ok false :error/code :durable-coordinator-intent-integrity-invalid
       :findings (coordinator/intent-findings coordinator-id durable-state intent)}
      (not= :library-lane/step (:dispatch/action intent))
      {:ok false :error/code :library-lane-intent-migration-action-invalid}
      (not (keyword? phase))
      {:ok false :error/code :library-lane-intent-migration-phase-invalid}
      (and (some? (:dispatch/parameters intent))
           (not= {:phase phase} (:dispatch/parameters intent)))
      {:ok false :error/code :library-lane-intent-migration-parameters-invalid}
      (and (= {:phase phase} (:dispatch/parameters intent))
           (= {:ruling/one-of phase-rulings}
              (:expected/postcondition intent)))
      {:ok true :status :already-migrated}
      :else
      (let [amended (assoc intent
                           :dispatch/parameters {:phase phase}
                           :expected/postcondition
                           {:ruling/one-of phase-rulings})
            amended (assoc amended :intent/digest
                           (coordinator/intent-digest amended))
            updated (-> durable-state
                        (assoc :coordinator/pending-intent amended)
                        (update :coordinator/intent-migrations (fnil conj [])
                                {:migration/type :library-phase-parameter
                                 :prior-intent/digest (:intent/digest intent)
                                 :amended-intent/digest (:intent/digest amended)
                                 :phase phase}))]
        (persistence/atomic-persist!
         (Path/of (:coordinator/state-path registration)
                  (make-array String 0))
         updated)))))

(defn resume! [registry-path coordinator-id]
  (coordinator/resume! registry-path coordinator-id))
