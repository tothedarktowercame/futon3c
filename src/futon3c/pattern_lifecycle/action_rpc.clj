(ns futon3c.pattern-lifecycle.action-rpc
  "Automatic PSR→action→PUR boundary with one stamped identity context.

  This is not the manual !psr/!pur command path. Callers provide an action;
  this boundary persists selection before invoking it and persists outcome
  after it returns or throws."
  (:require [clojure.string :as str]
            [futon3c.evidence.boundary :as evidence])
  (:import [java.util UUID]))

(defn- require-text! [field value]
  (when (or (not (string? value)) (str/blank? value))
    (throw (ex-info "pattern action identity is incomplete"
                    {:failure-kind :pattern-action-identity-incomplete
                     :field field})))
  value)

(defn- append-required! [store entry stage]
  (let [receipt (evidence/append! store entry)]
    (when-not (:ok receipt)
      (throw (ex-info "pattern action evidence was not persisted"
                      {:failure-kind :pattern-action-evidence-not-persisted
                       :stage stage
                       :receipt receipt})))
    receipt))

(defn execute!
  "Run ACTION inside an automatic pattern lifecycle.

  Required opts: :evidence-store, :pattern-id, :agent-id, :session-id,
  :task-id, :rationale, and zero-argument :action. The returned value includes
  the action result and both verified persistence receipts. If ACTION throws,
  a failure PUR is persisted with the same pattern/agent/session and the
  exception is rethrown with :pattern-action/pur-receipt attached."
  [{:keys [evidence-store pattern-id agent-id session-id task-id rationale action]}]
  (doseq [[field value] [[:pattern-id pattern-id]
                         [:agent-id agent-id]
                         [:session-id session-id]
                         [:task-id task-id]
                         [:rationale rationale]]]
    (require-text! field value))
  (when-not (ifn? action)
    (throw (ex-info "pattern action requires an executable action"
                    {:failure-kind :pattern-action-missing-action})))
  (let [psr-id (str "psr-" (UUID/randomUUID))
        common {:subject {:ref/type :pattern :ref/id pattern-id}
                :author agent-id
                :session-id session-id
                :pattern-id pattern-id}
        psr (merge common
                   {:evidence-id psr-id
                    :type :pattern-selection
                    :claim-type :observation
                    :body {:event :pattern-action/selected
                           :task-id task-id
                           :selected pattern-id
                           :rationale rationale
                           :automatic? true}
                    :tags [:psr :pattern-action-rpc]})
        psr-receipt (append-required! evidence-store psr :psr)]
    (try
      (let [result (action)
            pur (merge common
                       {:evidence-id (str "pur-" (UUID/randomUUID))
                        :type :pattern-outcome
                        :claim-type :conclusion
                        :in-reply-to psr-id
                        :body {:event :pattern-action/completed
                               :task-id task-id
                               :outcome :completed
                               :automatic? true}
                        :tags [:pur :pattern-action-rpc]})
            pur-receipt (append-required! evidence-store pur :pur)]
        {:ok true
         :result result
         :psr-receipt psr-receipt
         :pur-receipt pur-receipt})
      (catch Throwable action-error
        (let [pur (merge common
                         {:evidence-id (str "pur-" (UUID/randomUUID))
                          :type :pattern-outcome
                          :claim-type :conclusion
                          :in-reply-to psr-id
                          :body {:event :pattern-action/failed
                                 :task-id task-id
                                 :outcome :failed
                                 :error-class (.getName (class action-error))
                                 :automatic? true}
                          :tags [:pur :pattern-action-rpc]})
              pur-receipt (append-required! evidence-store pur :pur)]
          (throw (ex-info "pattern action failed after outcome persistence"
                          {:failure-kind :pattern-action-failed
                           :pattern-action/psr-receipt psr-receipt
                           :pattern-action/pur-receipt pur-receipt}
                          action-error)))))))
