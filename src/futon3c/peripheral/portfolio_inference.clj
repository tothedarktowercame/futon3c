(ns futon3c.peripheral.portfolio-inference
  "Portfolio Inference peripheral — derived attention surface.

   A flat tool-dispatch peripheral (not cycle machine) that exposes:

   - cross-repo mission-feature scoring with criterion-shape and
     family-mapping annotations
   - per-family precision-proxy aggregation (expectation drift, loss
     severity, witness quality, actionability gradient)
   - direct-promotion candidates: structural-recurring criteria
     touching candidate invariant families
   - the AIF loop entry points (aif-step / portfolio-step! /
     portfolio-heartbeat!) re-exposed as peripheral tools

   Companion to:
   - futon3c.peripheral.mission-control (read-only sensory surface)
   - futon3c.portfolio.* (the AIF generative-model engine)
   - futon5a/scripts/run_mission_feature_loop.clj (regression baseline)

   Design parallel: same shape as mission-control.clj — start/step/stop
   PeripheralRunner, internal dispatch via dispatch-pi-tool with fall
   through to generic tool dispatch.

   Fruit: {:precision-table [...] :promotion-candidates [...] :steps-taken int}
   Exit context: {:session-id str :latest-table-id str}"
  (:require [futon3c.blackboard :as bb]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.portfolio-inference-backend :as pib]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Tool dispatch — portfolio-inference-specific tools
;; =============================================================================

(defn- dispatch-pi-tool
  "Dispatch a portfolio-inference-specific tool.
   Returns {:ok true :result ...} or {:ok false :error ...}."
  [tool args state]
  (let [opts (or (first args) {})
        repos-from-state (:repos state)
        opts-with-repos (cond-> opts
                          (and repos-from-state (not (:repos opts)))
                          (assoc :repos repos-from-state))]
    (case tool
      :pi-mission-features
      (try
        {:ok true :result (pib/build-mission-features opts-with-repos)}
        (catch Exception e
          {:ok false :error (str "pi-mission-features failed: " (.getMessage e))}))

      :pi-precision-table
      (try
        {:ok true :result (pib/build-precision-table opts-with-repos)}
        (catch Exception e
          {:ok false :error (str "pi-precision-table failed: " (.getMessage e))}))

      :pi-promotion-candidates
      (try
        {:ok true :result (pib/build-promotion-candidates opts-with-repos)}
        (catch Exception e
          {:ok false :error (str "pi-promotion-candidates failed: " (.getMessage e))}))

      :pi-aif-step
      (try
        {:ok true :result (pib/aif-step-pure opts)}
        (catch Exception e
          {:ok false :error (str "pi-aif-step failed: " (.getMessage e))}))

      :pi-step
      (try
        {:ok true :result (pib/aif-step-live
                           (cond-> opts
                             (and (not (:evidence-store opts))
                                  (:evidence-store state))
                             (assoc :evidence-store (:evidence-store state))))}
        (catch Exception e
          {:ok false :error (str "pi-step failed: " (.getMessage e))}))

      :pi-heartbeat
      (try
        {:ok true :result (pib/heartbeat
                           (cond-> opts
                             (and (not (:evidence-store opts))
                                  (:evidence-store state))
                             (assoc :evidence-store (:evidence-store state))))}
        (catch Exception e
          {:ok false :error (str "pi-heartbeat failed: " (.getMessage e))}))

      :pi-adjacent
      (try
        {:ok true :result (pib/adjacent-set opts)}
        (catch Exception e
          {:ok false :error (str "pi-adjacent failed: " (.getMessage e))}))

      ;; Fall through to generic tools (read/glob/grep etc).
      nil)))

;; =============================================================================
;; Step dispatch
;; =============================================================================

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :portfolio-inference action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (or (dispatch-pi-tool tool args state)
                              (tools/dispatch-tool tool args spec backend))]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :portfolio-inference :tool-execution-failed
                             "Portfolio inference tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              ev (evidence/make-step-evidence
                  :portfolio-inference (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result})
                            (cond->
                                (= tool :pi-precision-table)
                              (assoc :latest-precision-table result)
                              (= tool :pi-promotion-candidates)
                              (assoc :latest-promotion-candidates result)))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

;; =============================================================================
;; PeripheralRunner
;; =============================================================================

(defrecord PortfolioInferencePeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :portfolio-inference context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            repos (or (:repos context) pib/default-extended-roots)
            ev (evidence/make-start-evidence :portfolio-inference sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :repos repos
                   :latest-precision-table nil
                   :latest-promotion-candidates nil
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (let [result (dispatch-step spec backend state action)]
      (when (:ok result)
        (bb/project! :portfolio-inference (:state result)))
      result))

  (stop [_ state reason]
    (let [fruit {:precision-table (:latest-precision-table state)
                 :promotion-candidates (:latest-promotion-candidates state)
                 :steps-taken (count (:steps state))}
          ev (evidence/make-stop-evidence
              :portfolio-inference
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)
                   :latest-table-id (when (:latest-precision-table state)
                                      (str "pi-table-" (:session-id state)))}
         :fruit fruit
         :evidence ev}))))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-portfolio-inference
  "Create a portfolio-inference peripheral with injected backend.
   Backend must satisfy ToolBackend (for generic tools like :read, :glob).
   Domain-specific tools (:pi-*) are dispatched internally."
  ([] (make-portfolio-inference (tools/make-mock-backend)))
  ([backend]
   (->PortfolioInferencePeripheral (common/load-spec :portfolio-inference) backend))
  ([spec backend]
   (->PortfolioInferencePeripheral spec backend)))
