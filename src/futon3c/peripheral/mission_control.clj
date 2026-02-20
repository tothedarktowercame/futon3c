(ns futon3c.peripheral.mission-control
  "Mission control peripheral — portfolio-level observation and review.

   A simpler peripheral (not cycle machine) that provides tools for:
   - Cross-repo mission inventory scanning
   - Devmap reading and coverage analysis
   - Mana pool queries
   - Portfolio review emission as evidence

   Design decision D1: simpler peripheral. Portfolio review is not cyclic
   in the way proof or mission development is. A flat tool-dispatch
   peripheral with structured evidence emission is more natural.

   Fruit: {:review PortfolioReview :steps-taken int}
   Exit context: {:session-id str :review-id str}"
  (:require [futon3c.blackboard :as bb]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Tool dispatch — mission control domain tools
;; =============================================================================

(defn- dispatch-mc-tool
  "Dispatch a mission-control-specific tool.
   Returns {:ok true :result ...} or {:ok false :error ...}."
  [tool args state]
  (let [repos (:repos state)]
    (case tool
      :mc-inventory
      {:ok true :result (mcb/build-inventory repos)}

      :mc-devmaps
      (let [f5 (or (:futon5 repos) (:futon5 mcb/default-repo-roots))]
        {:ok true :result (mcb/read-all-devmaps f5)})

      :mc-coverage
      (let [missions (mcb/build-inventory repos)
            f5 (or (:futon5 repos) (:futon5 mcb/default-repo-roots))
            devmaps (mcb/read-all-devmaps f5)]
        {:ok true :result (mcb/compute-coverage devmaps missions)})

      :mc-mana
      (let [f5 (or (:futon5 repos) (:futon5 mcb/default-repo-roots))]
        {:ok true :result (mcb/query-mana f5)})

      :mc-review
      {:ok true :result (mcb/build-portfolio-review repos)}

      :mc-bulletin
      (let [text (first args)]
        (if (string? text)
          {:ok true :result {:bulletin text}}
          {:ok false :error "mc-bulletin requires a string argument"}))

      ;; Tickle — stall detection and agent paging
      :tickle-scan
      {:ok true :result (mcb/tickle-scan (:evidence-store state) (or (first args) {}))}

      :tickle-page
      {:ok true :result (mcb/tickle-page (:evidence-store state) (first args))}

      :tickle-cycle
      {:ok true :result (mcb/tickle-cycle (:evidence-store state) (or (first args) {}))}

      ;; Fall through to generic tools
      nil)))

;; =============================================================================
;; Step dispatch
;; =============================================================================

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :mission-control action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          ;; Try domain-specific tools first, then fall back to generic dispatch
          dispatch-result (or (dispatch-mc-tool tool args state)
                              (tools/dispatch-tool tool args spec backend))]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :mission-control :tool-execution-failed
                             "Mission control tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              ev (evidence/make-step-evidence
                  :mission-control (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result})
                            ;; Track the latest review if one was produced
                            (cond-> (and (= tool :mc-review) (:portfolio/missions result))
                              (assoc :latest-review result)))
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

(defrecord MissionControlPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :mission-control context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            repos (or (:repos context) mcb/default-repo-roots)
            ev (evidence/make-start-evidence :mission-control sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :repos repos
                   :latest-review nil
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (let [result (dispatch-step spec backend state action)]
      (when (:ok result)
        (bb/project! :mission-control (:state result)))
      result))

  (stop [_ state reason]
    (let [review (:latest-review state)
          fruit {:review review
                 :steps-taken (count (:steps state))}
          ev (evidence/make-stop-evidence
              :mission-control
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
                   :review-id (when review
                                (str "review-" (:session-id state)))}
         :fruit fruit
         :evidence ev}))))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-mission-control
  "Create a mission control peripheral with injected backend.
   Backend must satisfy ToolBackend (for generic tools like :read, :glob).
   Domain-specific tools (:mc-*) are dispatched internally."
  ([] (make-mission-control (tools/make-mock-backend)))
  ([backend]
   (->MissionControlPeripheral (common/load-spec :mission-control) backend))
  ([spec backend]
   (->MissionControlPeripheral spec backend)))
