(ns futon3c.peripheral.problem
  "Problem peripheral — one registered experimental problem per cycle."
  (:require [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.tools :as tools]))

(def phase-order
  [:register :frame :guided-solve :intervene :student-attempts
   :adjudicate :promote :close])

(def advance :advance-problem-phase)

(def base-phase-tools
  {:register #{:read-registration :validate-registration :snapshot-store
               :freeze-stratum :pin-resources advance}
   :frame #{:emit-frame advance}
   :guided-solve #{:dispatch-solver :guide-solver :read-substrate advance}
   :intervene #{advance}
   :student-attempts #{:dispatch-student-fresh :read-attempt-result advance}
   :adjudicate #{:write-disposition :write-use advance}
   :promote #{:promote-artifact advance}
   :close #{:emit-trace :validate-trace :write-authorization advance}})

(def required-outputs
  {:register #{:registration :store-snapshot :stratum-frozen-at
               :environment-revision :harness-revision}
   :frame #{:frame :containment-probe}
   :guided-solve #{:solver-attempt :ground-control-events :memory-offers}
   :intervene #{:intervention}
   :student-attempts #{:student-attempts :memory-uses}
   :adjudicate #{:disposition :launch-gate-event}
   :promote #{:promotion-result}
   :close #{:measurement :trace}})

(def all-tools
  (into #{:begin-problem-cycle :load-registration :list-problems}
        (mapcat identity (vals base-phase-tools))))

(def tool-ops
  (zipmap all-tools
          (map #(if (#{:read-registration :validate-registration
                       :read-substrate :read-attempt-result :list-problems}
                     %)
                  :observe :action)
               all-tools)))

(defn- fruit [state]
  (some (fn [{:keys [tool result]}] (when (= :emit-trace tool) result))
        (reverse (:steps state))))

(defn- environment-arms-match [outputs]
  (let [pinned (:environment-revision outputs)
        solver (get-in outputs [:solver-attempt :cycle/environment-revision])
        students (map :cycle/environment-revision (:student-attempts outputs))]
    (when-not (and (= pinned solver) (every? #(= pinned %) students))
      {:failure :environment-mismatch-between-arms
       :pinned pinned :solver solver :students (vec students)})))

(defn- autoconf [context config]
  (let [intervention-tool (case (:cycle/mode context)
                            :store-mode :write-substrate
                            :harness-mode :tune-harness
                            nil)]
    (cond-> config
      intervention-tool
      (update-in [:phase-tools :intervene] conj intervention-tool))))

(def problem-domain-config
  {:domain-id :problem
   :phase-order phase-order
   :phase-tools base-phase-tools
   :setup-tools #{:begin-problem-cycle :load-registration :list-problems}
   :tool-ops (assoc tool-ops
                    :write-substrate :action
                    :tune-harness :action)
   :required-outputs required-outputs
   :enforce-required-outputs? true
   :output-invariants
   [{:id :environment-arms-match
     :requires #{:environment-revision :solver-attempt :student-attempts}
     :check environment-arms-match}]
   :cycle-begin-tool :begin-problem-cycle
   :cycle-advance-tool advance
   :state-init-fn (fn [context]
                    {:problem-id (:problem-id context)
                     :cycle/mode (:cycle/mode context)
                     :cycle/deposit-state (:cycle/deposit-state context)
                     :cycle/paired-with (:cycle/paired-with context)})
   :autoconf-fn autoconf
   :fruit-fn fruit
   :exit-context-fn (fn [state]
                      {:session-id (:session-id state)
                       :problem-id (:problem-id state)})})

(def problem-spec
  {:peripheral/id :problem
   :peripheral/tools (conj all-tools :write-substrate :tune-harness)
   :peripheral/scope :full-codebase
   :peripheral/entry #{:user-request}
   :peripheral/exit #{:user-request}
   :peripheral/context {}})

(def ^:private dispatch-channels
  {:dispatch-solver :push+pull
   :dispatch-student-fresh :pull-only})

(defrecord GroundControlBackend [inner-backend dispatch-fn]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (if-let [memory-channel (get dispatch-channels tool-id)]
      (let [[opts packet] args
            ;; assoc LAST: the role fixes the channel and a caller cannot
            ;; override it.  A caller-supplied :push to the student would be a
            ;; containment breach, so this precedence is load-bearing.
            dispatch-result (try (dispatch-fn (assoc (or opts {})
                                                     :memory-channel memory-channel)
                                              packet)
                                 (catch Throwable t t))]
        (if (instance? Throwable dispatch-result)
          ;; A failed BELL is not a failed recall.  Recall is best-effort and
          ;; degrades to an empty offer; the bell is the work itself, so it must
          ;; surface as a structured tool failure rather than escape as an
          ;; exception and break the ToolBackend contract.
          {:ok false
           :error (str "ground-control dispatch failed: "
                       (.getMessage ^Throwable dispatch-result))}
          ;; This is the receipt emitted by the dispatcher that made the offer,
          ;; not a second account synthesized by the cycle machine.  Even an
          ;; empty/failed recall has one offered receipt.
          {:ok true
           :result (assoc dispatch-result
                          :memory-offers [(:evidence dispatch-result)])}))
      (tools/execute-tool inner-backend tool-id args))))

(defn make-ground-control-backend
  "Wrap a cycle backend with real ground-control dispatches.

  Dispatch tools take `[opts packet]`.  The role fixes the memory channel:
  solver is `:push+pull`; student is `:pull-only`.  `dispatch-fn` is injectable
  so envelope tests never bell a live agent."
  ([inner-backend]
   (make-ground-control-backend inner-backend
                                dispatch-with-recall/run-dispatch!))
  ([inner-backend dispatch-fn]
   (->GroundControlBackend inner-backend dispatch-fn)))

(defn make-problem
  ([] (make-problem (tools/make-mock-backend)))
  ([backend]
   (cycle/make-cycle-peripheral
    problem-domain-config problem-spec (make-ground-control-backend backend)))
  ([backend dispatch-fn]
   (cycle/make-cycle-peripheral
    problem-domain-config problem-spec
    (make-ground-control-backend backend dispatch-fn))))
