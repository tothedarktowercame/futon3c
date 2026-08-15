(ns futon3c.peripheral.problem
  "Problem peripheral — one registered experimental problem per cycle."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.tools :as tools])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

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
  (into #{:begin-problem-cycle :load-registration :list-problems
          :problem-save :problem-load}
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

(defn- validate-loaded-state [current loaded]
  (when (not= (:cycle/mode current) (:cycle/mode loaded))
    {:failure :loaded-state-mode-mismatch
     :expected (:cycle/mode current)
     :actual (:cycle/mode loaded)}))

(defn- state-snapshot [state tool result]
  (when (and (= tool :problem-save) (:ok result))
    {:snapshot/subject {:ref/type :peripheral :ref/id "problem"}
     :snapshot/body {:snapshot :problem-save
                     :problem-id (:problem-id state)
                     :cycle-id (:current-cycle-id state)
                     :version (:version result)
                     :phase (:current-phase state)
                     :cycles-completed (:cycles-completed state)
                     :step-count (count (:steps state))}
     :snapshot/tags [:problem :snapshot]}))

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
   :state-io-tools {:save :problem-save :load :problem-load}
   :always-available-tools #{:problem-save :problem-load}
   :state-runtime-keys #{:cycle-config :evidence-store}
   :state-validate-fn validate-loaded-state
   :state-snapshot-fn state-snapshot
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

(def ^:private default-problem-state-root "data/problem-state")
(def ^:private problem-state-write-lock (Object.))

(defn- safe-cycle-id [state]
  (let [cycle-id (:current-cycle-id state)]
    (when-not (and (string? cycle-id)
                   (re-matches #"[A-Za-z0-9._-]+" cycle-id))
      (throw (ex-info "Problem state requires a safe active cycle id"
                      {:current-cycle-id cycle-id})))
    cycle-id))

(defn- version-number [filename]
  (some->> (re-matches #"v([0-9]+)\.edn" filename)
           second
           parse-long))

(defn- existing-versions [cycle-dir]
  (if (.isDirectory cycle-dir)
    (->> (.listFiles cycle-dir)
         (keep #(version-number (.getName %)))
         sort)
    []))

(defn- save-problem-state! [root state]
  ;; Version selection and rename are one JVM-critical section. ATOMIC_MOVE
  ;; makes publication indivisible, but Java leaves replacement of an existing
  ;; target implementation-specific; serial allocation prevents two engine
  ;; saves from ever presenting it with the same target.
  (locking problem-state-write-lock
    (let [cycle-id (safe-cycle-id state)
          cycle-dir (io/file root cycle-id)
          _ (Files/createDirectories (.toPath cycle-dir)
                                     (make-array FileAttribute 0))
          version (inc (or (last (existing-versions cycle-dir)) 0))
          target (.toPath (io/file cycle-dir (str "v" version ".edn")))
          temp (Files/createTempFile (.toPath cycle-dir) ".state-" ".tmp"
                                     (make-array FileAttribute 0))]
      (try
        (spit (.toFile temp) (pr-str state))
        (Files/move temp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE]))
        {:ok true :version version :path (str target)}
        (finally
          (Files/deleteIfExists temp))))))

(defn- load-problem-state [root cycle-id version]
  (when-not (and (string? cycle-id)
                 (re-matches #"[A-Za-z0-9._-]+" cycle-id))
    (throw (ex-info "Problem state requires a safe cycle id" {:cycle-id cycle-id})))
  (when-not (pos-int? version)
    (throw (ex-info "Problem state version must be a positive integer"
                    {:version version})))
  (edn/read-string (slurp (io/file root cycle-id (str "v" version ".edn")))))

(defrecord ProblemStateBackend [inner-backend root]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      :problem-save
      (let [[state] args]
        {:ok true :result (save-problem-state! root state)})

      :problem-load
      (let [[cycle-id version] args]
        {:ok true :result (load-problem-state root cycle-id version)})

      (tools/execute-tool inner-backend tool-id args))))

(defn make-problem-state-backend [inner-backend root]
  (->ProblemStateBackend inner-backend root))

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
   (make-problem backend dispatch-with-recall/run-dispatch!
                 default-problem-state-root))
  ([backend dispatch-fn]
   (make-problem backend dispatch-fn default-problem-state-root))
  ([backend dispatch-fn state-root]
   (cycle/make-cycle-peripheral
    problem-domain-config problem-spec
    (make-ground-control-backend
     (make-problem-state-backend backend state-root)
     dispatch-fn))))
