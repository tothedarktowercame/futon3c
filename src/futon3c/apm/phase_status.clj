(ns futon3c.apm.phase-status
  "Canonical closed vocabulary for statuses at APM execution boundaries.

  Boundaries have distinct domains: phase results propagate through frame
  supervisors, while stepper gate and workspace-retirement statuses do not.
  `propagation` declares actual translations, and `closure-findings` identifies
  an omitted mapping or a consumer that would reject the translated value."
  (:require [clojure.set :as set]))

(def boundary-status-classes
  {:phase-driver
   {:awaiting-terminal :waiting-terminal
    :awaiting-prior-job-terminal :waiting-terminal
    :orphaned :waiting-orphan-recovery
    :awaiting-orphan-recovery :waiting-orphan-recovery
    :awaiting-apparatus-repair :waiting-apparatus-repair
    :awaiting-substrate :waiting-substrate
    :transport-retry-scheduled :waiting-transport-retry
    :terminal-collected :terminal-evidence-collected
    :certified :certified}
   :live-supervisor-frame
   {:parked :waiting-terminal
    :orphan-recovery-scheduled :waiting-orphan-recovery
    :awaiting-substrate :waiting-substrate
    :transport-retry-scheduled :waiting-transport-retry
    :terminal-collected :terminal-evidence-collected
    :claim-recovered :claim-recovered
    :phase-advanced :phase-advanced
    :frame-complete :frame-complete}
   :problem-queue-frame
   {:parked :waiting-terminal
    :orphan-recovery-scheduled :waiting-orphan-recovery
    :awaiting-substrate :waiting-substrate
    :transport-retry-scheduled :waiting-transport-retry
    :terminal-collected :terminal-evidence-collected
    :claim-recovered :claim-recovered
    :phase-advanced :phase-advanced
    :frame-parked :frame-parked
    :frame-complete :frame-complete}
   :live-batch-frame
   {:parked :waiting-terminal
    :orphan-recovery-scheduled :waiting-orphan-recovery
    :awaiting-substrate :waiting-substrate
    :transport-retry-scheduled :waiting-transport-retry
    :terminal-collected :terminal-evidence-collected
    :claim-recovered :claim-recovered
    :phase-advanced :phase-advanced
    :frame-complete :frame-complete}
   :problem-queue-result
   {:frame-prepared :queue-progress
    :orphan-recovery-scheduled :queue-progress
    :parked :queue-progress
    :awaiting-substrate :queue-waiting-substrate
    :transport-retry-scheduled :queue-progress
    :phase-advanced :queue-progress
    :terminal-collected :queue-progress
    :claim-recovered :queue-progress
    ;; A refuted statement voids its slot and dispatches a repair to a guide.
    ;; While that repair is pending the tick keeps returning this, which is
    ;; progress -- work is in flight -- not a terminal or waiting state.
    :guide-statement-repair-dispatched :queue-progress
    :batch-paused :queue-terminal
    :batch-complete :queue-terminal}
   :jit-queue-postcondition
   {:frame-prepared :queue-progress
    :orphan-recovery-scheduled :queue-progress
    :parked :queue-progress
    :awaiting-substrate :queue-waiting-substrate
    :transport-retry-scheduled :queue-progress
    :phase-advanced :queue-progress
    :terminal-collected :queue-progress
    :claim-recovered :queue-progress
    ;; Omitting this stopped the campaign on 2026-09-08: f199's statement was
    ;; refuted, the queue dispatched a guide repair, and the coordinator
    ;; rejected its own supervisor's status as a postcondition violation. The
    ;; status had been emitted by problem-queue-supervisor since it was
    ;; written and declared in no boundary class, so closure-findings could
    ;; not see it -- that fence compares declarations to declarations.
    :guide-statement-repair-dispatched :queue-progress
    :batch-paused :queue-terminal
    :batch-complete :queue-terminal}
   :campaign-stepper-gate
   {:pass :passing :fail :failing}
   :queued-frame-retirement
   {:already-retired :complete :not-retired :pending}})

(def propagation
  [{:producer :phase-driver
    :consumer :live-supervisor-frame
    :mapping {:awaiting-terminal :parked
              :awaiting-prior-job-terminal :parked
              :orphaned :orphan-recovery-scheduled
              :awaiting-orphan-recovery :orphan-recovery-scheduled
              :awaiting-apparatus-repair :parked
              :awaiting-substrate :awaiting-substrate
              :transport-retry-scheduled :transport-retry-scheduled
              :terminal-collected :terminal-collected
              :certified :phase-advanced}}
   {:producer :live-supervisor-frame
    :consumer :problem-queue-frame
    :mapping {:parked :parked
              :orphan-recovery-scheduled :orphan-recovery-scheduled
              :awaiting-substrate :awaiting-substrate
              :transport-retry-scheduled :transport-retry-scheduled
              :terminal-collected :terminal-collected
              :claim-recovered :claim-recovered
              :phase-advanced :phase-advanced
              :frame-complete :frame-complete}}
   {:producer :live-supervisor-frame
    :consumer :live-batch-frame
    :mapping {:parked :parked
              :orphan-recovery-scheduled :orphan-recovery-scheduled
              :awaiting-substrate :awaiting-substrate
              :transport-retry-scheduled :transport-retry-scheduled
              :terminal-collected :terminal-collected
              :claim-recovered :claim-recovered
              :phase-advanced :phase-advanced
              :frame-complete :frame-complete}}
   {:producer :problem-queue-result
    :consumer :jit-queue-postcondition
    :mapping {:frame-prepared :frame-prepared
              :orphan-recovery-scheduled :orphan-recovery-scheduled
              :parked :parked
              :awaiting-substrate :awaiting-substrate
              :transport-retry-scheduled :transport-retry-scheduled
              :phase-advanced :phase-advanced
              :terminal-collected :terminal-collected
              :claim-recovered :claim-recovered
              :guide-statement-repair-dispatched
              :guide-statement-repair-dispatched
              :batch-paused :batch-paused
              :batch-complete :batch-complete}}])

(defn known-statuses
  ([boundary] (set (keys (get boundary-status-classes boundary))))
  ([] (into {} (map (fn [boundary] [boundary (known-statuses boundary)]))
            (keys boundary-status-classes))))

(defn classify [boundary status]
  (get-in boundary-status-classes [boundary status] :unknown))

(defn closure-findings
  ([] (closure-findings boundary-status-classes propagation))
  ([vocabulary routes]
   (vec
    (mapcat
     (fn [{:keys [producer consumer mapping]}]
       (let [producer-statuses (set (keys (get vocabulary producer)))
             consumer-statuses (set (keys (get vocabulary consumer)))
             mapped-statuses (set (keys mapping))]
         (concat
          (for [status (sort (set/difference producer-statuses mapped-statuses))]
            {:error/code :phase-status-propagation-mapping-missing
             :producer producer :consumer consumer :status status})
          (for [[status output] (sort-by (comp pr-str key) mapping)
                :when (not (contains? consumer-statuses output))]
            {:error/code :phase-status-consumer-vocabulary-incomplete
             :producer producer :consumer consumer
             :status status :output-status output}))))
     routes))))
