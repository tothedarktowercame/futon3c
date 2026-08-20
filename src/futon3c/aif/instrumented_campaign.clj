(ns futon3c.aif.instrumented-campaign
  "Explicit two-tick evidence campaign for R11, R12, R15, and R17.

  This is an opt-in wrapper around the ordinary full-loop actuator.  R11 and
  R15 transform its normal judgment inside the existing selection phase; the
  author/reviewer/grounding path is otherwise unchanged.  Tick A's independent
  witness updates the slow state used by Tick B, while R17 runs offline between
  the ticks and exposes its resulting structure to Tick B's input builders."
  (:require [futon2.aif.full-loop-runner :as full-loop]
            [futon2.aif.hierarchical-budget-adapter :as r11]
            [futon2.aif.r17-offline :as r17]
            [futon2.aif.temporal-hierarchy :as r15]
            [futon3c.aif.two-layer-calibration :as r12])
  (:import (java.time Instant)))

(def campaign-schema :futon3c.aif/instrumented-campaign-v2)

(defn- fail! [message data]
  (throw (ex-info message
                  (assoc data :failure-kind :invalid-instrumented-campaign))))

(defn- resolve-input [value & args]
  (if (fn? value) (apply value args) value))

(defn- opts->args [opts]
  (mapcat identity (sort-by (comp str key) opts)))

(defn- run-hierarchy [{:keys [state moves opts] :as input}]
  (when-not (and (map? input) (map? state) (vector? moves))
    (fail! "R15 input requires :state map and :moves vector" {:r15/input input}))
  (apply r15/hierarchical-rollout state moves (opts->args (or opts {}))))

(defn instrument-judgement
  "Pure R11+R15 selection transform used by the live runner hook.

  TICK supplies `:r11/request` and `:r15/input`; either may instead be a
  function.  The R11 function receives [judgment context].  The R15 function
  receives [judgment arbitration context].  Each R15 move identifies its R11
  proposal with `:proposal/id` (or, as a shorthand, `:move/id`).  The selected
  fast move must belong to R11's feasible portfolio and its action must already
  occur in the runner's normal ranked field."
  [judgement tick context]
  (let [request (resolve-input (:r11/request tick) judgement context)
        arbitration (r11/select-ranked-proposal-fields request)
        r15-input (resolve-input (:r15/input tick)
                                 judgement arbitration context)
        selected-ids (:selected-ids arbitration)
        eligible-moves
        (filterv #(contains? selected-ids
                             (or (:proposal/id %) (:move/id %)))
                 (:moves r15-input))
        _ (when (empty? eligible-moves)
            (fail! "R11 and R15 have no common feasible proposal"
                   {:r11/selected-ids selected-ids
                    :r15/move-ids (mapv #(or (:proposal/id %) (:move/id %))
                                        (:moves r15-input))}))
        effective-r15-input (assoc r15-input :moves eligible-moves)
        hierarchy (run-hierarchy effective-r15-input)
        policy-move (first (:policy hierarchy))
        source-move (first (filter #(= (:move/id policy-move) (:move/id %))
                                   eligible-moves))
        primary-move (merge source-move policy-move)
        primary-id (or (:proposal/id source-move)
                       (:proposal/id policy-move)
                       (:move/id policy-move))
        proposal (first (filter #(= primary-id (:id %))
                                (:selected arbitration)))
        action (:proposal/action proposal)
        ranked-entry (first (filter #(= action (:action %))
                                    (:ranked-actions judgement)))]
    (when-not proposal
      (fail! "R15 selected a move outside R11's feasible portfolio"
             {:proposal-id primary-id :r11/selected-ids selected-ids}))
    (when-not ranked-entry
      (fail! "Campaign primary action is absent from the live ranked judgment"
             {:proposal-id primary-id :action action}))
    {:judgement
     (-> judgement
         (assoc :r11/arbitration arbitration
                :r15/hierarchy {:slow-mode (get-in effective-r15-input
                                                   [:opts :slow-mode])
                                :input effective-r15-input
                                :result hierarchy
                                :primary-proposal-id primary-id})
         (update :decision merge
                 {:action action
                  :rank (:rank ranked-entry)
                  :controller-score (:controller-score ranked-entry)
                  :source :hierarchical-two-timescale-campaign}))
     :r11 arbitration
     :r15 {:input effective-r15-input
           :result hierarchy
           :primary-move primary-move
           :primary-proposal proposal}}))

(defn- selected-action-from-result [result]
  (get-in result [:checkpoints :selection :judgment :selected-action]))

(defn- calibration-input
  [campaign-id tick-id runner-opts instrumentation result]
  (let [action (get-in instrumentation [:r15 :primary-proposal
                                       :proposal/action])
        realised-action (selected-action-from-result result)
        reviewer-job (get-in result [:data :review-job])
        reviewer-id (:job-id reviewer-job)
        witness (get-in result [:data :witness])
        independent? (and (some? reviewer-id)
                          (not= (:author runner-opts)
                                (:reviewer runner-opts)))
        grounded? (and (= :grounded-change (:outcome result))
                       (true? (:resolved? witness))
                       (true? (:dial-moved? witness)))]
    {:layer-1/evidence
     #:layer-1{:prediction
               {:proposal-id (get-in instrumentation
                                     [:r15 :primary-proposal :id])
                :action action
                :policy-rollout-score
                (get-in instrumentation [:r15 :result
                                         :policy-rollout-score])}
               :realisation {:action realised-action
                             :outcome (:outcome result)
                             :attempt-id (:attempt-id result)}
               :verdict (if (= action realised-action) :pass :fail)
               :provenance
               #:provenance{:producer-id [:campaign campaign-id tick-id]
                            :trace-id (:attempt-id result)}}
     :layer-2/witness
     #:layer-2{:verdict (if grounded? :pass :fail)
               :independent? (boolean independent?)
               :observation witness
               :provenance
               #:provenance{:witness-id reviewer-id
                            :control (if independent? :independent :author)
                            :reviewer (:reviewer runner-opts)
                            :evidence-id (or (:discharge-id witness)
                                             (:implementation-id witness))}}}))

(defn- run-tick!
  [campaign-id tick slow-state structure runner-fn clock-fn]
  (let [started-ms (clock-fn)
        tick-id (:tick/id tick)
        context {:campaign/id campaign-id
                 :tick/id tick-id
                 :slow-state slow-state
                 :r17/structure structure}
        instrumentation (atom nil)
        transform
        (fn [judgement]
          (let [prepared (instrument-judgement judgement tick context)]
            (reset! instrumentation prepared)
            (:judgement prepared)))
        runner-opts (assoc (full-loop/config (:runner/opts tick))
                           :judgement-transform-fn transform
                           :campaign/id campaign-id
                           :campaign/tick-id tick-id)
        result (runner-fn runner-opts)
        ended-ms (clock-fn)
        prepared @instrumentation
        timing {:started-at (str (Instant/ofEpochMilli started-ms))
                :ended-at (str (Instant/ofEpochMilli ended-ms))
                :elapsed-ms (- ended-ms started-ms)}
        stop-line? (= :stop-the-line
                      (get-in result [:checkpoints :selection :judgment
                                      :selection-reasons :source]))]
    (cond
      prepared
      (let [evidence (calibration-input campaign-id tick-id runner-opts
                                        prepared result)
            report (r12/two-layer-report evidence)]
        {:tick/id tick-id
         :tick/status :campaign-selection-ran
         :tick/timing timing
         :runner/result result
         :transformed-judgement (:judgement prepared)
         :r11 (:r11 prepared)
         :r15 (:r15 prepared)
         :r12/input evidence
         :r12/report report})

      stop-line?
      {:tick/id tick-id
       :tick/status :campaign-preempted-by-stop-line
       :tick/timing timing
       :runner/result result
       :campaign/preemption
       {:source :stop-the-line
        :repair-id (get-in result [:checkpoints :selection :judgment
                                   :selection-reasons :repair-id])}}

      :else
      (fail! "The runner returned before the campaign selection hook ran"
             {:tick/id tick-id :runner/result result}))))

(defn- next-slow-state [slow-state tick]
  (if (true? (get-in tick [:r12/report :gate/clear?]))
    (let [move (get-in tick [:r15 :primary-move])
          action (get-in tick [:r15 :primary-proposal :proposal/action])]
      (r15/advance-slow-state
       slow-state
       {:fast/action-class (or (:move/class move) (:type action))
        :fast/witnessed? true
        :fast/succeeded? true
        :fast/attempt-id (get-in tick [:runner/result :attempt-id])}
       {:as-of (or (:tick/as-of tick) (:tick/id tick))
        :run-id (str "r15/" (:tick/id tick))
        :evidence-ref (or (get-in tick [:r12/input :layer-2/witness
                                       :layer-2/provenance
                                       :provenance/evidence-id])
                          (get-in tick [:runner/result :attempt-id]))}))
    (assoc slow-state :slow/feedback
           {:status :not-updated
            :reason (get-in tick [:r12/report :gate/reason])})))

(defn replay-decisions
  "Replay every pure decision boundary recorded by a campaign."
  [campaign]
  (let [tick-results
        (mapv
         (fn [tick]
           (if (= :campaign-preempted-by-stop-line (:tick/status tick))
             {:tick/id (:tick/id tick)
              :replay/status :not-applicable
              :r11/identical? true
              :r15/identical? true
              :r12/identical? true}
             (let [r11-replay (r11/replay (get-in tick [:r11 :replay/receipt]))
                   r15-replay (run-hierarchy (get-in tick [:r15 :input]))
                   r12-replay (r12/two-layer-report (:r12/input tick))]
               {:tick/id (:tick/id tick)
                :r11/identical? (:replay/identical? r11-replay)
                :r15/identical? (= r15-replay (get-in tick [:r15 :result]))
                :r12/identical? (= r12-replay (:r12/report tick))})))
         (:campaign/ticks campaign))
        r17-envelope (:campaign/r17 campaign)
        r17-identical? (if r17-envelope
                         (= r17-envelope (r17/replay r17-envelope))
                         true)]
    {:ticks tick-results
     :r17/identical? r17-identical?
     :all-identical?
     (and r17-identical?
          (every? #(and (:r11/identical? %)
                        (:r15/identical? %)
                        (:r12/identical? %))
                  tick-results))}))

(defn- confirm-record! [record-fn record]
  (let [record (assoc record :campaign/replay (replay-decisions record))
        receipt (record-fn record)]
    (when-not (true? (:ok receipt))
      (throw (ex-info "Instrumented campaign record was not confirmed"
                      {:failure-kind :campaign-record-unconfirmed
                       :campaign/id (:campaign/id record)
                       :receipt receipt
                       :record record})))
    (assoc record :campaign/storage-receipt receipt)))

(defn- campaign-timing [started-ms ended-ms r17-timing]
  (cond-> {:started-at (str (Instant/ofEpochMilli started-ms))
           :ended-at (str (Instant/ofEpochMilli ended-ms))
           :elapsed-ms (- ended-ms started-ms)}
    r17-timing (assoc :r17 r17-timing)))

(defn- stopped-record
  [campaign-id campaign-plan initial-slow-state started-ms ended-ms tick]
  (let [preempted? (= :campaign-preempted-by-stop-line (:tick/status tick))]
    {:campaign/schema campaign-schema
     :campaign/id campaign-id
     :campaign/status (if preempted?
                        :campaign-preempted-by-stop-line
                        :evidence-gate-failed)
     :campaign/compliant? false
     :campaign/plan campaign-plan
     :campaign/timing (campaign-timing started-ms ended-ms nil)
     :campaign/initial-slow-state initial-slow-state
     :campaign/ticks [tick]}))

(defn run-two-tick!
  "Run and durably receipt an explicit two-tick campaign.

  Required keys:
    :campaign/id, :initial-slow-state, :tick-a, :tick-b,
    :r17/run, and :record-fn.

  `:tick-b` and `:r17/run` may be functions.  The R17 function receives Tick
  A; the Tick B function receives a context containing Tick A, the updated
  slow state, and the R17 envelope.  `record-fn` must confirm with `{:ok true}`;
  the campaign never calls an unconfirmed write successful."
  [{campaign-id :campaign/id
    campaign-plan :campaign/plan
    :keys [initial-slow-state tick-a tick-b record-fn runner-fn clock-fn]
    r17-spec :r17/run}]
  (when-not (and campaign-id tick-a tick-b r17-spec (fn? record-fn))
    (fail! "Campaign id, two ticks, R17 input, and record-fn are required"
           {:campaign/id campaign-id}))
  (let [clock-fn (or clock-fn #(System/currentTimeMillis))
        campaign-started-ms (clock-fn)
        runner-fn (or runner-fn full-loop/run-opportunity!)
        first-tick (run-tick! campaign-id tick-a initial-slow-state nil
                              runner-fn clock-fn)]
    (if (or (= :campaign-preempted-by-stop-line (:tick/status first-tick))
            (not (true? (get-in first-tick [:r12/report :gate/clear?]))))
      (confirm-record!
       record-fn
       (stopped-record campaign-id campaign-plan initial-slow-state
                       campaign-started-ms (clock-fn) first-tick))
      (let [slow-after-a (next-slow-state initial-slow-state
                                          (assoc first-tick
                                                 :tick/as-of (:tick/as-of tick-a)))
            r17-input (resolve-input r17-spec first-tick)
            r17-started-ms (clock-fn)
            structure-envelope (r17/run r17-input)
            r17-ended-ms (clock-fn)
            r17-timing {:started-at (str (Instant/ofEpochMilli r17-started-ms))
                        :ended-at (str (Instant/ofEpochMilli r17-ended-ms))
                        :elapsed-ms (- r17-ended-ms r17-started-ms)}
            tick-b-context {:campaign/id campaign-id
                            :tick-a first-tick
                            :slow-state slow-after-a
                            :r17/envelope structure-envelope
                            :r17/structure (:r17/resulting-structure
                                           structure-envelope)}
            tick-b (resolve-input tick-b tick-b-context)
            second-tick (run-tick! campaign-id tick-b slow-after-a
                                   (:r17/resulting-structure structure-envelope)
                                   runner-fn clock-fn)
            campaign-ended-ms (clock-fn)
            compliant? (and (not= :campaign-preempted-by-stop-line
                                  (:tick/status second-tick))
                            (true? (get-in second-tick
                                          [:r12/report :gate/clear?])))
            record {:campaign/schema campaign-schema
                    :campaign/id campaign-id
                    :campaign/status
                    (cond
                      (= :campaign-preempted-by-stop-line
                         (:tick/status second-tick))
                      :campaign-preempted-by-stop-line

                      compliant? :two-tick-evidence-complete
                      :else :evidence-gate-failed)
                    :campaign/compliant? compliant?
                    :campaign/plan campaign-plan
                    :campaign/timing
                    (campaign-timing campaign-started-ms campaign-ended-ms
                                     r17-timing)
                    :campaign/initial-slow-state initial-slow-state
                    :campaign/slow-state-after-tick-a slow-after-a
                    :campaign/r17 structure-envelope
                    :campaign/ticks [first-tick second-tick]}]
        (confirm-record! record-fn record)))))
