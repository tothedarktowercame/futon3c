(ns futon3c.peripheral.live-wm-selection
  "Live-store verification and reason-bearing strategic selection.

   This composes the accepted Phase 4-7 seams without changing their
   admissible set. Relation weights remain illustrative. The result earns
   authority from chain integrity and auditability, not from a claim that the
   exploratory model has demonstrated better mission selection."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [futon3c.evidence.futon1b-backend :as futon1b]
            [futon3c.peripheral.strategic-cascade :as cascade]
            [futon3c.peripheral.strategic-outcomes :as outcomes]
            [futon3c.peripheral.strategic-policies :as policies]))

(def algorithm :live-wm-selection/reason-bearing-v1)
(def operator-decision-evidence-id
  "6e6f56a1-b9d7-4f83-928f-3a211ef890a0")
(def rollback-boundary "e74c7e7")
(def delivery-qa-endpoint
  "http://127.0.0.1:7070/api/alpha/morning-brief/addendum")

(defn- trace-by-id
  [traces id key-name]
  (or (first (filter #(= id (get % key-name)) traces))
      (throw (ex-info "required verification trace is absent"
                      {:id id :key key-name}))))

(defn- candidate-ranking
  [rows]
  (mapv :mission-id rows))

(defn- recalled-memory-ids
  [outer]
  (->> (:steps outer)
       (mapcat #(get-in % [:result :recalls]))
       (mapcat :memories)
       (map :memory/id)
       distinct
       sort
       vec))

(defn- recall-audits
  [outer]
  (mapv
   (fn [step]
     {:pattern-id (:pattern-id step)
      :elapsed-ms (get-in step [:result :recalls 0 :elapsed-ms])
      :audit (get-in step [:result :recalls 0 :audit])})
   (:steps outer)))

(defn run-verification
  "Run Phase 5-7 from the ordinary live WM recall seam.

   CTX must omit :recall-fn for the live acceptance run. Tests may inject it
   to exercise the pure composition. INPUT carries the already-reviewed
   control graph, accepted phase fixtures, a bounded illustrative operator,
   and a scheduler-habit counterfactual restricted to the same candidates."
  [ctx {:keys [phase5 phase6 phase7 rung2 control-edges ranking-options
               strategic-decision-id scheduler-habit-ranking latency]
        :as input}]
  (when-not (and (map? input)
                 (map? phase5)
                 (map? phase6)
                 (map? phase7)
                 (map? rung2)
                 (vector? control-edges)
                 (seq control-edges)
                 (map? ranking-options)
                 (string? strategic-decision-id)
                 (vector? scheduler-habit-ranking)
                 (map? latency))
    (throw (ex-info "invalid live WM verification input"
                    {:input-keys (keys input)})))
  (let [outer
        (cascade/execute-outer-cascade
         ctx (:cascade phase5) control-edges (:dependencies phase5)
         (:transition-warrants phase5)
         {:budget (count (get-in phase5 [:cascade :shown]))
          :memory-limit (:memory-limit input 10)})
        recall-failures
        (->> (:steps outer)
             (mapcat #(get-in % [:result :recalls]))
             (remove :ok)
             (mapv #(select-keys % [:endpoint :elapsed-ms :error])))
        _recall-check
        (when (seq recall-failures)
          (throw
           (ex-info "live WM shared-memory recall failed"
                    {:first-failed-seam :live-memory-recall
                     :failure-kind :strategic-selection-recall-failed
                     :recall-failures recall-failures})))
        candidates (get-in outer [:admissible-projection :candidates])
        candidate-domain (candidate-ranking candidates)
        candidate-set (set candidate-domain)
        _ (when (empty? candidate-domain)
            (throw (ex-info "live WM verification produced an empty frontier"
                            {:first-failed-seam :phase5-admissible-projection
                             :outer outer})))
        checkpoint
        (cascade/checkpoint-ranking
         outer (merge (:checkpoint phase5) ranking-options))
        phase6-result (outcomes/run-dark-ablation outer phase6)
        judgement
        (trace-by-id (:judgements phase6)
                     (:judgement-id rung2) :judgement-id)
        transition
        (trace-by-id (:held-out-outcomes phase6)
                     (:transition-id rung2) :transition-id)
        rung2-result
        (outcomes/outcome-conditioned-operator-update
         (:admissible-projection outer)
         (:training-transitions phase6)
         transition
         judgement
         {:min-observations (:min-outcome-observations phase6)
          :minimum-promotion-sample-size
          (:minimum-promotion-sample-size phase6)
          :phase6-promotion (:outcome-promotion phase6-result)
          :phase6-outcome-evaluation (:outcome-evaluation phase6-result)})
        phase6-judgement
        (trace-by-id (:judgement-traces phase6-result)
                     (:judgement-id rung2) :judgement-id)
        additive-ranking
        (candidate-ranking
         (get-in phase6-judgement [:rankings :current-additive]))
        phase7-result (policies/run-shadow-window outer phase7)
        strategic
        (trace-by-id (:shadow-traces phase7-result)
                     strategic-decision-id :decision-id)
        selected (first (:ranked-policies strategic))
        fixed-ranking
        (get-in checkpoint [:retrieval-checkpoint :control-ranking])
        typed-ranking
        (get-in checkpoint [:retrieval-checkpoint :typed-ranking])
        outcome-ranking
        (get-in rung2-result [:rankings :typed-after-one-outcome])
        compared-rankings
        [fixed-ranking typed-ranking outcome-ranking additive-ranking
         scheduler-habit-ranking]
        _ (when-not (every? #(= candidate-set (set %)) compared-rankings)
            (throw (ex-info
                    "a comparison ranking escaped the admissible candidate set"
                    {:candidate-domain candidate-domain
                     :rankings compared-rankings})))
        _ (when-not (and selected
                         (:explanation-complete? selected)
                         (seq (:memory-ids selected)))
            (throw (ex-info "strategic policy explanation is incomplete"
                            {:selected selected})))]
    {:status :verified-live-selection
     :algorithm algorithm
     :authority
     {:basis :chain-integrity-and-auditability
      :demonstrated-better-selection? false
      :admissible-set :unchanged-phase1-4
      :relation-weight-semantics :illustrative
      :earned-semantics-gate
      {:minimum-independently-witnessed-live-transitions 20
       :satisfied? false}}
     :candidate-domain candidate-domain
     :live-memory-ids (recalled-memory-ids outer)
     :recall-audits (recall-audits outer)
     :fixed-ranking fixed-ranking
     :typed-ranking typed-ranking
     :outcome-conditioned-ranking outcome-ranking
     :strategic-policy-ranking
     (mapv :policy-id (:ranked-policies strategic))
     :selected-policy-id (:policy-id selected)
     :selected-mission-ids (:mission-ids selected)
     :selected-memory-ids (:memory-ids selected)
     :selected-policy
     (select-keys selected
                  [:policy-id :mission-ids :memory-ids :e-s :predicted-g-s
                   :hard-support :proposal-reasons :provenance
                   :shadow-probability :explanation-complete?])
     :relation-contributions
     (get-in checkpoint
             [:retrieval-checkpoint :relation-contributions])
     :path-diversity
     (get-in checkpoint [:retrieval-checkpoint :path-diversity])
     :budget (:budget outer)
     :blockers (:excluded-missions outer)
     :holes (:holes outer)
     :calibration
     {:status (get-in rung2-result [:promotion :decision-reason])
      :sample-count (get-in rung2-result [:promotion :sample-count])
      :minimum (get-in rung2-result [:promotion :minimum-sample-size])
      :phase6-advance?
      (get-in rung2-result [:promotion :phase6-advance?])
      :exploratory-values-are-calibrated-probabilities? false}
     :one-outcome-update
     {:transition (select-keys (:transition rung2-result)
                              [:transition-id :mission-id :outcome
                               :witness-status :witness-id])
      :outcome-update (:outcome-update rung2-result)
      :operator-update (:operator-update rung2-result)}
     :latency latency
     :counterfactuals
     {:fixed fixed-ranking
      :additive-controller additive-ranking
      :scheduler-habit scheduler-habit-ranking}
     :actuation
     {:status :pending-downstream-gates
      :authorized? false
      :executed? false}
     :components
     {:outer outer
      :checkpoint checkpoint
      :phase6 phase6-result
      :rung2 rung2-result
      :phase7 phase7-result}}))

(defn- fixture-root
  []
  (or
   (some
    (fn [candidate]
      (let [root (io/file candidate "holes/labs/M-typed-memories")]
        (when (.isDirectory root) (.getPath root))))
    [(System/getProperty "user.dir")
     (str (System/getProperty "user.dir") "/../futon3c")
     "/home/joe/code/futon3c"])
   (throw (ex-info "cannot locate live WM selection fixtures" {}))))

(defn- read-fixture
  [root name]
  (edn/read-string (slurp (io/file root name))))

(defn enforce-serving-cache-gate
  "Require the selection used by the WM decision to come from a warm read.

   RUN-SELECTION is invoked once. If any endpoint exceeds MAX-ENDPOINT-MS,
   that call is treated only as warm-up and an immediate second selection is
   required to meet the bound. A second miss fails closed. This gate belongs
   here, immediately around selection, because the heavyweight WM scan can
   evict a cache warmed before the scheduler tick."
  [run-selection max-endpoint-ms]
  (letfn [(endpoint-ms [selection]
            (mapv
             (fn [{:keys [pattern-id elapsed-ms]}]
               {:pattern-id pattern-id :elapsed-ms elapsed-ms})
             (:recall-audits selection)))
          (within-bound? [rows]
            (and (seq rows)
                 (every? #(and (number? (:elapsed-ms %))
                               (<= (:elapsed-ms %) max-endpoint-ms))
                         rows)))]
    (let [first-selection (run-selection)
          first-latencies (endpoint-ms first-selection)]
      (if (within-bound? first-latencies)
        (assoc first-selection
               :serving-cache-gate
               {:status :warm
                :attempt-count 1
                :maximum-endpoint-ms max-endpoint-ms
                :accepted-endpoint-latencies first-latencies})
        (let [accepted (run-selection)
              accepted-latencies (endpoint-ms accepted)]
          (when-not (within-bound? accepted-latencies)
            (throw
             (ex-info
              "serving projection cache failed its immediate recheck"
              {:maximum-endpoint-ms max-endpoint-ms
               :warm-up-endpoint-latencies first-latencies
               :recheck-endpoint-latencies accepted-latencies})))
          (assoc accepted
                 :serving-cache-gate
                 {:status :warmed-and-rechecked
                  :attempt-count 2
                  :maximum-endpoint-ms max-endpoint-ms
                  :warm-up-endpoint-latencies first-latencies
                  :accepted-endpoint-latencies accepted-latencies}))))))

(defn authorize-bounded-autonomy
  "Promote a cache-gated selection to machine-determined enactment authority.

   Evidence 6e6f56a1-b9d7-4f83-928f-3a211ef890a0 retires operator
   confirm-to-enact. This does not execute the click. The full-loop runner
   retains T1--T13 and must close any delivery through Field Desk QA on 7070."
  [selection]
  (let [candidate-set (set (:candidate-domain selection))
        selected-set (set (:selected-mission-ids selection))
        cache-gate (:serving-cache-gate selection)
        maximum (:maximum-endpoint-ms cache-gate)
        accepted (:accepted-endpoint-latencies cache-gate)]
    (when-not (and (= :verified-live-selection (:status selection))
                   (seq candidate-set)
                   (seq selected-set)
                   (set/subset? selected-set candidate-set)
                   (contains? #{:warm :warmed-and-rechecked}
                              (:status cache-gate))
                   (number? maximum)
                   (<= maximum 1000)
                   (seq accepted)
                   (every? #(and (number? (:elapsed-ms %))
                                 (<= (:elapsed-ms %) maximum))
                           accepted))
      (throw
       (ex-info "bounded autonomy machine gates are incomplete"
                {:candidate-domain (:candidate-domain selection)
                 :selected-mission-ids (:selected-mission-ids selection)
                 :serving-cache-gate cache-gate})))
    (assoc selection :actuation
           {:status :machine-authorized-bounded-autonomy
            :authorized? true
            :executed? false
            :authority :machine-determined
            :operator-confirmation-required? false
            :operator-decision-evidence-id operator-decision-evidence-id
            :admissible-set :unchanged-phase1-4
            :allow-listed-mission-ids (:candidate-domain selection)
            :machine-gates
            {:armed-tripwire-count 13
             :query-bounds-retained? true
             :witness-and-admissibility-retained? true
             :serving-cache cache-gate}
            :delivery-qa
            {:required? true
             :endpoint delivery-qa-endpoint}
            :fallback
            {:controller :current-additive
             :mode :explicit-rollback-only
             :rollback-boundary rollback-boundary}})))

(defn current-selection
  "Run the reason-bearing selector against the current shared store.

   SCHEDULER-HABIT-RANKING must already be restricted to the Phase 1-4
   candidate ids. The operation is read-only but, after the immediate serving
   cache gate passes, returns machine enactment authority. It never executes
   the click."
  [{:keys [scheduler-habit-ranking evidence-store trace-id]}]
  (let [root (fixture-root)
        live-input
        (assoc (read-fixture root "live-wm-selection-input-20260724.edn")
               :phase5 (read-fixture root "phase5-outer-cascade.edn")
               :phase6 (read-fixture root "phase6-strategic-outcomes.edn")
               :phase7 (read-fixture root
                                     "phase7-strategic-policy-shadow.edn")
               :rung2 (read-fixture root "rung2-operator-update.edn")
               :scheduler-habit-ranking scheduler-habit-ranking)
        ctx {:evidence-store
             (or evidence-store (futon1b/make-futon1b-backend))
             :trace-id (or trace-id (:trace-id live-input))}
        max-endpoint-ms
        (get-in live-input
                [:latency :click-time-contract
                 :maximum-post-warm-endpoint-recall-ms]
                1000)]
    (authorize-bounded-autonomy
     (enforce-serving-cache-gate
      #(run-verification ctx live-input)
      max-endpoint-ms))))
