(require '[clojure.edn :as edn])
(require '[clojure.pprint :refer [pprint]])
(require '[futon3c.peripheral.dynamic-queries-rung4 :as rung4])
(require '[futon3c.peripheral.wm-memory :as wm-memory])

(defn read-edn [path]
  (-> path slurp edn/read-string))

(defn phase4-projection
  [{:keys [episodes control-edges]}]
  (let [patterns (->> episodes (mapcat :memory/pattern-ids) distinct vec)
        recall-fn
        (fn [_ endpoint _]
          {:ok true
           :endpoint endpoint
           :memories
           (filterv #(some #{endpoint} (:memory/pattern-ids %)) episodes)})]
    (get
     (wm-memory/dark-candidate-projection
      {:recall-fn recall-fn :trace-id "dynamic-queries-rung4-demo"}
      patterns control-edges {:limit 10})
     :projection)))

(defn run-battery
  [case floor]
  (rung4/coupled-propagation
   (-> case
       (select-keys
        [:projection :candidate-activation :pattern-activation
         :relation-weights :challenge-memories :query :k])
       (assoc :exploration-floor floor))))

(let [phase4-corpus
      (read-edn "holes/labs/M-typed-memories/phase4-wm-corpus.edn")
      phase4
      (rung4/coupled-propagation
       {:projection (phase4-projection phase4-corpus)
        :pattern-activation
        {"p4ng/R5-policy-evaluation" 0.6
         "p4ng/R6-candidate-pattern-action-space" 1.0
         "p4ng/R9-independent-witness" 0.25
         "p4ng/R10-liveness" 1.0}
        :relation-weights
        {:requires-control 1.0
         :repairs-control 1.0
         :instantiates-control 0.75
         :produces-evidence-for 0.5
         :blocked-by-control 0.0}
        :challenge-memories []
        :query "Frozen Phase 4 War Machine replay"
        :k 3
        :exploration-floor 0.1})
      case (-> (read-edn
                "holes/labs/M-typed-memories/rung4-collapse-battery.edn")
               :cases first)
      floor-off (run-battery case (:floor-off case))
      floor-on (run-battery case (:floor-on case))
      recovery-step
      (some (fn [{:keys [step typed-ranking]}]
              (when (= (:planted-target case) (first typed-ranking))
                step))
            (:per-step-trace floor-on))
      summary
      {:phase4
       {:typed-ranking (:typed-ranking phase4)
        :termination (:termination phase4)
        :steps-executed (:steps-executed phase4)
        :candidate-set-preserved? (:candidate-set-preserved? phase4)
        :selected-mission (:selected-mission phase4)
        :live-ordering-changed? (:live-ordering-changed? phase4)}
       :confirmation-collapse
       {:floor-off
        {:top-mission (first (:typed-ranking floor-off))
         :final-theta (get-in floor-off
                              [:per-step-trace
                               (dec (:steps-executed floor-off))
                               :theta-next])
         :challenge-reachable-every-step?
         (every? :challenge-reachable? (:per-step-trace floor-off))}
        :floor-on
        {:top-mission (first (:typed-ranking floor-on))
         :recovery-step recovery-step
         :final-theta (get-in floor-on
                              [:per-step-trace
                               (dec (:steps-executed floor-on))
                               :theta-next])
         :challenge-reachable-every-step?
         (every? :challenge-reachable? (:per-step-trace floor-on))}}
       :control-rankings (:control-rankings floor-on)
       :theta-semantics (:theta-semantics floor-on)}
      frozen
      (read-edn "holes/labs/M-typed-memories/rung4-results.edn")]
  (when-not (= (:expected-replay frozen) summary)
    (throw (ex-info "Rung 4 replay diverged from frozen result"
                    {:expected (:expected-replay frozen)
                     :actual summary})))
  (pprint summary))
