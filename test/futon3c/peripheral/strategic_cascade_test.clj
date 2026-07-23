(ns futon3c.peripheral.strategic-cascade-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-cascade :as strategic]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(defn- read-edn [path]
  (-> path io/file slurp edn/read-string))

(def phase4-corpus
  (delay (read-edn
          "holes/labs/M-typed-memories/phase4-wm-corpus.edn")))

(def phase5-fixture
  (delay (read-edn
          "holes/labs/M-typed-memories/phase5-outer-cascade.edn")))

(defn- query-step
  [episodes control-edges]
  (fn [pattern-id _remaining-budget]
    (wm-memory/dark-candidate-projection
     {:trace-id (str "phase5-" pattern-id)
      :recall-fn
      (fn [_ endpoint _]
        {:ok true
         :endpoint endpoint
         :memories
         (filterv #(some #{endpoint} (:memory/pattern-ids %))
                  episodes)})}
     [pattern-id] control-edges {:limit 10})))

(defn- golden-result
  ([] (golden-result {}))
  ([overrides]
   (let [{:keys [episodes control-edges]} @phase4-corpus
         {:keys [cascade dependencies transition-warrants]} @phase5-fixture]
     (strategic/outer-frontier
      (merge
       {:cascade cascade
        :dependencies dependencies
        :transition-warrants transition-warrants
        :query-step-fn (query-step episodes control-edges)
        :budget 4}
       overrides)))))

(deftest golden-outer-cascade-explains-frontier-blocker-and-holes
  (let [result (golden-result)
        by-id (into {} (map (juxt :mission-id identity))
                    (:frontier result))]
    (is (= ["M-aif-policy-conditioned-eig"
            "M-shared-memory-control-build-test"
            "M-wm-aif-policy-grain-compliance"]
           (mapv :mission-id (:frontier result))))
    (is (= :ready
           (get-in by-id ["M-aif-policy-conditioned-eig"
                          :dependency-status])))
    (is (= ["M-shared-memory-control-build-test"
            "M-aif-policy-conditioned-eig"]
           (get-in by-id ["M-aif-policy-conditioned-eig"
                          :mission-cascade])))
    (is (= :blocked
           (get-in by-id ["M-wm-aif-policy-grain-compliance"
                          :dependency-status])))
    (is (= "M-wm-tripwires"
           (get-in result [:excluded-missions 0 :mission-id])))
    (is (some #(= :missing-pattern-transition (:hole/type %))
              (:holes result)))
    (is (some #(= :blocked-mission-dependency (:hole/type %))
              (:holes result)))
    (is (= :unearned (get-in result [:mint-proposals 0 :prior])))
    (is (false? (get-in result
                        [:mint-proposals 0 :promotion-eligible?])))
    (is (nil? (:selected-mission result)))
    (is (false? (:live-ordering-changed? result)))))

(deftest stable-inputs-and-endpoint-order-have-set-semantics
  (let [a (golden-result)
        {:keys [cascade]} @phase5-fixture
        reversed (assoc cascade :shown (vec (reverse (:shown cascade))))
        b (golden-result {:cascade reversed})]
    (is (= a (golden-result)))
    (is (= (set (map :mission-id (:frontier a)))
           (set (map :mission-id (:frontier b)))))
    (is (= (set (map :mission-id (:excluded-missions a)))
           (set (map :mission-id (:excluded-missions b)))))))

(deftest warrant-retraction-and-irrelevant-memory-are-metamorphic
  (let [{:keys [episodes control-edges]} @phase4-corpus
        {:keys [cascade dependencies transition-warrants]} @phase5-fixture
        base (golden-result)
        retracted-edges
        (mapv #(if (= "e-wm-eig-support" (first (:memory-ids %)))
                 (assoc % :status :retracted)
                 %)
              control-edges)
        retracted
        (golden-result
         {:query-step-fn (query-step episodes retracted-edges)})
        irrelevant
        (assoc (first episodes)
               :memory/id "e-wm-irrelevant"
               :memory/pattern-ids ["p4ng/R20-interoception"]
               :memory/mission-ids ["M-irrelevant"])
        with-irrelevant
        (strategic/outer-frontier
         {:cascade cascade
          :dependencies dependencies
          :transition-warrants transition-warrants
          :query-step-fn
          (query-step (conj episodes irrelevant) control-edges)
          :budget 4})]
    (is (not (some #{"M-aif-policy-conditioned-eig"}
                   (map :mission-id (:frontier retracted)))))
    (is (= (:frontier base) (:frontier with-irrelevant)))
    (is (= (:excluded-missions base)
           (:excluded-missions with-irrelevant)))))

(deftest mint-lane-requires-repeated-independent-material
  (let [{:keys [cascade]} @phase5-fixture
        single-hole (assoc cascade :policy-holes
                           [(first (:policy-holes cascade))])
        result (golden-result {:cascade single-hole})]
    (is (empty? (:mint-proposals result)))
    (testing "similarity and author declaration are not promotion evidence"
      (let [declared
            (assoc cascade :policy-holes
                   [{:hole/type :unrepresented-control
                     :hole-signature "author-says-new-pattern"
                     :witness-ids ["same-author"]
                     :memory-ids ["same-memory"]
                     :similarity 0.99}
                    {:hole/type :unrepresented-control
                     :hole-signature "author-says-new-pattern"
                     :witness-ids ["same-author"]
                     :memory-ids ["same-memory"]
                     :author-declared? true}])]
        (is (empty?
             (:mint-proposals (golden-result {:cascade declared}))))))))

(deftest explicit-budget-stops-query-steps-and-surfaces-holes
  (let [calls (atom [])
        {:keys [episodes control-edges]} @phase4-corpus
        base-step (query-step episodes control-edges)
        result
        (golden-result
         {:budget 2
          :query-step-fn
          (fn [pattern-id remaining]
            (swap! calls conj [pattern-id remaining])
            (base-step pattern-id remaining))})]
    (is (= 2 (count @calls)))
    (is (= 2 (get-in result [:budget :spent])))
    (is (= 2 (count (get-in result [:budget :skipped-patterns]))))
    (is (= 2 (count (filter #(= :budget-exhausted (:hole/type %))
                            (:holes result)))))))

(deftest checkpoint-carries-useful-dark-instrumentation-only
  (let [{:keys [checkpoint]} @phase5-fixture
        result
        (strategic/checkpoint-ranking
         (golden-result)
         (merge
          checkpoint
          {:pattern-activation
           {"p4ng/R5-policy-evaluation" 0.6
            "p4ng/R6-candidate-pattern-action-space" 1.0
            "p4ng/R9-independent-witness" 0.25
            "p4ng/R10-liveness" 1.0}
           :relation-weights
           {:requires-control 1.0
            :repairs-control 1.0
            :instantiates-control 0.75
            :produces-evidence-for 0.5
            :blocked-by-control 0.0}}))
        checkpoint (:retrieval-checkpoint result)]
    (is (true? (:useful-inspectable-difference? checkpoint)))
    (is (true? (:instrumentation-carried? checkpoint)))
    (is (not= (:control-ranking checkpoint)
              (:typed-ranking checkpoint)))
    (is (seq (:relation-contributions checkpoint)))
    (is (pos? (get-in checkpoint
                      [:path-diversity :distinct-path-count])))
    (is (= (:control-ranking checkpoint)
           (:counterfactual-ranking checkpoint)))
    (is (= 0.0 (get-in checkpoint
                       [:held-out :control-hit-at-k])))
    (is (= 1.0 (get-in checkpoint
                       [:held-out :typed-hit-at-k])))
    (is (= (:budget result) (:budget checkpoint)))
    (is (nil? (:selected-mission checkpoint)))
    (is (false? (:live-ordering-changed? checkpoint)))))

(deftest rung3-budgeted-refinement-reuses-the-phase5-frontier
  (let [{:keys [episodes control-edges]} @phase4-corpus
        {:keys [cascade dependencies transition-warrants]} @phase5-fixture
        {:keys [budget information-models additional-transition-warrants]}
        (read-edn
         "holes/labs/M-typed-memories/rung3-facet-refinement.edn")
        all-warrants
        (into transition-warrants additional-transition-warrants)
        calls (atom [])
        base-step (query-step episodes control-edges)
        result
        (strategic/budgeted-facet-frontier
         {:cascade cascade
          :dependencies dependencies
          :transition-warrants all-warrants
          :information-models information-models
          :budget budget
          :query-step-fn
          (fn [pattern-id remaining-budget]
            (swap! calls conj [pattern-id remaining-budget])
            (base-step pattern-id remaining-budget))})
        refinement (:facet-refinement result)
        observed-challenges
        (set (mapcat :observed-challenge-memory-ids
                     (:observations refinement)))]
    (is (= [["p4ng/R9-independent-witness" 3]
            ["p4ng/R6-candidate-pattern-action-space" 2]
            ["p4ng/R10-liveness" 1]]
           @calls))
    (is (= ["p4ng/R9-independent-witness"
            "p4ng/R6-candidate-pattern-action-space"
            "p4ng/R10-liveness"]
           (:selected-patterns refinement)))
    (is (= #{"e-wm-compliance-challenge"
             "e-wm-memory-challenge"
             "e-wm-tripwire-challenge"}
           observed-challenges))
    (is (= 3
           (get-in refinement
                   [:path-diversity :distinct-challenge-memory-count])))
    (is (= 7
           (get-in refinement
                   [:path-diversity :distinct-evidence-path-count])))
    (is (= "M-wm-tripwires"
           (get-in result [:excluded-missions 0 :mission-id])))
    (is (not (some #{"M-aif-policy-conditioned-eig"}
                   (map :mission-id (:frontier result)))))
    (is (some #(and (= :budget-exhausted (:hole/type %))
                    (= "p4ng/R5-policy-evaluation"
                       (:control-pattern-id %)))
              (:holes result)))
    (is (= :outcome-model-not-memory-multiplicity
           (:evidence-counting refinement)))
    (is (nil? (:selected-mission result)))
    (is (false? (:live-ordering-changed? result)))))
