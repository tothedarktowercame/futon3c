(ns futon3c.peripheral.dynamic-queries-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.dynamic-queries :as dynamic-queries]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(def r5 "p4ng/R5-policy-evaluation")
(def r6 "p4ng/R6-candidate-pattern-action-space")
(def r9 "p4ng/R9-independent-witness")
(def r10 "p4ng/R10-liveness")

(def relation-weights
  {:requires-control 1.0
   :repairs-control 1.0
   :instantiates-control 0.75
   :produces-evidence-for 0.5
   :blocked-by-control 0.0})

(defn phase4-projection
  []
  (let [{:keys [episodes control-edges]}
        (-> "holes/labs/M-typed-memories/phase4-wm-corpus.edn"
            io/file slurp edn/read-string)
        patterns (->> episodes (mapcat :memory/pattern-ids) distinct vec)
        recall-fn
        (fn [_ endpoint _]
          {:ok true
           :endpoint endpoint
           :memories
           (filterv #(some #{endpoint} (:memory/pattern-ids %)) episodes)})]
    (get
     (wm-memory/dark-candidate-projection
      {:recall-fn recall-fn :trace-id "dynamic-queries-rung1-test"}
      patterns control-edges {:limit 10})
     :projection)))

(deftest fixed-typed-step-preserves-the-phase4-admissible-set
  (let [projection (phase4-projection)
        result
        (dynamic-queries/fixed-typed-ranking
         projection
         {:pattern-activation {r5 0.6
                               r6 1.0
                               r9 0.25
                               r10 1.0}
          :relation-weights relation-weights})
        ranked-by-id
        (into {} (map (juxt :mission-id identity))
              (:ranked-candidates result))]
    (is (= ["M-aif-policy-conditioned-eig"
            "M-shared-memory-control-build-test"
            "M-wm-aif-policy-grain-compliance"]
           (:control-ranking result)))
    (is (= ["M-shared-memory-control-build-test"
            "M-aif-policy-conditioned-eig"
            "M-wm-aif-policy-grain-compliance"]
           (:typed-ranking result)))
    (is (true? (:candidate-set-preserved? result)))
    (is (false? (:live-ordering-changed? result)))
    (is (not (some #{"M-wm-tripwires"} (:typed-ranking result))))
    (is (= 1.0
           (get-in ranked-by-id
                   ["M-shared-memory-control-build-test" :score])))
    (is (= [{:control-pattern-id r6
             :relation :repairs-control
             :memory-ids ["e-wm-memory-support"]
             :pattern-activation 1.0
             :relation-weight 1.0
             :contribution 1.0}]
           (get-in ranked-by-id
                   ["M-shared-memory-control-build-test"
                    :contributions])))))

(deftest duplicate-memory-handles-do-not-multiply-a-typed-path
  (let [projection
        {:candidates
         [{:mission-id "M-one"
           :support-relations
           [{:control-pattern-id r6
             :relation :repairs-control
             :memory-ids ["e-1" "e-2"]}
            {:control-pattern-id r6
             :relation :repairs-control
             :memory-ids ["e-2" "e-3"]}]}]}
        result
        (dynamic-queries/fixed-typed-ranking
         projection
         {:pattern-activation {r6 0.5}
          :relation-weights {:repairs-control 2.0}})
        ranked (first (:ranked-candidates result))]
    (is (= 1.0 (:score ranked)))
    (is (= ["e-1" "e-2" "e-3"]
           (get-in ranked [:contributions 0 :memory-ids])))
    (is (= 1 (count (:contributions ranked))))))

(deftest activation-cannot-address-an-excluded-candidate
  (let [projection
        {:candidates
         [{:mission-id "M-admitted"
           :support-relations
           [{:control-pattern-id r9
             :relation :requires-control
             :memory-ids ["e-witnessed"]}]}]}]
    (testing "candidate activation is confined to the admitted set"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"invalid candidate activation"
           (dynamic-queries/fixed-typed-ranking
            projection
            {:candidate-activation {"M-excluded" 100.0}}))))
    (testing "negative and unknown relation weights are rejected"
      (is (thrown? clojure.lang.ExceptionInfo
                   (dynamic-queries/fixed-typed-ranking
                    projection
                    {:relation-weights {:requires-control -1.0}})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (dynamic-queries/fixed-typed-ranking
                    projection
                    {:relation-weights {:invented-relation 1.0}}))))))

(deftest ties-retain-the-control-order
  (let [projection
        {:candidates
         [{:mission-id "M-z"
           :support-relations
           [{:control-pattern-id r5
             :relation :repairs-control
             :memory-ids ["e-z"]}]}
          {:mission-id "M-a"
           :support-relations
           [{:control-pattern-id r5
             :relation :repairs-control
             :memory-ids ["e-a"]}]}]}
        result (dynamic-queries/fixed-typed-ranking projection)]
    (is (= ["M-z" "M-a"] (:control-ranking result)))
    (is (= (:control-ranking result) (:typed-ranking result)))))
