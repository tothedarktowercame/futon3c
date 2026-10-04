(ns futon3c.diagramprover.skeleton.agency-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.regime :as regime]
            [futon3c.diagramprover.skeleton.agency :as agency]))

;; A lawful window: claude-1 asks codex-1 a question through the turn queue,
;; codex-1 hops into explore and back, publishes evidence that both read,
;; and answers.
(def lawful
  [{:op :register :agent "claude-1"}
   {:op :register :agent "codex-1"}
   {:op :ring :id "j1" :from "claude-1" :to "codex-1" :type :query}
   {:op :accept :bell "j1"}
   {:op :hop :agent "codex-1" :peripheral :explore}
   {:op :hop-back :agent "codex-1" :peripheral :explore}
   {:op :drain :agent "codex-1" :bell "j1"}
   {:op :publish :agent "codex-1" :evidence "ev1"}
   {:op :read :agent "claude-1" :evidence "ev1"}
   {:op :read :agent "codex-1" :evidence "ev1"}
   {:op :answer :agent "codex-1" :ref "j1"}])

(deftest lawful-window-is-clean
  (let [report (agency/check lawful)]
    (is (agency/lawful? report) (pr-str report))
    (is (= [] (:open-bells report)))
    (is (= [] (:crossings report)))))

(deftest evidence-is-cartesian-sessions-are-not
  (testing "two readers of one piece of evidence is lawful under the mixed regime"
    (is (= [] (:linearity (agency/check lawful)))))
  (testing "the same diagram under an all-linear regime flags the shared evidence"
    (let [g (:diagram (agency/ingest lawful))
          findings (regime/linearity-findings g {} agency/sort-of)]
      (is (= [{:finding :duplicated :sort :evidence :vtype :evidence
               :producers 1 :consumers 2 :ref "ev1"}]
             findings)))))

;; ---------------------------------------------------------------------------
;; Planted violations: each law must catch its own defect.

(deftest i1-spawned-clone-is-caught
  (let [report (agency/check (conj lawful {:op :spawn :agent "codex-1"}))]
    (is (= [:spawn] (map :value (:identity report))))
    (is (= ["codex-1"] (map :owner (:concurrency report))))
    (is (not (agency/lawful? report)))))

(deftest i1-second-concurrent-session-is-caught
  (let [report (agency/check (conj lawful {:op :register :agent "codex-1"}))]
    (is (= ["codex-1"] (map :owner (:concurrency report))))))

(deftest i1-restart-after-deregister-is-lawful
  (let [report (agency/check (into lawful [{:op :deregister :agent "codex-1"}
                                           {:op :register :agent "codex-1"}
                                           {:op :hop :agent "codex-1" :peripheral :edit}]))]
    (is (agency/lawful? report) (pr-str report))))

(deftest i2-transport-conjuring-an-agent-is-caught
  (let [report (agency/check (conj lawful {:op :transport/create :agent "ghost-1"}))]
    (is (= [:transport/create] (map :value (:identity report))))))

(deftest double-answer-duplicates-a-linear-obligation
  (let [report (agency/check (conj lawful {:op :answer :agent "codex-1" :ref "j1"}))]
    (is (= [:duplicated] (map :finding (:linearity report))))
    (is (= ["j1"] (map :ref (:linearity report))))))

(deftest answering-a-bell-never-received-is-ill-typed
  (let [events [{:op :ring :id "j2" :from "claude-1" :to "codex-1" :type :query}
                {:op :answer :agent "codex-1" :ref "j2"}]
        report (agency/check events)]
    (is (= [{:finding :ill-typed :value :answer
             :expected {:in [:agent :turn/query] :out [:agent]}
             :actual {:in [:agent :bell/query] :out [:agent]}}]
           (map #(dissoc % :event) (:signature report))))))

(deftest answering-a-request-as-a-query-is-ill-typed
  (let [events [{:op :ring :id "j3" :from "claude-1" :to "codex-1" :type :request}
                {:op :deliver :agent "codex-1" :bell "j3"}
                {:op :answer :agent "codex-1" :ref "j3"}]]
    (is (= [:answer] (map :value (:signature (agency/check events)))))))

(deftest misrouted-delivery-is-caught
  (let [events [{:op :ring :id "j4" :from "claude-1" :to "codex-1" :type :request}
                {:op :deliver :agent "codex-2" :bell "j4"}
                {:op :reply :agent "codex-2" :ref "j4"}]
        routing (:routing (agency/check events))]
    (is (= [[:deliver :request] :reply] (map :value routing)))
    (is (every? #(= "codex-1" (:addressed-to %)) routing))))

(deftest open-bells-and-crossings-are-the-output-boundary
  (let [events [{:op :ring :id "a" :from "claude-1" :to "codex-1" :type :request}
                {:op :ring :id "b" :from "codex-1" :to "claude-1" :type :request}
                {:op :ring :id "c" :from "claude-1" :to "claude-2" :type :query}]
        report (agency/check events)]
    (is (agency/lawful? report))
    (is (= #{"a" "b" "c"} (set (map :ref (:open-bells report)))))
    (is (= [["claude-1" "codex-1"]] (:crossings report)))))

(deftest windows-assume-what-they-did-not-see
  (testing "a session already running and a bell rung before the window enter as inputs"
    (let [g (:diagram (agency/ingest [{:op :reply :agent "codex-1" :ref "old"}]))
          ports (regime/open-ports g agency/sort-of)]
      (is (= #{[:agent "codex-1"] :turn/request} (set (map :vtype (:inputs ports)))))
      (is (= [[:agent "codex-1"]] (map :vtype (:outputs ports)))))))

;; ---------------------------------------------------------------------------
;; Equivalence and refinement

(deftest interleavings-of-independent-steps-are-the-same-run
  (let [a [{:op :ring :id "x" :from "claude-1" :to "codex-1" :type :request}
           {:op :ring :id "y" :from "claude-2" :to "codex-2" :type :request}
           {:op :deliver :agent "codex-1" :bell "x"}
           {:op :deliver :agent "codex-2" :bell "y"}]
        b [(a 1) (a 3) (a 0) (a 2)]]
    (is (agency/same-run? a b))
    (testing "but not when a dependency changes: each bell taken by the other agent"
      (is (not (agency/same-run? a [(a 0) (a 1) (assoc (a 2) :bell "y")
                                    (assoc (a 3) :bell "x")]))))))

(deftest queue-level-trace-refines-the-protocol
  (let [spec (mapv #(if (= :drain (:op %)) (assoc % :op :deliver) %)
                   (remove #(= :accept (:op %)) lawful))
        result (agency/refines? lawful spec)]
    (is (= {:refines? true :rewrite-steps 1} result))
    (testing "a bell accepted but never drained does not refine a delivered one"
      (is (false? (:refines? (agency/refines? (remove #(= :drain (:op %)) lawful) spec)))))))

(deftest delivery-rule-is-boundary-compatible
  (let [r (agency/delivery-rule "codex-1" :query)]
    (is (= (graph/domain (:lhs r)) (graph/domain (:rhs r))))
    (is (= 2 (graph/num-edges (:lhs r))))
    (is (= 1 (graph/num-edges (:rhs r))))))

;; ---------------------------------------------------------------------------
;; Causal tier

(deftest typed-bells-receipt-derives-the-recording-requirement
  (let [{:keys [verdicts]} (agency/typed-bells-receipt)
        by (into {} (map (juxt :regime identity)) verdicts)]
    (is (= :backdoor (get-in by [:load-recorded :method])))
    (is (some #{#{:load}} (map set (get-in by [:load-recorded :adjustment-sets]))))
    (is (= :front-door (get-in by [:load-unrecorded :method])))
    (is (= #{:query-threads} (get-in by [:load-unrecorded :mediators])))
    (is (= :refusal (get-in by [:load-unrecorded+direct-effect :method])))))
