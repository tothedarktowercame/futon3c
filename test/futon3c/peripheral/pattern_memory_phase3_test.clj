(ns futon3c.peripheral.pattern-memory-phase3-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-recall :as recall]
            [futon3c.peripheral.memory-trials :as trials]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.tools :as tools])
  (:import [java.time Instant]))

(def ctx
  {:agent-id "zai-phase3"
   :session-id "phase3-session"
   :domain :mathematics})

(def original-entry
  {:evidence/id "e-original"
   :evidence/subject {:ref/type :problem :ref/id "topology/old"}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author "zai-earlier"
   :evidence/session-id "earlier-session"
   :evidence/at "2026-07-20T00:00:00Z"
   :evidence/body {:hook "Use the old topology."}
   :evidence/tags [:memory :memory/assert]})

(def original-edge
  {:hx/id "hx-original"
   :hx/type :memory/assert
   :hx/endpoints ["e-original" "topology/old"
                  "math-formalization/tactic-algebra-interference"]
   :hx/props
   {:domain :mathematics
    :state :current
    :attachment-status :reviewed
    :witness-status :self-asserted
    :roles {:entry "e-original"
            :subjects ["topology/old"
                       "math-formalization/tactic-algebra-interference"]
            :patterns
            ["math-formalization/tactic-algebra-interference"]}}})

(defn- graph-fixture
  [initial]
  (let [current (atom (into {} (map (juxt :hx/id identity)) initial))
        history (atom [])]
    {:current current
     :history history
     :fetch
     (fn [endpoint {:keys [type limit]}]
       (->> (vals @current)
            (filter #(and (= type (:hx/type %))
                          (some #{endpoint} (:hx/endpoints %))))
            (take limit)
            vec))
     :post
     (fn [_ edge]
       (swap! history conj edge)
       (if (= "retract" (:hx/op edge))
         (swap! current dissoc (:hx/id edge))
         (swap! current assoc (:hx/id edge) edge))
       {:ok true :hyperedge edge})}))

(deftest failed-use-is-challenged-without-erasure-or-success-training
  (let [store (atom {:entries {"e-original" original-entry}
                     :order ["e-original"]})
        graph (graph-fixture [original-edge])
        outcome
        {:evidence/id "e-failed-check"
         :evidence/subject {:ref/type :task :ref/id "trial/fail"}
         :evidence/type :pattern-outcome
         :evidence/claim-type :observation
         :evidence/author "lean/external-checker"
         :evidence/session-id "checker"
         :evidence/at "2026-07-23T09:00:00Z"
         :evidence/body
         {:memory-outcome/witness-status :independently-witnessed
          :exit 1}
         :evidence/tags [:external-check]}
        _ (swap! store
                 (fn [s] (-> s
                             (assoc-in [:entries "e-failed-check"] outcome)
                             (update :order conj "e-failed-check"))))
        result
        (lifecycle/challenge-memory!
         (assoc ctx :evidence-store store)
         {:memory-id "e-original"
          :reason "The recalled tactic failed the external checker."
          :outcome-id "e-failed-check"}
         {:fetch-hyperedges (:fetch graph)
          :post-hyperedge (:post graph)})
        challenge (estore/get-entry* store (:challenge-id result))]
    (is (true? (:ok result)))
    (is (= :challenged
           (get-in @(:current graph)
                   ["hx-original" :hx/props :state])))
    (is (= :challenge (:evidence/claim-type challenge)))
    (is (= "e-failed-check"
           (get-in challenge [:evidence/body :memory/outcome-id])))
    (is (= original-entry (estore/get-entry* store "e-original")))
    (is (not= :success
              (get-in challenge [:evidence/body :memory/outcome])))))

(deftest volatile-topology-is-superseded-and-correction-is-preserved
  (let [store (atom {:entries {"e-original" original-entry}
                     :order ["e-original"]})
        graph (graph-fixture [original-edge])
        correcting-edge
        (-> original-edge
            (assoc :hx/id "hx-correcting")
            (assoc :hx/endpoints
                   ["e-correcting" "topology/new"
                    "math-formalization/tactic-algebra-interference"])
            (assoc-in [:hx/props :roles :entry] "e-correcting")
            (assoc-in [:hx/props :roles :subjects]
                      ["topology/new"
                       "math-formalization/tactic-algebra-interference"])
            (assoc-in [:hx/props :attachment-status] :proposed))
        correcting-entry
        (assoc original-entry
               :evidence/id "e-correcting"
               :evidence/body {:hook "Use the corrected topology."})
        record-memory
        (fn [_ _]
          (swap! store
                 (fn [s] (-> s
                             (assoc-in [:entries "e-correcting"]
                                       correcting-entry)
                             (update :order conj "e-correcting"))))
          ((:post graph) nil correcting-edge)
          {:ok true :id "e-correcting" :hx-id "hx-correcting"})
        valid-from (.minusSeconds (Instant/now) 60)
        result
        (lifecycle/supersede-memory!
         (assoc ctx :evidence-store store)
         {:memory-id "e-original"
          :reason "The service moved to the new topology."
          :valid-from valid-from
          :correction
          {:name "new-topology"
           :hook "Use the corrected topology."
           :kind :feedback
           :body "The service moved."}}
         {:fetch-hyperedges (:fetch graph)
          :post-hyperedge (:post graph)
          :record-memory record-memory})]
    (is (true? (:ok result)) result)
    (is (= :superseded
           (get-in @(:current graph)
                   ["hx-original" :hx/props :state])))
    (is (= "e-correcting"
           (get-in @(:current graph)
                   ["hx-original" :hx/props :superseded-by])))
    (is (= :reviewed
           (get-in @(:current graph)
                   ["hx-correcting" :hx/props :attachment-status])))
    (is (some? (estore/get-entry* store "e-original")))
    (is (some? (estore/get-entry* store "e-correcting")))
    (is (pos? (:correction-lag-ms result)))))

(deftest retracted-attachment-disappears-on-next-projection
  (let [store (atom {:entries {"e-original" original-entry}
                     :order ["e-original"]})
        graph (graph-fixture [original-edge])
        result
        (lifecycle/retract-memory!
         (assoc ctx :evidence-store store)
         {:memory-id "e-original" :reason "Attachment was mis-curated."}
         {:fetch-hyperedges (:fetch graph)
          :post-hyperedge (:post graph)})
        projection
        (recall/recall-by-endpoint
         {:domain :mathematics :evidence-store store}
         "math-formalization/tactic-algebra-interference"
         {:fetch-hyperedges (:fetch graph)
          :fetch-entry #(estore/get-entry* store %)})]
    (is (true? (:ok result)))
    (is (empty? (:memories projection)))
    (is (some? (estore/get-entry* store "e-original")))
    (is (= "retract" (:hx/op (last @(:history graph)))))))

(deftest lexical-memory-proposal-is-reviewed-and-bounded
  (let [calls (atom [])
        result
        (recall/propose-patterns-by-query
         {:domain :mathematics}
         "field_simp polynomial denominator normalization"
         {:limit 2
          :search-evidence
          (fn [_ opts]
            (swap! calls conj [:search opts])
            {:results
             [{:score -2.0
               :entry (assoc original-entry :evidence/id "e-original")}
              {:score -1.0
               :entry (assoc original-entry :evidence/id "e-unreviewed")}
              {:score -0.5
               :entry (assoc original-entry :evidence/id "e-over-limit")}]})
          :recall-batch-fn
          (fn [_ endpoints opts]
            (swap! calls conj [:batch endpoints opts])
            {:ok true
             :elapsed-ms 1.0
             :recalls
             (mapv
              (fn [memory-id]
                {:ok true
                 :endpoint memory-id
                 :memories
                 (if (= "e-original" memory-id)
                   [{:memory/id memory-id
                     :memory/hook "Normalize before field_simp."
                     :memory/pattern-ids
                     ["math-formalization/tactic-algebra-interference"]}]
                   [])})
              endpoints)})})]
    (is (= ["math-formalization/tactic-algebra-interference"]
           (mapv :pattern-id (:candidates result))))
    (is (= 2 (:checked-memory-count result)))
    (is (= 1 (count (filter #(= :batch (first %)) @calls))))
    (is (= ["e-original" "e-unreviewed"]
           (second (first (filter #(= :batch (first %)) @calls)))))))

(deftest lexical-memory-proposal-falls-back-to-bounded-token-disjunction
  (let [queries (atom [])
        memory-row
        {:score -1.0
         :entry (assoc original-entry :evidence/id "e-token-hit")}
        result
        (recall/propose-patterns-by-query
         {:domain :mathematics}
         "normalize a denominator before using field_simp"
         {:limit 5
          :search-evidence
          (fn [query _]
            (swap! queries conj query)
            {:results
             (if (= query "normalize OR denominator OR field_simp")
               [memory-row]
               [])})
          :recall-batch-fn
          (fn [_ endpoints _]
            {:ok true
             :elapsed-ms 1.0
             :recalls
             (mapv
              (fn [memory-id]
                {:ok true
                 :endpoint memory-id
                 :memories
                 [{:memory/id memory-id
                   :memory/hook "Normalize before field_simp."
                   :memory/pattern-ids
                   ["math-formalization/tactic-algebra-interference"]}]})
              endpoints)})})]
    (is (= :bounded-token-disjunction (:query-strategy result)))
    (is (= ["normalize" "denominator" "field_simp"]
           (:fallback-tokens result)))
    (is (= ["math-formalization/tactic-algebra-interference"]
           (mapv :pattern-id (:candidates result))))
    (is (= ["normalize a denominator before using field_simp"
            "normalize OR denominator OR field_simp"]
           @queries))))

(deftest phase2-residual-enters-through-memory-candidate-construction
  (let [proposal
        (fn [_ query _]
          {:ok true :query query
           :candidates
           [{:pattern-id
             "math-formalization/tactic-algebra-interference"
             :source :reviewed-memory-lexical-proposal
             :memory-support [{:memory-id "e-original"}]}]})
        backend
        (rb/make-real-backend
         {:cwd "/home/joe/code/futon3c"
          :notions-index-path
          "../futon3a/resources/notions/patterns-index.tsv"
          :memory-domain :mathematics
          :memory-proposal-fn proposal
          :memory-recall-batch-fn
          (fn [_ endpoints _]
            {:ok true
             :trace-id "test"
             :elapsed-ms 1.0
             :recalls
             (mapv (fn [endpoint]
                     {:ok true :endpoint endpoint
                      :memories [] :audit {}})
                   endpoints)})})
        result
        (tools/execute-tool
         backend :psr-search
         ["field_simp polynomial denominator normalization"
          {:top-k 5}])]
    (is (true? (:ok result)) result)
    (is (= "math-formalization/tactic-algebra-interference"
           (get-in result [:result :candidates 0 :pattern-id])))
    (is (= "e-original"
           (get-in result
                   [:result :candidates 0
                    :memory-proposal-support 0 :memory-id])))
    (is (string? (get-in result [:result :trace-id])))
    (is (number? (get-in result [:result :timing :total-ms])))))

(deftest repeated-trials-are-bounded-and-auditable
  (let [cases
        (edn/read-string
         (slurp (io/resource
                 "fixtures/pattern_memory_phase3_trials.edn")))
        result
        (trials/run-bounded!
         cases
         {:concurrency 2
          :run-trial
          (fn [case]
            (merge
             (trials/retrieval-case-row
              case [(:trial/expected-pattern case)])
             {:trial/use {:memory-ids ["e-original"]}
              :trial/external-outcome
              {:id (str "outcome/" (:trial/id case))
               :witness "lean"}
              :trial/corrections []}))})]
    (is (= :audit-only-no-success-rate (:claim result)))
    (is (= (count cases) (count (:rows result))))
    (is (every? #(true? (get-in % [:trial/recall :retrieved?]))
                (:rows result))))
  (testing "oversized and over-wide requests are refused, not truncated"
    (is (thrown? clojure.lang.ExceptionInfo
                 (trials/run-bounded!
                  (repeat 21 {:trial/id "x" :trial/query "q"})
                  {:run-trial identity})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (trials/run-bounded!
                  []
                  {:concurrency 5 :run-trial identity})))))
