(ns futon3c.peripheral.memory-recall-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.memory-recall :as memory-recall]
            [futon3c.substrate.client :as substrate]))

(defn- fixtures []
  (-> "fixtures/shared_memory_contract_fixtures.edn"
      io/resource
      slurp
      edn/read-string))

(defn- recall-fixture
  [domain endpoint edge entry extra-edges]
  (memory-recall/recall-by-endpoint
   {:domain domain :evidence-store ::unused}
   endpoint
   {:limit 3
    :fetch-hyperedges
    (fn [requested {:keys [type limit]}]
      (is (= endpoint requested))
      (is (= :memory/assert type))
      (is (= 9 limit))
      (into [edge] extra-edges))
    :fetch-entry
    (fn [memory-id]
      (when (= memory-id (:evidence/id entry)) entry))}))

(deftest same-client-function-recalls-mathematics-and-wm-memories
  (let [{math :mathematics wm :war-machine} (fixtures)
        math-edge (assoc-in (:edge math) [:hx/props :domain] :mathematics)
        wm-edge (assoc-in (:edge wm) [:hx/props :domain] :war-machine)
        math-result (recall-fixture :mathematics
                                    "lean/field-simp-denominator"
                                    math-edge (:entry math) [])
        wm-result (recall-fixture :war-machine
                                  "p4ng/R15"
                                  wm-edge (:entry wm) [])]
    (is (= ["e-math-1"] (mapv :memory/id (:memories math-result))))
    (is (= ["e-wm-1"] (mapv :memory/id (:memories wm-result))))
    (is (true? (:ok math-result)))
    (is (number? (:elapsed-ms math-result)))
    (is (= 1 (get-in math-result [:audit :returned-count])))
    (is (= 1 (get-in wm-result [:audit :returned-count])))))

(deftest cross-domain-memory-is-explicitly-excluded
  (let [{math :mathematics wm :war-machine} (fixtures)
        math-edge (assoc-in (:edge math) [:hx/props :domain] :mathematics)
        wm-edge (-> (:edge wm)
                    (assoc :hx/endpoints
                           (conj (:hx/endpoints (:edge wm))
                                 "lean/field-simp-denominator"))
                    (assoc-in [:hx/props :domain] :war-machine))
        result (recall-fixture :mathematics
                               "lean/field-simp-denominator"
                               math-edge (:entry math) [wm-edge])]
    (is (= ["e-math-1"] (mapv :memory/id (:memories result))))
    (is (= 1 (get-in result [:audit :domain-excluded])))
    (is (= 2 (get-in result [:audit :edge-count])))))

(deftest malformed-and-missing-bodies-fail-closed
  (let [{math :mathematics} (fixtures)
        malformed (-> (:edge math)
                      (assoc-in [:hx/props :domain] :mathematics)
                      (assoc-in [:hx/props :roles :entry] "e-missing"))
        result (memory-recall/recall-by-endpoint
                {:domain :mathematics :evidence-store ::unused}
                "lean/field-simp-denominator"
                {:fetch-hyperedges (fn [_ _] [malformed])
                 :fetch-entry (constantly nil)})]
    (is (empty? (:memories result)))
    (is (= 1 (get-in result [:audit :missing-entry])))))

(deftest retracted-memory-is-not-current-recall
  (let [{math :mathematics} (fixtures)
        edge (-> (:edge math)
                 (assoc-in [:hx/props :domain] :mathematics)
                 (assoc-in [:hx/props :state] :retracted))
        result (recall-fixture :mathematics
                               "lean/field-simp-denominator"
                               edge (:entry math) [])]
    (is (empty? (:memories result)))
    (is (= 1 (get-in result [:audit :state-excluded])))))

(deftest superseded-edge-does-not-consume-current-result-slot
  (let [{math :mathematics} (fixtures)
        superseded
        (-> (:edge math)
            (assoc :hx/id "hx-a-superseded")
            (assoc-in [:hx/props :domain] :mathematics)
            (assoc-in [:hx/props :state] :superseded))
        current
        (-> (:edge math)
            (assoc :hx/id "hx-z-current")
            (assoc-in [:hx/props :domain] :mathematics))
        result
        (memory-recall/recall-by-endpoint
         {:domain :mathematics :evidence-store ::unused}
         "lean/field-simp-denominator"
         {:limit 1
          :fetch-hyperedges
          (fn [_ {:keys [limit]}]
            (is (= 3 limit))
            [superseded current])
          :fetch-entry (constantly (:entry math))})]
    (is (= ["e-math-1"] (mapv :memory/id (:memories result))))
    (is (= 1 (get-in result [:audit :state-excluded])))))

(deftest proposed-attachment-is-not-a-recall-warrant
  (let [{math :mathematics} (fixtures)
        edge (-> (:edge math)
                 (assoc-in [:hx/props :domain] :mathematics)
                 (assoc-in [:hx/props :attachment-status] :proposed))
        result (recall-fixture :mathematics
                               "lean/field-simp-denominator"
                               edge (:entry math) [])]
    (is (empty? (:memories result)))
    (is (= 1 (get-in result [:audit :proposed-excluded])))))

(deftest full-body-recall-is-explicit-and-fails-closed-on-partial-entry
  (let [{math :mathematics} (fixtures)
        edge (assoc-in (:edge math) [:hx/props :domain] :mathematics)
        full (memory-recall/recall-by-endpoint
              {:domain :mathematics :evidence-store ::unused}
              "lean/field-simp-denominator"
              {:include-bodies? true
               :fetch-hyperedges (fn [_ _] [edge])
               :fetch-entry (constantly (:entry math))})
        partial (memory-recall/recall-by-endpoint
                 {:domain :mathematics :evidence-store ::unused}
                 "lean/field-simp-denominator"
                 {:include-bodies? true
                  :fetch-hyperedges (fn [_ _] [edge])
                  :fetch-entry
                  (constantly (assoc (:entry math) :evidence/body nil))})]
    (is (= {:hook "Normalize the denominator before field_simp."}
           (get-in full [:memories 0 :memory/body])))
    (is (empty? (:memories partial)))
    (is (= 1 (get-in partial [:audit :missing-body])))))

(deftest current-full-body-recall-selects-through-projection
  (let [{math :mathematics} (fixtures)
        edge (assoc-in (:edge math) [:hx/props :domain] :mathematics)
        calls (atom [])
        result
        (memory-recall/recall-by-endpoint
         {:domain :mathematics :evidence-store ::unused}
         "lean/field-simp-denominator"
         {:limit 3
          :include-bodies? true
          :trace-id "selection-trace"
          :fetch-components
          (fn [endpoints opts]
            (swap! calls conj [endpoints opts])
            {:ok true
             :groups
             [{:endpoint "lean/field-simp-denominator"
               :components
               [{:edge edge
                 :entry (select-keys (:entry math)
                                     [:evidence/id :evidence/type
                                      :evidence/claim-type :evidence/author
                                      :evidence/session-id])}]}]
             :audit {:distinct-edge-count 1}
             :timing {:service-total-ms 1.0}})
          :fetch-entry
          (fn [memory-id]
            (when (= "e-math-1" memory-id) (:entry math)))})]
    (is (= [[["lean/field-simp-denominator"]
             {:limit 9 :trace-id "selection-trace"}]]
           @calls))
    (is (= {:hook "Normalize the denominator before field_simp."}
           (get-in result [:memories 0 :memory/body])))
    (is (= 1
           (get-in result [:substrate :audit :distinct-edge-count])))))

(deftest query-limit-is-capped-and-store-failure-is-data
  (let [seen-limit (atom nil)
        result (memory-recall/recall-by-endpoint
                {:domain :mathematics :evidence-store ::unused}
                "lean/field-simp-denominator"
                {:limit 1000
                 :fetch-hyperedges
                 (fn [_ {:keys [limit]}]
                   (reset! seen-limit limit)
                   (throw (ex-info "admission busy" {:status 503})))})]
    (is (= memory-recall/max-limit @seen-limit))
    (is (false? (:ok result)))
    (is (= :substrate-read-failed (get-in result [:error :error/code])))
    (is (empty? (:memories result)))
    (is (number? (:elapsed-ms result)))))

(deftest substrate-client-sends-conjunctive-type-and-end
  (let [seen-url (atom nil)
        get-edn-var (ns-resolve 'futon3c.substrate.client 'get-edn!)]
    (with-redefs-fn
      {#'substrate/configured-url (constantly "http://substrate.test")
       get-edn-var (fn [url _timeout-ms]
                     (reset! seen-url url)
                     {:hyperedges []})}
      #(is (= [] (substrate/hyperedges-by-end
                   "p4ng/R15" {:type :memory/assert :limit 7}))))
    (is (= (str "http://substrate.test/api/alpha/hyperedges?"
                "end=p4ng%2FR15&type=memory%2Fassert&limit=7")
           @seen-url))))

(deftest batch-recall-projects-several-endpoints-with-one-substrate-call
  (let [{math :mathematics} (fixtures)
        edge (assoc-in (:edge math) [:hx/props :domain] :mathematics)
        calls (atom [])
        result
        (memory-recall/recall-by-endpoints
         {:domain :mathematics}
         ["lean/field-simp-denominator" "pattern/empty"]
         {:limit 3
          :trace-id "trace-batch"
          :fetch-components
          (fn [endpoints opts]
            (swap! calls conj [endpoints opts])
            {:ok true
             :trace-id "trace-batch"
             :temporal-basis {:mode :current}
             :groups
             [{:endpoint "lean/field-simp-denominator"
               :components [{:edge edge :entry (:entry math)}]
               :audit {:selected-count 1}}
              {:endpoint "pattern/empty"
               :components []
               :audit {:selected-count 0}}]
             :audit {:distinct-edge-count 1}
             :timing {:endpoint-selection-ms 12.0
                      :service-total-ms 15.0}})})]
    (is (= 1 (count @calls)))
    (is (= [["lean/field-simp-denominator" "pattern/empty"]
            {:limit 9 :trace-id "trace-batch"}]
           (first @calls)))
    (is (= [["e-math-1"] []]
           (mapv #(mapv :memory/id (:memories %)) (:recalls result))))
    (is (= 12.0
           (get-in result
                   [:substrate :timing :endpoint-selection-ms])))
    (is (number? (get-in result [:substrate :caller-wall-ms])))))

(deftest substrate-client-posts-bounded-memory-projection-with-trace
  (let [seen (atom nil)
        post-edn-var (ns-resolve 'futon3c.substrate.client 'post-edn!)]
    (with-redefs-fn
      {#'substrate/configured-url (constantly "http://substrate.test")
       post-edn-var
       (fn [url payload timeout-ms trace-id]
         (reset! seen [url payload timeout-ms trace-id])
         {:ok true :groups []})}
      #(is (= {:ok true :groups []}
              (substrate/memory-projection
               ["pattern/a" "pattern/b"]
               {:limit 9 :timeout-ms 1234 :trace-id "trace-1"}))))
    (is (= ["http://substrate.test/api/alpha/memory/projection"
            {:endpoints ["pattern/a" "pattern/b"] :limit 9}
            1234
            "trace-1"]
           @seen))))
