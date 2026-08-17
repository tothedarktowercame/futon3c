(ns futon3c.agents.memory-mcp-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.memory-mcp :as mcp]
            [futon3c.agents.memory-provisioning :as provisioning]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.peripheral.pull-receipts :as pull-receipts]
            [futon3c.peripheral.real-backend :as real-backend]
            [futon3c.peripheral.tools :as tools]))

(deftest configured-seats-have-mathematics-domain-and-scribe-is-exclusive
  (doseq [agent-id ["zai-4" "codex-2" "codex-5" "codex-6"]]
    (is (= :mathematics (provisioning/domain-for agent-id))))
  (is (provisioning/tool-enabled? "codex-2" :memory-record))
  (doseq [agent-id ["zai-4" "codex-5" "codex-6"]]
    (is (not (provisioning/tool-enabled? agent-id :memory-record)))))

(deftest mcp-tool-list-teaches-required-contract
  (let [response (mcp/handle-request
                  {}
                  {:jsonrpc "2.0" :id 1 :method "tools/list"})
        listed (get-in response [:result :tools])
        description (:description (first (filter #(= "memory_record" (:name %))
                                                 listed)))]
    (is (= #{"memory_record" "memory_search"} (set (map :name listed))))
    (doseq [required ["name (non-blank string)"
                      "body (memory content)"
                      "subjects [{ref/type, ref/id}]"]]
      (is (str/includes? description required)))))

(deftest memory-call-stamps-controller-identity-and-domain
  (let [seen (atom nil)
        response
        (mcp/handle-request
         {:agent-id "codex-2"
          :session-file nil
          :domain :mathematics
          :evidence-store ::store
          :record-memory-fn
          (fn [ctx payload]
            (reset! seen [ctx payload])
            {:ok true :id "e-test"})}
         {:jsonrpc "2.0" :id 2 :method "tools/call"
          :params {:name "memory_record"
                   :arguments {:name "n" :body "b"
                               :subjects [{"ref/type" "mission"
                                           "ref/id" "M"}]}}})]
    (is (= "codex-2" (get-in @seen [0 :agent-id])))
    (is (= :mathematics (get-in @seen [0 :domain])))
    (is (= ::store (get-in @seen [0 :evidence-store])))
    (is (= "n" (get-in @seen [1 :name])))
    (is (false? (get-in response [:result :isError])))))

(deftest memory-search-queries-explicit-store-and-never-records
  (let [seen (atom nil)
        record-called? (atom false)
        items [{:evidence/id "e-one" :evidence/type :memory}]
        response
        (with-redefs [store/query*
                      (fn [actual-store query]
                        (reset! seen [actual-store query])
                        items)]
          (mcp/handle-request
           {:evidence-store ::store
            :record-memory-fn
            (fn [& _]
              (reset! record-called? true)
              {:ok true})}
           {:jsonrpc "2.0" :id 3 :method "tools/call"
            :params {:name "memory_search"
                     :arguments
                     {:subject {:ref/type "git-commit" :ref/id "abc123"}
                      :type "memory"
                      :claim_type "assert"
                      :author "codex-5"
                      :since "2026-08-01T00:00:00Z"
                      :tags ["formalization" :lean]
                      :limit 150
                      :include_ephemeral true}}}))]
    (is (= ::store (first @seen)))
    (is (= {:query/subject {:ref/type :git-commit :ref/id "abc123"}
            :query/type :memory
            :query/claim-type :assert
            :query/author "codex-5"
            :query/since "2026-08-01T00:00:00Z"
            :query/tags [:formalization :lean]
            :query/limit 100
            :query/include-ephemeral? true}
           (second @seen)))
    (is (= (json/generate-string items)
           (get-in response [:result :content 0 :text])))
    (is (false? @record-called?))
    (is (false? (get-in response [:result :isError])))))

(deftest lone-tag-memory-search-adds-problem-subject-fallback
  (let [queries (atom [])
        tag-hit {:evidence/id "e-tag"}
        subject-hit {:evidence/id "e-subject"}]
    (with-redefs [store/query*
                  (fn [_ query]
                    (swap! queries conj query)
                    (if (:query/tags query) [tag-hit] [subject-hit]))
                  pull-receipts/record-pull-uses! (fn [& _] {:ok true})]
      (let [response (mcp/handle-request
                      {:agent-id "codex-2" :session-file nil
                       :domain :mathematics :evidence-store ::store}
                      {:jsonrpc "2.0" :id 4 :method "tools/call"
                       :params {:name "memory_search"
                                :arguments {:tags ["a03J04"] :limit 10}}})]
        (is (= [{:query/limit 10 :query/tags [:a03J04]}
                {:query/limit 10
                 :query/subject {:ref/type :problem :ref/id "a03J04"}}]
               @queries))
        (is (= [tag-hit subject-hit]
               (json/parse-string
                (get-in response [:result :content 0 :text]) true)))))))

(deftest cycle-dispatch-id-reaches-codex-memory-search-receipt
  (let [seen (atom nil)]
    (with-redefs [store/query* (fn [_ _] [{:evidence/id "e-codex-used"}])
                  pull-receipts/record-pull-uses!
                  (fn [ctx tool result]
                    (reset! seen [ctx tool result]) [])]
      (mcp/handle-request
       {:agent-id "codex-solver" :session-file nil :domain :mathematics
        :dispatch-id "job-codex-cycle" :evidence-store ::store}
       {:jsonrpc "2.0" :id 5 :method "tools/call"
        :params {:name "memory_search" :arguments {:tags ["lean"]}}}))
    (is (= "job-codex-cycle" (get-in @seen [0 :dispatch-id])))
    (is (= "memory_search" (second @seen)))
    (is (= "e-codex-used" (get-in @seen [2 :result :items 0 :evidence/id])))))

(deftest unknown-mcp-tool-still-returns-invalid-params
  (let [response (mcp/handle-request
                  {}
                  {:jsonrpc "2.0" :id 4 :method "tools/call"
                   :params {:name "not_a_tool" :arguments {}}})]
    (is (= {:code -32602 :message "Unknown tool"} (:error response)))))

(deftest codex-adapter-shaped-call-reaches-writer-with-controller-authorship
  (let [seen (atom nil)
        backend (real-backend/make-real-backend
                 {:evidence-store ::store
                  :agent-id "codex-2"
                  :session-id-fn (constantly "codex-session")
                  :memory-domain :mathematics})
        payload {:name "n" :body "b"
                 :subjects [{:ref/type "mission" :ref/id "M"}]}]
    (with-redefs [memory-write/record-memory!
                  (fn [ctx actual-payload]
                    (reset! seen [ctx actual-payload])
                    {:ok true :id "e-test"})]
      (tools/execute-tool backend :memory-record [payload]))
    (is (= "codex-2" (get-in @seen [0 :agent-id])))
    (is (= "codex-session" (get-in @seen [0 :session-id])))
    (is (= :mathematics (get-in @seen [0 :domain])))
    (is (= ::store (get-in @seen [0 :evidence-store])))
    (is (= payload (second @seen)))))
