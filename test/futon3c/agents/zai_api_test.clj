(ns futon3c.agents.zai-api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zai-api :as zai]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.peripheral.memory-backend :as memory-backend]
            [futon3c.peripheral.tools :as tools]))

(defn- tool-call
  [i]
  {:id (str "tc-" i)
   :type "function"
   :function {:name "noop"
              :arguments (str "{\"i\":" i "}")}})

(defn- tool-response
  [i]
  {:choices [{:message {:role "assistant"
                        :content ""
                        :tool_calls [(tool-call i)]}}]})

(defn- text-response
  [text]
  {:choices [{:message {:role "assistant"
                        :content text}}]})

(defn- text-response-with-usage
  [text usage]
  (assoc (text-response text) :usage usage))

(defn- fake-tool-result
  [_backend _tool-opts tc]
  {:detail {:id (:id tc)
            :name (get-in tc [:function :name])
            :input {}}
   :message {:role "tool"
             :tool_call_id (:id tc)
             :name (get-in tc [:function :name])
             :content "ok"}})

(defn- make-invoke
  [opts]
  (zai/make-invoke-fn
   (merge {:agent-id "zai-test"
           :api-key "test-key"
           :initial-session-id "sid-test"
           :evidence-store (atom {:entries {} :order []})
           :memory-mode :none
           :cwd "/tmp"}
          opts)))

(defn- synthetic-auto-continues
  [messages]
  (filter #(and (= "user" (:role %))
                (str/starts-with? (:content %) "[harness auto-continue "))
          messages))

(deftest auto-continues-after-budget-exhaustion-and-finishes
  (let [calls (atom [])
        events (atom [])
        responses (atom (concat (map tool-response (range 25))
                                [(text-response "done")]))
        invoke (make-invoke {})]
    (with-redefs-fn {#'zai/chat! (fn [_client _opts messages]
                                   (swap! calls conj messages)
                                   (let [resp (first @responses)]
                                     (swap! responses rest)
                                     resp))
                     #'zai/execute-tool fake-tool-result
                     #'zai/sink! (fn [_agent-id event]
                                   (swap! events conj event))}
      (fn []
      (let [resp (invoke "work" nil)
            final-messages (last @calls)
            auto-messages (synthetic-auto-continues final-messages)]
        (is (= "done" (:result resp)))
        (is (= "sid-test" (:session-id resp)))
        (is (nil? (:error resp)))
        (is (= 1 (count auto-messages)))
        (is (str/includes? (:content (first auto-messages))
                           "round budget exhausted mid-task"))
        (is (some #(= {:type "text" :text "[auto-continue 1/8]"} %) @events)))))))

(deftest natural-completion-does-not-inject-auto-continue
  (let [calls (atom [])
        invoke (make-invoke {})]
    (with-redefs-fn {#'zai/chat! (fn [_client _opts messages]
                                   (swap! calls conj messages)
                                   (text-response "already done"))
                     #'zai/execute-tool fake-tool-result}
      (fn []
      (let [resp (invoke "small task" nil)
            final-messages (last @calls)]
        (is (= "already done" (:result resp)))
        (is (nil? (:error resp)))
        (is (empty? (synthetic-auto-continues final-messages))))))))

(deftest cap-exhaustion-preserves-max-tool-rounds-error
  (let [calls (atom [])
        invoke (make-invoke {:auto-continue-max 2})]
    (with-redefs-fn {#'zai/chat! (fn [_client _opts messages]
                                   (swap! calls conj messages)
                                   (tool-response (count @calls)))
                     #'zai/execute-tool fake-tool-result}
      (fn []
      (let [resp (invoke "never finish" nil)
            final-messages (last @calls)]
        (is (= "[z.ai stopped after maximum tool rounds]" (:result resp)))
        (is (= "max-tool-rounds" (:error resp)))
        (is (= 2 (count (synthetic-auto-continues final-messages)))))))))

(deftest zero-auto-continue-cap-restores-single-budget-behavior
  (let [calls (atom [])
        invoke (make-invoke {:auto-continue-max 0})]
    (with-redefs-fn {#'zai/chat! (fn [_client _opts messages]
                                   (swap! calls conj messages)
                                   (tool-response (count @calls)))
                     #'zai/execute-tool fake-tool-result}
      (fn []
      (let [resp (invoke "old behavior" nil)
            final-messages (last @calls)]
        (is (= 24 (count @calls)))
        (is (= "[z.ai stopped after maximum tool rounds]" (:result resp)))
        (is (= "max-tool-rounds" (:error resp)))
        (is (empty? (synthetic-auto-continues final-messages))))))))

(deftest invoke-closure-dispatches-through-run-tool-rounds-var
  (let [invoke (make-invoke {})]
    (with-redefs [zai/run-tool-rounds! (fn [ctx]
                                         {:result (str "redefined for " (:agent-id ctx))
                                          :session-id (:sid ctx)})]
      (let [resp (invoke "hot swap?" "sid-hot")]
        (is (= "redefined for zai-test" (:result resp)))
        (is (= "sid-hot" (:session-id resp)))))))

(deftest cycle-student-budget-scales-the-interior-runner-envelope
  (let [invoke (make-invoke {})
        captured (atom nil)]
    (with-redefs [zai/run-tool-rounds! (fn [ctx]
                                        (reset! captured ctx)
                                        {:result "captured"
                                         :session-id "budget-session"})]
      (invoke "work" "budget-session"
              {:dispatch-id "budget-job"
               :timeout-ms 3600000
               :student-runner-budget {:wall-clock-minutes 60}})
      (is (= 3600000 (get-in @captured [:opts :timeout-ms])))
      (is (= 16 (:auto-continue-max @captured))
          "60 minutes doubles the historical 30-minute continuation allowance"))))

(deftest transcript-records-prompt-round-profile-and-stable-turn-id
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store :profile :zai})]
    (with-redefs [zai/chat! (fn [_client _opts _messages]
                              (text-response "proved"))]
      (is (= "proved" (:result (invoke "prove theorem T" nil))))
      (let [entries (mapv #(get-in @store [:entries %]) (:order @store))
            start (first entries)
            round (second entries)
            turn-id (get-in start [:evidence/body :turn-id])]
        (is (= 2 (count entries)))
        (is (= [:transcript :turn-start :zai] (:evidence/tags start)))
        (is (= "prove theorem T" (get-in start [:evidence/body :prompt])))
        (is (= :zai (get-in start [:evidence/body :profile])))
        (is (str/starts-with? turn-id "zai-turn-"))
        (is (= [:transcript :turn-round :zai] (:evidence/tags round)))
        (is (= turn-id (get-in round [:evidence/body :turn-id])))
        (is (= true (get-in round [:evidence/body :final])))
        (is (= "proved" (get-in round [:evidence/body :text])))))))

(deftest records-normalized-zai-usage-per-round
  (let [full-usage {:prompt_tokens 101
                    :prompt_tokens_details {:cached_tokens 23}
                    :completion_tokens 47
                    :completion_tokens_details {:reasoning_tokens 11}
                    :total_tokens 148}
        expected {:cost/input-tokens 101
                  :cost/cached-input-tokens 23
                  :cost/output-tokens 47
                  :cost/reasoning-tokens 11
                  :cost/total-tokens 148
                  :cost/source :zai}]
    (doseq [[usage expected-cost]
            [[full-usage expected]
             [(dissoc full-usage :prompt_tokens_details)
              (dissoc expected :cost/cached-input-tokens)]
             [(dissoc full-usage :completion_tokens_details)
              (dissoc expected :cost/reasoning-tokens)]
             [nil nil]]]
      (let [store (atom {:entries {} :order []})
            events (atom [])
            invoke (make-invoke {:evidence-store store})]
        (with-redefs [zai/chat! (fn [_client _opts _messages]
                                  (if usage
                                    (text-response-with-usage "still works" usage)
                                    (text-response "still works")))
                      zai/sink! (fn [_agent-id event] (swap! events conj event))]
          (is (= "still works" (:result (invoke "measure" nil))))
          (let [round (->> (:order @store)
                           (map #(get-in @store [:entries %]))
                           (filter #(= :turn-round
                                       (get-in % [:evidence/body :event])))
                           first)
                body (:evidence/body round)
                cost (select-keys body (keys expected))
                usage-events (filter #(= "usage" (:type %)) @events)]
            (is (= (or expected-cost {}) cost))
            (is (not-any? (fn [[_ v]] (nil? v))
                          (filter (fn [[k _]] (= "cost" (namespace k))) body)))
            (is (= (if expected-cost
                     [(assoc expected-cost :type "usage")]
                     [])
                   (vec usage-events)))))))))

(deftest zaif-profile-records-prompt-decision-and-final-round
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store :profile :zaif})]
    (with-redefs [zai/chat! (fn [_client _opts _messages]
                              (text-response "zaif done"))]
      (is (= "zaif done" (:result (invoke "choose and prove" nil))))
      (let [entries (mapv #(get-in @store [:entries %]) (:order @store))
            events (mapv #(get-in % [:evidence/body :event]) entries)
            turn-ids (set (keep #(get-in % [:evidence/body :turn-id]) entries))]
        (is (= [:turn-start :zaif-arm-choice :turn-round] events))
        (is (= 1 (count turn-ids)))
        (is (= [:transcript :turn-start :zaif]
               (:evidence/tags (first entries))))))))

(deftest invoke-construction-requires-evidence-store
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"requires a durable evidence store"
       (zai/make-invoke-fn {:agent-id "zai-no-store"
                            :api-key "test-key"
                            :memory-mode :none}))))

(deftest transcript-persistence-failure-is-counted-and-stops-turn
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        before (:failure-count (zai/transcript-persistence-status))]
    (with-redefs [boundary/append!
                  (fn [& _] {:ok false :error/code :store-rejected})]
      (is (thrown? clojure.lang.ExceptionInfo (invoke "must be durable" nil))))
    (let [status (zai/transcript-persistence-status)]
      (is (= (inc before) (:failure-count status)))
      (is (string? (:last-error status))))))

(deftest refused-memory-record-persists-full-violation-reason
  (let [reason (str "EvidenceEntry subject ref/type rejected: "
                    (apply str (repeat 400 "x")) " END-OF-REASON")
        result {:ok false
                :error {:error/code :invalid-entry
                        :error/message "EvidenceEntry did not conform to shape"
                        :error/context
                        {:receipt
                         {:ok false
                          :error/code :invalid-entry
                          :invariant/violation
                          {:invariant "I-evidence-per-turn"
                           :kind :shape
                           :reason reason}}}}}
        agent-content (#'zai/result-string result)
        executed [{:error? true
                   :result result
                   :message {:content agent-content}}]
        calls (#'zai/transcript-calls
               [{:name "memory_record" :input {:name "bad-ref"}}]
               executed)
        store (atom {:entries {} :order []})]
    (#'zai/persist-round!
     {:evidence-store store :agent-id "zai-1" :sid "sid-refusal"
      :turn-id "turn-refusal" :profile :zai :round 1 :text "" :calls calls})
    (let [entry (get-in @store [:entries (first (:order @store))])
          call (get-in entry [:evidence/body :calls 0])]
      ;; The model-facing result uses the 12k cap, not the 240-char transcript
      ;; preview, so this incident's diagnostic reached the agent in full.
      (is (str/includes? agent-content "END-OF-REASON"))
      (is (str/includes? (get-in call [:result :preview]) "…[+"))
      ;; The durable record separately preserves the structured refusal.
      (is (= :invalid-entry (get-in call [:refusal :error/code])))
      (is (= :shape (get-in call [:refusal :invariant/violation :kind])))
      (is (= reason (get-in call [:refusal :invariant/violation :reason]))))))

(deftest memory-read-is-registered-and-dispatched-with-the-evidence-store
  (let [specs (#'zai/openai-tools :full)
        spec (some #(when (= "memory_read" (get-in % [:function :name])) %)
                   specs)
        captured (atom nil)
        store (atom {:entries {} :order []})
        call {:id "tc-memory-read"
              :type "function"
              :function {:name "memory_read"
                         :arguments "{\"evidence_id\":\"e-one\"}"}}
        executed
        (with-redefs [memory-backend/memory-read
                      (fn [ctx args]
                        (reset! captured [ctx args])
                        {:ok true :result {:items []}})]
          (#'zai/execute-tool
           (tools/make-mock-backend)
           {:agent-id "zai-test" :session-id-atom (atom "sid")
            :evidence-store store}
           call))]
    (is (some? spec))
    (is (str/includes? (get-in spec [:function :description]) "FULL BODY"))
    (is (= {:evidence-id "e-one"} (second @captured)))
    (is (identical? store (:evidence-store (first @captured))))
    (is (true? (get-in executed [:result :ok])))))

(deftest memory-search-dispatches-the-f8-query-shape-to-shared-semantics
  (let [captured (atom nil)
        store (atom {:entries {} :order []})
        call {:id "tc-memory-search"
              :type "function"
              :function {:name "memory_search"
                         :arguments "{\"tags\":[\"a03J04\"],\"limit\":10}"}}
        executed
        (with-redefs [memory-backend/memory-search
                      (fn [ctx args]
                        (reset! captured [ctx args])
                        {:ok true :result {:items [{:id "e-d11811de"}]}})]
          (#'zai/execute-tool
           (tools/make-mock-backend)
           {:agent-id "f8-student" :session-id-atom (atom "sid")
            :evidence-store store}
           call))]
    (is (= {:tags ["a03J04"] :limit 10} (second @captured)))
    (is (identical? store (:evidence-store (first @captured))))
    (is (= "e-d11811de"
           (get-in executed [:result :result :items 0 :id])))))
