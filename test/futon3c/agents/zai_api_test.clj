(ns futon3c.agents.zai-api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zai-api :as zai]
            [futon3c.evidence.boundary :as boundary]))

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
