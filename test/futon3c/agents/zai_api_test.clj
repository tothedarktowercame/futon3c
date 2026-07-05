(ns futon3c.agents.zai-api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zai-api :as zai]))

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
