(ns futon3c.transport.ws-invoke-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.ws.invoke :as ws-invoke]))

(deftest ws-invoke-roundtrip
  (testing "invoke! sends JSON and resolve! delivers result"
    (let [sent (promise)]
      (ws-invoke/register! "agent-1" #(deliver sent %))
      (let [f (future (ws-invoke/invoke! "agent-1" "hi" "sess-1" 1000))
            payload (json/parse-string @sent true)
            invoke-id (:invoke_id payload)]
        (is (= "invoke" (:type payload)))
        (is (= "hi" (:prompt payload)))
        (ws-invoke/resolve! "agent-1" invoke-id {:result "ok" :session-id "sess-2"})
        (is (= {:result "ok" :session-id "sess-2"}
               (deref f 500 nil))))
      (ws-invoke/unregister! "agent-1"))))

(deftest ws-invoke-timeout
  (testing "invoke! returns timeout sentinel"
    (ws-invoke/register! "agent-2" (constantly nil))
    (is (= ws-invoke/timeout-sentinel
           (ws-invoke/invoke! "agent-2" "slow" nil 10)))
    (ws-invoke/unregister! "agent-2")))

(deftest ws-send-frame-best-effort
  (testing "send-frame! serializes JSON over an active WS sender"
    (let [sent (atom nil)]
      (ws-invoke/register! "agent-3" #(reset! sent %))
      (is (true? (ws-invoke/send-frame! "agent-3" {"type" "invoke_delivery"
                                                   "invoke_trace_id" "invoke-xyz"})))
      (is (= {"type" "invoke_delivery"
              "invoke_trace_id" "invoke-xyz"}
             (some-> @sent (json/parse-string))))
      (ws-invoke/unregister! "agent-3")))
  (testing "send-frame! returns false when no WS sender exists"
    (is (false? (ws-invoke/send-frame! "agent-missing" {"type" "invoke_delivery"})))))
