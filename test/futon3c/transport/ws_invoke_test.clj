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
