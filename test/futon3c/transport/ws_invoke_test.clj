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

(deftest ws-observer-broadcast-only
  (testing "observers receive broadcasts but are never invoke targets (I-1)"
    (let [obs-sent (atom [])
          agent-sent (atom [])]
      (ws-invoke/register! "emacs-hud" #(swap! obs-sent conj %) {:observer? true})
      (ws-invoke/register! "agent-4" #(swap! agent-sent conj %))
      ;; not invocable
      (is (false? (ws-invoke/available? "emacs-hud")))
      (is (true? (ws-invoke/available? "agent-4")))
      (is (= {:error :ws-observer-not-invocable}
             (ws-invoke/invoke! "emacs-hud" "hi" nil 1000)))
      ;; excluded from invocable set, present in observer set
      (is (not (contains? (set (ws-invoke/connected-agent-ids)) "emacs-hud")))
      (is (contains? (set (ws-invoke/connected-agent-ids)) "agent-4"))
      (is (contains? (set (ws-invoke/connected-observer-ids)) "emacs-hud"))
      ;; but broadcast-frame! DOES reach the observer
      (ws-invoke/broadcast-frame! {"type" "agents_status"})
      (is (= [(json/generate-string {"type" "agents_status"})] @obs-sent))
      (is (= [(json/generate-string {"type" "agents_status"})] @agent-sent))
      (ws-invoke/unregister! "emacs-hud")
      (ws-invoke/unregister! "agent-4"))))

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
