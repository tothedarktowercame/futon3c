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

(deftest ws-invoke-nil-timeout-waits-indefinitely
  (testing "a nil/non-positive timeout waits for the agent instead of imposing one"
    (let [sent (promise)]
      (ws-invoke/register! "agent-unbounded" #(deliver sent %))
      (let [f (future (ws-invoke/invoke! "agent-unbounded" "hi" nil nil))
            invoke-id (:invoke_id (json/parse-string @sent true))]
        ;; Comfortably past the old 60-minute-equivalent decision point: the
        ;; call is still waiting, not sentinel-ed.
        (is (= ::still-waiting (deref f 150 ::still-waiting)))
        (ws-invoke/resolve! "agent-unbounded" invoke-id {:result "late but real"})
        (is (= {:result "late but real"} (deref f 500 nil))))
      (ws-invoke/unregister! "agent-unbounded"))))

(deftest ws-disconnect-releases-unbounded-callers
  (testing "an unbounded caller is released when its socket dies, not left forever"
    ;; Without this, removing the WS deadline would trade a lost result for a
    ;; permanently wedged lane: no clock remains to rescue the blocked caller.
    (let [sent (promise)]
      (ws-invoke/register! "agent-drop" #(deliver sent %))
      (let [f (future (ws-invoke/invoke! "agent-drop" "hi" nil nil))]
        @sent
        (is (= ::still-waiting (deref f 100 ::still-waiting)))
        (ws-invoke/unregister! "agent-drop")
        (is (= ws-invoke/disconnected-result (deref f 1000 ::never-released))))))
  (testing "a closed connection releases callers via unregister-current!"
    (let [sent (promise)
          conn (Object.)]
      (ws-invoke/register! "agent-closed" #(deliver sent %) {:connection conn})
      (let [f (future (ws-invoke/invoke! "agent-closed" "hi" nil nil))]
        @sent
        (is (true? (ws-invoke/unregister-current! "agent-closed" conn)))
        (is (= ws-invoke/disconnected-result (deref f 1000 ::never-released)))))))

(deftest ws-invoke-late-result-is-harvested-not-dropped
  (testing "a result arriving after the caller gave up reaches the late handler"
    (let [sent (promise)
          late (atom nil)]
      (ws-invoke/register! "agent-late" #(deliver sent %))
      (ws-invoke/set-late-result-handler! #(reset! late %))
      (try
        (let [outcome (future (ws-invoke/invoke! "agent-late" "slow" nil 20))
              invoke-id (:invoke_id (json/parse-string @sent true))]
          (is (= ws-invoke/timeout-sentinel (deref outcome 500 nil)))
          ;; The old code dissoc'd the pending entry here, so resolve! returned
          ;; false and the agent's real reply was discarded.
          (is (true? (ws-invoke/resolve! "agent-late" invoke-id {:result "done anyway"})))
          (is (= "agent-late" (:agent-id @late)))
          (is (= invoke-id (:invoke-id @late)))
          (is (= {:result "done anyway"} (:result @late))))
        (finally
          (ws-invoke/set-late-result-handler! nil)
          (ws-invoke/unregister! "agent-late"))))))

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

(deftest broadcast-evicts-throwing-sender
  (testing "one dead sender is evicted while other broadcast receivers still get the frame"
    (let [good-sent (atom [])]
      (ws-invoke/register! "agent-good" #(swap! good-sent conj %))
      (ws-invoke/register! "agent-dead" (fn [_] (throw (ex-info "closed" {}))))
      (ws-invoke/broadcast-frame! {"type" "agents_status"})
      (is (= [(json/generate-string {"type" "agents_status"})] @good-sent))
      (is (contains? (set (ws-invoke/connected-agent-ids)) "agent-good"))
      (is (not (contains? (set (ws-invoke/connected-agent-ids)) "agent-dead")))
      (ws-invoke/unregister! "agent-good"))))

(deftest send-frame-evicts-false-returning-sender
  (testing "send-frame! evicts senders that explicitly return false"
    (ws-invoke/register! "agent-false" (constantly false))
    (is (false? (ws-invoke/send-frame! "agent-false" {"type" "invoke_delivery"})))
    (is (not (contains? (set (ws-invoke/connected-agent-ids)) "agent-false")))))

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
