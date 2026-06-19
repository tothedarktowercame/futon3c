(ns futon3c.agency.agent-pouch-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.agent-pouch :as pouch]))

(use-fixtures
  :each
  (fn [f]
    (pouch/clear!)
    (try
      (f)
      (finally
        (pouch/clear!)))))

(defn- fake-claude-bin []
  (let [f (java.io.File/createTempFile "fake-claude-stream-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys\n"
               "sid = 'fake-session-1'\n"
               "for line in sys.stdin:\n"
               "    data = json.loads(line)\n"
               "    text = data.get('message', {}).get('content', [{}])[0].get('text', '')\n"
               "    if text == 'CRASH':\n"
               "        sys.exit(7)\n"
               "    if text == 'SLOW':\n"
               "        import time; time.sleep(1)\n"
               "    print(json.dumps({'type':'system','session_id':sid}), flush=True)\n"
               "    print(json.dumps({'type':'assistant','message':{'content':[{'type':'text','text':'reply:' + text}]}}), flush=True)\n"
               "    print(json.dumps({'type':'result','session_id':sid,'is_error':False}), flush=True)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest enabled-defaults-off
  (System/clearProperty "FUTON3C_KANGAROO")
  (is (false? (pouch/enabled?))))

(deftest feed-turn-spawns-one-warm-process-and-reuses-it
  (let [bin (fake-claude-bin)
        r1 (pouch/feed-turn! "claude-test" "one" {:claude-bin bin :timeout-ms 2000})
        pid1 (get-in (pouch/snapshot) ["claude-test" :pid])
        r2 (pouch/feed-turn! "claude-test" "two" {:claude-bin bin :timeout-ms 2000})
        snap (pouch/snapshot)]
    (is (= "reply:one" (:result r1)))
    (is (= "reply:two" (:result r2)))
    (is (= "fake-session-1" (:session-id r2)))
    (is (= pid1 (get-in snap ["claude-test" :pid])))
    (is (= 2 (get-in snap ["claude-test" :turn-count])))))

(deftest crash-evicts-pouch-so-caller-can-fallback
  (let [bin (fake-claude-bin)]
    (is (thrown? Throwable
                 (pouch/feed-turn! "claude-crash" "CRASH" {:claude-bin bin :timeout-ms 2000})))
    (is (nil? (get (pouch/snapshot) "claude-crash")))))

(deftest idle-evict-removes-old-pouch
  (let [bin (fake-claude-bin)]
    (pouch/feed-turn! "claude-idle" "one" {:claude-bin bin :timeout-ms 2000})
    (Thread/sleep 5)
    (is (= ["claude-idle"] (pouch/evict-idle! 1)))
    (is (empty? (pouch/snapshot)))))

(deftest on-event-hook-sees-stream-and-cannot-kill-turn
  (let [bin (fake-claude-bin)
        seen (atom [])
        r (pouch/feed-turn! "claude-hook" "one"
                            {:claude-bin bin :timeout-ms 2000
                             :on-event (fn [event]
                                         (swap! seen conj (:type event))
                                         (throw (ex-info "hook boom" {})))})]
    (is (= "reply:one" (:result r)) "a throwing hook does not kill the turn")
    (is (= ["system" "assistant" "result"] @seen))))

(deftest in-flight-pouch-survives-idle-eviction
  ;; A turn can outlast the idle TTL (last-used is stamped at turn START, but a
  ;; 30-min turn still exceeds a 30-min TTL); mid-turn the pouch must be immune.
  (let [bin (fake-claude-bin)]
    (pouch/feed-turn! "claude-busy" "one" {:claude-bin bin :timeout-ms 2000})
    (let [slow (future
                 ;; SLOW makes the fake bin sleep before replying, holding the
                 ;; turn open while we attempt eviction underneath it.
                 (pouch/feed-turn! "claude-busy" "SLOW" {:claude-bin bin :timeout-ms 5000}))
          deadline (+ (System/currentTimeMillis) 2000)]
      (while (and (not (get-in (pouch/snapshot) ["claude-busy" :in-flight?]))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 20))
      (is (true? (get-in (pouch/snapshot) ["claude-busy" :in-flight?])))
      (is (= [] (pouch/evict-idle! 1)) "mid-turn pouch is not idle-evictable")
      (is (= "reply:SLOW" (:result @slow)))
      (is (false? (get-in (pouch/snapshot) ["claude-busy" :in-flight?])))
      (Thread/sleep 5)
      (is (= ["claude-busy"] (pouch/evict-idle! 1)) "evictable again once the turn ends"))))
