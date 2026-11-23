(ns futon3c.agency.agent-pouch-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
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
               ;; Like the real CLI, results carry the process's running total.
               "total = [0.0]\n"
               "for line in sys.stdin:\n"
               "    data = json.loads(line)\n"
               "    content = data.get('message', {}).get('content', '')\n"
               "    text = content if isinstance(content, str) else content[0].get('text', '')\n"
               "    if text == 'CRASH':\n"
               "        sys.exit(7)\n"
               "    if text == 'SLOW':\n"
               "        import time; time.sleep(1)\n"
               "    if text == 'BURST':\n"
               ;; One solicited turn, then — before reading the next stdin line —
               ;; an agent-initiated turn that is still being emitted when the
               ;; caller's drain runs (the sleep keeps it out of the drain).
               "        print(json.dumps({'type':'system','session_id':sid}), flush=True)\n"
               "        print(json.dumps({'type':'assistant','message':{'content':[{'type':'text','text':'reply:BURST'}]}}), flush=True)\n"
               "        print(json.dumps({'type':'result','session_id':sid,'is_error':False}), flush=True)\n"
               "        import time; time.sleep(0.4)\n"
               "        print(json.dumps({'type':'system','subtype':'task_notification'}), flush=True)\n"
               "        print(json.dumps({'type':'system','subtype':'init','session_id':sid}), flush=True)\n"
               "        print(json.dumps({'type':'assistant','message':{'content':[{'type':'text','text':'unsolicited report'}]}}), flush=True)\n"
               "        print(json.dumps({'type':'result','session_id':sid,'is_error':False}), flush=True)\n"
               "        continue\n"
               "    if text == '/compact':\n"
               "        print(json.dumps({'type':'system','subtype':'status','status':'compacting'}), flush=True)\n"
               "        print(json.dumps({'type':'system','subtype':'status','status':None,'compact_result':'success','compact_error':None}), flush=True)\n"
               "        print(json.dumps({'type':'system','subtype':'init','session_id':sid}), flush=True)\n"
               "        total[0] += 0.25\n"
               "        print(json.dumps({'type':'result','subtype':'success','session_id':sid,'usage':{'input_tokens':12},'total_cost_usd':total[0]}), flush=True)\n"
               "        continue\n"
               "    print(json.dumps({'type':'system','session_id':sid}), flush=True)\n"
               "    print(json.dumps({'type':'assistant','message':{'content':[{'type':'text','text':'reply:' + text}]}}), flush=True)\n"
               "    total[0] += 0.5\n"
               "    print(json.dumps({'type':'result','session_id':sid,'is_error':False,'total_cost_usd':total[0]}), flush=True)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest enabled-defaults-off
  (System/clearProperty "FUTON3C_KANGAROO")
  (is (false? (pouch/enabled?))))

(deftest headerless-first-turn-keeps-unattended-count-at-zero
  (is (= 0 (pouch/note-turn-origin! "claude-headerless-test" "fresh-session" "provider continuation")))
  (is (= 0 (pouch/turns-without-operator "claude-headerless-test" "fresh-session"))))

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

(deftest compact-pouch-sends-raw-control-and-collects-status
  (let [bin (fake-claude-bin)]
    (pouch/feed-turn! "claude-compact" "warm" {:claude-bin bin :timeout-ms 2000})
    (is (= {:ok true
            :compact-result "success"
            :compact-error nil
            :orphaned-turns 0
            :session-id "fake-session-1"
            :usage {:input_tokens 12}
            ;; warm turn left the running total at 0.5; compact took it to 0.75
            :total-cost-usd 0.25
            :pouch-total-cost-usd 0.75}
           (pouch/compact-pouch! "claude-compact" {:timeout-ms 2000})))))

(deftest compact-pouch-reads-past-an-in-flight-unsolicited-turn
  ;; claude-1, 2026-09-01 18:24: a task-notification turn was mid-flight when
  ;; /compact was written; its `result` was taken for the compaction's.
  (let [bin (fake-claude-bin)]
    (is (= "reply:BURST"
           (:result (pouch/feed-turn! "claude-burst" "BURST"
                                      {:claude-bin bin :timeout-ms 2000}))))
    (let [r (pouch/compact-pouch! "claude-burst" {:timeout-ms 3000})]
      (is (true? (:ok r)))
      (is (= "success" (:compact-result r)))
      (is (= 1 (:orphaned-turns r)) "the unsolicited turn's result is consumed, not reported")
      (is (nil? (:error r)))
      (is (nil? (:total-cost-usd r))
          "no earlier running total (BURST results carry none): no price, not the total"))
    ;; alignment survives: the next fed turn reads its own reply
    (is (= "reply:after"
           (:result (pouch/feed-turn! "claude-burst" "after"
                                      {:claude-bin bin :timeout-ms 2000}))))))

(deftest compact-control-is-literal-without-changing-normal-user-lines
  (let [control (json/parse-string (#'pouch/control-line) true)
        normal (json/parse-string (#'pouch/user-line "/compact") true)]
    (is (= "/compact" (get-in control [:message :content])))
    (is (= [{:type "text" :text "/compact"}]
           (get-in normal [:message :content])))))

(deftest compact-pouch-refuses-cold-or-busy-agent
  (is (= {:ok false :error "no warm pouch"}
         (pouch/compact-pouch! "claude-cold" {})))
  (let [bin (fake-claude-bin)]
    (pouch/feed-turn! "claude-busy-compact" "warm"
                      {:claude-bin bin :timeout-ms 2000})
    (let [slow (future
                 (pouch/feed-turn! "claude-busy-compact" "SLOW"
                                   {:claude-bin bin :timeout-ms 5000}))
          deadline (+ (System/currentTimeMillis) 2000)]
      (while (and (not (get-in (pouch/snapshot)
                               ["claude-busy-compact" :in-flight?]))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 10))
      (is (= {:ok false :error "turn in flight"}
             (pouch/compact-pouch! "claude-busy-compact" {})))
      @slow)))

(deftest compact-pouch-wait-option-serializes-behind-held-lock
  (let [bin (fake-claude-bin)]
    (pouch/feed-turn! "claude-wait-compact" "warm"
                      {:claude-bin bin :timeout-ms 2000})
    (let [slow (future
                 (pouch/feed-turn! "claude-wait-compact" "SLOW"
                                   {:claude-bin bin :timeout-ms 5000}))
          deadline (+ (System/currentTimeMillis) 2000)]
      (while (and (not (get-in (pouch/snapshot)
                               ["claude-wait-compact" :in-flight?]))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 10))
      (let [compact (future
                      (pouch/compact-pouch! "claude-wait-compact"
                                           {:wait? true :timeout-ms 3000}))]
        (is (= ::waiting (deref compact 100 ::waiting)))
        (is (= "reply:SLOW" (:result @slow)))
        (is (= "success" (:compact-result @compact)))))))

(defn- error-result-claude-bin
  "Answers 'STALE' the way `claude --resume <missing-id>` does (no assistant
   events, a result with is_error and errors), 'PARTIAL' with text followed by
   an error result, anything else normally."
  []
  (let [f (java.io.File/createTempFile "fake-claude-error-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys\n"
               "sid = 'fake-session-err'\n"
               "for line in sys.stdin:\n"
               "    data = json.loads(line)\n"
               "    content = data.get('message', {}).get('content', '')\n"
               "    text = content if isinstance(content, str) else content[0].get('text', '')\n"
               "    print(json.dumps({'type':'system','subtype':'init','session_id':sid}), flush=True)\n"
               "    if text == 'STALE':\n"
               "        print(json.dumps({'type':'result','subtype':'error_during_execution','is_error':True,'num_turns':0,'session_id':sid,'errors':['No conversation found with session ID: ' + sid]}), flush=True)\n"
               "        continue\n"
               "    print(json.dumps({'type':'assistant','message':{'content':[{'type':'text','text':'reply:' + text}]}}), flush=True)\n"
               "    print(json.dumps({'type':'result','session_id':sid,'is_error':text == 'PARTIAL'}), flush=True)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- check-empty-error-result-throws [agent-id]
  (let [bin (error-result-claude-bin)
        ex (try (pouch/feed-turn! agent-id "STALE" {:claude-bin bin :timeout-ms 5000})
                nil
                (catch Throwable e e))]
    ;; Off the demux path the read runs in a future, so the ex-info arrives
    ;; wrapped in an ExecutionException; dev.clj catches Throwable and logs
    ;; .getMessage, which carries the CLI's error text either way.
    ;; The caller (dev.clj) falls back cold on a throw; the cold path's
    ;; "Exit 1: No conversation found" is what triggers the session reset.
    (is (some? ex) "an empty error result must not read as a successful turn")
    (is (re-find #"No conversation found with session ID: fake-session-err"
                 (str (some-> ex .getMessage))))
    (is (nil? (get (pouch/snapshot) agent-id)) "failed pouch is evicted")
    ;; A turn that produced text is kept even if its result reports an error:
    ;; replaying it cold would repeat whatever it did.
    (is (= "reply:PARTIAL"
           (:result (pouch/feed-turn! agent-id "PARTIAL" {:claude-bin bin :timeout-ms 5000}))))
    (is (= "reply:ok"
           (:result (pouch/feed-turn! agent-id "ok" {:claude-bin bin :timeout-ms 5000}))))))

(deftest empty-error-result-throws-instead-of-placeholder
  (System/clearProperty "FUTON3C_POUCH_DEMUX")
  (check-empty-error-result-throws "claude-stale"))

(deftest empty-error-result-throws-instead-of-placeholder-demux
  (System/setProperty "FUTON3C_POUCH_DEMUX" "true")
  (try
    (check-empty-error-result-throws "claude-stale-demux")
    (finally
      (System/clearProperty "FUTON3C_POUCH_DEMUX"))))
