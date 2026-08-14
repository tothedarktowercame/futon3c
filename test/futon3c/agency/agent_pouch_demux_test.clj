(ns futon3c.agency.agent-pouch-demux-test
  "D6 — the single-READER dual of D3. See holes/excursions/E-unsolicited-pouch-turns.md.

  The pouch takes turns nobody fed it (a background task completing re-invokes
  the agent), and read-turn* returns on the FIRST result it sees, so each such
  turn shifts every later REPL reply one behind. Measured 2026-08-03 on
  claude-11: 23 self-initiated turns against 11 fed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.agent-pouch :as pouch]))

(use-fixtures
  :each
  (fn [f]
    (pouch/clear!)
    (pouch/set-unsolicited-sink! nil)
    (try
      (f)
      (finally
        (pouch/clear!)
        (pouch/set-unsolicited-sink! nil)
        (System/clearProperty "FUTON3C_POUCH_DEMUX")
        (System/clearProperty "FUTON3C_POUCH_MISSING_TRAILER_GRACE_MS")))))

(defn- fake-claude-bin
  "A pouch that has ALREADY taken a turn before anyone feeds it — the
  agent-initiated turn whose result sat unread in the buffer."
  []
  (let [f (java.io.File/createTempFile "fake-claude-demux-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys\n"
               "sid = 'fake-demux-1'\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               ;; unsolicited turn, emitted before any input is read, announced
               ;; by task_notification exactly as a live pouch does
               "emit({'type':'system','subtype':'task_notification'})\n"
               "emit({'type':'system','subtype':'init','session_id':sid})\n"
               "emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'AUTONOMOUS'}]}})\n"
               "emit({'type':'result','session_id':sid,'is_error':False})\n"
               "for line in sys.stdin:\n"
               "    data = json.loads(line)\n"
               "    text = data.get('message', {}).get('content', [{}])[0].get('text', '')\n"
               "    emit({'type':'system','subtype':'init','session_id':sid})\n"
               "    emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'reply:' + text}]}})\n"
               "    emit({'type':'result','session_id':sid,'is_error':False})\n"
               ;; and now a background task completes: a whole extra turn with
               ;; nobody waiting on it. This is the one-behind generator.
               "    emit({'type':'system','subtype':'task_notification'})\n"
               "    emit({'type':'system','subtype':'init','session_id':sid})\n"
               "    emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'AUTONOMOUS-after:' + text}]}})\n"
               "    emit({'type':'result','session_id':sid,'is_error':False})\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- notification-before-input-bin
  "A prior task notification is observed, then the process waits for a real
   operator input. The next init is therefore solicited, not autonomous."
  []
  (let [f (java.io.File/createTempFile "fake-claude-notify-first-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               "emit({'type':'system','subtype':'task_notification'})\n"
               "for line in sys.stdin:\n"
               "    data = json.loads(line)\n"
               "    text = data.get('message', {}).get('content', [{}])[0].get('text', '')\n"
               "    emit({'type':'system','subtype':'init','session_id':'notify-first'})\n"
               "    emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'reply:' + text}]}})\n"
               "    emit({'type':'result','session_id':'notify-first','is_error':False})\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- missing-result-bin
  "Emit a genuine text-only final assistant message but omit `result`, matching
   the live claude-10/14 CLI failure. Keep stdin open so EOF cannot rescue the
   waiter."
  []
  (let [f (java.io.File/createTempFile "fake-claude-missing-result-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys, time\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               "for _line in sys.stdin:\n"
               "    emit({'type':'system','subtype':'init','session_id':'missing-result'})\n"
               "    emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'FINAL-WITHOUT-RESULT'}]}})\n"
               "    time.sleep(60)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- duplicate-init-bin
  "A fresh Claude process may emit two init records for one solicited turn."
  []
  (let [f (java.io.File/createTempFile "fake-claude-double-init-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               "for line in sys.stdin:\n"
               "    text = json.loads(line)['message']['content'][0]['text']\n"
               "    emit({'type':'system','subtype':'init','session_id':'double-init'})\n"
               "    emit({'type':'system','subtype':'init','session_id':'double-init'})\n"
               "    emit({'type':'assistant','message':{'content':[{'type':'text','text':'reply:' + text}]}})\n"
               "    emit({'type':'result','session_id':'double-init','is_error':False})\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- demux-on! [] (System/setProperty "FUTON3C_POUCH_DEMUX" "true"))

(deftest demux-defaults-off
  (System/clearProperty "FUTON3C_POUCH_DEMUX")
  (is (false? (pouch/demux?))
      "load-dark: the OFF path stays the original synchronous read"))

(deftest a-turn-that-finished-before-our-write-is-never-our-answer
  ;; The claude-11 incident: text produced 2m39s before the operator's message
  ;; existed was returned as its answer.
  (demux-on!)
  (let [bin (fake-claude-bin)
        r (pouch/feed-turn! "demux-a" "one" {:claude-bin bin :timeout-ms 5000})]
    (is (= "reply:one" (:result r))
        "must be THIS turn's reply, not the turn the agent took on its own")
    (is (not= "AUTONOMOUS" (:result r)))))

(deftest unsolicited-turns-reach-the-sink-instead-of-being-discarded
  ;; drain-pending! threw the agent's own work away; the operator never saw it.
  (demux-on!)
  (let [seen (atom [])
        bin (fake-claude-bin)]
    (pouch/set-unsolicited-sink!
     (pouch/make-unsolicited-sink #(swap! seen conj %)))
    (pouch/feed-turn! "demux-b" "one" {:claude-bin bin :timeout-ms 5000})
    ;; the sink fires on the reader thread; give it a moment to land
    (let [deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (empty? @seen) (< (System/currentTimeMillis) deadline))
        (Thread/sleep 50)))
    (let [{:keys [agent-id speaker text]} (first @seen)]
      (is (= "demux-b" agent-id))
      (is (= "AUTONOMOUS" text))
      (is (.contains speaker pouch/agent-initiated-marker)
          "the operator sees an unambiguous not-a-reply marker"))))

(deftest a-throwing-unsolicited-sink-cannot-kill-the-demux-reader
  (demux-on!)
  (let [bin (fake-claude-bin)
        attempted (promise)]
    (pouch/set-unsolicited-sink!
     (fn [_aid _turn]
       (deliver attempted true)
       (throw (ex-info "surface unavailable" {}))))
    (let [r (pouch/feed-turn! "demux-throw" "one"
                              {:claude-bin bin :timeout-ms 5000})]
      (is (= "reply:one" (:result r))
          "surface failure cannot affect the solicited result")
      (is (true? (deref attempted 5000 false)))
      (is (= "reply:two"
             (:result (pouch/feed-turn! "demux-throw" "two"
                                        {:claude-bin bin :timeout-ms 5000})))
          "the sole stdout reader survives a throwing surface adapter"))))

(deftest a-blocked-unsolicited-sink-cannot-wedge-the-demux-reader
  (demux-on!)
  (let [bin (fake-claude-bin)
        started (promise)
        release (promise)]
    (pouch/set-unsolicited-sink!
     (fn [_aid _turn]
       (deliver started true)
       @release))
    (try
      (is (= "reply:one"
             (:result (pouch/feed-turn! "demux-block" "one"
                                        {:claude-bin bin :timeout-ms 5000}))))
      (is (true? (deref started 5000 false)))
      (is (= "reply:two"
             (:result (pouch/feed-turn! "demux-block" "two"
                                        {:claude-bin bin :timeout-ms 5000})))
          "a blocked surface worker cannot block the sole stdout reader")
      (finally
        (deliver release true)))))

(deftest sequential-turns-stay-aligned
  (demux-on!)
  (let [bin (fake-claude-bin)
        opts {:claude-bin bin :timeout-ms 5000}
        a (pouch/feed-turn! "demux-c" "one" opts)
        b (pouch/feed-turn! "demux-c" "two" opts)
        c (pouch/feed-turn! "demux-c" "three" opts)]
    (is (= ["reply:one" "reply:two" "reply:three"]
           (mapv :result [a b c]))
        "no one-behind drift across turns on one warm pouch")))

(deftest duplicate-init-does-not-replace-the-open-turn-owner
  ;; Live claude-2 launch greeting, 2026-08-06: the first init polled the
  ;; waiter; the second polled an empty queue and replaced the owner, parking
  ;; the invoke forever despite a healthy idle subprocess.
  (demux-on!)
  (let [events (atom [])
        result (pouch/feed-turn! "demux-double-init" "hello"
                                 {:claude-bin (duplicate-init-bin)
                                  :timeout-ms 5000
                                  :turn-id "double-init-turn"
                                  :on-event #(swap! events conj %)})]
    (is (= "reply:hello" (:result result)))
    (is (= "double-init-turn" (:turn-id result)))
    (is (= 2 (count (filter #(and (= "system" (:type %))
                                  (= "init" (:subtype %)))
                            @events)))
        "both init records stay attributed to the same solicited turn")))

(deftest solicited-and-unsolicited-turn-identities-cannot-cross
  ;; Both directions are load-bearing: an autonomous result must not satisfy a
  ;; waiter, and the waiter's result/events must never reach the autonomous sink.
  (demux-on!)
  (let [bin (fake-claude-bin)
        sink-turns (atom [])
        events (atom [])]
    (pouch/set-unsolicited-sink!
     (fn [_agent-id turn] (swap! sink-turns conj turn)))
    (let [result (pouch/feed-turn! "demux-identity" "one"
                                   {:claude-bin bin
                                    :timeout-ms 5000
                                    :turn-id "queue-turn-1"
                                    :on-event #(swap! events conj %)})
          deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (< (count @sink-turns) 2)
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 25))
      (is (= "reply:one" (:result result))
          "an unsolicited turn never satisfies the solicited waiter")
      (is (= "queue-turn-1" (:turn-id result)))
      (is (seq @events))
      (is (every? #(= "queue-turn-1" (:turn-id %)) @events)
          "every event delivered to the waiter carries its queue turn id")
      (is (every? #(str/starts-with? (:turn-id %) "pouch-autonomous-")
                  @sink-turns)
          "every unsolicited turn has a distinct autonomous identity")
      (is (not-any? #(= "reply:one" (:result %)) @sink-turns)
          "a solicited result is never misrouted to the unsolicited sink"))))

(deftest a-waiter-registered-after-notification-owns-the-next-init
  ;; Live claude-14 failure, 2026-08-05: the answer addressed Joe's exact
  ;; prompt but a stale notification bit routed it to [AGENT-INITIATED],
  ;; leaving the real HTTP waiter open forever with only `started`.
  (demux-on!)
  (let [bin (notification-before-input-bin)
        pouch* (#'pouch/ensure-pouch! "demux-notify-first"
                                      {:claude-bin bin})
        _ (#'pouch/ensure-demux! pouch*)
        notification-seq (get-in pouch* [:demux :notification-seq])
        deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (zero? @notification-seq)
                (< (System/currentTimeMillis) deadline))
      (Thread/sleep 10))
    (is (= 1 @notification-seq)
        "the notification is observed before the operator waiter registers")
    (let [result (pouch/feed-turn! "demux-notify-first" "joe-prompt"
                                   {:claude-bin bin :timeout-ms 5000
                                    :turn-id "joe-turn"})]
      (is (= "reply:joe-prompt" (:result result)))
      (is (= "joe-turn" (:turn-id result))
          "the solicited reply completes its waiter instead of the unsolicited sink"))))

(deftest text-only-quiescence-recovers-a-missing-result-trailer
  ;; Live claude-10 failure, 2026-08-06: the durable transcript and REPL both
  ;; contained the complete final response, but stream-json never emitted its
  ;; `result`, so the HTTP stream could never emit `done`.
  (demux-on!)
  (System/setProperty "FUTON3C_POUCH_MISSING_TRAILER_GRACE_MS" "50")
  (let [events (atom [])
        result (pouch/feed-turn! "demux-missing-result" "one"
                                 {:claude-bin (missing-result-bin)
                                  :timeout-ms 5000
                                  :turn-id "missing-result-turn"
                                  :on-event #(swap! events conj %)})]
    (is (= "FINAL-WITHOUT-RESULT" (:result result)))
    (is (true? (:pouch/trailer-inferred? result))
        "the caller can audit that done was inferred rather than runner-emitted")
    (is (= "missing-result-turn" (:turn-id result)))
    (is (= "missing_trailer_inferred"
           (:subtype (last (filter #(= "result" (:type %)) @events))))
        "the original event stream receives an explicit terminal tripwire")
    (is (nil? (get (pouch/snapshot) "demux-missing-result"))
        "the boundary-untrustworthy pouch is recycled, not reused or cold-replayed")))

(deftest off-path-still-serves-turns
  ;; Regression guard on the byte-for-byte OFF path. Deliberately no assertion
  ;; about WHICH turn it returns: with the synchronous reader that is exactly
  ;; the race this flag exists to remove, and pinning it would enshrine the bug.
  (System/clearProperty "FUTON3C_POUCH_DEMUX")
  (let [sink-calls (atom 0)
        _ (pouch/set-unsolicited-sink! (fn [_ _] (swap! sink-calls inc)))
        bin (fake-claude-bin)
        r (pouch/feed-turn! "demux-d" "one" {:claude-bin bin :timeout-ms 5000})]
    (is (string? (:result r)))
    (is (true? (:pouch/warm? r)))
    (is (zero? @sink-calls)
        "OFF uses the original synchronous reader and never enters D6 routing")))

(deftest closed-stdout-fails-the-waiter-rather-than-hanging-it
  (demux-on!)
  (let [f (java.io.File/createTempFile "fake-claude-die-" ".py")]
    (spit f (str "#!/usr/bin/env python3\n" "import sys\n" "sys.exit(0)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (is (thrown? Exception
                 (pouch/feed-turn! "demux-e" "one"
                                   {:claude-bin (.getAbsolutePath f)
                                    :timeout-ms 5000}))
        "a dead pouch surfaces an error; it must not park the caller until timeout")))

(deftest eviction-fails-an-already-registered-waiter
  (demux-on!)
  (let [f (java.io.File/createTempFile "fake-claude-wait-" ".py")]
    (spit f (str "#!/usr/bin/env python3\n"
                 "import sys, time\n"
                 "for _line in sys.stdin:\n"
                 "    time.sleep(60)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (let [outcome (future
                    (try
                      (pouch/feed-turn! "demux-evict" "one"
                                        {:claude-bin (.getAbsolutePath f)
                                         :timeout-ms 30000})
                      :unexpected-success
                      (catch Throwable _ :failed-explicitly)))
          pouches (var-get (ns-resolve 'futon3c.agency.agent-pouch '!pouches))
          waiter-count #(or (some-> @pouches (get "demux-evict") :demux
                                    :waiters .size)
                            0)
          deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (zero? (waiter-count))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 10))
      (is (pos? (waiter-count))
          "the test forces the load-bearing registered-waiter branch")
      (pouch/evict! "demux-evict")
      (is (= :failed-explicitly (deref outcome 2000 :still-hung))
          "eviction completes the waiter with an error instead of orphaning it"))))

(deftest eviction-fails-the-current-owner-after-init-polled-it
  (demux-on!)
  (let [f (java.io.File/createTempFile "fake-claude-owner-wait-" ".py")]
    (spit f (str "#!/usr/bin/env python3\n"
                 "import json, sys, time\n"
                 "def emit(o): print(json.dumps(o), flush=True)\n"
                 "for _line in sys.stdin:\n"
                 "    emit({'type':'system','subtype':'init','session_id':'owner-wait'})\n"
                 "    time.sleep(60)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (let [outcome (future
                    (try
                      (pouch/feed-turn! "demux-owner-evict" "one"
                                        {:claude-bin (.getAbsolutePath f)
                                         :timeout-ms 30000})
                      :unexpected-success
                      (catch Throwable _ :failed-explicitly)))
          pouches (var-get (ns-resolve 'futon3c.agency.agent-pouch '!pouches))
          owner #(some-> @pouches (get "demux-owner-evict") :demux :owner deref)
          deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (nil? (owner)) (< (System/currentTimeMillis) deadline))
        (Thread/sleep 10))
      (is (some? (owner))
          "the test forces the owner-removed-from-queue shutdown branch")
      (is (zero? (some-> @pouches (get "demux-owner-evict") :demux :waiters .size))
          "the current owner is no longer present in the queued waiters")
      (pouch/evict! "demux-owner-evict")
      (is (= :failed-explicitly (deref outcome 2000 :still-hung))
          "eviction fails the current owner instead of parking it forever"))))
