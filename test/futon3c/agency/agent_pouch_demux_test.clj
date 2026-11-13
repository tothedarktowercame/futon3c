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
               "    emit({'type':'rate_limit_event','rate_limit_info':{'status':'allowed'}})\n"
               "    time.sleep(60)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn- autonomous-missing-result-bin
  "Announce an autonomous turn and emit its final text without `result`.
   A real operator waiter is already registered behind it."
  []
  (let [f (java.io.File/createTempFile "fake-claude-auto-missing-result-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys, time\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               "emit({'type':'system','subtype':'task_notification'})\n"
               "emit({'type':'system','subtype':'init','session_id':'auto-missing-result'})\n"
               "emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'AUTONOMOUS-FINAL-WITHOUT-RESULT'}]}})\n"
               "emit({'type':'rate_limit_event','rate_limit_info':{'status':'allowed'}})\n"
               "time.sleep(60)\n"))
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

(defn- open-autonomous-turn-bin
  "Open an autonomous turn, then detect whether stdin arrives before result.
   Returns [executable order-file]. An early write recreates Claude Code's
   queued_command absorption and deliberately emits no second turn."
  []
  (let [order-file (java.io.File/createTempFile "pouch-open-turn-order-" ".txt")
        f (java.io.File/createTempFile "fake-claude-open-turn-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, select, sys, time\n"
               "order = " (pr-str (.getAbsolutePath order-file)) "\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               "def note(s):\n"
               "    with open(order, 'a') as out: out.write(s + '\\n')\n"
               "emit({'type':'system','subtype':'task_notification'})\n"
               "emit({'type':'system','subtype':'init','session_id':'open-auto'})\n"
               "emit({'type':'assistant','message':{'content':[{'type':'text','text':'AUTONOMOUS-OPEN'}]}})\n"
               "ready, _, _ = select.select([sys.stdin], [], [], 0.5)\n"
               "early = sys.stdin.readline() if ready else None\n"
               "note('INPUT-EARLY' if early else 'RESULT-ONE')\n"
               "emit({'type':'result','session_id':'open-auto','is_error':False})\n"
               "if not early:\n"
               "    line = sys.stdin.readline()\n"
               "    note('INPUT-AFTER')\n"
               "    text = json.loads(line)['message']['content'][0]['text']\n"
               "    emit({'type':'system','subtype':'init','session_id':'open-auto'})\n"
               "    emit({'type':'assistant','message':{'content':[{'type':'text','text':'reply:' + text}]}})\n"
               "    emit({'type':'result','session_id':'open-auto','is_error':False})\n"
               "time.sleep(1)\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.deleteOnExit order-file)
    [(.getAbsolutePath f) order-file]))

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

(deftest operator-write-waits-for-an-open-autonomous-turn
  ;; 2026-11-09 claude-6 incident: :in-flight? was false because the open turn
  ;; had no waiter, so Joe's user event became that turn's queued_command.
  (demux-on!)
  (let [[bin order-file] (open-autonomous-turn-bin)
        sink-turns (atom [])
        pouch* (#'pouch/ensure-pouch! "demux-open-autonomous" {:claude-bin bin})
        _ (#'pouch/ensure-demux! pouch*)
        open? (get-in pouch* [:demux :open?])
        deadline (+ (System/currentTimeMillis) 5000)]
    (pouch/set-unsolicited-sink!
     (fn [_agent-id turn] (swap! sink-turns conj turn)))
    (while (and (not @open?) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 10))
    (is @open? "the autonomous protocol turn is open before Joe writes")
    (let [result (pouch/feed-turn! "demux-open-autonomous" "joe-prompt"
                                   {:claude-bin bin :timeout-ms 5000
                                    :turn-id "joe-open-turn"})
          sink-deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (empty? @sink-turns)
                  (< (System/currentTimeMillis) sink-deadline))
        (Thread/sleep 10))
      (is (= ["RESULT-ONE" "INPUT-AFTER"]
             (str/split-lines (slurp order-file)))
          "stdin is written only after the autonomous result")
      (is (= "reply:joe-prompt" (:result result)))
      (is (= "joe-open-turn" (:turn-id result))
          "the fresh init binds Joe's already-registered waiter")
      (is (= "AUTONOMOUS-OPEN" (:result (first @sink-turns)))
          "the first turn remains unsolicited"))))

(deftest removing-the-open-turn-hold-recreates-the-stranded-reply
  ;; Construct the named bad case against the real dependency. With the one
  ;; guard removed, stdin arrives before result and the fake CLI absorbs it.
  (demux-on!)
  (let [[bin order-file] (open-autonomous-turn-bin)
        pouch* (#'pouch/ensure-pouch! "demux-open-autonomous-bad" {:claude-bin bin})
        _ (#'pouch/ensure-demux! pouch*)
        open? (get-in pouch* [:demux :open?])
        deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (not @open?) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 10))
    (is @open?)
    (with-redefs-fn {#'pouch/await-demux-idle! (fn [_demux _timeout] true)}
      #(is (thrown? java.util.concurrent.TimeoutException
                    (pouch/feed-turn! "demux-open-autonomous-bad" "joe-prompt"
                                      {:claude-bin bin :timeout-ms 800}))))
    (is (= "INPUT-EARLY" (str/trim (slurp order-file)))
        "without the hold, the user event is consumed inside the open turn")))

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

(deftest autonomous-text-only-quiescence-unblocks-the-operator-waiter
  ;; Live claude-9/13 failure, 2026-11-13: an autonomous/parked-resume turn
  ;; wrote its complete final answer but omitted `result`. The existing tripwire
  ;; required a non-nil owner, so it never closed the autonomous protocol turn;
  ;; the already-registered operator waiter remained queued forever.
  (demux-on!)
  (System/setProperty "FUTON3C_POUCH_MISSING_TRAILER_GRACE_MS" "50")
  (let [seen (promise)
        bin (autonomous-missing-result-bin)]
    (pouch/set-unsolicited-sink!
     (fn [_ turn] (deliver seen turn)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"pouch was evicted while a turn was waiting"
         (pouch/feed-turn! "demux-autonomous-missing-result" "operator-turn"
                           {:claude-bin bin :timeout-ms 5000
                            :turn-id "operator-turn"})))
    (is (= "AUTONOMOUS-FINAL-WITHOUT-RESULT"
           (:result (deref seen 5000 nil)))
        "the completed autonomous answer is delivered before recycling")
    (is (nil? (get (pouch/snapshot) "demux-autonomous-missing-result"))
        "the poisoned pouch is gone so the invoke layer can retry cold")))

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

(defn- notification-then-compaction-bin
  "Replays claude-6, 2026-11-08 05:00-05:02: after /compact is written, a
   background task notifies and takes a turn of its own, another notification
   lands, and only then does the compaction run. Ordinary user lines get an
   ordinary reply."
  []
  (let [f (java.io.File/createTempFile "fake-claude-notify-compact-" ".py")]
    (spit f
          (str "#!/usr/bin/env python3\n"
               "import json, sys\n"
               "sid = 'notify-compact'\n"
               "def emit(o): print(json.dumps(o), flush=True)\n"
               "for line in sys.stdin:\n"
               "    content = json.loads(line).get('message', {}).get('content')\n"
               "    if content == '/compact':\n"
               "        emit({'type':'system','subtype':'task_notification'})\n"
               "        emit({'type':'system','subtype':'init','session_id':sid})\n"
               "        emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'AUTONOMOUS'}]}})\n"
               "        emit({'type':'result','session_id':sid,'is_error':False})\n"
               "        emit({'type':'system','subtype':'task_notification'})\n"
               "        emit({'type':'system','subtype':'status','status':'compacting','session_id':sid})\n"
               "        emit({'type':'system','subtype':'status','status':None,"
               "'compact_result':'success','session_id':sid})\n"
               "        emit({'type':'system','subtype':'init','session_id':sid})\n"
               "        emit({'type':'system','subtype':'compact_boundary','session_id':sid})\n"
               "        emit({'type':'result','session_id':sid,'num_turns':0,'is_error':False})\n"
               "        continue\n"
               "    text = content[0]['text']\n"
               "    emit({'type':'system','subtype':'init','session_id':sid})\n"
               "    emit({'type':'assistant','message':{'content':"
               "[{'type':'text','text':'reply:' + text}]}})\n"
               "    emit({'type':'result','session_id':sid,'is_error':False})\n"))
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest a-compaction-after-a-notification-reaches-its-waiter
  ;; claude-6, 2026-11-08: a notification observed after the compact waiter
  ;; registered filed the compaction as agent-initiated; the waiter then lost
  ;; every later turn and held the seat's turn queue until its timeout.
  (demux-on!)
  (let [bin (notification-then-compaction-bin)
        sink-turns (atom [])]
    (pouch/set-unsolicited-sink! (fn [_ turn] (swap! sink-turns conj turn)))
    (is (= "reply:warm" (:result (pouch/feed-turn! "demux-notify-compact" "warm"
                                                   {:claude-bin bin :timeout-ms 5000}))))
    (let [r (pouch/compact-pouch! "demux-notify-compact" {:timeout-ms 5000})]
      (is (true? (:ok r)) "the compaction's own turn completes the compact waiter")
      (is (= "success" (:compact-result r))))
    (is (= ["AUTONOMOUS"] (mapv :result @sink-turns))
        "the background task's turn still goes to the unsolicited sink, and only it")
    (is (= "reply:after" (:result (pouch/feed-turn! "demux-notify-compact" "after"
                                                    {:claude-bin bin :timeout-ms 5000})))
        "the next fed turn reads its own reply")))
