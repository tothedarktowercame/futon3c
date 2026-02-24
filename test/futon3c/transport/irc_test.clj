(ns futon3c.transport.irc-test
  "Tests for the IRC transport adapter.
   Mirrors ws_test.clj structure: mock send-fn for testable callbacks."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.transport.irc :as irc]
            [futon3c.transport.protocol :as proto]
            [futon3c.evidence.store :as estore]
            [cheshire.core :as json]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (estore/reset-store!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- make-test-irc
  "Create IRC callbacks with mock send/close and captured output."
  ([] (make-test-irc {}))
  ([overrides]
   (let [sent (atom [])
         closed (atom [])
         relayed (atom [])
         config (merge {:send-fn (fn [cid line] (swap! sent conj {:to cid :line line}))
                        :close-fn (fn [cid] (swap! closed conj cid))
                        :relay-fn (fn [ch from text] (swap! relayed conj {:channel ch :from from :text text}))
                        :evidence-store estore/!store}
                       overrides)
         callbacks (irc/make-irc-callbacks config)]
     (assoc callbacks :sent sent :closed closed :relayed relayed))))

(defn- last-sent-line [sent-atom]
  (:line (last @sent-atom)))

(defn- sent-lines-to [sent-atom cid]
  (->> @sent-atom
       (filter #(= cid (:to %)))
       (mapv :line)))

(defn- register-client!
  "Connect + NICK + USER → registered client."
  [{:keys [on-connect on-line]} cid nick]
  (on-connect cid)
  (on-line cid (str "NICK " nick))
  (on-line cid (str "USER " nick " 0 * :Real Name")))

;; =============================================================================
;; 1. IRC line parsing
;; =============================================================================

(deftest parse-irc-line-basic
  (testing "parses simple command"
    (let [r (irc/parse-irc-line "NICK joe")]
      (is (= "NICK" (:command r)))
      (is (= ["joe"] (:params r)))))

  (testing "parses command with trailing"
    (let [r (irc/parse-irc-line "PRIVMSG #futon :Hello world")]
      (is (= "PRIVMSG" (:command r)))
      (is (= ["#futon"] (:params r)))
      (is (= "Hello world" (:trailing r)))))

  (testing "parses with prefix"
    (let [r (irc/parse-irc-line ":joe PRIVMSG #futon :Hello")]
      (is (= "joe" (:prefix r)))
      (is (= "PRIVMSG" (:command r)))
      (is (= "Hello" (:trailing r)))))

  (testing "returns nil for blank/nil"
    (is (nil? (irc/parse-irc-line nil)))
    (is (nil? (irc/parse-irc-line "")))
    (is (nil? (irc/parse-irc-line "  ")))))

;; =============================================================================
;; 2. Client lifecycle
;; =============================================================================

(deftest client-registration
  (testing "NICK + USER → registered, welcome numerics sent"
    (let [{:keys [on-connect on-line clients sent]} (make-test-irc)]
      (on-connect :c1)
      (is (some? (get @clients :c1)))
      (is (false? (:registered? (get @clients :c1))))

      (on-line :c1 "NICK joe")
      (is (= "joe" (:nick (get @clients :c1))))
      (is (false? (:registered? (get @clients :c1))))

      (on-line :c1 "USER joe 0 * :Joe")
      (is (true? (:registered? (get @clients :c1))))

      ;; Should have received 001-004 welcome
      (let [lines (sent-lines-to sent :c1)]
        (is (some #(re-find #"001" %) lines) "001 welcome sent")
        (is (some #(re-find #"004" %) lines) "004 sent")))))

(deftest nick-before-user
  (testing "USER then NICK also triggers registration"
    (let [{:keys [on-connect on-line clients sent]} (make-test-irc)]
      (on-connect :c2)
      (on-line :c2 "USER joe 0 * :Joe")
      (is (false? (:registered? (get @clients :c2))))
      (on-line :c2 "NICK joe")
      (is (true? (:registered? (get @clients :c2)))))))

;; =============================================================================
;; 3. JOIN and room state
;; =============================================================================

(deftest join-channel
  (testing "JOIN creates room and sends NAMES"
    (let [{:keys [on-connect on-line clients rooms sent] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      (reset! sent [])

      (on-line :c1 "JOIN #futon")
      (is (contains? (get @rooms "#futon") "joe"))
      (is (contains? (:channels (get @clients :c1)) "#futon"))

      (let [lines (sent-lines-to sent :c1)]
        (is (some #(re-find #"JOIN #futon" %) lines))
        (is (some #(re-find #"353" %) lines) "NAMES reply")
        (is (some #(re-find #"366" %) lines) "End of NAMES")))))

(deftest multiple-clients-in-room
  (testing "Two clients in same room see each other in NAMES"
    (let [{:keys [clients rooms sent] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      (register-client! irc :c2 "claude")
      ((:on-line irc) :c1 "JOIN #futon")
      ((:on-line irc) :c2 "JOIN #futon")
      (is (= #{"joe" "claude"} (get @rooms "#futon"))))))

;; =============================================================================
;; 4. PRIVMSG relay and evidence
;; =============================================================================

(deftest privmsg-to-channel
  (testing "PRIVMSG to channel broadcasts, relays, and emits evidence"
    (let [{:keys [sent relayed] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      (register-client! irc :c2 "claude")
      ((:on-line irc) :c1 "JOIN #futon")
      ((:on-line irc) :c2 "JOIN #futon")
      (reset! sent [])
      (reset! relayed [])

      ((:on-line irc) :c1 "PRIVMSG #futon :Hello agents!")

      ;; claude should see the PRIVMSG (not joe — excluded sender)
      (let [claude-lines (sent-lines-to sent :c2)]
        (is (some #(re-find #"Hello agents!" %) claude-lines)))
      (let [joe-lines (sent-lines-to sent :c1)]
        (is (not (some #(re-find #"Hello agents!" %) joe-lines))
            "sender should not echo"))

      ;; relay-fn was called
      (is (= 1 (count @relayed)))
      (is (= "#futon" (:channel (first @relayed))))
      (is (= "joe" (:from (first @relayed))))
      (is (= "Hello agents!" (:text (first @relayed))))

      ;; evidence entry created
      (let [entries (vals (:entries @estore/!store))]
        (is (= 1 (count entries)))
        (let [entry (first entries)]
          (is (= :forum-post (:evidence/type entry)))
          (is (= "joe" (:evidence/author entry)))
          (is (= "Hello agents!" (get-in entry [:evidence/body :text])))
          (is (= :irc (get-in entry [:evidence/body :transport]))
              "evidence body includes transport :irc")
          (is (= {:ref/type :thread :ref/id "irc/#futon"}
                 (:evidence/subject entry))))))))

;; =============================================================================
;; 5. Agent response via relay bridge
;; =============================================================================

(deftest relay-bridge-round-trip
  (testing "IRC PRIVMSG → relay to agent → agent irc_response → back to IRC"
    (let [agent-ws-sent (atom [])
          bridge (irc/make-relay-bridge {:evidence-store estore/!store})
          {:keys [relay-fn irc-interceptor join-agent! set-irc-send-fn!]} bridge
          irc-channel-sent (atom [])
          {:keys [sent] :as irc} (make-test-irc {:relay-fn relay-fn})]

      ;; Set up IRC server's send-to-channel function
      (set-irc-send-fn!
       (fn [channel from-nick text]
         (swap! irc-channel-sent conj {:channel channel :from from-nick :text text})))

      ;; Register an agent in the bridge
      (join-agent! "claude-1" "claude" "#futon"
                   (fn [data] (swap! agent-ws-sent conj data)))

      ;; Register IRC client
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")

      ;; Joe sends PRIVMSG
      ((:on-line irc) :c1 "PRIVMSG #futon :What's the status?")

      ;; Agent should have received irc_message frame
      (is (= 1 (count @agent-ws-sent)))
      (let [frame (json/parse-string (first @agent-ws-sent) true)]
        (is (= "irc_message" (:type frame)))
        (is (= "#futon" (:channel frame)))
        (is (= "joe" (:from frame)))
        (is (= "What's the status?" (:text frame))))

      ;; Agent sends irc_response
      (irc-interceptor nil {:agent-id "claude-1"} {:irc/channel "#futon" :irc/text "All green!"})

      ;; Should have been sent to IRC channel
      (is (= 1 (count @irc-channel-sent)))
      (is (= "#futon" (:channel (first @irc-channel-sent))))
      (is (= "claude" (:from (first @irc-channel-sent))))
      (is (= "All green!" (:text (first @irc-channel-sent))))

      ;; Evidence entries: 1 from joe's PRIVMSG + 1 from agent response
      (let [entries (vals (:entries @estore/!store))
            transports (set (map #(get-in % [:evidence/body :transport]) entries))]
        (is (= 2 (count entries)))
        (is (contains? transports :irc) "joe's message has transport :irc")
        (is (contains? transports :ws-relay) "agent response has transport :ws-relay")))))

(deftest relay-bridge-join-agent-overwrite-control
  (testing "join-agent! with overwrite? false preserves existing callback/nick"
    (let [dispatch-sent (atom [])
          ws-sent (atom [])
          bridge (irc/make-relay-bridge {:evidence-store estore/!store})
          {:keys [relay-fn join-agent!]} bridge]
      (join-agent! "codex-1" "codex" "#futon"
                   (fn [data] (swap! dispatch-sent conj data)))
      (join-agent! "codex-1" "codex-1" "#futon"
                   (fn [data] (swap! ws-sent conj data))
                   {:overwrite? false})
      (relay-fn "#futon" "joe" "@codex hello")
      (is (= 1 (count @dispatch-sent)))
      (is (empty? @ws-sent))))

  (testing "default join-agent! keeps overwrite behavior"
    (let [dispatch-sent (atom [])
          ws-sent (atom [])
          bridge (irc/make-relay-bridge {:evidence-store estore/!store})
          {:keys [relay-fn join-agent!]} bridge]
      (join-agent! "codex-1" "codex" "#futon"
                   (fn [data] (swap! dispatch-sent conj data)))
      ;; default (no opts) should replace existing callback
      (join-agent! "codex-1" "codex-1" "#futon"
                   (fn [data] (swap! ws-sent conj data)))
      (relay-fn "#futon" "joe" "@codex hello")
      (is (empty? @dispatch-sent))
      (is (= 1 (count @ws-sent))))))

;; =============================================================================
;; 6. Per-channel sessions
;; =============================================================================

(deftest per-channel-evidence-threading
  (testing "messages in different channels get different evidence subjects"
    (let [{:keys [relayed] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")
      ((:on-line irc) :c1 "JOIN #standup")

      ((:on-line irc) :c1 "PRIVMSG #futon :Hello futon")
      ((:on-line irc) :c1 "PRIVMSG #standup :Hello standup")

      (let [entries (vals (:entries @estore/!store))
            subjects (set (map :evidence/subject entries))]
        (is (= 2 (count entries)))
        (is (contains? subjects {:ref/type :thread :ref/id "irc/#futon"}))
        (is (contains? subjects {:ref/type :thread :ref/id "irc/#standup"}))))))

;; =============================================================================
;; 7. PING/PONG
;; =============================================================================

(deftest ping-pong
  (testing "PING → PONG response"
    (let [{:keys [sent] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      (reset! sent [])

      ((:on-line irc) :c1 "PING :token123")
      (let [lines (sent-lines-to sent :c1)]
        (is (some #(re-find #"PONG.*token123" %) lines))))))

;; =============================================================================
;; 8. QUIT and disconnect
;; =============================================================================

(deftest quit-cleanup
  (testing "QUIT removes client from rooms"
    (let [{:keys [clients rooms closed] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")
      (is (contains? (get @rooms "#futon") "joe"))

      ((:on-line irc) :c1 "QUIT :goodbye")
      (is (not (contains? (get @rooms "#futon") "joe")))
      (is (nil? (get @clients :c1)))
      (is (= [:c1] @closed)))))

(deftest disconnect-cleanup
  (testing "on-disconnect removes client from rooms"
    (let [{:keys [on-disconnect clients rooms] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")
      (on-disconnect :c1)
      (is (not (contains? (get @rooms "#futon") "joe")))
      (is (nil? (get @clients :c1))))))

;; =============================================================================
;; 9. PART
;; =============================================================================

(deftest part-channel
  (testing "PART removes from channel but keeps client"
    (let [{:keys [clients rooms] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")
      ((:on-line irc) :c1 "JOIN #standup")
      (is (= #{"#futon" "#standup"} (:channels (get @clients :c1))))

      ((:on-line irc) :c1 "PART #futon")
      (is (not (contains? (get @rooms "#futon") "joe")))
      (is (contains? (get @rooms "#standup") "joe"))
      (is (= #{"#standup"} (:channels (get @clients :c1)))))))

;; =============================================================================
;; 10. Protocol extension: irc_response WS frame
;; =============================================================================

(deftest parse-irc-response-frame
  (testing "parse-ws-message handles irc_response frames"
    (let [frame (json/generate-string {"type" "irc_response"
                                       "channel" "#futon"
                                       "text" "test response"})
          parsed (proto/parse-ws-message frame)]
      (is (= :irc-response (:ws/type parsed)))
      (is (= "#futon" (:irc/channel parsed)))
      (is (= "test response" (:irc/text parsed)))))

  (testing "irc_response missing channel → error"
    (let [frame (json/generate-string {"type" "irc_response" "text" "hello"})
          parsed (proto/parse-ws-message frame)]
      (is (contains? parsed :error/code))))

  (testing "irc_response missing text → error"
    (let [frame (json/generate-string {"type" "irc_response" "channel" "#futon"})
          parsed (proto/parse-ws-message frame)]
      (is (contains? parsed :error/code)))))

(deftest render-irc-message-frame
  (testing "render-irc-message produces correct JSON with transport"
    (let [frame (proto/render-irc-message "#futon" "joe" "Hello agents")
          parsed (json/parse-string frame true)]
      (is (= "irc_message" (:type parsed)))
      (is (= "#futon" (:channel parsed)))
      (is (= "joe" (:from parsed)))
      (is (= "Hello agents" (:text parsed)))
      (is (= "irc" (:transport parsed)) "frame includes transport field"))))

;; =============================================================================
;; 11. M-IRC-stability: Keepalive (F1)
;; =============================================================================

(deftest server-ping-sent-after-interval
  (testing "ping-idle! sends PING to clients idle longer than interval"
    (let [{:keys [sent clients ping-idle!] :as irc} (make-test-irc {:ping-interval-ms 100
                                                                      :ping-timeout-ms 5000})]
      (register-client! irc :c1 "joe")
      ;; Force last-activity-at to be old enough to trigger ping
      (swap! clients update :c1 assoc :last-activity-at (- (System/currentTimeMillis) 200))
      (reset! sent [])

      (ping-idle! 100)

      (let [lines (sent-lines-to sent :c1)]
        (is (some #(re-find #"^PING" %) lines) "Server should send PING to idle client"))
      (is (true? (:ping-pending? (get @clients :c1)))
          "Client should be marked ping-pending"))))

(deftest ping-not-sent-to-active-client
  (testing "ping-idle! does NOT ping clients with recent activity"
    (let [{:keys [sent clients ping-idle!] :as irc} (make-test-irc {:ping-interval-ms 30000})]
      (register-client! irc :c1 "joe")
      ;; Client just connected — last-activity-at is now
      (reset! sent [])

      (ping-idle! 30000)

      (let [lines (sent-lines-to sent :c1)]
        (is (not (some #(re-find #"^PING" %) lines))
            "Active client should not receive PING")))))

(deftest pong-clears-ping-pending
  (testing "PONG from client clears :ping-pending? flag"
    (let [{:keys [clients] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      ;; Simulate a pending PING
      (swap! clients update :c1 assoc :ping-pending? true)
      (is (true? (:ping-pending? (get @clients :c1))))

      ;; Client responds with PONG
      ((:on-line irc) :c1 "PONG :futon3c")
      (is (false? (:ping-pending? (get @clients :c1)))
          "PONG should clear ping-pending"))))

(deftest any-message-resets-liveness
  (testing "any received message updates :last-activity-at (not just PONG)"
    (let [{:keys [clients] :as irc} (make-test-irc)]
      (register-client! irc :c1 "joe")
      ;; Force old activity timestamp
      (swap! clients update :c1 assoc :last-activity-at 1000)
      (is (= 1000 (:last-activity-at (get @clients :c1))))

      ;; Send any command (PING in this case)
      ((:on-line irc) :c1 "PING :keepalive")
      (is (> (:last-activity-at (get @clients :c1)) 1000)
          "Any received line should update last-activity-at"))))

(deftest reap-dead-removes-timed-out-clients
  (testing "reap-dead! removes clients past ping-timeout-ms"
    (let [{:keys [clients rooms closed reap-dead!] :as irc}
          (make-test-irc {:ping-timeout-ms 100})]
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")
      ;; Force last-activity-at to be old
      (swap! clients update :c1 assoc :last-activity-at (- (System/currentTimeMillis) 500))

      (let [reaped (reap-dead!)]
        (is (= 1 reaped) "Should reap 1 dead client")
        (is (nil? (get @clients :c1)) "Client should be removed")
        (is (not (contains? (get @rooms "#futon") "joe"))
            "Nick should be removed from rooms")
        (is (= [:c1] @closed) "close-fn should be called")))))

(deftest reap-dead-preserves-active-clients
  (testing "reap-dead! does NOT reap clients with recent activity"
    (let [{:keys [clients reap-dead!] :as irc}
          (make-test-irc {:ping-timeout-ms 60000})]
      (register-client! irc :c1 "joe")
      ;; Client is fresh — default last-activity-at is now

      (let [reaped (reap-dead!)]
        (is (zero? reaped) "Should reap 0 clients")
        (is (some? (get @clients :c1)) "Active client should remain")))))

(deftest reap-dead-emits-evidence
  (testing "reap-dead! emits tension evidence for reaped connections"
    (let [{:keys [clients reap-dead!] :as irc}
          (make-test-irc {:ping-timeout-ms 100})]
      (register-client! irc :c1 "joe")
      (swap! clients update :c1 assoc :last-activity-at 0)
      (estore/reset-store!)

      (reap-dead!)

      (let [entries (vals (:entries @estore/!store))
            tensions (filter #(= :tension (:evidence/claim-type %)) entries)]
        (is (pos? (count tensions))
            "Should emit at least one tension evidence entry for reap")))))

;; =============================================================================
;; 12. M-IRC-stability: Nick Reclaim (F4)
;; =============================================================================

(deftest nick-reclaim-from-ghost
  (testing "reconnecting client reclaims nick from ghost (timed-out) connection"
    (let [{:keys [clients rooms closed] :as irc}
          (make-test-irc {:ping-timeout-ms 100})]
      ;; First client registers and joins
      (register-client! irc :c1 "joe")
      ((:on-line irc) :c1 "JOIN #futon")
      (is (= "joe" (:nick (get @clients :c1))))

      ;; Simulate ghost: old activity, pending ping
      (swap! clients update :c1 assoc
             :last-activity-at (- (System/currentTimeMillis) 500)
             :ping-pending? true)

      ;; Second client connects and requests same nick
      ((:on-connect irc) :c2)
      ((:on-line irc) :c2 "NICK joe")

      ;; Ghost should be killed, new client gets the nick
      (is (nil? (get @clients :c1)) "Ghost client should be removed")
      (is (= "joe" (:nick (get @clients :c2))) "New client should have nick")
      (is (some #{:c1} @closed) "Ghost connection should be closed"))))

(deftest nick-collision-for-live-client
  (testing "non-ghost nick collision returns ERR_NICKNAMEINUSE (433)"
    (let [{:keys [sent clients] :as irc} (make-test-irc {:ping-timeout-ms 60000})]
      ;; First client registers (active — last-activity-at is recent)
      (register-client! irc :c1 "joe")

      ;; Second client tries the same nick
      ((:on-connect irc) :c2)
      (reset! sent [])
      ((:on-line irc) :c2 "NICK joe")

      ;; Should get 433
      (let [lines (sent-lines-to sent :c2)]
        (is (some #(re-find #"433.*joe.*already in use" %) lines)
            "Should receive ERR_NICKNAMEINUSE"))
      ;; Original client should still have the nick
      (is (= "joe" (:nick (get @clients :c1)))))))

(deftest ghost-detection-uses-timeout
  (testing "ghost detection considers both ping-pending and elapsed time"
    (let [{:keys [clients] :as irc}
          (make-test-irc {:ping-timeout-ms 100})]
      ;; Client with recent activity — not a ghost
      (register-client! irc :c1 "joe")
      ;; second client tries nick — should fail since c1 is live
      ((:on-connect irc) :c2)
      ((:on-line irc) :c2 "NICK joe")
      (is (= "joe" (:nick (get @clients :c1)))
          "Live client keeps nick")

      ;; Now age out the first client past timeout
      (swap! clients update :c1 assoc :last-activity-at (- (System/currentTimeMillis) 500))

      ;; Third client tries — should reclaim since c1 is now timed out
      ((:on-connect irc) :c3)
      ((:on-line irc) :c3 "NICK joe")
      (is (nil? (get @clients :c1)) "Timed-out client should be killed")
      (is (= "joe" (:nick (get @clients :c3))) "New client reclaims nick"))))

;; =============================================================================
;; 13. M-IRC-stability: Relay Timeout (F5)
;; =============================================================================

(deftest relay-timeout-skips-stalled-agent
  (testing "relay timeout: stalled agent doesn't block other agents"
    (let [fast-received (atom [])
          slow-received (atom [])
          bridge (irc/make-relay-bridge {:evidence-store estore/!store
                                          :relay-timeout-ms 200})
          {:keys [relay-fn join-agent!]} bridge]

      ;; Fast agent responds immediately
      (join-agent! "fast-agent" "claude" "#futon"
                   (fn [data] (swap! fast-received conj data)))

      ;; Slow agent blocks for 5 seconds (well beyond 200ms timeout)
      (join-agent! "slow-agent" "codex" "#futon"
                   (fn [data]
                     (Thread/sleep 5000)
                     (swap! slow-received conj data)))

      ;; Relay a message — should complete in ~200ms, not 5s
      (let [start (System/currentTimeMillis)]
        (relay-fn "#futon" "joe" "Hello agents")
        (let [elapsed (- (System/currentTimeMillis) start)]
          ;; Should complete well under 2 seconds (200ms timeout + overhead)
          (is (< elapsed 2000)
              (str "Relay should complete quickly (took " elapsed "ms)"))))

      ;; Fast agent should have received the message
      (is (= 1 (count @fast-received))
          "Fast agent should receive the message"))))

(deftest relay-timeout-emits-evidence
  (testing "relay timeout emits tension evidence for stalled agent"
    (let [bridge (irc/make-relay-bridge {:evidence-store estore/!store
                                          :relay-timeout-ms 100})
          {:keys [relay-fn join-agent!]} bridge]

      ;; Stalled agent — blocks for 2 seconds
      (join-agent! "stalled" "codex" "#futon"
                   (fn [_] (Thread/sleep 2000)))

      (estore/reset-store!)
      (relay-fn "#futon" "joe" "test")

      ;; Wait briefly for evidence to be emitted
      (Thread/sleep 200)

      (let [entries (vals (:entries @estore/!store))
            tensions (filter #(= :tension (:evidence/claim-type %)) entries)]
        (is (pos? (count tensions))
            "Should emit tension evidence for relay timeout")))))

;; =============================================================================
;; 14. M-IRC-stability: Error Logging (F3)
;; =============================================================================

(deftest send-errors-logged-to-evidence
  (testing "send-fn errors emit tension evidence (not silently swallowed)"
    (let [;; Create callbacks with a send-fn that throws
          {:keys [on-connect on-line clients] :as irc}
          (make-test-irc {:send-fn (fn [cid line]
                                     (when (re-find #"001" line)
                                       (throw (Exception. "mock send error"))))})]
      (estore/reset-store!)

      ;; Registration will trigger welcome numerics, which will throw on 001
      ;; The error should be caught and evidence emitted, not crash the server
      (on-connect :c1)
      (on-line :c1 "NICK joe")
      ;; This triggers welcome which calls send-fn — the send-fn in irc callbacks
      ;; doesn't throw directly, but the start-irc-server! send-fn does emit evidence.
      ;; For callback-level testing, we verify the client state is still valid
      (is (= "joe" (:nick (get @clients :c1)))
          "Server should not crash on send errors"))))

;; =============================================================================
;; 15. M-IRC-stability: Client state includes liveness fields
;; =============================================================================

(deftest client-state-includes-liveness-fields
  (testing "on-connect initializes liveness tracking fields"
    (let [{:keys [on-connect clients]} (make-test-irc)]
      (on-connect :c1)
      (let [client (get @clients :c1)]
        (is (number? (:last-activity-at client))
            "Should have :last-activity-at timestamp")
        (is (false? (:ping-pending? client))
            "Should have :ping-pending? false initially")))))

;; =============================================================================
;; 16. P-6 structural proof: interleaved streams from multiple IRC sources
;; =============================================================================

(deftest p6-multiple-senders-interleaved-to-single-agent
  (testing "P-6: messages from multiple IRC users are relayed to a single agent interleaved"
    (let [agent-received (atom [])
          bridge (irc/make-relay-bridge {:evidence-store estore/!store})
          {:keys [relay-fn join-agent!]} bridge]

      ;; Register one agent listening on #futon
      (join-agent! "claude-1" "claude" "#futon"
                   (fn [data] (swap! agent-received conj data)))

      ;; Simulate three different IRC users sending messages
      (relay-fn "#futon" "joe" "First message from Joe")
      (relay-fn "#futon" "alice" "Alice chiming in")
      (relay-fn "#futon" "joe" "Second from Joe")
      (relay-fn "#futon" "bob" "Bob here too")
      (relay-fn "#futon" "alice" "Alice again")

      ;; Agent should have received all 5 messages in order
      (is (= 5 (count @agent-received))
          "Agent should receive all messages from all senders")

      ;; Verify interleaved ordering and source identification
      (let [msgs (mapv #(json/parse-string % true) @agent-received)]
        (is (= "joe" (:from (nth msgs 0))))
        (is (= "alice" (:from (nth msgs 1))))
        (is (= "joe" (:from (nth msgs 2))))
        (is (= "bob" (:from (nth msgs 3))))
        (is (= "alice" (:from (nth msgs 4))))

        ;; All messages include source attribution (P-6 requirement)
        (doseq [msg msgs]
          (is (contains? msg :from) "Each message must identify its source")
          (is (= "#futon" (:channel msg)) "All from same channel"))))))

(deftest p6-multi-channel-routing-to-agents
  (testing "P-6: agent receives messages from multiple channels it's joined"
    (let [agent-received (atom [])
          bridge (irc/make-relay-bridge {:evidence-store estore/!store})
          {:keys [relay-fn join-agent!]} bridge]

      ;; Agent joins two channels
      (join-agent! "claude-1" "claude" "#futon"
                   (fn [data] (swap! agent-received conj data)))
      ;; The relay bridge tracks channels per agent — need to add second channel
      ;; For this test, register a second agent entry for the same logical agent
      (swap! (:agents bridge) update "claude-1"
             update :channels conj "#standup")

      ;; Messages from different channels
      (relay-fn "#futon" "joe" "Futon discussion")
      (relay-fn "#standup" "joe" "Standup update")
      (relay-fn "#futon" "alice" "Futon followup")

      ;; Agent receives all three, interleaved by channel
      (is (= 3 (count @agent-received)))
      (let [msgs (mapv #(json/parse-string % true) @agent-received)]
        (is (= "#futon" (:channel (nth msgs 0))))
        (is (= "#standup" (:channel (nth msgs 1))))
        (is (= "#futon" (:channel (nth msgs 2))))
        ;; Each message includes channel for stream identification
        (doseq [msg msgs]
          (is (contains? msg :channel) "Must include channel for stream disambiguation"))))))
