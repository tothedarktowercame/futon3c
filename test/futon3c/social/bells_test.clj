(ns futon3c.social.bells-test
  "Tests for bell dispatcher — standup and test-bell coordination triggers."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.social.bells :as bells]
            [futon3c.agency.registry :as registry]
            [futon3c.evidence.store :as estore]
            [futon3c.transport.irc :as irc]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (registry/reset-registry!)
    (estore/reset-store!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- register-mock-agent!
  "Register a mock agent with a capture atom for invocations."
  [id]
  (let [invocations (atom [])]
    (registry/register-agent!
     {:agent-id {:id/value id :id/type :continuity}
      :type :mock
      :invoke-fn (fn [prompt session-id]
                   (swap! invocations conj {:prompt prompt :session-id session-id})
                   {:result (str "ack from " id) :session-id session-id})
      :capabilities [:chat :standup]})
    invocations))

(defn- make-test-irc
  "Create IRC callbacks with mock send/close for standup testing."
  []
  (let [sent (atom [])
        config {:send-fn (fn [cid line] (swap! sent conj {:to cid :line line}))
                :close-fn (fn [_])
                :relay-fn (fn [_ _ _])
                :evidence-store estore/!store}
        callbacks (irc/make-irc-callbacks config)]
    (assoc callbacks :sent sent)))

(defn- make-mock-irc-fns
  "Create mock IRC server functions for standup bell testing.
   Returns map with :join-virtual-nick!, :send-to-channel!, and tracking atoms."
  []
  (let [virtual-nicks (atom {})  ;; {channel -> #{nicks}}
        channel-msgs (atom [])]  ;; [{:channel :from :text}]
    {:join-virtual-nick! (fn [channel nick]
                           (swap! virtual-nicks update channel (fnil conj #{}) nick))
     :send-to-channel! (fn [channel from text]
                          (swap! channel-msgs conj {:channel channel :from from :text text}))
     :virtual-nicks virtual-nicks
     :channel-msgs channel-msgs}))

;; =============================================================================
;; Standup bell tests
;; =============================================================================

(deftest standup-bell-joins-all-agents
  (testing "ringing standup bell joins all registered agents to the room"
    (register-mock-agent! "claude-1")
    (register-mock-agent! "codex-1")
    (register-mock-agent! "codex-2")
    (let [{:keys [join-virtual-nick! send-to-channel! virtual-nicks channel-msgs]}
          (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:room "#standup"
                   :prompt "Good morning! Let's review yesterday's progress."
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!
                   :evidence-store estore/!store})]
      ;; Bell should succeed
      (is (= :standup (:bell/type result)))
      (is (= "#standup" (:bell/room result)))
      (is (string? (:bell/at result)))
      (is (string? (:bell/session-id result)))
      ;; All 3 agents should be joined
      (is (= 3 (count (:bell/agents result))))
      (let [agent-ids (set (map :id/value (:bell/agents result)))]
        (is (contains? agent-ids "claude-1"))
        (is (contains? agent-ids "codex-1"))
        (is (contains? agent-ids "codex-2")))
      ;; Virtual nicks should be in the room
      (let [nicks (get @virtual-nicks "#standup")]
        (is (= 3 (count nicks)))
        (is (contains? nicks "claude-1"))
        (is (contains? nicks "codex-1"))
        (is (contains? nicks "codex-2")))
      ;; Opening prompt should have been sent
      (is (= 1 (count @channel-msgs)))
      (is (= {:channel "#standup"
              :from "joe"
              :text "Good morning! Let's review yesterday's progress."}
             (first @channel-msgs))))))

(deftest standup-bell-emits-evidence
  (testing "standup bell emits bell-ring and arrival evidence"
    (register-mock-agent! "claude-1")
    (register-mock-agent! "codex-1")
    (let [{:keys [join-virtual-nick! send-to-channel!]} (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:room "#standup"
                   :prompt "standup time"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!
                   :evidence-store estore/!store})
          all-evidence (estore/query {:query/type :coordination})]
      ;; 1 bell-ring + 2 arrivals = 3 evidence entries
      (is (= 3 (count all-evidence)))
      ;; Bell ring evidence
      (let [bell-evs (filter #(= :step (:evidence/claim-type %)) all-evidence)]
        (is (= 1 (count bell-evs)))
        (is (= :standup (get-in (first bell-evs) [:evidence/body :bell-type])))
        (is (= "#standup" (get-in (first bell-evs) [:evidence/body :room])))
        (is (= 2 (count (get-in (first bell-evs) [:evidence/body :agents])))))
      ;; Arrival evidence
      (let [arrivals (filter #(= :observation (:evidence/claim-type %)) all-evidence)]
        (is (= 2 (count arrivals)))
        (is (every? #(= :arrived (get-in % [:evidence/body :status])) arrivals))))))

(deftest standup-bell-specific-agents
  (testing "standup bell can target specific agents instead of :all"
    (register-mock-agent! "claude-1")
    (register-mock-agent! "codex-1")
    (register-mock-agent! "codex-2")
    (let [{:keys [join-virtual-nick! send-to-channel! virtual-nicks]} (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:room "#standup"
                   :prompt "just us two"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!
                   :evidence-store estore/!store
                   :agents ["claude-1" "codex-1"]})]
      ;; Only 2 agents should be joined
      (is (= 2 (count (:bell/agents result))))
      (let [nicks (get @virtual-nicks "#standup")]
        (is (= 2 (count nicks)))
        (is (contains? nicks "claude-1"))
        (is (contains? nicks "codex-1"))
        (is (not (contains? nicks "codex-2")))))))

(deftest standup-bell-no-agents-registered
  (testing "standup bell with no registered agents returns empty agents list"
    (let [{:keys [join-virtual-nick! send-to-channel! virtual-nicks channel-msgs]}
          (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:room "#standup"
                   :prompt "anyone here?"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!
                   :evidence-store estore/!store})]
      (is (= :standup (:bell/type result)))
      (is (= [] (:bell/agents result)))
      ;; Prompt should still be sent (Joe might be talking to himself)
      (is (= 1 (count @channel-msgs)))
      ;; No virtual nicks in the room
      (is (empty? (get @virtual-nicks "#standup"))))))

(deftest standup-bell-missing-room-errors
  (testing "standup bell without room returns error"
    (let [{:keys [join-virtual-nick! send-to-channel!]} (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:prompt "no room"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!})]
      (is (false? (:ok result)))
      (is (string? (:error result))))))

(deftest standup-bell-no-prompt-skips-message
  (testing "standup bell without prompt joins agents but sends no message"
    (register-mock-agent! "claude-1")
    (let [{:keys [join-virtual-nick! send-to-channel! virtual-nicks channel-msgs]}
          (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:room "#standup"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!
                   :evidence-store estore/!store})]
      (is (= :standup (:bell/type result)))
      (is (= 1 (count (:bell/agents result))))
      (is (contains? (get @virtual-nicks "#standup") "claude-1"))
      ;; No message sent
      (is (empty? @channel-msgs)))))

(deftest standup-bell-with-relay-bridge
  (testing "standup bell joins agents in both IRC and relay bridge"
    (register-mock-agent! "claude-1")
    (let [{:keys [join-virtual-nick! send-to-channel!]} (make-mock-irc-fns)
          bridge (irc/make-relay-bridge {:evidence-store estore/!store})
          result (bells/ring-standup!
                  {:room "#standup"
                   :prompt "let's go"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :join-agent! (:join-agent! bridge)
                   :send-to-channel! send-to-channel!
                   :evidence-store estore/!store})]
      (is (= 1 (count (:bell/agents result))))
      ;; Agent should be in the relay bridge
      (let [agents @(:agents bridge)]
        (is (= 1 (count agents)))
        (is (contains? agents "claude-1"))
        (is (= "#standup" (first (:channels (get agents "claude-1")))))))))

(deftest standup-bell-with-custom-registry
  (testing "standup bell can use a custom registry atom instead of global"
    (let [custom-reg (atom {"local-agent"
                            {:agent/id {:id/value "local-agent" :id/type :continuity}
                             :agent/type :mock
                             :agent/invoke-fn (fn [_ _] {:result "ok"})
                             :agent/capabilities [:chat]}})
          {:keys [join-virtual-nick! send-to-channel! virtual-nicks]} (make-mock-irc-fns)
          result (bells/ring-standup!
                  {:room "#private-standup"
                   :prompt "custom registry test"
                   :author "joe"
                   :join-virtual-nick! join-virtual-nick!
                   :send-to-channel! send-to-channel!
                   :registry custom-reg})]
      (is (= 1 (count (:bell/agents result))))
      (is (= "local-agent" (:id/value (first (:bell/agents result)))))
      (is (contains? (get @virtual-nicks "#private-standup") "local-agent")))))

;; =============================================================================
;; Test-bell tests
;; =============================================================================

(deftest test-bell-returns-secret
  (testing "test bell returns a secret for the agent to ack"
    (register-mock-agent! "claude-1")
    (let [result (bells/ring-test-bell!
                  {:agent-id "claude-1"
                   :evidence-store estore/!store})]
      (is (= :test-bell (:bell/type result)))
      (is (= "claude-1" (:id/value (:bell/agent-id result))))
      (is (string? (:bell/secret result)))
      (is (string? (:bell/at result))))))

(deftest test-bell-unregistered-agent-errors
  (testing "test bell for unregistered agent returns error"
    (let [result (bells/ring-test-bell!
                  {:agent-id "ghost-agent"
                   :evidence-store estore/!store})]
      (is (false? (:ok result)))
      (is (string? (:error result))))))

(deftest test-bell-emits-evidence
  (testing "test bell emits coordination evidence"
    (register-mock-agent! "claude-1")
    (bells/ring-test-bell!
     {:agent-id "claude-1"
      :evidence-store estore/!store})
    (let [evs (estore/query {:query/type :coordination})]
      (is (= 1 (count evs)))
      (is (= :test-bell (get-in (first evs) [:evidence/body :bell-type]))))))

;; =============================================================================
;; Integration: standup bell with real IRC callbacks
;; =============================================================================

(deftest standup-bell-with-irc-callbacks
  (testing "standup bell integrates with IRC callback infrastructure"
    (register-mock-agent! "claude-1")
    (register-mock-agent! "codex-1")
    (let [{:keys [rooms] :as irc-cbs} (make-test-irc)
          ;; Use IRC rooms atom for virtual nick tracking
          join-virtual! (fn [channel nick]
                          (swap! rooms update channel (fnil conj #{}) nick))
          sent-to-channel (atom [])
          send-to-ch! (fn [channel from text]
                        ;; Broadcast to all nicks in room via IRC protocol
                        (swap! sent-to-channel conj {:channel channel :from from :text text}))
          result (bells/ring-standup!
                  {:room "#daily"
                   :prompt "standup time"
                   :author "joe"
                   :join-virtual-nick! join-virtual!
                   :send-to-channel! send-to-ch!
                   :evidence-store estore/!store})]
      (is (= :standup (:bell/type result)))
      (is (= 2 (count (:bell/agents result))))
      ;; Agents should be visible in IRC NAMES
      (let [nicks (get @rooms "#daily")]
        (is (contains? nicks "claude-1"))
        (is (contains? nicks "codex-1"))))))
