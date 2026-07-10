(ns futon3c.agency.federation-sync-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.federation :as fed]
            [futon3c.agency.logic :as logic]
            [futon3c.agency.registry :as reg]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (fed/reset-sync-state!)
    (fed/configure! {:peers [] :self-url nil :peer-sites []})
    (f)
    (fed/reset-sync-state!)))

(defn- peer-roster
  [& agent-ids]
  {:ok true
   :agents (into {}
                 (map (fn [agent-id]
                        [agent-id {:type "claude"
                                   :capabilities ["coordination/execute"]}]))
                 agent-ids)})

(defn- tick!
  [peer-url now-ms fetch-fn]
  (fed/sync-tick! {:peers [peer-url]
                   :now-ms now-ms
                   :interval-ms 1000
                   :jitter-fn (constantly 0)
                   :fetch-fn fetch-fn}))

(defn- route-info
  [agent-id]
  (get-in (reg/registry-status) [:agents agent-id]))

(deftest sync-tick-imports-new-peer-agents
  (testing "a hub-added agent appears locally as a proxy after one sync tick"
    (let [peer "http://hub:7070"
          roster (atom (peer-roster "claude-3"))]
      (fed/configure! {:peers [peer] :self-url "http://laptop:7070" :peer-sites ["lon"]})
      (tick! peer 1000 (fn [_] @roster))
      (is (some? (reg/get-agent "claude-3")))
      (reset! roster (peer-roster "claude-3" "claude-4"))
      (tick! peer 2000 (fn [_] @roster))
      (let [agent (reg/get-agent "claude-4")]
        (is (= true (get-in agent [:agent/metadata :proxy?])))
        (is (= true (get-in agent [:agent/metadata :remote?])))
        (is (= peer (get-in agent [:agent/metadata :origin-url])))
        (is (fn? (:agent/invoke-fn agent)))
        (is (empty? (logic/find-phantoms (logic/build-live-db))))))))

(deftest sync-tick-prunes-departed-proxies-but-not-local-agents
  (testing "departed peer agents are pruned, while real local records are never pruned"
    (let [peer "http://hub:7070"
          roster (atom (peer-roster "claude-3" "claude-4" "shared-1"))]
      (reg/register-agent!
       {:agent-id {:id/value "shared-1" :id/type :continuity}
        :type :claude
        :invoke-fn (fn [_ _] {:result "local"})
        :capabilities [:coordination/execute]
        :metadata {:local? true}})
      (tick! peer 1000 (fn [_] @roster))
      (is (some? (reg/get-agent "claude-4")))
      (is (nil? (get-in (reg/get-agent "shared-1") [:agent/metadata :proxy?])))
      (reset! roster (peer-roster "claude-3"))
      (tick! peer 2000 (fn [_] @roster))
      (is (nil? (reg/get-agent "claude-4")))
      (is (some? (reg/get-agent "shared-1")))
      (is (nil? (get-in (reg/get-agent "shared-1") [:agent/metadata :proxy?]))))))

(deftest unreachable-peer-marks-proxies-stale-and-recovers-with-backoff
  (testing "AG-3/AG-4: failed peer pull marks proxies stale until a later successful tick"
    (let [peer "http://hub:7070"
          state (atom :up)
          fetch (fn [_]
                  (case @state
                    :up (peer-roster "claude-4")
                    :down (throw (ex-info "hub unavailable" {:error "hub unavailable"}))))]
      (tick! peer 1000 fetch)
      (is (= :local (:invoke-route (route-info "claude-4"))))
      (reset! state :down)
      (let [[failed] (tick! peer 2000 fetch)
            agent (reg/get-agent "claude-4")
            unreachable (set (logic/find-unreachable-connected (logic/build-live-db)))]
        (is (false? (:ok failed)))
        (is (= 1 (:failure-count failed)))
        (is (= 1000 (:backoff-ms failed)))
        (is (= true (get-in agent [:agent/metadata :federation/stale?])))
        (is (= false (get-in agent [:agent/metadata :federation/reachable?])))
        (is (= :none (:invoke-route (route-info "claude-4"))))
        (is (contains? unreachable {:kind :stale-remote-health
                                    :agent-id "claude-4"}))
        (is (contains? unreachable {:kind :dead-channel
                                    :connection-id peer
                                    :declared-state :connected
                                    :channel-state :dead})))
      (is (= :backoff (:reason (first (tick! peer 2500 fetch)))))
      (let [[failed-again] (tick! peer 3000 fetch)]
        (is (= 2 (:failure-count failed-again)))
        (is (= 2000 (:backoff-ms failed-again))))
      (reset! state :up)
      (is (= :backoff (:reason (first (tick! peer 4000 fetch)))))
      (tick! peer 5000 fetch)
      (let [agent (reg/get-agent "claude-4")]
        (is (= false (get-in agent [:agent/metadata :federation/stale?])))
        (is (= true (get-in agent [:agent/metadata :federation/reachable?])))
        (is (= :local (:invoke-route (route-info "claude-4"))))
        (is (empty? (logic/find-unreachable-connected (logic/build-live-db))))))))

(deftest continuous-sync-daemon-defaults-off
  (testing "unset/zero interval preserves one-shot boot behavior by not starting a daemon"
    (let [old (System/getProperty "FUTON3C_FED_SYNC_INTERVAL_MS")]
      (try
        (System/setProperty "FUTON3C_FED_SYNC_INTERVAL_MS" "0")
        (let [status (fed/start-sync-daemon!)]
          (is (false? (:enabled? status)))
          (is (false? (:running? status))))
        (finally
          (if old
            (System/setProperty "FUTON3C_FED_SYNC_INTERVAL_MS" old)
            (System/clearProperty "FUTON3C_FED_SYNC_INTERVAL_MS")))))))
