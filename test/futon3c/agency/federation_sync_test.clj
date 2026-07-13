(ns futon3c.agency.federation-sync-test
  (:import [java.time Instant])
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

(deftest sync-tick-uses-configured-peers-when-none-injected
  ;; Regression (2026-07-12, found live on lucy): the destructured local
  ;; `peers` shadowed the `peers` fn, so a daemon tick — which passes no
  ;; :peers — called nil as a function and NPE'd on every tick. The daemon
  ;; never synced anything while tests, which always inject :peers, stayed
  ;; green. This tick exercises the daemon's actual call shape.
  (testing "a tick without :peers falls back to the configured peer list"
    (let [peer "http://hub:7070"]
      (fed/configure! {:peers [peer] :self-url "http://laptop:7070" :peer-sites ["lon"]})
      (let [results (fed/sync-tick! {:now-ms 1000
                                     :interval-ms 1000
                                     :jitter-fn (constantly 0)
                                     :fetch-fn (fn [_] (peer-roster "claude-9"))})]
        (is (= [peer] (mapv :peer results)))
        (is (every? :ok results))
        (is (= true (get-in (reg/get-agent "claude-9")
                            [:agent/metadata :proxy?])))))))

(deftest imported-proxy-gets-home-site-for-bare-ids
  ;; Roster-completeness gap #1 (chi-claude-1's report, 2026-07-12): bare-id
  ;; proxies (claude-3, codex-1, zai-1 …) imported with home-site nil, so the
  ;; site-grouped *agents* renderer dumped 7 of 8 London proxies in an
  ;; ungrouped bucket. home-site now resolves id-prefix → origin declaration
  ;; (roster metadata / announce body) → configured url→site mapping.
  (testing "metadata home-site in the synced roster entry wins for bare ids"
    (let [peer "http://hub:7070"]
      (fed/configure! {:peers [peer] :self-url "http://laptop:7070" :peer-sites ["lon"]})
      (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] {:ok true
                                          :agents {"claude-7" {:type "claude"
                                                               :capabilities ["coordination/execute"]
                                                               :metadata {:home-site "lon"}}}})})
      (is (= :lon (get-in (reg/get-agent "lon-claude-7") [:agent/metadata :home-site])))
      (is (= "claude-7" (get-in (reg/get-agent "lon-claude-7") [:agent/metadata :remote-agent-id])))
      (is (nil? (reg/get-agent "claude-7")))))
  (testing "configured url→site mapping covers entries with no declaration"
    (let [peer "http://hub:7070"]
      (fed/configure! {:peers [{:url peer :site "lon"}] :self-url "http://laptop:7070"})
      (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] (peer-roster "claude-8"))})
      (is (= :lon (get-in (reg/get-agent "lon-claude-8") [:agent/metadata :home-site])))))
  (testing "a site-qualified id keeps its own prefix over any fallback"
    (let [peer "http://hub:7070"]
      (fed/configure! {:peers [{:url peer :site "lon"}] :self-url "http://laptop:7070"})
      (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] (peer-roster "chi-claude-1"))})
      (is (= :chi (get-in (reg/get-agent "chi-claude-1") [:agent/metadata :home-site]))))))

(deftest colliding-bare-ids-import-qualified-not-refused
  ;; Joe's addressing contract (2026-07-12): locals stay bare ("tell codex-1"
  ;; = this box's codex-1); imports get their home-site prefix. The laptop's
  ;; codex-1 lands here as oxf-codex-1 instead of :skipped-local/-protected —
  ;; the claude-2 / zai-1 collision gap closes at the import seam.
  (testing "a peer agent whose bare id collides with a real local imports qualified"
    (let [peer "http://laptop:17070"]
      (fed/configure! {:peers [{:url peer :site "oxf"}] :self-url "http://lon:7070"})
      (reg/register-agent! {:agent-id {:id/value "codex-1" :id/type :continuity}
                            :type :codex
                            :invoke-fn (fn [_ _] {:ok true})
                            :capabilities [:coordination/execute]
                            :metadata {}})
      (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] (peer-roster "codex-1"))})
      (let [local (reg/get-agent "codex-1")
            imported (reg/get-agent "oxf-codex-1")]
        (is (nil? (get-in local [:agent/metadata :proxy?])))
        (is (= true (get-in imported [:agent/metadata :proxy?])))
        (is (= :oxf (get-in imported [:agent/metadata :home-site])))
        (is (= "codex-1" (get-in imported [:agent/metadata :remote-agent-id]))))))
  (testing "departure prunes the qualified id"
    (let [peer "http://laptop:17070"]
      (fed/configure! {:peers [{:url peer :site "oxf"}] :self-url "http://lon:7070"})
      (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] (peer-roster "zai-2"))})
      (is (some? (reg/get-agent "oxf-zai-2")))
      (fed/sync-tick! {:now-ms 3000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] (peer-roster))})
      (is (nil? (reg/get-agent "oxf-zai-2"))))))

(deftest uplink-roster-preserves-runtime-status
  (testing "WS-uplink roster export/import carries invoking previews and clears them on idle"
    (let [origin "ws-uplink:oxf"
          old-site (System/getProperty "FUTON3C_SITE")
          started-at (Instant/parse "2026-07-13T10:00:00Z")]
      (try
        (System/setProperty "FUTON3C_SITE" "oxf")
        (reg/register-agent!
         {:agent-id {:id/value "codex-7" :id/type :continuity}
          :type :codex
          :invoke-fn (fn [_ _] {:ok true})
          :capabilities [:coordination/execute]})
        (reg/update-agent!
         "codex-7"
         :agent/status :invoking
         :agent/invoke-started-at started-at
         :agent/invoke-prompt-preview "--- CURRENT TURN ---\nSurface: bell"
         :agent/invoke-activity "using bash")
        (let [entry (->> (fed/export-roster)
                         (filter #(= "codex-7" (:agent-id %)))
                         first)]
          (is (= :invoking (:status entry)))
          (is (= (str started-at) (:invoke-started-at entry)))
          (is (= "--- CURRENT TURN ---\nSurface: bell"
                 (:invoke-prompt-preview entry)))
          (reg/reset-registry!)
          (System/setProperty "FUTON3C_SITE" "lon")
          (fed/import-uplink-roster! origin [entry] {:uplink-site "oxf"})
          (let [proxy (get-in (reg/registry-status) [:agents "oxf-codex-7"])]
            (is (= :invoking (:status proxy)))
            (is (= (str started-at) (:invoke-started-at proxy)))
            (is (= "using bash" (:invoke-activity proxy)))
            (is (= "--- CURRENT TURN ---\nSurface: bell"
                   (:invoke-prompt-preview proxy)))))
        (fed/import-uplink-roster!
         origin
         [{:agent-id "codex-7"
           :type "codex"
           :capabilities ["coordination/execute"]
           :home-site "oxf"
           :status :idle
           :invoke-started-at nil
           :invoke-prompt-preview nil
           :invoke-activity nil}]
         {:uplink-site "oxf"})
        (let [proxy (get-in (reg/registry-status) [:agents "oxf-codex-7"])]
          (is (= :idle (:status proxy)))
          (is (nil? (:invoke-started-at proxy)))
          (is (nil? (:invoke-activity proxy)))
          (is (nil? (:invoke-prompt-preview proxy))))
        (finally
          (if old-site
            (System/setProperty "FUTON3C_SITE" old-site)
            (System/clearProperty "FUTON3C_SITE")))))))

(deftest own-site-reflections-are-not-imported
  ;; When we pull a peer, its roster contains ITS proxies of OUR agents.
  ;; Importing those would loop invokes to our own agent through the peer.
  (testing "an entry whose home-site is our own site is skipped"
    (let [peer "http://laptop:17070"]
      (System/setProperty "FUTON3C_SITE" "lon")
      (try
        (fed/configure! {:peers [{:url peer :site "oxf"}] :self-url "http://lon:7070"})
        (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                         :fetch-fn (fn [_] {:ok true
                                            :agents {"claude-6" {:type "claude"
                                                                 :capabilities ["coordination/execute"]
                                                                 :metadata {:home-site "lon"
                                                                            :proxy? true}}}})})
        (is (nil? (reg/get-agent "lon-claude-6")))
        (is (nil? (get-in (reg/get-agent "claude-6") [:agent/metadata :proxy?])))
        (finally (System/clearProperty "FUTON3C_SITE")))))
  (testing "an entry whose origin-url is our self-url is skipped (undecorated box)"
    (let [peer "http://hub:7070"]
      (fed/configure! {:peers [{:url peer :site "lon"}] :self-url "http://laptop:17070"})
      (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                       :fetch-fn (fn [_] {:ok true
                                          :agents {"zai-2" {:type "zai"
                                                            :capabilities ["coordination/execute"]
                                                            :metadata {:origin-url "http://laptop:17070"
                                                                       :proxy? true}}}})})
      (is (nil? (reg/get-agent "lon-zai-2")))
      (is (nil? (reg/get-agent "zai-2"))))))

(deftest bare-proxy-migrates-to-qualified-on-first-tick
  ;; Pre-qualification proxies (bare zai-2 etc. imported before this slice)
  ;; are replaced by their qualified twins in one tick: register creates
  ;; oxf-zai-2, prune drops bare zai-2 (no longer in the qualified roster set).
  (let [peer "http://laptop:17070"]
    (fed/configure! {:peers [{:url peer :site "oxf"}] :self-url "http://lon:7070"})
    (reg/register-agent! {:agent-id {:id/value "zai-9" :id/type :continuity}
                          :type :zai
                          :invoke-fn (fn [_ _] {:ok true})
                          :capabilities [:coordination/execute]
                          :metadata {:proxy? true :remote? true :origin-url peer}})
    (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                     :fetch-fn (fn [_] (peer-roster "zai-9"))})
    (is (nil? (reg/get-agent "zai-9")))
    (is (= true (get-in (reg/get-agent "oxf-zai-9") [:agent/metadata :proxy?])))))

(deftest direct-presence-registration-suppresses-proxy-import
  ;; codex-3 case: the agent maintains its own ws-bridge presence here (bare
  ;; id, declared home-site), AND appears in its home peer's roster. Without
  ;; dedup the sync import doubles it as oxf-codex-3.
  (let [peer "http://laptop:17070"]
    (fed/configure! {:peers [{:url peer :site "oxf"}] :self-url "http://lon:7070"})
    (reg/register-agent! {:agent-id {:id/value "codex-3" :id/type :continuity}
                          :type :codex
                          :invoke-fn nil
                          :capabilities [:coordination/execute]
                          :metadata {:ws-bridge? true :home-site :oxf}})
    (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                     :fetch-fn (fn [_] (peer-roster "codex-3"))})
    (is (nil? (reg/get-agent "oxf-codex-3")))
    (is (= true (get-in (reg/get-agent "codex-3") [:agent/metadata :ws-bridge?])))))

(deftest leftover-loop-back-proxies-self-heal
  ;; A loop-back proxy imported by pre-guard code (our own agent, proxied
  ;; back at us through a peer) must be pruned once the guard is live, not
  ;; kept alive by its presence in the peer's roster.
  (let [peer "http://hub:7070"]
    (fed/configure! {:peers [{:url peer :site "lon"}] :self-url "http://laptop:17070"})
    (reg/register-agent! {:agent-id {:id/value "oxf-zai-2" :id/type :continuity}
                          :type :zai
                          :invoke-fn (fn [_ _] {:ok true})
                          :capabilities [:coordination/execute]
                          :metadata {:proxy? true :remote? true :origin-url peer}})
    (fed/sync-tick! {:now-ms 1000 :interval-ms 1000 :jitter-fn (constantly 0)
                     :fetch-fn (fn [_] {:ok true
                                        :agents {"oxf-zai-2" {:type "zai"
                                                              :capabilities ["coordination/execute"]
                                                              :metadata {:proxy? true
                                                                         :origin-url "http://laptop:17070"}}}})})
    (is (nil? (reg/get-agent "oxf-zai-2")))))
