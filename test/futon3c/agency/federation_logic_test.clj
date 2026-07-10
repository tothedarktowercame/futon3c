(ns futon3c.agency.federation-logic-test
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.logic :as logic]))

(defn- agent-record
  [id {:keys [type invoke-fn session-id home-site proxy? origin-url status]
       :or {type :claude status :idle}}]
  {:agent/id {:id/value id :id/type :continuity}
   :agent/type type
   :agent/invoke-fn invoke-fn
   :agent/capabilities [:coordination/execute]
   :agent/session-id session-id
   :agent/status status
   :agent/metadata (cond-> {}
                     home-site (assoc :home-site home-site)
                     proxy? (assoc :proxy? true :remote? true)
                     origin-url (assoc :origin-url origin-url))})

(def ^:private positive-snapshot
  {:site :laptop
   :local-point :laptop
   :self-url "http://laptop:7070"
   :peers [{:site :lon :url "http://lon:7070"}]
   :peer-rosters {:lon #{"claude-1" "lon-claude-1"}}
   :registry {"claude-1" (agent-record "claude-1" {:invoke-fn (fn [_ _] {:ok true})
                                                   :session-id "sess-local"
                                                   :home-site :laptop})
              "lon-claude-1" (agent-record "lon-claude-1" {:session-id "sess-lon"
                                                           :home-site :lon
                                                           :proxy? true
                                                           :origin-url "http://lon:7070"})}
   :routing {"claude-1" {:invoke-route :local :invoke-ready? true}
             "lon-claude-1" {:invoke-route :none
                             :invoke-ready? false
                             :invoke-diagnostic "proxy present; no local invoke"}}
   :live-writers [{:agent-id "claude-1"
                   :session-id "sess-local"
                   :inhabitant-id "inhabitant-local"}]
   :connections [{:connection-id :lon-ws
                  :declared-state :connected
                  :channel-state :live}]
   :remote-health [{:agent-id "lon-claude-1"
                    :registered-ready? true
                    :current-ready? true}]
   :session-capacity [{:agent-id "claude-1"
                       :session-id "sess-local"
                       :capacity-state :ok}]
   :server-accept [{:server-id :laptop-7070
                    :listening? true
                    :accepting? true
                    :backlog 0
                    :backlog-limit 50
                    :accept-latency-ms 10
                    :latency-bound-ms 1000}]})

(defn- with-extra
  [snapshot & kvs]
  (update snapshot :registry #(apply assoc % kvs)))

(deftest snapshot-db-and-positive-federation-pass
  (testing "plain data snapshots build a DB and a correctly-federated proxy has no CP-A findings"
    (let [db (logic/snapshot->db positive-snapshot)
          v (logic/query-violations db)]
      (is (empty? (logic/find-phantoms db)))
      (is (empty? (logic/find-unpropagated db)))
      (is (= {:duplicate-sessions [] :duplicate-live-writers []}
             (logic/find-session-collisions db)))
      (is (empty? (logic/find-unreachable-connected db)))
      (is (not (logic/violations? v))))))

(deftest mission-violation-claude-4-unpropagated-from-lucy
  (testing "claude-4 registered locally on lucy/lon but absent from laptop roster is found"
    (let [snapshot {:site :lon
                    :local-point :lon
                    :self-url "http://lon:7070"
                    :peers [{:site :laptop :url "http://laptop:7070"}]
                    :peer-rosters {:laptop #{"lon-claude-1"}}
                    :registry {"claude-4" (agent-record "claude-4" {:invoke-fn (fn [_ _] {:ok true})
                                                                    :session-id "sess-claude-4"
                                                                    :home-site :lon})}
                    :routing {"claude-4" {:invoke-route :local :invoke-ready? true}}}
          db (logic/snapshot->db snapshot)]
      (is (= [{:agent-id "claude-4"
               :site :lon
               :missing-peer :laptop
               :peer-url "http://laptop:7070"}]
             (logic/find-unpropagated db))))))

(deftest mission-violation-remote-phantoms-on-laptop
  (testing "lon/chi agents present on laptop as local agents are AG-2 phantoms"
    (let [snapshot {:site :laptop
                    :local-point :laptop
                    :registry {"lon-claude-1" (agent-record "lon-claude-1"
                                                            {:invoke-fn (fn [_ _] {:wrong :local})
                                                             :session-id "sess-lon"
                                                             :home-site :lon})
                               "chi-claude-1" (agent-record "chi-claude-1"
                                                            {:invoke-fn (fn [_ _] {:wrong :local})
                                                             :session-id "sess-chi"
                                                             :home-site :chi})}
                    :routing {"lon-claude-1" {:invoke-route :local :invoke-ready? true}
                              "chi-claude-1" {:invoke-route :local :invoke-ready? true}}}
          db (logic/snapshot->db snapshot)]
      (is (= #{{:agent-id "lon-claude-1"
                :home-point :lon
                :local-point :laptop}
               {:agent-id "chi-claude-1"
                :home-point :chi
                :local-point :laptop}}
             (set (logic/find-phantoms db)))))))

(deftest ag-1-singular-identity-and-session-integrity
  (testing "passing fixture has no duplicate sessions or live writers"
    (let [db (logic/snapshot->db positive-snapshot)]
      (is (= {:duplicate-sessions [] :duplicate-live-writers []}
             (logic/find-session-collisions db)))))
  (testing "failing fixture catches shared sessions and bifurcated live writers"
    (let [snapshot (-> positive-snapshot
                       (with-extra "codex-1" (agent-record "codex-1" {:type :codex
                                                                      :session-id "sess-local"
                                                                      :home-site :laptop}))
                       (assoc :live-writers [{:agent-id "claude-1"
                                              :session-id "sess-local"
                                              :inhabitant-id "interactive"}
                                             {:agent-id "claude-1"
                                              :session-id "sess-local"
                                              :inhabitant-id "headless"}]))
          collisions (logic/find-session-collisions (logic/snapshot->db snapshot))]
      (is (= [["claude-1" "codex-1" "sess-local"]]
             (:duplicate-sessions collisions)))
      (is (= [["headless" "interactive" "sess-local"]]
             (:duplicate-live-writers collisions))))))

(deftest ag-2-home-point-routing
  (testing "proxy representation passes; remote-home local route fails"
    (let [pass-db (logic/snapshot->db positive-snapshot)
          fail-db (logic/snapshot->db
                    {:site :laptop
                     :local-point :laptop
                     :registry {"lon-claude-1" (agent-record "lon-claude-1"
                                                             {:invoke-fn (fn [_ _] {:wrong :local})
                                                              :session-id "sess-lon"
                                                              :home-site :lon})}
                     :routing {"lon-claude-1" {:invoke-route :local :invoke-ready? true}}})]
      (is (empty? (logic/find-phantoms pass-db)))
      (is (= ["lon-claude-1"]
             (pldb/with-db fail-db
               (l/run* [aid]
                 (logic/ag-2-phantomo aid))))))))

(deftest ag-3-4-liveness-honesty-is-consolidated
  (testing "live channel/current health pass; dead channel and stale health fail"
    (let [pass-db (logic/snapshot->db positive-snapshot)
          fail-db (logic/snapshot->db
                    {:connections [{:connection-id :codex-ws
                                    :declared-state :connected
                                    :channel-state :dead}]
                     :remote-health [{:agent-id "remote-codex"
                                      :registered-ready? true
                                      :current-ready? false}]})]
      (is (empty? (logic/find-unreachable-connected pass-db)))
      (is (= #{{:kind :dead-channel
                :connection-id :codex-ws
                :declared-state :connected
                :channel-state :dead}
               {:kind :stale-remote-health
                :agent-id "remote-codex"}}
             (set (logic/find-unreachable-connected fail-db))))
      (is (= #{[:dead-channel :codex-ws]
               [:stale-remote-health "remote-codex"]}
             (set (pldb/with-db fail-db
                    (l/run* [q]
                      (l/fresh [kind id]
                        (logic/ag-3-4-unreachable-connectedo kind id)
                        (l/== q [kind id]))))))))))

(deftest ag-5-total-diagnosable-invoke-readiness
  (testing "route :none with diagnostic passes; route :none without diagnostic fails"
    (let [pass-db (logic/snapshot->db positive-snapshot)
          fail-db (logic/snapshot->db {:routing {"codex-1" {:invoke-route :none
                                                            :invoke-ready? false}}})]
      (is (empty? (logic/query-invoke-readiness-gaps pass-db)))
      (is (= ["codex-1"]
             (pldb/with-db fail-db
               (l/run* [aid]
                 (logic/ag-5-invoke-readiness-gapo aid))))))))

(deftest ag-6-session-freshness
  (testing "ok capacity passes; context-exhausted capacity fails"
    (let [pass-db (logic/snapshot->db positive-snapshot)
          fail-db (logic/snapshot->db {:session-capacity [{:agent-id "codex-1"
                                                           :session-id "019ecbd7"
                                                           :capacity-state :context-exhausted}]})]
      (is (empty? (logic/query-exhausted-sessions pass-db)))
      (is (= [["codex-1" "019ecbd7"]]
             (pldb/with-db fail-db
               (l/run* [q]
                 (l/fresh [aid sid]
                   (logic/ag-6-exhausted-sessiono aid sid)
                   (l/== q [aid sid])))))))))

(deftest ag-7-server-accept-liveness
  (testing "accepting server passes; listening but unaccepting server fails"
    (let [pass-db (logic/snapshot->db positive-snapshot)
          fail-db (logic/snapshot->db {:server-accept [{:server-id :lon-7070
                                                        :listening? true
                                                        :accepting? false
                                                        :backlog 51
                                                        :backlog-limit 50
                                                        :accept-latency-ms 60000
                                                        :latency-bound-ms 1000}]})]
      (is (empty? (logic/query-unaccepting-servers pass-db)))
      (is (= [:lon-7070]
             (pldb/with-db fail-db
               (l/run* [server-id]
                 (logic/ag-7-unaccepting-servero server-id)))))
      (is (= [:lon-7070]
             (mapv :server-id (logic/query-unaccepting-servers fail-db)))))))

(deftest live-snapshot-smoke
  (testing "live registry/federation snapshot can be mapped without requiring live peer boxes"
    (is (some? (logic/build-live-db)))))
