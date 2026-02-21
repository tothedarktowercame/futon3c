(ns futon3c.agents.tickle-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.tickle :as tickle]
            [futon3c.evidence.store :as estore]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.time Instant]
           [java.util UUID]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (estore/reset-store!)
    (f)))

(defn- register-agent!
  [agent-id]
  (reg/register-agent!
   {:agent-id {:id/value agent-id :id/type :continuity}
    :type :mock
    :invoke-fn (fn [_ _] {:result "ok"})
    :capabilities [:coordination/execute]}))

(defn- make-evidence-store
  []
  (atom {:entries {} :order []}))

(defn- append-activity!
  [store agent-id at]
  (estore/append*
   store
   (fix/make-evidence-entry
    {:evidence/id (str "e-" (UUID/randomUUID))
     :evidence/subject (fix/make-artifact-ref :agent agent-id)
     :evidence/type :coordination
     :evidence/claim-type :observation
     :evidence/author agent-id
     :evidence/at (str at)
     :evidence/body {:event :heartbeat}
     :evidence/tags [:tickle :heartbeat]})))

(deftest scan-activity-empty-store-marks-all-stale
  (testing "scan-activity with empty evidence store marks all agents stale"
    (register-agent! "claude-1")
    (register-agent! "codex-1")
    (let [store (make-evidence-store)
          activity (tickle/scan-activity store (reg/registered-agents) 300)]
      (is (= #{"claude-1" "codex-1"} (set (keys activity))))
      (is (every? true? (map :stale? (vals activity)))))))

(deftest scan-activity-recent-evidence-not-stale
  (testing "scan-activity with recent evidence marks active agents non-stale"
    (register-agent! "claude-1")
    (register-agent! "codex-1")
    (let [store (make-evidence-store)]
      (append-activity! store "claude-1" (.minusSeconds (Instant/now) 20))
      (let [activity (tickle/scan-activity store (reg/registered-agents) 120)]
        (is (false? (get-in activity ["claude-1" :stale?])))
        (is (true? (get-in activity ["codex-1" :stale?])))))))

(deftest scan-activity-old-evidence-stale
  (testing "scan-activity with old evidence marks that agent stale"
    (register-agent! "claude-1")
    (let [store (make-evidence-store)]
      (append-activity! store "claude-1" (.minusSeconds (Instant/now) 900))
      (let [activity (tickle/scan-activity store (reg/registered-agents) 120)]
        (is (true? (get-in activity ["claude-1" :stale?])))
        (is (> (get-in activity ["claude-1" :stale-seconds]) 120))))))

(deftest scan-activity-excludes-self-id
  (testing "scan-activity excludes tickle's own agent-id"
    (register-agent! "claude-1")
    (register-agent! "tickle-1")
    (let [store (make-evidence-store)
          activity (tickle/scan-activity store (reg/registered-agents) 300
                                         :self-id "tickle-1")]
      (is (contains? activity "claude-1"))
      (is (not (contains? activity "tickle-1"))))))

(deftest scan-activity-default-self-id-is-tickle-1
  (testing "scan-activity defaults self-id to tickle-1"
    (register-agent! "claude-1")
    (register-agent! "tickle-1")
    (let [store (make-evidence-store)
          activity (tickle/scan-activity store (reg/registered-agents) 300)]
      (is (contains? activity "claude-1"))
      (is (not (contains? activity "tickle-1"))))))

(deftest detect-stalls-filters-only-stale
  (testing "detect-stalls returns only stale entries"
    (let [stalled (tickle/detect-stalls
                   {"claude-1" {:stale? false}
                    "codex-1" {:stale? true}
                    "tickle-1" {:stale? true}})]
      (is (= #{"codex-1" "tickle-1"} (set stalled))))))

(deftest page-agent-calls-test-bell
  (testing "page-agent! calls test bell and returns structured result"
    (let [called (atom nil)
          result (tickle/page-agent!
                  "claude-1"
                  {:ring-test-bell! (fn [m]
                                      (reset! called m)
                                      {:bell/type :test-bell})})]
      (is (= {:paged? true :agent-id "claude-1" :method :bell} result))
      (is (= "claude-1" (:agent-id @called))))))

(deftest escalate-invokes-notify-fn
  (testing "escalate! calls notify-fn with agent-id and reason"
    (let [called (atom nil)
          result (tickle/escalate!
                  "codex-1"
                  {:notify-fn (fn [agent-id reason]
                                (reset! called [agent-id reason]))})]
      (is (= {:escalated? true :agent-id "codex-1"} result))
      (is (= ["codex-1" :page-failed] @called)))))

(deftest run-scan-cycle-orchestrates-page-and-escalate
  (testing "run-scan-cycle! performs scan -> page -> escalate"
    (register-agent! "active-1")
    (register-agent! "paged-1")
    (register-agent! "escalate-1")
    (let [store (make-evidence-store)
          _ (append-activity! store "active-1" (.minusSeconds (Instant/now) 10))
          page-calls (atom [])
          escalations (atom [])
          result (tickle/run-scan-cycle!
                  {:evidence-store store
                   :threshold-seconds 120
                   :page-config {:ring-test-bell! (fn [{:keys [agent-id]}]
                                                    (swap! page-calls conj agent-id)
                                                    (if (= agent-id "paged-1")
                                                      {:bell/type :test-bell}
                                                      {:ok false :error "no-ack"}))}
                   :escalate-config {:notify-fn (fn [agent-id reason]
                                                  (swap! escalations conj [agent-id reason]))}})]
      (is (= 3 (:scanned result)))
      (is (= #{"paged-1" "escalate-1"} (set (:stalled result))))
      (is (= #{"paged-1"} (set (:paged result))))
      (is (= #{"escalate-1"} (set (:escalated result))))
      (is (= #{"paged-1" "escalate-1"} (set @page-calls)))
      (is (= [["escalate-1" :page-failed]] @escalations)))))

(deftest run-scan-cycle-excludes-self
  (testing "run-scan-cycle! does not page tickle-1"
    (register-agent! "claude-1")
    (register-agent! "tickle-1")
    (let [store (make-evidence-store)
          page-calls (atom [])
          result (tickle/run-scan-cycle!
                  {:evidence-store store
                   :threshold-seconds 60
                   :self-id "tickle-1"
                   :page-config {:ring-test-bell! (fn [{:keys [agent-id]}]
                                                    (swap! page-calls conj agent-id)
                                                    {:bell/type :test-bell})}})]
      (is (not (some #{"tickle-1"} (:stalled result))))
      (is (not (some #{"tickle-1"} @page-calls))))))

(deftest start-watchdog-stop-fn-halts-loop
  (testing "start-watchdog! returns stop-fn that halts the loop"
    (register-agent! "claude-1")
    (let [store (make-evidence-store)
          cycles (atom 0)
          {:keys [stop-fn started-at]}
          (tickle/start-watchdog!
           {:evidence-store store
            :interval-ms 25
            :threshold-seconds 60
            :page-config {:ring-test-bell! (fn [_] {:bell/type :test-bell})}
            :on-cycle (fn [_] (swap! cycles inc))})]
      (Thread/sleep 200)
      (is (instance? Instant started-at))
      (is (pos? @cycles))
      (stop-fn)
      ;; Allow any in-flight cycle to finish before sampling
      (Thread/sleep 150)
      (let [stopped-at @cycles]
        (Thread/sleep 150)
        (is (= stopped-at @cycles))))))

(deftest start-watchdog-on-cycle-callback-fires
  (testing "start-watchdog! invokes on-cycle callback"
    (register-agent! "claude-1")
    (let [store (make-evidence-store)
          fired (promise)
          {:keys [stop-fn]}
          (tickle/start-watchdog!
           {:evidence-store store
            :interval-ms 25
            :threshold-seconds 60
            :page-config {:ring-test-bell! (fn [_] {:bell/type :test-bell})}
            :on-cycle (fn [cycle-result] (deliver fired cycle-result))})]
      (let [cycle (deref fired 500 ::timeout)]
        (is (not= ::timeout cycle))
        (is (map? cycle)))
      (stop-fn))))

(deftest scan-cycle-emits-tickle-scan-evidence
  (testing "run-scan-cycle! emits evidence entry tagged [:tickle :scan]"
    (register-agent! "claude-1")
    (let [store (make-evidence-store)
          _ (tickle/run-scan-cycle!
             {:evidence-store store
              :threshold-seconds 42
              :page-config {:ring-test-bell! (fn [_] {:bell/type :test-bell})}})
          entries (estore/query* store {:query/type :coordination})
          tickle-entry (first (filter #(= [:tickle :scan] (:evidence/tags %)) entries))]
      (is (some? tickle-entry))
      (is (= 42 (get-in tickle-entry [:evidence/body :threshold-seconds])))
      (is (string? (get-in tickle-entry [:evidence/body :cycle-at]))))))
