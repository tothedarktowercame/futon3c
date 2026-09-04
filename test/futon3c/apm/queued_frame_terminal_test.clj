(ns futon3c.apm.queued-frame-terminal-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.queued-frame-terminal :as sut]))

(defn- run-at [state persisted now result calls]
  (with-redefs [sut/retire! (fn [_] (swap! calls inc) result)]
    (sut/retire-with-retry!
     {:retry-state state
      :now-ms-fn (constantly now)
      :persist-retry-fn #(do (reset! persisted %) {:ok true})})))

(defn- valid-terminal [frame]
  (let [body {:receipt/type :frame-terminal
              :frame/id (:frame/id frame) :problem/id (:problem/id frame)
              :frame/result :void :problem/outcome :unsolved
              :learning/outcome :unobserved
              :solver {:branch "exp/f84" :head (apply str (repeat 40 "a"))}
              :workspace/terminal-heads
              {:solver (apply str (repeat 40 "a"))
               :student (apply str (repeat 40 "b"))}}]
    (assoc body :receipt/id
           (machine/ledger-digest [body]))))

(deftest retire-surfaces-pending-without-retiring-seats
  (let [frame {:frame/id "f84" :problem/id "p84"}
        seats-called (atom 0)
        result (sut/retire!
                {:frame frame :terminal-receipt (valid-terminal frame)
                 :leases {:solver {:lease/id "s"} :student {:lease/id "t"}}
                 :persist-bank-fn (fn [& _] {:ok true})
                 :retirement-status-fn (fn [& _] {:ok true :status :not-retired})
                 :audit-fn
                 (fn [& _]
                   {:ok false
                    :error/code :workspace-retirement-audit-pending
                    :pending #{:no-running-or-parked-job-references-workspace}})
                 :retire-workspace-fn (fn [& _] (throw (ex-info "not due" {})))
                 :retire-seats-fn (fn [& _] (swap! seats-called inc) {:ok true})})]
    (is (false? (:ok result)))
    (is (= :workspace-retirement-audit-pending (:status result)))
    (is (zero? @seats-called))))

(deftest pending-retries-durably-then-succeeds
  (let [persisted (atom nil)
        calls (atom 0)
        pending {:ok false :error/code :workspace-retirement-audit-pending
                 :status :workspace-retirement-audit-pending
                 :pending #{:no-running-or-parked-job-references-workspace}}
        first-result (run-at nil persisted 1000 pending calls)
        waiting (run-at @persisted persisted 2000 pending calls)
        calls-before-due @calls
        success {:ok true :workspace-receipts {:solver {:receipt/id "done"}}}
        final-result (run-at @persisted persisted 16000 success calls)]
    (is (= :awaiting-substrate (:status first-result)))
    (is (= :awaiting-substrate (:status waiting)))
    (is (= 1 calls-before-due) "not-before prevents an early audit")
    (is (:ok final-result))
    (is (= :resolved (:retry/status @persisted)))
    (is (= [:pending :resolved]
           (mapv :classification (:retry/attempts @persisted))))
    (is (= 2 @calls))))

(deftest pending-exhausts-with-distinct-durable-failure
  (let [persisted (atom nil)
        calls (atom 0)
        pending {:ok false :error/code :workspace-retirement-audit-pending
                 :status :workspace-retirement-audit-pending
                 :pending #{:no-active-ledger-claim-references-workspace}}
        times (reductions + 0 sut/retirement-audit-retry-delays-ms)
        results (mapv (fn [now]
                        (run-at @persisted persisted now pending calls))
                      times)]
    (is (= :workspace-retirement-audit-retry-exhausted
           (:error/code (last results))))
    (is (= :exhausted (:retry/status @persisted)))
    (is (= 8 (count (:retry/attempts @persisted))))
    (is (= 8 @calls))))

(deftest structural-invalid-is-fatal-on-first-observation
  (let [persisted (atom nil)
        calls (atom 0)
        invalid {:ok false :error/code :workspace-retirement-audit-invalid
                 :missing #{:workspace-head-matches-terminal-head}}
        first-result (run-at nil persisted 0 invalid calls)
        replay (run-at @persisted persisted 100000 invalid calls)]
    (is (= invalid first-result))
    (is (= invalid replay))
    (is (= :structural-invalid (:retry/status @persisted)))
    (is (= 1 @calls) "a durable structural failure is never audited twice")))

(deftest retry-evidence-keeps-each-pending-set-and-timestamp
  (let [persisted (atom nil)
        calls (atom 0)
        pending-a {:ok false :error/code :workspace-retirement-audit-pending
                   :status :workspace-retirement-audit-pending
                   :pending #{:no-running-or-parked-job-references-workspace}}
        pending-b {:ok false :error/code :workspace-retirement-audit-pending
                   :status :workspace-retirement-audit-pending
                   :pending #{:no-active-ledger-claim-references-workspace}}]
    (run-at nil persisted 10 pending-a calls)
    (run-at @persisted persisted 15010 pending-b calls)
    (is (= [{:attempt 1 :observed-at-ms 10 :classification :pending
             :pending (:pending pending-a)}
            {:attempt 2 :observed-at-ms 15010 :classification :pending
             :pending (:pending pending-b)}]
           (:retry/attempts @persisted)))))
